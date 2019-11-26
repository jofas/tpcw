module affinity_schedule
  !
  ! B160509
  !
  ! Module containing the implementation of AffinitySched-
  ! ule, which is a schedule for OpenMP's loop directive.
  !
  ! It splits the iterations of a loop into #threads
  ! approximately equal splits (like the static schedule).
  ! Theses splits however are not processed in one chunk,
  ! they are again splitted into smaller chunks (the chunk
  ! size decreasing with more iterations already proces-
  ! sed). Once a thread is finished with its split, it
  ! takes chunks from the split that has the most
  ! iterations yet to do, making the AffinitySchedule a
  ! hybrid between the static and the dynamic schedule.
  !
  ! The AffinitySchedule uses a priority queue underneath
  ! to track which split has still the most iterations
  ! left.
  !
  use omp_lib
  use priority_queue

  implicit none

  private

  type OptionInterval
    logical :: is_none
    integer :: lower, upper
  end type


  type Split
    integer :: remaining_iter
    integer :: begin_next_interval
  end type


  type AffinitySchedule
    type(Split), dimension(:), allocatable, private :: splits
    integer(kind=omp_lock_kind), dimension(:), allocatable, &
      private :: split_locks
    type(MaxPriorityQueue), private :: priority_queue
    integer(kind=omp_lock_kind), private :: priority_queue_lock
    integer :: worker_amount
  end type


  public :: AffinitySchedule
  public :: OptionInterval

  public :: init_schedule
  public :: take

contains

  subroutine init_schedule(self, id, loop_size)
    !
    ! Initializes the AffinitySchedule. A single thread
    ! allocates the split instances and initializes the
    ! priority queue, before every thread initializes his
    ! designated split and its element in the priority
    ! queue.
    !
    ! Once all threads are finished, they wait before a
    ! single thread builds the heap of the priority queue.
    ! After this subroutine the priority queue is valid.
    !
    type(AffinitySchedule), intent(inout) :: self

    integer, intent(out) :: id
    integer, intent(in) :: loop_size

    id = omp_get_thread_num() + 1

    !$omp single
    self%worker_amount = omp_get_num_threads()

    call alloc_splits(self)
    call init_priority_queue(self)
    !$omp end single

    call init_split(self, loop_size, id)
    call set_element_unordered( &
      self%priority_queue, id, self%splits(id)%remaining_iter &
    )

    !$omp barrier

    !$omp single
    call build_max_heap(self%priority_queue)
    !$omp end single
  end


  subroutine alloc_splits(self)
    type(AffinitySchedule), intent(inout) :: self

    allocate(self%splits(self%worker_amount))
    allocate(self%split_locks(self%worker_amount))
  end


  subroutine init_priority_queue(self)
    !
    ! Get an instance of a MaxPriorityQueue and set its
    ! heap size to #threads. Also initialize the lock for
    ! the queue.
    !
    type(AffinitySchedule), intent(inout) :: self

    self%priority_queue = new_max_priority_queue(self%worker_amount)
    self%priority_queue%heap_size = self%worker_amount
    call omp_init_lock(self%priority_queue_lock)
  end


  subroutine init_split(self, loop_size, id)
    !
    ! Get an instance of Split. Also initialize the lock
    ! for it.
    !
    type(AffinitySchedule), intent(inout) :: self
    integer, intent(in) :: loop_size, id

    self%splits(id) = &
      new_split(loop_size, id, self%worker_amount)

    call omp_init_lock(self%split_locks(id))
  end


  type(OptionInterval) function take(self, id)
    !
    ! Takes the next chunk from a split. First this
    ! function tries to get a chunk from the id-split.
    ! If the id-split is already done, takes a chunk from
    ! the split that has still the most iterations to do.
    !
    ! If every split is done, an invalid interval is
    ! returned, which is then used to break the execution
    ! of the id-thread.
    !
    type(AffinitySchedule), intent(inout) :: self
    integer, intent(in) :: id

    integer :: remaining_iter, id_biggest

    call omp_set_lock(self%split_locks(id))

    if (self%splits(id)%remaining_iter > 0) then
      call take_(self, take, remaining_iter, id)
      call omp_unset_lock(self%split_locks(id))

    else
      call omp_unset_lock(self%split_locks(id))

      id_biggest = get_id_of_biggest_split(self)

      call omp_set_lock(self%split_locks(id_biggest))
      call take_(self, take, remaining_iter, id_biggest)
      call omp_unset_lock(self%split_locks(id_biggest))
    end if

    call decrease_key_(self, id, remaining_iter)
  end


  integer function get_id_of_biggest_split(self)
    type(AffinitySchedule), intent(inout) :: self

    call omp_set_lock(self%priority_queue_lock)
    get_id_of_biggest_split = get_max_element(self%priority_queue)
    call omp_unset_lock(self%priority_queue_lock)
  end


  subroutine take_(self, take, remaining_iter, id)
    !
    ! Gets the interval and the remaining iterations from
    ! the id-split. If the id-split has already finished,
    ! a invalid interval is returned.
    !
    type(AffinitySchedule), intent(inout) :: self
    type(OptionInterval), intent(out) :: take
    integer, intent(out) :: remaining_iter
    integer, intent(in) :: id

    take = get_interval(self%splits(id), self%worker_amount)

    remaining_iter = self%splits(id)%remaining_iter
  end


  subroutine decrease_key_(self, id, new_key)
    !
    ! Decrease the key of id-element in the heap of the
    ! priority queue to new_id.
    !
    type(AffinitySchedule), intent(inout) :: self
    integer, intent(in) :: id, new_key

    call omp_set_lock(self%priority_queue_lock)
    call decrease_key(self%priority_queue, id, new_key)
    call omp_unset_lock(self%priority_queue_lock)
  end


  type(OptionInterval) function get_interval(split_, worker_amount)
    type(Split), intent(inout) :: split_
    integer, intent(in) :: worker_amount

    if (split_%remaining_iter > 0) then
      get_interval = valid_interval(get_boundries( &
        split_, worker_amount &
      ))
    else
      get_interval = invalid_interval()
    end if
  end


  type(OptionInterval) function valid_interval(boundries)
    integer, dimension(2), intent(in) :: boundries

    valid_interval = &
      OptionInterval(.false., boundries(1), boundries(2))
  end


  type(OptionInterval) function invalid_interval()
    invalid_interval = OptionInterval(.true., -1, -2)
  end


  function get_boundries(split_, worker_amount)
    !
    ! Returns the boundries of the next chunk (interval) to
    ! be executed.
    !
    ! Updates the split instance afterwards.
    !
    integer, dimension(2) :: get_boundries

    type(Split), intent(inout) :: split_
    integer, intent(in) :: worker_amount

    integer :: chunk_size

    chunk_size = &
      get_chunk_size(split_%remaining_iter, worker_amount)

    get_boundries(1) = split_%begin_next_interval
    get_boundries(2) = split_%begin_next_interval &
                     + chunk_size - 1

    call update_split(split_, chunk_size)
  end


  integer function get_chunk_size(remaining_iter, worker_amount)
    !
    ! Returns the chunk size of the next to-be-executed
    ! chunk. The chunk size decreases with more iterations
    ! Already executed.
    !
    integer, intent(in) :: remaining_iter, worker_amount

    get_chunk_size = ceiling(float(remaining_iter) &
      / float(worker_amount))
  end


  subroutine update_split(split_, chunk_size)
    !
    ! Sets the reamining iterations and the pointer to the
    ! starting element of the next chunk to be taken from
    ! the split.
    !
    type(Split), intent(inout) :: split_
    integer, intent(in) :: chunk_size

    split_%remaining_iter = &
      split_%remaining_iter - chunk_size

    split_%begin_next_interval = &
      split_%begin_next_interval + chunk_size
  end


  type(Split) function new_split(loop_size, id, worker_amount)
    !
    ! Constructor of Split.
    !
    ! Takes the loop size, its place (id) and the amount of
    ! splits (worker_amount) as inputs and computes the
    ! pointer to the first element and how many iterations
    ! to do.
    !
    integer, intent(in) :: loop_size, id, worker_amount

    integer :: part, lower_boundry, upper_boundry

    part = (loop_size + worker_amount - 1) / worker_amount
    lower_boundry = (id - 1) * part + 1
    upper_boundry = get_upper_boundry(id, part, loop_size)

    new_split%begin_next_interval = lower_boundry
    new_split%remaining_iter = upper_boundry - lower_boundry + 1
  end


  integer function get_upper_boundry(id, part, loop_size)
    integer, intent(in) :: id, part, loop_size

    get_upper_boundry = id * part
    if (get_upper_boundry > loop_size) &
      get_upper_boundry = loop_size
  end
end
