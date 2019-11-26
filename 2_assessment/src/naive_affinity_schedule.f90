module naive_affinity_schedule
  !
  ! B160509
  !
  ! See affinity_schedule.f90.
  !
  ! Implements a naive version of the AffinitySchedule. It
  ! is naive in the sense that it uses brute force rather
  ! than a priority queue to get the split with the highest
  ! remaining iterations to do.
  !
  ! While brute force takes longer than accessing the
  ! priority queue, it needs less synchronization.
  !
  use omp_lib

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


  type NaiveAffinitySchedule
    type(Split), dimension(:), allocatable, private :: splits
    integer(kind=omp_lock_kind), dimension(:), allocatable, &
      private :: split_locks
    integer :: worker_amount
  end type


  public :: NaiveAffinitySchedule
  public :: OptionInterval

  public :: init_schedule
  public :: take

contains

  subroutine init_schedule(self, id, loop_size)
    !
    ! Initializes the NaiveAffinitySchedule. A single
    ! thread allocates the split instances, before every
    ! thread initializes his designated split.
    !
    ! Once all threads are finished, they wait for all
    ! threads to finish initialization, to avoid a race
    ! condition. Without the barrier, one thread could
    ! finish his split and try to access the split with
    ! the highest remaining iterations left. If a thread
    ! has not finished the initalization, this will result
    ! in a segfault.
    !
    type(NaiveAffinitySchedule), intent(inout) :: self

    integer, intent(out) :: id
    integer, intent(in) :: loop_size

    id = omp_get_thread_num() + 1

    !$omp single
    self%worker_amount = omp_get_num_threads()

    call alloc_splits(self)
    !$omp end single

    call init_split(self, loop_size, id)

    !$omp barrier
  end


  subroutine alloc_splits(self)
    type(NaiveAffinitySchedule), intent(inout) :: self

    allocate(self%splits(self%worker_amount))
    allocate(self%split_locks(self%worker_amount))
  end


  subroutine init_split(self, loop_size, id)
    !
    ! Get an instance of Split. Also initialize the lock
    ! for it.
    !
    type(NaiveAffinitySchedule), intent(inout) :: self
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
    type(NaiveAffinitySchedule), intent(inout) :: self
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
  end


  integer function get_id_of_biggest_split(self)
    !
    ! Naive brute force.
    !
    type(NaiveAffinitySchedule), intent(inout) :: self

    integer :: max_, i

    max_ = 0
    get_id_of_biggest_split = 1

    do i = 1, size(self%splits)
      call omp_set_lock(self%split_locks(i))

      if (self%splits(i)%remaining_iter > max_) then
        get_id_of_biggest_split = i
        max_ = self%splits(i)%remaining_iter
      end if

      call omp_unset_lock(self%split_locks(i))
    end do
  end


  subroutine take_(self, take, remaining_iter, id)
    !
    ! Gets the interval and the remaining iterations from
    ! the id-split. If the id-split has already finished,
    ! a invalid interval is returned.
    !
    type(NaiveAffinitySchedule), intent(inout) :: self
    type(OptionInterval), intent(out) :: take
    integer, intent(out) :: remaining_iter
    integer, intent(in) :: id

    take = get_interval(self%splits(id), self%worker_amount)

    remaining_iter = self%splits(id)%remaining_iter
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

    get_chunk_size = remaining_iter / worker_amount
    if(get_chunk_size == 0) get_chunk_size = 1
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

