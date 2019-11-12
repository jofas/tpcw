module affinity_schedule
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
    integer :: worker_amount
    logical :: done
  end type


  type AffinitySchedule
    type(Split), dimension(:), allocatable, private :: splits

    integer(kind=omp_lock_kind), dimension(:), allocatable, &
      private :: split_locks


    type(Heap), private :: priority_queue

    integer(kind=omp_lock_kind), private :: priority_queue_lock
  end type


  interface done
    procedure :: done_all
    procedure :: done_by_id
  end interface


  interface take
    procedure :: take_biggest
    procedure :: take_by_id
  end interface


  public :: AffinitySchedule
  public :: OptionInterval

  public :: init_schedule
  public :: done
  public :: take

contains

  subroutine init_schedule(self, id, loop_size)
    type(AffinitySchedule), intent(inout) :: self

    integer, intent(out) :: id
    integer, intent(in) :: loop_size

    integer :: worker_amount

    worker_amount = omp_get_num_threads()
    id = omp_get_thread_num() + 1

    !$omp single
    allocate(self%splits(worker_amount))
    allocate(self%split_locks(worker_amount))

    self%priority_queue = new_heap(worker_amount)
    call omp_init_lock(self%priority_queue_lock)

    !$omp end single

    self%splits(id) = new_split(loop_size, id, worker_amount)
    call omp_init_lock(self%split_locks(id))

    self%priority_queue%heap(id) = self%splits(id)%remaining_iter
    self%priority_queue%indexes(id) = id

    !$omp barrier

    !$omp single
    self%priority_queue%heap_size = worker_amount
    call build_max_heap(self%priority_queue)
    !$omp end single
  end


  logical function done_all(self)
    type(AffinitySchedule), intent(inout) :: self

    done_all = .false.
    !call omp_set_lock(self%priority_queue_lock)
    !done_all = self%priority_queue%heap_size == 0
    !call omp_unset_lock(self%priority_queue_lock)
  end


  logical function done_by_id(self, id)
    type(AffinitySchedule), intent(inout) :: self
    integer, intent(in) :: id

    call omp_set_lock(self%split_locks(id))
    done_by_id = self%splits(id)%done
    call omp_unset_lock(self%split_locks(id))
  end


  type(OptionInterval) function take_biggest(self)
    type(AffinitySchedule), intent(inout) :: self

    integer :: id, max_, i

    max_ = 0
    id = 1

    do i=1,size(self%splits)
      call omp_set_lock(self%split_locks(i))
      if (self%splits(i)%remaining_iter > max_) &
        id = i
      call omp_unset_lock(self%split_locks(i))
    end do
    !print *, "took: ", id, "done? ", done_by_id(self, id)
    !call omp_set_lock(self%priority_queue_lock)
    !id = get_max_idx(self%priority_queue)
    !call omp_unset_lock(self%priority_queue_lock)

    take_biggest = take_by_id(self, id)
  end


  type(OptionInterval) function take_by_id(self, id)
    type(AffinitySchedule), intent(inout) :: self
    integer, intent(in) :: id

    integer :: remaining_iter

    call omp_set_lock(self%split_locks(id))
    take_by_id = take_(self%splits(id))
    remaining_iter = self%splits(id)%remaining_iter
    call omp_unset_lock(self%split_locks(id))

    !call omp_set_lock(self%priority_queue_lock)
    !call decrease_key(self%priority_queue, id, remaining_iter)
    !call omp_unset_lock(self%priority_queue_lock)
  end


  type(OptionInterval) function take_(split_)
    type(Split), intent(inout) :: split_

    integer :: chunk_size, upper_boundry
    if (.not. split_%done) then
      chunk_size = split_%remaining_iter / split_%worker_amount
      if (chunk_size == 0) chunk_size = 1

      upper_boundry = split_%begin_next_interval + chunk_size - 1

      take_ = OptionInterval( &
        .false., &
        split_%begin_next_interval, &
        upper_boundry &
      )

      split_%remaining_iter = split_%remaining_iter - chunk_size
      split_%begin_next_interval = &
        split_%begin_next_interval + chunk_size

      if (split_%remaining_iter == 0) split_%done = .true.

    else
      take_ = OptionInterval(.true., -1, -2)
    end if
  end


  type(Split) function new_split(loop_size, id, worker_amount)
    integer, intent(in) :: loop_size, id, worker_amount

    integer :: part, lower_boundry, upper_boundry

    part = (loop_size + worker_amount - 1) / worker_amount

    lower_boundry = (id - 1) * part + 1

    upper_boundry = id * part
    if (upper_boundry > loop_size) upper_boundry = loop_size

    new_split%begin_next_interval = lower_boundry
    new_split%remaining_iter = upper_boundry - lower_boundry + 1
    new_split%worker_amount = worker_amount
    new_split%done = .false.
  end
end
