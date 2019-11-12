module affinity_schedule
  use omp_lib

  implicit none

  private

  type Interval
    integer :: lower, upper
  end type


  type Split
    integer :: remaining_iter
    integer :: begin_next_interval
    integer :: worker_amount
    logical :: done
  end type


  type AffinitySchedule
    type(Split), dimension(:), allocatable :: splits
  end type


  public :: AffinitySchedule
  public :: Interval

  public :: init_schedule
  public :: done
  public :: take

contains

  subroutine init_schedule(self, id, loop_size)
    ! (!) self must be intent(inout), so self is shared
    !     over all threads
    !
    type(AffinitySchedule), intent(inout) :: self

    integer, intent(out) :: id
    integer, intent(in) :: loop_size

    integer :: worker_amount

    worker_amount = omp_get_num_threads()
    id = omp_get_thread_num() + 1

    !$omp single
    allocate(self%splits(worker_amount))
    !$omp end single

    self%splits(id) = new_split(loop_size, id, worker_amount)
  end


  logical function done(self, id)
    type(AffinitySchedule), intent(inout) :: self
    integer, intent(in) :: id

    done = self%splits(id)%done
  end


  type(Interval) function take(self, id)
    type(AffinitySchedule), intent(inout) :: self
    integer, intent(in) :: id

    ! TODO: lock this function to avoid race condition
    take = take_(self%splits(id))
  end


  type(Interval) function take_(split_)
    type(Split), intent(inout) :: split_

    integer :: chunk_size, upper_boundry

    chunk_size = split_%remaining_iter / split_%worker_amount
    if (chunk_size == 0) chunk_size = 1

    upper_boundry = split_%begin_next_interval + chunk_size - 1

    take_ = Interval(split_%begin_next_interval, upper_boundry)

    split_%remaining_iter = split_%remaining_iter - chunk_size
    split_%begin_next_interval = &
      split_%begin_next_interval + chunk_size

    if (split_%remaining_iter == 0) split_%done = .true.
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
