module priority_queue
  !
  ! Module containing the implementation for a fixed size
  ! priority queue which prioritizes higher values.
  !
  ! The implementation is based on a heap and pairs the
  ! keys from the heap with a lookup array for indices of
  ! an array.
  !
  ! The queue is defined for positive integers excluding
  ! zero. Elements, which key is decreased to zero are
  ! automatically removed from the queue.
  !
  ! This implementation supports fast lookup of the element
  ! with the highest priority and the element-based
  ! decreasing of its corresponding heap key. In order to
  ! make the element-wise decreasing operation fast it also
  ! contains a reverse lookup array for fast access.
  !
  implicit none

  private

  type MaxPriorityQueue
    integer, dimension(:), allocatable, private :: heap
    integer, dimension(:), allocatable, private :: lookup
    integer, dimension(:), allocatable, private :: reverse_lookup
    integer :: heap_size
  end type

  public :: MaxPriorityQueue

  public :: new_max_priority_queue
  public :: get_max_element
  public :: build_max_heap
  public :: decrease_key
  public :: set_element_unordered

contains

  type(MaxPriorityQueue) function new_max_priority_queue(max_size)
    !
    ! Constructor of MaxPriorityQueue.
    !
    ! Takes the maximum size of the instance as argument.
    !
    integer, intent(in) :: max_size

    allocate(new_max_priority_queue%heap(max_size))
    allocate(new_max_priority_queue%lookup(max_size))
    allocate(new_max_priority_queue%reverse_lookup(max_size))
    new_max_priority_queue%heap_size = 0
  end


  subroutine set_element_unordered(self, i, key)
    !
    ! Sets the element of the heap at index i to key and
    ! sets the lookup and reverse lookup arrays correspon-
    ! dingly.
    !
    ! This subroutine can be used for initialization and
    ! should otherwise be used with care.
    !
    type(MaxPriorityQueue), intent(inout) :: self
    integer, intent(in) :: i, key

    self%heap(i) = key
    self%lookup(i) = i
    self%reverse_lookup(i) = i
  end


  integer function get_max_element(self)
    !
    ! Returns the element with the highest priority.
    !
    type(MaxPriorityQueue), intent(in) :: self

    get_max_element = self%lookup(1)
  end


  subroutine build_max_heap(self)
    !
    ! Builds the heap structure from unordered self.
    !
    type(MaxPriorityQueue), intent(inout) :: self

    integer :: i

    do i = self%heap_size / 2, 1, -1
      call max_heapify(self, i)
    end do
  end


  subroutine decrease_key(self, elem, key)
    !
    ! Decreases the key of the element. In order to keep
    ! the heap property, the element may be recursively
    ! switched with its biggest child inside the heap.
    !
    ! Automatically removes the element from self if the
    ! provided key is zero.
    !
    type(MaxPriorityQueue), intent(inout) :: self
    integer, intent(in) :: elem, key

    integer :: i, max_child, l, r

    if (self%heap_size == 0) return

    i = self%reverse_lookup(elem)

    if (key == 0) then
      call remove(self, i)
      return
    end if

    self%heap(i) = key

    do
      l = left(i)
      r = right(i)

      if (l <= self%heap_size) then
        max_child = l
      else
        exit
      end if

      if (r <= self%heap_size) then
        if (self%heap(r) > self%heap(l)) then
          max_child = r
        end if
      else
        exit
      end if

      if (self%heap(max_child) > self%heap(i)) then
        call exchange(self, i, max_child)
        i = max_child
      else
        exit
      end if
    end do
  end


  subroutine max_heapify(self, i_)
    !
    ! Builds heap property for element at index i_.
    !
    type(MaxPriorityQueue), intent(inout) :: self
    integer, intent(in) :: i_

    integer :: i, l, r, max_

    i = i_

    do
      l = left(i)
      r = right(i)

      if (l <= self%heap_size .and. self%heap(l) > self%heap(i)) then
        max_ = l
      else
        max_ = i
      end if

      if (r <= self%heap_size .and. self%heap(r) > self%heap(max_)) then
        max_ = r
      end if

      if (max_ /= i) then
        call exchange(self, i, max_)
        i = max_
      else
        exit
      end if
    end do
  end


  subroutine remove(self, i)
    !
    ! Removes element at index i from the queue.
    !
    type(MaxPriorityQueue), intent(inout) :: self
    integer, intent(in) :: i

    self%heap(i) = self%heap(self%heap_size)
    self%lookup(i) = self%lookup(self%heap_size)

    self%heap_size = self%heap_size - 1

    call max_heapify(self, i)
  end


  subroutine exchange(self, one, two)
    !
    ! Changes places of elements in the heap at index one
    ! with index two.
    !
    type(MaxPriorityQueue), intent(inout) :: self
    integer, intent(in) :: one, two

    integer :: tmp_key, tmp_idx

    self%reverse_lookup(self%lookup(one)) = two
    self%reverse_lookup(self%lookup(two)) = one

    tmp_key = self%heap(one)
    tmp_idx = self%lookup(one)

    self%heap(one) = self%heap(two)
    self%lookup(one) = self%lookup(two)

    self%heap(two) = tmp_key
    self%lookup(two) = tmp_idx
  end


  integer function left(i)
    !
    ! Returns left children in heap of element at index i.
    !
    integer, intent(in) :: i

    left = 2 * i
  end


  integer function right(i)
    !
    ! Returns right children in heap of element at index i.
    !
    integer, intent(in) :: i

    right = 2 * i + 1
  end
end
