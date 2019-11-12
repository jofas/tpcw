module priority_queue
  implicit none

  !private

  type Heap
    integer, dimension(:), allocatable :: heap
    integer, dimension(:), allocatable :: indexes
    integer :: heap_size
  end type

contains

  type(Heap) function new_heap(max_size)
    integer, intent(in) :: max_size

    allocate(new_heap%heap(max_size))
    allocate(new_heap%indexes(max_size))
    new_heap%heap_size = 0
  end


  integer function get_max_idx(self)
    type(Heap), intent(in) :: self
    get_max_idx = self%indexes(1)
  end


  subroutine build_max_heap(self)
    type(Heap), intent(inout) :: self

    integer :: i

    do i = self%heap_size / 2, 1, -1
      call heap_max_heapify(self, i)
    end do
  end


  subroutine decrease_key(self, idx, key)
    type(Heap), intent(inout) :: self
    integer, intent(in) :: idx, key

    integer :: i, p, l, r, old_key

    if (self%heap_size == 0) return

    !TODO: optimize with reverse lookup array
    i = get_heap_pos_by_idx(self, idx)

    old_key = self%heap(i)
    self%heap(i) = key

    do while (i <= self%heap_size)

      ! TODO: wrong approach -> don't take right, take biggest l,r

      l = heap_left(i)
      r = heap_right(i)

      if (r <= self%heap_size .and. self%heap(r) > self%heap(i)) then
        call exchange(self, i, r)

        i = r
        r = heap_right(i)
        l = heap_left(i)

      elseif (l <= self%heap_size .and. self%heap(l) > self%heap(i)) then
        call exchange(self, i, l)

        i = l
        r = heap_right(i)
        l = heap_left(i)

      else
        exit
      end if
    end do

    if (key == 0) then
      self%heap(i) = self%heap(self%heap_size)
      self%indexes(i) = self%indexes(self%heap_size)

      self%heap_size = self%heap_size - 1

      call heap_max_heapify(self, i)
      return
    end if
  end


  recursive subroutine heap_max_heapify(heap_, i)
    type(Heap), intent(inout) :: heap_
    integer, intent(in) :: i

    integer :: l, r
    integer :: max_, tmp

    l = heap_left(i)
    r = heap_right(i)

    if (l <= heap_%heap_size .and. heap_%heap(l) > heap_%heap(i)) then
      max_ = l
    else
      max_ = i
    end if

    if (r <= heap_%heap_size .and. heap_%heap(r) > heap_%heap(max_)) then
      max_ = r
    end if

    if (max_ /= i) then
      call exchange(heap_, i, max_)
      call heap_max_heapify(heap_, max_)
    end if
  end


  subroutine exchange(self, one, two)
    type(Heap), intent(inout) :: self
    integer, intent(in) :: one, two

    integer :: tmp_key, tmp_idx

    tmp_key = self%heap(one)
    tmp_idx = self%indexes(one)

    self%heap(one) = self%heap(two)
    self%indexes(one) = self%indexes(two)

    self%heap(two) = tmp_key
    self%indexes(two) = tmp_idx
  end


  integer function get_heap_pos_by_idx(self, idx)
    type(Heap), intent(in) :: self
    integer, intent(in) :: idx

    do get_heap_pos_by_idx = 1, self%heap_size
      if (self%indexes(get_heap_pos_by_idx) == idx) exit
    end do
  end


  integer function heap_parent(i)
    integer, intent(in) :: i

    heap_parent = i / 2
  end


  integer function heap_left(i)
    integer, intent(in) :: i

    heap_left = 2 * i
  end


  integer function heap_right(i)
    integer, intent(in) :: i

    heap_right = 2 * i + 1
  end


  subroutine print_heap(self)
    type(Heap) :: self

    print *, self%heap
    print *, self%indexes, self%heap_size
  end
end
