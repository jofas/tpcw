program loops
  !
  ! B160509
  !

  use affinity_schedule

  use omp_lib

  implicit none
  integer, parameter :: N=729
  integer, parameter :: reps=1000

  real(kind=8), allocatable ::  a(:,:), b(:,:), c(:)
  integer :: jmax(N)


  real(kind=8) :: start1,start2,end1,end2
  integer :: r

  allocate(a(N,N), b(N,N), c(N))

  call init1()

  start1 = omp_get_wtime()

  do r = 1,reps
     call runloop(1)
  end do

  end1  = omp_get_wtime()

  call valid1();

  print *, "Total time for ",reps," reps of loop 1 = ", end1-start1

  call init2()

  start2 = omp_get_wtime()

  do r = 1,reps
     call runloop(2)
  end do

  end2  = omp_get_wtime()

  call valid2();

  print *, "Total time for ",reps," reps of loop 2 = ", end2-start2

contains

subroutine init1()

  implicit none

  integer ::  i,j

  do i = 1,N
     do j = 1,N
        a(j,i) = 0.0
        b(j,i) = 3.142*(i+j)
     end do
  end do

end subroutine init1


subroutine init2()

  implicit none

  integer ::  i,j,expr

  do i = 1,N
     expr = mod(i,3*(i/30)+1)
     if (expr == 0) then
        jmax(i) = N
     else
        jmax(i) = 1
     end if
     c(i) = 0.0
  end do

  do i = 1,N
     do j = 1,N
        b(j,i) = dble(i*j+1)/dble(N*N)
     end do
  end do

end subroutine init2

subroutine runloop(loopid)

  implicit none

  integer, intent(in) :: loopid
  integer :: myid

  type(AffinitySchedule) :: schedule
  type(OptionInterval) :: interv

  !$omp parallel default(none)  private(myid, interv) &
  !$omp   shared(loopid, schedule)

  call init_schedule(schedule, myid, N)

  do
    interv = take(schedule, myid)
    if (.not. interv%is_none) then
      call do_work(loopid, interv%lower, interv%upper)
    else
      exit
    end if
  end do

  !$omp end parallel

end subroutine runloop


subroutine do_work(loopid,lo,hi)

  implicit none

  integer, intent(in) :: loopid,lo,hi

  select case (loopid)
    case (1)
      call loop1chunk(lo,hi)
    case (2)
      call loop2chunk(lo,hi)
  end select

end subroutine do_work


subroutine loop1chunk(lo,hi)

  implicit none

  integer ::  i,j,lo,hi

  do i = lo,hi
     do j = N,i,-1
        a(j,i) = a(j,i) + cos(b(j,i))
     end do
  end do

end subroutine loop1chunk



subroutine loop2chunk(lo,hi)

  implicit none

  integer :: i,j,k,lo,hi
  real (kind=8) :: rN2

  rN2 = 1.0 / dble (N*N)

  do i = lo,hi
     do j = 1, jmax(i)
        do k = 1,j
           c(i) = c(i) + k * log(b(j,i)) *rN2
        end do
     end do
  end do

end subroutine loop2chunk



subroutine valid1()

  implicit none

  integer :: i,j
  real (kind=8) :: suma

  suma= 0.0

  do i = 1,N
     do j = 1,N
        suma = suma + a(j,i)
     end do
  end do

  print *, "Loop 1 check: Sum of a is ", suma

end subroutine valid1



subroutine valid2()

  implicit none

  integer i
  real (kind=8) sumc

  sumc= 0.0
  do i = 1,N
     sumc = sumc + c(i)
  end do

  print *, "Loop 2 check: Sum of c is ", sumc

end subroutine valid2


end program loops
