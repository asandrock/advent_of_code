program main
  use StringiFor
  use PENF, only: I4P
  !use SF_ARRAYS, only: linspace
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: lines(:), max_time(:), min_dist(:)

  integer :: j, k, time, dist, farther, prod

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  call lines(1)%split(tokens=max_time, sep=' ')
  call lines(2)%split(tokens=min_dist, sep=' ')

  prod = 1
  do j = 1, size(max_time) - 1
    time = max_time(j + 1)%to_number(kind=I4P)
    dist = min_dist(j + 1)%to_number(kind=I4P)
    farther = 0
    do k = 0, time
      if (distance(k, time) > dist) farther = farther + 1
    end do
    !print *, farther
    prod = prod*farther
  end do
  print *, prod
contains
  function distance(velocity, time)
    integer :: distance
    integer, intent(in) :: velocity, time

    distance = velocity*(time - velocity)
  end function distance
end program main
