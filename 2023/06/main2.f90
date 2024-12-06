program main
  use StringiFor
  use fmzm
  use PENF, only: I4P
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input, race_time, race_dist
  type(string), allocatable :: lines(:), max_time(:), min_dist(:)

  integer :: j
  integer :: farther, time, k

  type(im) :: dist

  call fm_set(50)

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  call lines(1)%split(tokens=max_time, sep=' ')
  call lines(2)%split(tokens=min_dist, sep=' ')

  race_time = ''
  race_dist = ''
  do j = 1, size(max_time) - 1
    race_time = race_time // max_time(j + 1)
    race_dist = race_dist // min_dist(j + 1)
  end do
  print *, race_time, race_dist
  time = race_time%to_number(kind=I4P)
  dist = to_im(race_dist%raw)

  farther = 0
  do k = 0, time
    if (distance(to_im(k), to_im(time)) > dist) farther = farther + 1
  end do
  print *, farther
contains
  function distance(velocity, time)
    type(im) :: distance
    type(im), intent(in) :: velocity, time

    distance = velocity*(time - velocity)
  end function distance
end program main
