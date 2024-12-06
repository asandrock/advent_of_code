program main
  use StringiFor
  use PENF, only: I1P
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable, dimension(:) :: lines, parts
  character(len=1), allocatable, dimension(:) :: direction
  integer, allocatable, dimension(:) :: distance
  character(len=7), allocatable, dimension(:) :: color
  integer :: n_lines, j

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  n_lines = size(lines)
  allocate(direction(n_lines), color(n_lines), distance(n_lines))
  do j = 1, n_lines
    call lines(j)%split(tokens=parts, sep=' ')
    direction(j) = parts(1)%raw
    distance(j) = parts(2)%to_number(kind=I1P)
    color(j) = parts(3)%raw(2:8)
  end do

  
end program main
