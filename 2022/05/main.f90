program main
  use stringifor
  implicit none
  character(len=*), parameter :: filename='test_input'
  !character(len=*), parameter :: filename='input'
  type(string), allocatable :: lines(:)
  integer :: funit, empty_line(1), j

  open(newunit=funit, file=filename)
  call read_lines(funit, lines)
  close(funit)
  empty_line = findloc(lines%len(), 0)
  print *, empty_line
  print *, size(lines)
  print *, new_line('a')
  do j = 1, size(lines)
    print *, lines(j)%len()
  end do
end program main
