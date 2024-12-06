program test
  use stringifor
  implicit none
  character(len=*), parameter :: filename = "test_input"
  integer :: funit, j, empty_loc(1), num_lines, num_empty, old_loc
  type(string), allocatable :: lines(:)

  open(newunit=funit, file=filename)
  call read_lines(funit, lines)
  close(funit)
  num_lines = size(lines)
  write (*,*) 'Number of lines: ', num_lines
  num_empty = count(lines%len() == 0)
  write (*,*) 'Number of empty lines: ', num_empty

  old_loc = 1
  do j = 1, num_empty
    empty_loc = minloc(lines(old_loc:num_lines)%len())
    write (*,*) empty_loc
    old_loc = empty_loc(1) + 1
  end do
end program test
