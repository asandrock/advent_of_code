program main
  use stringifor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  integer :: dial, n_lines, j, k, direction, distance, n_zero
  type(string) :: input
  type(string), allocatable :: lines(:)
  character :: dir

  call input%read_file(file=filename)
  call input%split(tokens=lines, sep=new_line('a'))
  n_lines = size(lines, dim=1)

  dial = 50
  n_zero = 0
  do j = 1, n_lines
    read (lines(j)%raw, '(A,I4)') dir, distance
    if (dir == 'L') then
      direction = -1
    else if (dir == 'R') then
      direction = +1
    else
      stop 'Unknown direction'
    end if

    do k = 1, distance
      dial = modulo(dial + direction, 100)
      if (dial == 0) then
        n_zero = n_zero + 1
      end if
    end do
  end do

  print *, 'n_zero: ', n_zero
end program main
