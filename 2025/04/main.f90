program main
  use stringifor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: rows(:), cells(:,:)
  integer :: row_number, column_number, r, c, neighbour_number, accessible_rolls, &
    c_upper, c_lower, r_upper, r_lower

  ! File I/O and parsing
  call input%read_file(file=filename)
  call input%split(tokens=rows, sep=new_line('a'))
  row_number = size(rows, dim=1)
  column_number = rows(1)%len()
  allocate(cells(row_number, column_number))
  do r = 1, row_number
    do c = 1, column_number
      cells(r, c) = rows(r)%slice(c, c)
    end do
  end do

  ! Iterate through cells and calculate number of neighbouring rolls
  accessible_rolls = 0
  do r = 1, row_number
    do c = 1, column_number
      if (cells(r, c) == '@') then
        ! Check for edges and corners
        r_lower = max(1, r - 1)
        r_upper = min(row_number, r + 1)
        c_lower = max(1, c - 1)
        c_upper = min(column_number, c + 1)

        neighbour_number = count(cells(r_lower:r_upper, c_lower:c_upper) == '@') - 1
        if (neighbour_number < 4) accessible_rolls = accessible_rolls + 1
      end if
    end do
  end do
  print *, accessible_rolls
end program
