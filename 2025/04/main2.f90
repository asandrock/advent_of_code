program main
  use stringifor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: rows(:), cells(:,:), cells2(:,:)
  integer :: row_number, column_number, r, c, neighbour_number, removed_rolls, &
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
  removed_rolls = 0
  do
    cells2 = cells
    do r = 1, row_number
      do c = 1, column_number
        if (cells(r, c) == '@') then
          ! Check for edges and corners
          r_lower = max(1, r - 1)
          r_upper = min(row_number, r + 1)
          c_lower = max(1, c - 1)
          c_upper = min(column_number, c + 1)
 
          neighbour_number = count(cells(r_lower:r_upper, c_lower:c_upper) == '@') - 1
          if (neighbour_number < 4) then
            removed_rolls = removed_rolls + 1
            cells2(r, c) = '.'
          end if
        end if
      end do
    end do
    !print *, 'Existing rolls', count(cells == '@')
    !print *, 'Removed rolls: ', removed_rolls
    if (all(cells == cells2)) exit
    cells = cells2
  end do
  print *, removed_rolls
end program
