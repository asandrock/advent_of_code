program main
  use stringifor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: rows(:), cells(:,:)
  integer :: x, y, rows_number, columns_number, loc(1), splits

  call input%read_file(file=filename)
  call input%split(tokens=rows, sep=new_line('a'))
  rows_number = size(rows)
  columns_number = rows(1)%len()
  allocate(cells(columns_number, rows_number))
  do x = 1, columns_number
    do y = 1, rows_number
      cells(x, y) = rows(y)%slice(x, x)
    end do
  end do

  !! Initial state:
  !call print_cells

  ! Start at 'S' in row 1
  loc = rows(1)%scan('S')
  !print *, loc
  cells(loc(1), 1) = '|'
  !call print_cells

  splits = 0
  do y = 2, rows_number
    do x = 1, columns_number
      if (cells(x, y - 1) == '|') then
        if (cells(x, y) == '.') then
          cells(x, y) = '|'
        else if (cells(x, y) == '^') then
          splits = splits + 1
          if (x > 1) cells(x - 1, y) = '|'
          if (x < columns_number) cells(x + 1, y) = '|'
        end if
      end if
    end do
    !call print_cells
  end do

  !print *, count(cells(:, rows_number) == '|')
  print *, splits
contains
  subroutine print_cells()
    implicit none
    integer :: j
    do j = 1, rows_number
      print *, cells(:, j)
    end do
    print *
  end subroutine print_cells
end program main
