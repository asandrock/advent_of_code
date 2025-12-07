program main
  use stringifor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: rows(:), cells(:,:), new_cells(:,:)
  integer :: x, y, rows_number, columns_number, loc(1)
  integer(I8P), allocatable :: timelines(:,:)

  call input%read_file(file=filename)
  call input%split(tokens=rows, sep=new_line('a'))
  rows_number = size(rows)
  columns_number = rows(1)%len()
  allocate(cells(columns_number, rows_number), &
    timelines(columns_number, rows_number), &
    new_cells(columns_number, rows_number))
  do x = 1, columns_number
    do y = 1, rows_number
      cells(x, y) = rows(y)%slice(x, x)
    end do
  end do
  timelines = 0

  ! Start at 'S' in row 1
  loc = rows(1)%scan('S')
  cells(loc(1), 1) = '|'
  new_cells = cells
  timelines(loc(1), 1) = 1

  do y = 2, rows_number
    do x = 1, columns_number
      if (cells(x, y - 1) == '|') then
        if (cells(x, y) == '.') then
          new_cells(x, y) = '|'
          timelines(x, y) = timelines(x, y) + timelines(x, y - 1)
        else if (cells(x, y) == '^') then
          if (x > 1) then
            new_cells(x - 1, y) = '|'
            timelines(x - 1, y) = timelines(x - 1, y) + timelines(x, y - 1)
          end if
          if (x < columns_number) then
            new_cells(x + 1, y) = '|'
            timelines(x + 1, y) = timelines(x + 1, y) + timelines(x, y - 1)
          end if
        end if
      end if
    end do
    cells = new_cells
  end do

  !call print_cells
  !call print_timelines
  print *, sum(timelines(:, rows_number))
contains
  subroutine print_cells()
    implicit none
    integer :: j
    do j = 1, rows_number
      print *, cells(:, j)
    end do
    print *
  end subroutine print_cells

  subroutine print_timelines()
    implicit none
    integer :: j
    do j = 1, rows_number
      print *, timelines(:, j)
    end do
    print *
  end subroutine print_timelines
end program main
