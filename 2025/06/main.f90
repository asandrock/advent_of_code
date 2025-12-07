program main
  use stringifor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: rows(:), cells(:,:), tmp(:)
  integer :: j, k, rows_number, columns_number
  integer(I8P) :: grand_total
  integer(I8P), allocatable :: numbers(:,:)

  call input%read_file(file=filename)
  call input%split(tokens=rows, sep=new_line('a'))
  rows_number = size(rows)
  call rows(1)%split(tokens=tmp, sep=' ')
  columns_number = size(tmp)
  allocate(cells(rows_number, columns_number))
  cells(1, :) = tmp
  do j = 2, rows_number
    call rows(j)%split(tokens=tmp, sep=' ')
    cells(j, :) = tmp
  end do

  allocate(numbers(1:rows_number - 1, 1:columns_number))
  do k = 1, columns_number
    do j = 1, rows_number - 1
      numbers(j, k) = cells(j, k)%to_number(kind=I8P)
    end do
  end do

  grand_total = 0
  do j = 1, columns_number
    if (cells(rows_number, j) == '*') then
      grand_total = grand_total + product(numbers(:, j))
    else
      grand_total = grand_total + sum(numbers(:, j))
    end if
  end do
  print *, grand_total
end program main
