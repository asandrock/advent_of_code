program main2
  use stringifor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: rows(:), cells(:,:), columns(:), operation(:)
  integer :: rows_number, columns_number, j, k, last
  integer(I8P) :: grand_total
  integer(I8P), allocatable :: numbers(:)

  call input%read_file(file=filename)
  call input%split(tokens=rows, sep=new_line('a'))
  rows_number = size(rows)
  columns_number = rows(1)%len()
  allocate(cells(rows_number, columns_number), columns(columns_number))
  do k = 1, columns_number
    do j = 1, rows_number
      cells(j, k) = rows(j)%slice(k, k)
    end do
  end do

  deallocate(rows)
  do k = 1, columns_number
    columns(k) = ''
    do j = 1, rows_number
      columns(k) = columns(k) // cells(j, k)
    end do
  end do

  allocate(rows(count(cells == '+') + count(cells == '*')))
  allocate(operation(size(rows)))
  do j = 1, size(rows)
    rows(j) = ''
  end do

  j = 1
  do k = columns_number, 1, -1
    if (columns(k)%len_trim() == 0) then
      last = rows(j)%len()
      operation(j) = rows(j)%slice(last, last)
      rows(j) = trim(rows(j)%slice(1, last - 1))
      j = j + 1
    else
      rows(j) = rows(j) // columns(k)
    end if
  end do
  last = rows(j)%len()
  operation(j) = rows(j)%slice(last, last) ! last operation, since there is no empty line at the end
  rows(j) = trim(rows(j)%slice(1, last - 1))

  grand_total = 0
  do j = 1, size(rows)
    call rows(j)%split(tokens=columns, sep=' ')
    allocate(numbers(size(columns)))
    do k = 1, size(columns)
      numbers(k) = columns(k)%to_number(kind=I8P)
    end do
    !print *, numbers, operation(j)
    if (operation(j) == '*') then
      grand_total = grand_total + product(numbers)
      !print *, product(numbers)
    else if (operation(j) == '+') then
      grand_total = grand_total + sum(numbers)
      !print *, sum(numbers)
    else
      print *, 'Error: unknown operation', j, operation(j)
    end if
    deallocate(numbers)
  end do
 
  print *, grand_total
end program main2
