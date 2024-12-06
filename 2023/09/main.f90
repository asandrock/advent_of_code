program main
  use StringiFor
  use PENF
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: lines(:), words(:)
  integer :: j, k, next, sum_next
  integer, allocatable :: values(:)

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  sum_next = 0
  do j = 1, size(lines)
    call lines(j)%split(tokens=words, sep=' ')
    allocate(values(size(words)))
    do k = 1, size(values)
      values(k) = words(k)%to_number(kind=I4P)
    end do

    call extrapolate(values, next)
    print *, next
    sum_next = sum_next + next

    deallocate(values)
  end do
  print *, '======'
  print *, 'Sum = ', sum_next
contains
  recursive subroutine extrapolate(values, next)
    implicit none
    integer, intent(in) :: values(:)
    integer, intent(out) :: next

    integer, allocatable :: diff(:)
    integer :: j, next_diff

    allocate(diff(size(values) - 1))
    do j = 1, size(values) - 1
      diff(j) = values(j + 1) - values(j)
    end do

    if (all(diff == 0)) then
      next = values(size(values))
    else
      call extrapolate(diff, next_diff)
      next = values(size(values)) + next_diff
    end if

    deallocate(diff)
  end subroutine extrapolate
end program main
