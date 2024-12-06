program main
  use StringiFor
  use PENF, only: I1P
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: lines(:), body(:), lists(:), numbers(:)

  integer :: j, k, points_per_card, total_points
  integer, allocatable :: winning_numbers(:), numbers_i_have(:)

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  total_points = 0

  do j = 1, size(lines)
    call lines(j)%split(tokens=body, sep=':')
    call body(2)%split(tokens=lists, sep='|')

    call lists(1)%split(tokens=numbers, sep=' ')
    allocate(winning_numbers(size(numbers)))
    do k = 1, size(numbers)
      winning_numbers(k) = numbers(k)%to_number(kind=1_I1P)
    end do

    call lists(2)%split(tokens=numbers, sep=' ')
    allocate(numbers_i_have(size(numbers)))
    do k = 1, size(numbers)
      numbers_i_have(k) = numbers(k)%to_number(kind=1_I1P)
    end do

    points_per_card = 0
    do k = 1, size(winning_numbers)
      if (count(numbers_i_have == winning_numbers(k)) > 0) then
        if (points_per_card == 0) then
          points_per_card = 1
        else
          points_per_card = 2*points_per_card
        end if
      end if
    end do
    total_points = total_points + points_per_card
    deallocate(winning_numbers, numbers_i_have)
  end do
  print *, total_points
end program main
