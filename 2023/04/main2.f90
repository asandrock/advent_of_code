program main
  use StringiFor
  use PENF, only: I1P
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: lines(:), body(:), lists(:), numbers(:)

  integer :: j, k, matches
  integer, allocatable :: winning_numbers(:), numbers_i_have(:), cards(:)

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  allocate(cards(size(lines)))

  cards = 1
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

    matches = 0
    do k = 1, size(winning_numbers)
      if (count(numbers_i_have == winning_numbers(k)) > 0) then
        matches = matches + 1
      end if
    end do
    print *, 'Card ', j, ' has ', matches, ' matching numbers, number of cards ', cards(j)

    do k = 1, matches
      if (j + k <= size(lines)) then
        cards(j + k) = cards(j + k) + cards(j)
      end if
    end do

    deallocate(winning_numbers, numbers_i_have)
  end do
  print *, sum(cards)
end program main
