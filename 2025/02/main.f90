program main
  use stringifor
  use penf
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'input'
  type(string) :: input

  type(string), allocatable :: id_ranges(:), first_last(:)
  integer :: n_id_ranges, j
  integer(kind=I8P) :: first, last, id, length, sum_invalid_ids
  type(string) :: first_half, second_half
  character(len=20) :: s

  call input%read_file(file=filename)
  call input%split(tokens=id_ranges, sep=',')
  n_id_ranges = size(id_ranges, dim=1)

  sum_invalid_ids = 0
  do j = 1, n_id_ranges
    call id_ranges(j)%split(tokens=first_last, sep='-')
    read (first_last(1)%raw, *) first
    read (first_last(2)%raw, *) last
    !print *, 'first id = ', first, ', last_id = ', last

    do id = first, last
      write (s, '(I0)') id
      length = len_trim(s)
      if (modulo(length, 2) /= 0) cycle
      first_half = s(1:length/2)
      second_half = s(length/2 + 1:length)
      !print *, '* id = ', id, ', first half = ', first_half, ', second half = ', second_half

      ! Test validity of id
      if (first_half == second_half) then
        !print *, '** invalid id: ', id
        sum_invalid_ids = sum_invalid_ids + id
      end if
    end do
  end do

  print *, 'sum of invalid ids: ', sum_invalid_ids
end program main
