program main
  use stringifor
  use penf
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input

  type(string), allocatable :: id_ranges(:), first_last(:)
  integer :: n_id_ranges, j, pattern_length
  integer(kind=I8P) :: first, last, id, length, sum_invalid_ids
  type(string) :: id_str
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
      id_str = s

      do pattern_length = 1, length/2
        ! Check length of pattern fits into string
        if (modulo(length, pattern_length) /= 0) cycle

        ! Produce copies and compare
        if (id_str == repeat(s(1:pattern_length), length/pattern_length)) then
          !print *, '** invalid id: ', id
          sum_invalid_ids = sum_invalid_ids + id
          exit
        end if
      end do
    end do
  end do

  print *, 'sum of invalid ids: ', sum_invalid_ids
end program main
