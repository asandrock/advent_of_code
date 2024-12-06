program main
  use stringifor
  implicit none
  !character(len=*), parameter :: filename='test_input2'
  !character(len=*), parameter :: filename='test_input'
  character(len=*), parameter :: filename='input'

  type(string) :: input
  type(string), allocatable :: lines(:)
  integer :: first, last, j, num_i, sum_num

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))

  sum_num = 0
  do j = 1, size(lines)
    first = find_digit(lines(j), reverse=.false.)
    last = find_digit(lines(j), reverse=.true.)
    num_i = 10*first + last
    print *, num_i, lines(j)
    sum_num = sum_num + num_i
  end do
  write (*,*) sum_num
contains
  function find_digit(str, reverse)
    ! find first digit
    integer :: find_digit
    type(string), value :: str
    logical, intent(in) :: reverse

    integer :: j, res(1)

    integer :: occurrence(9), pos_digit, pos_word
    type(string) :: num_word(9)
    character(len=1) :: num_digit(9)

    num_word(1) = 'one'
    num_word(2) = 'two'
    num_word(3) = 'three'
    num_word(4) = 'four'
    num_word(5) = 'five'
    num_word(6) = 'six'
    num_word(7) = 'seven'
    num_word(8) = 'eight'
    num_word(9) = 'nine'

    do j = 1, 9
      write(num_digit(j), '(I1)') j
    end do

    occurrence = 0
    do j = 1, 9
      pos_word = str%index(num_word(j), reverse)
      pos_digit = str%index(num_digit(j), reverse)
      if (pos_word > 0 .and. pos_digit > 0) then
        if (reverse) then
          occurrence(j) = max(pos_word, pos_digit)
        else
          occurrence(j) = min(pos_word, pos_digit)
        end if
      else if (pos_word > 0 .and. pos_digit == 0) then
        occurrence(j) = pos_word
      else if (pos_word == 0 .and. pos_digit > 0) then
        occurrence(j) = pos_digit
      else
        occurrence(j) = 0
      end if
    end do
    if (reverse) then
      res = maxloc(occurrence, mask=occurrence > 0)
    else
      res = minloc(occurrence, mask=occurrence > 0)
    end if
    find_digit = res(1)
  end function find_digit
end program main
