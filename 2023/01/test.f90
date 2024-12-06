program test
  use stringifor
  implicit none
  type(string) :: line
  integer :: f

  line = 'threerznlrhtkjp23mtflmbrzq395three'
  print *, line
  !print *, index(line, '4')
  !print *, index(line, 'nine')
  !print *, index(line, '2', .true.)
  !print *, index(line, 'seven', .true.)
  f = find_digit(line, .true.)
  print *, f
  !f = find_digit(line, .true.)
  !print *, f
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
      print *, j, pos_word, pos_digit
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
        occurrence(j) = str%len() + 1
      end if
    end do
    print *, occurrence
    res = minloc(occurrence)
    find_digit = res(1)
  end function find_digit
end program test
