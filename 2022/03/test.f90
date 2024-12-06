program main
  use stringifor
  implicit none

  write (*,*) char_to_int('a'), 1
  write (*,*) char_to_int('z'), 26
  write (*,*) char_to_int('A'), 27
  write (*,*) char_to_int('Z'), 52
  write (*,*)
  write (*,*) int_to_char([1]), 'a'
  write (*,*) int_to_char([26]), 'z'
  write (*,*) int_to_char([27]), 'A'
  write (*,*) int_to_char([52]), 'Z'
contains
  function char_to_int(ch)
    implicit none
    integer :: char_to_int
    character, intent(in) :: ch

    type(string) :: str
    integer :: ascii

    str = ch
    ascii = iachar(str%raw)
    if (str%is_lower()) then
      char_to_int = ascii - iachar('a') + 1
    else ! str%is_upper()
      char_to_int = ascii - iachar('A') + 27
    end if
  end function char_to_int

  function int_to_char(i)
    implicit none
    type(string) :: int_to_char
    integer, intent(in) :: i(1)

    integer :: ascii

    if (i(1) < 27) then
      ascii = i(1) + iachar('a') - 1
    else
      ascii = i(1) + iachar('A') - 27
    end if
    int_to_char = achar(ascii)
  end function int_to_char
end program
