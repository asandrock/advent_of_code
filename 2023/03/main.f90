program main
  use StringiFor
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'input'
  type(string) :: input

  type(string), allocatable :: lines(:)
  character(len=*), parameter :: numbers = '0123456789'
  integer :: j, pos_num, pos_non, num_numbers

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))

  ! Find numbers
  do j = 1, size(lines)
    num_numbers = 0
    pos_num = scan(lines(j), numbers)
    if (pos_num > 0) then
      num_numbers = 1
      do
        pos_non = verify(lines(j)%raw(pos_num:), numbers)
        if (pos_non == 0) then
          ! No other non-number characters, last number of this line
          exit
        else
          pos_num = scan(lines(j)%raw(pos_num + pos_non:), numbers)
          if (pos_num == 0) then
            ! No other number characters
            exit
          else
            num_numbers = num_numbers + 1
          end if
        end if
      end do
    end if
    if (num_numbers > 0) then
      print *, j, 'Found ', num_numbers, ' numbers'
    end if
  end do
end program
