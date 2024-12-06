program main
  use stringifor
  implicit none
  character(len=*), parameter :: filename='test_input'
  !character(len=*), parameter :: filename='input'

  type(string) :: input
  type(string), allocatable :: lines(:)
  integer :: first, last, j, num_i, sum_num
  !character(len=4) :: num_c

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))

  sum_num = 0
  do j = 1, size(lines)
    first = find_digit(lines(j))
    last = find_digit(lines(j)%reverse())
    !write (num_c, '(2I1)') first, last
    !read (num_c, '(2I1)') num_i
    num_i = 10*first + last
    print *, num_i
    sum_num = sum_num + num_i
  end do
  write (*,*) sum_num
contains
  function find_digit(str)
    ! find first digit
    integer :: find_digit
    type(string), intent(in) :: str

    integer :: j

    do j = 1, str%len()
      select case (str%raw(j:j))
      case ('0':'9')
        read (str%raw(j:j), '(I1)') find_digit
        return
      end select
    end do
    find_digit = 10
  end function find_digit
end program main
