program main
  use stringifor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: bank(:)
  integer :: j, k, l, largest, sum_largest, num
  character(len=2) :: num_str

  call input%read_file(file=filename)
  call input%split(tokens=bank, sep=new_line('a'))

  sum_largest = 0
  do j = 1, size(bank, dim=1)
    largest = 0
    do k = 1, bank(j)%len_trim() - 1
      do l = k + 1, bank(j)%len_trim()
        write (num_str, '(2A)') bank(j)%raw(k:k), bank(j)%raw(l:l)
        !print *, '"', num_str, '"'
        read (num_str, '(I2)') num
        !print *, num
        if (num > largest) then
          largest = num
        end if
      end do
    end do
    sum_largest = sum_largest + largest
  end do
  print *, sum_largest
end program
