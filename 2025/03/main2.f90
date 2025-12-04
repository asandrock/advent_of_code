program main
  use stringifor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: bank(:)
  integer :: j, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12
  integer(kind=I8P) :: largest, sum_largest, num 
  character(len=12) :: num_str

  call input%read_file(file=filename)
  call input%split(tokens=bank, sep=new_line('a'))

  sum_largest = 0
  do j = 1, size(bank, dim=1)
    largest = 0
    do k1 = 1, bank(j)%len_trim() - 11
    do k2 = k1 + 1, bank(j)%len_trim() - 10
    do k3 = k2 + 1, bank(j)%len_trim() - 9
    do k4 = k3 + 1, bank(j)%len_trim() - 8
    do k5 = k4 + 1, bank(j)%len_trim() - 7
    do k6 = k5 + 1, bank(j)%len_trim() - 6
    do k7 = k6 + 1, bank(j)%len_trim() - 5
    do k8 = k7 + 1, bank(j)%len_trim() - 4
    do k9 = k8 + 1, bank(j)%len_trim() - 3
    do k10 = k9 + 1, bank(j)%len_trim() - 2
    do k11 = k10 + 1, bank(j)%len_trim() - 1
    do k12 = k11 + 1, bank(j)%len_trim()
      !print *, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, bank(j)%len_trim()
      write (num_str, '(12A)') bank(j)%raw(k1:k1), bank(j)%raw(k2:k2), bank(j)%raw(k3:k3), &
        bank(j)%raw(k4:k4), bank(j)%raw(k5:k5), bank(j)%raw(k6:k6), bank(j)%raw(k7:k7), &
        bank(j)%raw(k8:k8), bank(j)%raw(k9:k9), bank(j)%raw(k10:k10), bank(j)%raw(k11:k11), &
        bank(j)%raw(k12:k12)
      read (num_str, '(I12)') num
      if (num > largest) then
        largest = num
      end if
    end do
    end do
    end do
    end do
    end do
    end do
    end do
    end do
    end do
    end do
    end do
    end do
    sum_largest = sum_largest + largest
  end do
  print *, sum_largest
end program
