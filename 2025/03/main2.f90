program main
  use stringifor
  use logger_mod, only: logger_init, logger => master_logger
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: bank(:)
  integer, allocatable :: bank_int(:)
  integer :: j, l, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12
  integer, dimension(1) :: kk1, kk2, kk3, kk4, kk5, kk6, kk7, kk8, kk9, kk10, kk11, kk12
  integer(kind=I8P) :: largest, sum_largest, num 
  character(len=12) :: num_str
  character(len=80) :: message

  call logger_init(logfile='main2.log')
  call logger%info('main', 'Read input file ' // filename)
  call input%read_file(file=filename)
  call input%split(tokens=bank, sep=new_line('a'))

  sum_largest = 0
  allocate(bank_int(bank(1)%len_trim()))
  do j = 1, size(bank, dim=1)
    write(message, *) 'bank ', j, '/', size(bank, dim=1)
    call logger%info('main', message)

    largest = 0
    do l = 1, bank(j)%len_trim()
      read (bank(j)%raw(l:l), *) bank_int(l)
    end do
    kk1  = maxloc(bank_int(      1:bank(j)%len_trim() - 11)); k1  = kk1 (1)
    kk2  = maxloc(bank_int(k1  + 1:bank(j)%len_trim() - 10)); k2  = kk2 (1) + k1
    kk3  = maxloc(bank_int(k2  + 1:bank(j)%len_trim() -  9)); k3  = kk3 (1) + k2
    kk4  = maxloc(bank_int(k3  + 1:bank(j)%len_trim() -  8)); k4  = kk4 (1) + k3
    kk5  = maxloc(bank_int(k4  + 1:bank(j)%len_trim() -  7)); k5  = kk5 (1) + k4
    kk6  = maxloc(bank_int(k5  + 1:bank(j)%len_trim() -  6)); k6  = kk6 (1) + k5
    kk7  = maxloc(bank_int(k6  + 1:bank(j)%len_trim() -  5)); k7  = kk7 (1) + k6
    kk8  = maxloc(bank_int(k7  + 1:bank(j)%len_trim() -  4)); k8  = kk8 (1) + k7
    kk9  = maxloc(bank_int(k8  + 1:bank(j)%len_trim() -  3)); k9  = kk9 (1) + k8
    kk10 = maxloc(bank_int(k9  + 1:bank(j)%len_trim() -  2)); k10 = kk10(1) + k9
    kk11 = maxloc(bank_int(k10 + 1:bank(j)%len_trim() -  1)); k11 = kk11(1) + k10
    kk12 = maxloc(bank_int(k11 + 1:bank(j)%len_trim()     )); k12 = kk12(1) + k11
    print *, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, bank(j)%len_trim()
    write (num_str, '(12A)') bank(j)%raw(k1:k1), bank(j)%raw(k2:k2), bank(j)%raw(k3:k3), &
      bank(j)%raw(k4:k4), bank(j)%raw(k5:k5), bank(j)%raw(k6:k6), bank(j)%raw(k7:k7), &
      bank(j)%raw(k8:k8), bank(j)%raw(k9:k9), bank(j)%raw(k10:k10), bank(j)%raw(k11:k11), &
      bank(j)%raw(k12:k12)
    read (num_str, '(I12)') num
    if (num > largest) then
      largest = num
    end if
    sum_largest = sum_largest + largest
    print *, largest
  end do
  print *, sum_largest
end program
