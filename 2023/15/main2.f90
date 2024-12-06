program main
  use StringiFor
  use hash_mod
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), dimension(:), allocatable :: parts
  integer :: j, summa, term

  call input%read_file(filename)
  call input%split(tokens=parts, sep=',')
  summa = 0
  do j = 1, size(parts)
    term = hash(parts(j)%raw)
    !write (*, '(A,2X,2I4)') parts(j)%raw, parts(j)%len(), term
    summa = summa + term
  end do
  !write (*,*) '-------------'
  write (*,*) summa
end program main
