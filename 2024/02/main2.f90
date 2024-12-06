program main1
  use stdlib_io, only: loadtxt
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  integer, allocatable :: input(:,:)

  call loadtxt(filename, input)
end program main1
