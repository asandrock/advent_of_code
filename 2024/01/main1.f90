program main1
  use stdlib_io, only: loadtxt
  use stdlib_sorting, only: ord_sort
  character(len=*), parameter :: filename = 'input'!'test_input'
  integer, allocatable :: input(:,:), left(:), right(:)

  call loadtxt(filename, input)
  left = input(:, 1)
  right = input(:, 2)

  call ord_sort(left)
  call ord_sort(right)

  write (*,*) sum(abs(right - left))
end program main1
