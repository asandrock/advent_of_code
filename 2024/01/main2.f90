program main2
  use stdlib_io, only: loadtxt
  use stdlib_sorting, only: ord_sort
  character(len=*), parameter :: filename = 'input'!'test_input'
  integer, allocatable :: input(:,:), left(:), right(:)
  integer :: similarity_score, j

  call loadtxt(filename, input)
  left = input(:, 1)
  right = input(:, 2)

  similarity_score = 0
  do j = 1, size(left)
    similarity_score = left(j)*count(right == left(j)) + similarity_score
  end do
  write (*,*) similarity_score
end program main2
