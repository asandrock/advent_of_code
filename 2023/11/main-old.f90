program main
  use StringiFor
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable, dimension(:) :: lines
  character(len=1), allocatable, dimension(:,:) :: obs_map, expand_map
  integer :: j, k, empty_rows, empty_cols

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  allocate(obs_map(lines(1)%len(), size(lines)))
  do j = 1, size(lines)
    do k = 1, lines(1)%len()
      obs_map(k, j) = lines(j)%raw(k:k)
    end do
  end do

  ! Find empty rows and columns
  empty_rows = 0
  do j = 1, size(lines)
    if (all(obs_map(:,j) == '.')) then
      empty_rows = empty_rows + 1
    end if
  end do
  empty_cols = 0
  do k = 1, lines(1)%len()
    if (all(obs_map(k,:) == '.')) then
      empty_cols = empty_cols + 1
    end if
  end do
  print *, empty_rows, empty_cols

  allocate(expand_map(size(obs_map, 1) + empty_cols, size(obs_map, 2) + empty_rows)
  k = 1
  do j = 1, size(lines)
    expand_map(:, k) = obs_map(:, j)
    if (all(obs_map(:, j) == '.')) then
      expand_map(:, k + 1) = obs_map(:, j)
      k = k + 2
    else
      k = k + 1
    end if
  end do
  j = 1
  do k = 1, lines(1)%len()
    ! FIXME
  end do
end program main
