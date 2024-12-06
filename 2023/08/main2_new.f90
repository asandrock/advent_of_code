program main
  use StringiFor
  implicit none
  !character(len=*), parameter :: filename = 'test_input3'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input, pattern
  type(string), dimension(:), allocatable :: lines

  character(len=3), dimension(:), allocatable :: key, left, right, start_keys
  integer :: j, k, n_keys, n_start
  integer, dimension(:), allocatable :: n_steps
  character(len=3) :: old_key, new_key
  character(len=1) :: direction
  integer :: loc(1)

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))

  pattern = lines(1)
  n_keys = size(lines) - 1
  allocate(key(n_keys), left(n_keys), right(n_keys))
  do j = 1, n_keys
    read (lines(j + 1)%raw, '(A3,4X,A3,2X,A3,X)') key(j), left(j), right(j)
  end do

  n_start = count(is_start(key))
  print *, 'Start positions: ', n_start
  allocate(start_keys(n_start))
  k = 1
  do j = 1, n_keys
    if (is_start(key(j))) then
      start_keys(k) = key(j)
      k = k + 1
    end if
  end do

  allocate(n_steps(n_start))
  n_steps = 0
  do j = 1, n_start
    old_key = start_keys(j)
    do
      if (is_end(old_key)) exit
      n_steps(j) = n_steps(j) + 1
      k = modulo(n_steps(j) - 1, pattern%len()) + 1
      direction = pattern%raw(k:k)
      loc = findloc(key, old_key)
      if (direction == 'L') then
        new_key = left(loc(1))
      else
        new_key = right(loc(1))
      end if
      old_key = new_key
    end do
    print *, j, n_steps(j)
  end do
  print *, least_common_multiple(n_steps)
contains
  elemental function is_start(key)
    character(len=3), intent(in) :: key
    logical :: is_start

    is_start = key(3:3) == 'A'
  end function is_start

  elemental function is_end(key)
    character(len=3), intent(in) :: key
    logical :: is_end

    is_end = key(3:3) == 'Z'
  end function is_end

  function least_common_multiple(X0)
    use PENF, only: I8P
    implicit none
    integer, dimension(:), intent(in) :: X0
    integer :: least_common_multiple

    integer(I8P), dimension(size(X0)) :: X
    integer, dimension(1) :: loc_min

    X = X0
    do
      if (all(X == X(1))) exit
      loc_min = minloc(X)
      X(loc_min(1)) = X(loc_min(1)) + X0(loc_min(1))
    end do
    least_common_multiple = X(1)
  end function least_common_multiple
end program main
