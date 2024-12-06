program main
  use StringiFor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable, dimension(:) :: lines

  integer :: n_x, n_y, j, k, i
  character(len=1), allocatable, dimension(:,:) :: platform
  integer, allocatable, dimension(:,:) :: load

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  n_x = size(lines)
  n_y = lines(1)%len()
  print *, 'n_x =', n_x
  print *, 'n_y =', n_y
  allocate(platform(n_x, n_y))
  do k = 1, n_y
    do j = 1, n_x
      platform(j, k) = lines(j)%raw(k:k)
    end do
  end do
  !! platform contains the unperturbed platform
  !print *, 'unperturbed platform'
  !do j = 1, n_x
  !  print *, platform(j, :)
  !end do

  ! roll rocks to the north
  do k = 1, n_y
    do j = 1, n_x
      if (platform(j,k) == 'O') then
        i = j
        do
          if (i == 1 .or. platform(i - 1, k) /= '.') then
            exit
          else
            platform(i - 1, k) = 'O'
            platform(i, k) = '.'
          end if
          i = i - 1
        end do
      end if
    end do
  end do

  !! platform contains the tilted platform
  !print *, 'tilted platform'
  !do j = 1, n_x
  !  print *, platform(j, :)
  !end do

  ! Calculate load
  allocate(load(n_x, n_y))
  load = 0
  do k = 1, n_y
    do j = 1, n_x
      if (platform(j, k) == 'O') then
        load(j, k) = n_y - j + 1
      end if
    end do
  end do
  print *, 'sum(load) = ', sum(load)
end program main
