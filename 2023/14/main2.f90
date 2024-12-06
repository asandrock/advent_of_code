program main
  use StringiFor
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable, dimension(:) :: lines

  integer :: n_x, n_y, j, k, l
  character(len=1), allocatable, dimension(:,:) :: platform
  integer, allocatable, dimension(:,:) :: load
  !integer, parameter :: n_cycles = 4
  integer, parameter :: n_cycles = 1000000000

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

  do l = 1, n_cycles
    call roll_north
    call roll_west
    call roll_south
    call roll_east
    !print *, 'After ', l, ' cycles:'
    !do j = 1, n_x
    !  print *, platform(j, :)
    !end do
  end do 

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
contains
  subroutine roll_north()
    integer :: i
    ! roll rocks to the north
    do k = 1, n_y
      do j = 1, n_x
        if (platform(j,k) == 'O') then
          i = j
          do
            if (i == 1) exit
            if (platform(i - 1, k) /= '.') exit
            platform(i - 1, k) = 'O'
            platform(i, k) = '.'
            i = i - 1
          end do
        end if
      end do
    end do
  end subroutine roll_north

  subroutine roll_south()
    integer :: i
    ! roll rocks to the south
    do k = 1, n_y
      do j = n_x, 1, -1
        if (platform(j,k) == 'O') then
          i = j
          do
            if (i == n_x) exit
            if (platform(i + 1, k) /= '.') exit
            platform(i + 1, k) = 'O'
            platform(i, k) = '.'
            i = i + 1
          end do
        end if
      end do
    end do
  end subroutine roll_south

  subroutine roll_west()
    integer :: i
    ! roll rocks to the west
    do k = 1, n_y
      do j = 1, n_x
        if (platform(j,k) == 'O') then
          i = k
          do
            if (i == 1 .or. platform(j, i - 1) /= '.') then
              exit
            else
              platform(j, i - 1) = 'O'
              platform(j, i) = '.'
            end if
            i = i - 1
          end do
        end if
      end do
    end do
  end subroutine roll_west

  subroutine roll_east()
    integer :: i
    ! roll rocks to the east
    do k = n_y, 1, -1
      do j = 1, n_x
        if (platform(j,k) == 'O') then
          i = k
          do
            if (i == n_y) exit
            if (platform(j, i + 1) /= '.') exit
            platform(j, i + 1) = 'O'
            platform(j, i) = '.'
            i = i + 1
          end do
        end if
      end do
    end do
  end subroutine roll_east
end program main
