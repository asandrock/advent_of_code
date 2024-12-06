program main
  use StringiFor
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable, dimension(:) :: lines

  integer :: n_x, n_y, x, y
  character(len=1), dimension(:,:), allocatable :: layout
  logical, dimension(:,:), allocatable :: energized

  integer, dimension(2), parameter :: up = [0, -1], down = [0, 1], &
    left = [-1, 0], right = [1, 0]

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  n_x = lines(1)%len()
  n_y = size(lines)
  allocate(layout(n_x, n_y), energized(n_x, n_y))

  energized = .false.
  do y = 1, n_y
    do x = 1, n_x
      layout(x, y) = lines(y)%raw(x:x)
    end do
  end do

  call beam([1, 1], right)

  do y = 1, n_y
    print *, layout(:,y), '  ', energized(:,y)
  end do

  print *, count(energized)
contains
  recursive subroutine beam(pos, direction)
    implicit none
    integer, intent(in) :: pos(2), direction(2)

    integer :: x, y, new_dir(2)

    x = pos(1)
    y = pos(2)
    new_dir = 0
    if (x >=1 .and. x <= n_x .and. y >= 1 .and. y <= n_y) then
      if (layout(x, y) == '.') then ! empty space
        call beam(pos + direction, direction)
      else if (layout(x, y) == '/') then ! mirror
        if (all(direction == up)) then
          new_dir = right
        else if (all(direction == down)) then
          new_dir = left
        else if (all(direction == right)) then
          new_dir = up
        else if (all(direction == left)) then
          new_dir = down
        else
          stop 'Unknown direction'
        end if
        call beam(pos + new_dir, new_dir)
      else if (layout(x, y) == '\') then ! mirror
        if (all(direction == up)) then
          new_dir = left
        else if (all(direction == down)) then
          new_dir = right
        else if (all(direction == right)) then
          new_dir = down
        else if (all(direction == left)) then
          new_dir = up
        else
          stop 'Unknown direction'
        end if
        call beam(pos + new_dir, new_dir)
      else if (layout(x, y) == '-') then ! splitter
        if (all(direction == left) .or. all(direction == right)) then
          call beam(pos + direction, direction)
        else if(all(direction == up) .or. all(direction == down)) then ! splitting
          new_dir = right
          call beam(pos + new_dir, new_dir)
          new_dir = left
          call beam(pos + new_dir, new_dir)
        else
          stop 'Unknown direction'
        end if
      else if (layout(x, y) == '|') then ! splitter
        if (all(direction == up) .or. all(direction == down)) then
          call beam(pos + direction, direction)
        else if (all(direction == left) .or. all(direction == right)) then ! splitting
          new_dir = up
          call beam(pos + new_dir, new_dir)
          new_dir = down
          call beam(pos + new_dir, new_dir)
        else
          stop 'Unknown direction'
        end if
      else
        stop 'Unknown layout part'
      end if
      energized(x, y) = .true.
    else
    !  print *, 'outside layout'
      return
    end if
  end subroutine beam
end program main
