program main
  use StringiFor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), dimension(:), allocatable :: lines
  character(len=1), dimension(:,:), allocatable :: map
  integer :: n_x, n_y, x_start, y_start, x, y, n_steps
  integer, dimension(2) :: pos1, pos2, dir1, dir2
  logical :: north, south, west, east

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  n_x = len(lines(1)%raw)
  n_y = size(lines)
  allocate(map(n_x + 2, n_y + 2))
  !print *, n_x, n_y

  ! Fill map amd find starting position
  map = ' '
  do y = 1, n_y
    do x = 1, n_x
      map(x, y) = lines(y)%raw(x:x)
      if (map(x, y) == 'S') then
        x_start = x
        y_start = y
      end if
    end do
  end do
  !print *, x_start, y_start

  ! Find out connectivity of start and replace map accodingly
  north = verify(map(x_start, y_start - 1), '7|F') == 0
  south = verify(map(x_start, y_start + 1), 'J|L') == 0
  west = verify(map(x_start - 1, y_start), '-LF') == 0
  east = verify(map(x_start + 1, y_start), '-J7') == 0
  if (north .and. south) then
    map(x_start, y_start) = '|'
    dir1 = [0, -1]; dir2 = [0, 1]
  else if (east .and. west) then
    map(x_start, y_start) = '-'
    dir1 = [1, 0]; dir2 = [-1, 0]
  else if (north .and. east) then
    map(x_start, y_start) = 'L'
    dir1 = [0, -1]; dir2 = [1, 0]
  else if (north .and. west) then
    map(x_start, y_start) = 'J'
    dir1 = [0, -1]; dir2 = [-1, 0]
  else if (south .and. west) then
    map(x_start, y_start) = '7'
    dir1 = [0, 1]; dir2 = [-1, 0]
  else if (south .and. east) then
    map(x_start, y_start) = 'F'
    dir1 = [0, 1]; dir2 = [1, 0]
  else
    stop 'Start does not have two connectors'
  end if
  !print *, map(x_start, y_start)

  ! Starting point
  pos1 = [x_start, y_start]
  pos2 = [x_start, y_start]
  n_steps = 0
  do
    !print *, pos1, dir1, pos2, dir2
    call move(pos1, dir1)
    call move(pos2, dir2)
    n_steps = n_steps + 1
    if (all(pos1 == pos2)) exit
  end do
  print *, n_steps
contains
  subroutine move(pos, dir)
    integer, dimension(2), intent(inout) :: pos, dir

    pos = pos + dir
    if (dir(1) == 0 .and. dir(2) == 1) then
      if (map(pos(1), pos(2)) == '|') then
        dir = [0, 1]
      else if (map(pos(1), pos(2)) == 'J') then
        dir = [-1, 0]
      else if (map(pos(1), pos(2)) == 'L') then
        dir = [1, 0]
      else
        print *, pos, dir
        stop 'Cannot determine new direction'
      end if
    else if (dir(1) == 0 .and. dir(2) == -1) then
      if (map(pos(1), pos(2)) == '|') then
        dir = [0, -1]
      else if (map(pos(1), pos(2)) == '7') then
         dir = [-1, 0]
      else if (map(pos(1), pos(2)) == 'F') then
        dir = [1, 0]
      else
        print *, pos, dir
        stop 'Cannot determine new direction'
      end if
    else if (dir(1) == 1 .and. dir(2) == 0) then
      if (map(pos(1), pos(2)) == '-') then
        dir = [1, 0]
      else if (map(pos(1), pos(2)) == 'J') then
        dir = [0, -1]
      else if (map(pos(1), pos(2)) == '7') then
        dir = [0, 1]
      else
        print *, pos, dir
        stop 'Cannot determine new direction'
      end if
    else if (dir(1) == -1 .and. dir(2) == 0) then
      if (map(pos(1), pos(2)) == '-') then
        dir = [-1, 0]
      else if (map(pos(1), pos(2)) == 'F') then
        dir = [0, 1]
      else if (map(pos(1), pos(2)) == 'L') then
        dir = [0, -1]
      else
        print *, pos, dir
        stop 'Cannot determine new direction'
      end if
    end if
  end subroutine move
end program main
