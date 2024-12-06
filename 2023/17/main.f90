program main
  use StringiFor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable, dimension(:) :: lines
  integer, allocatable, dimension(:,:) :: layout
  integer :: x, y, n_x, n_y

  integer, parameter :: n_samples = int(1e8)
  integer :: n, heat_loss, min_heat_loss

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  n_x = lines(1)%len()
  n_y = size(lines)
  !print *, n_x, n_y
  allocate(layout(n_x, n_y))
  do x = 1, n_x
    do y = 1, n_y
      read (unit=lines(y)%raw(x:x), fmt='(I1)') layout(x, y)
    end do
  end do

  min_heat_loss = sample_trajectory(n_x, n_y)
  do n = 2, n_samples
    heat_loss = sample_trajectory(n_x, n_y)
    if (heat_loss < min_heat_loss) min_heat_loss = heat_loss
  end do
  print *, min_heat_loss
contains
  function sample_trajectory(n_x, n_y) result(heat_loss)
    integer :: heat_loss
    integer, intent(in) :: n_x, n_y

    integer :: steps_same_direction
    integer, parameter :: max_steps = 3
    integer, dimension(2) :: pos, dir, new_dir
    integer, dimension(2), parameter :: right = [1, 0], left = [-1, 0], &
      up = [0, -1], down = [0, 1]

    integer :: n_directions, rand_int

    heat_loss = 0
    pos = [1, 1]
    n_directions = 2
    rand_int = random_integer(n_directions)
    if (rand_int == 1) then
      new_dir = right
    else
      new_dir = down
    end if
    steps_same_direction = 1

    do
      !print *, pos, new_dir
      if (any(pos <= 0) .or. any(pos > [n_x, n_y])) stop 'Outside the map'
      pos = pos + new_dir
      dir = new_dir
      heat_loss = heat_loss + layout(pos(1), pos(2))
      if (all(pos == [n_x, n_y])) exit

      if (all(dir == right)) then
        if (pos(1) == n_x .or. steps_same_direction == max_steps) then
          if (pos(2) == 1) then
            new_dir = down
          else if (pos(2) == n_y) then
            new_dir = up
          else
            rand_int = random_integer(2)
            select case (rand_int)
            case (1)
              new_dir = down
            case (2)
              new_dir = up
            end select
          end if
        else
          if (pos(2) == 1) then
            rand_int = random_integer(2)
            select case (rand_int)
            case (1)
              new_dir = right
            case (2)
              new_dir = down
            end select
          else if (pos(2) == n_y) then
            rand_int = random_integer(2)
            select case (rand_int)
            case (1)
              new_dir = right
            case (2)
              new_dir = up
            end select
          else
            rand_int = random_integer(3)
            select case (rand_int)
            case (1)
              new_dir = right
            case (2)
              new_dir = up
            case (3)
              new_dir = down
            end select
          end if
        end if
      else if (all(dir == left)) then
        if (pos(1) == 1 .or. steps_same_direction == max_steps) then
          if (pos(2) == 1) then
            new_dir = down
          else if (pos(2) == n_y) then
            new_dir = up
          else
            rand_int = random_integer(2)
            select case (rand_int)
            case (1)
              new_dir = down
            case (2)
              new_dir = up
            end select
          end if
        else
          if (pos(2) == 1) then
            rand_int = random_integer(2)
            select case (rand_int)
            case (1)
              new_dir = left
            case (2)
              new_dir = down
            end select
          else if (pos(2) == n_y) then
            rand_int = random_integer(2)
            select case (rand_int)
            case (1)
              new_dir = left
            case (2)
              new_dir = up
            end select
          else
            rand_int = random_integer(3)
            select case (rand_int)
            case (1)
              new_dir = left
            case (2)
              new_dir = up
            case (3)
              new_dir = down
            end select
          end if
        end if
      else if (all(dir == down)) then
        if (pos(2) == n_y .or. steps_same_direction == max_steps) then
          if (pos(1) == 1) then
            new_dir = right
          else if (pos(1) == n_x) then
            new_dir = left
          else
            rand_int = random_integer(2)
            select case (rand_int)
            case (1)
              new_dir = right
            case (2)
              new_dir = left
            end select
          end if
        else
          if (pos(1) == 1) then
            rand_int = random_integer(2)
            select case (rand_int)
            case (1)
              new_dir = right
            case (2)
              new_dir = down
            end select
          else if (pos(1) == n_x) then
            rand_int = random_integer(2)
            select case (rand_int)
            case (1)
              new_dir = left
            case (2)
              new_dir = down
            end select
          else
            rand_int = random_integer(3)
            select case (rand_int)
            case (1)
              new_dir = right
            case (2)
              new_dir = left
            case (3)
              new_dir = down
            end select
          end if
        end if
      else if (all(dir == up)) then
        if (pos(2) == 1 .or. steps_same_direction == max_steps) then
          if (pos(1) == 1) then
            new_dir = right
          else if (pos(1) == n_x) then
            new_dir = left
          else
            rand_int = random_integer(2)
            select case (rand_int)
            case (1)
              new_dir = right
            case (2)
              new_dir = left
            end select
          end if
        else
          if (pos(1) == 1) then
            rand_int = random_integer(2)
            select case (rand_int)
            case (1)
              new_dir = right
            case (2)
              new_dir = up
            end select
          else if (pos(1) == n_x) then
            rand_int = random_integer(2)
            select case (rand_int)
            case (1)
              new_dir = left
            case (2)
              new_dir = up
            end select
          else
            rand_int = random_integer(3)
            select case (rand_int)
            case (1)
              new_dir = right
            case (2)
              new_dir = left
            case (3)
              new_dir = up
            end select
          end if
        end if
      end if

      if (all(dir == new_dir)) then
        steps_same_direction = steps_same_direction + 1
      else
        steps_same_direction = 1
      end if
    end do
  end function sample_trajectory

  function random_integer(n_max)
    integer :: random_integer
    integer, intent(in) :: n_max

    real :: random_real

    call random_number(random_real)
    random_integer = 1 + floor(n_max*random_real)
  end function random_integer
end program main
