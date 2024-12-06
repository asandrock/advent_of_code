program main1
  use stringifor
  implicit none
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: lines(:)
  logical, allocatable :: visited(:,:)
  integer :: pos_guard(2), dir_guard(2), j

  call input%read_file(file=filename)
  call input%split(tokens=lines, sep=new_line('a'))
  allocate(visited(size(lines), len(lines(1)%raw)))
  visited = .false.

  print *, input

  call find_guard(pos_guard, dir_guard)
  visited(pos_guard(1), pos_guard(2)) = .true.
  lines(pos_guard(1))%raw(pos_guard(2):pos_guard(2)) = 'X'

  do
    call move(pos_guard, dir_guard)
    if (any(pos_guard > shape(visited)) .or. any(pos_guard <= 0)) then
      exit
    else
      visited(pos_guard(1), pos_guard(2)) = .true.
      lines(pos_guard(1))%raw(pos_guard(2):pos_guard(2)) = 'X'
    end if
  end do
  print *
  do j = 1, size(visited, dim=1)
    print *, lines(j)
  end do
  print *
  print *, count(visited)
contains
  subroutine find_guard(pos, dir)
    integer, intent(out) :: pos(2), dir(2)
    integer :: j, k

    do j = 1, size(lines)
      k = scan(lines(j), '^><v')
      if (k /= 0) then
        pos = [j, k]
        select case (lines(j)%raw(k:k))
        case ('^')
          dir = [-1, 0]
        case ('>')
          dir = [0, 1]
        case ('<')
          dir = [0, -1]
        case ('v')
          dir = [1, 0]
        end select
        exit
      end if
    end do
  end subroutine find_guard

  subroutine move(pos, dir)
    integer, dimension(2), intent(inout) :: pos, dir

    integer :: new_pos(2), right_rotation(2, 2)

    right_rotation(1, :) = [0, 1]
    right_rotation(2, :) = [-1, 0]

    new_pos = pos + dir
    if (any(new_pos > shape(visited)) .or. any(new_pos <= 0)) then
      pos = new_pos
    else if (lines(new_pos(1))%raw(new_pos(2):new_pos(2)) == '#') then
      dir = matmul(right_rotation, dir)
    else
      pos = new_pos
    end if
  end subroutine move
end program main1
