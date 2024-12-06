program main
  use StringiFor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: games(:), subsets(:), cubes(:), ids(:)
  integer :: j, k, red, green, blue, power, sum_powers, current_id

  call input%read_file(filename)
  call input%split(tokens=games, sep=new_line('a'))

  sum_powers = 0
  do j = 1, size(games)
  associate(game=>games(j))
    ! determine game id
    call game%split(tokens=subsets, sep=':')
    call subsets(1)%split(tokens=ids, sep=' ')
    read(ids(2)%raw, *) current_id
    !print *, current_id

    ! separate game into separate draws
    call subsets(2)%split(tokens=cubes, sep=';')
    red = 0; blue = 0; green = 0
    do k = 1, size(cubes)
    associate(cube=>cubes(k))
      red = max(red, get_cube(cube, string('red')))
      blue = max(blue, get_cube(cube, string('blue')))
      green = max(green, get_cube(cube, string('green')))
    end associate
    end do
    power = red*green*blue
    sum_powers = sum_powers + power
    !print *, red, green, blue, power
  end associate
  end do
  print *, sum_powers
contains
  function get_cube(str, cube_name) result(cube_int)
    implicit none
    type(string), intent(in) :: str, cube_name
    integer :: cube_int

    integer :: i
    type(string), allocatable :: components(:), parts(:)


    if (index(str, cube_name) > 0) then
      call str%split(tokens=components, sep=',')
      do i = 1, size(components)
        if (index(components(i), cube_name) > 0) then
          call components(i)%split(tokens=parts, sep=' ')
          read (parts(1)%raw, *) cube_int
        end if
      end do
    else
      cube_int = 0
    end if
  end function
end program main
