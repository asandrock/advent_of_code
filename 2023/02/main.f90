program main
  use StringiFor
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: games(:), subsets(:), cubes(:), ids(:)
  integer :: j, k, &
    sum_ids, current_id, red, green, blue
  integer, parameter :: max_red = 12, max_green = 13, max_blue = 14
  logical :: possible

  call input%read_file(filename)
  call input%split(tokens=games, sep=new_line('a'))

  sum_ids = 0
  do j = 1, size(games)
  associate(game=>games(j))
    ! determine game id
    call game%split(tokens=subsets, sep=':')
    call subsets(1)%split(tokens=ids, sep=' ')
    read(ids(2)%raw, *) current_id
    print *, current_id

    ! separate game into separate draws
    possible = .true.
    call subsets(2)%split(tokens=cubes, sep=';')
    do k = 1, size(cubes)
    associate(cube=>cubes(k))
      call get_cube(cube, string('red'), red)
      call get_cube(cube, string('blue'), blue)
      call get_cube(cube, string('green'), green)
      possible = possible .and. &
        red <= max_red .and. blue <= max_blue .and. green <= max_green
      print *, red, ' red, ', blue, ' blue, ', green, ' green:', possible
    end associate
    end do 
    if (possible) sum_ids = sum_ids + current_id
  end associate
  end do
  print *, sum_ids
contains
  subroutine get_cube(str, cube_name, cube_int)
    implicit none
    type(string), intent(in) :: str, cube_name
    integer, intent(out) :: cube_int

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
  end subroutine
end program main
