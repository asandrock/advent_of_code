program main
  use ftlStringModule
  use ftlDynArrayIntModule
  use iso_fortran_env
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  integer :: funit, stat
  character(len=80) :: msg

  type(ftlString) :: input, column
  type(ftlString), allocatable :: lines(:)
  integer :: j, k, x, y
  type(ftlDynArrayInt) :: empty_row, empty_col, galaxy_x, galaxy_y
  character(len=1), dimension(:,:), allocatable :: expand_map
  integer, dimension(:,:), allocatable :: distances

  open(newunit=funit, file=filename, status='old', action='read', &
    iostat=stat, iomsg=msg)
  if (stat /= 0) then
    if (stat /= iostat_end) then
      print *, stat, msg
      stop 'Error reading ' // filename
    end if
  end if

  call input%ReadUntilEOF(funit)

  close(unit=funit, status='keep', iostat=stat, iomsg=msg)
  if (stat /= 0) then
    if (stat /= iostat_end) then
      print *, stat, msg
      stop 'Error reading ' // filename
    end if
  end if

  lines = input%SplitLines()
  !print *, size(lines), len(lines(1))
  call empty_row%New()
  call empty_col%New()
  do j = 1, size(lines)
    if (verify(lines(j), '.') == 0) then
      call empty_row%PushBack(j)
    end if
  end do
  !print *, empty_row%data
  do k = 1, len(lines(1))
    column = lines(1)%raw(k:k)
    do j = 2, size(lines)
      column = column // lines(j)%raw(k:k)
    end do
    if (verify(column, '.') == 0) then
      call empty_col%PushBack(k)
    end if
  end do
  !print *, empty_col%data

  allocate(expand_map(size(lines) + size(empty_row), &
    len(lines(1)) + size(empty_col)))
  call galaxy_x%New()
  call galaxy_y%New()
  expand_map = '.'
  y = 1
  do k = 1, len(lines(1))
    x = 1
    do j = 1, size(lines)
      expand_map(x, y) = lines(j)%raw(k:k)

      if (expand_map(x, y) == '#') then
        call galaxy_x%PushBack(x)
        call galaxy_y%PushBack(y)
      end if

      x = x + 1
      if (any(empty_row%data == j)) then
        x = x + 1
      end if
    end do
    y = y + 1
    if (any(empty_col%data == k)) then
      y = y + 1
    end if
  end do
  !do j = 1, size(expand_map, dim=1)
  !  print *, expand_map(j,:)
  !end do

  !print *, galaxy_x%data
  !print *, galaxy_y%data

  allocate(distances(size(galaxy_x%data), size(galaxy_y%data)))
  distances = 0
  do j = 1, size(galaxy_x) - 1
    do k = j + 1, size(galaxy_y)
      distances(j, k) = abs(galaxy_x%data(k) - galaxy_x%data(j)) &
        + abs(galaxy_y%data(k) - galaxy_y%data(j))
    end do
  end do
  print *, sum(distances)
end program
