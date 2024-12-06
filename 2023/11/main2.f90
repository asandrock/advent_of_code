program main
  use ftlStringModule
  use ftlDynArrayIntModule
  use iso_fortran_env
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'input'
  integer :: funit, stat
  character(len=80) :: msg

  type(ftlString) :: input, column
  type(ftlString), allocatable :: lines(:)
  integer :: j, k
  type(ftlDynArrayInt) :: empty_row, empty_col, galaxy_x, galaxy_y
  integer, dimension(:,:), allocatable :: distances

  integer, parameter :: times = 2!100!0000

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
  call galaxy_x%New()
  call galaxy_y%New()
  do j = 1, size(lines)
    if (verify(lines(j), '.') == 0) then
      call empty_row%PushBack(j)
    else
      do k = 1, len(lines(j))
        if (lines(j)%raw(k:k) == '#') then
          call galaxy_x%PushBack(j)
          call galaxy_y%PushBack(k)
        end if
      end do
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
  !print *, galaxy_x%data
  !print *, galaxy_y%data

  allocate(distances(size(galaxy_x%data), size(galaxy_y%data)))
  distances = 0
  do j = 1, size(galaxy_x) - 1
    do k = j + 1, size(galaxy_y)
      distances(j, k) = &
          abs(galaxy_x%data(k) - galaxy_x%data(j)) &
          + (times - 1)*count(empty_col%data > min(galaxy_x%data(j), galaxy_x%data(k)) &
          .and. empty_col%data < max(galaxy_x%data(j), galaxy_x%data(k))) &
        + abs(galaxy_y%data(k) - galaxy_y%data(j)) &
          + (times - 1)*count(empty_row%data > min(galaxy_y%data(j), galaxy_y%data(k)) &
          .and. empty_row%data < max(galaxy_y%data(j), galaxy_y%data(k)))
    end do
  end do
  print *, sum(distances)
end program
