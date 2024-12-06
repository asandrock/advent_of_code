program main
  use ftlStringModule
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'input'
  type(ftlString) :: input
  type(ftlString), allocatable, dimension(:) :: lines
  integer :: funit, stat
  character(len=80) :: msg

  integer :: n_x, n_y, j, k
  character(len=1), allocatable, dimension(:,:) :: platform

  open(newunit=funit, file=filename, iostat=stat, iomsg=msg, &
    status='old', action='write')
  if (stat /= 0) then
    print *, filename
    print *, stat
    print *, msg
    stop
  end if
  call input%readUntilEOF(funit)
  close(unit=funit, status='keep', iostat=stat, iomsg=msg)
  if (stat /= 0) then
    print *, filename
    print *, stat
    print *, msg
    stop
  end if

  lines = input%splitLines()
  n_x = size(lines)
  n_y = len(lines(1))
  print *, 'n_x =', n_x
  print *, 'n_y =', n_y
  allocate(platform(n_x, n_y))
  do k = 1, n_y
    do j = 1, n_x
      platform(j, k) = lines(j)%raw(k:k)
    end do
  end do
  ! platform contains the unperturbed platform

  print *, 'original state of platform'
  do j = 1, n_x
    print *, platform(j, :)
  end do
end program main
