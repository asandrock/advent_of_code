program main
  use ftlHashMapStringDirectionModule
  use directionModule
  use ftlStringModule
  use iso_fortran_env
  implicit none
  integer :: funit, stat
  character(len=80) :: msg
  character(len=*), parameter :: filename = 'test_input3'
  !character(len=*), parameter :: filename = 'input'

  type(ftlHashMapStringDirection) :: hash_map
  type(ftlString) :: pattern, starting, new_key
  type(ftlString), allocatable :: start_key(:), old_key(:)
  character(len=3) :: key, left, right
  integer :: len_pattern, num_steps, j, k
  type(direction), pointer :: dir
  character(len=300) :: raw_pattern

  open(newunit=funit, file=filename, status='old', action='read', &
    iostat=stat, iomsg=msg)
  if (stat /= 0) stop msg

  read (funit, *) raw_pattern
  pattern = trim(raw_pattern)
  len_pattern = len(pattern)
  !print *, len_pattern

  call hash_map%new(1)
  starting = ''
  do
    read (unit=funit, fmt='(A3,4X,A3,2X,A3,X)', iostat=stat, iomsg=msg) &
      key, left, right
    if (stat /= 0) then
      if (stat == IOSTAT_END) then
        exit
      else
        stop msg
      end if
    else
      call hash_map%Set(key, direction(left, right))
      if (ends_with_A(ftlString(key))) then
        !print *, key, ' is a start'
        starting = starting // key
      end if
    end if
  end do

  close(unit=funit, status='keep', iostat=stat, iomsg=msg)
  if (stat /= 0) stop msg

  num_steps = 0
  allocate(start_key(len(starting)/3), old_key(len(starting)/3))
  !print *, size(start_key), ' starting positions'
  do j = 0, size(start_key) - 1
    start_key(j + 1) = starting%raw(3*j+1:3*j+3)
    !print *, start_key(j + 1)
  end do

  old_key = start_key
  do
    !print *, old_key
    if (all_end_with_Z(old_key)) exit
    num_steps = num_steps + 1
    j = modulo(num_steps - 1, len_pattern) + 1
    do k = 1, size(old_key)
      if (.not. hash_map%has(old_key(k))) then
        stop 'Stuck, hash_map%has(old_key(k)) == F'
      else
         dir => hash_map%get(old_key(k))
         if (.not. associated(dir)) stop 'not associated'
      end if
      if (pattern%raw(j:j) == 'L') then
        new_key = dir%left
      else
        new_key = dir%right
      end if
      old_key(k) = new_key
    end do
  end do
  print *, num_steps
contains
  function ends_with_A(string)
    type(ftlString) :: string
    logical :: ends_with_A

    ends_with_A = (string%raw(3:3) == 'A')
  end function ends_with_A

  function ends_with_Z(string)
    type(ftlString) :: string
    logical :: ends_with_Z

    ends_with_Z = (string%raw(3:3) == 'Z')
  end function ends_with_Z

  function all_end_with_Z(strings)
    type(ftlString), intent(in) :: strings(:)
    logical :: all_end_with_Z

    integer :: k
    logical, dimension(size(strings)) :: mask

    do k = 1, size(strings)
      mask(k) = strings(k)%raw(3:3) == 'Z'
    end do
    all_end_with_Z = all(mask)
  end function all_end_with_Z
end program main
