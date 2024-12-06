program main
  use ftlHashMapStringDirectionModule
  use directionModule
  use ftlStringModule
  use iso_fortran_env
  implicit none
  integer :: funit, stat
  character(len=80) :: msg
  !character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'test_input2'
  character(len=*), parameter :: filename = 'input'

  type(ftlHashMapStringDirection) :: hash_map
  type(ftlString) :: pattern, old_key, new_key
  character(len=3) :: key, left, right
  integer :: len_pattern, num_steps, j
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
      !print *, key, left, right
    end if
  end do

  close(unit=funit, status='keep', iostat=stat, iomsg=msg)
  if (stat /= 0) stop msg

  !print *, hash_map%size()

  num_steps = 0
  old_key = 'AAA'
  do
    !print *, old_key
    if (old_key == 'ZZZ') exit
    num_steps = num_steps + 1
    j = modulo(num_steps - 1, len_pattern) + 1
    if (.not. hash_map%has(old_key)) then
      stop 'Stuck, hash_map%has(old_key) == F'
    else
       dir => hash_map%get(old_key)
       if (.not. associated(dir)) stop 'not associated'
    end if
    if (pattern%raw(j:j) == 'L') then
      new_key = dir%left
    else
      new_key = dir%right
    end if
    old_key = new_key
  end do
  print *, num_steps
end program main
