program main
  use ftlStringModule
  use iso_fortran_env, only: int64
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'

  integer :: in_unit
  integer(int64) :: j, k, sep(1), num_fresh
  type(ftlString) :: input
  type(ftlString), allocatable :: lines(:), ranges(:), available(:), parts(:)

  integer(int64), allocatable :: range_min(:), range_max(:), available_ids(:)
  !logical, allocatable :: fresh_ids(:)

  open(newunit=in_unit, file=filename)
  call input%readUntilEOF(in_unit)
  close(in_unit)

  lines = input%splitLines()
  do j = 1, size(lines)
    if (len_trim(lines(j)) == 0) sep = j
  end do
  ranges = lines(1:sep(1) - 1)
  available = lines(sep(1) + 1:size(lines))

  allocate(range_min(size(ranges)), range_max(size(ranges)))
  do j = 1, size(ranges)
    parts = ranges(j)%split('-')
    read (parts(1)%raw, *) range_min(j)
    read (parts(2)%raw, *) range_max(j)
  end do

  allocate(available_ids(size(available)))
  do j = 1, size(available)
    read (available(j)%raw, *) available_ids(j)
  end do

  !allocate(fresh_ids(min(1, minval(range_min)):max(maxval(available_ids), maxval(range_max))))
  !fresh_ids = .false. ! Everything spoilt unless explicitly fresh
  !do j = 1, size(ranges)
  !  do k = range_min(j), range_max(j)
  !    fresh_ids(k) = .true.
  !  end do
  !end do
  !do j = 1, size(fresh_ids)
  !  print *, j, fresh_ids(j)
  !end do

  num_fresh = 0
  do j = 1, size(available_ids)
  !  if (fresh_ids(available_ids(j))) then
  !    !print *, 'Ingredient ', available_ids(j), ' is fresh'
  !    num_fresh = num_fresh + 1
  !  !else
  !  !  print *, 'Ingredient ', available_ids(j), ' is spoilt'
  !  end if
    if (any(range_min <= available_ids(j) .and. range_max >= available_ids(j))) then
    !  print *, 'Ingredient ', available_ids(j), ' is fresh'
      num_fresh = num_fresh + 1
    !else
    !  print *, 'Ingredient ', available_ids(j), ' is spoilt'
    end if
  end do
  print *, num_fresh
end program main
