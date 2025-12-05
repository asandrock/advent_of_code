program main
  use ftlStringModule
  use ftlArrayIntModule
  use ftlArrayIntAlgorithmsModule
  use iso_fortran_env, only: int64
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'

  integer :: in_unit, n_ranges
  integer(int64) :: j, sep(1)
  !real :: num_fresh
  integer(int64) :: num_fresh
  type(ftlString) :: input
  type(ftlString), allocatable :: lines(:), ranges(:), parts(:)

  integer(int64), allocatable :: range_min(:), range_max(:), endpoints(:)

  open(newunit=in_unit, file=filename)
  call input%readUntilEOF(in_unit)
  close(in_unit)

  lines = input%splitLines()
  do j = 1, size(lines)
    if (len_trim(lines(j)) == 0) sep = j
  end do
  ranges = lines(1:sep(1) - 1)
  n_ranges = size(ranges)
  allocate(range_min(n_ranges), range_max(n_ranges), endpoints(2*n_ranges))
  do j = 1, size(ranges)
    parts = ranges(j)%split('-')
    read (parts(1)%raw, *) range_min(j)
    read (parts(2)%raw, *) range_max(j)
    endpoints(2*j - 1) = range_min(j)
    endpoints(2*j) = range_max(j) + 1
  end do
  range_max = range_max + 1
  call ftlSort(endpoints)

  num_fresh = 0
  do j = 1, 2*n_ranges - 1
    !if (endpoints(j) == endpoints(j + 1)) then
    !  cycle
    !else if (any(endpoints(j) >= range_min .and. endpoints(j + 1) <= range_max)) then
    !print *, 'pair: ', endpoints(j), endpoints(j + 1)
    if (any(endpoints(j) >= range_min .and. endpoints(j + 1) <= range_max)) then
      !print *, 'inside, adding ', endpoints(j + 1) - endpoints(j)
      num_fresh = num_fresh + (endpoints(j + 1) - endpoints(j))
    else
      !print *, 'outside'
    end if
  end do

  print *, num_fresh
end program main
