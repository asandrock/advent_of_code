program main
  use stdlib_string_type
  use stdlib_strings
  use stdlib_logger, logger=>global_logger
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'input'
  integer :: input, stat
  character(len=80) :: msg

  type(string_type) :: line, elf(2)
  integer :: num_lines, comma, hyphen, ranges(2, 2), j, contained
  character(len=:), allocatable :: number_string

  call logger%configure(level=debug_level)
  call logger%log_information('Advent of Code 2022, Day 4, Puzzle 1')
  open(newunit=input, file=filename, status='old', action='read', iostat=stat, iomsg=msg)
  if (stat /= 0) call logger%log_io_error(filename, iostat=stat, iomsg=msg)

  num_lines = 0
  contained = 0
  do
    read (unit=input, fmt=*, iostat=stat, iomsg=msg) line
    if (is_iostat_end(stat)) exit
    if (stat /= 0) then
      call logger%log_io_error(filename, iostat=stat, iomsg=msg)
      exit
    end if
    num_lines = num_lines + 1

    call logger%log_debug('line = ' // char(line))
    comma = find(line, ',')
    call logger%log_debug('comma = ' // to_string(comma))
    elf(1) = slice(line, 1, comma - 1)
    elf(2) = slice(line, comma + 1)
    do j = 1, 2
      hyphen = find(elf(j), '-')
      call logger%log_debug('hyphen = ' // to_string(hyphen))

      number_string = char(slice(elf(j), 1, hyphen - 1))
      read(unit=number_string, fmt=*) ranges(j, 1)
      deallocate(number_string)

      number_string = char(slice(elf(j), hyphen + 1))
      read(unit=number_string, fmt=*) ranges(j, 2)
      deallocate(number_string)

      call logger%log_debug('Range ' // char(elf(j)) // &
        ' starts at ' // to_string(ranges(j, 1)) // &
        ' and ends at ' // to_string(ranges(j, 2)))
    end do
    ! Testing for containment
    if ((ranges(1,1) <= ranges(2,1) .and. ranges(1,2) >= ranges(2,2)) &
      .or. (ranges(2,1) <= ranges(1,1) .and. ranges(2,2) >= ranges(1,2))) then
      contained = contained + 1
      call logger%log_debug('ranges contained completely')
    end if
  end do

  call logger%log_information('Number of lines read: ' // to_string(num_lines))
    print *, 'Contained assignments = ' // to_string(contained)

  close(unit=input, status='keep', iostat=stat, iomsg=msg)
  if (stat /= 0) call logger%log_io_error(filename, iostat=stat, iomsg=msg)
end program main
