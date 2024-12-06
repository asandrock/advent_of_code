program main
  use stringifor
  use logger_mod, only: logger_init, logger=>master_logger, debug, info
  use penf, only: i1p
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'

  type(string) :: input
  type(string), allocatable :: lines(:), elves(:), numbers(:)
  character(len=80) :: msg
  integer :: j, k, num_contained, ranges(2, 2)

  !call logger_init('main.log', stdout_threshold=debug)
  call logger_init('main.log', stdout_threshold=info)
  call logger%info('main', 'Advent of Code 2022, Day 4, Puzzle 1')

  call input%read_file(file=filename)
  call input%split(tokens=lines, sep=new_line('a'))

  num_contained = 0
  do j = 1, size(lines)
    call logger%debug('line', 'line = ' // lines(j)%raw)
    call lines(j)%split(tokens=elves, sep=',')
    do k = 1, 2
      call elves(k)%split(tokens=numbers, sep='-')
      ranges(k, 1) = numbers(1)%to_number(i1p)
      ranges(k, 2) = numbers(2)%to_number(i1p)
      write (msg, *) 'Range ', elves(k), ' starts at ', ranges(k, 1), &
        ' and ends at ', ranges(k, 2)
      call logger%debug('elves', msg)
    end do
    if ((ranges(1,1) <= ranges(2,1) .and. ranges(1,2) >= ranges(2,2)) &
      .or. (ranges(2,1) <= ranges(1,1) .and. ranges(2,2) >= ranges(1,2))) then
      num_contained = num_contained + 1
      call logger%debug('line', 'Ranges contained completely')
    end if
  end do

  write (msg, *) 'Number of lines: ', size(lines)
  call logger%info('main', msg)
  write (msg, *) 'Contained assignments: ', num_contained
  call logger%info('main', msg)
end program main
