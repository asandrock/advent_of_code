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
  integer :: j, k, num_overlap, starts(2), stops(2)

  !call logger_init('main.log', stdout_threshold=debug)
  call logger_init('main.log', stdout_threshold=info)
  call logger%info('main', 'Advent of Code 2022, Day 4, Puzzle 2')

  call input%read_file(file=filename)
  call input%split(tokens=lines, sep=new_line('a'))

  num_overlap = 0
  do j = 1, size(lines)
    call logger%debug('line', 'line = ' // lines(j)%raw)
    call lines(j)%split(tokens=elves, sep=',')
    do k = 1, 2
      call elves(k)%split(tokens=numbers, sep='-')
      starts(k) = numbers(1)%to_number(i1p)
      stops(k) = numbers(2)%to_number(i1p)
      write (msg, *) 'Range ', elves(k), ' starts at ', starts(k), &
        ' and ends at ', stops(k)
      call logger%debug('elves', msg)
    end do
    if ((starts(2) <= stops(1) .and. stops(2) >= stops(1)) &
      .or. (starts(1) <= stops(2) .and. stops(1) >= stops(2))) then
      num_overlap = num_overlap + 1
      call logger%debug('line', 'Ranges overlap')
    end if
  end do

  write (msg, *) 'Number of lines: ', size(lines)
  call logger%info('main', msg)
  write (msg, *) 'Contained assignments: ', num_overlap
  call logger%info('main', msg)
end program main
