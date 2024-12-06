program main
  use stringifor
  implicit none
  !character(len=*), parameter :: input_filename = "test_input"
  character(len=*), parameter :: input_filename = "input"

  integer, parameter :: &
    fail = 0, draw = 3, win = 6, &
    rock = 1, paper = 2, scissors = 3

  type(string) :: input
  type(string), allocatable :: rounds(:), shapes(:)
  integer :: j, num_rounds, outcome, shape_score, total_score

  call input%read_file(file=input_filename)
  call input%split(tokens=rounds, sep=new_line('a'))
  num_rounds = size(rounds)

  total_score = 0
  do j = 1, num_rounds
    call rounds(j)%split(tokens=shapes, sep=' ')

    select case (shapes(1)%chars())
    case ('A') ! Rock
      select case (shapes(2)%chars())
      case ('X') ! need to lose
        shape_score = scissors
        outcome = fail
      case ('Y') ! need to draw
        shape_score = rock
        outcome = draw
      case ('Z') ! need to win
        shape_score = paper
        outcome = win
      end select
    case ('B') ! Paper
      select case (shapes(2)%chars())
      case ('X') ! need to lose
        shape_score = rock
        outcome = fail
      case ('Y') ! need to draw
        shape_score = paper
        outcome = draw
      case ('Z') ! need to win
        shape_score = scissors
        outcome = win
      end select
    case ('C') ! Scissors
      select case (shapes(2)%chars())
      case ('X') ! need to lose
        shape_score = paper
        outcome = fail
      case ('Y') ! need to draw
        shape_score = scissors
        outcome = draw
      case ('Z') ! need to win
        shape_score = rock
        outcome = win
      end select
    end select
    total_score = total_score + shape_score + outcome
  end do
  write (*,*) total_score
end program main
