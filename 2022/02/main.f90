program main
  use stringifor
  implicit none
  !character(len=*), parameter :: input_filename = "test_input"
  character(len=*), parameter :: input_filename = "input"
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
      case ('X') ! Rock
        shape_score = 1
        outcome = 3 ! draw
      case ('Y') ! Paper
        shape_score = 2
        outcome = 6 ! win
      case ('Z') ! Scissors
        shape_score = 3
        outcome = 0 ! fail
      end select
    case ('B') ! Paper
      select case (shapes(2)%chars())
      case ('X') ! Rock
        shape_score = 1
        outcome = 0 ! fail
      case ('Y') ! Paper
        shape_score = 2
        outcome = 3 ! draw
      case ('Z') ! Scissors
        shape_score = 3
        outcome = 6 ! win
      end select
    case ('C') ! Scissors
      select case (shapes(2)%chars())
      case ('X') ! Rock
        shape_score = 1
        outcome = 6 ! win
      case ('Y') ! Paper
        shape_score = 2
        outcome = 0 ! fail
      case ('Z') ! Scissors
        shape_score = 3
        outcome = 3 ! draw
      end select
    end select
    total_score = total_score + shape_score + outcome
  end do
  write (*,*) total_score
end program main
