program main
  use stringifor
  use penf
  implicit none
  !character(len=*), parameter :: input_filename = "test_input"
  character(len=*), parameter :: input_filename = "input"
  type(string), allocatable :: lines(:)

  type :: elf
    integer, allocatable :: calories(:)
    integer :: total
  end type elf
  type(elf), allocatable :: elves(:)

  integer :: funit, j, k, &
    num_elves, num_lines, num_items, &
    empty_location(1), old_location, &
    max_calories(3)
  integer, allocatable :: lengths(:)

  open(newunit=funit, file=input_filename)
  call read_lines(funit, lines)
  close(funit)

  num_lines = size(lines)
  allocate(lengths(num_lines))
  lengths = lines%len()
  num_elves = count(lengths == 0) + 1
  !write (*,*) 'Number of elves: ', num_elves
  allocate(elves(num_elves))

  old_location = 0
  do j = 1, num_elves
    empty_location = findloc(lengths(old_location + 1:num_lines), 0)
    if (empty_location(1) /= 0) then
      num_items = empty_location(1) - 1
    else
      num_items = num_lines - old_location
    end if
    !write (*,*) 'Number of items: ', num_items
    allocate(elves(j)%calories(num_items))
    do k = 1, num_items
      elves(j)%calories(k) = lines(old_location + k)%to_number(kind=1_I4P)
    end do
    elves(j)%total = sum(elves(j)%calories)
    old_location = old_location + empty_location(1)
  end do

  max_calories(1) = maxval(elves%total)
  ! Puzzle 1: Find top elf by number of calories
  write (*,*) 'Maximum number of calories: ', max_calories(1)
  write (*,*) ' found at elf number ', maxloc(elves%total)

  ! Puzzle 2: Find top three elves by number of calories
  max_calories(2) = maxval(elves%total, mask=(elves%total < max_calories(1)))
  write (*,*) 'Second largest number of calories: ', max_calories(2)
  write (*,*) ' found at elf number ', maxloc(elves%total, mask=(elves%total < max_calories(1)))

  max_calories(3) = maxval(elves%total, mask=(elves%total < max_calories(2)))
  write (*,*) 'Second largest number of calories: ', max_calories(3)
  write (*,*) ' found at elf number ', maxloc(elves%total, mask=(elves%total < max_calories(2)))

  write (*,*) 'Total number of calories of top three elves: ', sum(max_calories)
end program main
