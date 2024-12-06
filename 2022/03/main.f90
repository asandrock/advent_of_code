program main
  use stringifor
  use logger_mod, only: logger_init, logger=>master_logger, debug, info
  implicit none
  !character(len=*), parameter :: input_file_name = 'test_input'
  character(len=*), parameter :: input_file_name = 'input'
  type(string) :: input, num_str
  type(string), allocatable :: rucksacks(:)
  type(string) :: compartment(2)
  integer :: num_rucksacks, length, j, k, sum_priorities
  integer :: items(2, 52), common_item(1)

  !call logger_init('main.log', stdout_threshold=debug)
  call logger_init('main.log', stdout_threshold=info)

  call input%read_file(file=input_file_name)
  call input%split(tokens=rucksacks, sep=new_line('a'))
  num_rucksacks = size(rucksacks); num_str = num_rucksacks
  call logger%debug('main', 'Number of rucksacks: ' // num_str)

  sum_priorities = 0
  do j = 1, num_rucksacks
    length = rucksacks(j)%len()
    compartment(1) = rucksacks(j)%slice(1, length/2)
    compartment(2) = rucksacks(j)%slice(length/2 + 1, length)
    items = 0
    do k = 1, compartment(1)%len() ! Compartments have the same length
      items(1, char_to_int(compartment(1)%slice(k, k))) = 1
      items(2, char_to_int(compartment(2)%slice(k, k))) = 1
    end do
    common_item = maxloc(items(1,:), mask=(items(1,:) == items(2,:)))
    call logger%debug('rucksack', 'Common item: ' // &
      int_to_char(common_item))
    sum_priorities = sum_priorities + common_item(1)
  end do
  num_str = sum_priorities
  call logger%info('main', 'Sum of priorities: ' // num_str)
contains
  function char_to_int(ch)
    implicit none
    integer :: char_to_int
    character, intent(in) :: ch

    type(string) :: str
    integer :: ascii

    str = ch
    ascii = iachar(str%raw)
    if (str%is_lower()) then
      char_to_int = ascii - iachar('a') + 1
    else ! str%is_upper()
      char_to_int = ascii - iachar('A') + 27
    end if
  end function char_to_int

  function int_to_char(i)
    implicit none
    type(string) :: int_to_char
    integer, intent(in) :: i(1)

    integer :: ascii

    if (i(1) < 27) then
      ascii = i(1) + iachar('a') - 1
    else
      ascii = i(1) + iachar('A') - 27
    end if
    int_to_char = achar(ascii)
  end function int_to_char
end program
