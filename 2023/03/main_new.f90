program main_new
  use StringiFor
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'input'
  type(string) :: input, number_string
  type(string), allocatable :: lines(:)
  character(len=1), dimension(:,:), allocatable :: schematic
  logical, dimension(:,:), allocatable :: num, sym, num_neighbour_sym
  integer :: n_lines, n_chars, j, k
  character(len=*), parameter :: numbers = '0123456789'
  logical :: swapped

  call input%read_file(filename)
  call input%split(tokens=lines, sep=new_line('a'))
  n_lines = size(lines)
  n_chars = lines(1)%len()
  print *, 'Pad by one square on every side, avoids edge cases when searching neighbours'
  allocate(schematic(n_chars + 2, n_lines + 2), num(n_chars + 2, n_lines + 2), &
    sym(n_chars + 2, n_lines + 2))
  schematic = ' '
  num = .false.
  sym = .false.
  do k = 1, n_lines
    do j = 1, n_chars
      schematic(j + 1, k + 1) = lines(k)%raw(j:j)
      num(j + 1, k + 1) = (scan(schematic(j, k), numbers) /= 0)
      sym(j + 1, k + 1) = .not. num(j,k) .and. (scan(schematic(j,k), '.') == 0)
    end do
  end do

  print *, 'Iterate through all fields to look for numbers in the neighbourhood of symbols'
  allocate(num_neighbour_sym(n_chars + 2, n_lines + 2))
  num_neighbour_sym = .false.
  do k = 2, n_lines + 1
    do j = 2, n_chars + 1
      if (sym(j, k) .and. any(num(j-1:j+1, k-1:k+1))) then
        num_neighbour_sym(j-1:j+1, k-1:k+1) = num(j-1:j+1, k-1:k+1)
      end if
    end do
  end do

  print *, 'Now all numbers with neighbouring symbol have at least one digit marked'
  print *, 'Check iteratively for other digits in the same line and enter into num_neighbour_sym'
  do
    swapped = .false.
    do k = 2, n_lines + 1
      do j = 2, n_chars + 1
        if (num_neighbour_sym(j, k)) then
          if (num(j-1, k) .or. num(j + 1, k)) then
            num_neighbour_sym(j-1:j+1, k) = num(j-1:j+1, k)
            swapped = .true.
          end if
        end if
      end do
    end do
    if (.not. swapped) exit
  end do

  do k = 2, n_lines + 1
    print *, schematic(2, n_chars + 1)
    print *, num_neighbour_sym(2, n_chars + 1)
    print *
  end do
end program main_new
