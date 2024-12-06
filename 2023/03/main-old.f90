program main
  use StringiFor
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  !character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: lines(:), characters(:,:)

  integer :: j, k
  logical :: neighbour

  call input%read_file(filename)

  ! Convert text file into character array
  call input%split(tokens=lines, sep=new_line('a'))
  allocate(characters(size(lines), lines(1)%len()))
  do j = 1, size(lines)
    do k = 1, lines(1)%len()
      characters(j, k) = lines(j)%slice(k, k)
    end do
  end do

  ! Go through every line and search for numbers
  do j = 1, size(lines)
    do k = 1, lines(1)%len()
      if (characters(j, k)%is_number()) then
        ! Check surrounding characters for a symbol
        neighbour = check_env_for_symbol(characters, j, k)
      end if
    end do
  end do
contains
  function check_env_for_symbols(characters, j, k) result(neighbour)
    type(string), intent(in) :: characters(:,:)
    integer, intent(in) :: j, k
    logical :: neighbour

    character(len=*), parameter :: symbol = '*=/#+-%$&'

    neighbour = .false.
    if (j == 1) then
      if (k == 1) then
        neighbour = any(characters(j, k:k+1)%scan(symbols) > 0 &
          .or. characters(j, k:k+1)%is_number() &
          .or. characters(j:j+1, k+1)%scan(symbols) > 0 &
          .or. characters(j:j+1, k+1)%is_number() )
      else if (k == dim(characters(2))) then
        neighbour = any(characters(j:j+1, k - 1)%scan(symbols) > 0 &
          .or. characters(j:j+1, k - 1)%is_number() &
          .or. characters(j+1, k)%scan(symbols) > 0 &
          .or. characters(j+1, k)%is_number() )
      else
        neighbour = any(characters(j:j+1, k - 1)%scan(symbols) > 0 &
          .or. characters(j:j+1, k - 1)%is_number() &
          .or. characters(j:j+1, k + 1)%scan(symbols) > 0 &
          .or. characters(j:j+1, k + 1)%is_number() &
          )
      end if
    else if (j == dim(characters, 1)) then
      if (k == 1) then
      else if (k == dim(characters(2))) then
      end if
    else
      neighbour = any(characters(j-1:j+1, k - 1)%scan(symbols) > 0 &
        .or. characters(j-1:j+1, k - 1)%is_number() &
        .or. characters(j-1:j+1, k + 1)%scan(symbols) > 0 &
        .or. characters(j-1:j+1, k + 1)%is_number() &
        .or. characters(j-1, k)%scan(symbols) > 0 &
        .or. characters(j-1, k)%is_number() &
        .or. characters(j+1, k)%scan(symbols) > 0 &
        .or. characters(j+1, k)%is_number() &
        )
    end if
  end function check_env_for_symbols
end program main
