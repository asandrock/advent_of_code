program main2
  use stringifor
  implicit none
  character(len=*), parameter :: filename = 'input'
  type(string) :: input
  type(string), allocatable :: lines(:), cells(:)
  integer, allocatable :: report(:)
  integer :: j, safe

  call input%read_file(file=filename)
  call input%split(tokens=lines, sep=new_line('a'))

  safe = 0
  do j = 1, size(lines)
    call lines(j)%split(tokens=cells, sep=' ')
    allocate(report(size(cells)))
    report = cells%to_number(kind=1)
    if (is_safe(report)) then
      safe = safe + 1
    end if
    deallocate(report)
  end do
  write (*,*) safe
contains
  function is_safe(rep)
    logical :: is_safe
    integer, intent(in) :: rep(:)

    integer, allocatable :: rep2(:)
    integer :: j

    logical, allocatable :: dampened(:)

    is_safe = is_safe_deeper(rep)
    if (.not. is_safe) then ! Problem Dampener
      allocate(dampened(size(rep)), rep2(size(rep) - 1))
      do j = 1, size(rep)
        if (j == 1) then
          rep2 = rep(2:)
        else if (j == size(rep)) then
          rep2 = rep(:size(rep2))
        else
          rep2(1:j - 1) = rep(1:j - 1)
          rep2(j:size(rep2)) = rep(j + 1: size(rep))
        end if
        dampened(j) = is_safe_deeper(rep2)
      end do
      is_safe = any(dampened)
    end if
  end function is_safe

  function is_safe_deeper(rep)
    logical :: is_safe_deeper
    integer, intent(in) :: rep(:)

    integer, allocatable :: diff(:)
    integer :: j

    allocate(diff(size(rep) - 1))
    do j = 1, size(diff)
      diff(j) = rep(j + 1) - rep(j)
    end do
    is_safe_deeper = all(sign(diff, diff(1)) == diff) .and. &
    & all(abs(diff) >= 1 .and. abs(diff) <= 3)
    deallocate(diff)
  end function is_safe_deeper
end program main2
