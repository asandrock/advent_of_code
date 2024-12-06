program main1
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

    integer, allocatable :: diff(:)
    integer :: j

    allocate(diff(size(rep) - 1))
    do j = 1, size(diff)
      diff(j) = rep(j + 1) - rep(j)
    end do
    is_safe = all(sign(diff, diff(1)) == diff) .and. &
    & all(abs(diff) >= 1 .and. abs(diff) <= 3)
    deallocate(diff)
  end function is_safe
end program main1
