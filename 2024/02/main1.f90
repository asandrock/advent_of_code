program main1
  use stdlib_string_type
  implicit none
  character(len=*), parameter :: filename = 'test_input'
  type(string_type) :: line
  integer, allocatable :: report(:)
  integer :: funit, ios, safe, j
  character, allocatable :: string(:)
  character(len=:), allocatable :: tmp

  safe = 0
  open(newunit=funit, file=filename, status='old', action='read')
  do
    read (funit, *, iostat=ios) line
    if (ios /= 0) then
      exit
    else
      write (*,*) 'current line: ', line

      allocate(string(len(trim(line))))
      do j = 1, len(line)
        string(j) = char(line, j)
      end do
      allocate(report(count(string == ' ') + 1))
      print *, 'length of report: ', size(report)
      deallocate(string)

      tmp = char(line)
      read (tmp, *) report
      deallocate(tmp)

      print *, 'is safe: ', is_safe(report)
      if (is_safe(report)) then
        safe = safe + 1
      end if

      deallocate(report)
    end if
  end do
  write (*,*) safe
  close(funit)
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
    is_safe = .false.
    if (all(diff > 0) .or. all(diff < 0)) then
      if (all(abs(diff) >= 1) .or. all(abs(diff) <= 3)) then
        is_safe = .true.
      end if
    end if
    deallocate(diff)
  end function is_safe
end program main1
