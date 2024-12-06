module hash_mod
  implicit none
contains
  function hash(string)
    integer :: hash
    character(len=*), intent(in) :: string

    integer :: j

    hash = 0
    do j = 1, len(string)
      if (string(j:j) /= new_line('a')) then
        hash = hash + iachar(string(j:j))
        hash = hash*17
        hash = modulo(hash, 256)
      end if
    end do
  end function hash
end module hash_mod
