program test
  use stdlib_string_type
  use stdlib_strings
  implicit none
  type(string_type) :: string
  integer :: io, stat

  open(newunit=io, file='test_input')
  do
    !read(io, *, iostat=stat) string
    read(io, *, iostat=stat) string
    print *, string
    if (is_iostat_end(stat)) exit
  end do
  close(io)
end program test
