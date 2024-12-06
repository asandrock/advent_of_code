program test
  use ftlStringModule
  implicit none
  type(ftlString) :: string
  type(ftlString) :: array_string(2)
  integer :: j

  string = 'Hello world'
  print *, string

  array_string = ['Hello', 'world']
  do j = 1, 2
    print *, array_string(j)
  end do

  print *, 'len = ', len(string)
  do j = 1, 2*len(string)
    print *, modulo(j - 1, len(string)) + 1
  end do
end program test
