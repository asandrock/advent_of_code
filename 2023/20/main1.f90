program main1
  use stringifor
  use ftlHashMapStringModModule
  use iso_fortran_env, only: error_unit, iostat_end
  implicit none

  ! Input file
  character(len=*), parameter :: filename = 'test_input'
  type(string) :: input
  type(string), allocatable :: rows(:)

  call input%read_file(file=filename)
  call input%split(tokens=rows, sep=new_line('a'))

end program main1
