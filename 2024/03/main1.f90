program main1
  use stringifor
  implicit none
  character(len=*), parameter :: filename = 'input'
  type(string) :: input

  call input%read_file(file=filename)

end program main1
