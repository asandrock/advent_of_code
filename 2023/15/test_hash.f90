program test_hash
  use hash_mod
  implicit none
  !character(len=*), parameter :: test = 'HASH'
  character(len=*), parameter :: test = 'ot=7' // new_line('a')

  print *, hash(test)
  print *, hash(test)
  print *, hash(test)
  print *, hash(test)
end program test_hash
