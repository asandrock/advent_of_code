program least_common_multiple
  implicit none
  integer, dimension(*), parameter :: X0 = [3, 4, 6]
  integer, dimension(size(X0)) :: X
  integer, dimension(1) :: loc_min

  X = X0
  do
    print *, X
    if (all(X == X(1))) exit
    loc_min = minloc(X)
    X(loc_min(1)) = X(loc_min(1)) + X0(loc_min(1))
  end do
end program least_common_multiple
