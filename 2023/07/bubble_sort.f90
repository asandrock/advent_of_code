program bubble_sort
  implicit none
  integer, dimension(8) :: liste = [8,7,6,5,4,3,2,1]![5,3,1,6,7,2,4,8]

  integer :: j, k, tmp

  print *, 'Eingabeliste ', liste

  do j = size(liste), 2, - 1
    do k = 1, j - 1
      if (liste(k) > liste(k + 1)) then
        tmp = liste(k + 1)
        liste(k + 1) = liste(k)
        liste(k) = tmp
      end if
    end do
  end do

  print *, 'Ausgabeliste ', liste
end program bubble_sort
