program main1
  use stringifor
  implicit none
  character(len=*), parameter :: filename = 'input'!'test_input'
  type(string) :: input, antennae, current
  type(string), allocatable :: lines(:)
  character(len=1), allocatable :: map(:,:), map2(:,:)
  logical, allocatable :: antinode(:,:)
  integer :: j, k, l, m, pos(2)
  integer, allocatable :: positions(:,:), vec(:)

  call input%read_file(file=filename)

  ! determine antennae, put them all into one string
  antennae = ''
  do j = 1, input%len()
    current = input%slice(j, j)
    if (current /= '.') then ! current symbol is antenna
      if (antennae%scan(current) == 0) then ! antenna not yet in list
        antennae = antennae // current
      end if
    end if
  end do
  !print *, antennae

  ! turn input multi-line string into 2D array of characters
  call input%split(tokens=lines, sep=new_line('a'))
  allocate(map(size(lines), lines(1)%len()), map2(size(lines), lines(1)%len()))
  do j = 1, size(map, dim=1)
    do k = 1, size(map, dim=2)
      map(j, k) = lines(j)%slice(k, k)
    end do
  end do

  ! determine antinodes
  allocate(antinode(size(lines), lines(1)%len()))
  antinode = .false.
  do j = 1, antennae%len()
    current = antennae%slice(j, j)
    if (count(map == current%raw) > 1) then
      ! pairs of antennae exist, determine all positions
      allocate(positions(count(map == current%raw), 2))
      m = 1
      do k = 1, size(lines)
        do l = 1, lines(1)%len()
          if (map(k, l) == current%raw) then
            positions(m, 1) = k
            positions(m, 2) = l
            antinode(k, l) = .true.
            m = m + 1
          end if
        end do
      end do

      ! Check each pair of antennae
      do k = 1, size(positions, dim=1) - 1
        do l = k + 1, size(positions, dim=1)
          vec = positions(k,:) - positions(l,:)

          m = 1
          do
            pos = positions(l,:) + m*vec
            if (all(pos > 0) .and. all(pos <= shape(map))) then
              antinode(pos(1), pos(2)) = .true.
            else
              exit
            end if
            m = m + 1
          end do

          m = 1
          do
            pos = positions(l,:) - m*vec
            if (all(pos > 0) .and. all(pos <= shape(map))) then
              antinode(pos(1), pos(2)) = .true.
            else
              exit
            end if
            m = m + 1
          end do
        end do
      end do
      deallocate(positions)
    end if
  end do

  where (antinode)
    map2 = '#'
  elsewhere
    map2 = map
  end where

  !do j = 1, size(antinode, dim=1)
  !  print *, map2(j, :)
  !end do
  print *, count(antinode)
end program main1
