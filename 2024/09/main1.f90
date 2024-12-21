program main1
  use stringifor
  implicit none
  character(len=*), parameter :: filename = 'input'
  type(string) :: input, system, n_block
  character(len=80) :: id
  integer(kind=i4p) :: j, k, l, length
  integer(kind=i4p) :: checksum
  character(len=1), dimension(:), allocatable :: aux

  integer, allocatable, dimension(:) :: file_system

  call input%read_file(file=filename)
  !input = '12345'
  !input = '2333133121414131402'

  ! decode input
  system = ''
  do j = 1, input%len()
    write (unit=id, fmt=*) (j - 1)/2
    n_block = input%slice(j, j)
    if (mod(j, 2) == 1) then
      system = system // repeat(trim(adjustl(id)) // ',', n_block%to_number(kind=1_i4p))
    else
      system = system // repeat('.,', n_block%to_number(kind=1_i4p))
    end if
  end do

  !print *, system
  length = system%len()
  write (*,*) length
  allocate(aux(length))
  do j = 1, length
    aux(j) = system%slice(j, j)
  end do
  write (*,*) count(aux == ',')

  allocate(file_system(count(aux == ',')))
  l = 1
  do j = 1, input%len()
    n_block = input%slice(j, j)
    if (mod(j, 2) == 1) then
      do k = 1, n_block%to_number(kind=1_i4p)
        file_system(l) = (j - 1)/2
        l = l + 1
      end do
    else
      do k = 1, n_block%to_number(kind=1_i4p)
        file_system(l) = -1
        l = l + 1
      end do
    end if
  end do

  ! defragmentation
  do
    do j = 1, size(file_system)
      if (file_system(j) == -1) exit
    end do
    do k = size(file_system), 1, -1
      if (file_system(k) /= -1) exit
    end do
    if (j > k) exit

    file_system(j) = file_system(k)
    file_system(k) = -1
    !print *, file_system
  end do

  ! calculate checksum
  checksum = 0
  do j = 1, size(file_system) - count(file_system == -1)
    !if (file_system(j) == -1) exit
    checksum = checksum + (j - 1)*file_system(j)
  end do
  print *, checksum
end program main1
