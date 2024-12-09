program main1
  use stringifor
  implicit none
  character(len=*), parameter :: filename = 'input'
  type(string) :: input, system, n_block, n_free
  character(len=80) :: id
  type(string), allocatable :: file_blocks(:)
  integer :: j, k, checksum

  call input%read_file(file=filename)

  ! decode input
  system = ''
  do j = 0, input%len()/2 - 1
    write (unit=id, fmt=*) j
    n_block = input%slice(2*j + 1, 2*j + 1)
    n_free = input%slice(2*j + 2, 2*j + 2)
    system = system // repeat(trim(adjustl(id)) // ',' , n_block%to_number(kind=1_i1p)) &
    & // repeat('.,', n_free%to_number(kind=1_I1P))
  end do
  call system%split(tokens=file_blocks, sep=',')
  !print *, file_blocks

  ! defragmentation
  do
    do j = 1, size(file_blocks) ! left-most empty space
      if (file_blocks(j) == '.') then
        exit
      end if
    end do
    do k = size(file_blocks), 1, -1 ! right-most file block
      if (file_blocks(k) /= '.') then
        exit
      end if
    end do
    !print *, j, k
    if (j > k) then
      exit
    else
      file_blocks(j) = file_blocks(k)
      file_blocks(k) = '.'
    end if
    !print *, file_blocks
  end do

  ! calculate checksum
  checksum = 0
  do j = 0, size(file_blocks)
    if (file_blocks(j) == '.') then
      exit
    else
      checksum = checksum + (j - 1)*file_blocks(j)%to_number(kind=1_I1P)
    end if
  end do
  print *, checksum
end program main1
