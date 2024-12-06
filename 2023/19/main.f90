program main
  use ftlStringModule
  use WorkflowModule
  use ftlHashMapStringWorkflowModule
  use PartModule
  use ftlListPartModule
  use iso_fortran_env, only: iostat_end
  implicit none
  !character(len=*), parameter :: filename = 'test_input'
  character(len=*), parameter :: filename = 'input'
  integer :: funit, stat
  character(len=80) :: msg
  type(ftlString) :: input
  type(ftlString), allocatable, dimension(:) :: lines
  integer :: j, summa

  type(ftlListPart) :: part_list, accepted, rejected
  type(ftlHashMapStringWorkflow) :: workflow_map
  type(part) :: current_part

  open(newunit=funit, file=filename, status='old', action='read', &
    iostat=stat, iomsg=msg)
  if (stat /= 0) then
    print *, msg
    stop filename
  end if

  call input%ReadUntilEOF(funit)

  close(unit=funit, status='keep', iostat=stat, iomsg=msg)
  if (stat /= 0) then
    print *, msg
    stop filename
  end if

  lines = input%Split()
  call part_list%new()
  call workflow_map%new(16)
  call accepted%new()
  call rejected%new()

  do j = 1, size(lines)
    if (lines(j)%raw(1:1) == '{') then
      call read_part(lines(j))
    else
      call read_workflow(lines(j))
    end if
  end do

  !print *, size(workflow_map), size(part_list)

  do
    if (part_list%Begin() == part_list%End()) exit
    current_part = part_list%PopFront()
    call go_through_workflow(current_part)
  end do

  summa = 0
  do
    if (accepted%Begin() == accepted%End()) exit
    current_part = accepted%PopFront()
    summa = summa + current_part%a + current_part%x + current_part%s + current_part%m
  end do
  print *, summa
contains
  subroutine go_through_workflow(in_part)
    type(part), intent(in) :: in_part

    type(ftlString) :: current_key
    type(workflow), pointer :: current_workflow
    integer :: j, variable, limit
    type(ftlString), allocatable, dimension(:) :: items, it2

    current_key = 'in'
    outer: do
      !print *, current_key
      current_workflow => workflow_map%Get(current_key)
      inner: do j = 1, size(current_Workflow%step)
        associate (step=>current_workflow%step(j))
        !print *, step
        if (step == 'A') then
          call accepted%PushBack(in_part)
          exit outer
        else if (step == 'R') then
          call rejected%PushBack(in_part)
          exit outer
        else ! parse
          if ('>' .in. step) then
            items = step%split('>')
            !print *, items(1)
            !print *, items(2)
            select case (items(1)%raw)
            case('x')
              variable = in_part%x
            case('m')
              variable = in_part%m
            case('a')
              variable = in_part%a
            case('s')
              variable = in_part%s
            end select
            it2 = items(2)%split(':')
            limit = int(it2(1))
            if (variable > limit) then
              if (it2(2) == 'A') then
                call accepted%PushBack(in_part)
                exit outer
              else if (it2(2) == 'R') then
                call rejected%PushBack(in_part)
                exit outer
              else
                current_key = it2(2)
                exit inner
              end if
            end if
          else if ('<' .in. step) then
            items = step%split('<')
            !print *, items(1)
            !print *, items(2)
            select case (items(1)%raw)
            case('x')
              variable = in_part%x
              !print *, 'x'
            case('m')
              variable = in_part%m
              !print *, 'm'
            case('a')
              variable = in_part%a
              !print *, 'a'
            case('s')
              variable = in_part%s
              !print *, 's'
            end select
            it2 = items(2)%split(':')
            limit = int(it2(1))
            !print *, limit
            !print *, it2(2)
            if (variable < limit) then
              if (it2(2) == 'A') then
                call accepted%PushBack(in_part)
                exit outer
              else if (it2(2) == 'R') then
                call rejected%PushBack(in_part)
                exit outer
              else
                current_key = it2(2)
                !print *, current_key
                exit inner
              end if
            end if
          else
            current_key = step
            !print *, current_key
            exit inner
          end if
        end if
        end associate
      end do inner
    end do outer
  end subroutine go_through_workflow

  subroutine read_workflow(string)
    type(ftlString), intent(in) :: string

    type(ftlString), allocatable, dimension(:) :: items, it2
    type(Workflow) :: new_workflow
    integer :: j

    items = string%split('{')
    items(2) = items(2)%Replace('}', '')
    it2 = items(2)%Split(sep=',')

    new_workflow%name = items(1)
    allocate(new_workflow%step(size(it2)))
    do j = 1, size(it2)
      new_workflow%step(j) = it2(j)
    end do
    call workflow_map%Set(new_workflow%name, new_workflow)
  end subroutine read_workflow

  subroutine read_part(string)
    type(ftlString), intent(inout) :: string

    type(part) :: new_part
    type(ftlString), allocatable, dimension(:) :: items, it2
    integer :: j

    string = string%Replace('{', '')
    string = string%Replace('}', '')
    items = string%split(sep=',')
    do j = 1, size(items)
      it2 = items(j)%split(sep='=')
      select case (it2(1)%raw)
      case ('x')
        new_part%x = int(it2(2))
      case ('m')
        new_part%m = int(it2(2))
      case ('a')
        new_part%a = int(it2(2))
      case ('s')
        new_part%s = int(it2(2))
      end select
    end do
    call part_list%PushBack(new_part)
  end subroutine read_part
end program main
