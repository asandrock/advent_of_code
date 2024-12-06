module WorkflowModule
  use ftlStringModule
  implicit none
  type Workflow
    type(ftlString) :: name
    type(ftlString), allocatable, dimension(:) :: step
  end type
end module WorkflowModule
