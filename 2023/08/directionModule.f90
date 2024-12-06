module directionModule
  use ftlStringModule
  implicit none
  private
  public :: direction
  type direction
    type(ftlString) :: left, right
  end type direction

  interface direction
    module procedure init_direction_string, init_direction_character
  end interface direction
contains
  function init_direction_string(left, right) result(init_direction)
    implicit none
    type(ftlString), intent(in) :: left, right
    type(direction) :: init_direction

    init_direction%left = left
    init_direction%right = right
  end function init_direction_string

  function init_direction_character(left, right) result(init_direction)
    implicit none
    character(len=*), intent(in) :: left, right
    type(direction) :: init_direction

    init_direction%left = left
    init_direction%right = right
  end function init_direction_character
end module directionModule
