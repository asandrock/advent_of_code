module mod_module
  use stringifor
  implicit none
  private
  public :: mod_type, flip_flop_type, conjunction_type

  type mod_type
    type(string), allocatable :: destinations(:)
    logical :: pulse
  end type mod_type

  !type, extends(mod_type) :: broadcaster_type
  !
  !end type broadcaster_type

  type, extends(mod_type) :: flip_flop_type
    logical :: on_off
  end type flip_flop_type

  type, extends(mod_type) :: conjunction_type
    logical :: last_pulse
  end type conjunction_type
contains
  function init_mod_type(destinations) result(t_mod)
    type(mod_type) :: t_mod
    type(string), dimension(:), intent(in) :: destinations

    t_mod%destinations = destinations
  end function init_mod_type
end module mod_module
