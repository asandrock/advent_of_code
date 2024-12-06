module pulse_module
  use stringifor
  implicit none
  private
  public :: flip_flop, conjunction, broadcaster

  type flip_flop
    type(string) :: mod_name
    type(string), dimension(:), allocatable :: destinations
    logical :: state
  contains
    procedure :: output
  end type flip_flop

  type conjunction
    type(string) :: mod_name
    type(string), dimension(:), allocatable :: destinations
    logical, dimension(:), allocatable :: last_pulse
  contains
    procedure :: output
  end type conjunction

  type broadcaster
    type(string) :: mod_name
    type(string), dimension(:), allocatable :: destinations
  contains
    procedure :: output
  end type broadcaster
contains
  function output(self, input)
    type(flip_flop), intent(inout) :: self
    logical, intent(in) :: input
    logical :: output

    if (.not. input) then ! low pulse received
      self%state = not(self%state)
      output = not(self%state)
    end if
  end function output

  function output(self, input)
    type(conjunction), intent(inout) :: self
    logical, dimension(:), intent(in) :: input
    logical :: output

    if (.not. input) then ! low pulse received
      self%state = not(self%state)
      output = not(self%state)
    end if
  end function output
end module pulse_module
