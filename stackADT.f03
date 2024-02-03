module stackADT
  implicit none
  private

  public :: initialize_stack, push, pop, is_empty, stack_type

  integer, parameter :: MAX_STACK_SIZE = 1000

  type stack_type
    integer :: l, r
  end type stack_type

  type(stack_type), dimension(MAX_STACK_SIZE) :: stack
  integer :: top = 0

contains

  subroutine initialize_stack()
    top = 0
  end subroutine initialize_stack

  subroutine push(left, right)
    integer, intent(in) :: left, right
    if (top < MAX_STACK_SIZE) then
      top = top + 1
      stack(top)%l = left
      stack(top)%r = right
    else
      print *, "Stack overflow"
      stop
    end if
  end subroutine push

  subroutine pop(left, right)
    integer, intent(out) :: left, right
    if (top > 0) then
      left = stack(top)%l
      right = stack(top)%r
      top = top - 1
    else
      print *, "Stack underflow"
      stop
    end if
  end subroutine pop

  logical function is_empty() 
    is_empty = (top == 0)
  end function is_empty

end module stackADT