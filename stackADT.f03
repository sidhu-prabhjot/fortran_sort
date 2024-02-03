module stackADT
  implicit none
  private

  public :: initialize_stack, push, pop, is_empty, stack_type

  ! Maximum size of the stack
  integer, parameter :: MAX_STACK_SIZE = 1000

  ! Define the stack_type structure to hold left and right integers
  type stack_type
    integer :: left, right
  end type stack_type

  ! Declare an array of stack_type to represent the stack
  type(stack_type), dimension(MAX_STACK_SIZE) :: stack

  ! Declare a variable to keep track of the top of the stack
  integer :: top = 0

contains

  ! Subroutine to initialize the stack
  subroutine initialize_stack()
    top = 0
  end subroutine initialize_stack

  ! Subroutine to push values onto the stack
  subroutine push(left_val, right_val)
    integer, intent(in) :: left_val, right_val
    if (top < MAX_STACK_SIZE) then
      top = top + 1
      stack(top)%left = left_val
      stack(top)%right = right_val
    else
      print *, "Stack overflow"
      stop
    end if
  end subroutine push

  ! Subroutine to pop values from the stack
  subroutine pop(left_val, right_val)
    integer, intent(out) :: left_val, right_val
    if (top > 0) then
      left_val = stack(top)%left
      right_val = stack(top)%right
      top = top - 1
    else
      print *, "Stack underflow"
      stop
    end if
  end subroutine pop

  ! Function to check if the stack is empty
  logical function is_empty() 
    is_empty = (top == 0)
  end function is_empty

end module stackADT
