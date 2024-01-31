subroutine iterativeQsort(a)
  implicit none
  integer, intent(inout) :: a(10)
  integer :: i, j, l, r, x, w
  integer :: s
  integer, parameter :: m = 5
  type :: stack_type
    integer :: l, r
  end type stack_type
  type(stack_type) :: stack(m)

  s = 1
  stack(1)%l = 1
  stack(1)%r = 10

  do while (s /= 0)
    ! take top request from stack
    l = stack(s)%l
    r = stack(s)%r
    s = s - 1

    do while (l < r)
      ! partition a(l) to a(r)
      i = l
      j = r
      x = a((l + r) / 2)
      
      do while (i <= j)
        do while (a(i) < x)
          i = i + 1
        end do
        do while (x < a(j))
          j = j - 1
        end do

        if (i <= j) then
          ! swap a(i) and a(j)
          w = a(i)
          a(i) = a(j)
          a(j) = w
          i = i + 1
          j = j - 1
        end if
      end do

      if (j - l < r - i) then
        if (i < r) then
          ! stack request to sort right partition
          s = s + 1
          stack(s)%l = i
          stack(s)%r = r
        end if
        r = j ! continue sorting left partition
      else
        if (l < j) then
          ! stack request for sorting left partition
          s = s + 1
          stack(s)%l = l
          stack(s)%r = j
        end if
        l = i ! continue sorting right partition
      end if
    end do
  end do
end subroutine iterativeQsort

program iqsort
  implicit none
  integer :: a(10)
  integer :: i

  ! Read 10 integers into array a
  do i = 1, 10
    read(*, *) a(i)
  end do

  ! Call iterativeQsort subroutine to sort the array
  call iterativeQsort(a)

  ! Print the sorted array
  do i = 1, 10
    write(*, *) a(i)
  end do
end program iqsort

