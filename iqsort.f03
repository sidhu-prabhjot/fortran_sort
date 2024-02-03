subroutine iterativeQsort(a, n)
  use stackADT
  implicit none
  integer, intent(in) :: n
  integer, intent(inout) :: a(n)
  integer :: i, j, l, r, x, w
  integer :: s
  integer, parameter :: m = 5

  s = 1
  call initialize_stack()
  call push(1, n)

  do while (.not. is_empty())
    ! take top request from stack
    call pop(l, r)

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
          call push(i, r)
        end if
        r = j ! continue sorting left partition
      else
        if (l < j) then
          ! stack request for sorting left partition
          call push(l, j)
        end if
        l = i ! continue sorting right partition
      end if
    end do
  end do
end subroutine iterativeQsort

program iqsort
  use intIO
  implicit none
  integer, allocatable :: a(:)
  integer :: n
  real(8) :: start_time, end_time, elapsed_time

  call readUnsorted(a)

  n = size(a)

  ! Call iterativeQsort subroutine to sort the array
  call cpu_time(start_time)
  call iterativeQsort(a, n)
  call cpu_time(end_time)

  ! Print the sorted array
  call writeSorted(a)

  ! Calculate the elapsed time in seconds
  elapsed_time = end_time - start_time

  ! Display the elapsed time
  print *, "iterative quicksort took", elapsed_time, " seconds to run"
end program iqsort