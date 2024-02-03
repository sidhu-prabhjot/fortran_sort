subroutine iterative_quick_sort(arr, size)
  ! Subroutine to perform iterative quicksort
  use stackADT
  implicit none
  integer, intent(in) :: size
  integer, intent(inout) :: arr(size)
  integer :: left, right, pivot, i, j, temp
  integer :: stack_size
  integer, parameter :: min_stack_size = 5

  stack_size = 1
  call initialize_stack()
  call push(1, size)

  do while (.not. is_empty())
    ! Take the top request from the stack
    call pop(left, right)

    do while (left < right)
      ! Partition arr(left) to arr(right)
      i = left
      j = right
      pivot = arr((left + right) / 2)

      do while (i <= j)
        do while (arr(i) < pivot)
          i = i + 1
        end do
        do while (pivot < arr(j))
          j = j - 1
        end do

        if (i <= j) then
          ! Swap arr(i) and arr(j)
          temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp
          i = i + 1
          j = j - 1
        end if
      end do

      if (j - left < right - i) then
        if (i < right) then
          ! Stack request to sort the right partition
          call push(i, right)
        end if
        right = j ! Continue sorting the left partition
      else
        if (left < j) then
          ! Stack request for sorting the left partition
          call push(left, j)
        end if
        left = i ! Continue sorting the right partition
      end if
    end do
  end do
end subroutine iterative_quick_sort

program iqsort_program
  use intIO
  implicit none
  integer, allocatable :: array(:)
  integer :: array_size
  real(8) :: start_time, end_time, elapsed_time

  call readUnsorted(array)

  array_size = size(array)

  ! Call the iterative_quick_sort subroutine to sort the array
  call cpu_time(start_time)
  call iterative_quick_sort(array, array_size)
  call cpu_time(end_time)

  ! Print the sorted array
  call writeSorted(array)

  ! Calculate the elapsed time in seconds
  elapsed_time = end_time - start_time

  ! Display the elapsed time
  print *, "Iterative quicksort took", elapsed_time, " seconds to run"
end program iqsort_program
