program quicksort
    use intIO

    implicit none

    integer, allocatable :: arr(:)   ! Dynamic integer array for sorting
    integer :: n                      ! Number of elements in the array
    real(8) :: start_time, end_time, elapsed_time

    ! Read the unsorted array from input
    call readUnsorted(arr)

    n = size(arr)

    ! Record the starting time
    call cpu_time(start_time)

    ! Call the quick sort algorithm
    call quick_sort(arr, 1, n)

    ! Record the ending time
    call cpu_time(end_time)

    ! Write the sorted array to output
    call writeSorted(arr)

    ! Calculate the elapsed time in seconds
    elapsed_time = end_time - start_time

    ! Display the elapsed time
    print *, "Recursive quicksort took", elapsed_time, " seconds to run"

contains

    ! Recursive subroutine for quicksort
    recursive subroutine quick_sort(arr, low, high)
        integer, intent(inout) :: arr(:)  ! Input/output array
        integer, intent(in) :: low, high   ! Low and high indices for subarray
        integer :: pivot                   ! Pivot element index

        if (low < high) then
            pivot = partition(arr, low, high)  ! Partition the array
            call quick_sort(arr, low, pivot - 1)  ! Sort the left subarray
            call quick_sort(arr, pivot + 1, high)  ! Sort the right subarray
        end if
    end subroutine quick_sort

    ! Function to partition the array
    function partition(arr, low, high) result(pivot)
        integer :: arr(:)            ! Input array
        integer, intent(in) :: low, high  ! Low and high indices for subarray
        integer :: i, j, pivot, temp  ! Loop variables and pivot element

        pivot = arr(high)
        i = low - 1

        do j = low, high - 1
            if (arr(j) <= pivot) then
                i = i + 1
                temp = arr(i)
                arr(i) = arr(j)
                arr(j) = temp
            end if
        end do

        temp = arr(i + 1)
        arr(i + 1) = arr(high)
        arr(high) = temp

        pivot = i + 1
    end function partition

end program quicksort
