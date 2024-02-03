program quicksort
    use intIO

    implicit none

    integer, allocatable :: arr(:)
    integer :: n
    real(8) :: start_time, end_time, elapsed_time

    call readUnsorted(arr)

    n = size(arr)

    call cpu_time(start_time)
    call quick_sort(arr, 1, n)
    call cpu_time(end_time)

    call writeSorted(arr)

    ! Calculate the elapsed time in seconds
    elapsed_time = end_time - start_time

    ! Display the elapsed time
    print *, "recursive quicksort took", elapsed_time, " seconds to run"

contains

    recursive subroutine quick_sort(arr, low, high)
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        integer :: pivot

        if (low < high) then
            pivot = partition(arr, low, high)
            call quick_sort(arr, low, pivot - 1)
            call quick_sort(arr, pivot + 1, high)
        end if
    end subroutine quick_sort

    function partition(arr, low, high) result(pivot)
        integer :: arr(:)
        integer, intent(in) :: low, high
        integer :: i, j, pivot, temp

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
