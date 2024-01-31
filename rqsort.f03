module rqsort
  use intIO

  implicit none

  contains

  recursive subroutine recursiveQsort(a, l, r)
    integer, dimension(:) :: a
    integer :: l, r, i, j, x, w

    if (l < r) then
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
          w = a(i)
          a(i) = a(j)
          a(j) = w
          i = i + 1
          j = j - 1
        end if
      end do
      call recursiveQsort(a, l, j)
      call recursiveQsort(a, i, r)
    end if
  end subroutine recursiveQsort

  program main
    integer, dimension(100000) :: a

    call readUnsorted(a)
    call recursiveQsort(a, 1, size(a))
    call writeSorted(a)
  end program main

end module rqsort
