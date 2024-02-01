program test

use intIO

integer, allocatable :: myArray(:)
integer :: i

call readUnsorted(myArray)
call writeSorted(myArray)

! Print the array elements
write(*,*) "Array elements:"
do i = 1, size(myArray)
    write(*,*) myArray(i)
end do

deallocate(myArray)

end program test