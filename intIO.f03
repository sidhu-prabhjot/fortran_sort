module intIO
  implicit none

  contains

subroutine readUnsorted(myArray)
  ! Declare variables
  integer, allocatable, intent(inout) :: myArray(:)
  integer :: numElements, i
  character(len=100) :: filename
  logical :: fileExists
  integer :: ios
  integer :: done = 0

  do while (done == 0)

    ! Prompt user for filename
    write(*,*) "Enter the filename (enter 'q' to quit):"
    read(*,*) filename

    if (filename == 'q') then
      done = 1
      stop
    else
      ! Check if the file exists
      inquire(file=filename, exist=fileExists)

      if (fileExists) then
        ! Open the file
        open(unit=10, file=trim(filename), status='old', action='read', iostat=ios)

        if (ios == 0) then
          ! Determine the number of elements in the file
          numElements = 0
          do
            read(10, *, iostat=ios)
            if (ios /= 0) exit
            numElements = numElements + 1
          end do

          ! Allocate memory for the array
          allocate(myArray(numElements))

          ! Rewind the file to the beginning
          rewind(10)

          ! Read the data into the array
          do i = 1, numElements
            read(10, *) myArray(i)
          end do

          ! Close the file
          close(10)
          done = 1
        else
          write(*,*) "Error opening file:", trim(filename)
        end if
      else
        write(*,*) "File does not exist:", trim(filename)
      end if
    end if

  end do

end subroutine readUnsorted

subroutine writeSorted(numArray)

    integer, intent(inout) :: numArray(:)
    integer :: i
    character(len=100) :: filename
    logical :: fileExists
    integer :: ios
    character(len=1) :: response

    ! Check if the file 'sortedNum.txt' exists
    inquire(file='sortedNum.txt', exist=fileExists)

    if (fileExists) then
        ! File exists, prompt user for overwrite
        write(*, *) "File 'sortedNum.txt' already exists. Do you want to overwrite? (y/n)"
        read(*, *) response

        if (response /= 'y' .and. response /= 'Y') then
            ! User does not want to overwrite, prompt for another filename
            write(*, *) "Enter another filename:"
            read(*, *) filename
        else
            ! User wants to overwrite, use the existing filename
            filename = 'sortedNum.txt'
        end if
    else
        ! File does not exist, use the default filename
        filename = 'sortedNum.txt'
    end if

    ! Open the file for writing
    open(unit=20, file=trim(filename), status='replace', action='write', iostat=ios)

    if (ios == 0) then
        ! Write the array elements to the file
        do i = 1, size(numArray)
            write(20, '(I0)') numArray(i)
        end do

        ! Close the file
        close(20)
    else
        write(*, *) "Error opening file for writing:", trim(filename)
    end if

end subroutine writeSorted


end module intIO