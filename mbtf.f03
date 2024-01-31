PROGRAM mbtf

INTEGER, DIMENSION(5) :: inputArr
INTEGER :: i

DO i = 1, 5
    write(*, *) 'input an integer'
    read(*, *) inputArr(i)
END DO

DO i = 1, 5
    write(*, *) 'value at ', i , ' = ', inputArr(i)
END DO

END PROGRAM mbtf