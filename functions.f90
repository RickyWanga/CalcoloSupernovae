program main
    
    implicit none
    real*16, dimension(200) :: x,y
    integer :: i

    open(unit=3, file="test_riga.dat")
    i = 1
    do i=1, 7
        read(3, *) x(i), y(i)
        print *, x(i), y(i)
    end do
    close(3)


end program main
