program main
    
    implicit none
    real(8), dimension(10) :: x,y,z,controllo
    integer :: i
    integer :: FID = 3
    character*256 :: CTMP
    integer :: J = 0, IERR = 0, NUM_LINES = 0

    controllo = 99.90000
    open(unit=3, file="test_riga.dat")

    do while (IERR == 0)
        NUM_LINES = NUM_LINES + 1
        read(FID,*,iostat=IERR) CTMP
    end do
    NUM_LINES = NUM_LINES - 1
    write(*,'(A,I0)') "Number of lines = ", NUM_LINES

    REWIND(3)
    i = 1
    do i=1, NUM_LINES
        read(3, *) x(i), y(i), z(i)
        if(y(i)<99) then
            print '(3(1x f0.5))', x(i), y(i), z(i)
        end if
    end do
    close(3)


end program main
