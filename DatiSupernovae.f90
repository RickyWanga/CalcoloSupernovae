program DatiSupernovae
    implicit none
    ! COMPILARE CON IL FLAG -ffree-line-length-none
    character*6, allocatable :: SN(:)
    real(kind=8), allocatable :: zhel(:)
    real(kind=8), allocatable :: zcmb(:)
    real(kind=8), allocatable :: sBV(:)
    real(kind=8), allocatable :: e_sBV(:)
    real(kind=8), allocatable :: dm15(:)
    real(kind=8), allocatable :: e_dm15(:)
    real(kind=8), allocatable :: Vmax(:)
    real(kind=8), allocatable :: e_Vmax(:)
    real(kind=8), allocatable :: EBV(:)
    real(kind=8), allocatable :: e_EBV(:)
    real(kind=8), allocatable :: Rv(:)
    real(kind=8), allocatable :: e_Rv(:)

    integer :: FID = 42
    character*256 :: CTMP
    integer :: I = 0, IERR = 0, NUM_LINES = 0, J = 0, N = 0

    open(unit=FID,file='SN_data/newtable2.txt')

    ! 2. Get number of lines
    do while (IERR == 0)
        NUM_LINES = NUM_LINES + 1
        read(FID,*,iostat=IERR) CTMP
    end do
    NUM_LINES = NUM_LINES - 1
    write(*,'(A,I0)') "Number of lines = ", NUM_LINES

    allocate(SN(NUM_LINES-31), zhel(NUM_LINES-31), zcmb(NUM_LINES-31), sBV(NUM_LINES-31), e_sBV(NUM_LINES-31), dm15(NUM_LINES-31), e_dm15(NUM_LINES-31), Vmax(NUM_LINES-31), e_Vmax(NUM_LINES-31), EBV(NUM_LINES-31), e_EBV(NUM_LINES-31), Rv(NUM_LINES-31), e_Rv(NUM_LINES-31))
    
    rewind(FID)
    do I=1, NUM_LINES
        if(I>=32) then
            J = I - 31
            read(FID,100) SN(J), zhel(J), zcmb(J), sBV(J), e_sBV(J), dm15(J), e_dm15(J), Vmax(J), e_Vmax(J), EBV(J), e_EBV(J), Rv(J)
            100 format(A6, F7.5, F7.5, F5.3, F5.3, F5.3, F5.3, f6.3, F5.3, F5.3, F3.1, F3.1)
            !print 100, SN(J), zhel(J), zcmb(J), sBV(J), e_sBV(J), dm15(J), e_dm15(J), Vmax(J), e_Vmax(J), EBV(J), e_EBV(J), Rv(J)
        else
            read(FID,*)
        end if
    end do

    close(FID)

    open(42, file="datiSupernovae.dat")
        N = NUM_LINES -31
        do I = 1, N
            write(42,*) SN(I), zcmb(I), EBV(I), Rv(I)
            print *, SN(I)
        end do
    close(42)

end program