program DatiSupernovae
    implicit none

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

    ! 1. Assuming that no line of text.txt contains more than 256 characters
    character*256, allocatable :: MY_ARRAY(:)
    integer :: I = 0, IERR = 0, NUM_LINES = 0

    open(unit=FID,file='SN_data/newtable2.txt')

    ! 2. Get number of lines
    do while (IERR == 0)
        NUM_LINES = NUM_LINES + 1
        read(FID,*,iostat=IERR) CTMP
    end do
    NUM_LINES = NUM_LINES - 1
    write(*,'(A,I0)') "Number of lines = ", NUM_LINES

    allocate(SN(NUM_LINES-31))

    rewind(FID)
    do I=1, NUM_LINES
        if(I>=32) then
            read(FID,100) SN(I-31)
            100 format(A6)
            print *, SN(I-31)
        end if
    end do

    close(FID)

end program