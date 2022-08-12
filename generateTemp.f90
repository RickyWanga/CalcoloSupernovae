program generateTemp
    implicit none
    ! Dichiaro gli array di tipo reale 
    real(kind=8), allocatable :: MJD(:)
    real(kind=8), allocatable :: BandaB(:)
    real(kind=8), allocatable :: third(:)
    character*6, allocatable :: nomiSupernovae(:)
    character*20, allocatable :: FileNomiSupernovae(:)
    character*24, allocatable :: PathSupernovae(:)
    integer :: NUM_RIGHE = 0, FID = 1, N = 1, NUM_SUP = 0, M = 0

    character*256 :: CTMP
    integer :: I = 0, IERR = 0, NUM_LINES = 0

    NUM_SUP = 70
    write(*,'(A,I0)') "Number of lines = ", NUM_SUP
  
    open(99,file="NomiFileSupernovae.dat")
    allocate(nomiSupernovae(NUM_SUP), FileNomiSupernovae(NUM_SUP), PathSupernovae(NUM_SUP))
    do N = 1, NUM_SUP
        read(99,*) nomiSupernovae(N)
    end do
    close(99)
   
    do N = 1, NUM_SUP
        PathSupernovae(N) = 'SN_datatemp/SN' // (nomiSupernovae(N)) // '.dat'
        FileNomiSupernovae = 'SN_data/SN' // (nomiSupernovae(N)) // '.dat'
        print *, PathSupernovae(N)
    end do

    do I=10, NUM_SUP + 9
        open(I, file=FileNomiSupernovae(I-9))
            read(I, *) MJD(I-9), x, y, BandaB()
        close(I)
    end do


end program