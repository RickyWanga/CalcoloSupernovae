module ContaRighe
    implicit none
    ! Variabile per lo stream del file da aprire
    integer :: FID = 1
    ! Variabili per il conteggio dei file
    character*256 :: CTMP
    integer :: I = 0, IERR = 0, NUM_LINES = 0, numRighe = 0
    public :: numRighe

contains

integer function Conteggio(nomeFile)
    
    character(*), intent(in) :: nomeFile

    open(unit=FID, file=nomeFile)

    do while (IERR == 0)
        NUM_LINES = NUM_LINES + 1
        read(FID,*,iostat=IERR) CTMP
    end do
    NUM_LINES = NUM_LINES - 1

    close(FID)

    Conteggio = NUM_LINES
    return
    
end function Conteggio

end module ContaRighe