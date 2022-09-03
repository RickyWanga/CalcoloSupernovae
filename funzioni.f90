module funzioni
    implicit none
    !Dichiaro le variabili
    character*256, private :: CTMP
    integer, private :: IERR = 0, NUM_LINES = 0, FIDConta = 99
    integer, public :: numRighe = 0
contains
    integer function contaRighe(nomeFile)
        character(*), intent(in) :: nomeFile
        contaRighe = 0
        open(unit=FIDConta, file=nomeFile)

        do while (IERR == 0)
            NUM_LINES = NUM_LINES + 1
            read(FIDConta,*,iostat=IERR) CTMP
        end do
        numRighe = NUM_LINES - 1
        close(FIDConta)
        
        contaRighe = numRighe

        return
    end function contaRighe

end module funzioni