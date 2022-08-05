program testContaRighe
    use contaRighe
    implicit none

    integer :: numerinoRighe = 0

    numerinoRighe = Conteggio("test_riga.dat")

    print *, numerinoRighe

end program