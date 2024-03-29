program generateTemp
    implicit none
    ! Dichiaro gli array di tipo reale 
    real(kind=8), allocatable :: MJD(:)
    real(kind=8), allocatable :: BandaB(:)
    real(kind=8), allocatable :: e_BandaB(:)
    real(kind=8), allocatable :: MJDtemp(:)
    real(kind=8), allocatable :: BandaBtemp(:)
    real(kind=8), allocatable :: e_BandaBtemp(:)
    real(kind=8) :: x = 0, y = 0
    character*6, allocatable :: nomiSupernovae(:)
    character*20, allocatable :: FileNomiSupernovae(:)
    character*24, allocatable :: PathSupernovae(:)
    integer :: NUM_RIGHE = 0, F = 1, N = 1, NUM_SUP = 0, M = 0, Max = 0, Scartati = 0
    integer :: J = 0, Z = 0

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
   
    open(unit=99, file="PathNames.dat")
    do N = 1, NUM_SUP
        PathSupernovae(N) = 'SN_datatemp/SN' // (nomiSupernovae(N)) // '.dat'
        FileNomiSupernovae = 'SN_data/SN' // (nomiSupernovae(N)) // '.dat'
        print *, PathSupernovae(N), FileNomiSupernovae(N)
        write(99, *) FileNomiSupernovae(N)
    end do
    close(99)

    print *, "Numero Supernovae: ", NUM_SUP

    do N = 10, NUM_SUP + 9 
        F = N - 9
        open(unit=N, file=FileNomiSupernovae(F))
        print *, "opened file no : ", F
        print *, "Nome file : ", FileNomiSupernovae(F)
        NUM_LINES = 0
        IERR = 0
        CTMP = ""
        do while (IERR == 0)
            NUM_LINES = NUM_LINES + 1
            read(N,*,iostat=IERR) CTMP
        end do
        NUM_LINES = NUM_LINES - 1
        NUM_RIGHE = NUM_LINES
      
        print *, "Numero righe totali file: ", NUM_RIGHE
        Max = NUM_RIGHE - 5
        print *, "Limite Max dopo differenza offset: ", Max
        ! Provo a stampare i valori da prendere in considerazione per ciascun file
        allocate(MJD(Max),BandaB(Max),e_BandaB(Max))
        rewind(N)
        do I = 1, NUM_RIGHE
          if(I>=6) then
            M = I - 5
            read(N,*) MJD(M), x, y, BandaB(M), e_BandaB(M)
          else
            read(N,*)
          end if
        end do
    
        I = 1
        do
        ! Se I diventa uguale al numero di righe esce dal loop
          if(I > Max) then
            print *, "Ultimo file"
            allocate(MJDtemp(Max-Scartati), BandaBtemp(Max-Scartati), e_BandaBtemp(Max-Scartati))
            J = 1
            Z = 1
            print *, "Valori scartati : ", Scartati
            do while(J<Max)
                if (.not.((BandaB(J) >= 99.0).and.(BandaB(J)<= 99.99))) then
                    MJDtemp(Z) = MJD(J)
                    BandaBtemp(Z) = BandaB(J)
                    e_BandaBtemp(Z) = e_BandaB(J)
                    J = J + 1
                    Z = Z + 1
                    print *,"NumRiga, NumArray, NumArraytemp ",I, J, Z
                else
                    J = J + 1
                    print *, "Vado avanti perche dato non valido"
                end if
            end do
            deallocate(MJDtemp, BandaBtemp, e_BandaBtemp)
            exit
        ! Controllo che i dati dell'osservazione siano validi
          else if (.not.((BandaB(I) >= 99.0).and.(BandaB(I)<= 99.99))) then
            print *, MJD(I), BandaB(I)
            I = I + 1
          else
            print *, "Valore scartato"
            Scartati = Scartati + 1
            I = I + 1
          end if
        end do
    
        deallocate(MJD, BandaB, e_BandaB)
        
        close(N)
        
      end do
      print *, "Fine del programma"
      deallocate(nomiSupernovae, FileNomiSupernovae, PathSupernovae)

end program