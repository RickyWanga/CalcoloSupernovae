program readText
  !use ContaRighe
  implicit none
  ! Dichiaro gli array di tipo reale 
  real(kind=8), allocatable :: first(:)
  real(kind=8), allocatable :: second(:)
  real(kind=8), allocatable :: third(:)
  character*6, allocatable :: nomiSupernovae(:)
  character*12, allocatable :: FileNomiSupernovae(:)
  character*20, allocatable :: PathSupernovae(:)
  integer :: NUM_RIGHE = 0, FID = 1, N = 1, NUM_SUP = 0

  character*256 :: CTMP
  integer :: I = 0, IERR = 0, NUM_LINES = 0
  
  !NUM_SUP = Conteggio("NomiFileSupernovae.dat")
  NUM_SUP = 70
  write(*,'(A,I0)') "Number of lines = ", NUM_SUP
  
  open(99,file="NomiFileSupernovae.dat")
  allocate(nomiSupernovae(NUM_SUP), FileNomiSupernovae(NUM_SUP), PathSupernovae(NUM_SUP))
  do N = 1, NUM_SUP
    read(99,*) nomiSupernovae(N)
  end do
  close(99)
  
  open(98,file="NomeFileSupernovae.dat")
  do N = 1, NUM_SUP
    FileNomiSupernovae(N) = 'SN' // (nomiSupernovae(N)) // '.dat'
    write(98,*) FileNomiSupernovae(N)
  end do
  close(98)

  do N = 10, NUM_SUP + 9
    PathSupernovae(N - 9) = 'SN_data/' // (FileNomiSupernovae(N - 9))
    print *, PathSupernovae(N -9 )
  end do

  do N = 10, NUM_SUP + 9 
  open(unit=N, file=PathSupernovae(N -9))
    NUM_LINES = 0
    IERR = 0
    CTMP = ""
    do while (IERR == 0)
        NUM_LINES = NUM_LINES + 1
        read(N,*,iostat=IERR) CTMP
    end do
    NUM_LINES = NUM_LINES - 1
    NUM_RIGHE = NUM_LINES - 5
  close(N)
  print *, NUM_RIGHE
  end do

  deallocate(nomiSupernovae, FileNomiSupernovae, PathSupernovae)

  write(*,'(A,I0)') "Number of lines = ", NUM_RIGHE
  open(unit=FID, file="test_riga.dat")
  
  ! 3. Alloca i vari array per memorizzare i dati
  allocate(first(NUM_RIGHE),second(NUM_RIGHE),third(NUM_RIGHE))
  
  ! Legge i contenuti del file
  rewind(FID)
  do I = 1, NUM_RIGHE
    read(FID,*) first(I), second(I), third(I)
  end do
  !Stampa i dati presi dal file sul terminale
  I = 1
  do
    ! Se I diventa uguale al numero di righe esce dal loop
    if(I == NUM_RIGHE) then
      exit
    ! Controllo che i dati dell'osservazione siano validi
    else if (.not.((second(I) >= 99.0).and.(second(I)<= 99.99))) then
      print *, first(I), second(I), third(I)
      I = I + 1
    else
      print *, "Valore scartato"
      I = I + 1
    end if
  end do
  
  ! Deallocazioe dei vari array e chiudo la stream con il file dati
  deallocate(first, second, third)
  close(FID)
  
  
end program readText
  
  