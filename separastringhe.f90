program readText
  use ContaRighe
  implicit none
  ! Dichiaro gli array di tipo reale 
  real(kind=8), allocatable :: first(:)
  real(kind=8), allocatable :: second(:)
  real(kind=8), allocatable :: third(:)
  integer :: NUM_RIGHE = 0, FID = 1

  NUM_RIGHE = Conteggio("test_riga.dat")
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
  
  