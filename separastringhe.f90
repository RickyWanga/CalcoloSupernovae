program readText
  implicit none
  ! Dichiaro gli array di tipo reale 
  real(kind=8), allocatable :: first(:)
  real(kind=8), allocatable :: second(:)
  real(kind=8), allocatable :: third(:)
  
  integer :: FID = 1
  character*256 :: CTMP
  
  integer :: I = 0, IERR = 0, NUM_LINES = 0
  
  open(unit=FID,file='test_riga.dat')
  
  ! 2. Conta le righe di dati/osservazioni presenti
  do while (IERR == 0)
    NUM_LINES = NUM_LINES + 1
    read(FID,*,iostat=IERR) CTMP
  end do
  NUM_LINES = NUM_LINES - 1
  write(*,'(A,I0)') "Number of lines = ", NUM_LINES
  
  ! 3. Alloca i vari array per memorizzare i dati
  allocate(first(NUM_LINES),second(NUM_LINES),third(NUM_LINES))
  
  ! Legge i contenuti del file
  rewind(FID)
  do I = 1, NUM_LINES
    read(FID,*) first(I), second(I), third(I)
  end do
  !Stampa i dati presi dal file sul terminale
  do I=1, NUM_LINES
    print *, first(I), second(I), third(I)
  end do

  write(*,*) "Printing the funny part"
  
  ! Deallocazioe dei vari array e chiudo la stream con il file dati
  deallocate(first, second, third)
  close(FID)
  
end program readText
  
  