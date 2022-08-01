program readText
  implicit none

  character*50, dimension(70) :: x
  integer :: FID = 1
  character*256 :: CTMP

  ! 1. Assuming that no line of text.txt contains more than 256 characters
  character*256, allocatable :: MY_ARRAY(:)
  integer :: I = 0, IERR = 0, NUM_LINES = 0, NUM_LINES2 = 0, j = 0

  open(unit=FID,file='SN_data/newtable2.txt')

  ! 2. Get number of lines
  do while (IERR == 0)
    NUM_LINES = NUM_LINES + 1
    read(FID,*,iostat=IERR) CTMP
  end do
  NUM_LINES = NUM_LINES - 1
  write(*,'(A,I0)') "Number of lines = ", NUM_LINES

  ! 3. Allocate array of strings
  allocate(MY_ARRAY(NUM_LINES))

  ! 4. Read the file content
  rewind(FID)
  do I = 1, NUM_LINES
    read(FID,'(A)') MY_ARRAY(I)
  end do

  ! 5. Print array to standard output
  do I = 32,size(MY_ARRAY,1)
    write(*,*) trim(MY_ARRAY(I))
  end do

  ! Scrivo i dati su un file di prova temporaneo
  open(5, file="NomiFileSupernovae.dat", status="old")
    do I = 32, NUM_LINES
      write(5, *) MY_ARRAY(I)
    end do
  close(5)

  open(5, file="NomiFileSupernovae.dat", status="old")

  open(7, file="NomiFileSupernovae.txt", status="old")

  do j=1,70
    read(5, *) x(j)
    write(7, *) x(j)
  end do
  
  close(5)
  close(7)


  deallocate(MY_ARRAY)
  close(FID)

end program readText

