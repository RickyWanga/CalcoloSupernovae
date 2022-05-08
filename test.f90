program readText
implicit none

integer :: FID = 1
character*256 :: CTMP

! 1. Assuming that no line of text.txt contains more than 256 characters
character*256, allocatable :: MY_ARRAY(:)
integer :: I = 0, IERR = 0, NUM_LINES = 0

open(unit=FID,file='SN_data/SN2004dt.dat')

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
do I = 6,size(MY_ARRAY,1)
  write(*,*) trim(MY_ARRAY(I))
end do

do I = 6,NUM_LINES
    write(*,*) "Righe di dati"
    write(*,*) trim(MY_ARRAY(I))
end do

! Scrivo i dati su un file di prova temporaneo
open(5, file="test_riga.dat", status="old")
    do I = 6, NUM_LINES
    write(5, *) MY_ARRAY(I)
    end do
close(5)


deallocate(MY_ARRAY)
close(FID)

end program readText

