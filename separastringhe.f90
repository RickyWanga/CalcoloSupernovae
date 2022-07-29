program main
    use stringifor_string_t
       implicit none
       type( string ) ::  string1
       type( string ), allocatable :: substrings( : )
       character*256, allocatable :: MY_ARRAY(:)
        integer :: I = 0, IERR = 0, NUM_LINES = 0
       integer :: i
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
        call MY_ARRAY(I)%split(sep=' ', tokens=substrings)
        do i=1,size(substrings, dim=1)
            write(*,*) substrings(i)
        enddo
       end do
    end program main