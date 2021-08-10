        PROGRAM READRAW
        integer*4, DIMENSION(:,:,:), allocatable ::  LABMAT0
        integer*1, DIMENSION(:,:,:), allocatable ::  LABMAT
        integer*4 :: NX,NY,NZ
        CHARACTER*30 LABFL,CHAR*15,wrt*1
        integer*8 :: rlen
        integer :: error
        integer*1 :: nbit
!
        OPEN(1,FILE='addheader.dat',status='old')
1		READ(1,*,ERR=999) LABFL
        WRITE(*,'(a,$)') ' Processing Data : ', LABFL
        read(1,*) nz, ny, nx
		WRITE(*,*)
!
        allocate(labmat0(NX,NY,NZ))
        rlen=NX*NY*NZ
!
        OPEN(2,FILE=trim(LABFL)//'.raw',STATUS='OLD',access='direct',form='Binary',recl=rlen)
        READ(2,rec=1) LABMAT0
        CLOSE(2)
!
		    nbit=1
        rlen=NX*NY*NZ+13
        allocate(labmat(NX,NY,NZ))
		    OPEN(2,FILE=trim(LABFL)//'_h.raw',STATUS='UNKNOWN')
		    CLOSE(2,STATUS='DELETE')
        labmat=labmat0
        OPEN(2,FILE=trim(LABFL)//'_h.raw',ACTION='WRITE',access='STREAM')
        WRITE(2) nbit,NZ,NY,NX,LABMAT
        CLOSE(2)

		    deallocate(labmat0)
	     	goto 1
!
999    STOP
       END
