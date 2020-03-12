        PROGRAM EXPAND
        integer*1, DIMENSION(:,:,:), allocatable :: LABEL
        integer*4 :: MAX_X,MAX_Y,MAX_Z
        CHARACTER*30 LABFL,OUTFL
        integer :: error
        integer*8 :: rlen
        LOGICAL :: fexist1,fexist2,fexist3,fexist4,fexist5
        character*15 :: char, char1
        integer*1 :: nbit
!
        WRITE(*,*) ' Run expanding ... '
        write(*,*) ' The original input file can be 1).header&.binary, 2).raw, and 3).bin1.'
        write(*,*) ' The output file after expanding is only in .raw format.'
        write(*,*) ' ***In- & out-put names (without extension) should be listed in epd_list.dat'
        write(*,*) ' ***epd_list.dat fixed format: 30 & 30 characters for input & ouput per line.'
        write(*,*)
        open(10,file='epd_list.dat',status='old')
1       READ(10,'(2A30)',end=999) LABFL,OUTFL
!
       inquire(file=trim(LABFL)//'.bin1',exist=fexist1)
       inquire(file=trim(LABFL)//'.header',exist=fexist2)
       inquire(file=trim(LABFL)//'.binary',exist=fexist3)
       inquire(file=trim(LABFL)//'.raw',exist=fexist4)
       if ((fexist1.and.fexist3).or.(fexist1.and.fexist4).or.(fexist3.and.fexist4)) then
         write(*,*) ' There are more than one data files available. Check and keep only one!'
         stop
       endif
!
       if (fexist1) then
         open(1,FILE=trim(LABFL)//'.bin1',action='read',&
         &form='unformatted',iostat=error)
         WRITE(*,*) ' Input material-label-data : ',trim(LABFL),'.bin1'
         read(1) max_x
         read(1) max_y
         read(1) max_z
         allocate(label(0:max_x+1, 0:max_y+1, 0:max_z+1))
         read(1) (((label(i,j,k),i=1,max_x),j=1,max_y),k=1,max_z) 
         close(1)
         label=1-label
       endif
!
       if (fexist2.and.fexist3) then
         OPEN(1,FILE=trim(LABFL)//'.header',STATUS='OLD')
	  DO I=1,3
           READ(1,*)
	  ENDDO
         READ(1,'(a15,a15)') char,char1
         CLOSE(1)
         open(2,file='max',status='unknown')
         write(2,*) char1
         close(2)
         open(2,file='max',status='old')
         read(2,*) MAX_X,MAX_Y,MAX_Z
         close(2,status='delete')
         WRITE(*,*) ' Original material-label-data : ',trim(LABFL),'.binary'
         rlen=MAX_X*MAX_Y*MAX_Z
         allocate(label(0:max_x+1, 0:max_y+1, 0:max_z+1))
         OPEN(1,FILE=trim(LABFL)//'.binary',STATUS='OLD',access='direct',form='Binary',recl=rlen)
         read(1,rec=1) (((label(i,j,k),i=1,max_x),j=1,max_y),k=1,max_z) 
         CLOSE(1)
       endif
! 
       if (fexist4) then
         OPEN(1,FILE=trim(LABFL)//'.raw',STATUS='OLD',access='direct',form='Binary',recl=13)
         READ(1,rec=1) nbit,MAX_Z,MAX_Y,MAX_X
         CLOSE(1)
         if (nbit.ne.0.and.nbit.ne.1) stop ' Label-data is not integer*1, cannot read correctly!'
         allocate(label(0:max_x+1, 0:max_y+1, 0:max_z+1))
         WRITE(*,*) ' Original material-label-data : ',trim(LABFL),'.raw'
         rlen=MAX_X*MAX_Y*MAX_Z+13
         OPEN(1,FILE=trim(LABFL)//'.raw',STATUS='OLD',access='direct',form='Binary',recl=rlen)
         read(1,rec=1)  nbit,MAX_Z,MAX_Y,MAX_X,(((label(i,j,k),i=1,max_x),j=1,max_y),k=1,max_z) 
         CLOSE(1)
       endif 
!
        write(*,*) 'nx, ny, nz=',max_x,max_y,max_z
!
        LABEL(0, 1:MAX_Y, 1:MAX_Z) = LABEL(1, 1:MAX_Y, 1:MAX_Z)
        LABEL(1:MAX_X, 0, 1:MAX_Z) = LABEL(1:MAX_X, 1, 1:MAX_Z)
        LABEL(1:MAX_X, 1:MAX_Y, 0) = LABEL(1:MAX_X, 1:MAX_Y, 1)
        LABEL(MAX_X+1, 1:MAX_Y, 1:MAX_Z) = LABEL(MAX_X, 1:MAX_Y, 1:MAX_Z)
        LABEL(1:MAX_X, MAX_Y+1, 1:MAX_Z) = LABEL(1:MAX_X, MAX_Y, 1:MAX_Z)
        LABEL(1:MAX_X, 1:MAX_Y, MAX_Z+1) = LABEL(1:MAX_X, 1:MAX_Y, MAX_Z)
        DO K=1,MAX_Z
          DO J=1,MAX_Y
            DO I=1,MAX_X
             IF (LABEL(I,J,K).EQ.0) THEN
              IF (LABEL(I-1,J,K).EQ.1.OR.LABEL(I,J-1,K).EQ.1.OR.LABEL(I,J,K-1).EQ.1) LABEL(I,J,K)=-1
              IF (LABEL(I+1,J,K).EQ.1.OR.LABEL(I,J+1,K).EQ.1.OR.LABEL(I,J,K+1).EQ.1) LABEL(I,J,K)=-1
             ENDIF
            ENDDO
          ENDDO
        ENDDO
!
        DO K=1,MAX_Z
          DO J=1,MAX_Y
            DO I=1,MAX_X
              IF (LABEL(I,J,K).EQ.-1) LABEL(I,J,K)=1
            ENDDO
          ENDDO
        ENDDO

        write(*,*) ' Output expanded material-label-data : ',trim(OUTFL),'.raw'
	 open(3,file=trim(OUTFL)//'.raw',ACTION='WRITE',ACCESS='STREAM')
        nbit=0
        write(3) nbit,MAX_Z,MAX_Y,MAX_X,(((label(i,j,k),i=1,max_x),j=1,max_y),k=1,max_z)
        close(3)
        DEALLOCATE(LABEL)
!
        write(*,*)
        goto 1
!
999    STOP 'FINISH PROGRAM !'
       END
