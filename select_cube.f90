       program select_cube
!
       implicit none
       integer*1, dimension(:,:,:), allocatable :: labmat
       integer*8 :: FI_XL0,rlen
       CHARACTER*80 LABFL,OUTNAME
       CHARACTER*4 CHX0,CHXM,CHY0,CHYM,CHZ0,CHZM
       integer :: error,i,j,k,m,IM,ik,IL,inix,iniy,iniz
       INTEGER*4 :: MAX_X0,MAX_Y0,MAX_Z0,VOLX0,VOLXM,VOLY0,VOLYM,VOLZ0,VOLZM
       INTEGER*8 :: MAX_X,MAX_Y,MAX_Z
       INTEGER :: ll,istepx, istepy,istepz,nfi0,nfi,z0,y0
       real :: pbden, emu, FI_XL,TR_TMT,temp,TV
       integer ::  NLARGE,SEED,SUBLARG,outsides0,samelab,outsides1
       LOGICAL :: fexist1,fexist2,fexist3,fexist4,fexist5
       character*15 :: char, char0, select*1
       integer*1 :: nbit
!
!       write(*,'(a,$)') ' Reading data from: '
!       READ(*,*) LABFL
!
       OPEN(10,FILE='select_cube.dat',STATUS='OLD')
       READ(10,*) LABFL
!       inquire(file=trim(LABFL)//'.bin1',exist=fexist1)
       inquire(file=trim(LABFL)//'.header',exist=fexist2)
       inquire(file=trim(LABFL)//'.binary',exist=fexist3)
       inquire(file=trim(LABFL)//'.raw',exist=fexist4)
       if ((fexist1.and.fexist3).or.(fexist1.and.fexist4).or.(fexist3.and.fexist4)) then
         write(*,*) ' There are more than one data files available. Check and keep only one!'
         stop
       endif
!
!       if (fexist1) then
!         open(1,FILE=trim(LABFL)//'.bin1',action='read',&
!         &form='unformatted',iostat=error)
!         WRITE(*,*) ' ***** CALCULATION OF FILE ',trim(LABFL),'.bin1'
!         read(1) max_x0
!         read(1) max_y0
!         read(1) max_z0
!         allocate(labmat(max_x0,max_y0,max_z0))
!         read(1) labmat
!         close(1)
!       endif
!
       if (fexist2.and.fexist3) then
         OPEN(1,FILE=trim(LABFL)//'.header',STATUS='OLD')
	  DO I=1,3
           READ(1,*)
	  ENDDO
         READ(1,'(a15,a15)') char,char0
         CLOSE(1)
         open(2,file='max',status='unknown')
         write(2,*) char0
         close(2)
         open(2,file='max',status='old')
         read(2,*) MAX_X0,MAX_Y0,MAX_Z0
         close(2,status='delete')
         WRITE(*,*) ' ***** CALCULATION OF FILE ',trim(LABFL),'.binary'
         rlen=MAX_X0*MAX_Y0*MAX_Z0
         allocate(labmat(max_x0,max_y0,max_z0))
         OPEN(1,FILE=trim(LABFL)//'.binary',STATUS='OLD',access='direct',form='Binary',recl=rlen)
         READ(1,rec=1) LABMAT
         CLOSE(1)
!         labmat=1-labmat
       endif
!
       if (fexist4) then
         OPEN(1,FILE=trim(LABFL)//'.raw',STATUS='OLD',access='direct',form='Binary',recl=13)
         READ(1,rec=1) nbit,MAX_Z0,MAX_Y0,MAX_X0
         CLOSE(1)
!         if (nbit.ne.0.and.nbit.ne.1) stop ' Label-data is not integer*1, cannot read correctly!'
         allocate(labmat(MAX_X0,MAX_Y0,MAX_Z0))
         WRITE(*,*) ' ***** CALCULATION OF FILE ',trim(LABFL),'.raw'
         rlen=MAX_X0*MAX_Y0*MAX_Z0+13
         OPEN(1,FILE=trim(LABFL)//'.raw',STATUS='OLD',access='direct',form='Binary',recl=rlen)
         READ(1,rec=1) nbit,MAX_Z0,MAX_Y0,MAX_X0,LABMAT
         CLOSE(1)
!         labmat=1-labmat
       endif
!
       WRITE(*,*) 'READ LABEL DATA PASS.'
!
! CALCULATING POROSITY OF THE VOLUME (VARIABLE 'FI_XL0')
       FI_XL0=0
       DO K=1,MAX_Z0
         DO J=1,MAX_Y0
           DO I=1,MAX_X0
             IF (LABMAT(I,J,K).EQ.1) FI_XL0=FI_XL0+1
           ENDDO
         ENDDO
       ENDDO
!
       TV=real(MAX_X0*MAX_Y0*MAX_Z0)
       FI_XL=REAL(FI_XL0)/TV
       NFI0=NINT(FI_XL*100.)
       write(*,'(a31,i2,a1)') ' The porosity of the model is:', NFI0,'%'
!
       write(*,'(a,$)') ' *** Now begin to select a cube of side-length:'
       read(*,*) LL
       istepx=int((max_x0-ll)/11)+1
       istepy=int((max_y0-ll)/12)+1
       istepz=int((max_z0-ll)/13)+1
       write(*,'(a,$)') ' Input the starting point of z and y:'
       read(*,*) z0, y0
       if (z0.eq.0) z0=1
       if (y0.eq.0) y0=1
!
       DO INIZ=z0,MAX_Z0-LL+1,istepz
         DO INIY=y0,MAX_Y0-LL+1,istepy
           DO INIX=1,MAX_X0-LL+1,istepx
!
             FI_XL0=0
             DO K=INIZ,INIZ+LL-1
               DO J=INIY,INIY+LL-1
                 DO I=INIX,INIX+LL-1
                   IF (LABMAT(I,J,K).EQ.1) FI_XL0=FI_XL0+1
                 ENDDO
               ENDDO
             ENDDO
             FI_XL=REAL(FI_XL0)/real(LL*LL*LL)
             NFI=NINT(FI_XL*100.)
!
!            if (abs(nfi-nfi0).le.2) then
            if (nfi.eq.nfi0) then
              write(*,*) ' Found one in:',inix,iniy,iniz
              write(*,*) ' nfi=',nfi
              write(*,*) ' Do you want to select this one and write it to a raw file?'
              write(*,'(a,$)') ' Type Y or y to select it, or any else to search again: '
              read(*,*) select
              if (select.eq.'y'.or.select.eq.'Y') then
                if (inix.lt.10) write(chx0,'(i1)') inix
                if (inix.ge.10.and.inix.lt.100) write(chx0,'(i2)') inix
                if (inix.ge.100.and.inix.lt.1000) write(chx0,'(i3)') inix
                if (inix.ge.1000.and.inix.lt.10000) write(chx0,'(i4)') inix
!
                if (iniy.lt.10) write(chy0,'(i1)') iniy
                if (iniy.ge.10.and.iniy.lt.100) write(chy0,'(i2)') iniy
                if (iniy.ge.100.and.iniy.lt.1000) write(chy0,'(i3)') iniy
                if (iniy.ge.1000.and.iniy.lt.10000) write(chy0,'(i4)') iniy
!
                if (iniz.lt.10) write(chz0,'(i1)') iniz
                if (iniz.ge.10.and.iniz.lt.100) write(chz0,'(i2)') iniz
                if (iniz.ge.100.and.iniz.lt.1000) write(chz0,'(i3)') iniz
                if (iniz.ge.1000.and.iniz.lt.10000) write(chz0,'(i4)') iniz
!
                if (LL.lt.10) write(chzm,'(i1)') LL
                if (LL.ge.10.and.LL.lt.100) write(chzm,'(i2)') LL
                if (LL.ge.100.and.LL.lt.1000) write(chzm,'(i3)') LL
                if (LL.ge.1000.and.LL.lt.10000) write(chzm,'(i4)') LL
!
         outname=trim(labfl)//'_'//trim(chx0)//'_'//trim(chy0)//'_'//trim(chz0)//'_L'//trim(chzm)//'.raw'
                write(*,*) ' Selected! Write a raw file in: ',trim(outname)
                OPEN(6,FILE=outname,STATUS='UNKNOWN')
                CLOSE(6,STATUS='DELETE')
                OPEN(6,FILE=outname,ACTION='WRITE',ACCESS='STREAM')
                WRITE(6) nbit,LL,LL,LL,(((labmat(i,j,k),i=inix,inix+ll-1),j=iniy,iniy+ll-1),k=iniz,iniz+ll-1)
                CLOSE(6)
                stop
              endif
            endif
!C
          ENDDO
        ENDDO
      ENDDO
!
      stop
      end
