      PROGRAM STEREO
      IMPLICIT NONE
      REAL, DIMENSION(1:10000000,1:3,1:3) :: TMT
      REAL, DIMENSION(1:10000000) :: ISO
      REAL, DIMENSION(3) :: DIP, AZIMUTH, SUMCOS
      INTEGER :: I,L,NN,BGN,EDN,COUNT,IC,K,PROJ,NSUMCOS(3)
      INTEGER :: J,VOL,SURF,NCLUSTER
      REAL :: DIM_1,DIM_2,DIM_3,ELONG,MIN_ISO,MAX_ISO
      INTEGER :: X0,XM,Y0,YM,Z0,ZM
      CHARACTER*50 :: INFL,CBGN,CEDN,IBGN*4,IEDN*4
      CHARACTER*80 :: OUTFL

!      WRITE(*,'(A,$)') ' INPUT DATA IN FILE :> '
      OPEN(10,FILE='stereo.dat',STATUS='OLD')
1      READ(10,'(50a)',END=999) INFL
      write(*,*) ' Now processing file: ',INFL
!
      OPEN(11,FILE=trim(INFL)//'.out2',STATUS='OLD')
      READ(11,*)
      DO I=1,10000000
        READ(11,*,END=101) J,VOL,SURF,DIM_1,DIM_2,DIM_3,ISO(J),ELONG
      ENDDO
101   NCLUSTER=J
      CLOSE(11)
      WRITE(*,*) ' Read .out2 file and got isotropy index.'
      WRITE(*,*) ' The number of clusters in .out2 is:', ncluster
!
      OPEN(1,FILE=trim(INFL)//'.out4',STATUS='OLD')
!
      READ(1,*) X0,XM,Y0,YM,Z0,ZM
      WRITE(*,*) ' The size of the volume is :'
      WRITE(*,*)   X0,XM,Y0,YM,Z0,ZM
      COUNT=1
2     READ(1,*,END=111,err=2) NN
      READ(1,*,err=3) (TMT(NN,1,L),L=1,3)
      READ(1,*,err=3) (TMT(NN,2,L),L=1,3)
      READ(1,*,err=3) (TMT(NN,3,L),L=1,3)
      DO L=1,3
        SUMCOS(L)=TMT(NN,1,L)*TMT(NN,1,L) + TMT(NN,2,L)*TMT(NN,2,L) + TMT(NN,3,L)*TMT(NN,3,L)
        NSUMCOS(L)=INT(SUMCOS(L)*100.+0.5)
        IF (NSUMCOS(L).NE.100) THEN
          WRITE(*,*) ' Summation of direction cosines <> 1 in cluster ',NN
          WRITE(*,*) ' Check the data!'
          STOP
        ENDIF
      ENDDO
      COUNT=COUNT+1
      GOTO 2
3     STOP ' DATA FORMAT ERROR! --- STOP READING!'
111   CONTINUE
      CLOSE(1)
!
      WRITE(*,*) ' The number of clusters in .out4 is:',COUNT-1
      WRITE(*,*) ' Select the range of tensors to be analyzed: '
      READ(10,*) BGN,EDN
      WRITE(*,'(A21,I8)') '     Begin from No.: ',BGN
!      WRITE(*,'(A21,$)') '     Begin from No.: '
!      READ(*,*) BGN
      WRITE(CBGN,'(I8)') BGN
      IF (BGN.GE.1.AND.BGN.LT.10) WRITE(CBGN,'(I1)') BGN
      IF (BGN.GE.10.AND.BGN.LT.100) WRITE(CBGN,'(I2)') BGN
      IF (BGN.GE.100.AND.BGN.LT.1000) WRITE(CBGN,'(I3)') BGN
      IF (BGN.GE.1000.AND.BGN.LT.10000) WRITE(CBGN,'(I4)') BGN
      IF (BGN.GE.10000.AND.BGN.LT.100000) WRITE(CBGN,'(I5)') BGN
      IF (BGN.GE.100000.AND.BGN.LT.1000000) WRITE(CBGN,'(I6)') BGN
      IF (BGN.GE.1000000.AND.BGN.LT.10000000) WRITE(CBGN,'(I7)') BGN
      WRITE(*,'(A17,I8)') '     End to No.: ',EDN
!      WRITE(*,'(A17,$)') '     End to No.: '
!      READ(*,*) EDN
      WRITE(CEDN,'(I8)') EDN
      IF (EDN.GE.1.AND.EDN.LT.10) WRITE(CEDN,'(I1)') EDN
      IF (EDN.GE.10.AND.EDN.LT.100) WRITE(CEDN,'(I2)') EDN
      IF (EDN.GE.100.AND.EDN.LT.1000) WRITE(CEDN,'(I3)') EDN
      IF (EDN.GE.1000.AND.EDN.LT.10000) WRITE(CEDN,'(I4)') EDN
      IF (EDN.GE.10000.AND.EDN.LT.100000) WRITE(CEDN,'(I5)') EDN
      IF (EDN.GE.100000.AND.EDN.LT.1000000) WRITE(CEDN,'(I6)') EDN
      IF (EDN.GE.1000000.AND.EDN.LT.10000000) WRITE(CEDN,'(I7)') EDN
!
      IF (BGN.LT.0.OR.BGN.GT.NCLUSTER) THEN
        WRITE(*,*) ' *** Beginning number incorrect!'
      ENDIF
      IF (EDN.LT.BGN.OR.EDN.GT.NCLUSTER) THEN
        WRITE(*,*) ' *** Ending number incorrect!'
      ENDIF
!
!      WRITE(*,*) 'Specify the range of isotropy index (minimum,maximum):'
      READ(10,*) MIN_ISO,MAX_ISO
      WRITE(*,'(a32,f5.2,a2,f5.2)') ' The range of isotropy index is:', MIN_ISO,' -',MAX_ISO
      WRITE(IBGN,'(f4.2)') MIN_ISO
      WRITE(IEDN,'(f4.2)') MAX_ISO
      OUTFL=trim(INFL)//'_N'//TRIM(CBGN)//'-'//TRIM(CEDN)
      OUTFL=trim(OUTFL)//'_ISO'//TRIM(IBGN)//'-'//TRIM(IEDN)
      OPEN(2,FILE=trim(OUTFL)//'_EV1.TXT',STATUS='UNKNOWN')
      OPEN(3,FILE=trim(OUTFL)//'_EV2.TXT',STATUS='UNKNOWN')
      OPEN(4,FILE=trim(OUTFL)//'_EV3.TXT',STATUS='UNKNOWN')
!
      DO IC=BGN,EDN
      IF (ISO(IC).GE.MIN_ISO.AND.ISO(IC).LE.MAX_ISO) THEN
      write(*,*) ic,iso(ic)
        DO L=1,3
          DIP(L)=ACOSD(TMT(IC,3,L))
          IF (DIP(L).GT.90.) THEN
            DIP(L)=180.0-DIP(L)
            TMT(IC,1,L)=-TMT(IC,1,L)
            TMT(IC,2,L)=-TMT(IC,2,L)
          ENDIF
          IF (TMT(IC,1,L).EQ.0.0.AND.TMT(IC,2,L).EQ.0.0) THEN
            AZIMUTH(L)=0.0
          ELSE
            AZIMUTH(L)=ATAN2D(TMT(IC,1,L),TMT(IC,2,L))
          ENDIF
          IF (AZIMUTH(L).LE.0.) AZIMUTH(L)= 360.0 + AZIMUTH(L)
        ENDDO
        DO L=1,3
          WRITE(L+1,'(2F10.1,5x,A)') AZIMUTH(L),90.0-DIP(L),'L'
        ENDDO
      ENDIF
      ENDDO
!
      WRITE(*,*) '  *** Done. Output files ***'
      WRITE(*,*) ' Minimum principal axes in: ',trim(OUTFL)//'_EV1.TXT'
      WRITE(*,*) ' Mediate principal axes in: ',trim(OUTFL)//'_EV2.TXT'
      WRITE(*,*) ' Maximum principal axes in: ',trim(OUTFL)//'_EV3.TXT'
     CLOSE(2)
     CLOSE(3)
     CLOSE(4)

     GOTO 1
999  WRITE(*,*) ' ALL DONE!'

     STOP
     END
