       PROGRAM STA_CLUS
!
       IMPLICIT NONE
       INTEGER :: I,N,MAXICI,NPORE,VOLUM,NCLUST
       INTEGER*4 :: ICI
       REAL :: RS_SQ,PP,AV_RS,FRACT,S_FRACT
       INTEGER, DIMENSION(:), ALLOCATABLE :: N_SUM
       REAL, DIMENSION(:), ALLOCATABLE :: SUM_RS
       CHARACTER*80 FLNAME
!
!       WRITE(*,*) ' INPUT FILE NAME:'
        open(10,file='sta_clus.dat',status='old')
1       READ(10,'(a80)',end=999) FLNAME
        WRITE(*,*) ' INPUT FILE NAME:',FLNAME
       OPEN(1,FILE=trim(FLNAME)//'.clus',STATUS='OLD')
       READ(1,*) NCLUST
       READ(1,*)
       READ(1,*) N,ICI,RS_SQ,NPORE,PP
       ALLOCATE(N_SUM(ICI),SUM_RS(ICI))
       N_SUM=0
       SUM_RS=0.0
       MAXICI=ICI
       N_SUM(ICI)=1
       SUM_RS(ICI)=RS_SQ
       DO I=2,NCLUST
         READ(1,*) N,ICI,RS_SQ,NPORE,PP
!         WRITE(*,*) N,ICI,RS_SQ,NPORE,PP
         N_SUM(ICI)=N_SUM(ICI)+1
         SUM_RS(ICI)=SUM_RS(ICI)+RS_SQ
       ENDDO
       CLOSE(1)
!
!       WRITE(*,*) ' OUTPUT FILE NAME:',
       OPEN(2,FILE=trim(FLNAME)//'.cnss',STATUS='UNKNOWN')
       WRITE(2,*) '  PORE-SIZE     NUMBERS       RADIUS^2       FRACTION   ACC-FRACTION'
       VOLUM=0.0
       DO I=1,MAXICI
         IF (N_SUM(I).NE.0) THEN
           AV_RS=SUM_RS(I)/N_SUM(I)
           FRACT=REAL(N_SUM(I)*I)/REAL(NPORE)
           VOLUM=VOLUM+N_SUM(I)*I
           S_FRACT=REAL(VOLUM)/REAL(NPORE)
           WRITE(2,'(2I12,3E15.5)') I, N_SUM(I),AV_RS,FRACT*100.,S_FRACT*100.
         ENDIF
       ENDDO
       CLOSE(2)
!
       DEALLOCATE(N_SUM,SUM_RS)
       GOTO 1
!
999    STOP
       END
