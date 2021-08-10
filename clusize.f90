       PROGRAM CLUSIZE
!
       IMPLICIT NONE
       INTEGER :: I,J,II
       REAL :: FRACT,ACCF,AVE_S,COR_LEN
       REAL*8 :: DENOM1,NUMER1,DENOM2,NUMER2
       INTEGER*4, DIMENSION(:) :: S(100000), NS(100000)
       REAL, DIMENSION (:) :: RS_SQ(100000)
       CHARACTER :: FLNAME*80
!
       NS=0
       RS_SQ=0.0
!       WRITE(*,'(a,$)') ' Input file name:'
        open(10,file='clusize.dat',status='old')
1       READ(10,'(a80)',end=999) FLNAME
        WRITE(*,*) ' INPUT FILE NAME:',FLNAME
       OPEN(1,FILE=TRIM(FLNAME)//'.cnss',STATUS='OLD')
       READ(1,*)
       S=0
       NS=0
       RS_SQ=0.0
       DO I=1,100000
         READ(1,*,END=99) S(I),NS(I), RS_SQ(I), FRACT,ACCF
         II=I
!         write(*,*) S(I),NS(I), RS_SQ(I), FRACT,ACCF
         DO J=1,I
           DENOM1=DENOM1+real(NS(J))*real(S(J))
         ENDDO
         NUMER1=real(NS(I))*real(S(I))*real(S(I))
         AVE_S=AVE_S+NUMER1/DENOM1
         DENOM2=DENOM2+real(NS(I))*real(S(I))*real(S(I))
         NUMER2=NUMER2+real(NS(I))*real(S(I))*real(S(I))*RS_SQ(I)
       ENDDO
!
99     CONTINUE
       OPEN(2,FILE=TRIM(FLNAME)//'.mscl',STATUS='UNKNOWN')
       COR_LEN=SQRT(2*NUMER2/DENOM2)
       WRITE(2,*) 'MODEL: ',TRIM(FLNAME)
       WRITE(2,*)
       WRITE(2,*) ' WITH THE LARGEST CLUSTER IN THE SUMS:'
       WRITE(2,*) ' THE AVERAGE CLUSTER SIZE IS:', AVE_S
       WRITE(2,*) ' THE CORRELATION LENGTH IS :', COR_LEN
       WRITE(2,*)
!
       WRITE(2,*) ' WITHOUT THE LARGEST CLUSTER IN THE SUMS:'
       AVE_S=AVE_S-NUMER1/DENOM1
       DENOM2=DENOM2-real(NS(II))*real(S(II))*real(S(II))
       NUMER2=NUMER2-real(NS(II))*real(S(II))*real(S(II))*RS_SQ(II)
       COR_LEN=SQRT(2*NUMER2/DENOM2)
       WRITE(2,*) ' THE AVERAGE CLUSTER SIZE IS:', AVE_S
       WRITE(2,*) ' THE CORRELATION LENGTH IS :', COR_LEN
!
       GOTO 1
!
999    STOP
       END
       