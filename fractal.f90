module sort
contains
recursive subroutine QsortC(A,idx)
  integer, dimension(:), intent(inout) :: A
  integer, dimension(:), intent(inout) :: idx
  integer :: iq

  if(size(A) > 1) then
     call Partition(A,idx,iq)
     call QsortC(A(:iq-1),idx(:iq-1))
     call QsortC(A(iq:),idx(iq:))
  endif
end subroutine QsortC

subroutine Partition(A,idx, marker)
  integer, dimension(:) :: A
  integer, dimension(:) :: idx
  integer, intent(out) :: marker
  integer :: i, j
  integer :: tempA
  integer :: tempidx
  integer :: x      ! pivot point

  x = A(1)
  i= 0
  j= size(A) + 1

  do
     j = j-1
     do
        if (A(j) >= x) exit
        j = j-1
     end do
     i = i+1
     do
        if (A(i) <= x) exit
        i = i+1
     end do
     if (i < j) then
        ! exchange A(i) and A(j)
        tempA = A(i)
        tempidx = idx(i)
        A(i) = A(j)
        idx(i) = idx(j)
        A(j) = tempA
        idx(j) = tempidx
     elseif (i == j) then
        marker = i+1
        return
     else
        marker = i
        return
     endif
  end do

end subroutine Partition
end module sort
!
!
       PROGRAM fractal
!
!
       use sort
!
       implicit none
       INTEGER :: I,N,MAXICI,NPORE,VOLUM,NCLUST,N_SUM,NUMB
       REAL :: PP
       INTEGER, DIMENSION(:), ALLOCATABLE :: ICI, IR, RANK
       REAL, DIMENSION(:), ALLOCATABLE :: RS_SQ,RS,RS_NORM
       CHARACTER*80 FLNAME
!
!       WRITE(*,*) ' INPUT FILE NAME:'
        open(10,file='clus_fract.dat',status='old')
1       READ(10,'(a80)',end=999) FLNAME
        WRITE(*,*) ' INPUT FILE NAME:',FLNAME
       OPEN(1,FILE=trim(FLNAME)//'.clus',STATUS='OLD')
       READ(1,*) NCLUST
       READ(1,*)
       ALLOCATE(ICI(NCLUST),IR(NCLUST),RANK(NCLUST))
       ALLOCATE(RS_SQ(NCLUST),RS(NCLUST),RS_NORM(NCLUST))
       DO I=1,NCLUST
         READ(1,*) N,ICI(N),RS_SQ(N),NPORE,PP
         IR(N)=INT(RS_SQ(N)*100.+0.5)
         RANK(I)=I
       ENDDO
       call QsortC(IR(:nclust),rank(:nclust))
       CLOSE(1)
!
       OPEN(3,FILE=trim(FLNAME)//'.chk',STATUS='UNKNOWN')
       CLOSE(3,STATUS='DELETE')
       OPEN(3,FILE=trim(FLNAME)//'.chk',STATUS='NEW')
       WRITE(3,*) '    Sort_RS      Radius     RS_Norm    No_voxel       Radius^2'
       DO I=1,NCLUST
         RS(I)=sqrt(real(IR(I))/100.)
         IF (RS(I).EQ.0.0) RS(I)=0.25
         RS_NORM(I)=RS(I)/sqrt(real(IR(1))/100.)
         WRITE(3,22) I,RS(I),RS_NORM(I),ICI(RANK(I)),real(IR(I))/100.
       ENDDO
       CLOSE(3)
22     FORMAT(I12,2F12.5,I12,F15.3)
!
       OPEN(3,FILE=trim(FLNAME)//'.chk',STATUS='OLD')
       READ(3,*)
!       WRITE(*,*) ' OUTPUT FILE NAME:',
       OPEN(2,FILE=trim(FLNAME)//'.ftdm',STATUS='UNKNOWN')
       WRITE(2,*) '      RADIUS    NUMBER   TOTAL_NUM    LOG(L)      LOG(N)'
       NUMB=1
       READ(3,*) N,RS(N),RS_NORM(N),ICI(N),IR(N)
       N_SUM=1
       DO I=2,NCLUST
         READ(3,*) N,RS(N),RS_NORM(N),ICI(N),IR(N)
         N_SUM=N_SUM+1
         IF (RS(N).EQ.RS(N-1)) THEN
           NUMB=NUMB+1
         ELSE
           WRITE(2,33)RS_NORM(N-1),NUMB,N_SUM-1,LOG(RS_NORM(N-1)),LOG(REAL(N_SUM-1))
           NUMB=1
         ENDIF  
       ENDDO
       WRITE(2,33)RS_NORM(N),NUMB,N_SUM,LOG(RS_NORM(N)),LOG(REAL(N_SUM))
33     FORMAT(F15.7,I8,I10,2F12.5)
         
       DEALLOCATE(ICI,IR,RANK,RS_SQ,RS,RS_NORM)
       GOTO 1
!
999    STOP
       END PROGRAM fractal
