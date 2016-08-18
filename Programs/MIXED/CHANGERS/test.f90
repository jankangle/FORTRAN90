MODULE vars
 IMPLICIT NONE
 TYPE gbs
    CHARACTER(LEN=2) :: lmax
    INTEGER :: ngbs
    REAL,DIMENSION(:,:),ALLOCATABLE :: fgbs
 END TYPE gbs
 INTEGER :: nblk,size
 TYPE(gbs),DIMENSION(:),ALLOCATABLE :: g,h
 CHARACTER(LEN=15) :: atom,newatm
END MODULE vars

 SUBROUTINE angchk(ang,check)
 IMPLICIT NONE
 CHARACTER(LEN=2), INTENT(IN) :: ang
 LOGICAL, INTENT(OUT) :: check
 IF (ang == "SP" .or. ang == "L") then
   check = .TRUE.
 ELSE
   check = .FALSE.
 ENDIF
 END SUBROUTINE angchk

 SUBROUTINE Lprint(ang,lblk,ndim,i,j)
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: lblk
 INTEGER, INTENT(INOUT) :: ndim,i,j
 !f2py intent(in,out) :: ndim,i,j
 CHARACTER(LEN=1), INTENT(INOUT) :: ang
 !f2py intent(in,out) :: ang
 INTEGER :: k,l
 REAL,ALLOCATABLE,DIMENSION(:,:) :: matrix
 ALLOCATE(matrix(1,3))
 IF (i == 1) THEN
 ELSE
   IF (ang == "P") THEN
     j=j+1
   ENDIF
   IF (j == lblk) THEN
     i=i+1
     REWIND(12)
     DO k=1,lblk
       READ(12,*) ang,ndim
       WRITE(11,*) ang,ndim
       DO l=1,ndim
         READ(12,*) matrix(j,:)
         WRITE(11,*) l,matrix(j,2),matrix(j,3)
       ENDDO
     ENDDO
   ENDIF 
 ENDIF
 DEALLOCATE(matrix)
 END SUBROUTINE Lprint

 SUBROUTINE angmom(ang,m)
 IMPLICIT NONE
 CHARACTER(LEN=1), INTENT(INOUT) :: ang
 !f2py intent(in,out) :: ang
 INTEGER, INTENT(OUT) :: m
 IF (ang == "S") THEN
   m=0
 ELSEIF (ang == "P") THEN
   m=1
 ELSEIF (ang == "L") THEN
   m=0
 ELSEIF (ang == "D") THEN
   m=2 
 ELSEIF (ang == "F") THEN
   m=3
 ENDIF
 END SUBROUTINE angmom
 
 SUBROUTINE revangmom(ang,m)
 IMPLICIT NONE
 INTEGER, INTENT(INOUT) :: m
 !f2py intent(in,out) :: m
 CHARACTER(LEN=1), INTENT(OUT) :: ang
 IF (m == 0) THEN
   ang = "S"
 ELSEIF (m == 1) THEN
   ang = "P"
 ELSEIF (m == 2) THEN
   ang = "D"
 ELSEIF (m == 3) THEN
   ang = "F"
 ENDIF
 END SUBROUTINE revangmom

 SUBROUTINE atmnmtrunc(name)
 IMPLICIT NONE
 CHARACTER(LEN=15), INTENT(INOUT) :: name
 !f2py intent(in,out) :: name
 IF (name == "HYDROGEN") THEN
   name = "H"
 ELSEIF (name == "CARBON") THEN
   name = "C"
 ELSEIF (name == "OXYGEN") THEN
   name = "O"
 ELSEIF (name == "CERIUM") THEN
   name = "Ce"
 ENDIF
 END SUBROUTINE atmnmtrunc

 SUBROUTINE revatmnmtrunc(name)
 IMPLICIT NONE
 CHARACTER(LEN=15), INTENT(INOUT) :: name
 !f2py intent(in,out) :: name
 IF (name == "H") THEN
   name = "HYDROGEN"
 ELSEIF (name == "C") THEN
   name = "CARBON"
 ELSEIF (name == "O") THEN
   name = "OXYGEN"
 ELSEIF (name == "Ce") THEN
   name = "CERIUM"
 ENDIF
 END SUBROUTINE revatmnmtrunc

 SUBROUTINE nblkcnt_nwc(count)
 IMPLICIT NONE
 INTEGER :: i,ios,j
 INTEGER, INTENT(OUT) :: count
 REAL :: test
 count=0
 A:DO i=1,100
      READ(22,*,iostat=ios) test
      IF (ios == 0) THEN
         count=count+1
      ELSE
         EXIT A
      ENDIF
   ENDDO A
   DO j=1,count+1
      backspace(22)
   ENDDO
 END SUBROUTINE nblkcnt_nwc

 SUBROUTINE read_file_gam_gen(bsize,finp)
 USE vars
 IMPLICIT NONE
 INTEGER :: i,j,k,cnt=0,cnt1=0,lblk,ndim
 INTEGER, INTENT(IN) :: bsize
 CHARACTER(LEN=80), INTENT(IN) :: finp
 LOGICAL :: CHECK

 OPEN(11,FILE="INTERMEDIATE")
 OPEN(22,FILE=TRIM(ADJUSTL(finp)))
 WRITE(11,*) bsize
 DO k=1,bsize
   READ(22,*) atom,nblk
   
   READ(22,*) lblk
   WRITE(11,*) TRIM(ADJUSTL(atom)), nblk+lblk
   OPEN(12,FILE="temp")
   ALLOCATE(g(nblk))
   DO i=1,nblk
     READ(22,*) g(i)%lmax,ndim
     g(i)%ngbs=ndim
     CALL angchk(g(i)%lmax,check)
     IF (g(i)%lmax == "L") THEN
       g(i)%lmax = "S"
     ENDIF
     WRITE(11,*) g(i)%lmax,ndim
     IF (check .eqv. .TRUE.) THEN
       ALLOCATE(g(i)%fgbs(ndim,4))
       g(i)%lmax = "P"
       WRITE(12,*) g(i)%lmax, ndim
     ELSE
       ALLOCATE(g(i)%fgbs(ndim,3))
     ENDIF
     DO j=1,ndim
       READ(22,*) g(i)%fgbs(j,:)
       WRITE(11,*) j,g(i)%fgbs(j,2),g(i)%fgbs(j,3)
       IF (g(i)%lmax == "P") THEN
         WRITE(12,*) j,g(i)%fgbs(j,2),g(i)%fgbs(j,4)
       ENDIF
     ENDDO
     CALL Lprint(g(i)%lmax,lblk,ndim,cnt,cnt1)
   ENDDO
   WRITE(11,*)
   DEALLOCATE(g)
 ENDDO
 CLOSE(11)
 CLOSE(12)
 CLOSE(22)
 END SUBROUTINE read_file_gam_gen

 SUBROUTINE read_file_gau_gen(bsize,finp)
 USE vars
 IMPLICIT NONE
 INTEGER :: i,j,k,cnt=0,cnt1=0,lblk,ndim
 INTEGER, INTENT(IN) :: bsize
 CHARACTER(LEN=80), INTENT(IN) :: finp
 REAL :: junk
 LOGICAL :: check
 OPEN(11,FILE="INTERMEDIATE")
 OPEN(22,FILE=TRIM(ADJUSTL(finp)))
 WRITE(11,*) bsize
 DO k=1,bsize
   READ(22,*) atom,nblk
   CALL revatmnmtrunc(atom)
   READ(22,*) lblk
   WRITE(11,*) TRIM(ADJUSTL(atom)), nblk+lblk
   OPEN(12,FILE="temp")
   ALLOCATE(g(nblk))
   DO i=1,nblk
     READ(22,*) g(i)%lmax,ndim,junk
     g(i)%ngbs=ndim
     CALL angchk(g(i)%lmax,check)
     IF (g(i)%lmax == "SP") THEN
        g(i)%lmax = "S"
     ENDIF
     WRITE(11,*) g(i)%lmax,ndim
     IF (check .EQV. .TRUE.) THEN
        ALLOCATE(g(i)%fgbs(ndim,3))
        g(i)%lmax = "P"
        WRITE(12,*) g(i)%lmax,ndim
     ELSE
        ALLOCATE(g(i)%fgbs(ndim,2))
     ENDIF
     DO j=1,ndim
        READ(22,*) g(i)%fgbs(j,:)
        WRITE(11,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,2)
        IF (g(i)%lmax == "P") THEN
           WRITE(12,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,3)
        ENDIF
     ENDDO
     CALL Lprint(g(i)%lmax,lblk,ndim,cnt,cnt1)
  ENDDO
  WRITE(11,*)
  DEALLOCATE(g)
 ENDDO
 CLOSE(11)
 CLOSE(12)
 CLOSE(22)
 END SUBROUTINE read_file_gau_gen

 SUBROUTINE read_file_nwc_gen(bsize,finp)
 USE vars
 IMPLICIT NONE
 INTEGER :: i,j,k,lblk,ndim,cnt=0,cnt1=0
 CHARACTER(LEN=2) :: ang
 LOGICAL :: check
 INTEGER, INTENT(IN) :: bsize
 CHARACTER(LEN=80), INTENT(IN) :: finp
 OPEN(11,FILE="INTERMEDIATE")
 OPEN(22,FILE=TRIM(ADJUSTL(finp)))
 DO k=1,bsize
    READ(22,*) nblk,lblk
    READ(22,*) atom,ang
    CALL revatmnmtrunc(atom)
    BACKSPACE(22)
    IF (k == 1) THEN
       WRITE(11,*) bsize
    ENDIF
    WRITE(11,*) TRIM(ADJUSTL(atom)), nblk+lblk
    ALLOCATE(g(nblk))
    DO i=1,nblk
       READ(22,*) atom, g(i)%lmax
       CALL nblkcnt_nwc(ndim)
       g(i)%ngbs=ndim
       CALL angchk(g(i)%lmax,check)
       IF (g(i)%lmax == "SP") THEN
          g(i)%lmax = "S"
       ENDIF
       WRITE(11,*) g(i)%lmax,ndim
       IF (check .EQV. .TRUE.) THEN
          ALLOCATE(g(i)%fgbs(ndim,3))
          g(i)%lmax = "P"
          WRITE(12,*) g(i)%lmax, ndim
       ELSE
          ALLOCATE(g(i)%fgbs(ndim,2))
       ENDIF
       DO j=1,ndim
          READ(22,*) g(i)%fgbs(j,:)
          WRITE(11,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,2)
          IF (g(i)%lmax == "P") THEN
             WRITE(12,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,3)
          ENDIF
       ENDDO
       CALL Lprint(g(i)%lmax,lblk,ndim,cnt,cnt1)
    ENDDO
    WRITE(11,*)
    DEALLOCATE(g)
    IF (size >=2) THEN
       READ(22,*)
    ENDIF
  ENDDO
  CLOSE(11)
  CLOSE(12)
  CLOSE(22)
 END SUBROUTINE read_file_nwc_gen

 SUBROUTINE read_file_dem_gen(bsize,finp)
 USE vars
 IMPLICIT NONE
 INTEGER :: i,j,k,trash,m,ndim
 INTEGER, INTENT(IN) :: bsize
 CHARACTER(LEN=10) :: dummy,dummy2
 CHARACTER(LEN=1) :: ang
 CHARACTER(LEN=80), INTENT(IN) :: finp
 OPEN(11,FILE="INTERMEDIATE")
 OPEN(22,FILE=TRIM(ADJUSTL(finp)))
 WRITE(11,*) bsize
 DO k=1,bsize
    READ(22,*) dummy,atom,dummy2
    READ(22,*) nblk
    CALL revatmnmtrunc(atom)
    WRITE(11,*) atom,nblk
    ALLOCATE(g(nblk))
    DO i=1,nblk
       READ(22,*) trash,m,ndim
       CALL revangmom(ang,m)
       WRITE(11,*) ang,ndim
       g(i)%ngbs=ndim
       ALLOCATE(g(i)%fgbs(ndim,2))
       DO j=1,ndim
          READ(22,*) g(i)%fgbs(j,:)
          WRITE(11,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,2)
       ENDDO
    ENDDO
    WRITE(11,*)
    DEALLOCATE(g)
 ENDDO
 CLOSE(11)
 CLOSE(22)
 END SUBROUTINE read_file_dem_gen

 SUBROUTINE read_file_gen_gam(finp,output)
 USE vars
 IMPLICIT NONE
 INTEGER :: i,j,k,ndim,bsize
 CHARACTER(LEN=80), INTENT(IN) :: finp,output
 100 FORMAT(1X,A1,1X,I1)
 110 FORMAT(4X,I1,3X,2E20.10)
 OPEN(13,FILE=TRIM(ADJUSTL(output)))
 OPEN(11,FILE="INTERMEDIATE")
 READ(11,*) bsize
 DO k=1,bsize
   READ(11,*) atom,nblk
   WRITE(13,*) atom
   ALLOCATE(g(nblk))
   DO i=1,nblk
     READ(11,*) g(i)%lmax,ndim
     WRITE(13,100) TRIM(ADJUSTL(g(i)%lmax)),ndim
     g(i)%ngbs=ndim
     ALLOCATE(g(i)%fgbs(ndim,3))
     DO j=1,ndim
       READ(11,*) g(i)%fgbs(j,:)
       WRITE(13,110) j,g(i)%fgbs(j,2),g(i)%fgbs(j,3)
     ENDDO
   ENDDO
   WRITE(13,*)
   DEALLOCATE(g)
   IF (k <= size-1) THEN
     READ(11,*)
   ENDIF
 ENDDO
 CLOSE(11)
 CLOSE(13)
 END SUBROUTINE read_file_gen_gam

 SUBROUTINE read_file_gen_gau(finp,output)
 USE vars
 IMPLICIT NONE
 INTEGER :: i,j,k,bsize,ndim
 CHARACTER(LEN=80), INTENT(IN) :: finp,output
 100 FORMAT(1X,A1,1X,I1,1X,A)
 OPEN(13,FILE=TRIM(ADJUSTL(output)))
 OPEN(11,FILE="INTERMEDIATE")
 READ(11,*) bsize
 DO k=1,bsize
   READ(11,*) atom,nblk
   CALL atmnmtrunc(atom)
   WRITE(13,*) atom
   ALLOCATE(g(nblk))
   DO i=1,nblk
     READ(11,*) g(i)%lmax,ndim
     WRITE(13,100) TRIM(ADJUSTL(g(i)%lmax)),ndim,"1.00"
     g(i)%ngbs=ndim
     ALLOCATE(g(i)%fgbs(ndim,3))
     DO j=1,ndim
       READ(11,*) g(i)%fgbs(j,:)
       WRITE(13,'(2E20.10)') g(i)%fgbs(j,2),g(i)%fgbs(j,3)
     ENDDO
   ENDDO
   DEALLOCATE(g)
   WRITE(13,*)
 ENDDO
 CLOSE(11)
 CLOSE(13)
 END SUBROUTINE read_file_gen_gau

 SUBROUTINE read_file_gen_nwc(finp,output)
 USE vars
 IMPLICIT NONE
 INTEGER :: i,j,k,bsize,ndim
 CHARACTER(LEN=80), INTENT(IN) :: finp,output
 100 format(1X,A2,1X,A1)
 OPEN(13,FILE=TRIM(ADJUSTL(output)))
 OPEN(11,FILE="INTERMEDIATE")
 READ(11,*) bsize
 DO k=1,bsize
   READ(11,*) atom,nblk
   CALL atmnmtrunc(atom)
   ALLOCATE(g(nblk))
   DO i=1,nblk
     READ(11,*) g(i)%lmax,ndim
     WRITE(13,100) TRIM(ADJUSTL(atom)),g(i)%lmax
     g(i)%ngbs=ndim
     ALLOCATE(g(i)%fgbs(ndim,3))
     DO j=1,ndim
       READ(11,*) g(i)%fgbs(j,:)
       WRITE(13,'(2E20.10)') g(i)%fgbs(j,2),g(i)%fgbs(j,3)
     ENDDO
   ENDDO
   DEALLOCATE(g)
   WRITE(13,*)
 ENDDO
 CLOSE(11)
 CLOSE(13)
 END SUBROUTINE read_file_gen_nwc
 
 SUBROUTINE read_file_gen_dem(finp,output)
 USE vars
 IMPLICIT NONE
 INTEGER :: i,j,k,m,temp,ndim,n=0,cntp=0,cntd=0,cntf=0,bsize
 CHARACTER(LEN=80), INTENT(IN) :: finp,output
 100 FORMAT(1X,I1,3X,I1,3X,I1)
 OPEN(13,FILE=TRIM(ADJUSTL(output)))
 OPEN(11,FILE="INTERMEDIATE")
 READ(11,*) bsize
 DO k=1,bsize
   READ(11,*) atom, nblk
   newatm=atom
   CALL atmnmtrunc(atom)
   WRITE(13,*) "O-",TRIM(ADJUSTL(newatm))," ",TRIM(ADJUSTL(atom))," (",TRIM(ADJUSTL(finp)),")"
   WRITE(13,*) nblk
   ALLOCATE(g(nblk))
   DO I=1,nblk
     READ(11,*) g(i)%lmax,ndim
     temp=ndim
     CALL angmom(g(i)%lmax,m)
     IF (g(i)%lmax == "P") THEN
       n=i-2
       n=i-n
       WRITE(13,100) n+cntp,m,ndim
       cntp=cntp+1
     ELSEIF (g(i)%lmax == "D") THEN
       n=i-3
       n=i-n
       WRITE(13,100) n+cntd,m,ndim
       cntd=cntd+1
     ELSEIF (g(i)%lmax == "F") THEN
       n=i-4
       n=i-n
       WRITE(13,100) n+cntf,m,ndim
       cntf=cntf+1
     ELSE
        WRITE(13,100) i,m,ndim
     ENDIF
     g(i)%ngbs=ndim
     ALLOCATE(g(i)%fgbs(ndim,3))
     DO j=1,ndim
        READ(11,*) g(i)%fgbs(j,:)
        WRITE(13,'(2E20.10)') g(i)%fgbs(j,2),g(i)%fgbs(j,3)
     ENDDO
  ENDDO
  WRITE(13,*)
  DEALLOCATE(g)
 ENDDO
 CLOSE(11)
 CLOSE(13)
 END SUBROUTINE read_file_gen_dem
