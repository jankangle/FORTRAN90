program zzz
implicit none
integer :: i,j,imax,k,count
real, dimension(:,:), allocatable :: initial,output,inv,M,U,L
real, dimension(:), allocatable :: c,x,d,guess,old
real :: diff

write(6,*) "Input size of matrix"
read(5,*) i
print*, "Size of matrix:",i
count=0
j=i
imax=i
k=i
allocate(initial(i,j))
allocate(c(i))
allocate(x(i))
allocate(inv(i,j))
allocate(d(i))
allocate(U(i,j))
allocate(M(i,j))
allocate(guess(i))
allocate(L(i,j))
allocate(old(i))

open(11,file="array.dat")
read(11,*) initial
write(6,*) "Initial Matrix:"
call printmatrixscreen(initial,i,i)
call strictdcmp(initial,L,U,i)
call inverse(L,output,i)
open(13,file="matrix.dat")
read(13,*) inv
open(17,file="c.dat")
read(17,*) c
write(6,*) "b vector:"
call printmatrixscreen(c,i,1)
x=matmul(inv,c)
open(14,file="U.dat")
read(14,*) U
U=transpose(U)
M=matmul(inv,U)
open(15,file="guess.dat")
read(15,*) guess
diff=10

do while (abs(diff) >= 0.0000001 .and. count < 1000)
   d=matmul(M,guess)
   old=guess
   guess=x-d
   diff=SUM(guess-old)
   count=count+1
   print*, "Iteration Number:",count,guess
enddo

print*, "Number of Iterations:", count
write(6,*) "Final Answer"
call printmatrixscreen(guess,i,1)

deallocate(old)
deallocate(L)
deallocate(guess)
deallocate(M)
deallocate(U)
deallocate(c)
deallocate(inv)
deallocate(initial)
deallocate(x)
deallocate(d)

close(17)
close(11)
close(15)
close(14)
close(13)
print*, "Done"
!------------------------------------------------------------
contains
  subroutine ludcmp(a,indx,d)
    use nrtype; use nrutil, only: assert_eq,imaxloc,nrerror,outerprod,swap
    implicit none
    real(SP), dimension(:,:), intent(inout) :: a
    integer(I4B), dimension(:), intent(out) :: indx
    real(SP), intent(out) :: d
    real(SP), dimension(size(a,1)) :: vv !vv stores the implicit scaling of each row
    real(SP), parameter :: tiny=1.0e-20_sp !a small number
    integer(I4B) :: j,n,imax
    n=assert_eq(size(a,1),size(a,2),size(indx), 'ludcmp')
    d=1.0 !no row interchanges yet
    vv=maxval(abs(a),dim=2) !loop over rows to get the implicit scaling information
    if (any(vv==0.0)) call nrerror('singular matrix in ludcmp')
    vv=1.0_sp/vv !save the scaling
    do j=1,n
       imax=(j-1)+imaxloc(vv(j:n)*abs(a(j:n,j))) !find  the pivot row
       if (j /= imax) then !do we need to interchange rows?
          call swap(a(imax,:),a(j,:)) !yes, do so...
          d=-d !... and change the parity of d
          vv(imax)=vv(j) !also change the scale factor
       endif
       indx(j)=imax
       if (a(j,j) == 0.0) a(j,j)=tiny
       a(j+1:n,j)=a(j+1:n,j)/a(j,j)
       a(j+1:n,j+1:n)=a(j+1:n,j+1:n)-outerprod(a(j+1:n,j),a(j,j+1:n))
    enddo
  endsubroutine ludcmp
!------------------------------------------------------------
  subroutine printmatrixsmall(array,mrow,mcol)
    implicit none
    real, intent(in) :: array(mrow,mcol)
    integer, intent(in) :: mrow,mcol
    integer :: k
    do k=1,mrow
       write(11,110) array(k,:)
    enddo
110 format(999F11.5,1X) !Match the size of the matrix
  end subroutine printmatrixsmall
!------------------------------------------------------------
  subroutine printmatrixscreen(array,mrow,mcol)
    implicit none
    real, intent(in) :: array(mrow,mcol)
    integer, intent(in) :: mrow,mcol
    integer :: k
    do k=1,mrow
       write(6,110) array(k,:)
    enddo
    110 format(999F11.5,1X) !Match the size of the matrix
  end subroutine printmatrixscreen
!------------------------------------------------------------
  subroutine printmatrixbig(array,mrow,mcol)
    implicit none
    real, intent(in) :: array(mrow,mcol)
    integer, intent(in) :: mrow,mcol
    integer :: k
    open(13,file="matrix.dat")
    do k=1,mrow
       write(13,110) array(:,k)
    enddo
    110 format(999F11.5,1X) !Match the size of the matrix
    close(13)
  end subroutine printmatrixbig
!------------------------------------------------------------
  subroutine lubksb(a,indx,b)
    use nrtype; use nrutil, only: assert_eq
    implicit none
    real(SP), dimension(:,:), intent(in) :: a
    integer(I4B), dimension(:), intent(in) :: indx
    real(SP), dimension(:), intent(inout) :: b
    integer(I4B) :: i,n,ii,ll
    real(SP) :: summ
    n=assert_eq(size(a,1),size(a,2),size(indx),'lubksb')
    ii=0
    do i=1,n
       ll=indx(i)
       summ=b(ll)
       b(ll)=b(i)
       if (ii /= 0) then
          summ=summ-dot_product(a(i,ii:i-1),b(ii:i-1))
       else if (summ /= 0.0) then
          ii=i
       endif
       b(i)=summ
    enddo
    do i=n,1,-1
       b(i)= (b(i)-dot_product(a(i,i+1:n),b(i+1:n)))/a(i,i)
    enddo
  endsubroutine lubksb
!---------------------------------------------------------------
  subroutine inverse(a,inv,lol)
    implicit none
    real, dimension(:,:), intent(in) :: a
    integer, intent(in) :: lol
    real, dimension(:,:), allocatable, intent(out) :: inv
    real, dimension(:,:), allocatable :: matrix
    integer, dimension(:), allocatable :: indx
    real, dimension(:,:), allocatable :: f,g
    real, dimension(:), allocatable :: b
    real :: d
    integer :: i,j,c,cmax,m,l
    i=lol
    m=i
    l=i
    j=i
    cmax=i
    allocate(f(i,j))
    allocate(g(i,j))
    allocate(inv(i,j))
    allocate(b(m))
    allocate(indx(l))
    open(12,file="transpose.dat")
    allocate(matrix(i,j))
    do i=1,cmax
       do j=1,cmax
          if (i==j) then
             matrix(i,j) = 1
          else
             matrix(i,j) = 0
          endif
       enddo
    enddo
    f=a
    g=a
    do c=1,cmax
       b(:) = matrix(:,c)
       call ludcmp(g,indx,d)
       call lubksb(g,indx,b)
       write(12,*) b
       g=f
    enddo
    close(12)
    deallocate(f)
    deallocate(g)
    deallocate(b)
    deallocate(indx)
    open(12,file="transpose.dat")
    read(12,*) inv
    inv= transpose(inv)
    call printmatrixbig(output,cmax,cmax) !Match the size of the matrix
    deallocate(matrix)
    deallocate(inv)
    close(12)
  endsubroutine inverse
!------------------------------------------------------------
  subroutine strictdcmp(input,L,U,size)
    implicit none
    real, dimension(:,:), intent(in) :: input
    integer, intent(in) :: size
    integer :: i,j,cmax
    real, dimension(:,:), intent(inout) :: L,U
    i=size
    j=i
    cmax=i
    do i=1,cmax
       do j=1,cmax
          if (i <= j)then
             L(i,j) = input(i,j)
             U(i,j) = 0
          else
             L(i,j) = 0
             U(i,j) = input(i,j)
          endif
       enddo
    enddo
    call printmatrixU(U,size,size)
  endsubroutine strictdcmp
!------------------------------------------------------------
  subroutine printmatrixU(array,mrow,mcol)
    implicit none
    real, intent(in) :: array(mrow,mcol)
    integer, intent(in) :: mrow,mcol
    integer :: k
    open(13,file="U.dat")
    do k=1,mrow
       write(13,110) array(:,k)
    enddo
    110 format(999F11.7,1X) !Match the size of the matrix
    close(13)
  end subroutine printmatrixU
!---------------------------------------------------------------
  subroutine printmatrixL(array,mrow,mcol)
    implicit none
    real, intent(in) :: array(mrow,mcol)
    integer, intent(in) :: mrow,mcol
    integer :: k
    open(13,file="L.dat")
    do k=1,mrow
       write(13,110) array(:,k)
    enddo
    110 format(999F11.7,1X) !Match the size of the matrix
    close(13)
  end subroutine printmatrixL
  !--------------------------------------------------------------
  subroutine connum(A,B,onorm,inorm,tnorm)
    implicit none
    real, dimension(:,:), intent(in) :: A,B
    real, intent(out) :: onorm, inorm, tnorm
    real :: onorm1, onorm2, inorm1, inorm2, tnorm1, tnorm2

    onorm1=maxval(SUM(abs(A), DIM=1))
    onorm2=maxval(SUM(abs(B), DIM=1))
    onorm= onorm1 * onorm2

    inorm1=maxval(SUM(abs(A), DIM=2))
    inorm2=maxval(SUM(abs(A), DIM=2))
    inorm= inorm1 * inorm2

    tnorm1=sqrt(sum(A**2.0))
    tnorm2=sqrt(sum(A**2.0))
    tnorm= tnorm1 * tnorm2
  endsubroutine connum
!---------------------------------------------------------------
  real function onenorm(A, norm)
    implicit none
    real,dimension(:,:) ,intent(in) :: A
    real, intent(out) :: norm
    norm=maxval(SUM(abs(A), DIM=1))
  endfunction onenorm
!---------------------------------------------------------------
  real function infnorm(A, norm)
    implicit none
    real, dimension(:,:), intent(in) :: A
    real, intent(out) :: norm
    norm=maxval(SUM(abs(A), DIM=2))
  endfunction infnorm
!---------------------------------------------------------------
  real function twonorm(A, norm)
    implicit none
    real, dimension(:,:), intent(in) :: A
    real, intent(out) :: norm
    norm=sqrt(sum(A**2.0))
  endfunction twonorm
!---------------------------------------------------------------
endprogram zzz
