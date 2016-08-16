! Title: Gauss-Seidel Linear systems solver
! Date Created: May 4th,2016
! Date Last Modified: July 29th,2016 
! Version: 1.0
!
! Author: Jonathan Kung
! University of Calgary
! Purpose: Iteratively solve Linear systems of equations using Gauss-Seidel method



module vars
  implicit none
  character(len=80) :: finp
  
contains
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
  
  subroutine read_input
    implicit none
    integer :: i,ndim, iargc
    character(len=80),dimension(:),allocatable :: argv
    ndim=iargc()
    allocate(argv(ndim))
    do i=1,ndim
       call getarg(i,argv(i))
    enddo
    finp=trim(adjustl(argv(1))) !trim the file name
    deallocate(argv)
  endsubroutine read_input
  
  subroutine createident(m,ident)
    implicit none
    integer, intent(in) :: m
    integer, allocatable, dimension(:,:), intent(out) :: ident
    integer :: i,j
    
    allocate(ident(m,m))
    do i=1,m
       do j=1,m
          if (i==j) then
             ident(i,j) = 1
          else
             ident(i,j) = 0
          endif
       enddo
    enddo
  endsubroutine createident
  
  subroutine strictdcmp(input,L,U,size)
    implicit none
    real, dimension(:,:), intent(in) :: input
    integer, intent(in) :: size
    integer :: i,j
    real, dimension(:,:), allocatable, intent(out) :: L,U
    allocate(L(size,size))
    allocate(U(size,size))
    do i=1,size
       do j=1,size
          if (i <= j) then
             L(i,j) = input(i,j)
             U(i,j) = 0
          else
             L(i,j) = 0
             U(i,j) = input(i,j)
          endif
       enddo
    enddo
    !call printmatrixscreen(U,size,size)
  endsubroutine strictdcmp

  subroutine matsize(size)
    implicit none
    integer, intent(out) :: size
    write(6,*) "Input size of nxn matrix"
    read(5,*) size
  endsubroutine matsize

  subroutine printmatrixscreen(array,mrow,mcol)
    implicit none
    real, intent(in) :: array(mrow,mcol)
    integer, intent(in) :: mrow,mcol
    integer :: k

    if (mcol /= 1) then
       do k=1,mrow
          write(6,*) array(:,k)
       enddo
    else
       do k=1,mrow
          write(6,*) array(k,:)
       enddo
    endif
  endsubroutine printmatrixscreen

  subroutine inv(m,newmat,L,U)
    implicit none
    integer :: i
    real, allocatable, dimension(:,:) :: matrix,temp
    real, allocatable, dimension(:,:), intent(out) :: newmat,L,U
    integer, allocatable, dimension(:,:) :: ident
    integer, allocatable, dimension(:) :: indx
    integer, intent(in) :: m
    real, allocatable, dimension(:) :: b
    real :: d
    
    allocate(matrix(m,m))

    call createident(m,ident)
    open(11,file=trim(adjustl(finp)))
    read(11,*) matrix
    call strictdcmp(matrix,L,U,m)

    allocate(b(m))
    allocate(indx(m))
    allocate(newmat(m,m))
    
    temp=L
    do i=1,m
       b(:) = ident(:,i)
       call ludcmp(L,indx,d)
       call lubksb(L,indx,b)
       newmat(:,i) = b
       L=temp
    enddo    
    
    deallocate(indx)
    deallocate(matrix)
    deallocate(b)
  endsubroutine inv

  subroutine matmath(m)
    implicit none 
    real, allocatable, dimension(:,:) :: res,L,U,newmat
    integer :: count=0
    integer, intent(in) :: m
    real :: diff
    character(len=80) :: file_name
    real, allocatable, dimension(:) :: c,x,guess,d,old

    allocate(c(m))
    allocate(guess(m))
    allocate(res(m,m))

    call inv(m,newmat,L,U)
    call input_file_c(file_name)
    open(12,file=trim(adjustl(file_name)))
    read(12,*) c
    x=matmul(c,newmat)
    res=matmul(U,newmat)
    call input_file_guess(file_name)
    open(13,file=trim(adjustl(file_name)))
    read(13,*) guess
    diff=10
    allocate(d(m))
    do while (abs(diff) >= 0.0000001 .and. count < 1000)
       d=matmul(guess,res)
       old=guess
       guess=x-d
       diff=SUM(guess-old)
       count=count+1
       write(6,*) "Iteration Number:",count,guess
    enddo
    write(6,*) "Number of Iterations:", count
    write(6,*) "Final Answer"
    call printmatrixscreen(guess,m,1)
  endsubroutine matmath

  subroutine input_file_c(file_name)
    implicit none
    character(len=80), intent(out) :: file_name

    write(6,*) "Input file for vector c"
    read(5,*) file_name
  endsubroutine input_file_c

  subroutine input_file_guess(file_name)
    implicit none
    character(len=80), intent(out) :: file_name

    write(6,*) "Input file for guess"
    read(5,*) file_name
  endsubroutine input_file_guess

endmodule vars

  program GS
    use vars
    implicit none
    integer :: size

    call read_input
    call matsize(size)
    call matmath(size)
  end program GS
