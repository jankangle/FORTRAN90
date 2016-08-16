! Title: Inverse
! Date Created: July 29,2016
! Date Last Modified: July 29,2016
! Version: 1.0
!
! Author: Jonathan Kung
! University of Calgary
! Purpose: Calculate the inverse of a matrix

module vars
  implicit none
  character(len=80) :: finp !file name input
contains
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

  subroutine inv
    implicit none
    integer :: i,m
    real, allocatable, dimension(:,:) :: matrix,temp,newmat
    integer, allocatable, dimension(:,:) :: ident
    integer, allocatable, dimension(:) :: indx
    real, allocatable, dimension(:) :: b
    real :: d
    write(6,*) "Indicate the size of the nxn matrix"
    read(5,*) m

    allocate(matrix(m,m))
    call createident(m,ident)

    open(11,file=trim(adjustl(finp)))
    read(11,*) matrix

    allocate(b(m))
    allocate(indx(m))
    allocate(newmat(m,m))
    
    temp=matrix
    do i=1,m
       b(:) = ident(:,i)
       call ludcmp(matrix,indx,d)
       call lubksb(matrix,indx,b)
       newmat(:,i) = b
       matrix=temp
    enddo
    
    call printmatrixscreen(newmat,m,m)


    deallocate(indx)
    deallocate(matrix)
    deallocate(newmat)
    deallocate(b)
  endsubroutine inv

  subroutine printmatrixscreen(array,mrow,mcol)
    implicit none
    real, intent(in) :: array(mrow,mcol)
    integer, intent(in) :: mrow,mcol
    integer :: k
    do k=1,mrow
       write(6,*) array(k,:)
    enddo
  endsubroutine printmatrixscreen

  
endmodule vars



program inverse
  use vars
  implicit none
  call read_input
  call inv
  write(6,*) "Done!"
endprogram inverse
