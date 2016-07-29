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
