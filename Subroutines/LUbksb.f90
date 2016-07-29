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
