subroutine printmaxtrixscreen(array,mrow,mcol)
      implicit none
      real, intent(in) :: array(mrow,mcol)
      integer, intent(in) :: mrow,mcol
      integer :: k
      do k=1,mrow
         write(11,*) array(k,:)
      enddo
endsubroutine printmaxtrixscreen
