subroutine filesize(i)
      implicit none
      integer :: ierr 
      integer, intent(out) :: i
      open(11,file=trim(adjustl(finp)))
      i=0
      A:do
         read(11,*,iostat=ierr)
         i=i+1         
         if (ierr /= 0) exit A
      enddo A
      i=i-1
      close(11)
endsubroutine filesize
