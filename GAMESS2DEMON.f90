  module vars
    implicit none
    type gbs
       character(len=2) :: lmax
       integer :: ngbs
       real,dimension(:,:),allocatable :: fgbs
    end type gbs
    integer :: nblk,size
    type(gbs),dimension(:),allocatable :: g,h
    character(len=80) :: finp !file name input
    character(len=15) :: atom,newatm !atom name
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

    subroutine printmaxtrixscreen(array,mrow,mcol)
      implicit none
      real, intent(in) :: array(mrow,mcol)
      integer, intent(in) :: mrow,mcol
      integer :: k
      do k=1,mrow
         write(11,*) array(k,:)
      enddo
    endsubroutine printmaxtrixscreen

    subroutine atmnmtrunc(name)
      implicit none
      character(len=15), intent(inout) :: name
      if (name == "HYDROGEN") then
         name = "H"
      elseif (name == "CARBON") then
         name = "C"
      elseif (name == "OXYGEN") then
         name = "O"
      elseif (name == "CERIUM") then
         name = "Ce"
      endif
    endsubroutine atmnmtrunc

    subroutine angmom(ang,m)
      implicit none
      character(len=1), intent(inout) :: ang
      integer, intent(out) :: m
      if (ang == "S") then
         m=0
      elseif (ang == "P") then
         m=1
      elseif (ang == "L") then
         m=0
      elseif (ang == "D") then
         m=2
      elseif (ang == "F") then
         m=3
      endif
    endsubroutine angmom

    subroutine basissize(size)
      implicit none
      integer, intent(out) :: size
      write(6,*) "Indicate Basis size:"
      read(5,*) size !Indicate the number of atoms
    endsubroutine basissize
    
    subroutine lblksize(lblk,atom)
      implicit none
      integer, intent(out) :: lblk
      character(len=15), intent(in) :: atom
      write(6,*) "How many Ls for atom ", trim(adjustl(atom))," ?"
      read(5,*) lblk !reads the number of Ls
    endsubroutine lblksize

    subroutine Lprint(ang,lblk,m,ndim,count,datcount)
      implicit none
      integer, intent(in) :: lblk
      integer, intent(inout) :: m,ndim,count,datcount
      integer :: i,x,j
      real, allocatable, dimension (:,:) :: matrix
      character(len=1), intent(inout) :: ang
      allocate(matrix(1,2))
      if (count == 1) then
      else
         if (ang /= "L") then
            datcount=datcount+1
         endif
         if (ang == "L") then
            datcount=datcount+1
         endif
         !print*, datcount
         if (datcount .gt. lblk) then
            count=count+1
            rewind(12)
            do i=1,lblk
               read(12,*) x,m,ndim
               write(11,*) x,m,ndim
               do j=1,ndim
                  read(12,*) matrix(j,:)
                  write(11,*) matrix(j,1), matrix(j,2)
               enddo
            enddo
         endif
      endif
      deallocate(matrix)
    end subroutine Lprint
    
    subroutine specLprint(ang,lblk,m,ndim,count,datcount)
      implicit none
      integer, intent(in) :: lblk
      integer, intent(inout) :: m,ndim,count,datcount
      integer :: i,x,j
      real, allocatable, dimension (:,:) :: matrix
      character(len=1), intent(inout) :: ang
      allocate(matrix(1,2))

      if (count == 1) then
      else
         if (ang == "L") then
            datcount=datcount+1
         endif
         if (datcount == lblk) then
            count=count+1
            rewind(12)
            do i=1,lblk
               read(12,*) x,m,ndim
               write(11,*) x,m,ndim
               do j=1,ndim
                  read(12,*) matrix(j,:)
                  write(11,*) matrix(j,1), matrix(j,2)
               enddo
            enddo
         endif
      endif
      deallocate(matrix)    
    endsubroutine specLprint

    subroutine read_file
      implicit none
      integer :: i,j,ndim,m,lblk,k,n,cnt1,cnt2,x,temp,ncnt,ncnt1
      call basissize(size)
      open(11,file="OUTPUT")
      write(11,*) "BASIS FILE: ", trim(adjustl(finp))
      open(22,file=trim(adjustl(finp)))
      do k=1,size
         read (22,*) atom, nblk
         newatm=atom
         call lblksize(lblk,atom)
         call atmnmtrunc(atom)
         write(11,*) "O-",trim(adjustl(newatm))," ", trim(adjustl(atom)), " (",trim(adjustl(finp)),")"
         write(11,*) nblk+lblk
         allocate(g(nblk))
         cnt1=0
         cnt2=0
         ncnt=0
         ncnt1=0
         open(12,file="temp")
         rewind(12)
         do i=1,nblk
            read(22,*) g(i)%lmax,ndim
            temp=ndim
            call angmom(g(i)%lmax,m)
            ndim=temp
            if (g(i)%lmax == "D") then
               n=0
               n=i-3
               n=i-n
               write(11,*) n+cnt1,m,ndim 
               cnt1=cnt1+1                          
            elseif (g(i)%lmax == "F") then
               n=0
               n=i-4
               n=i-n
               write(11,*) n+cnt2,m,ndim
               cnt2=cnt2+1
            else
               write(11,*) i,m,ndim
            endif

            g(i)%ngbs=ndim

            if (g(i)%lmax == "L") then
               allocate(g(i)%fgbs(ndim,4))
            else
               allocate(g(i)%fgbs(ndim,3))
            endif

            if (g(i)%lmax == "L") then
               x=i+1
               write(12,*) x,m+1,ndim
            endif

            do j=1,ndim
               read(22,*) g(i)%fgbs(j,:)
               write(11,*) g(i)%fgbs(j,2),g(i)%fgbs(j,3)
               if (g(i)%lmax == "L") then
                  write(12,*) g(i)%fgbs(j,2),g(i)%fgbs(j,4)
               endif
            enddo
            
            call specLprint(g(i)%lmax,lblk,m,ndim,ncnt,ncnt1)
            
         enddo
         write(11,*)
         deallocate(g)
      enddo
      close(22)
      close(11)
    endsubroutine read_file
  end module vars

program main
  use vars
  implicit none
  call read_input
  call read_file
  write(6,*) "Done!"
end program main
