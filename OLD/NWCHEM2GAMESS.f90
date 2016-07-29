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
    character(len=15) :: atom !atom name
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

    subroutine revatmnmtrunc(name)
      implicit none
      character(len=15), intent(inout) :: name
      if (name == "H") then
         name = "HYDROGEN"
      elseif (name == "C") then
         name = "CARBON"
      elseif (name == "O") then
         name = "OXYGEN"
      elseif (name == "Ce") then
         name = "CERIUM"
      endif
    endsubroutine revatmnmtrunc

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

    subroutine revangmom(ang,m)
      implicit none
      integer, intent(inout) :: m
      character(len=1), intent(out) :: ang
      if (m == 0) then
         ang = "S"
      elseif (m == 1) then
         ang = "P"
      elseif (m == 2) then
         ang = "D"
      elseif (m == 3) then
         ang = "F"
      endif
    endsubroutine revangmom

    subroutine basissize(size)
      implicit none
      integer, intent(out) :: size
      write(6,*) "Indicate Basis size:"
      read(5,*) size !Indicate the number of atoms
    endsubroutine basissize
    
    subroutine chkbasissize(size,chksize)
      implicit none
      integer, intent(in) :: size
      logical, intent(out) :: chksize
      if (size > 1) then
         chksize = .TRUE.
      else
         chksize = .FALSE.
      endif
    endsubroutine chkbasissize

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
      integer :: i,j,k,ndim,m,lblk,trash,ios,l,n,count
      real :: test
      character(len=1) :: ang
      logical :: chksize
      call basissize(size)
      open(11,file="OUTPUT")
      write(11,*) "BASIS FILE: ", trim(adjustl(finp))
      open(22,file=trim(adjustl(finp)))
      do k=1,size
         read(22,*) nblk
         read (22,*)  atom,ang
         call revatmnmtrunc(atom)
         backspace(22)
         write(11,*) atom
         allocate(g(nblk))
         do i=1,nblk
            count=0
            read (22,*)  atom,ang
            A:do l=1,100
               read(22,*,iostat=ios) test
               if (ios == 0) then
                  count=count+1
               else
                  exit A
               endif
            enddo A               
            
            do n=1,count+1
               backspace(22)
            enddo
            ndim=count
            write(11,*) ang,ndim
            g(i)%ngbs=ndim
            allocate(g(i)%fgbs(ndim,2))
            
            do j=1,ndim
               read(22,*) g(i)%fgbs(j,:)
               write(11,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,2)
            enddo        
         enddo
         write(11,*)
         call chkbasissize(size,chksize)
         if (chksize .eqv. .TRUE.) then
            read(22,*)
         endif
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
