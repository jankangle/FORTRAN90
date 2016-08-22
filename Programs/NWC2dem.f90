! Title: Basis set changer (NWC2DEM)
! Date Created: Aug 19th,2016
! Date Last Modified: Aug 22th,2016
! Version: 1.0
!
! Author: Jonathan Kung
! University of Calgary
! Purpose: Change basis sets between different programs (NWC2DEM)
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
!-----------------------------------------------------------------------------
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
!-----------------------------------------------------------------------------
    subroutine atmnmtrunc(name)
      implicit none
      character(len=15), intent(inout) :: name
      if (name == "HYDROGEN") then
         name = "H"
      elseif (name == "HELIUM") then
         name = "He"
      elseif (name == "LITHIUM") then
         name = "Li"
      elseif (name == "BERYLLIUM") then
         name = "Be"
      elseif (name == "BORON") then
         name = "B"
      elseif (name == "CARBON") then
         name = "C"
      elseif (name == "NITROGEN") then
         name = "N"
      elseif (name == "OXYGEN") then
         name = "O"
      elseif (name == "FLUORINE") then
         name = "F"
      elseif (name == "NEON") then
         name = "Ne"
      elseif (name == "SODIUM") then
         name = "Na"
      elseif (name == "MAGNESIUM") then
         name = "Mg"
      elseif (name == "ALUMINIUM") then
         name = "Al"
      elseif (name == "SILICON") then
         name = "Si"
      elseif (name == "PHOSPHORUS") then
         name = "P"
      elseif (name == "SULFUR") then
         name = "S"
      elseif (name == "CHLORINE") then
         name = "Cl"
      elseif (name == "ARGON") then
         name = "Ar"
      elseif (name == "POTASSIUM") then
         name = "K"
      elseif (name == "CALCIUM") then
         name = "Ca"
      elseif (name == "SCANDIUM") then
         name = "Sc"
      elseif (name == "CERIUM") then
         name = "Ce"
      endif
    endsubroutine atmnmtrunc
!-----------------------------------------------------------------------------
    subroutine revatmnmtrunc(name)
      implicit none
      character(len=15), intent(inout) :: name
      if (name == "H") then
         name = "HYDROGEN"
      elseif (name == "He") then
         name = "HELIUM"
      elseif (name == "Li") then
         name = "LITHIUM"
      elseif (name == "Be") then
         name = "BERYLLIUM"
      elseif (name == "B") then
         name = "BORON"
      elseif (name == "C") then
         name = "CARBON"
      elseif (name == "N") then
         name = "NITROGEN"
      elseif (name == "O") then
         name = "OXYGEN"
      elseif (name == "F") then
         name = "FLUORINE"
      elseif (name == "Ne") then
         name = "NEON"
      elseif (name == "Na") then
         name = "SODIUM"
      elseif (name == "Mg") then
         name = "MAGNESIUM"
      elseif (name == "Al") then
         name = "ALUMINIUM"
      elseif (name == "Si") then
         name = "SILICON"
      elseif (name == "P") then
         name = "PHOSPHORUS"
      elseif (name == "S") then
         name = "SULFUR"
      elseif (name == "Cl") then
         name = "CHLORINE"
      elseif (name == "Ar") then
         name = "ARGON"
      elseif (name == "K") then
         name = "POTASSIUM"
      elseif (name == "Ca") then
         name = "CALCIUM"
      elseif (name == "Sc") then
         name = "SCANDIUM"
      elseif (name == "Ce") then
         name = "CERIUM"
      endif
    endsubroutine revatmnmtrunc
!-----------------------------------------------------------------------------
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
      elseif (ang == "G") then
         m=4
      endif
    endsubroutine angmom
!-----------------------------------------------------------------------------
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
      elseif (m == 4) then
         ang = "G"
      endif
    endsubroutine revangmom
!-----------------------------------------------------------------------------
    subroutine nblkcnt_nwc(count)
      implicit none
      integer :: i,ios,j
      integer, intent(out) :: count
      real :: test
      count=0
      A:do i=1,100
         read(22,*, iostat=ios) test
         if (ios == 0) then
            count=count+1
         else
            exit A
         endif
      enddo A
      do j=1,count+1
         backspace(22)
      enddo
    endsubroutine nblkcnt_nwc
!-----------------------------------------------------------------------------
    subroutine angchk(ang,check)
      implicit none
      character(len=2), intent(in) :: ang
      logical, intent(out) :: check
      if (ang == "SP" .or. ang == "L") then
         check = .TRUE.
      else
         check = .FALSE.
      endif
    endsubroutine angchk
!-----------------------------------------------------------------------------  
    subroutine Lprint(ang,lblk,ndim,i,j)
      implicit none
      integer, intent(in) :: lblk
      integer, intent(inout) :: ndim,i,j
      character(len=1), intent(inout) :: ang
      integer :: k,l
      real, allocatable, dimension(:,:) :: matrix
      allocate(matrix(1,3))
      
      if (i == 1) then
      else
         if (ang == "P") then
            j=j+1
         endif
         if (j == lblk) then
            i=i+1
            rewind(12)
            do k=1,lblk
               read(12,*) ang,ndim
               write(11,*) ang,ndim
               do l=1,ndim
                  read(12,*) matrix(j,:)
                  write(11,*) l,matrix(j,2), matrix(j,3)
               enddo
            enddo
         endif
      endif
      deallocate(matrix)
    endsubroutine Lprint
!-----------------------------------------------------------------------------
    subroutine read_extra
      implicit none
      integer :: ierr,j,m
      character(len=*), parameter :: search_str = "basis"
      character(len=1000) :: text
      character (len=10) :: word
      open(14,file="EXTRA")
      A:do j=1,50
         read(22,'(A)', iostat=ierr) text
         read(text,*,iostat=ierr) word
         if (word == search_str) then
            exit A
         endif
         write(14,'(A)') trim(adjustl(text))
      enddo A
      backspace(22)   
      close(14)
    endsubroutine read_extra
!-----------------------------------------------------------------------------
    subroutine read_file_nwc_gen
      implicit none
      integer :: i,j,k,m,lblk,ndim,fsize,n
      integer :: cnt,cnt1,scnt
      integer :: s,p,d,f,l,Gb,blah
      character(len=2) :: ang
      character(len=10) :: ok
      logical :: check,dirP,actv
      call filesize(fsize)
      call bsizecnt_nwc(fsize,size)
      open(11,file="INTERMEDIATE")
      open(22,file=trim(adjustl(finp)))
      call read_extra
      
      do k=1,size
         cnt=0
         cnt1=0
         scnt=0
         actv = .FALSE.
         call blkcnt_nwc(nblk,n)
         call blkcnter_nwc(n,s,p,d,f,Gb,l,dirP)
         read(22,*) atom, ang
         call revatmnmtrunc(atom)
         backspace(22)
         call lblksize(lblk)
         if (k == 1) then
            write(11,*) size
         endif
         write(11,*) trim(adjustl(atom)), nblk+lblk
         allocate(g(nblk))
         open(12,file="temp")
         do i=1,nblk
            if (p /= 0) then
               if (l /= 0) then
                  if (dirP .eqv. .TRUE.) then
                     if (i == s+1) then
                        call rearranger(actv)
                     endif
                  endif
               endif
            endif
            read(22,*) atom, g(i)%lmax
            call nblkcnt_nwc(ndim)
            g(i)%ngbs=ndim
            call angchk(g(i)%lmax,check)
         
            if (g(i)%lmax == "SP") then
               g(i)%lmax = "S"
            endif
            if (i == s+p+l .and. actv .eqv. .FALSE.) then
               write(11,*) g(i)%lmax, ndim
            endif

            if (check .eqv. .TRUE.) then
               allocate(g(i)%fgbs(ndim,3))
               g(i)%lmax = "P"
               write(12,*) g(i)%lmax, ndim
            else
               allocate(g(i)%fgbs(ndim,2))
            endif

            do j=1,ndim
               read(22,*) g(i)%fgbs(j,:)
               if (i == s+p+l .and. actv .eqv. .FALSE.) then
               write(11,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,2)
               endif
               if (check .eqv. .TRUE.) then
                  write(12,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,3)
               endif
            enddo
            deallocate(g(i)%fgbs)
            if (check .eqv. .TRUE.) then
               call Lprint(g(i)%lmax, lblk, ndim,cnt,cnt1)
            endif 
         enddo
         write(11,*)
         deallocate(g)
         read(22,*) 
      enddo
      close(22)
      close(11)
      close(12)
    endsubroutine read_file_nwc_gen
!-----------------------------------------------------------------------------
    subroutine read_file_gen_dem
      implicit none
      integer :: i,j,k,size,temp,n,m,cnts,cntp,cntd,cntf,ndim,o,ierr
      character(len=1000) :: text
100   format(1X,I2,3X,I1,3X,I2)
101   format(2A,10A,1A,2A,2A,9A,1A)
      open(13,file="OUTPUT")
      open(11,file="INTERMEDIATE")
      open(14,file="EXTRA")
      A:do o=1,50
         read(14,'(A)',iostat=ierr) text
         write(13,'(A)') trim(adjustl(text))
         if (ierr /= 0) exit A
      enddo A
      read(11,*) size
      do k=1,size
         read(11,*) atom,nblk
         newatm=atom
         call atmnmtrunc(atom)
         write(13,101) "O-",trim(adjustl(newatm))," ", trim(adjustl(atom)), " (",trim(adjustl(finp)),")"
         write(13,*) nblk
         allocate(g(nblk))
         
         cnts=0
         cntp=0
         cntd=0
         cntf=0
         
         do i=1,nblk
            read(11,*) g(i)%lmax,ndim
            temp=ndim
            call angmom(g(i)%lmax,m)
            n=0
            if (g(i)%lmax == "S") then
               cnts=cnts+1
               write(13,100) cnts,m,ndim
            elseif (g(i)%lmax == "P") then
               n=i-2
               n=i-n
               write(13,100) n+cntp,m,ndim
               cntp=cntp+1
            elseif (g(i)%lmax == "D") then
               n=i-3
               n=i-n
               write(13,100) n+cntd,m,ndim
               cntd=cntd+1
            elseif (g(i)%lmax == "F") then
               n=i-4
               n=i-n
               write(13,100) n+cntf,m,ndim
               cntf=cntf+1
            endif
            
            g(i)%ngbs=ndim
            allocate(g(i)%fgbs(ndim,3))
            
            do j=1,ndim
               read(11,*) g(i)%fgbs(j,:)
               write(13,'(2E20.10)') g(i)%fgbs(j,2),g(i)%fgbs(j,3)
            enddo
         enddo
         write(13,*)
         deallocate(g)
      enddo
      close(13)
      close(11)
      close(14)
    endsubroutine read_file_gen_dem
!-----------------------------------------------------------------------------
    subroutine nwc2dem
      implicit none
      call read_file_nwc_gen
      call read_file_gen_dem
    endsubroutine nwc2dem
!-----------------------------------------------------------------------------
    subroutine blkcnt_nwc(blkcnt,j)
      implicit none
      character(len=5) :: search_str
      character(len=10) :: junk,atom,word
      character(len=1000) :: text
      integer :: count,ierr,i
      integer, intent(out) :: blkcnt,j
      read(22,*) junk
      read(22,*) atom,junk
      atom=trim(adjustl(atom))
      search_str = atom
      backspace(22)
      count=0
      j=0
      do while(text /= "end")
         read(22,*,iostat=ierr) text
         j=j+1
         text=trim(adjustl(text))
         read(text,*) word
         if (word == search_str) then
            count=count+1
         endif
      enddo
      blkcnt=count
      do i=1,j
         backspace(22)
      enddo
    endsubroutine blkcnt_nwc
!-----------------------------------------------------------------------------
    subroutine lblksize(lblkcnt)
      implicit none
      character(len=5), parameter :: search_str = "SP"
      character(len=10) :: junk,word,atom,ang
      character(len=1000) :: text
      integer :: count,ierr,i,j,k
      integer, intent(out) :: lblkcnt
      count=0
      j=0
      do while (junk /= "end")
         read(22,*,iostat=ierr) junk,word
         j=j+1
         word=trim(adjustl(word))
         if (word == search_str) then
            count=count+1
         endif
      enddo
      lblkcnt=count
      do i=1,j+1
         backspace(22)
      enddo
    endsubroutine lblksize
!-----------------------------------------------------------------------------
    subroutine bsizecnt_nwc(i,bsize)
      implicit none
      character(len=5), parameter :: search_str = "basis"
      character(len=1000) :: text
      character(len=10) :: word
      integer :: count=0,ierr,j
      integer, intent(in) :: i
      integer, intent(out) :: bsize

      open(22,file=trim(adjustl(finp)))
      do j=1,i
         read(22,*,iostat=ierr) text
         read(text,*) word
         if (word == search_str) then
            count=count+1
         endif
      enddo
      bsize=count
      close(22)
    endsubroutine bsizecnt_nwc
!-----------------------------------------------------------------------------
    subroutine filesize(i)
      implicit none
      integer :: ierr 
      integer, intent(out) :: i
      open(22,file=trim(adjustl(finp)))
      i=0
      A:do
         read(22,*,iostat=ierr)
         i=i+1         
         if (ierr /= 0) exit A
      enddo A
      i=i-1
      close(22)
    endsubroutine filesize
!-----------------------------------------------------------------------------
    subroutine wordfinder(i)
      implicit none
      integer :: ierr,j,m
      integer, intent(in) :: i
      character(len=*), parameter :: search_str = "basis"
      character(len=1000) :: text
      character (len=10) :: word
      m=0
      A:do j=1,i
         read(22,*, iostat=ierr) text
         read(text,*) word
         if (word == search_str) then
            exit A
         endif
      enddo A
      backspace(22)
    endsubroutine wordfinder
!------------------------------------------------------------------------------
    subroutine lchecker(ang,dirP,i,j)
      implicit none
      character(len=1), intent(in) :: ang
      integer, intent(in) :: i,j
      integer :: di,dj
      logical, intent(out) :: dirP
      di=i
      dj=j

      if (ang == "P") then
         di=di+1
         if (di == 1 .and. dj == 0) then
            dirP = .TRUE.
         endif
      elseif (ang == "SP") then
         dj=dj+1
         if (di == 0 .and. dj == 1) then
            dirP = .FALSE.
         endif
      endif
      
    endsubroutine lchecker
!------------------------------------------------------------------------------
    subroutine blkcnter_nwc(k,s,p,d,f,Gb,l,dirP)
      implicit none
      character(len=10) :: atom
      character(len=2) :: ang
      integer :: i,j,ierr,m,n
      integer, intent(out) :: s,p,d,f,l,Gb
      integer, intent(in) :: k
      logical, intent(out) :: dirP
      s=0
      p=0
      d=0
      f=0
      Gb=0
      l=0

      m=0 !p first
      n=0 !sp first

      do i=1,k
         read(22,*,iostat=ierr) atom,ang
         ang=trim(adjustl(ang))
         if (ang == "S") then
            s=s+1
         elseif (ang == "P") then
            p=p+1
            if (l == 0) then
               m=1
            endif
         elseif (ang == "D") then
            d=d+1
         elseif (ang == "F") then
            f=f+1
         elseif (ang == "SP") then
            l=l+1
            if (p == 0) then
               n=1
            endif
         elseif (ang == "G") then
            Gb=Gb+1
         endif
      enddo
      if (m == 1) then
         dirP = .TRUE.
      elseif (n == 1) then
         dirP = .FALSE.
      endif
      do j=1,k+1
         backspace(22)
      enddo
    endsubroutine blkcnter_nwc
!------------------------------------------------------------------------------
    subroutine rearranger(activate)
      implicit none
      character(len=5), parameter :: search_str = "SP"
      character(len=10) :: junk,word,atom
      character(len=2) :: ang
      character(len=1000) :: text
      integer :: ierr,i,j,k,ndim
      logical, intent(inout) :: activate
      real,allocatable,dimension(:,:) :: matrixa
      activate = .TRUE.
      j=0
      A: do while (junk /= "end")
         read(22,*,iostat=ierr) junk,word
         j=j+1
         word=trim(adjustl(word))
         if (word == search_str) exit A
      enddo A
      backspace(22)
      read(22,*) junk,ang
      call nblkcnt_nwc(ndim)
      allocate(matrixa(ndim,3))
      write(11,*) "S", ndim
      do k=1,ndim
         read(22,*) matrixa(k,:)
         write(11,*) k,matrixa(k,1),matrixa(k,2)
      enddo
      do i=1,j+1
         backspace(22)
      enddo
      deallocate(matrixa)
    endsubroutine rearranger
!------------------------------------------------------------------------------
  endmodule vars

program main
  use vars
  implicit none
  integer :: i,bsize
  call read_input
  call nwc2dem
  write(6,*) "Done!"
end program main
