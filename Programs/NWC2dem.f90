! Title: Basis set changer (NWC2DEM)
! Date Created: Aug 19th,2016
! Date Last Modified: Aug 25th,2016
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
      elseif (name == "TITANIUM") then
         name = "Ti"
      elseif (name == "VANADIUM") then
         name = "V"
      elseif (name == "CHROMIUM") then
         name = "Cr"
      elseif (name == "MANGANESE") then
         name = "Mn"
      elseif (name == "IRON") then
         name = "Fe"
      elseif (name == "COBALT") then
         name = "Co"
      elseif (name == "NICKEL") then
         name = "Ni"
      elseif (name == "COPPER") then
         name = "Cu"
      elseif (name == "ZINC") then
         name = "Zn"
      elseif (name == "GALLIUM") then
         name = "Ga"
      elseif (name == "GERMANIUM") then
         name = "Ge"
      elseif (name == "ARSENIC") then
         name = "As"
      elseif (name == "SELENIUM") then
         name = "Se"
      elseif (name == "BROMINE") then
         name = "Br"
      elseif (name == "KRYPTON") then
         name = "Kr"
      elseif (name == "RUBIDIUM") then
         name = "Rb"
      elseif (name == "STRONTIUM") then
         name = "Sr"
      elseif (name == "YTTRIUM") then
         name = "Y"
      elseif (name == "ZIRCONIUM") then
         name = "Zr"
      elseif (name == "NIOBIUM") then
         name = "Nb"
      elseif (name == "MOLYBDENUM") then
         name = "Mo"
      elseif (name == "TECHNETIUM") then
         name = "Tc"
      elseif (name == "RUTHENIUM") then
         name = "Ru"
      elseif (name == "RHODIUM") then
         name = "Rh"
      elseif (name == "PALLADIUM") then
         name = "Pd"
      elseif (name == "SILVER") then
         name = "Ag"
      elseif (name == "CADMIUM") then
         name = "Cd"
      elseif (name == "INDIUM") then
         name = "In"
      elseif (name == "TIN") then
         name = "Sn"
      elseif (name == "ANTIMONY") then
         name = "Sb"
      elseif (name == "TELLURIUM") then
         name = "Te"
      elseif (name == "IODINE") then
         name = "I"
      elseif (name == "XENON") then
         name = "Xe"
      elseif (name == "CESIUM") then
         name = "Cs"
      elseif (name == "BARIUM") then
         name = "Ba"
      elseif (name == "LANTHANUM") then
         name = "La"
      elseif (name == "CERIUM") then
         name = "Ce"
      elseif (name == "PRASEODYMIUM") then
         name = "Pr"
      elseif (name == "NEODYMIUM") then
         name = "Nd"
      elseif (name == "PROMETHIUM") then
         name = "Pm"
      elseif (name == "SAMARIUM") then
         name = "Sm"
      elseif (name == "EUROPIUM") then
         name = "Eu"
      elseif (name == "GADOLINIUM") then
         name = "Gd"
      elseif (name == "TERBIUM") then
         name = "Tb"
      elseif (name == "DYSPROSIUM") then
         name = "Dy"
      elseif (name == "HOLMIUM") then
         name = "Ho"
      elseif (name == "ERBIUM") then
         name = "Er"
      elseif (name == "THULIUM") then
         name = "Tm"
      elseif (name == "YTTERBIUM") then
         name = "Yb"
      elseif (name == "LUTETIUM") then
         name = "Lu"
      elseif (name == "HAFNIUM") then
         name = "Hf"
      elseif (name == "TANTALUM") then
         name = "Ta"
      elseif (name == "TUNGSTEN") then
         name = "W"
      elseif (name == "RHENIUM") then
         name = "Re"
      elseif (name == "OSMIUM") then
         name = "Os"
      elseif (name == "IRIDIUM") then
         name = "Ir"
      elseif (name == "PLATINUM") then
         name = "Pt"
      elseif (name == "GOLD") then
         name = "Au"
      elseif (name == "MERCURY") then
         name = "Hg"
      elseif (name == "THALLIUM") then
         name = "Tl"
      elseif (name == "LEAD") then
         name = "Pb"
      elseif (name == "BISMUTH") then
         name = "Bi"
      elseif (name == "POLONIUM") then
         name = "Po"
      elseif (name == "ASTATINE") then
         name = "At"
      elseif (name == "RADON") then
         name = "Rn"
      elseif (name == "FRANCIUM") then
         name = "Fr"
      elseif (name == "RADIUM") then
         name = "Ra"
      elseif (name == "ACTINIUM") then
         name = "Ac"
      elseif (name == "THORIUM") then
         name = "Th"
      elseif (name == "PROACTINIUM") then
         name = "Pa"
      elseif (name == "URANIUM") then
         name = "U"
      elseif (name == "NEPTUNIUM") then
         name = "Np"
      elseif (name == "PLUTONIUM") then
         name = "Pu"
      elseif (name == "AMERICIUM") then
         name = "Am"
      elseif (name == "CURIUM") then
         name = "Cm"
      elseif (name == "BERKELIUM") then
         name = "Bk"
      elseif (name == "CALIFORNIUM") then
         name = "Cf"
      elseif (name == "EINSTEINIUM") then
         name = "Es"
      elseif (name == "FERMIUM") then
         name = "Fm"
      elseif (name == "MENDELEVIUM") then
         name = "Md"
      elseif (name == "NOBELIUM") then
         name = "No"
      elseif (name == "LAWRENCIUM") then
         name = "Lr"
      else
         name = "ERROR"
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
      elseif (name == "Ti") then
         name = "TITANIUM"
      elseif (name == "V") then
         name = "VANADIUM"
      elseif (name == "Cr") then
         name = "CHROMIUM"
      elseif (name == "Mn") then
         name = "MANGANESE"
      elseif (name == "Fe") then
         name = "IRON"
      elseif (name == "Co") then
         name = "COBALT"
      elseif (name == "Ni") then
         name = "NICKEL"
      elseif (name == "Cu") then
         name = "COPPER"
      elseif (name == "Zn") then
         name = "ZINC"
      elseif (name == "Ga") then
         name = "GALLIUM"
      elseif (name == "Ge") then
         name = "GERMANIUM"
      elseif (name == "As") then
         name = "ARSENIC"
      elseif (name == "Se") then
         name = "SELENIUM"
      elseif (name == "Br") then
         name = "BROMINE"
      elseif (name == "Kr") then
         name = "KRYPTON"
      elseif (name == "Rb") then
         name = "RUBIDIUM"
      elseif (name == "Sr") then
         name = "STRONTIUM"
      elseif (name == "Y") then
         name = "Yttrium"
      elseif (name == "Zr") then
         name = "ZIRCONIUM"
      elseif (name == "Nb") then
         name = "NIOBIUM"
      elseif (name == "Mo") then
         name = "MOLYBDENUM"
      elseif (name == "Tc") then
         name = "TECHNETIUM"
      elseif (name == "Ru") then
         name = "RUTHENIUM"
      elseif (name == "Rh") then
         name = "RHODIUM"
      elseif (name == "Pd") then
         name = "PALLADIUM"
      elseif (name == "Ag") then
         name = "SILVER"
      elseif (name == "Cd") then
         name = "CADMIUM"
      elseif (name == "In") then
         name = "INDIUM"
      elseif (name == "Sn") then
         name = "TIN"
      elseif (name == "Sb") then
         name = "ANTIMONY"
      elseif (name == "Te") then
         name = "TELLURIUM"
      elseif (name == "I") then
         name = "IODINE"
      elseif (name == "Xe") then
         name = "XENON"
      elseif (name == "Cs") then
         name = "CESIUM"
      elseif (name == "Ba") then
         name = "BARIUM"
      elseif (name == "La") then
         name = "LANTHANUM"
      elseif (name == "Ce") then
         name = "CERIUM"
      elseif (name == "Pr") then
         name = "PRASEODYMIUM"
      elseif (name == "Nd") then
         name = "NEODYMIUM"
      elseif (name == "Pm") then
         name = "PROMETHIUM"
      elseif (name == "Sm") then
         name = "SAMARIUM"
      elseif (name == "Eu") then
         name = "EUROPIUM"
      elseif (name == "Gd") then
         name = "GADOLINIUM"
      elseif (name == "Tb") then
         name = "TERBIUM"
      elseif (name == "Dy") then
         name = "DYSPROSIUM"
      elseif (name == "Ho") then
         name = "HOLMIUM"
      elseif (name == "Er") then
         name = "ERBIUM"
      elseif (name == "Tm") then
         name = "THULIUM"
      elseif (name == "Yb") then
         name = "YTTERBIUM"
      elseif (name == "Lu") then
         name = "LUTETIUM"
      elseif (name == "Hf") then
         name = "HAFNIUM"
      elseif (name == "Ta") then
         name = "TANTALUM"
      elseif (name == "W") then
         name = "TUNGSTEN"
      elseif (name == "Re") then
         name = "RHENIUM"
      elseif (name == "Os") then
         name = "OSMIUM"
      elseif (name == "Ir") then
         name = "IRIDIUM"
      elseif (name == "Pt") then
         name = "PLATINUM"
      elseif (name == "Au") then
         name = "GOLD"
      elseif (name == "Hg") then
         name = "MERCURY"
      elseif (name == "Tl") then
         name = "THALLIUM"
      elseif (name == "Pb") then
         name = "LEAD"
      elseif (name == "Bi") then
         name = "BISMUTH"
      elseif (name == "Po") then
         name = "POLONIUM"
      elseif (name == "At") then
         name = "ASTATINE"
      elseif (name == "Rn") then
         name = "RADON"
      elseif (name == "Fr") then
         name = "FRANCIUM"
      elseif (name == "Ra") then
         name = "RADIUM"
      elseif (name == "Ac") then
         name = "ACTINIUM"
      elseif (name == "Th") then
         name = "THORIUM"
      elseif (name == "Pa") then
         name = "PROTACTINIUM"
      elseif (name == "U") then
         name = "URANIUM"
      elseif (name == "Np") then
         name = "NEPTUNIUM"
      elseif (name == "Pu") then
         name = "PLUTONIUM"
      elseif (name == "Am") then
         name = "AMERICIUM"
      elseif (name == "Cm") then
         name = "CURIUM"
      elseif (name == "Bk") then
         name = "BERKELIUM"
      elseif (name == "Cf") then
         name = "CALIFORNIUM"
      elseif (name == "Es") then
         name = "EINSTEINIUM"
      elseif (name == "Fm") then
         name = "FERMIUM"
      elseif (name == "Md") then
         name = "MENDELEVIUM"
      elseif (name == "No") then
         name = "NOBELIUM"
      elseif (name == "Lr") then
         name = "LAWRENCIUM"
      else
         name = "ERROR"
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
      elseif (ang == "H") then
         m=5
      endif
    endsubroutine angmom
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
    subroutine read_extra
      implicit none
      integer :: ierr,j,m
      character(len=*), parameter :: search_str = "basis"
      character(len=1000) :: text
      character (len=10) :: word
      open(18,file="EXTRA")
      A:do j=1,100
         read(22,'(A)', iostat=ierr) text
         read(text,*,iostat=ierr) word
         if (word == search_str) then
            exit A
         endif
         write(18,'(A)') trim(adjustl(text))
      enddo A
      backspace(22)   
      close(18)
    endsubroutine read_extra
!-----------------------------------------------------------------------------
    subroutine fgbsprint(ndim,ang)
      implicit none
      real,allocatable,dimension(:,:) :: matrix
      integer, intent(in) :: ndim
      character(len=2), intent(in) :: ang
      integer :: i,j,k,l,col

      call colcnt(i)
      allocate(matrix(ndim,i))

      if (ang == "S") then
         write(12,*) ang, ndim*i-ndim
      elseif (ang == "P") then
         write(13,*) ang, ndim*i-ndim
      elseif (ang == "SP") then
         write(12,*) "S", ndim
         write(13,*) "P", ndim
      elseif (ang == "D") then
         write(14,*) ang, ndim*i-ndim
      elseif (ang == "F") then
         write(15,*) ang, ndim*i-ndim
      elseif (ang == "G") then
         write(16,*) ang, ndim*i-ndim
      !elseif (ang == "H") then
      !   write(17,*) ang, ndim*i-ndim
      endif

      do col=2,i
         do j=1,ndim
            read(22,*) matrix(j,:)
            if (ang == "S") then
               write(12,*) j+(col-2)*ndim,matrix(j,1),matrix(j,col)
            elseif (ang == "P") then
               write(13,*) j+(col-2)*ndim,matrix(j,1),matrix(j,col)
            elseif (ang == "D") then
               write(14,*) j+(col-2)*ndim,matrix(j,1),matrix(j,col)
            elseif (ang == "F") then
               write(15,*) j+(col-2)*ndim,matrix(j,1),matrix(j,col)
            elseif (ang == "G") then
               write(16,*) j+(col-2)*ndim,matrix(j,1),matrix(j,col)
       !     elseif (ang == "H") then
       !        write(17,*) j+(col-2)*ndim,matrix(j,1),matrix(j,col)
            endif
            if (col == 2) then
               if (ang == "SP") then
                  write(12,*) j,matrix(j,1),matrix(j,2)
                  write(13,*) j,matrix(j,1),matrix(j,3)
               endif
            endif
         enddo
         do k=1,ndim
            backspace(22)
         enddo
      enddo

      do l=1,ndim
         read(22,*)
      enddo

      deallocate(matrix)
    endsubroutine fgbsprint
!-----------------------------------------------------------------------------
    subroutine read_file_nwc_gen
      implicit none
      integer :: i,j,k,m,lblk,ndim,fsize,n,ierr
      integer :: cnt,cnt1,scnt
      integer :: s,p,d,f,l,Gb,Hb
      character(len=2) :: ang
      character(len=10) :: ok
      character(len=50) :: text
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
         actv = .TRUE.
         !!! count how many blocks for each atom !!!
         call blkcnt_nwc(nblk,n)
         !!! count the number of blocks for each ang !!!
         call blkcnter_nwc(n,s,p,d,f,Gb,Hb,l,dirP)
         read(22,*) atom, ang
         backspace(22)
         !!! unshrink the atom name !!!
         call revatmnmtrunc(atom)
         if (atom == "ERROR") then
            backspace(22)
            read(22,*) atom, ang
            call revatmnmtrunc(atom)
            backspace(22)
         endif
         !!! find the number of lblks !!!
         call lblksize(lblk)
         !!! write the size of the basis set !!!
         if (k == 1) then
            write(11,*) size
         endif
         !!! write the number of blocks for each atom !!!
         write(11,*) trim(adjustl(atom)), nblk+lblk
         allocate(g(nblk))
         open(12,file="sINT")
         open(13,file="pINT")
         open(14,file="dINT")
         open(15,file="fINT")
         open(16,file="gINT")
        ! open(17,file="hINT")
         !!! print to the block intermediate files !!!
         do i=1,nblk
            read(22,*) atom, g(i)%lmax
            !print*, atom,g(i)%lmax
            call nblkcnt_nwc(ndim)
            g(i)%ngbs=ndim
            call fgbsprint(ndim,g(i)%lmax)
         enddo
         !!! rewind to the top of the files !!!
         rewind(12)
         rewind(13)
         rewind(14)
         rewind(15)
         rewind(16)
         !rewind(17)
         ierr=0
         !!! write s block !!!
         if (s /= 0 .or. l /= 0) then
            A:do j=1,1000
               read(12,'(A)', iostat=ierr) text
               if (ierr /= 0) exit A
               write(11,'(A)') trim(text)
            enddo A
         endif
         ierr=0
         !!! write p block !!!
         if (p /= 0 .or. l /= 0) then
            B:do j=1,1000
               read(13,'(A)', iostat=ierr) text
               if (ierr /= 0) exit B
               write(11,'(A)') trim(text)
            enddo B
         endif
         ierr=0
         !!! write d block !!!
         if (d /= 0) then
            C:do j=1,1000
               read(14,'(A)', iostat=ierr) text
               if (ierr /= 0) exit C
               write(11,'(A)') trim(text)
            enddo C
         endif
         ierr=0
         !!! write f block !!!
         if (f /= 0) then
            E:do j=1,1000
               read(15,'(A)', iostat=ierr) text
               if (ierr /= 0) exit E
               write(11,'(A)') trim(text)
            enddo E
         endif
         ierr=0
         !!! write g block !!!
         if (Gb /= 0) then
            H:do j=1,1000
               read(16,'(A)', iostat=ierr) text
               if (ierr /= 0) exit H
               write(11,'(A)') trim(text)
            enddo H
         endif
         ierr=0
         !!! write h block !!!
         !if (Hb /= 0) then
         !   Ib: do j=1,1000
         !      read(17,'(A)',iostat=ierr) text
         !      if(ierr /= 0) exit Ib
         !      write(11,'(A)') trim(text)
         !   enddo Ib
         !endif
         close(12)
         close(13)
         close(14)
         close(15)
         close(16)
         !close(17)
         !!! clear files !!!
         open(12,file="sINT")
         open(13,file="pINT")
         open(14,file="dINT")
         open(15,file="fINT")
         open(16,file="gINT")
         !open(17,file="hINT")
         write(12,*)
         write(13,*)
         write(14,*)
         write(15,*)
         write(16,*)
         !write(17,*)
         close(12)
         close(13)
         close(14)
         close(15)
         close(16)
         !close(17)

         write(11,*)
         deallocate(g)
         read(22,*) 
      enddo
      close(22)
      close(11)
    endsubroutine read_file_nwc_gen
!-----------------------------------------------------------------------------
    subroutine read_file_gen_dem
      implicit none
      integer :: i,j,k,size,temp,n,m,ndim,o,ierr
      integer :: cnts,cntp,cntd,cntf,cntg,cnth
      character(len=1000) :: text
100   format(1X,I2,3X,I1,3X,I3)
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
         cntg=0
         cnth=0
         
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
            elseif (g(i)%lmax == "G") then
               n=i-5
               n=i-n
               write(13,100) n+cntg,m,ndim
               cntg=cntg+1
           ! elseif (g(i)%lmax == "H") then
           !    n=i-6
           !    n=i-n
           !    write(13,100) n+cnth,m,ndim
           !    cnth=cnth+1
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
    subroutine colcnt(i)
      implicit none
      integer :: ierr
      integer,intent(out) :: i
      real :: R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15,R16,R17,R18,R19,R20
      character(len=1000) :: text
      read(22,'(A)') text
      backspace(22)
      text=trim(text)
      ierr=0
      A: do while (ierr == 0)
         i=0
         read(text,*,iostat=ierr) R1
         if (ierr /= 0) exit A
         i=1
         read(text,*,iostat=ierr) R1,R2
         if (ierr /= 0) exit A
         i=2
         read(text,*,iostat=ierr) R1,R2,R3
         if (ierr /= 0) exit A
         i=3
         read(text,*,iostat=ierr) R1,R2,R3,R4
         if (ierr /= 0) exit A
         i=4
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5
         if (ierr /= 0) exit A
         i=5
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6
         if (ierr /= 0) exit A
         i=6
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7
         if (ierr /= 0) exit A
         i=7
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8
         if (ierr /= 0) exit A
         i=8
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8,R9
         if (ierr /= 0) exit A
         i=9
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8,R9,R10
         if (ierr /= 0) exit A
         i=10
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11
         if (ierr /= 0) exit A
         i=11
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12
         if (ierr /= 0) exit A
         i=12
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13
         if (ierr /= 0) exit A
         i=13
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14
         if (ierr /= 0) exit A
         i=14
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15
         if (ierr /= 0) exit A
         i=15
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15,R16
         if (ierr /= 0) exit A
         i=16
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15,R16,R17
         if (ierr /= 0) exit A
         i=17
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15,R16,R17,R18
         if (ierr /= 0) exit A
         i=18
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15,R16,R17,R18,R19
         if (ierr /= 0) exit A
         i=19
         read(text,*,iostat=ierr) R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14,R15,R16,R17,R18,R19,R20
         if (ierr /= 0) exit A
         i=20
      enddo A
    endsubroutine colcnt
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
      do i=0,j
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
    subroutine blkcnter_nwc(k,s,p,d,f,Gb,Hb,l,dirP)
      implicit none
      character(len=10) :: atom
      character(len=2) :: ang
      integer :: i,j,ierr,m,n
      integer, intent(out) :: s,p,d,f,l,Gb,Hb
      integer, intent(in) :: k
      logical, intent(out) :: dirP
      s=0
      p=0
      d=0
      f=0
      Gb=0
      Hb=0
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
         elseif (ang == "H") then
            Hb=Hb+1
         endif
      enddo
      if (m == 1) then
         dirP = .TRUE.
      elseif (n == 1) then
         dirP = .FALSE.
      endif

      do j=0,k
         backspace(22)
      enddo
    endsubroutine blkcnter_nwc
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
