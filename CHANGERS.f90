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
    subroutine printmaxtrixscreen(array,mrow,mcol)
      implicit none
      real, intent(in) :: array(mrow,mcol)
      integer, intent(in) :: mrow,mcol
      integer :: k
      do k=1,mrow
         write(11,*) array(k,:)
      enddo
    endsubroutine printmaxtrixscreen
!-----------------------------------------------------------------------------
    subroutine atmnmtrunc(name)
      implicit none
      character(len=15), intent(inout) :: name
      if (name == "HYDROGEN") then
         name = "H"
      elseif (name == "BORON") then
         name = "B"
      elseif (name == "CARBON") then
         name = "C"
      elseif (name == "OXYGEN") then
         name = "O"
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
      elseif (name == "C") then
         name = "CARBON"
      elseif (name == "O") then
         name = "OXYGEN"
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
      endif
    endsubroutine revangmom
!-----------------------------------------------------------------------------
    subroutine basissize(size)
      implicit none
      integer, intent(out) :: size
      write(6,*) "Indicate Basis size:"
      read(5,*) size !Indicate the number of atoms
    endsubroutine basissize
 !-----------------------------------------------------------------------------   
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
!-----------------------------------------------------------------------------
    subroutine lblksize(lblk,atom)
      implicit none
      integer, intent(out) :: lblk
      character(len=15), intent(in) :: atom
      write(6,*) "How many Ls for atom ", trim(adjustl(atom))," ?"
      read(5,*) lblk !reads the number of Ls
    endsubroutine lblksize
!-----------------------------------------------------------------------------
    subroutine lblkcnt_nwc(i)
      implicit none
      integer, intent(out) :: i
      integer :: j,k
      character(len=1) :: y
      character(len=2) :: l_ang
      i=0
      j=0
      do while (y /= "#")
         read(22,*) y,l_ang
         if (l_ang == "SP") then
            i=i+1
         endif
         j=j+1
      enddo
      do k=1,j
         backspace(22)
      enddo
    endsubroutine lblkcnt_nwc
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
    subroutine Lprint_1(ang,lblk,m,ndim,count,datcount)
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
    end subroutine Lprint_1
 !-----------------------------------------------------------------------------  
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
    subroutine read_file_gam_gen
      implicit none
      integer :: i,j,k,size,ndim,cnt,cnt1,lblk
      logical :: check
      call basissize(size)
      open(11,file="INTERMEDIATE")
      open(22,file=trim(adjustl(finp)))
      write(11,*) size
      do k=1,size
         cnt=0
         cnt1=0
         read(22,*) atom,nblk
         call lblksize(lblk,atom)
         write(11,*) trim(adjustl(atom)), nblk+lblk
         open(12,file="temp")
         allocate(g(nblk))
         do i=1,nblk
            read(22,*) g(i)%lmax,ndim
            g(i)%ngbs=ndim
            call angchk(g(i)%lmax,check)
            if (g(i)%lmax == "L") then
               g(i)%lmax = "S"
            endif
            write(11,*) g(i)%lmax, ndim
            if (check .eqv. .TRUE.) then
               allocate(g(i)%fgbs(ndim,4))
               g(i)%lmax = "P"
               write(12,*) g(i)%lmax, ndim
            else
               allocate(g(i)%fgbs(ndim,3))
            endif
            do j=1,ndim
               read(22,*) g(i)%fgbs(j,:)
               write(11,*) j,g(i)%fgbs(j,2),g(i)%fgbs(j,3)
               if (g(i)%lmax == "P") then
                  write(12,*) j,g(i)%fgbs(j,2),g(i)%fgbs(j,4)
               endif
            enddo
            call Lprint(g(i)%lmax,lblk,ndim,cnt,cnt1)
         enddo
         write(11,*)
         deallocate(g)
      enddo
      close(12)
      close(11)
      close(22)

    endsubroutine read_file_gam_gen
!-----------------------------------------------------------------------------
    subroutine read_file_gau_gen
      implicit none
      integer :: i,j,k,size,ndim,cnt,cnt1,lblk
      real :: junk
      logical :: check
      call basissize(size)
      open(11,file="INTERMEDIATE")
      open(22,file=trim(adjustl(finp)))
      write(11,*) size
      do k=1,size
         cnt=0
         cnt1=0
         read(22,*) atom,nblk
         call revatmnmtrunc(atom)
         call lblksize(lblk,atom)
         write(11,*) trim(adjustl(atom)), nblk+lblk
         open(12,file="temp")
         allocate(g(nblk))
         do i=1,nblk
            read(22,*) g(i)%lmax,ndim,junk
            g(i)%ngbs=ndim
            call angchk(g(i)%lmax,check)
            if (g(i)%lmax == "SP") then
               g(i)%lmax = "S"
            endif
            write(11,*) g(i)%lmax, ndim
            if (check .eqv. .TRUE.) then
               allocate(g(i)%fgbs(ndim,3))
               g(i)%lmax = "P"
               write(12,*) g(i)%lmax, ndim
            else
               allocate(g(i)%fgbs(ndim,2))
            endif
            do j=1,ndim
               read(22,*) g(i)%fgbs(j,:)
               write(11,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,2)
               if (g(i)%lmax == "P") then
                  write(12,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,3)
               endif
            enddo
            call Lprint(g(i)%lmax,lblk,ndim,cnt,cnt1)
         enddo
         write(11,*)
         deallocate(g)
      enddo
      close(12)
      close(11)
      close(22)

    endsubroutine read_file_gau_gen

!-----------------------------------------------------------------------------
    subroutine read_file_nwc_gen
      implicit none
      integer :: i,j,k,lblk,ndim
      integer :: cnt,cnt1
      character(len=2) :: ang
      logical :: check
      call basissize(size)
      open(11,file="INTERMEDIATE")
      open(22,file=trim(adjustl(finp)))
      
      do k=1,size
         cnt=0
         cnt1=0
         read(22,*) nblk
         read(22,*) atom, ang
         call revatmnmtrunc(atom)
         backspace(22)
         !call lblkcnt_nwc(lblk)
         call lblksize(lblk,atom)
         if (k == 1) then
            write(11,*) size
         endif
         write(11,*) trim(adjustl(atom)), nblk+lblk
         allocate(g(nblk))
         open(12,file="temp")
         do i=1,nblk
            read(22,*) atom, g(i)%lmax
            call nblkcnt_nwc(ndim)
            g(i)%ngbs=ndim
            call angchk(g(i)%lmax,check)
            if (g(i)%lmax == "SP") then
               g(i)%lmax = "S"
            endif
            write(11,*) g(i)%lmax, ndim

            if (check .eqv. .TRUE.) then
               allocate(g(i)%fgbs(ndim,3))
               g(i)%lmax = "P"
               write(12,*) g(i)%lmax, ndim
            else
               allocate(g(i)%fgbs(ndim,2))
            endif

            do j=1,ndim
               read(22,*) g(i)%fgbs(j,:)
               write(11,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,2)
               if (g(i)%lmax == "P") then
                  write(12,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,3)
               endif
            enddo

            call Lprint(g(i)%lmax, lblk, ndim,cnt,cnt1)
         enddo
         write(11,*)
         deallocate(g)
         if (size >= 2) then
            read(22,*)
         endif
      enddo
      close(22)
      close(11)
      close(12)
    endsubroutine read_file_nwc_gen
!-----------------------------------------------------------------------------
    subroutine read_file_gen_gam
      implicit none
      integer :: i,j,k,size,ndim

100   format(1X,A1,1X,I1)
110   format(4X,I1,3X,2E20.10)

      open(13,file="OUTPUT")
      open(11,file="INTERMEDIATE")
      read(11,*) size

      do k=1,size
         read(11,*) atom,nblk
         write(13,*) atom
         allocate(g(nblk))
         do i=1,nblk
            read(11,*) g(i)%lmax,ndim
            write(13,100) trim(adjustl(g(i)%lmax)),ndim
            g(i)%ngbs=ndim
            allocate(g(i)%fgbs(ndim,3))
            do j=1,ndim
               read(11,*) g(i)%fgbs(j,:)
               write(13,110) j,g(i)%fgbs(j,2),g(i)%fgbs(j,3)
            enddo
         enddo
         write(13,*)
         deallocate(g)
         if (k <= size-1) then
            read(11,*)
            !read(11,*)
         endif
      enddo
      close(13)
      close(11)

    endsubroutine read_file_gen_gam
!-----------------------------------------------------------------------------
    subroutine read_file_gen_dem
      implicit none
      integer :: i,j,k,size,temp,n,m,cntp,cntd,cntf,ndim

100   format(1X,I1,3X,I1,3X,I1)

      open(13,file="OUTPUT")
      open(11,file="INTERMEDIATE")
      read(11,*) size
      do k=1,size
         read(11,*) atom,nblk
         newatm=atom
         call atmnmtrunc(atom)
         write(13,*) "O-",trim(adjustl(newatm))," ", trim(adjustl(atom)), " (",trim(adjustl(finp)),")"
         write(13,*) nblk
         allocate(g(nblk))
         
         cntp=0
         cntd=0
         cntf=0
         
         do i=1,nblk
            read(11,*) g(i)%lmax,ndim
            temp=ndim
            call angmom(g(i)%lmax,m)
            n=0
            if (g(i)%lmax == "P") then
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
            else
               write(13,100) i,m,ndim
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

    endsubroutine read_file_gen_dem
!-----------------------------------------------------------------------------
    subroutine read_file_gen_gau
      implicit none
      integer :: i,j,k,size,ndim

100   format(1X,A1,1X,I1,1X,A)

      open(13,file="OUTPUT")
      open(11,file="INTERMEDIATE")
      read(11,*) size
      
      do k=1,size
         read(11,*) atom,nblk
         call atmnmtrunc(atom)
         write(13,*) atom
         allocate(g(nblk))
         do i=1,nblk
            read(11,*) g(i)%lmax,ndim
            write(13,100) trim(adjustl(g(i)%lmax)),ndim,"1.00"
            g(i)%ngbs=ndim
            allocate(g(i)%fgbs(ndim,3))
            do j=1,ndim
               read(11,*) g(i)%fgbs(j,:)
               write(13,'(2E20.10)') g(i)%fgbs(j,2),g(i)%fgbs(j,3)
            enddo
         enddo
         deallocate(g)
         write(13,*)
      enddo
      close(13)
      close(11)
    endsubroutine read_file_gen_gau
!-----------------------------------------------------------------------------
    subroutine read_file_gen_nwc
      implicit none
      integer :: i,j,k,size,ndim

100   format(1X,A2,1X,A1)

      open(13,file="OUTPUT")
      open(11,file="INTERMEDIATE")
      read(11,*) size
      do k=1,size
         read(11,*) atom,nblk
         call atmnmtrunc(atom)
         allocate(g(nblk))
         do i=1,nblk
            read(11,*) g(i)%lmax,ndim
            write(13,100) trim(adjustl(atom)),g(i)%lmax
            g(i)%ngbs=ndim
            allocate(g(i)%fgbs(ndim,3))
            do j=1,ndim
               read(11,*) g(i)%fgbs(j,:)
               write(13,'(2E20.10)') g(i)%fgbs(j,2),g(i)%fgbs(j,3)
            enddo
         enddo
         deallocate(g)
         write(13,*)
      enddo
      close(13)
      close(11)
    endsubroutine read_file_gen_nwc
!-----------------------------------------------------------------------------
    subroutine read_file_dem_gen
      implicit none
      integer :: i,j,k,size,trash,m,ndim
      character(len=10) :: dummy,dummy2
      character(len=1) :: ang
      call basissize(size)
      open(11,file="INTERMEDIATE")
      write(11,*) size
      open(22,file=trim(adjustl(finp)))
      do k=1,size
         read(22,*) dummy,atom,dummy2
         read(22,*) nblk
         call revatmnmtrunc(atom)
         write(11,*) atom,nblk
         allocate(g(nblk))
         do i=1,nblk
            read(22,*) trash,m,ndim
            call revangmom(ang,m)
            write(11,*) ang,ndim

            g(i)%ngbs=ndim
            allocate(g(i)%fgbs(ndim,2))
            do j=1,ndim
               read(22,*) g(i)%fgbs(j,:)
               write(11,*) j,g(i)%fgbs(j,1),g(i)%fgbs(j,2)
            enddo
         enddo
         write(11,*)
         deallocate(g)
      enddo
      close(22)
      close(11)
    endsubroutine read_file_dem_gen

!-----------------------------------------------------------------------------
    subroutine initform(x)
      implicit none
      character(len=20) :: Initial
      integer, intent(inout) :: x
      write(6,*) "Initial Formats shown below..."
      write(6,*) "GAMESS(US), NWCHEM, GAUSSIAN, DEMON2K"
      write(6,*) "Input initial basis set format"
      read(5,*) Initial
      Initial = trim(adjustl(Initial))

      do while (x /= 0 .and. x /= 1 .and. x /= 2 .and. x /= 3)
         if (Initial == "GAMESS(US)") then
            x = 0
         elseif (Initial == "NWCHEM") then
            x = 1
         elseif (Initial == "GAUSSIAN") then
            x = 2
         elseif (Initial == "DEMON2K") then
            x = 3
         else
            write(6,*) "Choose a format below"
            write(6,*) "GAMESS(US), NWCHEM, GAUSSIAN, DEMON2K"
            read(5,*) Initial
             if (Initial == "GAMESS(US)") then
                x = 0
             elseif (Initial == "NWCHEM") then
                x = 1
             elseif (Initial == "GAUSSIAN") then
                x = 2
             elseif (Initial == "DEMON2K") then
                x = 3
             endif
          endif
       enddo
    endsubroutine initform
!-----------------------------------------------------------------------------
    subroutine finform(y)
      implicit none
      character(len=20) :: fin
      integer, intent(inout) :: y
      write(6,*) "Final Formats shown below..."
      write(6,*) "GAMESS(US), NWCHEM, GAUSSIAN, DEMON2K"
      write(6,*) "Input final basis set format"
      read(5,*) fin
      fin = trim(adjustl(fin))

      do while (y /= 0 .and. y /= 1 .and. y /= 2 .and. y /= 3)
         if (fin == "GAMESS(US)") then
            y = 0
         elseif (fin == "NWCHEM") then
            y = 1
         elseif (fin == "GAUSSIAN") then
            y = 2
         elseif (fin == "DEMON2K") then
            y = 3
         else
            write(6,*) "Choose a format below"
            write(6,*) "GAMESS(US), NWCHEM, GAUSSIAN, DEMON2K"
            read(5,*) fin
            if (fin == "GAMESS(US)") then
               y = 0
            elseif (fin == "NWCHEM") then
               y = 1
            elseif (fin == "GAUSSIAN") then
               y = 2
            elseif (fin == "DEMON2K") then
               y = 3
            endif
         endif
      enddo
    endsubroutine finform
!-----------------------------------------------------------------------------
   subroutine changers(x,y)
      implicit none
      integer, intent(inout) :: x,y
      if (x == y) then
         write(6,*) "Changing to the same format"
         write(6,*) "Nothing was done"
         write(6,*) "Program Exit"
         write(6,*) 
      elseif (x == 0 .and. y == 1) then
         call read_file_gam_gen
         call read_file_gen_nwc
      elseif (x == 0 .and. y == 2) then
         call read_file_gam_gen
         call read_file_gen_gau
      elseif (x == 0 .and. y == 3) then
         call read_file_gam_gen
         call read_file_gen_dem
      elseif (x == 1 .and. y == 0) then
         call read_file_nwc_gen
         call read_file_gen_gam
      elseif (x == 1 .and. y == 2) then
         call read_file_nwc_gen
         call read_file_gen_gau
      elseif (x == 1 .and. y == 3) then
         call read_file_nwc_gen
         call read_file_gen_dem
      elseif (x == 2 .and. y == 0) then
         call read_file_gau_gen
         call read_file_gen_gam
      elseif (x == 2 .and. y == 1) then
         call read_file_gau_gen
         call read_file_gen_nwc
      elseif (x == 2 .and. y == 3) then
         call read_file_gau_gen
         call read_file_gen_dem
      elseif (x == 3 .and. y == 0) then
         call read_file_dem_gen
         call read_file_gen_gam
      elseif (x == 3 .and. y == 1) then
         call read_file_dem_gen
         call read_file_gen_nwc
      elseif (x == 3 .and. y == 2) then
         call read_file_dem_gen
         call read_file_gen_gau
      endif
    endsubroutine changers
!-----------------------------------------------------------------------------
    subroutine deets
      implicit none
      integer :: x=-1
      character(len=1) :: ans
      write(6,*) "Prerequisite knowledge about input basis sets required"
      write(6,*) "Print required information: (Y)es or (N)o"
      read(5,*) ans
      do while (ans /= "N" .and. ans /= "Y")
         write(6,*) "Print required information: (Y)es or (N)o"
         read(5,*) ans
      enddo
      if (ans == "N") then
         write(6,*) "Information skipped"
         write(6,*) "Proceeding to main program"
         write(6,*)
         write(6,*)
      elseif (ans == "Y") then
         do while (x /= 0 .and. x /= 1 .and. x /= 2 .and. x /= 3)
            write(6,*) "Which program? (Insert number)"
            write(6,*) "(0) GAMESS(US), (1) NWCHEM, (2) GAUSSIAN, (3) DEMON2K"
            read(5,*) x
         enddo
         write(6,*)
         write(6,*) "Instructions:"
         if (x == 0) then
            !GAMESS(US) INFO
            write(6,*) "1. Insert the number of blocks per atom right after each atom's name (on the same line)"
         elseif (x == 1) then
            !NWCHEM INFO
            write(6,*) "1. Insert the number of blocks per atom before each atom's basis set (line before)"
            write(6,*) "2. Insert a '#' at the end of each atom's basis set"
            write(6,*)
         elseif (x == 2) then
            !GAUSSIAN INFO
            write(6,*) "1. Insert the number of blocks per atom before each atom's basis set (line before)"
         elseif (x == 3) then
            !DEMON2K INFO
            write(6,*) "No special format needed"
         endif
         write(6,*) 
         write(6,*) "Information finished"
         write(6,*) "Proceeding to main program"
         write(6,*)
         write(6,*)
      endif

    endsubroutine deets
!-----------------------------------------------------------------------------
  endmodule vars


program main
  use vars
  implicit none
  integer :: x,y
  x=-1
  y=-2
  call read_input
  call deets
  call initform(x)
  call finform(y)
  call changers(x,y)
  write(6,*) "Done!"
end program main
