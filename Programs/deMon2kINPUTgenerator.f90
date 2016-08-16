! Title: deMon2k Input Generator
! Date Created: Aug 1st,2016
! Date Last Modified: Aug 3rd,2016
! Version: 1.0
!
! Author: Jonathan Kung
! University of Calgary
! Purpose: Easily create a deMon2k input

module vars
  implicit none
  character(len=80) :: finp

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

  subroutine whitespace(space)
    implicit none
    integer, intent(in) :: space
    integer :: i,j
    do i=1,j
       write(6,*)
    enddo
  endsubroutine whitespace

  subroutine title
    implicit none
    character(len=50) :: ti
    write(6,*) "Input Title:"
    read(5,*) ti
    ti = trim(adjustl(ti))
    open(11,file=trim(adjustl(finp)))
    write(11,102) "TITLE",ti
102 format(A5,1X,50A)
  endsubroutine title

  subroutine VXCTYP
    implicit none
    integer :: ans1
    character(len=5) :: word,func
    do while (ans1 /= 1 .and. ans1 /=  2)
       write(6,*) "Choose vxctyp"
       write(6,*) "1) AUXIS (default)"
       write(6,*) "2) BASIS"
       read(5,*) ans1
    enddo
    call functional(func)
    if (ans1 == 1) then
       word = "AUXIS"
    else
       word = "BASIS"
    endif

 101 format(A6,1X,A5,1X,5A)
    write(11,101) "VXCTYP",trim(adjustl(word)),trim(adjustl(func))
  endsubroutine VXCTYP

  subroutine functional(func)
    implicit none
    character(len=5), intent(out) :: func
    write(6,*) "Input functional"
    read(5,*) func
  endsubroutine functional
  
  subroutine basis
    implicit none
    character(len=20) :: bas
    integer :: ans1=0
    do while (ans1 /= 1 .and. ans1 /= 2 .and. ans1 /= 3 .and. ans1 /= 4)
       write(6,*) "Input Basis set name"
       write(6,*) "1) DZVP"
       write(6,*) "2) TZVP"
       write(6,*) "3) TZVP-GGA"
       write(6,*) "4) Enter your own"
       read(5,*) ans1
    enddo

    if (ans1 == 1) then
       bas = "DZVP"
    elseif (ans1 == 2) then
       bas = "TZVP"
    elseif (ans1 == 3) then
       bas = "TZVP-GGA"
    elseif (ans1 == 4) then
       write(6,*) "Enter Basis set name"
       read(5,*) bas
       bas = trim(adjustl(bas))
    endif
    write(11,104) "BASIS (",trim(adjustl(bas)),")"
104 format(A7,19A,A1)
  endsubroutine basis

  subroutine ecps
    implicit none
    character(len=100) :: ecp
    integer :: ans1=0
    do while (ans1 /= 1 .and. ans1 /= 2)
       write(6,*) "Input ECP?"
       write(6,*) "1) Yes"
       write(6,*) "2) No (Default)"
       read(5,*) ans1
    enddo

    if (ans1 == 1) then
       write(6,*) "Input ECPS name"
       read(5,*) ecp
       write(11,105) "ECPS (",trim(adjustl(ecp)),")"
105    format(A6,30A,A1)
    endif
  endsubroutine ecps

  subroutine auxis
    implicit none
    character(len=10) :: aux
    write(6,*) "Input auxis name"
    read(5,*) aux
    write(11,106) "AUXIS (",trim(adjustl(aux)),")"
106 format(A7,10A,A1)
  endsubroutine auxis

  subroutine scftyp
    implicit none
    character(len=10) :: scf,tight,tol
    integer :: maxiter,ans1=0,ans2=0

    do while (ans1 /= 1 .and. ans1 /= 2 .and. ans1 /= 3)
       write(6,*) "Shell type?"
       write(6,*) "1) RKS (default for closed-shell systems)"
       write(6,*) "2) ROKS (default for open-shell systems)"
       write(6,*) "3) UKS"
       read(5,*) ans1
    enddo
    
    if (ans1 == 1) then
       scf = "RKS"
    elseif (ans1 == 2) then
       scf = "ROKS"
    elseif (ans1 == 3) then
       scf = "UKS"
    endif

    write(6,*) "Input Max interations (default is 100)"
    read(5,*) maxiter

    do while (ans2 /= 1 .and. ans2 /= 2)
       write(6,*) "Tighten settings"
       write(6,*) "1) TIGHTEN"
       write(6,*) "2) NOTIGHTEN (default)"
       read(5,*) ans2
    enddo
       
    if (ans2 == 1) then
       tight = "TIGHTEN"
    elseif (ans2 == 2) then
       tight = "NOTIGHTEN"
    endif

    write(6,*) "Input Tolerance"
    write(6,*) "1.0E-5 is default"
    read(5,*) tol

100 format(A6,1X,A3,1X,A4,I3,1X,A4,7A)
110 format(A6,1X,A3,1X,A9,1X,A4,I3,1X,A4,7A)

    if (tight == "TIGHTEN") then
       write(11,100) "SCFTYP",trim(adjustl(scf)),"MAX=",maxiter,"TOL=",trim(adjustl(tol))
    else
       write(11,110) "SCFTYP",trim(adjustl(scf)),trim(adjustl(tight)),"MAX=",maxiter,"TOL=",trim(adjustl(tol))
    endif
  endsubroutine scftyp

  subroutine shift
    implicit none
    integer :: ans1=0
    character(len=4) :: amnt

    do while (ans1 /= 1 .and. ans1 /= 2)
       write(6,*) "Input Shift?"
       write(6,*) "1) Yes"
       write(6,*) "2) No (default)"
       read(5,*) ans1
    enddo

    if (ans1 == 1) then
       write(6,*) "Input Shift amount"
       read(5,*) amnt
       write(11,98) "SHIFT",amnt
    endif
98 format(A5,1X,4A)
  end subroutine shift

  subroutine grid
    implicit none
    integer :: ans1=0,ans2=0
    character(len=6) :: fix

    do while (ans1 /= 1 .and. ans1 /= 2)
       write(6,*) "Fix grid?"
       write(6,*) "1) Yes"
       write(6,*) "2) No (default)"
       read(5,*) ans1
    enddo

120 format(A10,1X,A6)
    if (ans1 == 1) then
       do while (ans2 /= 1 .and. ans2 /= 2 .and. ans2 /= 3)
          write(6,*) "Fix grid"
          write(6,*) "1) MEDIUM"
          write(6,*) "2) COARSE"
          write(6,*) "3) FINE"
          read(5,*) ans2
       enddo
       if (ans2 == 1) then
          fix = "MEDIUM"
          write(11,120) "GRID FIXED",fix
       elseif (ans2 == 2) then
          fix = "COARSE"
          write(11,120) "GRID FIXED",fix
       elseif (ans2 == 3) then
          fix = "FINE"
          write(11,120) "GRID FIXED",fix
       endif
    endif
  end subroutine grid

  subroutine guess
    implicit none
    character(len=4) :: gue
    integer :: ans1=0
    
    do while (ans1 /= 1 .and. ans1 /= 2)
       write(6,*) "Guess what?"
       write(6,*) "1) TB (default)"
       write(6,*) "2) CORE"
       read(5,*) ans1
    enddo

    if (ans1 == 1) then
       gue = "TB"
       write(11,108) "GUESS", gue
    elseif (ans1 == 2 ) then
       gue = "CORE"
       write(11,108) "GUESS", gue
    endif
108 format(A5,1X,A4)
  end subroutine guess

  subroutine diis
    implicit none
    integer :: ans1=0
    character(len=3) :: ans
    
    do while(ans1 /= 1 .and. ans1 /= 2)
       write(6,*) "DIIS?"
       write(6,*) "1) ON (default)"
       write(6,*) "2) OFF"
       read(5,*) ans1
    enddo

    if (ans1 == 2) then
       write(11,109) "DIIS OFF"
    endif
109 format(A8)
  endsubroutine diis

  subroutine calc
    implicit none
    character(len=12) :: word,xyz
    integer :: maxiter,ans1=0,ans2=0
130 format(A12,1X,A9,1X,A4,I3)
    
    do while (ans1 /= 1 .and. ans1 /= 2)
       write(6,*) "Type of calculation?"
       write(6,*) "1) Single point energy (default)"
       write(6,*) "2) Optimization"
        read(5,*) ans1
    enddo

    if (ans1 == 2) then
       do while (ans2 /= 1 .and. ans2 /= 2)
          write(6,*) "Geometry type"
          write(6,*) "1) Cartesian"
          write(6,*) "2) Internal (z-matrix)"
          read(5,*) ans2
       enddo
       write(6,*) "Max number of iterations? (default is 50)"
       read(5,*) maxiter
    endif

    if (ans1 == 1) then
    elseif (ans2 == 2) then
       word = "OPTIMIZATION"
       if (ans2 == 1) then
          xyz = "CARTESIAN"
       elseif (ans2 == 2) then
          xyz = "INTERNAL"
       endif
       write(11,*) trim(adjustl(word)),trim(adjustl(xyz)),"MAX=",maxiter
    endif
111 format(12A,1X,12A,1X,4A,I3)
  endsubroutine calc

  subroutine geom
    implicit none
    character(len=10) :: word,word1
    integer :: ans1=0,ans2=0
140 format(A8,1X,A9,1X,A8)


    do while(ans1 /= 1 .and. ans1 /= 2)
       write(6,*) "ANGSTROM or BOHR?"
       write(6,*) "1) ANGSTROM"
       write(6,*) "2) BOHR"
       read(5,*) ans1
    enddo

    do while (ans2 /= 1 .and. ans2 /= 2)
       write(6,*) "Type of geometry input"
       write(6,*) "1) CARTESIAN"
       write(6,*) "2) ZMATRIX"
       read(5,*) ans2
    enddo

   if (ans1 == 1) then
      word = "ANGSTROM"
   elseif (ans1 == 2) then
      word = "BOHR"   
   endif

   if (ans2 == 1) then
      word1 = "CARTESIAN"
   elseif (ans2 == 2) then
      word1 = "ZMATRIX"
   endif
112 format(A8,1X,10A,1X,10A)
   write(11,112) "GEOMETRY", word1,word
    call geom_input
    close(11)
  end subroutine geom

  subroutine geom_input
    implicit none
    character(len=20) :: blah
    integer :: nblk,i
    character(len=2), dimension(:),allocatable :: atm
    real,dimension(:,:),allocatable :: matrix
    
    write(6,*) "File name for the geometry coordinates"
    read(5,*) blah

    open(12,file=trim(adjustl(blah)))
    read(12,*) nblk

    allocate(atm(nblk))
    allocate(matrix(nblk,3))
150 format(A2,4X,F10.7,3X,F10.7,3X,F10.7)
    do i=1,nblk
       read(12,*) atm(i),matrix(i,1),matrix(i,2),matrix(i,3)
       write(11,150) atm(i),matrix(i,1),matrix(i,2),matrix(i,3)
    enddo
    close(12)
  end subroutine geom_input

  subroutine multiplicity
    implicit none
    integer :: mult,ans1=0
    do while (ans1 /= 1 .and. ans1 /= 2)
       write(6,*) "Specify multiplicity? (default: 1 for closed shell, 2 for open shell)"
       write(6,*) "1) Yes"
       write(6,*) "2) No"
       read(5,*) ans1
    enddo
    if (ans1 == 1) then
       write(6,*) "Enter Multiplicity of the system"
       read(5,*) mult
97     format(A12,1X,I1)
       write(11,97) "MULTIPLICITY",mult
    endif
  endsubroutine multiplicity

  subroutine charge
    implicit none
    integer :: ans1=0
    character(len=2) chrg
    do while (ans1 /= 1 .and. ans1 /= 2)
       write(6,*) "Specify Charge? (default is 0)"
       write(6,*) "1) Yes"
       write(6,*) "2) No"
       read(5,*) ans1
    enddo
    if (ans1 == 1) then
       write(6,*) "Input charge of the system (default is 0)"
       read(5,*) chrg
96     format(A6,1X,2A)
       write(11,96) "CHARGE",trim(adjustl(chrg))
    endif
  endsubroutine charge

  subroutine frequency
    implicit none
    integer :: ans1=0,ans2=0
    do while (ans1 /= 1 .and. ans1 /= 2)
       write(6,*) "Frequency calculation?"
       write(6,*) "1) Yes"
       write(6,*) "2) No"
       read(5,*) ans1
    enddo
    if (ans1 == 1) then
       do while (ans2 /= 1 .and. ans2 /= 2)
          write(6,*) "THERMO calculation?"
          write(6,*) "1) Yes"
          write(6,*) "2) No"
          read(5,*) ans2
       enddo
       write(11,'(A9)') "FREQUENCY"
       if (ans2 == 1) then
          call thermo
       endif
    endif
  endsubroutine frequency

  subroutine thermo
    implicit none
    write(11,'(6A)') "THERMO"
  endsubroutine thermo

end module vars

program deminp
  use vars
  implicit none

  call read_input
  call title
  call VXCTYP
  call basis
  call ecps
  call auxis
  call scftyp
  call multiplicity
  call charge
  call shift
  call grid
  call guess
  call diis
  call calc
  call frequency
  call geom

endprogram deminp
