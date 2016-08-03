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


  subroutine title
    implicit none
    character(len=100) :: ti
    write(6,*) "Input Title:"
    read(5,*) ti
    ti = trim(adjustl(ti))
    open(11,file=trim(adjustl(finp)))
    write(11,102) "TITLE",ti
102 format(A5,1X,A100)
  endsubroutine title

  subroutine VXCTYP
    implicit none
    
    character(len=5) :: vxc,func
    do while (vxc /= "AUXIS" .and. vxc /= "BASIS")
       write(6,*) "AUXIS OR BASIS?"
       read(5,*) vxc
    enddo
   
    call functional(func)
    write(11,103)"VXCTYP",trim(adjustl(vxc)),trim(adjustl(func))
103 format(A6,1X,A5,1X,5A)
  endsubroutine VXCTYP

  subroutine functional(func)
    implicit none
    character(len=5), intent(out) :: func
    write(6,*) "Input functional"
    read(5,*) func
  endsubroutine functional
  
  subroutine basis
    implicit none
    character(len=100) :: bas
    write(6,*) "Input Basis set name"
    read(5,*) bas
    write(11,104) "BASIS (",trim(adjustl(bas)),")"
104 format(A7,30A,A1)
  endsubroutine basis

  subroutine ecps
    implicit none
    character(len=100) :: ecp
    write(6,*) "Input ECPS name"
    read(5,*) ecp
    write(11,105) "ECPS (",trim(adjustl(ecp)),")"
105 format(A6,30A,A1)
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
    integer :: maxiter
    !real :: tol
    
    do while (scf /= "RKS" .and. scf /= "UKS" .and. scf /= "ROKS")
       write(6,*) "RKS,UKS or ROKS?"
       read(5,*) scf
    enddo
    
    write(6,*) "Input Max interations for SCF"
    read(5,*) maxiter

    do while (tight /= "TIGHTEN" .and. tight /= "NOTIGHTEN")
       write(6,*) "TIGHTEN or NOTIGHTEN"
       read(5,*) tight
    enddo
    
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
    character(len=1) :: ans
    real :: amnt
    do while (ans /= "Y" .and. ans /= "N")
       write(6,*) "Input Shift? (Y/N)"
       read(5,*) ans
    enddo
    if (ans == "Y") then
       write(6,*) "Input Shift amount"
       read(5,*) amnt
       write(11,101) "SHIFT",amnt
    endif
101 format(A5,1X,F4.1)
  end subroutine shift

  subroutine grid
    implicit none
    character(len=1) :: ans
    character(len=6) :: fix
    do while (ans /= "Y" .and. ans /= "N")
       write(6,*) "Fix grid? (Y/N)"
       read(5,*) ans
    enddo

120 format(A10,1X,A6)
    if (ans == "Y") then
       do while (fix /= "MEDIUM" .and. fix /= "COARSE" .and. fix /= "FINE")
          write(6,*) "MEDIUM, COARSE, or FINE?"
          read(5,*) fix
          fix=trim(adjustl(fix))
       enddo
       write(11,120) "GRID FIXED",fix
    endif
  end subroutine grid

  subroutine guess
    implicit none
    character(len=4) :: gue
    
    do while (gue /= "TB" .and. gue /= "CORE")
       write(6,*) "guess TB or CORE"
       read(5,*) gue
       gue = trim(adjustl(gue))
    enddo
    if (gue == "CORE") then
       write(11,108) "GUESS", gue
    endif
108 format(A5,1X,A4)
  end subroutine guess

  subroutine diis
    implicit none
    character(len=3) :: ans
    
    do while(ans /= "ON" .and. ans /= "OFF")
       write(6,*) "DIIS ON or OFF?"
       read(5,*) ans
       ans=trim(adjustl(ans))
    enddo
    if (ans == "OFF") then
       write(11,109) "DIIS",ans
    endif
109 format(A4,1X,A3)
  endsubroutine diis

  subroutine calc(xyz)
    implicit none
    character(len=3) :: ans
    character(len=10), intent(out) :: xyz
    integer :: maxiter
130 format(A12,1X,A9,1X,A4,I3)
    do while (ans /= "OPT" .and. ans /= "SIN")
       write(6,*) "(OPT)IMIZATION or (SIN)GLE POINT ENERGY?"
       read(5,*) ans
       ans=trim(adjustl(ans))
    enddo
    if (ans == "OPT") then
       do while(xyz /= "CARTESIAN" .and. xyz /= "INTERNAL")
          write(6,*) "CARTESIAN or INTERNAL"
          read(5,*) xyz
          xyz=trim(adjustl(xyz))
       enddo
       write(6,*) "MAX ITERATIONS?"
       read(5,*) maxiter
       write(11,130) "OPTIMIZATION",xyz,"MAX=",maxiter
    endif
  endsubroutine calc

  subroutine geom(xyz)
    implicit none
    character(len=10),intent(in) :: xyz
    character(len=9) :: ang,temp,dumx

140 format(A8,1X,A9,1X,A8)
    temp=xyz
    dumx=xyz
    if (temp == "INTERNAL") then
       temp = "ZMATRIX"
    endif
    do while (ang /= "ANGSTROM" .and. ang /= "BOHR")
       write(6,*) "ANGSTROM or BOHR"
       read(5,*) ang
       ang=trim(adjustl(ang))
    enddo

    if (dumx == "CARTESIAN" .or. dumx == "INTERNAL") then
       write(11,140) "GEOMETRY",temp,ang
    else
       do while(dumx /= "CARTESIAN" .and. dumx /= "INTERNAL" .and. dumx /= "ZMATRIX")
          if(temp /= "CARTESIAN" .and. temp /= "INTERNAL") then
             write(6,*) "CARTESIAN or ZMATRIX"
             read(5,*) dumx
          endif
       enddo
       write(11,140) "GEOMETRY",dumx,ang
    endif
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

end module vars


program deminp
  use vars
  implicit none
  character(len=10) :: xyz

  call read_input
  call title
  call VXCTYP
  call basis
  call ecps
  call auxis
  call scftyp
  call shift
  call grid
  call guess
  call diis
  call calc(xyz)
  call geom(xyz)

endprogram deminp
