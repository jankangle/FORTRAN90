! Title: NWChem Input Generator
! Date Created: Aug 3,2016
! Date Last Modified: Aug 5,2016
! Version: 1.0
!
! Author: Jonathan Kung
! University of Calgary
! Purpose: Easily create an NWChem input

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
    open(11,file=trim(adjustl(finp)))
    write(11,*)
99  format(30A)
    write(11,99) "echo"
    write(11,*)
    write(6,*) "Input title name:"
    read(5,*) ti
    write(11,100) "start",ti
100 format(A5,1X,A100)
    write(11,*)
  endsubroutine title

  subroutine memory
    implicit none
    integer :: mem
    write(6,*) "Input total memory in MB"
    read(5,*) mem
    write(11,101) "memory total",mem, "MB"
101 format(A12,1X,I5,1X,A2)
    write(11,*)
  endsubroutine memory

  subroutine geom
    implicit none
99  format(30A)
    write(11,99) "geometry noautoz noautosym"
    write(11,99) "symmetry group c1"
    call geom_input
    write(11,99) "end"
    write(11,*)
  endsubroutine geom

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

  subroutine basis
    implicit none
    
    character(len=1) :: ans1,ans2
    character(len=20) :: basisname
    character(len=2) :: atomnm
    write(11,99) "basis print"

    do while (ans1 /= "Y" .and. ans1 /= "N")
       write(6,*) "Specify each atom's basis set? (Y/N)"
       read(5,*) ans1
    enddo
102 format(2X,A2,1X,A9,20A,1A)
103 format(2X,A11,20A,1A)
    if (ans1 == "Y") then
          write(6,*) "Insert atom name"
          read(5,*) atomnm
          write(6,*) "Input basis set name:"
          read(5,*) basisname
          write(11,102) atomnm,'library "',trim(adjustl(basisname)),'"'
       do while (ans2 /= "Y" .and. ans2 /= "N")
          write(6,*) "Add another atom? (Y/N)"
          read(5,*) ans2
          do while (ans2 == "Y")
             write(6,*) "Insert atom name"
             read(5,*) atomnm
             write(6,*) "Input basis set name:"
             read(5,*) basisname
             write(11,102) atomnm,'library "',trim(adjustl(basisname)),'"'
             write(6,*) "Add another atom? (Y/N)"
             read(5,*) ans2
          enddo
       enddo
    else
       write(6,*) "Input basis set name:"
       read(5,*) basisname
       write(11,103) '* library "',trim(adjustl(basisname)),'"'
    endif
99  format(30A)
    write(11,99) "end"
    write(11,*)
  endsubroutine basis

  subroutine ecp
    implicit none
    
    character(len=1) :: ans1,ans2,ans3
    character(len=20) :: ecpname
    character(len=2) :: atomnm
    ans1="k"
    ans2="k"
    ans3="k"
    do while (ans3 /= "Y" .and. ans3 /= "N")
       write(6,*) "Input ECP? (Y/N)"
       read(5,*) ans3
    enddo

    if (ans3 == "Y") then
       write(11,99) "ecp"
       do while (ans1 /= "Y" .and. ans1 /= "N")
          write(6,*) "Specify each atom's ECP? (Y/N)"
          read(5,*) ans1
       enddo
103    format(2X,A11,20A,1A)
102    format(2X,A2,1X,A9,20A,1A)

       if (ans1 == "Y") then
          write(6,*) "Insert atom name"
          read(5,*) atomnm
          write(6,*) "Input ECP name:"
          read(5,*) ecpname
          write(11,102) atomnm,'library "',trim(adjustl(ecpname)),'"'
          do while (ans2 /= "Y" .and. ans2 /= "N")
             write(6,*) "Add another atom?"
             read(5,*) ans2
             do while (ans2 == "Y")
                write(6,*) "Insert atom name"
                read(5,*) atomnm
                write(6,*) "Input ECP name:"
                read(5,*) ecpname
                write(11,102) atomnm,'library "',trim(adjustl(ecpname)),'"'
                write(6,*) "Add another atom?"
                read(5,*) ans2
             enddo
          enddo
       else
          write(6,*) "Input ECP name:"
          read(5,*) ecpname
          write(11,103) '* library "',trim(adjustl(ecpname)),'"'
       endif
       write(11,99) "end"
99     format(30A)
       write(11,*)
    endif
  endsubroutine ecp

  subroutine driver
    implicit none
    
    character(len=1) :: ans1
    character(len=7) :: ans2
    integer :: maxiter

    ans1="k"
    do while (ans1 /= "Y" .and. ans1 /= "N")
       write(6,*) "Input Driver settings? (Y/N)"
       read(5,*) ans1
    enddo
    if (ans1 == "Y") then
       write(11,99) "driver"
       do while (ans2 /= "default" .and. ans2 /= "tight")
          write(6,*) "default or tight?"
          read(5,*) ans2
       enddo
104    format(2X,7A)
       if (ans2 == "tight") then
          write(11,104) "tight"
       endif

       write(6,*) "Input max iterations (default is 20)"
       read(5,*) maxiter
105    format(2X,A7,1X,3I1)
       write(11,105) "maxiter", maxiter
       write(11,99) "end"
99     format(30A)

       write(11,*)
    endif
  endsubroutine driver

  subroutine hessian
    implicit none
    
    character(len=1) :: ans1
    character(len=10) :: tol
    ans1="k"
    do while (ans1 /= "Y" .and. ans1 /= "N")
       write(6,*) "Input Hessian settings? (Y/N)"
       read(5,*) ans1
    enddo
    if (ans1 == "Y") then

       write(11,99) "hessian"
       
       write(6,*) "Input thresh tolerance (default is 1.0e-6)"
       read(5,*) tol
106    format(2X,A6,1X,A10)
       write(11,106) "thresh",tol
       write(11,99) "end"
99     format(30A)
       write(11,*)
    endif
  endsubroutine hessian

  subroutine calc
    implicit none
    
    character(len=7) :: calctyp

    do while (calctyp /= "SCF" .and. calctyp /= "DFT" .and. calctyp /= "MP2" &
         .and. calctyp /= "CISD" .and. calctyp /= "CCSD" .and. &
         calctyp /= "CCSD(T)" .and. calctyp /= "MCSCF")
       write(6,*) "Enter calculation type:"
       write(6,*) "SCF,DFT,MP2,CISD,CCSD,CCSD(T),MCSCF"
       read(5,*) calctyp
    enddo

    if (calctyp == "SCF") then
       call SCF
       call task(calctyp)
    elseif (calctyp == "DFT") then
       call SCF
       call DFT
       calctyp="scf"
       call task(calctyp)
       calctyp="dft"
       call task(calctyp)
    elseif (calctyp == "MP2") then
       call SCF
       call MP2
       calctyp="scf"
       call task(calctyp)
       calctyp="mp2"
       call task(calctyp)
    elseif (calctyp == "CISD") then
       call SCF
       call TCE_(calctyp)
       calctyp="TCE"
       call task(calctyp)
    elseif (calctyp == "CCSD") then
       call SCF
       call TCE_(calctyp)
       calctyp="TCE"
       call task(calctyp)
    elseif (calctyp == "CCSD(T)") then
       call SCF
       call TCE_(calctyp)
       call task(calctyp)
    elseif (calctyp == "MCSCF") then
       call SCF
       call MCSCF
       calctyp="scf"
       call task(calctyp)
       calctyp="mcscf"
       call task(calctyp)
    endif

  endsubroutine calc


  subroutine SCF
    implicit none
    character(len=1) :: ans1
    character(len=10) :: mult,maxiter
    write(6,*) "*** ENTERING SCF BLOCK ***"
    write(6,*) "input the multiplicity in words"
    write(6,*) "ie singlet,doublet,triplet,etc"
    read(5,*) mult
    mult=trim(adjustl(mult))
    if (mult /= "singlet") then
       do while (ans1 /= "Y" .and. ans1 /= "N")
          write(6,*) "UHF setting? (Y/N)"
          read(5,*) ans1
       enddo
    endif
    write(6,*) "Input max iterations"
    read(5,*) maxiter

111 format(2X,A3)
109 format(2X,A6)
99  format(30A)
105 format(2X,A7,1X,3A)
108 format(2X,10A)
107 format(2X,40A)
    write(11,99) "scf"
    write(11,107) "vectors input atomic output scf.mo"
    write(11,108) mult
    if (ans1 == "Y") then
       write(11,111) "uhf"
    endif
    write(11,109) "direct"
    write(11,105) "maxiter", maxiter
    write(11,99) "end"
    write(11,*)
    write(6,*) "*** EXITING SCF BLOCK ***"

  endsubroutine SCF

  subroutine DFT
    implicit none

    character(len=1) :: ans1,mult
    character(len=7) :: ans2,func,maxiter
    write(6,*) "*** ENTERING DFT BLOCK ***"
    write(6,*) "Input Multiplicity (integer value)"
    read(5,*) mult
    write(6,*) "Input max iterations (default is 30)"
    read(5,*) maxiter
    do while (ans1 /= "Y" .and. ans1 /= "N")
       write(6,*) "Input Grid settings? (Y/N)"
       read(5,*) ans1
    enddo
    if (ans1 == "Y") then
       do while (ans2 /= "medium" .and. ans2 /= "fine" .and. ans2 /= "xfine")
          write(6,*) "Input Grid Setting:"
          write(6,*) "medium (default), fine, xfine"
          read(5,*) ans2
       enddo
    endif
    write(6,*) "Input XC functional"
    read(5,*) func
117 format(2X,4A,7A)
118 format(2X,2A,7A)
99  format(30A)
105 format(2X,A7,1X,3A)
115 format(2X,A4,1X,A1)
116 format(2X,5A)
114 format(2X,30A)
    write(11,99) "dft"
    write(11,114) "vectors input scf.mo"
    write(11,115) "mult",mult
    if(mult /= "1") then
       write(11,116) "rodft"
    endif
    write(11,105) "maxiter",maxiter
    if (ans1 == "Y") then
       write(11,117) "grid ", ans2
    endif
    write(11,118) "xc ", func
    write(11,99) "end"
    write(11,*)
    write(6,*) "*** EXITING DFT BLOCK ***"
  endsubroutine DFT

  subroutine MP2
    implicit none
    character(len=1) :: ans1
    write(6,*) "*** ENTERING MP2 BLOCK ***"
    do while (ans1 /= "Y" .and. ans1 /= "N")
       write(6,*) "Tight setting? (Y/N)"
       read(5,*) ans1
    enddo

120 format(2X,5A)
99  format(30A)
119 format(2X,30A)
    write(11,99) "mp2"
    write(11,119) "vectors input scf.mo"
    if (ans1 == "Y") then
       write(11,120) "tight"
    endif
    write(11,99) "end"
    write(11,*)
    write(6,*) "*** EXITING MP2 BLOCK ***"
  endsubroutine MP2

  subroutine TCE_(calctyp)
    implicit none
    character(len=7), intent(in) :: calctyp
    character(len=10) :: thresh, maxiter,diis
    write(6,*) "*** ENTERING TCE BLOCK ***"
    write(6,*) "Input max iterations"
    read(5,*) maxiter
    write(6,*) "Insert thresh setting (1.0e-6 is default)"
    read(5,*) thresh
    write(6,*) "Insert DIIS setting (Integer, 5 is default)"
    write(6,*) "0 diis will turn it off"
    read(5,*) diis

110 format(2X,7A,1X,3A)
124 format(2X,A6,1X,A10)
122 format(2X,A4,1X,2A)
99  format(30A)
121 format(2X,A7)
123 format(2X,A3)
    write(11,99) "TCE"
    write(11,123) "scf"
    write(11,121) calctyp
    write(11,110) "maxiter ", trim(adjustl(maxiter))
    write(11,124) "thresh", thresh
    write(11,122) "diis",trim(adjustl(diis))
    write(11,99) "end"
    write(11,*)
    write(6,*) "*** EXITING TCE BLOCK ***"
  endsubroutine TCE_
  
  subroutine MCSCF
    implicit none
    character(len=2) :: actspc,actelec,mult
    character(len=10) :: thresh,maxiter
    write(6,*) "*** ENTERING MCSCF BLOCK ***"
    write(11,99) "mcscf"
    write(6,*) "Number of orbitals in the active space?"
    read(5,*) actspc
    write(6,*) "Number of electrons in the active space?"
    read(5,*) actelec
    write(6,*) "Input multiplicity (integer value)"
    read(5,*) mult
    write(6,*) "Input max iterations"
    read(5,*) maxiter
    write(6,*) "Insert thresh setting (1.0e-6 is default)"
    read(5,*) thresh
99  format(30A) 
125 format(2X,12A,1X,A3)
126 format(2X,30A)
127 format(2X,6A,1X,A10)
    write(11,125) "active ",trim(adjustl(actspc))
    write(11,125) "actelec ",trim(adjustl(actelec))
    write(11,125) "multiplicity ",trim(adjustl(mult))
    write(11,126) "vectors input scf.mo"
    write(11,125) "maxiter ",trim(adjustl(maxiter))
    write(11,127) "thresh ",trim(adjustl(thresh))
    write(11,99) "end"
    write(11,*)
    write(6,*) "*** EXITING MCSCF BLOCK ***"
  endsubroutine MCSCF

  subroutine task(calctyp)
    implicit none
    
    character(len=7), intent(in) :: calctyp
    character(len=1) :: ans1,ans2,ans3
    character(len=100) :: title
    write(6,*) "Insert title name for:",calctyp
    read(5,*) title
113 format(A7,100A,A1)
    write(11,113) 'title "',trim(adjustl(title)),'"'
    ans1="k"
    ans2="k"
    ans3="k"
    do while (ans1 /= "Y" .and. ans1 /= "N")
       write(6,*) "Optimization? (Y/N)"
       read(5,*) ans1
    enddo
    do while (ans2 /= "Y" .and. ans2 /= "N")
        write(6,*) "Energy? (Y/N)"
        read(5,*) ans2
    enddo

    do while (ans3 /= "Y" .and. ans3 /= "N")
        write(6,*) "Frequency? (Y/N)"
        read(5,*) ans3
    enddo
112 format(A4,1X,5A,1X,10A)


    if (ans1 == "Y") then
       write(11,112) "task",trim(adjustl(calctyp))," optimize"
    endif
    if (ans2 == "Y") then
       write(11,112) "task",trim(adjustl(calctyp))," energy"
    endif
    if (ans3 == "Y") then
       write(11,112) "task",trim(adjustl(calctyp))," freq"
    endif
    write(11,*)
  endsubroutine task

endmodule vars



program main
  use vars
  implicit none
  
  call read_input
  call title
  call memory
  call geom
  call basis
  call ecp
  call driver
  call hessian
  call calc

endprogram main
