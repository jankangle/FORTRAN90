! Title: deMon2k Final Geometry Finder
! Date Created: Aug 2nd,2016
! Date Last Modified: Aug 3rd,2016
! Version: 1.0
!
! Author: Jonathan Kung
! University of Calgary
! Purpose: This program searches the demon2k output file. It will check if the geometry has converged, and if it has, take the coordinates and writes it in an output file readable by molecule visualization tools.


module vars
  implicit none
  character(len=80) :: finp !file name input

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
  
  subroutine wordfinder(i)
    implicit none
    integer :: ierr,j,m
    integer, intent(in) :: i
    character(len=*), parameter :: search_str = "FINAL"
    character(len=1000) :: text
    character (len=10) :: word
    open(11,file=trim(adjustl(finp)))
    m=0
    A:do j=1,i
       read(11,*, iostat=ierr) text
       read(text,*) word
       if (word == search_str) then
          exit A
       endif
    enddo A
  endsubroutine wordfinder

  subroutine mkmatrix
    implicit none
    integer :: i,j,k,atoms
    character(len=10) :: a,b,c,d,e,f,g
    character(len=20) :: efile
    real,dimension(3,3) :: geom
    character,allocatable,dimension(:) :: atm
    write(6,*) "Number of atoms:"
    read(5,*) atoms
    allocate(atm(atoms))
    read(11,*)
    read(11,*)
100 format(A1,3X,F10.7,3X,F10.7,3X,F10.7)
101 format(2X,I3)
    call output(efile)
    open(13,file=trim(adjustl(efile)))
    write(13,101) atoms
    write(13,*)

    do i=1,atoms
       read(11,*) a,b,c,d,e,f,g
       read(b,*) atm(i)
       read(c,*) geom(i,1)
       read(d,*) geom(i,2)
       read(e,*) geom(i,3)
       write(13,100) atm(i),geom(i,:)
    enddo
    
  endsubroutine mkmatrix

  subroutine optchk(i,ok)
    implicit none
    integer :: ierr,j
    integer, intent(out) :: ok
    integer, intent(in) :: i
    character(len=*), parameter :: search_str = "OPTIMIZED"
    character(len=1000) :: text
    open(11,file=trim(adjustl(finp)))

    A:do j=1,i
       read(11,'(A)', iostat=ierr) text
       ok = index(text, search_str)
       if (ok /= 0) then
          write(6,*)
          write(6,*) "*** THE GEOMETRY IS OPTIMIZED ***"
          write(6,*)
          exit A
       endif
    enddo A
    close(11)
  endsubroutine optchk

  subroutine output(efile)
    implicit none
    character(len=20), intent(out) :: efile

    write(6,*) "Input the output file name"
    read(5,*) efile
  endsubroutine output

endmodule vars

program finalgeom
  use vars
  implicit none
  integer :: i,ok

  call read_input
  call filesize(i)
  call optchk(i,ok)

  if (ok == 0) then
     write(6,*) "*** THE GEOMETRY IS NOT OPTIMIZED ***"
     stop
  endif

  call wordfinder(i)
  call mkmatrix

endprogram finalgeom
