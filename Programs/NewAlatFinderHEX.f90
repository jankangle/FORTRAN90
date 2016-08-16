! Title: New Alat Finder for HEX
! Date Created: July 29th,2016
! Date Last Modified: July 29th,2016
! Version: 1.0
!
! Author: Jonathan Kung
! University of Calgary
! Purpose: This program is used to find a new alat for Quantum Espresso calculations.This only works for units cells that have HEX geometry.

 module vars
    implicit none
    character(len=80) :: finp !file name input

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
    subroutine wordfinder(i)
      implicit none
      integer :: ierr,j,m
      integer, intent(in) :: i
      character(len=*), parameter :: search_str = "Begin"
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

    subroutine cellfinder
      implicit none
      integer :: ierr,j,m
      character(len=*), parameter :: search_str = "CELL_PARAM"
      character(len=1000) :: text
      character (len=10) :: word
      A:do j=1,10
         read(11,*, iostat=ierr) text
         read(text,*) word
         if (word == search_str) then
            exit A
         endif
      enddo A
      backspace(11)
    endsubroutine cellfinder

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

    subroutine alat_finder(alat)
      implicit none
      real, intent(out) :: alat
      character(len=15) :: a,b,c
      read(11,*) a,b,c
      c=trim(adjustl(c))
      read(c(1:10),*) alat
      !print*, alat
    endsubroutine alat_finder

    subroutine mkmatrix(matrix)
      implicit none
      integer :: i,j
      real, dimension(3,3), intent(out) :: matrix
      do i=1,3
            read(11,*) matrix(i,1),matrix(i,2),matrix(i,3)
      enddo
    endsubroutine mkmatrix

    subroutine printmaxtrixscreen(array,mrow,mcol)
      implicit none
      real, intent(in) :: array(mrow,mcol)
      integer, intent(in) :: mrow,mcol
      integer :: k
      do k=1,mrow
         write(6,*) array(k,:)
      enddo
    endsubroutine printmaxtrixscreen  

    subroutine themath(alat,matrix)
      real, dimension(3,3), intent(in) :: matrix
      real, dimension(3,3) :: blah
      real, intent(in) :: alat
      real :: b,n
      b = matrix(1,1) * alat
      n = matrix(3,3)
      write(6,*) "The new alat is:", b
      write(6,*) "The new c/a is:", n


    endsubroutine themath
  end module vars

  program main
    use vars
    implicit none
    integer :: i
    real :: alat
    real, dimension(3,3) :: matrix

    call read_input
    call filesize(i)
    call wordfinder(i)
    call cellfinder
    call alat_finder(alat)
    call mkmatrix(matrix)
    call themath(alat,matrix)


    write(6,*) "Done!" 
  endprogram main
