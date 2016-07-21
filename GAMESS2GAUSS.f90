    module vars
    implicit none
    !
    type gbs
      character(len=2) :: lmax
      integer :: ngbs
      real,dimension(:,:),allocatable :: fgbs 
    end type
    integer :: nblk,size
    type(gbs),dimension(:),allocatable :: g
    character(len=80) :: finp !file name input
    character(len=15) :: atom !atom name
      !
      contains
      !
      subroutine read_input
      implicit none 
      integer :: i,ndim
      integer :: iargc 
      character(len=80),dimension(:),allocatable :: argv
        ndim=iargc()
        allocate(argv(ndim))
        do i=1,ndim
          call getarg(i,argv(i))
        end do 
        finp=trim(adjustl(argv(1))) !trim the file name
        deallocate(argv)
      end subroutine read_input
      !
      subroutine atmnmtrunc(name)
        character(len=15), intent(inout) :: name
        if (name == "HYDROGEN") then
           name = "H"
        elseif (name == "CARBON") then
           name = "C"
        elseif (atom == "OXYGEN") then
           name = "O"
        elseif (atom == "CERIUM") then
           name = "Ce"
        endif
      endsubroutine atmnmtrunc
      subroutine read_file
      implicit none 
      integer :: i,j,ndim,k
        write(6,*) "Indicate Basis size:"
        read(5,*) size !Indicate the number of atoms
        open(11,file="OUTPUT") !output file of new basis set
        write(11,*) "BASIS FILE:",trim(adjustl(finp)) !Indicates original basis set inputted
        open(22,file=trim(adjustl(finp))) !open original basis set
        !allocate(g(nblk))
        do k=1,size !loops over size of basis
           read(22,*) atom, nblk  !read atom name and number of blocks
           call atmnmtrunc(atom)
           write(11,*) atom !write atom name
           allocate(g(nblk))
           do i=1,nblk 
              read(22,*) g(i)%lmax,ndim !read the letter and the integer 
              if (g(i)%lmax == "L") then 
                 g(i)%lmax = "SP" !converts L to SP
                 write(11,*) g(i)%lmax,ndim,"1.00" !writes the letter and integer
                 g(i)%ngbs=ndim
                 allocate(g(i)%fgbs(ndim,4))
                 do j=1,ndim
                    read(22,*) g(i)%fgbs(j,:) !read the row of the block
                    write(11,*) g(i)%fgbs(j,2),g(i)%fgbs(j,3),g(i)%fgbs(j,4) !print 3 cols for "L"
                 enddo
              else !If its not SP, everything else has 2 cols
                 write(11,*) g(i)%lmax,ndim, "1.00" !writes the letter and integer
                 g(i)%ngbs=ndim
                 allocate(g(i)%fgbs(ndim,3))
                 do j=1,ndim !# of rows in block
                    read(22,*) g(i)%fgbs(j,:) !read the row of the block
                    write(11,*) g(i)%fgbs(j,2),g(i)%fgbs(j,3) !print 2 cols for everything else
                 enddo
              endif
           end do
        deallocate(g)
        write(11,*) !space between atom basis sets
        enddo
        close(22)
      end subroutine read_file

    end module vars

    program main 
    use vars
    implicit none
      call read_input
      call read_file
    end program main 
