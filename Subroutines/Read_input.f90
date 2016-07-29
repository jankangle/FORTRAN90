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
