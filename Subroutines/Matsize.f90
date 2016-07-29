  subroutine matsize(size)
    implicit none
    integer, intent(out) :: size
    write(6,*) "Input size of nxn matrix"
    read(5,*) size
  endsubroutine matsize
