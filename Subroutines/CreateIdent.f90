  subroutine createident(m,ident)
    implicit none
    integer, intent(in) :: m
    integer, allocatable, dimension(:,:), intent(out) :: ident
    integer :: i,j

    allocate(ident(m,m))
    do i=1,m
       do j=1,m
          if (i==j) then
             ident(i,j) = 1
          else
             ident(i,j) = 0
          endif
       enddo
    enddo
  endsubroutine createident
