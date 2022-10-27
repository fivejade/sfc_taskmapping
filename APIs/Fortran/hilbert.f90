module Hilbert

implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine dec_to_bin(dec, bin, count)
Implicit None
integer*4, intent(inout) :: dec
integer*4, intent(inout) :: bin(100)
integer*4, intent(out) :: count
integer*4 :: i
count=1
do i=1,100
    if (mod(dec,2)==0) then
        bin(i)=0
    else
        bin(i)=1
    end if
    dec=dec/2 !Notice the use of truncated result
    count=count+1
    if (dec==0) then
        exit
    end if
end do
end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine bin_to_dec(bin, dec, count)
Implicit None
integer*4, intent(in) :: count
integer*4, intent(inout) :: bin(count)
integer*4, intent(inout) :: dec
integer*4 :: i,j
j=0
dec=0
do i=count,1,-1
    dec = dec + bin(i)*(2**j)
    j=j+1
end do
end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine binary_repr(points, width, binary_out)
Implicit None
integer*4, intent(inout), dimension(3) :: points
integer*4, intent(in) :: width
integer*4, intent(out) :: binary_out
integer*4 count,i,j,k,dec,bin(width*3),bin_tmp(100)
bin(:)=0
do i=1,3
    !j=(i)*width
    bin_tmp(:)=0
    call dec_to_bin(points(i), bin_tmp, count)
    !write(*,*) "i:",i, "bin_tmp:", bin_tmp(1:count-1)
    k=1
    do j=(3)*(width)-(3-i),1,-3
    !do j=2*(width)+i,1,-width
        bin(j)=bin_tmp(k)
        k=k+1
    enddo
enddo
!write (*,*) "binary repr"
!write (*,*)(bin)
call bin_to_dec(bin, binary_out, width*3)
end subroutine

! The algorithm of xy2d was taken from the folowing research: A.R. Butz: Alternative algorithm for Hilbert’s space filling curve. IEEE Trans. On Computers, 20:424-42, April 1971.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Purpose:
!    xy2d converts a 2D Cartesian coordinate to a 1D Hilbert coordinate.
!  Discussion:
!    It is assumed that a square has been divided into an NxN array of cells,
!    where N is a power of 2.
!    Cell (0,0) is in the lower left corner, and (N-1,N-1) in the upper 
!    right corner.
!  Parameters:
!    Input, int M, the index of the Hilbert curve.
!    The number of cells is N=2^M.
!    0 < M.
!    Input, int X, Y, the Cartesian coordinates of a cell.
!    0 <= X, Y < N.
!    Output, int XY2D, the Hilbert coordinate of the cell.
!    0 <= D < N * N.
subroutine xy2d(m,x,y,d)
Implicit None
integer, intent(in) :: m
integer, intent(in) :: x,y
integer, intent(out) :: d
integer :: n,rx,ry,s
integer :: x2,y2
d = 0
n = 2**m
s = n/2
x2 = x
y2 = y
do
    if (s.le.0) exit
    rx=0
    ry=0
    if (and(x2,s).gt.0) rx=1
    if (and(y2,s).gt.0) ry=1
    d = d+s*s*xor(3*rx,ry)
    call rot(s,x2,y2,rx,ry)
    s = s/2
enddo
end subroutine


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Purpose:
subroutine distance_from_2D_points(points, pp, distance) 
integer*4, intent(inout), dimension(2) :: points
integer*4, intent(in) :: pp
integer*4, intent(out) :: distance

call xy2d(pp, points(1), points(2), distance)

end subroutine

! The distance_from_3D_points was implemented by leveraging the algorithm of the following URL: https://github.com/galtay/hilbertcurve

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Purpose:
subroutine distance_from_3D_points(points, pp, binary_out)
Implicit None
integer*4, intent(inout), dimension(3) :: points
integer*4, intent(in) :: pp
integer*4, intent(out) :: binary_out
integer*4 m,q,i,t,p

!write(*,*) points
m = 2**(pp-1)
! Inverse undo excess work
q = m
do while (q.gt.1)
    p = q-1
    do i=1,3 ! for 3 dimensonal space
        if (iand(points(i), q).ne.0) then
            points(1) = ieor(points(1), p)
        else
            t = iand(ieor(points(1), points(i)), p)
            points(1) = ieor(points(1), t)
            points(i) = ieor(points(i), t)
        endif
    enddo
    q=ishft(q,-1)
enddo
! Gray encode
do i=2,3
    points(i) = ieor(points(i), points(i-1))
enddo
t=0
q=m
do while (q.gt.1)
    if (iand(points(3), q).ne.0) t = ieor(t, q-1)
    q=ishft(q,-1)
enddo
do i=1,3
    points(i) = ieor(points(i), t)
enddo
!write(*,*) 'out'
!write(*,*) points
call binary_repr(points, pp, binary_out)
!write(*,*) 'distance: ', binary_out
end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Purpose:
!    rot rotates and flips a quadrant appropriately.
!  Parameters:
!    Input, int N, the length of a side of the square.  N must be a power of 2.
!    Input/output, int *X, *Y, the old and the new coordinates.
!    Input, int RX, RY
subroutine rot(n,x,y,rx,ry)
Implicit None
! Input, N, the length of a side of the square. N must be a power of 2.
integer, intent(in) :: n,rx,ry
! Input/output, X, Y, the old and the new coordinates.
integer, intent(inout) :: x,y
integer t
if (ry.eq.0) then
   if (rx.eq.1) then
      x = n-1-x
      y = n-1-y
   endif
   t = x
   x = y
   y = t
endif
end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Purpose:
!    remap_tasks_Hilbert_2D does task-remapping using Hilbert-curve.
!  Discussion:
!    It is assumed that a square has been divided into an Nx x Ny array of cells,
!    where Nx and Ny are a power of 2.
!  Parameters:
!    cur_comm = current communicator (input)
!    new_comm = task-remapped new communicator (return)
subroutine remap_tasks_Hilbert_2D(Nx, Ny, cur_comm, new_comm) 
use mpi
Implicit None
INTEGER, intent(in) :: Nx,Ny
INTEGER*4, intent(in) :: cur_comm
INTEGER*4, intent(out) :: new_comm
INTEGER, dimension(:), allocatable :: NEW_RANKS

INTEGER nprocs, myrank, IERR, nodeID, NEW_GROUP, MPI_WORLD_GROUP
INTEGER i,j,k,log2_Nx,new_nodeID
INTEGER p ! iterations
INTEGER, dimension(2) :: points
!!! Assert that Nx and Ny should be same and a power of 2.
if (Nx/=Ny) then
	write(*,*) "Wrong dimensions: Nx(", Nx, ") and Ny(", Ny, ") should be same."
	new_comm = cur_comm
	return
endif
log2_Nx = INT(LOG(REAL(Nx)) / LOG(2.))
if (Nx/=2**log2_Nx) then
	write(*,*) "Wrong dimensions: Nx(", Nx, ") should be a power of 2."
	new_comm = cur_comm
	return
endif 

call MPI_Comm_size( MPI_COMM_WORLD, nprocs, IERR) 
call MPI_Comm_rank( MPI_COMM_WORLD, myrank, IERR)
! calcaute the number of iterations, p
p = INT(LOG(REAL(Nx*Ny)) / LOG(2.) / 2)
ALLOCATE(NEW_RANKS(0:nprocs-1))
do i=0,Nx-1
do j=0,Ny-1
	nodeID = j+(i)*Ny
	points = (/ i, j/)
	call distance_from_2D_points(points, p, new_nodeID)
	NEW_RANKS(new_nodeID)=nodeID
enddo
enddo
CALL MPI_COMM_GROUP(cur_comm, MPI_WORLD_GROUP, IERR)
CALL MPI_GROUP_INCL(MPI_WORLD_GROUP, nprocs, NEW_RANKS(0:nprocs-1), NEW_GROUP, IERR)
CALL MPI_Comm_create_group(cur_comm, NEW_GROUP, 0, new_comm, IERR)
DEALLOCATE(NEW_RANKS)
end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Purpose:
!    remap_tasks_Hilbert_3D does task-remapping using Hilbert-curve.
!  Discussion:
!    It is assumed that Nx, Ny, and Nz are a power of 2.
!  Parameters:
!    cur_comm = current communicator (input)
!    new_comm = task-remapped new communicator (return)
subroutine remap_tasks_Hilbert_3D(Nx, Ny, Nz, cur_comm, new_comm) 
use mpi
Implicit None
INTEGER, intent(in) :: Nx,Ny,Nz
INTEGER*4, intent(in) :: cur_comm
INTEGER*4, intent(out) :: new_comm
INTEGER, dimension(:), allocatable :: NEW_RANKS

INTEGER nprocs, myrank, IERR, nodeID, NEW_GROUP, MPI_WORLD_GROUP
INTEGER i,j,k,log2_Nx,new_nodeID
INTEGER p ! iterations
INTEGER, dimension(3) :: points

!!! Assert that Nx, Ny and Nz should be same and a power of 2.
if (Nx/=Ny.and.Ny/=Nz) then
	write(*,*) "Wrong dimensions: Nx(", Nx, ") , Ny(", Ny, "), and Nz(", Nz, ") should be same."
	new_comm = cur_comm
	return
endif
log2_Nx = INT(LOG(REAL(Nx)) / LOG(2.))
if (Nx/=2**log2_Nx) then
	write(*,*) "Wrong dimensions: Nx(", Nx, ") should be a power of 2."
	new_comm = cur_comm
	return
endif 

call MPI_Comm_size( MPI_COMM_WORLD, nprocs, IERR) 
call MPI_Comm_rank( MPI_COMM_WORLD, myrank, IERR)
! calcaute the number of iterations, p
p = INT(LOG(REAL(Nx*Ny*Nz)) / LOG(2.) / 3)
ALLOCATE(NEW_RANKS(0:nprocs-1))
do i=0,Nx-1
do j=0,Ny-1
do k=0,Nz-1
	nodeID = k+(j)*nz+(i)*nz*ny
	points = (/ i, j, k/)
	call distance_from_3D_points(points, p, new_nodeID)
    !if (myrank.eq.0) then
    ! write(*,*) i, j ,k, "old:", nodeID , "new:", new_nodeID
    !endif
	NEW_RANKS(new_nodeID)=nodeID
enddo
enddo
enddo
CALL MPI_COMM_GROUP(cur_comm, MPI_WORLD_GROUP, IERR)
CALL MPI_GROUP_INCL(MPI_WORLD_GROUP, nprocs, NEW_RANKS(0:nprocs-1), NEW_GROUP, IERR)
CALL MPI_Comm_create_group(cur_comm, NEW_GROUP, 0, new_comm, IERR)
DEALLOCATE(NEW_RANKS)
end subroutine

end module Hilbert
