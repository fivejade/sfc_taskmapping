module mpi_topology

    implicit none

    private

    integer(kind=4), public :: nprocs, myrank
    integer(kind=4), public :: mpi_world_cart
    integer(kind=4), public :: np_dim(0:2)  !!!!x1:0, x2:1, x3:2
    logical, public         :: period(0:2) !!!!x1:0, x2:1, x3:2

    type, public :: cart_comm_1d
        integer(kind=4) :: myrank, nprocs
        integer(kind=4) :: west_rank, east_rank
        integer(kind=4) :: mpi_comm
    end type cart_comm_1d

    type(cart_comm_1d), public, target      :: comm_1d_x, comm_1d_y, comm_1d_z

    type, public :: boundary_comm
        integer(kind=4) :: myrank, nprocs
        integer(kind=4) :: mpi_comm
    end type boundary_comm

    type(boundary_comm), public     :: comm_boundary

    public  :: mpi_topology_create
    public  :: mpi_topology_destroy
    public  :: mpi_boundary_create

    contains

    subroutine mpi_topology_create()
        
        use mpi
		use hilbert

        implicit none

        logical         :: remain(0:2)
        integer(kind=4) :: ierr

        INTEGER*4 :: MPI_WORLD_GROUP, NEW_GROUP, NEW_COMM
        INTEGER :: new_rank, nprocs, myrank
        integer d,m,n,x,y
        integer nx, ny, nz
        integer i, j, k, l, len
        integer nodeID
!        integer new_rankID1, new_rankID2
!        integer, dimension(3) :: points
!        INTEGER, dimension(:), allocatable :: NEW_RANKS

        integer*4 hostid, cnt
!        integer*4, dimension(:), allocatable :: host_array
!        real*4, dimension(:), allocatable :: frags
!        real*4 fragment
!        character(len=8) hostname

        call MPI_Comm_size( MPI_COMM_WORLD, nprocs, ierr)
        call MPI_Comm_rank( MPI_COMM_WORLD, myrank, ierr)

        if (nprocs.eq.8) then
            nx = 2
            ny = 2
            nz = 2
        elseif (nprocs.eq.64) then
            nx = 4
            ny = 4
            nz = 4
        elseif (nprocs.eq.512) then
            nx = 8
            ny = 8
            nz = 8
        elseif (nprocs.eq.4096) then
            nx = 16
            ny = 16
            nz = 16
        elseif (nprocs.eq.32768) then
            nx = 32
            ny = 32
            nz = 32
        else
            write(*,*) "wrong nprocs!", nprocs
            call MPI_Finalize(ierr)
            stop
        endif
        if (myrank.eq.0) then
            write(*,*) "# of ranks: ", nprocs, '=', nx, '*', ny, '*', nz
        endif
#ifdef HILBERT
        if (myrank.eq.0) then
            write(*,*) "Hilbert mode"
        endif
		call remap_tasks_Hilbert_3D(nx, ny, nz, MPI_COMM_WORLD, NEW_COMM)
#else
        if (myrank.eq.0) then
            write(*,*) "Normal mode"
        endif
        NEW_COMM = MPI_COMM_WORLD
#endif
        call MPI_Cart_create( NEW_COMM    &!  input  | integer(kind=4)      | Input communicator (handle).
                            , 3                 &!  input  | integer(kind=4)      | Number of dimensions of Cartesian grid (integer(kind=4)).
                            , np_dim            &!  input  | integer(kind=4)(1:3) | integer(kind=4) array of size ndims specifying the number of processes in each dimension.
                            , period            &!  input  | logical(1:3) | Logical array of size ndims specifying whether the grid is periodic (true=1) or not (false=0) in each dimension.
                            , .false.           &!  input  | logical      | Ranking may be reordered (true=1) or not (false=0) (logical).
                            , mpi_world_cart    &! *output | integer(kind=4)      | Communicator with new Cartesian topology (handle).
                            , ierr              &!  output | integer(kind=4)      | Fortran only: Error status
                            )

        remain(0) = .true.
        remain(1) = .false.
        remain(2) = .false.
        call MPI_Cart_sub( mpi_world_cart, remain, comm_1d_x%mpi_comm, ierr)
        call MPI_Comm_rank(comm_1d_x%mpi_comm, comm_1d_x%myrank, ierr)
        call MPI_Comm_size(comm_1d_x%mpi_comm, comm_1d_x%nprocs, ierr)
        call MPI_Cart_shift(comm_1d_x%mpi_comm, 0, 1, comm_1d_x%west_rank, comm_1d_x%east_rank, ierr)
    
        remain(0) = .false.
        remain(1) = .true.
        remain(2) = .false.
        call MPI_Cart_sub( mpi_world_cart, remain, comm_1d_y%mpi_comm, ierr)
        call MPI_Comm_rank(comm_1d_y%mpi_comm, comm_1d_y%myrank, ierr)
        call MPI_Comm_size(comm_1d_y%mpi_comm, comm_1d_y%nprocs, ierr)
        call MPI_Cart_shift(comm_1d_y%mpi_comm, 0, 1, comm_1d_y%west_rank, comm_1d_y%east_rank, ierr)

        remain(0) = .false.
        remain(1) = .false.
        remain(2) = .true.
        call MPI_Cart_sub( mpi_world_cart, remain, comm_1d_z%mpi_comm, ierr)
        call MPI_Comm_rank(comm_1d_z%mpi_comm, comm_1d_z%myrank, ierr)
        call MPI_Comm_size(comm_1d_z%mpi_comm, comm_1d_z%nprocs, ierr)
        call MPI_Cart_shift(comm_1d_z%mpi_comm, 0, 1, comm_1d_z%west_rank, comm_1d_z%east_rank, ierr)

        !call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        !if (myrank.eq.0) then
        !   write(*,*) "myrank: ", myrank, " x: ", comm_1d_x%myrank, " y: ", comm_1d_y%myrank, " z: ", comm_1d_z%myrank
        !endif

    end subroutine mpi_topology_create

    subroutine mpi_topology_destroy()

        implicit none
        integer(kind=4) :: ierr

        call MPI_Comm_free(mpi_world_cart, ierr)

    end subroutine mpi_topology_destroy

    subroutine mpi_boundary_create()

        use mpi

        implicit none

        integer(kind=4) :: i, j, k
        integer(kind=4) :: ierr
        integer(kind=4) :: coords(0:2)
        integer(kind=4) :: group_boundary, group_cart
        integer(kind=4), allocatable    :: rank(:)
        integer(kind=4) :: curr_rank, n_rank

        call MPI_Comm_group(mpi_world_cart, group_cart, ierr)

        allocate(rank(product(np_dim)))

        rank = -1
        n_rank = 0
        do i = 0, np_dim(0)-1
            do j = 0, np_dim(1)-1
                do k = 0, np_dim(2)-1
                    coords(0) = i
                    coords(1) = j
                    coords(2) = k

                    if((i.eq.0 .or. i.eq.np_dim(0)-1).and.(period(0).eq..false.)) then
                        call MPI_Cart_rank(mpi_world_cart, coords, curr_rank, ierr)
                        n_rank = n_rank + 1
                        rank(n_rank) = curr_rank
                        ! print *, myrank, coords(0), coords(1), coords(2), n_rank, rank(n_rank)
                    else if((j.eq.0 .or. j.eq.np_dim(1)-1).and.(period(1).eq..false.)) then
                        call MPI_Cart_rank(mpi_world_cart, coords, curr_rank, ierr)
                        n_rank = n_rank + 1
                        rank(n_rank) = curr_rank
                    else if((k.eq.0 .or. k.eq.np_dim(2)-1).and.(period(2).eq..false.)) then
                        call MPI_Cart_rank(mpi_world_cart, coords, curr_rank, ierr)
                        n_rank = n_rank + 1
                        rank(n_rank) = curr_rank
                    endif
                enddo
            enddo
        enddo
        if(n_rank.ne.0) then
            call MPI_Group_incl(group_cart, n_rank, rank, group_boundary, ierr)
        endif
        
        call MPI_Comm_create(mpi_world_cart, group_boundary, comm_boundary%mpi_comm, ierr)

        comm_boundary%nprocs = -1
        comm_boundary%myrank = -1

        if(comm_boundary%mpi_comm.ne.MPI_COMM_NULL) then
            call MPI_Comm_size(comm_boundary%mpi_comm, comm_boundary%nprocs, ierr)
            call MPI_Comm_rank(comm_boundary%mpi_comm, comm_boundary%myrank, ierr)
        endif

        deallocate(rank)
        call MPI_Group_free(group_cart,ierr)
        call MPI_Group_free(group_boundary,ierr)

    end subroutine mpi_boundary_create

Subroutine dec_to_bin(dec, bin, count)
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
end Subroutine

Subroutine bin_to_dec(bin, dec, count)
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
end Subroutine

Subroutine binary_repr(points, width, binary_out)
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

end Subroutine

Subroutine distance_from_point(points, pp, binary_out)
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

end Subroutine

! ROT rotates and flips a quadrant appropriately.
Subroutine rot(n,x,y,rx,ry)
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
end Subroutine

! xy2d converts a 2D Cartesian coordinate to a 1D Hilbert coordinate.
Subroutine xy2d(m,x,y,d)
Implicit None
!    Input, int M, the index of the Hilbert curve.
!    The number of cells is N=2^M.
!    0 < M.
integer, intent(in) :: m
!    Input, int X, Y, the Cartesian coordinates of a cell.
!    0 <= X, Y < N.
integer, intent(in) :: x,y
!    Output, int XY2D, the Hilbert coordinate of the cell.
!    0 <= D < N * N.
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

end Subroutine

end module mpi_topology
