Program record_compute_nodes

use mpi
use hilbert
implicit none
integer(kind=4) :: ierr
INTEGER :: new_rank, nprocs, myrank
INTEGER :: i, j, k, argc, len, Nx
CHARACTER(len=32) :: arg
INTEGER*4 :: dd = 0
character(len=8) hostname
LOGICAL  problem /.false./
INTEGER, dimension(:), allocatable :: NEW_RANKS
integer nodeID, new_nodeID, p, hostid
integer, dimension(2) :: points2
integer, dimension(3) :: points3
integer*4, dimension(:), allocatable :: host_array

call MPI_INIT(ierr)
call MPI_Comm_size( MPI_COMM_WORLD, nprocs, ierr)
call MPI_Comm_rank( MPI_COMM_WORLD, myrank, ierr)
call MPI_GET_PROCESSOR_NAME(hostname, len, ierr)

if (myrank == 0) then
	argc = iargc()
	DO i = 1, iargc()
  		CALL getarg(i, arg)
		if (i==1) then
			read (arg,'(I2)') dd
			!write(*,*) "DD: ", dd
		endif
	END DO
	if (dd/=2.and.dd/=3) then
		call print_help()
		problem = .true.
	endif
endif

if (myrank == 0) then
	if (dd.eq.2) then
		Nx = INT(sqrt(REAL(nprocs))) 
		if (Nx*Nx/=nprocs) then
			write(*,*)  "Wrong number of processes: ", nprocs
			problem = .true.
		endif
	elseif (dd.eq.3) then
        Nx = INT(nprocs**(1.0/3.0))
		if (Nx*Nx*Nx/=nprocs) then
			write(*,*)  "Wrong number of processes: ", nprocs
			problem = .true.
		endif
	endif

    if (problem) then
       call MPI_Finalize (ierr)
       stop 
    endif

    ALLOCATE(NEW_RANKS(0:nprocs-1))
    if (dd.eq.3) then
        p = INT(LOG(REAL(Nx*Nx*Nx)) / LOG(2.) / 3)
	    do i=0,Nx-1
   	   	do j=0,Nx-1
       	do k=0,Nx-1
           	nodeID = k+(j)*Nx+(i)*Nx*Nx
            points3 = (/i,j,k/)
          	call distance_from_3d_points(points3, p, new_nodeID)
           	NEW_RANKS(new_nodeID)=nodeID
       	enddo
       	enddo
       	enddo
	elseif (dd.eq.2) then
        p = INT(LOG(REAL(Nx*Nx)) / LOG(2.) / 2)
        do i=0,Nx-1
   	   	do j=0,Nx-1
          	nodeID = j+(i)*Nx
            points2 = (/i,j/)
          	call distance_from_2d_points(points2, p, new_nodeID)
           	NEW_RANKS(new_nodeID)=nodeID
       	enddo
       	enddo
	endif
    write(*,*) "==== start of hilbert node ID info"
    do i=0, nprocs-1
       write(*,*) "prev_nodeID: ", i, " new_nodeID:",NEW_RANKS(i)
    enddo
    write(*,*) "==== end of hilbert node ID info"
endif

read(hostname(5:8),'(I4)') hostid
allocate(host_array(nprocs))
call mpi_gather(hostid, 1, MPI_INTEGER4, host_array, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)

if (myrank.eq.0) then
	write(*,*) "==== start of node info (rankid,hostid)"
    do i=0, nprocs-1
       write(*,*) i, host_array(i+1)
    enddo
    write(*,*) "==== end of node info"
endif

deallocate(host_array)
if (myrank.eq.0) then
    deallocate(NEW_RANKS)
endif
call MPI_Finalize (ierr)
End Program

subroutine print_help ()
	write(*,*) "Usage: record_compute_nodes dd"
	write(*,*) " dd means a domain decomposition strategy: "
    write(*,*) "   must be specified 2(2D) or 3(3D)"
	write(*,*) ""
	write(*,*) "This tool generates a list of MPI_ID and hostname pairs where pairs are "
	write(*,*) "re-mapped using hilbert-curve according to the domain decomposition stragegy."
	write(*,*) "For example, 'record_compute_nodes 2' records a list of pairs using "
	write(*,*) "hilbert-curve for 2D domain decomposition."
end subroutine


