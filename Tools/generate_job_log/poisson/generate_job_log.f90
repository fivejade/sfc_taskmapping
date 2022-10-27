program generate_job_log

use calculate_fragmentation_3D
use M_process ,only: process_readall, split

character(len=20) :: filename0
integer i,j,k,cnt
integer*4 :: sw_num
integer*4 :: Nnode

integer*4 :: domain_size
integer*4 :: nx, ny, nz
real*4, dimension(:), allocatable :: frags
real*4 fragment
real*4 communication_cost
logical hilbert
INTEGER*4 status, system
character(len=:),allocatable :: string
character(len=256) :: h_filename, n_filename, c_filename
character(len=256),allocatable :: array(:)
character(len=256),allocatable :: string_array(:)
real*8 :: elapsed_time(7)
real*8 :: normal_overall, hilbert_overall, normal_p2p_comm, normal_coll_comm, hilbert_p2p_comm, hilbert_coll_comm

INTEGER*4 job_id
real*4 :: results(12)

filename0 = "leaf_edge_node.list"

write(*,*) "number, cores, size, normal_overall, normal_x_p2p, normal_y_p2p, normal_z_p2p, normal_x_coll, normal_y_coll, ", &
    "normal_z_coll, normal_p2p, normal_coll, hilbert_overall, hilbert_x_p2p, hilbert_y_p2p, hilbert_z_p2p, hilbert_x_coll, ", &
    "hilbert_y_coll, hilbert_z_coll, hilbert_p2p, hilbert_coll, overall_perf_impr, p2p_perf_impr, coll_perf_impr, ", &
    "normal_1st_x_frag, normal_1st_y_frag, normal_1st_z_frag, normal_edge_x_frag, normal_edge_y_frag, ", &
    "normal_edge_z_frag, normal_core_x_frag, normal_core_y_frag, normal_core_z_frag, ", &
    "hilbert_1st_x_frag, hilbert_1st_y_frag, hilbert_1st_z_frag, ", &
    "hilbert_edge_x_frag, hilbert_edge_y_frag, hilbert_edge_z_frag, hilbert_core_x_frag, hilbert_core_y_frag, ", &
    "hilbert_core_z_frag"

call readfile_from_edgenode_list(filename0)
string=process_readall('ls out.hilbert.*',delim=NEW_LINE("A"),ierr=ierr)
call split(string,array,delimiters=NEW_LINE("A"))

do j=1,size(array)
!do j=1,11,10

call split(trim(array(j)),string_array, delimiters='.')
h_filename=trim(array(j))//char(0)
!n_filename=trim(string_array(1))//"."//trim(string_array(3))//"."//trim(string_array(4))
!c_filename=trim(string_array(1))//".host."//trim(string_array(3))//"."//trim(string_array(4))
n_filename=trim(string_array(1))//"."//trim(string_array(3))//"."//trim(string_array(4))//"."//trim(string_array(5))
c_filename=trim(string_array(1))//".host."//trim(string_array(3))//"."//trim(string_array(4))//"."//trim(string_array(5))

!read(string_array(3),*,iostat=ierr)  Nnode
!ny = INT(sqrt(FLOAT(Nnode)))
!nz = ny
read(string_array(4),*,iostat=ierr) job_id
!read(string_array(3),*,iostat=ierr) job_id

!call readfile_from_newnode_list(filename2, Nnode)
call read_domainsize_from_file(h_filename, nx, ny, nz, domain_size, Nnode)
!call readfile_from_node_info_list(filename1, Nnode)

domain_size = (domain_size/nx)*(domain_size/ny)*(domain_size/nz)*8/1024 ! Kbyte
write(*,fmt="(I10,A)", advance="no") job_id,", " ! job id
write(*,fmt="(I10,A)", advance="no") Nnode,", " ! cores 
write(*,fmt="(I8,A)", advance="no") domain_size,", "
!total_domain_size = domain_size*ny ! Kbyte
!write(*,fmt="(I8,A)", advance="no") total_domain_size,", "

call readfile_from_node_info_list(c_filename, Nnode)

call get_elaspsed_time(n_filename, elapsed_time)
do i=1,7
   write(*,fmt="(F10.5)", advance="no") elapsed_time(i)
   write(*,fmt="(A)", advance="no") ", "
enddo
normal_overall = elapsed_time(1)
normal_p2p_comm = elapsed_time(2)+elapsed_time(3)+elapsed_time(4)
normal_coll_comm = elapsed_time(5)+elapsed_time(6)+elapsed_time(7)
write(*,fmt="(F10.5)", advance="no") normal_p2p_comm
write(*,fmt="(A)", advance="no") ", "
write(*,fmt="(F10.5)", advance="no") normal_coll_comm
write(*,fmt="(A)", advance="no") ", "

call get_elaspsed_time(h_filename, elapsed_time)
do i=1,7
   write(*,fmt="(F10.5,A)", advance="no") elapsed_time(i), ", "
enddo
hilbert_overall = elapsed_time(1)
hilbert_p2p_comm = elapsed_time(2)+elapsed_time(3)+elapsed_time(4)
hilbert_coll_comm = elapsed_time(5)+elapsed_time(6)+elapsed_time(7)
write(*,fmt="(F10.5)", advance="no") hilbert_p2p_comm
write(*,fmt="(A)", advance="no") ", "
write(*,fmt="(F10.5)", advance="no") hilbert_coll_comm
write(*,fmt="(A)", advance="no") ", "

write(*,fmt="(F10.5)", advance="no") normal_overall/hilbert_overall
write(*,fmt="(A)", advance="no") ", "
write(*,fmt="(F10.5)", advance="no") normal_p2p_comm/hilbert_p2p_comm
write(*,fmt="(A)", advance="no") ", "
write(*,fmt="(F10.5)", advance="no") normal_coll_comm/hilbert_coll_comm
write(*,fmt="(A)", advance="no") ", "

do i=1,2
    if (i.eq.1) then 
        hilbert = .false.
    !    write(*,*) "Normal case"
    else
        hilbert = .true.
    !    write(*,*) "Hilbert case"
    endif
    call calculate_fragmentation_3D_host_level_x(nx, ny, nz, results(1), hilbert)
    !write(*,*) "1st-level x-fragment: ",  results(1)
    call calculate_fragmentation_3D_host_level_y(nx, ny, nz, results(2), hilbert)
    !write(*,*) "1st-level y-fragment: ", results(2)
    call calculate_fragmentation_3D_host_level_z(nx, ny, nz, results(3), hilbert)
    !write(*,*) "1st-level z-fragment: ", results(3)
 

    call calculate_fragmentation_2D_edge_level_x(nx, ny, nz, results(4), hilbert)
    !write(*,*) "edge-level x-fragment: ",  results(4)
    call calculate_fragmentation_2D_edge_level_y(nx, ny, nz, results(5), hilbert)
    !write(*,*) "edge-level y-fragment: ", results(5)
    call calculate_fragmentation_2D_edge_level_z(nx, ny, nz, results(6), hilbert)
    !write(*,*) "edge-level z-fragment: ", results(6)

    call calculate_fragmentation_2D_core_level_x(nx, ny, nz, results(7), hilbert)
    !write(*,*) "core-level x-fragment: ",  results(7)
    call calculate_fragmentation_2D_core_level_y(nx, ny, nz, results(8), hilbert)
    !write(*,*) "core-level y-fragment: ", results(8)
    call calculate_fragmentation_2D_core_level_z(nx, ny, nz, results(9), hilbert)
    !write(*,*) "core-level z-fragment: ", results(9)

    do k=1,9
        if (k.eq.9.and.i.eq.2) then
            write(*,fmt="(F10.5)") results(k)
        else
            write(*,fmt="(F10.5,A)", advance="no") results(k), ", "
        endif
    enddo
enddo

call destory_node_info_list
call destory_newnode_list
enddo
deallocate(array)

call destory_node_list()

end program

subroutine read_domainsize_from_file(filename, nx, ny, nz, domain_size, list_num)
    implicit none
    character(len=256), intent(in) :: filename
    integer*4, intent(inout) :: nx, ny, nz
    integer*4, intent(inout) :: domain_size
    integer, intent(inout) :: list_num
    character(len=10) :: a, b, c
    integer u,iostatus,i,n,rank
    
    n = 0
    open (u, FILE=filename, STATUS='OLD')
    READ(u, *, IOSTAT=iostatus) a, b, c, list_num
    READ(u, *, IOSTAT=iostatus)
    READ(u, *, IOSTAT=iostatus)
    
    !write(*,*) "Node num: ", list_num 

    if (list_num.eq.32768) then
    nx = 32
    ny = 32
    nz = 32
    domain_size = 4096
    elseif (list_num.eq.4096) then
    nx = 16
    ny = 16
    nz = 16
    domain_size = 2048
    elseif (list_num.eq.512) then
    nx = 8
    ny = 8
    nz = 8
    domain_size = 1024
    endif

    close(u)
end subroutine
