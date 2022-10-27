program generate_job_log

use calculate_fragmentation_2D
use M_process ,only: process_readall, split

character(len=20) :: filename0
integer i,j,k
integer*4 :: Nnode

integer*4 :: ny, nz
logical hilbert
INTEGER*4 domain_size, total_domain_size
character(len=:),allocatable :: string
character(len=256) :: h_filename, n_filename, c_filename
character(len=256),allocatable :: array(:)
character(len=256),allocatable :: string_array(:)
real*8 :: elapsed_time(10), normal_alltoall, hilbert_alltoall, normal_overall, hilbert_overall, perf_impr
!real*4 :: ratios_64(0:1999), ratios_128(0:1999), ratios_256(0:1999)

INTEGER*4 job_id
real*4 :: results(16)

filename0 = "leaf_edge_node.list"

! write header info
write(*,*) "number, cores, size, total_size, normal_overall, normal_avg, normal_y_avg, normal_z_avg, hilbert_overall, ", &
    "hilbert_avg, hilbert_y_avg, hilbert_z_avg, perf_impr, normal_1st_y_frag, normal_1st_z_frag, normal_edge_y_frag, ", &
    "normal_edge_z_frag, normal_core_y_frag, normal_core_z_frag, hilbert_1st_y_frag, ", &
    "hilbert_1st_z_frag, hilbert_edge_y_frag, hilbert_edge_z_frag, hilbert_core_y_frag, hilbert_core_z_frag, "
                   
call readfile_from_node_list(filename0)
string=process_readall('ls out.hilbert.*',delim=NEW_LINE("A"),ierr=ierr)
call split(string,array,delimiters=NEW_LINE("A"))
do j=1,size(array)
!do j=1,1

call split(trim(array(j)),string_array, delimiters='.')
h_filename=trim(array(j))//char(0)
n_filename=trim(string_array(1))//"."//trim(string_array(3))//"."//trim(string_array(4))//"."//trim(string_array(5))
c_filename=trim(string_array(1))//".host."//trim(string_array(3))//"."//trim(string_array(4))//"."//trim(string_array(5))

read(string_array(3),*,iostat=ierr)  Nnode
ny = INT(sqrt(FLOAT(Nnode)))
nz = ny
read(string_array(4),*,iostat=ierr) job_id

!write(*,*) Nnode
call read_domainsize_from_file(h_filename, domain_size)
domain_size = domain_size*(domain_size/ny)*(domain_size/2/nz)*8/1024 ! Kbyte
write(*,fmt="(I10,A)", advance="no") job_id,", " ! job id
write(*,fmt="(I10,A)", advance="no") Nnode,", " ! cores 
write(*,fmt="(I8,A)", advance="no") domain_size,", "
total_domain_size = domain_size*ny ! Kbyte
write(*,fmt="(I8,A)", advance="no") total_domain_size,", "

call readfile_from_node_info_list(c_filename, Nnode)

call get_elaspsed_time(n_filename, elapsed_time)
write(*,fmt="(F10.5)", advance="no") elapsed_time(10)*20 ! overall. time
write(*,fmt="(A)", advance="no") ", "
write(*,fmt="(F10.5)", advance="no") elapsed_time(1) ! alltoall. time
write(*,fmt="(A)", advance="no") ", "
write(*,fmt="(F10.5)", advance="no") elapsed_time(4) ! alltoall_1. time
write(*,fmt="(A)", advance="no") ", "
write(*,fmt="(F10.5)", advance="no") elapsed_time(7) ! alltoall_2. time
write(*,fmt="(A)", advance="no") ", "
normal_overall = elapsed_time(10)
normal_alltoall=elapsed_time(1)

call get_elaspsed_time(h_filename, elapsed_time)
write(*,fmt="(F10.5)", advance="no") elapsed_time(10)*20 ! overall. time
write(*,fmt="(A)", advance="no") ", "
write(*,fmt="(F10.5)", advance="no") elapsed_time(1) ! avg. time
write(*,fmt="(A)", advance="no") ", "
write(*,fmt="(F10.5)", advance="no") elapsed_time(4) ! avg. time
write(*,fmt="(A)", advance="no") ", "
write(*,fmt="(F10.5)", advance="no") elapsed_time(7) ! avg. time
write(*,fmt="(A)", advance="no") ", "
hilbert_overall = elapsed_time(10)
hilbert_alltoall=elapsed_time(1)
perf_impr = normal_overall/hilbert_overall

write(*,fmt="(F10.5)", advance="no") perf_impr
write(*,fmt="(A)", advance="no") ", "

do i=1,2
    if (i.eq.1) then 
        hilbert = .false.
     !   write(*,*) "Normal case"
    else
        hilbert = .true.
     !   write(*,*) "Hilbert case"
    endif
    call calculate_fragmentation_2D_host_level_x(ny, nz, results(1+(i-1)*8), hilbert)   
    !write(*,*) "1st-level y-fragment: ", results(1+(i-1)*8)
    call calculate_fragmentation_2D_host_level_y(ny, nz, results(2+(i-1)*8), hilbert)
    !write(*,*) "1st-level z-fragment: ", results(2+(i-1)*8)

    call calculate_fragmentation_2D_edge_level_x(ny, nz, results(3+(i-1)*8), hilbert)
    !write(*,*) "edge-level y-fragment: ", results(3+(i-1)*8)
    call calculate_fragmentation_2D_edge_level_y(ny, nz, results(4+(i-1)*8), hilbert)
    !write(*,*) "edge-level z-fragment: ", results(4+(i-1)*8)

    call calculate_fragmentation_2D_core_level_x(ny, nz, results(5+(i-1)*8), hilbert)
    !write(*,*) "core-level y-fragment: ", results(5+(i-1)*8)
    call calculate_fragmentation_2D_core_level_y(ny, nz, results(6+(i-1)*8), hilbert)
    !write(*,*) "core-level z-fragment: ", results(6+(i-1)*8)
enddo

do k=1,14
    if (k.eq.7.or.k.eq.8) then
        !write(*,fmt="(F12.2,A)", advance="no") results(k), ", "
    else if (k.eq.14) then
        write(*,fmt="(F10.7,A)", advance="no") results(k)
    else
        write(*,fmt="(F10.7,A)", advance="no") results(k), ", " 
    endif
enddo

write(*,*) ""

call destory_node_info_list
call destory_newnode_list

enddo
!deallocate(array)

call destory_node_list()
end program

subroutine read_domainsize_from_file(filename, domain_size)
    implicit none
    character(len=256), intent(in) :: filename
    integer*4, intent(inout) :: domain_size
    character(len=10) :: temp, a, b, c
    integer u,iostatus
    
    !n = 0
    open (u, FILE=filename, STATUS='OLD')
  
    do while (.true.) 
      READ(u, *, IOSTAT=iostatus) temp
      IF (iostatus < 0) then
        write(*,*) "Error file", filename
        stop
      endif
      if (temp.eq."Double") exit
    enddo
    READ(u, *, IOSTAT=iostatus) a, b, c
    READ(b,'(I4)') domain_size
    close(u)
end subroutine
