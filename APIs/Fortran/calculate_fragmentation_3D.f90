module calculate_fragmentation_3D
  
implicit none

type node_type
integer*4 :: core_sw_num
integer*4 :: rack_num 
integer*4 :: unit_num
integer*4 :: sw_num
end type

integer*4, parameter :: NODES_NUM = 8305
type (node_type), dimension(:), allocatable:: node_list

type node_info_type
integer*4 :: host_id
integer*4 :: rack_num
integer*4 :: unit_num
integer*4 :: sw_num
end type

type (node_info_type), dimension(:), allocatable:: node_info_list
integer*4, dimension(:), allocatable:: newnode_list

contains

subroutine readfile_from_newnode_list(file_name, list_num)
    implicit none
    character(len=20) :: file_name
    character(len=10) :: a, b
    integer list_num
    integer u,iostatus,i,n,rank
    
    allocate(newnode_list(0:list_num-1))
    
    n = 0
    open (u, FILE=file_name, STATUS='OLD')
    DO
      ! rank,hostname,rack_num,unit_num,sw_num
      READ(u, *, IOSTAT=iostatus) a, rank, b, newnode_list(n)
      IF (iostatus < 0) EXIT
      n=n+1
    END DO
        !write(*,*) sw_name, sw_num, node_num
    close(u)
end subroutine  
  
subroutine destory_newnode_list()
    implicit none 
    deallocate(newnode_list)
end subroutine

subroutine readfile_from_node_info_list(filename, list_num)
    implicit none
    character(len=256), intent(IN) :: filename
    integer, intent(in) :: list_num
    integer u,iostatus,i,n,rank
    character(len=10) :: temp, a, b
    integer*4, dimension(:), allocatable :: newnodes

    allocate(newnodes(0:list_num-1))
    allocate(node_info_list(0:list_num-1))
    
    !write(*,*) "reading file: ", filename
    n = 0
    open (u, FILE=filename, STATUS='OLD', IOSTAT=iostatus)
    if (iostatus<0) return
    
    do while (.true.) 
        READ(u, *, IOSTAT=iostatus) temp
        if (temp.eq."====") exit
    enddo
        
    allocate(newnode_list(0:list_num-1))
    DO i=1,list_num
       ! rank,hostname,rack_num,unit_num,sw_num
       READ(u, *, IOSTAT=iostatus) a, rank, b, newnode_list(i-1)
       IF (iostatus < 0) EXIT
    END DO
    
    do i=1,2
        READ(u, *) 
    enddo 
  
    DO i=1,list_num
        READ(u, *, IOSTAT=iostatus) rank, newnodes(i-1)
        IF (iostatus < 0) EXIT
    ENDDO
    close(u)
    
    do n=0,list_num-1  
        node_info_list(n)%host_id = newnodes(n)
        node_info_list(n)%rack_num = node_list(node_info_list(n)%host_id)%rack_num
        node_info_list(n)%unit_num = node_list(node_info_list(n)%host_id)%unit_num
        node_info_list(n)%sw_num = node_list(node_info_list(n)%host_id)%sw_num
    enddo    

    deallocate(newnodes)
end subroutine
  
subroutine destory_node_info_list()
    implicit none
    deallocate(node_info_list)
end subroutine

subroutine readfile_from_edgenode_list(file_name)
implicit none
character(len=20) :: file_name
integer n,u,iostatus,i
character(len=10) :: node_num
character(len=10) :: edge_sw_name
character(len=10) :: core_sw_name
integer*4:: sw_num
allocate(node_list(NODES_NUM))
!open (u, FILE="./leaf_edge_node.list", STATUS='OLD')
open (u, FILE=file_name, STATUS='OLD')
DO
  READ(u, *, IOSTAT=iostatus) core_sw_name, edge_sw_name, sw_num, node_num
  IF (iostatus < 0) EXIT
  !IF (n.le.10) write(*,*) sw_name, sw_num, "_", node_num(1:4), "_"
  IF (node_num(1:4).eq.'node') then
      !write(*,*) sw_name, sw_num, node_num
    read(node_num(5:8),'(I4)') i
    if (edge_sw_name(7:7)=="u") then
        read(edge_sw_name(5:6),'(I4)') node_list(i)%rack_num
        read(edge_sw_name(8:9),'(I4)') node_list(i)%unit_num
    elseif (edge_sw_name(8:8)=="u") then
        read(edge_sw_name(5:7),'(I4)') node_list(i)%rack_num
        read(edge_sw_name(9:10),'(I4)') node_list(i)%unit_num
    endif
	read(core_sw_name(2:5),'(I4)') node_list(i)%core_sw_num
    !node_list(i)%sw_name = sw_name(4:)
    node_list(i)%sw_num = sw_num
  endif
END DO
!write(*,*) sw_name, sw_num, node_num
close(u)
end subroutine

subroutine destory_node_list()
implicit none
deallocate(node_list)
end subroutine

logical function in_same_node(i,j)
implicit none
integer*4, intent(in) :: i,j
in_same_node=i.eq.j
return
end function

logical function in_same_edge_sw(i,j)
implicit none
integer*4, intent(in) :: i,j
logical x,y
x=node_list(i)%rack_num.eq.node_list(j)%rack_num
y=node_list(i)%unit_num.eq.node_list(j)%unit_num
in_same_edge_sw=x.and.y
return
end function

logical function in_same_core_sw(i,j)
implicit none
integer*4, intent(in) :: i,j
in_same_core_sw=node_list(i)%core_sw_num.eq.node_list(j)%core_sw_num
return
end function

Subroutine calculate_fragmentation_3D_host_level_z(nx, ny, nz, fragmentation, hilbert)
implicit none
integer*4, intent(in) :: nx, ny, nz
real*4, intent(inout) :: fragmentation
logical, intent(in) :: hilbert
real*4, dimension(:), allocatable :: frags
integer*4 :: i, j, k, i2, j2, cnt1, cnt2, new_rankID1, new_rankID2
allocate(frags(nz))
do k=1,nz
    cnt1=0
    cnt2=0
    do i=1,nx
    do j=1,ny
        new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
        !new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
        do i2=1,nx
        do j2=1,ny
           if (((i-1)*ny+j).le.((i2-1)*ny+j2-1)) then
                new_rankID2=(i2-1)*ny*nz+(j2-1)*nx+k
                !new_rankID2=(j-1)*nx*nz+(i2-1)*nz+k2
                if (hilbert) then
                    if (in_same_node(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                    node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
                else
                    if (in_same_node(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) cnt1=cnt1+1
                endif
                cnt2=cnt2+1
            endif
        enddo
        enddo
    enddo
    enddo
    frags(k) = real(cnt2-cnt1)/real(cnt2)
enddo
fragmentation = 0.0
do k=1,nz
    fragmentation = fragmentation + frags(k)
enddo
fragmentation = fragmentation/real(nz)
deallocate(frags)
End Subroutine

Subroutine calculate_fragmentation_3D_host_level_y(nx, ny, nz, fragmentation, hilbert)
    implicit none
    integer*4, intent(in) :: nx, ny, nz
    real*4, intent(inout) :: fragmentation
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, i2, k2, cnt1, cnt2, new_rankID1, new_rankID2
    allocate(frags(ny))
    do j=1,ny
        cnt1=0
        cnt2=0
        do i=1,nx
        do k=1,nz
            new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            !new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            do i2=1,nx
            do k2=1,nz
               if (((i-1)*nz+k).le.((i2-1)*nz+k2-1)) then
                    new_rankID2=(i2-1)*ny*nz+(j-1)*nx+k2
                    !new_rankID2=(j-1)*nx*nz+(i2-1)*nz+k2
                    if (hilbert) then
                        if (in_same_node(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                            node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
                    else
                        if (in_same_node(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) cnt1=cnt1+1
                    endif
                    cnt2=cnt2+1
                endif
            enddo
            enddo
        enddo
        enddo
        frags(j) = real(cnt2-cnt1)/real(cnt2)
    enddo
    fragmentation = 0.0
    do j=1,ny
        fragmentation = fragmentation + frags(j)
    enddo
    fragmentation = fragmentation/real(ny)
    deallocate(frags)
End Subroutine
    
subroutine calculate_fragmentation_3D_host_level_x(nx, ny, nz, fragmentation, hilbert)
    implicit none
    integer*4, intent(in) :: nx, ny, nz
    real*4, intent(inout) :: fragmentation
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, j2, k2, cnt1, cnt2, new_rankID1, new_rankID2        
    allocate(frags(nx))
    do i=1,nx
        cnt1=0
        cnt2=0
        do j=1,ny
        do k=1,nz
            new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            do j2=1,ny
            do k2=1,nz
               if (((j-1)*nz+k).le.((j2-1)*nz+k2-1)) then
                    new_rankID2=(i-1)*ny*nz+(j2-1)*nx+k2
                    !if (i.le.1.and.j.le.2.and.k.le.2.and.j2.le.2.and.k2.le.2) then 
                    !endif
                    if (hilbert) then
                        if (in_same_node(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                            node_info_list(newnode_list(new_rankID2-1))%host_id)) then
                            !write(*,*)
                            !if (cnt1.le.10) then
                            !write(*,*) cnt1, "new_rankID1: ", new_rankID1, "new_rankID2: ", new_rankID2, " newnode_list1: ", &
                            !    newnode_list(new_rankID1-1), " newnode_list2: ", newnode_list(new_rankID2-1), &
                            !    " nodeinfo1: ", node_info_list(newnode_list(new_rankID1-1))%host_id, & 
                            !    " nodeinfo2: ", node_info_list(newnode_list(new_rankID2-1))%host_id
                            !endif    
                            cnt1=cnt1+1
                        endif
                    else
                        if (in_same_node(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) cnt1=cnt1+1
                    endif
                    cnt2=cnt2+1
                endif
            enddo
            enddo
        enddo
        enddo
        frags(i) = real(cnt2-cnt1)/real(cnt2)
        !write(*,*) "i:", i, frags(i)
    enddo
    fragmentation = 0.0
    do i=1,nx
        fragmentation = fragmentation + frags(i)
    enddo
    fragmentation = fragmentation/real(nx)
    deallocate(frags)
End Subroutine
    
Subroutine calculate_fragmentation_2D_edge_level_z(nx, ny, nz, fragmentation, hilbert)
    implicit none
    integer*4, intent(in) :: nx, ny, nz
    real*4, intent(inout) :: fragmentation
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, i2, j2, cnt1, cnt2, new_rankID1, new_rankID2
    allocate(frags(nz))    
    do k=1,nz
        cnt1=0
        cnt2=0
        do i=1,nx
        do j=1,ny
            new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            !new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            do i2=1,nx
            do j2=1,ny
               if (((i-1)*ny+j).le.((i2-1)*ny+j2-1)) then
                    new_rankID2=(i2-1)*ny*nz+(j2-1)*nx+k
                    !new_rankID2=(j-1)*nx*nz+(i2-1)*nz+k2
                    if (hilbert) then
                        if (in_same_edge_sw(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                        node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
                    else
                        if (in_same_edge_sw(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) &
                            cnt1=cnt1+1
                    endif
                    cnt2=cnt2+1
                endif
            enddo
            enddo
        enddo
        enddo
        frags(k) = real(cnt2-cnt1)/real(cnt2)
    enddo
    fragmentation = 0.0
    do k=1,nz
        fragmentation = fragmentation + frags(k)
    enddo
    fragmentation = fragmentation/real(nz)
    deallocate(frags)
End Subroutine
    
Subroutine calculate_fragmentation_2D_edge_level_y(nx, ny, nz, fragmentation, hilbert)
    implicit none
    integer*4, intent(in) :: nx, ny, nz
    real*4, intent(inout) :: fragmentation
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, i2, k2, cnt1, cnt2, new_rankID1, new_rankID2
    allocate(frags(ny))
    do j=1,ny
        cnt1=0
        cnt2=0
        do i=1,nx
        do k=1,nz
            new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            !new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            do i2=1,nx
            do k2=1,nz
               if (((i-1)*nz+k).le.((i2-1)*nz+k2-1)) then
                    new_rankID2=(i2-1)*ny*nz+(j-1)*nx+k2
                    !new_rankID2=(j-1)*nx*nz+(i2-1)*nz+k2
                    if (hilbert) then
                        if (in_same_edge_sw(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                        node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
                    else
                        if (in_same_edge_sw(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) &
                            cnt1=cnt1+1
                    endif
                    cnt2=cnt2+1
                endif
            enddo
            enddo
        enddo
        enddo
        frags(j) = real(cnt2-cnt1)/real(cnt2)
    enddo
    fragmentation = 0.0
    do j=1,ny
        fragmentation = fragmentation + frags(j)
    enddo
    fragmentation = fragmentation/real(ny)
    deallocate(frags)
End Subroutine
    
subroutine calculate_fragmentation_2D_edge_level_x(nx, ny, nz, fragmentation, hilbert)
    implicit none
    integer*4, intent(in) :: nx, ny, nz
    real*4, intent(inout) :: fragmentation
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, j2, k2, cnt1, cnt2, new_rankID1, new_rankID2        
    allocate(frags(nx))
    do i=1,nx
        cnt1=0
        cnt2=0
        do j=1,ny
        do k=1,nz
            new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            do j2=1,ny
            do k2=1,nz
               if (((j-1)*nz+k).le.((j2-1)*nz+k2-1)) then
                    new_rankID2=(i-1)*ny*nz+(j2-1)*nx+k2
                    if (hilbert) then
                        if (in_same_edge_sw(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                            node_info_list(newnode_list(new_rankID2-1))%host_id)) then
                                cnt1=cnt1+1
                                !if (cnt1.le.10) then
                                !    write(*,*) cnt1, "new_rankID1: ", new_rankID1, "new_rankID2: ", new_rankID2, &
                                !        " newnode_list1: ", &
                                !        newnode_list(new_rankID1-1), " newnode_list2: ", newnode_list(new_rankID2-1), &
                                !        " nodeinfo1: ", node_info_list(newnode_list(new_rankID1-1))%host_id, & 
                                !        " nodeinfo2: ", node_info_list(newnode_list(new_rankID2-1))%host_id
                                !endif                                    
                        endif
                    else
                        if (in_same_edge_sw(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) &
                            cnt1=cnt1+1
                    endif
                    cnt2=cnt2+1
                endif
            enddo
            enddo
        enddo
        enddo
        frags(i) = real(cnt2-cnt1)/real(cnt2)
        !write(*,*) "i:", i, frags(i)
    enddo
    fragmentation = 0.0
    do i=1,nx
        fragmentation = fragmentation + frags(i)
    enddo
    fragmentation = fragmentation/real(nx)
    !write(*,*) "avg: " ,fragment
    deallocate(frags)
End Subroutine

Subroutine calculate_fragmentation_2D_core_level_z(nx, ny, nz, fragmentation, hilbert)
    implicit none
    integer*4, intent(in) :: nx, ny, nz
    real*4, intent(inout) :: fragmentation
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, i2, j2, cnt1, cnt2, new_rankID1, new_rankID2
    allocate(frags(nz))    
    do k=1,nz
        cnt1=0
        cnt2=0
        do i=1,nx
        do j=1,ny
            new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            !new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            do i2=1,nx
            do j2=1,ny
               if (((i-1)*ny+j).le.((i2-1)*ny+j2-1)) then
                    new_rankID2=(i2-1)*ny*nz+(j2-1)*nx+k
                    !new_rankID2=(j-1)*nx*nz+(i2-1)*nz+k2
                    if (hilbert) then
                        if (in_same_core_sw(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                        node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
                    else
                        if (in_same_core_sw(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) &
                            cnt1=cnt1+1
                    endif
                    cnt2=cnt2+1
                endif
            enddo
            enddo
        enddo
        enddo
        frags(k) = real(cnt2-cnt1)/real(cnt2)
    enddo
    fragmentation = 0.0
    do k=1,nz
        fragmentation = fragmentation + frags(k)
    enddo
    fragmentation = fragmentation/real(nz)
    deallocate(frags)

End Subroutine
    
Subroutine calculate_fragmentation_2D_core_level_y(nx, ny, nz, fragmentation, hilbert)
    implicit none
    integer*4, intent(in) :: nx, ny, nz
    real*4, intent(inout) :: fragmentation
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, i2, k2, cnt1, cnt2, new_rankID1, new_rankID2
    allocate(frags(ny))
    do j=1,ny
        cnt1=0
        cnt2=0
        do i=1,nx
        do k=1,nz
            new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            !new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            do i2=1,nx
            do k2=1,nz
               if (((i-1)*nz+k).le.((i2-1)*nz+k2-1)) then
                    new_rankID2=(i2-1)*ny*nz+(j-1)*nx+k2
                    !new_rankID2=(j-1)*nx*nz+(i2-1)*nz+k2
                    if (hilbert) then
                        if (in_same_core_sw(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                        node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
                    else
                        if (in_same_core_sw(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) &
                            cnt1=cnt1+1
                    endif
                    cnt2=cnt2+1
                endif
            enddo
            enddo
        enddo
        enddo
        frags(j) = real(cnt2-cnt1)/real(cnt2)
    enddo
    fragmentation = 0.0
    do j=1,ny
        fragmentation = fragmentation + frags(j)
    enddo
    fragmentation = fragmentation/real(ny)
    deallocate(frags)
End Subroutine
    
subroutine calculate_fragmentation_2D_core_level_x(nx, ny, nz, fragmentation, hilbert)
    implicit none
    integer*4, intent(in) :: nx, ny, nz
    real*4, intent(inout) :: fragmentation
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, j2, k2, cnt1, cnt2, new_rankID1, new_rankID2        
    allocate(frags(nx))
    do i=1,nx
        cnt1=0
        cnt2=0
        do j=1,ny
        do k=1,nz
            new_rankID1=(i-1)*ny*nz+(j-1)*nz+k
            do j2=1,ny
            do k2=1,nz
               if (((j-1)*nz+k).le.((j2-1)*nz+k2-1)) then
                    new_rankID2=(i-1)*ny*nz+(j2-1)*nx+k2
                    if (hilbert) then
                        if (in_same_core_sw(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                        node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
                    else
                        if (in_same_core_sw(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) &
                            cnt1=cnt1+1
                    endif
                    cnt2=cnt2+1
                endif
            enddo
            enddo
        enddo
        enddo
        frags(i) = real(cnt2-cnt1)/real(cnt2)
    enddo
    fragmentation = 0.0
    do i=1,nx
        fragmentation = fragmentation + frags(i)
    enddo
    fragmentation = fragmentation/real(nx)
    deallocate(frags)

End Subroutine

Subroutine get_elaspsed_time(filename, elapsed_time)
use M_process ,only: split
implicit none

character(len=256) :: filename
character(len=60) :: string1, string2, string3, string4, string5, string6
character(len=256),allocatable :: array(:)
real*8, intent(inout) :: elapsed_time(7)
integer i, u, iostatus

!write(*,*) "reading file: ", filename
open (u, FILE=filename, STATUS='OLD', IOSTAT=iostatus)
IF (iostatus < 0) return
DO
  ! rank,hostname,rack_num,unit_num,sw_num
  READ(u, *, IOSTAT=iostatus) string1, string2
  IF (iostatus < 0) EXIT
  IF (string1.eq."[Poisson]".and.string2.eq."RMS") then 
    READ(u, *, IOSTAT=iostatus) string1, string2, string3, string4, string5, string6, elapsed_time(1)
    IF (iostatus < 0) EXIT

    DO i=1, 30
        READ(u, *, IOSTAT=iostatus) 
    ENDDO
    READ(u, *, IOSTAT=iostatus) string1, string2, string3, string4, string5, elapsed_time(2) ! mpi send/recv x-axis
    READ(u, *, IOSTAT=iostatus) string1, string2, string3, string4, string5, elapsed_time(3) ! mpi send/recv y-axis
    READ(u, *, IOSTAT=iostatus) string1, string2, string3, string4, string5, elapsed_time(4) ! mpi send/recv z-axis
    READ(u, *, IOSTAT=iostatus) string1, string2, string3, string4, string5, elapsed_time(5) ! mpi_allreduce x-axis
    READ(u, *, IOSTAT=iostatus) string1, string2, string3, string4, string5, elapsed_time(6) ! mpi_allreduce y-axis
    READ(u, *, IOSTAT=iostatus) string1, string2, string3, string4, string5, elapsed_time(7) ! mpi_allreduce z-axis
    exit
  endif
END DO
!#ifdef DEBUG
!do n=0,list_num-1
!    write(*,*) n, newnode_list(n)
!enddo
!#endif
!write(*,*) sw_name, sw_num, node_num
close(u)


End Subroutine


end module
