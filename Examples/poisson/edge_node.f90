module edge_node
  
implicit none

type edge_node_type
!character(len=7) :: sw_name
integer*4 :: rack_num
integer*4 :: unit_num
integer*4 :: sw_num
end type

integer, parameter :: EDGE_NODE_NUM = 8305

type (edge_node_type), dimension(:), allocatable:: edge_node_list

contains

subroutine readfile_from_edgenode_list()
implicit none
integer n,u,iostatus,i,ierr
character(len=10) :: node_num
character(len=10) :: sw_name
integer*4:: sw_num

allocate(edge_node_list(EDGE_NODE_NUM))

open (u, FILE="edge_node.list", STATUS='OLD', iostat=ierr)
if (ierr /=0) then
write(*,*)'error#',ierr,'- edge_node.list file not found' 
deallocate(edge_node_list)
stop 
endif
n=0
DO
  READ(u, *, IOSTAT=iostatus) sw_name, sw_num, node_num
  IF (iostatus < 0) EXIT
    n = n + 1
  !IF (n.le.10) write(*,*) sw_name, sw_num, "_", node_num(1:4), "_"
  IF (node_num(1:4).eq.'node') then
      !write(*,*) sw_name, sw_num, node_num
    read(node_num(5:8),'(I4)') i
    if (sw_name(7:7)=="u") then
        read(sw_name(5:6),'(I4)') edge_node_list(i)%rack_num
        read(sw_name(8:9),'(I4)') edge_node_list(i)%unit_num
    elseif (sw_name(8:8)=="u") then
        read(sw_name(5:7),'(I4)') edge_node_list(i)%rack_num
        read(sw_name(9:10),'(I4)') edge_node_list(i)%unit_num
    endif
    !edge_node_list(i)%sw_name = sw_name(4:)
    edge_node_list(i)%sw_num = sw_num
  endif
END DO
!do i=1,EDGE_NODE_NUM
!    write(*,*) i, edge_node_list(i)%rack_num, edge_node_list(i)%unit_num, edge_node_list(i)%sw_num
!enddo
!write(*,*) sw_name, sw_num, node_num
close(u)
end subroutine

subroutine destory_edge_node_list()
implicit none
deallocate(edge_node_list)
end subroutine

logical function in_same_sw(i,j)
implicit none
integer*4, intent(in) :: i,j
logical x,y
x=edge_node_list(i)%rack_num.eq.edge_node_list(j)%rack_num
y=edge_node_list(i)%unit_num.eq.edge_node_list(j)%unit_num
in_same_sw=x.and.y
return
end function

end module
