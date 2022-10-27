module calculate_fragmentation_2D
  
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
 
subroutine destory_newnode_list()
implicit none 
    deallocate(newnode_list)
end subroutine

subroutine readfile_from_node_list(file_name)
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
#ifdef DEBUG
do i=1,NODES_NUM
    write(*,*) i, node_list(i)%core_sw_num, node_list(i)%rack_num, node_list(i)%unit_num, node_list(i)%sw_num
enddo
#endif
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

Subroutine calculate_fragmentation_2D_host_level_z(ny, nz, fragment, hilbert)
implicit none
integer*4, intent(in) :: ny, nz
real*4, intent(inout) :: fragment
logical, intent(in) :: hilbert
real*4, dimension(:), allocatable :: frags
integer*4 :: i, j, k, l, cnt1, cnt2, new_rankID1, new_rankID2
    
allocate(frags(nz))
do j=1,nz
    cnt1=0
    cnt2=0
    do k=1,ny
        new_rankID1=(k-1)*nz+j
        do l=1,k-1
            new_rankID2=(l-1)*nz+j
            !write(*,*) "compare ", new_rankID1-1, node_info_list(new_rankID1-1)%host_id, new_rankID2-1, &
            !    node_info_list(new_rankID2-1)%host_id
            if (hilbert) then
                if (in_same_node(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
            else
                if (in_same_node(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) cnt1=cnt1+1
            endif
            cnt2 = cnt2 + 1
        enddo
        !if (k.eq.l) cnt=cnt+1
    enddo
    frags(j)=real(cnt2-cnt1)/real(cnt2)
!#ifdef DEBUG
    !write(*,*) "z_frag: ", j, k, " cnt: ", cnt1, cnt2, " frag: ", frags(j)
!#endif
enddo
fragment = 0.0
do j=1,nz
    fragment = fragment + frags(j)
enddo
fragment = fragment/real(nz)
!deallocate(frags)
End Subroutine

Subroutine calculate_fragmentation_2D_host_level_y(ny, nz, fragment, hilbert)
    implicit none
    integer*4, intent(in) :: ny, nz
    real*4, intent(inout) :: fragment
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, l, cnt1, cnt2, new_rankID1, new_rankID2
        
    allocate(frags(ny))
    do j=1,ny
        cnt1=0
        cnt2=0
        do k=1,nz
            new_rankID1=(j-1)*nz+k
            do l=1,k-1
                new_rankID2=(j-1)*nz+l
                !if (k.eq.2) then
                !    write(*,*) "compare ", new_rankID1-1, node_info_list(new_rankID1-1)%host_id, new_rankID2-1, &
                !        node_info_list(new_rankID2-1)%host_id, in_same_node(node_info_list(new_rankID1-1)%host_id,&
                !        node_info_list(new_rankID2-1)%host_id)
                !endif
                if (hilbert) then
                    if (in_same_node(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                        node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
                else
                    if (in_same_node(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) cnt1=cnt1+1
                endif
                cnt2 = cnt2 + 1 
            enddo
            !if (k.eq.l) cnt=cnt+1
        enddo
        !frags(j)=real(cnt)/real(ny)
        frags(j)=real(cnt2-cnt1)/real(cnt2)
!#ifdef DEBUG
        !write(*,*) "y-frag:", j, k, " cnt: ", cnt1, cnt2, " frag: ", frags(j)
!#endif
    enddo
    fragment = 0.0
    do j=1,ny
        fragment = fragment + frags(j)
    enddo
    fragment = fragment/real(ny)
    deallocate(frags)
End Subroutine
    
Subroutine calculate_fragmentation_2D_edge_level_z(ny, nz, fragment, hilbert)
    implicit none
    integer*4, intent(in) :: ny, nz
    real*4, intent(inout) :: fragment
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, l, cnt1, cnt2, new_rankID1, new_rankID2
        
    allocate(frags(nz))
    do j=1,nz
        cnt1=0
        cnt2=0
        do k=1,ny
            new_rankID1=(k-1)*nz+j
            do l=1,k-1
                new_rankID2=(l-1)*nz+j
                !write(*,*) "compare ", new_rankID1-1, node_info_list(new_rankID1-1)%host_id, new_rankID2-1, &
                !    node_info_list(new_rankID2-1)%host_id
                if (hilbert) then
                    if (in_same_edge_sw(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                    node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
                else
                    if (in_same_edge_sw(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) cnt1=cnt1+1
                endif
                cnt2=cnt2+1
            enddo
            !if (k.eq.l) cnt=cnt+1
        enddo
        frags(j)=real(cnt2-cnt1)/real(cnt2)
!#ifdef DEBUG
 !   write(*,*) "z_frag: ", j, k, " cnt: ", cnt, " frag: ", frags(j)
!#endif
    enddo
    fragment = 0.0
    do j=1,nz
        fragment = fragment + frags(j)
    enddo
    fragment = fragment/real(nz)
    deallocate(frags)
End Subroutine
    
Subroutine calculate_fragmentation_2D_edge_level_y(ny, nz, fragment, hilbert)
    implicit none
    integer*4, intent(in) :: ny, nz
    real*4, intent(inout) :: fragment
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, l, cnt1, cnt2, new_rankID1, new_rankID2
        
    allocate(frags(ny))
    do j=1,ny
        cnt1=0
        cnt2=0
        do k=1,nz
            new_rankID1=(j-1)*nz+k
            do l=1,k-1
                new_rankID2=(j-1)*nz+l
                !write(*,*) "compare ", new_rankID1-1, node_info_list(new_rankID1-1)%host_id, new_rankID2-1, &
                !    node_info_list(new_rankID2-1)%host_id
                if (hilbert) then
                    if (in_same_edge_sw(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                    node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
                else
                    if (in_same_edge_sw(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) cnt1=cnt1+1
                endif
                cnt2=cnt2+1
            enddo
            !if (k.eq.l) cnt=cnt+1
        enddo
        frags(j)=real(cnt2-cnt1)/real(cnt2)
!#ifdef DEBUG
!        write(*,*) "y-frag:", j, k, " cnt: ", cnt, " frag: ", frags(j)
!#endif
    enddo
    fragment = 0.0
    do j=1,ny
        fragment = fragment + frags(j)
    enddo
    fragment = fragment/real(ny)
    deallocate(frags)
End Subroutine
    
Subroutine calculate_fragmentation_2D_core_level_z(ny, nz, fragment, hilbert)
    implicit none
    integer*4, intent(in) :: ny, nz
    real*4, intent(inout) :: fragment
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, l, cnt1, cnt2, new_rankID1, new_rankID2
        
    allocate(frags(nz))
    do j=1,nz
        cnt1=0
        cnt2=0
        do k=1,ny
            new_rankID1=(k-1)*nz+j
            do l=1,k-1
                new_rankID2=(l-1)*nz+j
                !write(*,*) "compare ", new_rankID1-1, node_info_list(new_rankID1-1)%host_id, new_rankID2-1, &
                !    node_info_list(new_rankID2-1)%host_id
                if (hilbert) then
                    if (in_same_core_sw(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                    node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
                else
                    if (in_same_core_sw(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) cnt1=cnt1+1
                endif
                cnt2=cnt2+1
            enddo
            !if (k.eq.l) cnt=cnt+1
        enddo
        frags(j)=real(cnt2-cnt1)/real(cnt2)
        !#ifdef DEBUG
         !   write(*,*) "z_frag: ", j, k, " cnt: ", cnt, " frag: ", frags(j)
        !#endif
    enddo
    fragment = 0.0
    do j=1,nz
        fragment = fragment + frags(j)
    enddo
    fragment = fragment/real(nz)
    deallocate(frags)
End Subroutine
    
Subroutine calculate_fragmentation_2D_core_level_y(ny, nz, fragment, hilbert)
    implicit none
    integer*4, intent(in) :: ny, nz
    real*4, intent(inout) :: fragment
    logical, intent(in) :: hilbert
    real*4, dimension(:), allocatable :: frags
    integer*4 :: i, j, k, l, cnt1, cnt2, new_rankID1, new_rankID2
        
    allocate(frags(ny))
    do j=1,ny
        cnt1=0
        cnt2=0
        do k=1,nz
            new_rankID1=(j-1)*nz+k
            do l=1,k-1
                new_rankID2=(j-1)*nz+l
                !write(*,*) "compare ", new_rankID1-1, node_info_list(new_rankID1-1)%host_id, new_rankID2-1, &
                !    node_info_list(new_rankID2-1)%host_id
                if (hilbert) then
                    if (in_same_core_sw(node_info_list(newnode_list(new_rankID1-1))%host_id,&
                        node_info_list(newnode_list(new_rankID2-1))%host_id)) cnt1=cnt1+1
                else
                    if (in_same_core_sw(node_info_list(new_rankID1-1)%host_id,node_info_list(new_rankID2-1)%host_id)) cnt1=cnt1+1
                endif
                cnt2=cnt2+1
            enddo
            !if (k.eq.l) cnt=cnt+1
        enddo
        frags(j)=real(cnt2-cnt1)/real(cnt2)
!#ifdef DEBUG
!        write(*,*) "y-frag:", j, k, " cnt: ", cnt, " frag: ", frags(j)
!#endif
    enddo
    fragment = 0.0
    do j=1,ny
        fragment = fragment + frags(j)
    enddo
    fragment = fragment/real(ny)
    deallocate(frags)
End Subroutine

Subroutine get_elaspsed_time(filename, elapsed_time)
use M_process ,only: split
implicit none

character(len=256) :: filename
character(len=100) :: string1, string2, string3, string4, string5, string6
character(len=256),allocatable :: array(:)
real*8, intent(inout) :: elapsed_time(10)
integer i, u, iostatus

!write(*,*) "reading file: ", filename
open (u, FILE=filename, STATUS='OLD', IOSTAT=iostatus)
IF (iostatus < 0) return
DO
  ! rank,hostname,rack_num,unit_num,sw_num
  READ(u, "(A)", IOSTAT=iostatus) string1
  IF (iostatus < 0) EXIT
  IF (string1(1:9).eq."Transform") then
    string2 = strtok(string1(31:), " ")
    READ(string2, '(E8.6)') elapsed_time(10)
  elseif (string1(1:10).eq."Unpackrecv") then
    READ(u, *, IOSTAT=iostatus) string1, string2, string3, string4
    READ(string2(2:),'(E8.6)') elapsed_time(1)
    READ(string3,'(E8.6)') elapsed_time(2)
    READ(string4(1:len(string4)-1),'(E8.6)') elapsed_time(3)
  elseif (string1(1:10).eq."Alltoall_0") then
    READ(u, *, IOSTAT=iostatus) string1, string2, string3, string4
    READ(string2(2:),'(E8.6)') elapsed_time(4)
    READ(string3,'(E8.6)') elapsed_time(5)
    READ(string4(1:len(string4)-1),'(E8.6)') elapsed_time(6)
    READ(u, *, IOSTAT=iostatus) string1, string2, string3, string4
    READ(string2(2:),'(E8.6)') elapsed_time(7)
    READ(string3,'(E8.6)') elapsed_time(8)
    READ(string4(1:len(string4)-1),'(E8.6)') elapsed_time(9)
    exit
  endif
END DO
!#ifdef DEBUG
!write(*,100) elapsed_time(1), " ", elapsed_time(2), " ", elapsed_time(7)
!100 format (F8.6, A, F8.6, A, F8.6)

!#endi
close(u)

End Subroutine

  
subroutine readfile_from_node_info_list(filename, list_num)
    implicit none
    character(len=256), intent(IN) :: filename
    integer, intent(in) :: list_num
    integer u,iostatus,i,n,rank
    character(len=10) :: temp, a, b
    integer*4, dimension(:), allocatable :: newnodes

    allocate(node_info_list(0:list_num-1))
    allocate(newnodes(0:list_num-1))
    
    !write(*,*) "reading file: ", filename
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
    ! rank,hostname,rack_num,unit_num,sw_num
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

    !#ifdef DEBUG
    !do n=0,list_num-1
    !    write(*,*) n, node_info_list(n)%host_id, node_info_list(n)%rack_num,&
    !        node_info_list(n)%unit_num,node_info_list(n)%sw_num
    !enddo
    !#endif
    !write(*,*) sw_name, sw_num, node_num

    deallocate(newnodes)
end subroutine
    
subroutine destory_node_info_list()
    implicit none
    deallocate(node_info_list)
end subroutine

CHARACTER*255 FUNCTION strtok (source_string, delimiters)

!     @(#) Tokenize a string in a similar manner to C routine strtok(3c). 
!
!     Usage:  First call STRTOK() with the string to tokenize as SOURCE_STRING,
!             and the delimiter list used to tokenize SOURCE_STRING in DELIMITERS.
!
!             then, if the returned value is not equal to CHAR(0), keep calling until it is
!             with SOURCE_STRING set to CHAR(0).
!
!            STRTOK will return a token on each call until the entire line is processed,
!            which it signals by returning CHAR(0). 
!
!     Input:  source_string =   Source string to tokenize. 
!             delimiters    =   delimiter string.  Used to determine the beginning/end of each token in a string.
!
!     Output: strtok()
!
!     LIMITATIONS:
!     can not be called with a different string until current string is totally processed, even from different procedures
!     input string length limited to set size
!     function returns fixed 255 character length
!     length of returned string not given

!     PARAMETERS:
      CHARACTER(len=*),intent(in)  :: source_string
      CHARACTER(len=*),intent(in)  :: delimiters

!     SAVED VALUES:
      CHARACTER(len=255),save :: saved_string
      INTEGER,save :: isaved_start  ! points to beginning of unprocessed data
      INTEGER,save :: isource_len   ! length of original input string

!     LOCAL VALUES:
      INTEGER :: ibegin        ! beginning of token to return
      INTEGER :: ifinish       ! end of token to return

      ! initialize stored copy of input string and pointer into input string on first call
      IF (source_string(1:1) .NE. CHAR(0)) THEN
          isaved_start = 1                 ! beginning of unprocessed data
          saved_string = source_string     ! save input string from first call in series
          isource_len = LEN(saved_string)  ! length of input string from first call
      ENDIF

      ibegin = isaved_start

      DO
         IF ( (ibegin .LE. isource_len) .AND. (INDEX(delimiters,saved_string(ibegin:ibegin)) .NE. 0)) THEN
             ibegin = ibegin + 1
         ELSE
             EXIT
         ENDIF
      ENDDO

      IF (ibegin .GT. isource_len) THEN
          strtok = CHAR(0)
          RETURN
      ENDIF

      ifinish = ibegin

      DO
         IF ((ifinish .LE. isource_len) .AND.  (INDEX(delimiters,saved_string(ifinish:ifinish)) .EQ. 0)) THEN
             ifinish = ifinish + 1
         ELSE
             EXIT
         ENDIF
      ENDDO

      !strtok = "["//saved_string(ibegin:ifinish-1)//"]"
      strtok = saved_string(ibegin:ifinish-1)
      isaved_start = ifinish

END FUNCTION strtok

end module
