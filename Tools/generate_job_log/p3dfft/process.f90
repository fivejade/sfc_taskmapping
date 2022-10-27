module M_process
    ! only: c_int, c_char, c_null_char, c_associated, c_ptr, c_null_ptr, c_new_line
    use, intrinsic :: ISO_C_BINDING
    implicit none
    character(len=*),parameter :: ident="@(#)M_process(3fm): call C process open,close,read,write functions "
    
    PRIVATE
    PUBLIC  ::  process_open_read  ! (cmd,fp,ierr)                  ! open process to read from
    PUBLIC  ::  process_open_write ! (cmd,fp,ierr)                  ! open process to write to
    PUBLIC  ::  process_close      ! (fp,ierr)                      ! close process
    PUBLIC  ::  process_readline   ! (string,fp,ierr)               ! read line from process
    PUBLIC  ::  process_readall    ! (cmd,ierr) result(string)      ! read all lines from process
    PUBLIC  ::  process_writeline  ! (string,fp,ierr)               ! write line to process
    PUBLIC  ::  split  
    PRIVATE ::  process_open       ! (fp,ierr)                      ! open process
    
    logical, PUBLIC ::  process_debug=.false.
    
    type, public       :: streampointer
       type (c_ptr)    :: handle = c_null_ptr
    end type streampointer
    
    
    interface process_writeline
       module procedure process_writeline_scalar, process_writeline_array
    end interface
    
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    ! popen
    interface
       function system_popen(path, mode) bind(C, name='popen')
          use, intrinsic :: ISO_C_BINDING
          character(kind=c_char), dimension(*) :: path, mode
          type (c_ptr) :: system_popen
       end function
    end interface
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !!##NAME
    !!    fgets(3fp) - get character string from a file or stream by calling fgets(3c)
    !!##SYNOPSIS
    !!
    !!    #include <stdio.h>
    !!    char *fgets(char *BUF, int N, FILE *FP);
    !!##DESCRIPTION
    !!    Reads at most N-1 characters from FP until a newline is found. The
    !!    characters including to the newline are stored in BUF. The buffer
    !!    is terminated with a 0.
    !!##RETURNS
    !!    fgets(3c) returns the buffer passed to it, with the data filled
    !!    in. If end of file occurs with some data already accumulated, the
    !!    data is returned with no other indication. If no data are read,
    !!    NULL is returned instead.
    !!##PORTABILITY
    !!    Note that fgets(3c) returns all of the data, including the newline.
    !===================================================================================================================================
    ! fgets
    interface
       function system_fgets(buf, siz, handle) bind(C, name='fgets')
          use, intrinsic :: ISO_C_BINDING
          type (c_ptr) :: system_fgets
          character(kind=c_char), dimension(*) :: buf
          integer(kind=c_int), value :: siz
          type (c_ptr), value :: handle
       end function
    end interface
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    ! pclose
    interface
       function system_pclose(handle) bind(C, name='pclose')
          use, intrinsic :: ISO_C_BINDING
          integer(c_int) :: system_pclose
          type (c_ptr), value :: handle
       end function
    end interface
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !!##NAME
    !!        fputs(3fp) - write a character string in a file or stream
    !!##SYNOPSIS
    !!
    !!        #include <stdio.h>
    !!        int fputs(const char *S, FILE *FP);
    !!##DESCRIPTION
    !!        `fputs'  writes  the string at S (but without the trailing null) to the
    !!        file or stream identified by FP.
    !!##RETURNS
    !!        If successful, the result is `0'; otherwise, the result is `EOF'.
    !!##PORTABILITY
    !!        ANSI  C  requires `fputs', but does not specify that the result on
    !!        success must be `0'; any non-negative value is permitted.
    !===================================================================================================================================
    interface
       function system_fputs(buf, handle) bind(C, name='fputs')
       use, intrinsic :: ISO_C_BINDING
          integer(c_int) :: system_fputs
          character(kind=c_char), dimension(*) :: buf
          type (c_ptr), value :: handle
       end function
    end interface
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !!##NAME
    !!        fflush(3fp) - flush buffered file output
    !!##SYNOPSIS
    !!
    !!      Syntax:
    !!
    !!       #include <stdio.h>
    !!       int fflush(FILE *FP);
    !!##DESCRIPTION
    !!      The `stdio' output functions can buffer output before delivering it to
    !!      the host system, in order to minimize the overhead of system calls.
    !!
    !!      Use `fflush' to deliver any such pending output (for the file or
    !!      stream identified by FP) to the host system.
    !!
    !!      If FP is `NULL', `fflush' delivers pending output from all open
    !!      files.
    !!
    !!      Additionally, if FP is a seekable input stream visiting a
    !!      file descriptor, set the position of the file descriptor to
    !!      match next unread byte, useful for obeying POSIX semantics when
    !!      ending a process without consuming all input from the stream.
    !!##RETURNS
    !!        fflush returns '0' unless it encounters a write error; in that
    !!        situation, it returns `EOF'.
    !!##PORTABILITY
    !!        ANSI C requires `fflush'.  The behavior on input streams is only
    !!        specified by POSIX, and not all implementations follow POSIX rules.
    !!
    !!        No supporting OS subroutines are required.
    !===================================================================================================================================
    interface
       function fflush(handle) bind(C, name='fflush')
       use, intrinsic :: ISO_C_BINDING
          integer(c_int) :: fflush
          type (c_ptr), value :: handle
       end function
    end interface
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    contains
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    subroutine process_open_read(cmd,fp,ierr)
    character(len=*),parameter :: ident="@(#)M_process::process_open_read(3f):open process to read from"
    
       character(len=*),intent(in)     :: cmd  ! shell command to start process with
       type(streampointer),intent(out) :: fp           ! file pointer returned for process
       integer,intent(out)             :: ierr         ! status for attempt to open process (0= no error)
    
       character(len=3),parameter      :: mode='r'     ! read/write mode parameter to pass to popen(3c)
    !-----------------------------------------------------------------------------------------------------------------------------------
       ierr=0
       call process_open(cmd,mode,fp,ierr)
    
    end subroutine process_open_read
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    subroutine process_open_write(cmd,fp,ierr)
    character(len=*),parameter :: ident="@(#)M_process::process_open_write(3f):open process to write to"
    
       character(len=*),intent(in)     :: cmd  ! shell command to start process with
       type(streampointer),intent(out) :: fp           ! file pointer returned for process
       integer,intent(out)             :: ierr         ! status for attempt to open process (0= no error)
       character(len=3),parameter      :: mode='w'     ! read/write mode parameter to pass to popen(3c)
    !-----------------------------------------------------------------------------------------------------------------------------------
       ierr=0
       call process_open(cmd,mode,fp,ierr)
    
    end subroutine process_open_write
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    subroutine process_open(cmd,mode,fp,ierr)
    character(len=*),parameter :: ident="@(#)M_process::process_open(3fp):open process"
    
       character(len=*),intent(in)     :: cmd  ! shell command to start process with
       character(len=*),intent(in)     :: mode         ! read/write/mode parameter to pass to popen(3c)
       type(streampointer),intent(out) :: fp           ! file pointer returned for process
       integer,intent(out)             :: ierr         ! status for attempt to open process (0= no error)
    !-----------------------------------------------------------------------------------------------------------------------------------
       ierr=0
       fp%handle = system_popen(trim(cmd) // C_NULL_CHAR, trim(mode) // C_NULL_CHAR)
    
       if (.not.c_associated(fp%handle)) then
          write(*,*) '*process_open_write* ERROR: Could not open pipe!'
          ierr=-1
       else
          if(process_debug)then
             write(*,*) '*process_open_write* Opened pipe successfully'
          endif
       end if
    
    end subroutine process_open
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    subroutine process_close(fp,ierr)
    character(len=*),parameter :: ident="@(#)M_process::process_close(3f):close process"
       ! DO NOT MAKE fp INTENT(IN)
       type(streampointer) :: fp           ! file pointer returned for process
       integer(c_int) ::  ios
       integer :: ierr
    !-----------------------------------------------------------------------------------------------------------------------------------
       ierr=fflush(fp%handle)
       ios=0
    
       if (.not.c_associated(fp%handle)) then
          write(*,*)'*process_close* process not found'
       else
          ios=system_pclose(fp%handle)
       endif
    
       if(process_debug)then
          write(*,*) '*process_close* Closed pipe with status ',ios
       endif
    
       ierr=min(-1_c_int,ios)
    
    end subroutine process_close
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    subroutine process_readline(readfrom,fp,ierr)
    character(len=*),parameter :: ident="@(#)M_process::process_readline(3f):read line from process"
    !  readfrom must be at least two
       character(len=*),intent(out)   :: readfrom
       type(streampointer),intent(in) :: fp
       integer,intent(out)            :: ierr
    
       integer (kind=c_int) :: clen
       integer :: eos, i
       integer :: ios
    !-----------------------------------------------------------------------------------------------------------------------------------
       clen=len(readfrom)-1
       readfrom=' '
    
       do while (c_associated(system_fgets(readfrom, clen, fp%handle)))
          eos=2
          do i=1, clen
             if (readfrom(i:i) == C_NULL_CHAR) then
                eos=i-2  ! assuming line terminator character and line string terminator should not be printed
                readfrom(eos+1:)=' '
                exit
             end if
          end do
          if(process_debug)then
             write(*,*) eos, ': "', trim(readfrom(1:eos)), '"'
          endif
          ierr=0
          return
       end do
    
       ios=0
       !!ios = system_pclose(fp%handle)
       !!if(process_debug)then
       !!   write(*,*) '*process_readline* Closed pipe with status ',ios
       !!endif
       ierr=min(-1,ios)
    
    end subroutine process_readline
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !!##NAME
    !!       process_readall(3f) - [M_process] read all lines from process into single string
    !!##SYNOPSIS
    !!
    !!       syntax:
    !!
    !!        function process_readall(cmd,delim,ierr)  result(string)
    !!        character(len=*),intent(in)              :: cmd
    !!        character(len=*),intent(in),optional     :: delim
    !!        integer,intent(out)                      :: ierr
    !!        character(len=:),allocatable             :: string
    !!##OPTIONS
    !!       cmd               command to pass to system
    !!       delim             delimiter to place between output lines when they
    !!                         are concatenated. Defaults to a space
    !!       ierr              check status of calls to process module routines
    !!##RESULTS
    !!       process_readall   Assuming sufficient memory is available all the output of the
    !!                         system command are concatenated into a string with
    !!                         spaces added between the output lines of the command.
    !!##EXAMPLE
    !!
    !!
    !!  Read all output of a command to a single string
    !!
    !!   program test_process_readall
    !!   use M_process ,only: process_readall
    !!   implicit none
    !!   integer :: ierr
    !!   character(len=:),allocatable :: string
    !!   string=process_readall('ls',ierr=ierr)
    !!   write(*,*)ierr,string
    !!   end program test_process_readall
    !!
    !!  Read all output of a command to an array using split(3f)
    !!
    !!    program test_process_readall
    !!    use M_process ,only: process_readall
    !!    use M_strings ,only: split
    !!    implicit none
    !!    integer                      :: ierr
    !!    integer                      :: i
    !!    character(len=:),allocatable :: string
    !!    !character(len=:),allocatable :: array(:)
    !!    character(len=256),allocatable :: array(:)
    !!       string=process_readall('ls',delim=NEW_LINE("A"),ierr=ierr)
    !!       call split(string,array,delimiters=NEW_LINE("A"))
    !!       do i=1,size(array)
    !!          write(*,'(i0,t10,"[",a,"]")')i,trim(array(i))
    !!       enddo
    !!    end program test_process_readall
    !!   Results:
    !!
    !!    1   [Articles]
    !!    2   [LIBRARY]
    !!    3   [PC]
    !!    4   [SHIP]
    !!    5   [SPEC]
    !!    6   [crib.dat]
    !!    7   [doc]
    !!    8   [html]
    !!    9   [index.html]
    !!    10  [plan.txt]
    !!    11  [questions]
    !!    12  [scripts]
    !!    13  [tmp]
    !!
    !!##SEE ALSO
    !!       M_process(3fm)
    !=============================================================================================================================
    function process_readall(cmd,delim,ierr)  result(string)      !! not hardened
    character(len=*),parameter     :: ident="@(#)M_process::process_readall(3f):read all lines from process"
    character(len=*),intent(in)              :: cmd
    character(len=:),allocatable             :: string      !! assume will not run out of memory
    integer,intent(out),optional             :: ierr
    character(len=*),intent(in),optional     :: delim
    character(len=:),allocatable             :: delim_local
       integer                               :: ierr_local(3), ierr_read
       integer                               :: i
       type(streampointer)                   :: fp
       character(len=4096)                   :: line        !! assumed long enough
    !------------------------------------------------------------------------------------------------------------------------------
       if(present(delim))then
          delim_local=delim
       else
          delim_local=' '
       endif
    
       !! change to stream I/O so do not have to have arbitrary line length limit, or at least make length an option
       string=''
       ierr_local(:)=0
       call process_open_read(cmd,fp,ierr_local(1))  ! start command
    
       if(ierr_local(1).eq.0)then
          do
             call process_readline(line,fp,ierr_read)  ! read line from command output
             if(ierr_read.ne.0)then
                exit
             endif
             string=string//trim(line)//delim_local
          enddo
          string=trim(string)
       endif
    
       call process_close(fp,ierr_local(3)) ! Wrap up
    
       if(present(ierr))then
          do i=1,size(ierr_local)
             if(ierr_local(i).ne.0)then
                ierr=ierr_local(i)
                exit
             endif
          enddo
       elseif(any(ierr_local.ne.0))then
          !!write(*,*)'*M_process::process_readall(3f)* error values=',ierr_local
          stop
       endif
    
    end function process_readall
    !-----------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
    !-----------------------------------------------------------------------------------------------------------------------------------
    subroutine process_writeline_scalar(writefrom,fp,ierr)
    character(len=*),parameter :: ident="@(#)M_process::process_writeline_scalar(3fp):write line to process"
    character(len=*),intent(in)    :: writefrom
    type(streampointer),intent(in) :: fp
    integer,intent(out)            :: ierr
    
       integer                     :: ios
    !-----------------------------------------------------------------------------------------------------------------------------------
       ierr=system_fputs(trim(writefrom)//C_NEW_LINE//C_NULL_CHAR,fp%handle)
       if(ierr.lt.0)then
          ios = system_pclose(fp%handle)
          if(process_debug)then
             write(*,*) '*process_writeline_scalar* Closed pipe with status ',ios
          endif
          ierr=min(-1,ios)
       endif
       if(ierr.eq.0)then
          ierr=fflush(fp%handle)
       endif
    
    end subroutine process_writeline_scalar

    subroutine process_writeline_array(writefrom,fp,ierr)
    character(len=*),parameter :: ident="@(#)M_process::process_writeline_array(3fp):write lines to process"
    character(len=*),intent(in)    :: writefrom(:)
    type(streampointer),intent(in) :: fp
    integer,intent(out)            :: ierr
       integer                     :: i
    !------------------------------------------------------------------------------------------------------------------------------
    
       ierr=0
       do i=1,size(writefrom,dim=1)
          call process_writeline_scalar(writefrom(i),fp,ierr)
          if(ierr.lt.0)exit
       enddo
    
    end subroutine process_writeline_array

    elemental pure function lower(str,begin,end) result (string)

    character(len=*),parameter::ident_19="@(#)M_strings::lower(3f): Changes a string to lowercase over specified range"
    
    character(*), intent(In)     :: str
    character(len(str))          :: string
    integer,intent(in),optional  :: begin, end
       integer                   :: i
       integer                   :: ibegin, iend
       string = str
    
       ibegin = 1
       if (present(begin))then
          ibegin = max(ibegin,begin)
       endif
    
       iend = len_trim(str)
       if (present(end))then
          iend= min(iend,end)
       endif
    
       do i = ibegin, iend                               ! step thru each letter in the string in specified range
          select case (str(i:i))
          case ('A':'Z')
             string(i:i) = char(iachar(str(i:i))+32)     ! change letter to miniscule
          case default
          end select
       end do
    
    end function lower

   !===================================================================================================================================
   subroutine split(input_line,array,delimiters,order,nulls)
   !-----------------------------------------------------------------------------------------------------------------------------------
    
    character(len=*),parameter::ident_6="&
    &@(#)M_strings::split(3f): parse string on delimiter characters and store tokens into an allocatable array"
    

    !  John S. Urban
    !-----------------------------------------------------------------------------------------------------------------------------------
       intrinsic index, min, present, len
    !-----------------------------------------------------------------------------------------------------------------------------------
    !  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
    !    o by default adjacent delimiters in the input string do not create an empty string in the output array
    !    o no quoting of delimiters is supported
       character(len=*),intent(in)              :: input_line  ! input string to tokenize
       character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
       character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
       character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
       character(len=*),allocatable,intent(out) :: array(:)    ! output array of tokens
    !-----------------------------------------------------------------------------------------------------------------------------------
       integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
       integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
       integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
       character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
       character(len=:),allocatable  :: ordr                   ! string containing order keyword
       character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
       integer                       :: ii,iiii                ! loop parameters used to control print order
       integer                       :: icount                 ! number of tokens found
       integer                       :: ilen                   ! length of input string with trailing spaces trimmed
       integer                       :: i10,i20,i30            ! loop counters
       integer                       :: icol                   ! pointer into input string as it is being parsed
       integer                       :: idlim                  ! number of delimiter characters
       integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
       integer                       :: inotnull               ! count strings not composed of delimiters
       integer                       :: ireturn                ! number of tokens returned
       integer                       :: imax                   ! length of longest token
    !-----------------------------------------------------------------------------------------------------------------------------------
       ! decide on value for optional DELIMITERS parameter
       if (present(delimiters)) then                                     ! optional delimiter list was present
          if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
             dlim=delimiters
          else                                                           ! DELIMITERS was specified on call as empty string
             dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
          endif
       else                                                              ! no delimiter value was specified
          dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
       endif
       idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
    !-----------------------------------------------------------------------------------------------------------------------------------
       if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
       if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
    !-----------------------------------------------------------------------------------------------------------------------------------
       n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
       allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
       allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
       ibegin(:)=1
       iterm(:)=1
    !-----------------------------------------------------------------------------------------------------------------------------------
       ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
       icount=0                                                       ! how many tokens found
       inotnull=0                                                     ! how many tokens found not composed of delimiters
       imax=0                                                         ! length of longest token found
    !-----------------------------------------------------------------------------------------------------------------------------------
       select case (ilen)
    !-----------------------------------------------------------------------------------------------------------------------------------
       case (:0)                                                      ! command was totally blank
    !-----------------------------------------------------------------------------------------------------------------------------------
       case default                                                   ! there is at least one non-delimiter in INPUT_LINE if get here
          icol=1                                                      ! initialize pointer into input line
          INFINITE: do i30=1,ilen,1                                   ! store into each array element
             ibegin(i30)=icol                                         ! assume start new token on the character
             if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
                iterm(i30)=ilen                                       ! initially assume no more tokens
                do i10=1,idlim                                        ! search for next delimiter
                   ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
                   IF(ifound.gt.0)then
                      iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
                   endif
                enddo
                icol=iterm(i30)+2                                     ! next place to look as found end of this token
                inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
             else                                                     ! character is a delimiter for a null string
                iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
                icol=icol+1                                           ! advance pointer into input string
             endif
             imax=max(imax,iterm(i30)-ibegin(i30)+1)
             icount=i30                                               ! increment count of number of tokens found
             if(icol.gt.ilen)then                                     ! no text left
                exit INFINITE
             endif
          enddo INFINITE
    !------------------------------------------------------------------------------------------------------------------------------
       end select
    !------------------------------------------------------------------------------------------------------------------------------
       select case (trim(adjustl(nlls)))
       case ('ignore','','ignoreend')
          ireturn=inotnull
       case default
          ireturn=icount
       end select
       !X!allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
       allocate(array(ireturn))                                      ! allocate the array to turn
    !-----------------------------------------------------------------------------------------------------------------------------------
       select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
       case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
       case default             ; ii=1       ; iiii=1                 ! first to last
       end select
    !-----------------------------------------------------------------------------------------------------------------------------------
       do i20=1,icount                                                ! fill the array with the tokens that were found
          if(iterm(i20).lt.ibegin(i20))then
             select case (trim(adjustl(nlls)))
             case ('ignore','','ignoreend')
             case default
                array(ii)=' '
                ii=ii+iiii
             end select
          else
             array(ii)=input_line(ibegin(i20):iterm(i20))
             ii=ii+iiii
          endif
       enddo
    !-----------------------------------------------------------------------------------------------------------------------------------
       end subroutine split
    !------------------------------------------------------------------------------------------------------------------------------
    !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
    !------------------------------------------------------------------------------------------------------------------------------
    end module M_process