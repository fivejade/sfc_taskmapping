module timer

    use mpi

    implicit none

    real(kind=8), private   :: t_zero, t_zero1, t_curr
    real(kind=8), public    :: t_array(36), t_array_sum(36), t_array_max(36), t_array_min(36)

    contains

    subroutine  timer_init

        t_array(:) = 0.0d0
        t_array_sum(:) = 0.0d0
        t_array_max(:) = 0.0d0
        t_array_min(:) = 0.0d0

    end subroutine timer_init

    subroutine timer_stamp0
        t_zero = MPI_Wtime()
    end subroutine timer_stamp0

    subroutine timer_stamp1
        t_zero1 = MPI_Wtime()
    end subroutine timer_stamp1

    subroutine timer_stamp(timer_id)
        integer(kind=4), intent(in) :: timer_id
        t_curr = MPI_Wtime()
        t_array(timer_id) = t_array(timer_id) + t_curr - t_zero
        t_zero = t_curr
    end subroutine timer_stamp

    subroutine timer_stamp1_end(timer_id)
        integer(kind=4), intent(in) :: timer_id
        t_curr = MPI_Wtime()
        t_array(timer_id) = t_array(timer_id) + t_curr - t_zero1
        t_zero1 = t_curr
    end subroutine timer_stamp1_end

    subroutine timer_start(timer_id)
        integer(kind=4), intent(in) :: timer_id
        t_array(timer_id) = MPI_Wtime()
    end subroutine timer_start

    subroutine timer_end(timer_id)
        integer(kind=4), intent(in) :: timer_id
        t_array(timer_id) = MPI_Wtime() - t_array(timer_id)
    end subroutine timer_end

    function timer_elapsed(timer_id) result(t_elapsed)
        integer(kind=4), intent(in) :: timer_id
        real(kind=8)    :: t_elapsed
        t_elapsed = MPI_Wtime() - t_array(timer_id)
        return
    end function timer_elapsed

    subroutine timer_reduction

        integer(kind=4) :: ierr
        call MPI_Reduce(t_array, t_array_sum, 36, MPI_REAL8, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
        call MPI_Reduce(t_array, t_array_max, 36, MPI_REAL8, MPI_MAX, 0, MPI_COMM_WORLD, ierr)
        call MPI_Reduce(t_array, t_array_min, 36, MPI_REAL8, MPI_MIN, 0, MPI_COMM_WORLD, ierr)

    end subroutine timer_reduction

end module timer
