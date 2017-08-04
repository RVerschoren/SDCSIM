program SimDWF

    use utils, only : dp
    !use sim, only : SSDDWFRuns
    use dwfsim, only : SSDDWF
    use rng, only: rng_t, rng_seed

    implicit none

    integer :: stat,b,d,N,maxPE,nruns,startrun,it
    real(dp) :: rho,r,f
    logical :: exists, initrandom
    character(len=32) :: arg


    !! Separate command line arguments
    ! b, d, rho, f, r, startrun, nruns, N, maxPE, initrandom
    call get_command_argument(1, arg)
    read (arg, *) b
    call get_command_argument(2, arg)
    read (arg, *) d
    call get_command_argument(3, arg)
    read (arg, *) rho
    call get_command_argument(4, arg)
    read (arg, *) f
    call get_command_argument(5, arg)
    read (arg, *) r
    call get_command_argument(6, arg)
    read (arg, *) startrun
    call get_command_argument(7, arg)
    read (arg, *) nruns
    call get_command_argument(8, arg)
    read (arg, *) N
    call get_command_argument(9, arg)
    read (arg, *) maxPE
    call get_command_argument(10, arg)
    read (arg, *) initrandom

    call SSDDWFRuns(nruns,startrun,N,b,d,rho,r,f,maxPE, initrandom)

    contains
    
    subroutine SSDDWFRuns(nruns,startrun,N,b,d,rho,r,f,maxPE, initrandom)
            use utils, only : dp
!            use rng, only : rng_seed, rng_t
    
            integer, intent(in) :: nruns,b,N,d,maxPE,startrun
            real(dp), intent(in) :: rho,r,f
            logical, intent(in) :: initrandom
    
            integer :: it
            type(rng_t), dimension(1:nruns) :: prng
    
            do it=1,nruns
                print *, it + startrun-1
                call rng_seed(prng(it), 932117 + it + startrun -1)
                call SSDDWF(N,b,d,rho,r,f,maxPE,it+startrun-1, prng(it), initrandom)
                print *, "done ", it + startrun -1
            end do
    end subroutine SSDDWFRuns

end program SimDWF
