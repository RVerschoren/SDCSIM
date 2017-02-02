program SimHCWF

    use utils, only : dp
    use sim, only : SSDHCWFRuns
    implicit none

    integer :: stat,b,d,N,maxPE,nruns,startrun
    real(dp) :: rho,r,f
    logical :: exists, initrandom
    character(len=32) :: arg

    !! Separate command line arguments
    ! b, d, rho, f, r, startrun, nruns, N, maxPE
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

    call SSDHCWFRuns(nruns,startrun, N,b,d,rho,r,f,maxPE, initrandom)

end program SimHCWF
