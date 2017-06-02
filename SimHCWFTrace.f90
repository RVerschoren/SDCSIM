program SimHCWFTrace
    use utils, only : dp
    use hcwfsim, only : SSDHCWFTrace
    implicit none

    integer, parameter :: H=2
    integer :: b,d,maxPE,nruns,numreq,it,j, startrun
    real(dp) :: rho,f
    integer, dimension(:,:), allocatable :: requests
    logical :: exists, initrandom, fail
    character(len=32) :: arg
    character(len=4) :: traceid
    
    call get_command_argument(1, arg)
    read (arg, *) b
    call get_command_argument(2, arg)
    read (arg, *) d
    call get_command_argument(3, arg)
    read (arg, *) rho
    call get_command_argument(4, arg)
    read (arg, *) f
    call get_command_argument(5, arg)
    read (arg, *) startrun
    call get_command_argument(6, arg)
    read (arg, *) nruns
    call get_command_argument(7, arg)
    read (arg, *) maxPE
    call get_command_argument(8, arg)
    read (arg, *) numreq
    call get_command_argument(10, arg)
    read (arg, *) initrandom
    call get_command_argument(9, arg)
    traceid=arg(1:4) ! First 4 letters of trace filename as trace "ID"

    inquire(file=arg, EXIST=exists)
    if(exists) then
        allocate(requests(1:numreq,1:2))
        open(99, file=arg, status='old', action='read')
        do it = 1,numreq
            read(99,*) requests(it,:)
        end do
        call SSDHCWFTraceRuns(traceid,startrun,nruns,b,d,rho,f,maxPE,numreq, requests, initrandom)
        deallocate(requests)
    else
        stop 'Input file does not exist.'
    end if

    contains

    subroutine SSDHCWFTraceRuns(traceid,startrun,nruns,b,d,rho,f,maxPE,numreq, requests, initrandom)
            use utils, only : dp
            use rng, only : rng_seed, rng_t

            integer, intent(in) :: nruns,b,d,maxPE, numreq, startrun
            character(4), intent(in):: traceid
            real(dp), intent(in) :: rho,f
            logical, intent(in) :: initrandom
            integer, dimension(1:numreq,1:2), intent(in):: requests

            integer :: it, maxLBA
            type(rng_t), dimension(1:nruns) :: rng

            maxLBA=maxval(requests)
            print *, maxLBA, ceiling(dble(maxLBA)/(b*rho))

            do it=1,nruns
                call rng_seed(rng(it), 932117 + it +startrun-1)
                call SSDHCWFTrace(traceid, maxLBA, b,d,rho,f,maxPE,numreq,requests,it +startrun-1,rng(it), initrandom)
                print *, "done ", it +startrun-1
            end do
    end subroutine SSDHCWFTraceRuns

end program SimHCWFTrace
