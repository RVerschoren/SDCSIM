program SimHCWFTrace
    use utils, only : dp
    use sim, only : SSDCOLDDCHTraceRuns
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
        call SSDCOLDDCHTraceRuns(traceid,startrun,nruns,b,d,rho,f,maxPE,numreq, requests, initrandom)
        deallocate(requests)
    else
        stop 'Input file does not exist.'
    end if

end program SimHCWFTrace
