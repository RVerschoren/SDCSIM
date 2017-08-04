program SimHCORACLETrace
    use utils, only : dp
    use hcwfsim, only : SSDHCORACLETrace
    implicit none

    integer :: b,d,maxPE,nruns,numreq,it,j,startrun,lookahead,lpn,lpncnt,hotThreshold
    integer :: hotness,nrFrames
    real(dp) :: rho,f
    integer, dimension(:,:), allocatable :: requests
    logical, dimension(:), allocatable :: isHotRequest
    logical :: exists, oracleexists, initrandom, fail
    character(len=32) :: arg, tracefile, oraclefile
    character(len=4) :: traceid
    
    !32  10 0.9 0.010      1 1 10 5000 $linecount $trace T $traceid-010-1-oracle.csv
    
    call get_command_argument(1, arg)
    read (arg, *) b
    call get_command_argument(2, arg)
    read (arg, *) d
    call get_command_argument(3, arg)
    read (arg, *) rho
    call get_command_argument(4, arg)
    read (arg, *) f
    call get_command_argument(5, arg)
    read (arg, *) nrFrames
    call get_command_argument(6, arg)
    read (arg, *) startrun
    call get_command_argument(7, arg)
    read (arg, *) nruns
    call get_command_argument(8, arg)
    read (arg, *) maxPE
    call get_command_argument(9, arg)
    read (arg, *) numreq
    call get_command_argument(10, tracefile)
    traceid=tracefile(1:4) ! First 4 letters of trace filename as trace "ID"
    call get_command_argument(11, arg)
    read (arg, *) initrandom
    call get_command_argument(12, oraclefile)

    inquire(file=tracefile, EXIST=exists)
    inquire(file=oraclefile, EXIST=oracleexists)
    if(exists .and. oracleexists) then
        allocate(requests(1:numreq,1:2))
        allocate(isHotRequest(1:numreq))
        open(99, file=tracefile, status='old', action='read')
        open(98, file=oraclefile, status='old', action='read')
        ! Read trace
        do it = 1,numreq
            read(99,*) requests(it,:)
            
            read(98,*) hotness
            isHotRequest(it) = hotness > 0
            if(mod(it,100) == 0) then
                print *, it, requests(it,:)
            end if
        end do
        
        call SSDHCORACLETraceRuns(traceid,startrun,nruns,b,d,rho,f,nrFrames,maxPE,numreq, requests,isHotRequest, initrandom)
        deallocate(requests)
        deallocate(isHotRequest)
    else
        stop 'Input file does not exist.'
    end if


    contains
    
    subroutine SSDHCORACLETraceRuns(traceid,startrun,nruns,b,d,rho,f,nrFrames,maxPE,numreq, requests,isHotRequest, initrandom)
        use rng, only : rng_seed, rng_t

        integer, intent(in) :: nruns,b,d,maxPE, numreq, startrun, nrFrames
        character(4), intent(in):: traceid
        real(dp), intent(in) :: rho,f
        logical, intent(in) :: initrandom
        integer, dimension(1:numreq,1:2), intent(in):: requests
        logical, dimension(1:numreq), intent(in):: isHotRequest

        integer :: it, maxLBA
        type(rng_t), dimension(1:nruns) :: rng

        maxLBA=maxval(requests)
        !print *, maxLBA, ceiling(dble(maxLBA)/(b*rho))

        do it=1,nruns
            call rng_seed(rng(it), 932117 + it +startrun-1)
            call SSDHCORACLETrace(traceid, maxLBA, b,d,rho,f,nrFrames,maxPE,numreq,requests,isHotRequest,&
                it +startrun-1,rng(it), initrandom)
            print *, "done ", it +startrun-1
        end do
    end subroutine SSDHCORACLETraceRuns

end program SimHCORACLETrace
