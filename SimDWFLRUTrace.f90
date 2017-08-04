subroutine SSDDWFTrace(traceid,maxLBA, b,d,rho,f,maxPE,numreq,requests,runit,rng, initrandom)
        use rng, only: rng_t, randi, rng_uniform

        integer, parameter :: hotsections=1000, testhotnesscount = 1000
        type(rng_t), intent(inout) :: rng
        character(4) :: traceid
        integer, intent(in):: maxLBA,b,d,maxPE,runit,numreq
        integer, intent(in), dimension(1:numreq,1:2) ::requests
        real(dp), intent(in) :: rho,f
        logical, intent(in), optional :: initrandom

        real(dp), allocatable, dimension(:) :: dist
        real(dp), dimension(0:maxPE) :: endurance, fairness, rhoeff
        real(dp), dimension(0:b) :: validdist
        integer(8), dimension(0:b*(b+1)) :: victimhotness, transientdist

        integer :: N,it,WFI,WFE,p,bl,hotness,i,j,jst,k,lpn,victim,maxnumvalid,kdiff,&
                    distL, distU,distit,temphot,temphotindex,&
                    gccalls,numgccalls,currentPE,sumPE,testhotnessinterval,&
                    bi,bj,WFEindex,WFIindex,hotj,hotk, maxnumhot
        integer(16) :: intW, extW, maxit, reqit, numreqlong
        integer, allocatable, dimension(:):: PE,validPages, hotValidPages
        integer, dimension(1:b) :: victimcontent
        integer, dimension(0:b) :: WFEhotness,WFIhotness,victimValids
        integer, allocatable, dimension(:,:):: FTL, SSD
        character(len=64) :: distfilename,endufilename, fairfilename,rhoefffilename,&
                        validfilename,WFEhfilename,WFIhfilename,victimfilename, &
                        victimhotfilename,transientdistfilename, WAfilename

        real(dp) :: pageCount, WA
        logical :: failure, isWrite

        N=ceiling(dble(maxLBA)/(b*rho))
        maxit=N*maxPE
        pageCount=dble(b*N)
        maxnumvalid=maxLBA
        testhotnessinterval=maxit/testhotnesscount
        numreqlong=numreq

        !! Initialize
        allocate(FTL(1:maxnumvalid,1:2))
        allocate(PE(1:N))
        allocate(validPages(1:N))
        allocate(hotValidPages(1:N))
        allocate(SSD(1:b,1:N))

        SSD=0
        FTL=0
        WFI=1
        WFE=2
        maxnumhot=floor(f*maxLBA)


        WFEindex=0
        WFIindex=0
        reqit=0
        hotValidPages=0
        validPages=0

        if( present(initrandom) .and. initrandom) then
            do it=1, maxnumvalid
                failure=.true.
                do while(failure)
                    p =randi(rng, b)
                    bl=randi(rng, N)

                    if(SSD(p,bl) <= 0 .and. bl /= WFE .and. bl /= WFI) then
                        SSD(p,bl) = it
                        FTL(it,1)=p
                        FTL(it,2)=bl
                        failure=.false.
                    end if
                end do
            end do
            validPages = count(SSD > 0, 1)
            hotValidPages = count(0 < SSD .and. SSD <= maxnumhot,1)
        else
            !! Warmup with trace
            do while(sum(validPages) < maxnumvalid)
                do while(WFEindex < b)

                    reqit=reqit+1
                    reqit=mod(reqit-1,numreqlong)+1
                    lpn=requests(mod(reqit, numreqlong)+1,1)
                    isWrite=requests(mod(reqit, numreqlong)+1,2) /= 0
                    p=FTL(lpn,1)
                    bl=FTL(lpn,2)

                    if(lpn <= maxnumhot) then
                        hotness=1
                    else
                        hotness=0
                    end if

                    if(isWrite) then
                        WFEindex=WFEindex+1
                        if(p /= 0) then !Invalidate if valid page
                            !Remove old page
                            SSD(p,bl) = 0
                            validPages(bl)=validPages(bl)-1
                            hotValidPages(bl)=hotValidPages(bl)-hotness
                        end if
                        !Write update to WFE
                        FTL(lpn, 1:2) =(/ WFEindex, WFE/)
                        SSD(WFEindex,WFE)=lpn
                        validPages(WFE)=validPages(WFE)+1
                        hotValidPages(WFE)=hotValidPages(WFE)+hotness
                    else if (p /= 0) then ! TRIM
                        SSD(p,bl) = 0
                        validPages(bl)=validPages(bl)-1
                        hotValidPages(bl)=hotValidPages(bl)-hotness
                        FTL(lpn, 1:2) =(/0, 0/)
                    end if
                end do !(WFEvalid < b)

                !! GCA invocation
                failure=.true.

                do while(failure)
                    validPages((/WFE,WFI/))=b+1
                    victim=GC(rng,d,N,validPages)
                    validPages((/WFE,WFI/))=count(SSD(:,(/WFE,WFI/)) > 0, 1)

                    if(victim /= WFE .and. victim /= WFI) then
                        j=validPages(victim)
                        hotj=hotValidPages(victim)

                        bi=hotValidPages(victim)
                        bj=validPages(victim)

                        jst=WFIindex! Not equal to validPages(WFI), because incoming requests can invalidate in WFI
                        k=b-jst !Free in WFI
                        victimcontent=SSD(1:b,victim)

                        !! Erase victim
                        SSD(:,victim)=0
                        validPages(victim)=0
                        hotValidPages(victim)=0

                        if(j <=  k) then ! Sufficient space to copy over to WFI
                            !! Copy all content to WFI
                            WFIindex=jst
                            do i=1,b
                                lpn=victimcontent(i)
                                if(lpn /= 0) then
                                    WFIindex=WFIindex+1
                                    FTL(lpn,:)=(/WFIindex, WFI/)
                                    SSD(WFIindex,WFI)=lpn
                                end if
                            end do
                            hotValidPages(WFI)=hotValidPages(WFI)+hotj
                            validPages(WFI)=validPages(WFI)+j
                            WFE=victim
                            WFEindex=0
                            failure=.false.
                        else ! Copy what can be copied to WFI, j > k
                            kdiff=k
                            hotk=0
                            if(kdiff == 0) then !Turns 0 for first time
                                WFIindex = 0 ! WFIindex remains between GCA invocations, only reset here
                            end if

                            do i=1,b
                                lpn=victimcontent(i)
                                if(lpn /= 0) then
                                    WFIindex=WFIindex+1
                                    if (kdiff == 0) then ! No space on WFI, copy back to self
                                        SSD(WFIindex,victim)=lpn
                                        FTL(lpn,:)=(/WFIindex,victim/)
                                    else ! (kdiff > 0) !Copy to WFI
                                        SSD(WFIindex,WFI)=lpn
                                        FTL(lpn,:)=(/WFIindex, WFI/)

                                        if(lpn <= maxnumhot) then
                                            hotk=hotk+1
                                        end if

                                        kdiff=kdiff-1
                                        if(kdiff == 0) then !Turns 0 for first time
                                            WFIindex = 0 ! WFIindex remains between GCA invocations, only reset here
                                        end if
                                    end if
                                end if !(lpn /= 0)
                            end do !(i=1,b)
                            validPages(victim)=j-k !This is done local (during GC), so this always holds true (even if requests for pages in WFI and WFE)
                            validPages(WFI)=validPages(WFI)+k ! Old WFI filled completely during loop
                            hotValidPages(victim)=hotj-hotk
                            hotValidPages(WFI)=hotValidPages(WFI)+hotk

                            WFI = victim
                        end if !(j <=k)
                    end if !(victim /= WFE,WFI)

                end do !(failure)
            end do
        end if

        !! Reset metrics
        it=0
        currentPE=0
        intW=0
        extW=0
        PE=0
        sumPE=sum(PE)
        fairness=0.0_dp
        endurance=0.0_dp
        victimValids=0
        numgccalls =0
        victimhotness=0
        transientdist=0
        WFEhotness =0
        WFIhotness =0
        reqit=0

        !! Simulation
        do while(currentPE < maxPE )!.or. it <= maxit)
            do while(WFEindex < b)

                reqit=reqit+1
                reqit=mod(reqit-1,numreqlong)+1
                lpn=requests(mod(reqit, numreqlong)+1,1)
                isWrite=requests(mod(reqit, numreqlong)+1,2) /= 0
                p=FTL(lpn,1)
                bl=FTL(lpn,2)

                if(lpn <= maxnumhot) then
                    hotness=1
                else
                    hotness=0
                end if
                if(isWrite) then
                    WFEindex=WFEindex+1
                    if(p /= 0) then !Invalidate if valid page
                        !Remove old page
                        SSD(p,bl) = 0
                        validPages(bl)=validPages(bl)-1
                        hotValidPages(bl)=hotValidPages(bl)-hotness
                    end if
                    !Write update to WFE
                    FTL(lpn, 1:2) =(/ WFEindex, WFE/)
                    SSD(WFEindex,WFE)=lpn
                    validPages(WFE)=validPages(WFE)+1
                    hotValidPages(WFE)=hotValidPages(WFE)+hotness

                    intW=intW+1
                    extW=extW+1
                else if (p /= 0) then ! TRIM
                    SSD(p,bl) = 0
                    validPages(bl)=validPages(bl)-1
                    hotValidPages(bl)=hotValidPages(bl)-hotness
                    FTL(lpn, 1:2) =(/0, 0/)
                end if
            end do !(WFEvalid < b)
            !! GCA invocation
            gccalls=0
            failure=.true.

            do while(failure)
                validPages((/WFE,WFI/))=b+1
                victim=GC(rng,d,N,validPages)
                validPages((/WFE,WFI/))=count(SSD(:,(/WFE,WFI/)) > 0, 1)

                if(victim /= WFE .and. victim /= WFI) then
                    if(mod(it, testhotnessinterval) == 0) then
                        do distit=1,N
                            bi=hotValidPages(distit)
                            bj=validPages(distit)
                            temphotindex=bj*b+bi
                            transientdist(temphotindex)=transientdist(temphotindex)+1
                        end do
                    end if

                    if(PE(victim) == currentPE .and. currentPE < maxPE) then
                        currentPE=currentPE+1
                        fairness(currentPE)=sum(PE)/dble(N*currentPE)
                        endurance(currentPE)=dble(extW)/pageCount
                        rhoeff(currentPE)=dble(sum(validPages))/pageCount
                    end if

                    it=it+1

                    j=validPages(victim)
                    hotj=hotValidPages(victim)
                    victimValids(j)=victimValids(j)+1

                    bi=hotValidPages(victim)
                    bj=validPages(victim)
                    temphotindex=bj*b+bi
                    if(it < maxit) then
                        victimhotness(temphotindex)=victimhotness(temphotindex)+1
                    end if

                    jst=WFIindex ! Not equal to validPages(WFI), because incoming requests can invalidate in WFI
                    gccalls=gccalls+1

                    k=b-jst !Free in WFI
                    victimcontent=SSD(1:b,victim)

                    intW=intW+validPages(victim)
                    !! Erase victim
                    SSD(:,victim)=0
                    validPages(victim)=0
                    hotValidPages(victim)=0

                    if(j <=  k) then ! Sufficient space to copy over to WFI
                        !! Copy all content to WFI
                        WFIindex=jst
                        do i=1,b
                            lpn=victimcontent(i)
                            if(lpn /= 0) then
                                WFIindex=WFIindex+1
                                FTL(lpn,:)=(/WFIindex, WFI/)
                                SSD(WFIindex,WFI)=lpn
                            end if
                        end do
                        hotValidPages(WFI)=hotValidPages(WFI)+hotj
                        validPages(WFI)=validPages(WFI)+j

                        temphot=count(0 < SSD(1:b,WFE) .and. SSD(1:b,WFE) <= maxnumhot)
                        WFEhotness(temphot)=WFEhotness(temphot)+1
                        WFE=victim

                        WFEindex=0

                        failure=.false.
                    else ! Copy what can be copied to WFI, j > k
                        kdiff=k
                        hotk=0
                        if(kdiff == 0) then !Turns 0 for first time
                            WFIindex = 0 ! WFIindex remains between GCA invocations, only reset here

                        end if

                        do i=1,b
                            lpn=victimcontent(i)
                            if(lpn /= 0) then
                                WFIindex=WFIindex+1
                                if (kdiff == 0) then ! No space on WFI, copy back to self
                                    SSD(WFIindex,victim)=lpn
                                    FTL(lpn,:)=(/WFIindex,victim/)

                                else ! (kdiff > 0) !Copy to WFI
                                    SSD(WFIindex,WFI)=lpn
                                    FTL(lpn,:)=(/WFIindex, WFI/)

                                    if(lpn <=maxnumhot) then
                                        hotk=hotk+1
                                    end if

                                    kdiff=kdiff-1
                                    if(kdiff == 0) then !Turns 0 for first time
                                        WFIindex = 0 ! WFIindex remains between GCA invocations, only reset here
                                    end if
                                end if
                            end if !(lpn /= 0)
                        end do !(i=1,b)
                        validPages(victim)=j-k !This is done local (during GC), so this always holds true (even if requests for pages in WFI and WFE)
                        validPages(WFI)=validPages(WFI)+k ! Old WFI filled completely during loop
                        hotValidPages(victim)=hotj-hotk
                        hotValidPages(WFI)=hotValidPages(WFI)+hotk
                        temphot=count(0 < SSD(1:b,WFI) .and. SSD(1:b,WFI) <= maxnumhot)
                        WFIhotness(temphot)=WFIhotness(temphot)+1

                        WFI = victim
                    end if !(j <=k)

                    !! PE cycle on victim
                    PE(victim)=PE(victim)+1
                    sumPE=sumPE+1

                end if !(victim /= WFE,WFI)

            end do !(failure)

            numgccalls=numgccalls+gccalls

        end do !(currentPE < maxPE .or. it < maxit)
        WA=dble(intW)/dble(extW)

        if(present(initrandom) .and. initrandom) then
            write (WAfilename,           20)    b,d,rho,f,traceid,runit
            write (distfilename,         21)    b,d,rho,f,traceid,runit
            write (fairfilename,         22)    b,d,rho,f,traceid,runit
            write (endufilename,         23)    b,d,rho,f,traceid,runit
            write (victimfilename,       24)    b,d,rho,f,traceid,runit
            write (victimhotfilename,    25)    b,d,rho,f,traceid,runit
            write (WFEhfilename,         26)    b,d,rho,f,traceid,runit
            write (WFIhfilename,         27)    b,d,rho,f,traceid,runit
            write (transientdistfilename,28)    b,d,rho,f,traceid,runit
            write (validfilename,        29)    b,d,rho,f,traceid,runit
            write (rhoefffilename,       30)    b,d,rho,f,traceid,runit
        else
            write (WAfilename,           40)    b,d,rho,f,traceid,runit
            write (distfilename,         41)    b,d,rho,f,traceid,runit
            write (fairfilename,         42)    b,d,rho,f,traceid,runit
            write (endufilename,         43)    b,d,rho,f,traceid,runit
            write (victimfilename,       44)    b,d,rho,f,traceid,runit
            write (victimhotfilename,    45)    b,d,rho,f,traceid,runit
            write (WFEhfilename,         46)    b,d,rho,f,traceid,runit
            write (WFIhfilename,         47)    b,d,rho,f,traceid,runit
            write (transientdistfilename,48)    b,d,rho,f,traceid,runit
            write (validfilename,        49)    b,d,rho,f,traceid,runit
            write (rhoefffilename,       50)    b,d,rho,f,traceid,runit
        end if

        call PrintToFile (WAfilename,    (/WA/), 0, 0)
        call PrintToFile (distfilename,  dist, distL, distU)
        call PrintToFile (fairfilename,  fairness, 0, maxPE)
        call PrintToFile (endufilename,  endurance, 0, maxPE)
        call PrintInteger(victimfilename,  victimValids, 0, b)
        call PrintReal   (victimhotfilename,  victimhotness/dble(maxit), 0, b*(b+1))
        call PrintInteger(WFEhfilename,  WFEhotness, 0, b)
        call PrintInteger(WFIhfilename,  WFIhotness, 0, b)
        call PrintReal   (transientdistfilename, transientdist/(testhotnesscount+1.0_dp), 0,b*(b+1))
        call PrintReal   (validfilename, validdist, 0,b)
        call PrintToFile (rhoefffilename,  rhoeff, 0, maxPE)

        deallocate(FTL)
        deallocate(validPages)
        deallocate(PE)
        deallocate(SSD)
        deallocate(dist)

        20  format('dwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-WA.',I2,'.csv')
        21  format('dwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-dist.',I2,'.csv')
        22  format('dwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-fair.',I2,'.csv')
        23  format('dwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-end.',I2,'.csv')
        24  format('dwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-victim.',I2,'.csv')
        25  format('dwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-victimh.',I2,'.csv')
        26  format('dwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-WFEh.',I2,'.csv')
        27  format('dwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-WFIh.',I2,'.csv')
        28  format('dwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-trans.',I2,'.csv')
        29  format('dwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-valid.',I2,'.csv')
        30  format('dwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-rhoeff.',I2,'.csv')

        40  format('dwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-WA.',I2,'.csv')
        41  format('dwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-dist.',I2,'.csv')
        42  format('dwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-fair.',I2,'.csv')
        43  format('dwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-end.',I2,'.csv')
        44  format('dwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-victim.',I2,'.csv')
        45  format('dwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-victimh.',I2,'.csv')
        46  format('dwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-WFEh.',I2,'.csv')
        47  format('dwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-WFIh.',I2,'.csv')
        48  format('dwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-trans.',I2,'.csv')
        49  format('dwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-valid.',I2,'.csv')
        50  format('dwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-rhoeff.',I2,'.csv')
    end subroutine SSDDWFTrace

    subroutine SSDDWFTraceRuns(traceid,startrun,nruns,b,d,rho,f,maxPE,numreq, requests, initrandom)
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

            !!$OMP PARALLEL DO
            do it=1,nruns
                print *, it + startrun-1
                call rng_seed(rng(it), 932117 + it + startrun-1)
                call SSDDWFTrace(traceid, maxLBA, b,d,rho,f,maxPE,numreq,requests,it +startrun-1,rng(it), initrandom)
                print *, "done ", it + startrun-1
            end do
            !!$OMP END PARALLEL DO
    end subroutine SSDDWFTraceRuns
end module SimDWFLRUMod


program SimDWFLRUTrace

    use SimDWFLRUMod, only : dp
    use sim, only : SSDDWFTraceRuns
    implicit none

    integer, parameter :: H=2
    integer :: b,d,maxPE,nruns,numreq,it, startrun
    real(dp) :: rho,f
    integer, dimension(:,:), allocatable :: requests
    logical :: exists, initrandom
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
        call SSDDWFTraceRuns(traceid,startrun,nruns,b,d,rho,f,maxPE,numreq, requests, initrandom)
        deallocate(requests)
    else
        stop 'Input file does not exist.'
    end if

end program SimDWFLRUTrace
