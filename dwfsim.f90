module dwfsim
    use utils, only: dp,PrintInteger, PrintToFile, PrintReal, Print2ColReal
    use gca, only : GC
    implicit none

    private SSDDWFTrace
    public  SSDDWF, SSDDWFTraceRuns

    contains

    subroutine SSDDWF(N,b,d,rho,r,f,maxPE,runit, rng, initempty,initrandom)
        use rng, only: rng_t, randi, rng_uniform

        integer, parameter :: hotsections=1000, testhotnesscount = 1000
        type(rng_t), intent(inout) :: rng
        integer, intent(in):: N,b,d,maxPE,runit
        real(dp), intent(in) :: rho,r,f
        logical, intent(in), optional :: initempty,initrandom
        real(dp), allocatable, dimension(:) :: dist
        real(dp), dimension(0:maxPE) :: endurance, fairness
        real(dp), dimension(0:b) :: validdist
        integer(8), dimension(0:b*(b+1)) :: victimhotness, transientdist

        integer :: WFI,WFE,p,bl,it,hotness,i,j,jst,k,lpn,victim,maxnumvalid,kdiff,&
                    distL, distU,distit,maxit,temphot,temphotindex,intW, extW,&
                    gcwrites,gccalls,numgccalls,currentPE,sumPE,testhotnessinterval,bi,bj
        integer, dimension(1:b,1:N)::SSD
        integer, dimension(1:N):: validPages, PE,hotValidPages
        integer, dimension(1:b) :: victimcontent
        integer, dimension(1:2) :: maxnumhot
        integer, dimension(0:b) :: WFEhotness,WFIhotness,victimValids
        integer, allocatable, dimension(:,:):: FTL
        character(len=64) :: distfilename,endufilename, fairfilename,&
                        validfilename,WFEhfilename,WFIhfilename,victimfilename, &
                        victimhotfilename,transientdistfilename

        real(dp) :: pageCount, rannr, WA
        logical :: failure

        maxit=N*maxPE
        pageCount=dble(b*N)
        maxnumvalid=ceiling(rho*pageCount)
        testhotnessinterval=maxit/testhotnesscount

        !! Initialize
        allocate(FTL(1:maxnumvalid,1:2))

        SSD=0
        FTL=0
        WFI=1
        WFE=2
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
        end if
        maxnumhot(1)=floor(f*rho*pageCount)
        maxnumhot(2)=maxnumvalid-maxnumhot(1)
        validPages = count(SSD > 0, 1)
        hotValidPages = count(0 < SSD .and. SSD <= maxnumhot(1), 1)


        !! Simulation
        it=0
        currentPE=0
        intW=0
        extW=0
        PE=0
        sumPE=sum(PE)
        fairness=0.0_dp
        endurance=0.0_dp
        WFEhotness =0
        WFIhotness =0
        victimValids=0
        numgccalls =0
        victimhotness=0
        transientdist=0

        do while(currentPE < maxPE .or. it <= maxit)
            do while(validPages(WFE) < b)
                rannr=rng_uniform(rng)
                if(rannr < r) then
                    hotness=1
                else
                    hotness=2
                end if

                failure=.true.
                do while (failure)
                    if(hotness == 1) then
                        lpn=randi(rng,maxnumhot(1))
                    else
                        lpn=maxnumhot(1)+randi(rng,maxnumhot(2))
                    end if
                    !lpn=sum(maxnumhot(1:hotness-1))+randi(rng,maxnumhot(hotness))
                    p=FTL(lpn,1)
                    bl=FTL(lpn,2)

                    if(p /= 0) then !Choose valid page
                        if(bl /= WFE .and. bl /= WFI) then
                            !Remove old page
                            SSD(p,bl) = 0
                            validPages(bl)=validPages(bl)-1
                            !Write update to victim
                            validPages(WFE)=validPages(WFE)+1
                            FTL(lpn, 1:2) =(/validPages(WFE), WFE/)
                            SSD(validPages(WFE),WFE)=lpn
                            if(hotness == 1) then
                                hotValidPages(bl)=hotValidPages(bl)-1
                                hotValidPages(WFE)=hotValidPages(WFE)+1
                            end if
                            failure=.false.
                        end if
                    else ! Write to empty logical page
                        validPages(WFE)=validPages(WFE)+1
                        FTL(lpn, 1:2) =(/ validPages(WFE), WFE/)
                        SSD(validPages(WFE),WFE)=lpn
                        if(hotness == 1) then
                            hotValidPages(WFE)=hotValidPages(WFE)+1
                        end if
                        failure=.false.
                    end if
                end do !(failure)

                extW=extW+1
            end do !(WFEvalid < b)

            !! GCA invocation
            gccalls=0
            gcwrites=0
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
                    end if
                    it=it+1


                    if (it == maxit) then
                        !! Stats at end of run
                        distL=minval(PE)
                        distU=maxval(PE)
                        allocate(dist(distL:distU))
                        dist(distL)=count(PE==distL)
                        do distit=distL+1,distU
                            dist(distit)=dist(distit-1)+count(PE == distit)
                        end do
                        dist=dist/N

                        do distit=0,b
                            validdist(distit)=count(validPages == distit)
                        end do
                        validdist=validdist/N


                        do distit=1,N
                            bi=hotValidPages(distit)
                            bj=validPages(distit)
                            temphotindex=bj*b+bi
                            transientdist(temphotindex)=transientdist(temphotindex)+1
                        end do

                        it=maxit+1 !make sure this does not happen again
                    end if !(it == maxit)


                    j=validPages(victim)
                    victimValids(j)=victimValids(j)+1

                    bi=hotValidPages(victim)
                    bj=validPages(victim)
                    temphotindex=bj*b+bi
                    if(it < maxit) then
                        victimhotness(temphotindex)=victimhotness(temphotindex)+1
                    end if

                    jst=validPages(WFI)
                    gccalls=gccalls+1
                    gcwrites=gcwrites+b-j

                    k=b-jst !Free in WFI
                    victimcontent=SSD(1:b,victim)


                    if(j <=  k) then ! Sufficient space to copy over to WFI
                        !! Copy all content to WFI
                        do i=1,b
                            lpn=victimcontent(i)
                            if(lpn /= 0) then
                                validPages(WFI)=validPages(WFI)+1
                                FTL(lpn,:)=(/validPages(WFI), WFI/)
                                SSD(validPages(WFI),WFI)=lpn
                            end if
                        end do
                        !! Erase victim
                        SSD(:,victim)=0
                        validPages(victim)=0
                        hotValidPages(victim)=0
                        failure=.false.
                    else ! Copy what can be copied to WFI, j > k
                        kdiff=k
                        !Erase victim
                        SSD(1:b,victim)=0
                        validPages(victim)=0!Done to copy stuff, maybe back to self
                        do i=1,b
                            lpn=victimcontent(i)
                            if(lpn /= 0) then
                                if (kdiff == 0) then ! No space on WFI, copy back to self
                                    validPages(victim)=validPages(victim)+1
                                    FTL(lpn,:)=(/validPages(victim),victim/)
                                    SSD(validPages(victim),victim)=lpn
                                else ! (kdiff > 0) !Copy to WFI
                                    validPages(WFI)=validPages(WFI)+1
                                    FTL(lpn,:)=(/validPages(WFI), WFI/)
                                    SSD(validPages(WFI),WFI)=lpn
                                    kdiff=kdiff-1
                                end if
                            end if !(lpn /= 0)
                        end do !(i=1,b)

                        temphot=count(0 < SSD(1:b,WFI) .and. SSD(1:b,WFI) <= maxnumhot(1))
                        WFIhotness(temphot)=WFIhotness(temphot)+1
                        hotValidPages(WFI)=temphot

                        !! Replace WFI with victim
                        WFI=victim
                        validPages(WFI)=j-k
                        hotValidPages(WFI)=count(0 < SSD(1:b,WFI) .and. SSD(1:b,WFI) <= maxnumhot(1))
                    end if !(j <=k)

                    !! PE cycle on victim
                    PE(victim)=PE(victim)+1
                    sumPE=sumPE+1

                end if !(victim /= WFE,WFI)
            end do !(failure)
            temphot=count(0 < SSD(1:b,WFE) .and. SSD(1:b,WFE) <= maxnumhot(1))
            WFEhotness(temphot)=WFEhotness(temphot)+1
            hotValidPages(WFE)=temphot
            WFE=victim
            hotValidPages(WFE)=count(0 < SSD(1:b,WFE) .and. SSD(1:b,WFE) <= maxnumhot(1))

            intW=intW+gcwrites!(b-validPages(WFE))
            numgccalls=numgccalls+gccalls


        end do !(currentPE < maxPE .or. it < maxit)
        WA=dble(intW)/numgccalls
        WA=dble(b)/WA

        write (distfilename, 21)         b,d,rho,r,f,runit
        write (fairfilename, 23)         b,d,rho,r,f,runit
        write (endufilename, 24)         b,d,rho,r,f,runit
        write (victimfilename, 25)       b,d,rho,r,f,runit
        write (victimhotfilename, 26)    b,d,rho,r,f,runit
        write (WFEhfilename, 27)         b,d,rho,r,f,runit
        write (WFIhfilename, 29)         b,d,rho,r,f,runit
        write (transientdistfilename,30) b,d,rho,r,f,runit
        write (validfilename,31)         b,d,rho,r,f,runit

        call PrintToFile (distfilename,  dist, distL, distU)
        call PrintToFile (fairfilename,  fairness, 0, maxPE)
        call PrintToFile (endufilename,  endurance, 0, maxPE)
        call PrintInteger(victimfilename,  victimValids, 0, b)
        call PrintReal   (victimhotfilename,  victimhotness/dble(maxit), 0, b*(b+1))
        call PrintInteger(WFEhfilename,  WFEhotness, 0, b)
        call PrintInteger(WFIhfilename,  WFIhotness, 0, b)
        call PrintReal   (transientdistfilename, transientdist/(testhotnesscount+1.0_dp), 0,b*(b+1))
        call PrintReal   (validfilename, validdist, 0,b)

        21  format('dwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-dist.',I2,'.csv')
        23  format('dwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-fair.',I2,'.csv')
        24  format('dwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-end.',I2,'.csv')
        25  format('dwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-victim.',I2,'.csv')
        26  format('dwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-victimh.',I2,'.csv')
        27  format('dwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-WFEh.',I2,'.csv')
        29  format('dwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-WFIh.',I2,'.csv')
        30  format('dwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-trans.',I2,'.csv')
        31  format('dwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-valid.',I2,'.csv')
    end subroutine SSDDWF

    !subroutine SSDDWFRuns(nruns,startrun,N,b,d,rho,r,f,maxPE, initrandom)
    !        use utils, only : dp
    !        use rng, only : rng_seed, rng_t
    !
    !        integer, intent(in) :: nruns,b,N,d,maxPE,startrun
    !        real(dp), intent(in) :: rho,r,f
    !        logical, intent(in) :: initrandom
    !
    !        integer :: it
    !        type(rng_t), dimension(1:nruns) :: rng
    !
    !        do it=1,nruns
    !            print *, it + startrun-1
    !            call rng_seed(rng(it), 932117 + it + startrun -1)
    !            call SSDDWF(N,b,d,rho,r,f,maxPE,it+startrun-1, rng(it), initrandom)
    !            print *, "done ", it + startrun -1
    !        end do
    !end subroutine SSDDWFRuns



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


end module dwfsim
