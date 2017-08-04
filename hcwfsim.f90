module hcwfsim
    use utils, only: dp,PrintInteger, PrintToFile, PrintReal, Print2ColReal
    use hcwfutils, only : initHCWFrandom, processGCHCWF, initHCWFTracerandom, processGCHCWFTrace
    use gca, only : GC, GCCOLD
    
    implicit none

    !private processGCHCWF,initHCWFrandom,processGCHCWFTrace,initHCWFTracerandom
    public SSDHCWF, SSDHCWFTrace, SSDHCWFCOLD, SSDCOLDDCHTrace, SSDHCORACLETrace

    contains


    !! !!!!!!!!!!!!!! !
    !! Synthetic data !
    !! !!!!!!!!!!!!!! !
    !subroutine initHCWFrandom(N,b,SSD,FTL,maxnumvalid,maxhotbl, maxnumhot,&
    !    WF, hotBlock,coldBlock, rng)
    !
    !    use rng, only: rng_t, randi
    !
    !    integer, intent(in) :: b,N,maxnumvalid,maxhotbl,hotBlock,coldBlock
    !    integer, intent(in), dimension(0:1) :: WF, maxnumhot
    !    integer, intent(inout), dimension(1:b,1:N) :: SSD
    !    integer, intent(inout), dimension(1:maxnumvalid,1:2) :: FTL
    !    type(rng_t), intent(inout) :: rng
    !    
    !    integer :: it, p, bl
    !    logical :: failure
    !    
    !    do it=1, maxnumvalid
    !        failure=.true.
    !        do while(failure)
    !            p=randi(rng,b)
    !            if(it < maxnumhot(hotBlock)) then
    !                bl=randi(rng,maxhotbl)
    !            else
    !                bl=maxhotbl+randi(rng,N-maxhotbl)
    !            end if ! (it < maxnumhot(1))
    !            if(SSD(p,bl) <= 0 .and. all(WF /= bl)) then
    !                SSD(p,bl) = it
    !                FTL(it,1)=p
    !                FTL(it,2)=bl
    !                failure=.false.
    !            end if ! (SSD(p,bl) <= 0 .and. all(WF /= bl))
    !        end do ! (failure)
    !    end do ! (it=1,maxnumvalid)
    !end subroutine initHCWFrandom
    !
    !function processGCHCWF(N,b,SSD,FTL,maxnumvalid,validPages,PE,&
    !    blockhotness,victim,j,k,WF,same,other) result(failure)
    !
    !    integer, intent(in) :: b,N, victim, j,k, same, other,maxnumvalid
    !    integer, intent(inout), dimension(1:b,1:N) :: SSD
    !    integer, intent(inout), dimension(1:maxnumvalid,1:2) :: FTL
    !    integer, intent(inout), dimension(0:1) :: WF
    !    integer, intent(inout), dimension(1:N) :: PE, blockhotness, validPages
    !
    !    logical :: failure
    !
    !    integer :: i,tmpvictimfill
    !    integer, dimension(1:b) :: valpvict
    !
    !
    !    failure=.true.!Assume failure to choose block with same hotness
    !    
    !    valpvict=SSD(:,victim)
    !
    !    if(blockhotness(victim) == same) then ! Hot victim
    !        validPages(victim)= 0
    !        do i=1,b
    !            if(valpvict(i)>0) then
    !                validPages(victim)=validPages(victim)+1
    !                SSD(validPages(victim),victim)=valpvict(i)
    !                FTL(valpvict(i),:)=(/validPages(victim),victim/)
    !            end if
    !        end do
    !        SSD(validPages(victim)+1:b,victim)=0
    !
    !        WF(same)=victim
    !        failure=.false.
    !
    !    elseif(j <= k) then ! Cold victim, sufficient space in CWF
    !        ! Copy to CWF
    !        do i=1,b
    !            if(valpvict(i)>0) then
    !                validPages(WF(other))=validPages(WF(other))+1
    !                SSD(validPages(WF(other)),WF(other))=valpvict(i)
    !                FTL(valpvict(i),:)=(/ validPages(WF(other)),WF(other) /)
    !            end if
    !        end do
    !        ! HWF <- victim
    !        WF(same)=victim
    !        validPages(WF(same))=0
    !        SSD(:,WF(same))=0
    !        blockhotness(WF(same))=same
    !        failure=.false.
    !
    !    else ! j > k
    !        ! Copy k of j to CWF, rest to self
    !        tmpvictimfill=0
    !        SSD(:,victim)=0
    !        do i=1,b
    !            if(valpvict(i)>0) then
    !                validPages(WF(other))=validPages(WF(other))+1
    !                if(validPages(WF(other)) <= b)then
    !                    !Copy to old CWF
    !                    SSD(validPages(WF(other)),WF(other))=valpvict(i)
    !                    FTL(valpvict(i),:)=(/validPages(WF(other)),WF(other)/)
    !                else
    !                    ! Copy to self
    !                    tmpvictimfill=tmpvictimfill+1
    !                    SSD(tmpvictimfill,victim)=valpvict(i)
    !                    FTL(valpvict(i),:)=(/tmpvictimfill,victim/)
    !                end if ! (validPages(WF(other)))
    !            end if ! (valpvict(i)>0)
    !        end do ! (i=1,b)
    !        !! CWF <- victim
    !        validPages(WF(other))=b ! Possibly went over b for validPages, so repair
    !        WF(other)=victim
    !        validPages(WF(other))=j-k
    !        SSD((j-k+1):b,WF(other))=0
    !    end if ! (blockhotness(victim) == same)
    !
    !end function processGCHCWF
    !
    !
    !
    !
    !
    !
    !! !!!!!!!!!!!!!!! !
    !!   Trace-based   !
    !! !!!!!!!!!!!!!!! !
    !subroutine initHCWFTracerandom(N,b,SSD,FTL,maxnumvalid,maxhotbl, maxnumhot,&
    !    WF, WFindex, hotBlock,coldBlock, rng)
    !
    !    use rng, only: rng_t, randi
    !
    !    integer, intent(in) :: b,N,maxnumvalid,maxhotbl,hotBlock,coldBlock,maxnumhot
    !    integer, intent(in), dimension(0:1) :: WF
    !    integer, intent(inout), dimension(0:1) :: WFindex
    !    integer, intent(inout), dimension(1:b,1:N) :: SSD
    !    integer, intent(inout), dimension(1:maxnumvalid,1:2) :: FTL
    !    type(rng_t), intent(inout) :: rng
    !    
    !    integer :: it, p, bl, hotness
    !    logical :: failure, isHot
    !
    !    do it=1, maxnumvalid
    !        failure=.true.
    !        do while(failure)
    !            p=randi(rng,b)
    !            isHot= (it <= maxnumhot)
    !            if(it <= maxnumhot) then
    !                bl=randi(rng,maxhotbl)
    !            else
    !                bl=maxhotbl+randi(rng,N-maxhotbl)
    !            end if
    !            if(SSD(p,bl) <= 0) then
    !                if(all(WF /= bl) ) then
    !                    SSD(p,bl) = it
    !                    FTL(it,:)=(/p,bl/)
    !                    failure=.false.
    !                else
    !                    if(it <= maxnumhot) then
    !                        hotness=hotBlock
    !                    else
    !                        hotness=coldBlock
    !                    end if
    !                    if(WFindex(hotness) < b) then
    !                        WFindex(hotness)=WFindex(hotness)+1
    !                        FTL(it, :) =(/ WFindex(hotness), WF(hotness)/)
    !                        SSD(WFindex(hotness),WF(hotness))=it
    !                        failure=.false.
    !                    end if
    !                end if
    !            end if
    !        end do
    !    end do
    !
    !end subroutine initHCWFTracerandom
    !
    !function processGCHCWFTrace(N,b,SSD,FTL,maxnumvalid,validPages,PE,&
    !    blockhotness,victim,WF,WFindex,same,other) result(failure)
    !
    !    integer, intent(in) :: b,N, victim, same, other,maxnumvalid
    !    integer, intent(inout), dimension(1:b,1:N) :: SSD
    !    integer, intent(inout), dimension(1:maxnumvalid,1:2) :: FTL
    !    integer, intent(inout), dimension(0:1) :: WF, WFindex
    !    integer, intent(inout), dimension(1:N) :: PE,blockhotness, validPages
    !
    !    logical :: failure
    !
    !    integer :: i,j,jst,k,kdiff,tmpvictimfill,lpn
    !    integer, dimension(1:b) :: victimcontent, valpvict
    !    
    !    j=validPages(victim)
    !    !victimValids(j)=victimValids(j)+1
    !
    !    jst=WFindex(other) ! Not equal to validPages(WF(other)), because incoming requests can invalidate in WFI
    !
    !    k=b-jst !Free in WFI
    !    victimcontent=SSD(1:b,victim)
    !
    !    !! Erase victim
    !    SSD(:,victim)=0
    !    validPages(victim)=0
    !
    !    if(blockhotness(victim) == same) then
    !        WF(same)=victim
    !        WFindex(same)=0
    !        do i=1,b
    !            lpn=victimcontent(i)
    !            if(lpn /= 0) then
    !                WFindex(same)=WFindex(same)+1
    !                FTL(lpn,:)=(/WFindex(same), WF(same)/)
    !                SSD(WFindex(same),WF(same))=lpn
    !                !intW=intW+1
    !            end if ! (lpn /= 0)
    !        end do ! (i=1,b)
    !
    !        validPages(WF(same))=j
    !
    !        failure=.false.
    !
    !    elseif(j <= k) then ! Cold victim, sufficient space in CWF
    !
    !        !Copy to CWF
    !        do i=1,b
    !            lpn=victimcontent(i)
    !            if(lpn /= 0) then
    !                WFindex(other)=WFindex(other)+1
    !                FTL(lpn,:)=(/WFindex(other), WF(other)/)
    !                SSD(WFindex(other),WF(other))=lpn
    !                !intW=intW+1
    !            end if
    !        end do
    !        validPages(WF(other))=validPages(WF(other))+j
    !        !No modifications for (hot)validPages necessary here
    !        validPages(victim)=0
    !
    !        ! HWF <- victim
    !        WF(same) = victim
    !        blockhotness(WF(same))=same
    !        WFindex(same)=0
    !
    !        failure=.false.
    !
    !    else ! j > k
    !
    !        ! Copy k of j to CWF, rest to self
    !        kdiff=k
    !        if(kdiff == 0) then !Turns 0 for first time
    !            WFindex(other) = 0 ! WFIindex remains between GCA invocations, only reset here
    !        end if ! (kdiff == 0)
    !
    !        do i=1,b
    !            lpn=victimcontent(i)
    !            if(lpn /= 0) then
    !                WFindex(other) = WFindex(other)+1
    !                if (kdiff == 0) then ! No space on WFI, copy back to self
    !                    SSD(WFindex(other),victim)=lpn
    !                    FTL(lpn,:)=(/WFindex(other),victim/)
    !                else ! (kdiff > 0) !Copy to WFI
    !                    SSD(WFindex(other),WF(other))=lpn
    !                    FTL(lpn,:)=(/WFindex(other), WF(other)/)
    !
    !                    kdiff=kdiff-1
    !                    if(kdiff == 0) then !Turns 0 for first time
    !                        WFindex(other) = 0 ! WFIindex remains between GCA invocations, only reset here
    !                    end if ! (kdiff == 0)
    !                end if ! (kdiff == 0)
    !                !intW=intW+1
    !            end if ! (lpn /= 0)
    !        end do ! (i=1,b)
    !        validPages(victim)=j-k ! This is done local (during GC), so this always holds true (even if requests for pages in WFI and WFE)
    !        validPages(WF(other))=validPages(WF(other))+k ! Old WF(other) filled completely during loop
    !
    !        WF(other) = victim
    !        blockhotness(WF(other))=other
    !    end if ! (blockhotness(victim) == same)
    !end function processGCHCWFTrace



    subroutine SSDHCWFTrace(traceid,maxLBA, b,d,rho,f,maxPE,numreq,requests,runit,rng, initrandom, initempty)
        use rng, only: rng_t, randi, rng_uniform

        integer, parameter :: hotsections=1000, testhotnesscount = 1000
        integer, parameter :: numhotgcbins=12
        integer, parameter :: hotBlock=1, coldBlock=0
        type(rng_t), intent(inout) :: rng
        character(4) :: traceid
        integer, intent(in):: maxLBA,b,d,maxPE,runit, numreq
        integer, intent(in), dimension(1:numreq,1:2) ::requests
        real(dp), intent(in) :: rho,f
        logical, intent(in), optional :: initrandom, initempty

        real(dp), allocatable, dimension(:) :: dist
        real(dp), dimension(0:maxPE) :: endurance, fairness, hotblf,coldblf,hotpgf,coldpgf
        real(dp), dimension(0:b) :: validdist
        integer(8), dimension(0:b*(b+1)) :: victimhotness, transientdist

        integer :: N,p,bl,it,hotness,i,j,jst,k,lpn,victim,maxnumvalid,kdiff,maxnumhot,&
                    distL, distU,distit,maxit,temphot,temphotindex,&
                    gccalls,numgccalls,currentPE,sumPE,testhotnessinterval,&
                    bi,bj,hotj,hotk,maxhotbl,same, other, binID,numhotgccalls,interhotvictim
        integer(8) :: intW, extW, numreqlong, reqit
        integer, allocatable, dimension(:):: PE,validPages, blockhotness
        integer, dimension(1:b) :: victimcontent
        integer, dimension(0:1) :: WF, WFindex
        integer, dimension(0:b) :: victimValids
        integer, dimension(0:numhotgcbins-1) :: interhotvictimlim,interhotvictimbins
        integer, allocatable, dimension(:,:):: FTL, SSD
        character(len=64) :: distfilename,endufilename, fairfilename,&
                        validfilename,WFEhfilename,WFIhfilename,victimfilename, &
                        victimhotfilename,transientdistfilename,WAfilename,&
                        hotvicfreqfilename, interhotvicfilename,coldpgffilename,&
                        hotblffilename, coldblffilename, hotpgffilename

        real(dp) :: pageCount, hotGCfreq, WA
        logical :: failure, metrics, isWrite, collectstats,warmupdone

        numreqlong=numreq

        N=ceiling(dble(maxLBA)/(b*rho))
        maxit=N*maxPE
        pageCount=dble(b*N)
        maxnumvalid=maxLBA
        testhotnessinterval=maxit/testhotnesscount

        !! Initialize
        allocate(FTL(1:maxnumvalid,1:2))
        allocate(PE(1:N))
        allocate(validPages(1:N))
        allocate(blockhotness(1:N))
        allocate(SSD(1:b,1:N))

        SSD=0
        FTL=0
        maxnumhot=floor(f*maxLBA)
        maxhotbl=ceiling(f*N)
        WF(coldBlock)=maxhotbl+1 !CWF
        WF(hotBlock)=1 !HWF
        WFindex=0
        victimValids=0
        validPages=0
        blockhotness(1:maxhotbl)=hotBlock
        blockhotness(maxhotbl+1:N)=coldBlock

        reqit=0
        if( present(initrandom) .and. initrandom) then ! Fill disk randomly
            call initHCWFTracerandom(N,b,SSD,FTL,maxnumvalid,maxhotbl,maxnumhot,WF,WFindex,hotBlock,coldBlock,rng)
            validPages = count(SSD > 0, 1)
            collectstats=.true.
            warmupdone=.true.
        else if( present(initempty) .and. initempty) then ! Start from empty disk, but collect stats
            collectstats=.true.
            warmupdone=.true.
        else ! Warmup with trace
            collectstats=.false.
            warmupdone=.false.
        end if

        !Reset metrics
        it=0
        currentPE=0
        intW=0
        extW=0
        PE=0
        sumPE=sum(PE)
        fairness=0.0_dp
        endurance=0.0_dp
        numgccalls =0
        victimhotness=0
        transientdist=0
        reqit=0
        hotpgf(currentPE)=count(FTL(1:maxnumhot,1) /= 0)
        hotblf(currentPE)=count(blockhotness == hotBlock)
        coldpgf(currentPE)=count(FTL(maxnumhot+1:maxnumvalid,1) /= 0)
        coldblf(currentPE)=N-hotblf(currentPE)

        numhotgccalls=0
        interhotvictim=0! Time between choosing Hot victim blocks
        !Upper limits of items in bin
        do distit=0,numhotgcbins-2
            interhotvictimlim(distit)=distit!5*distit
        end do ! (distit=0,numhotgcbins-2)
        interhotvictimlim(numhotgcbins-1)=0 !Actually infinity
        interhotvictimbins=0

        !! Simulation
        do while(currentPE < maxPE .or. it <= maxit)

            collectstats=collectstats .or. warmupdone
            do while(all(WFindex < b))
                reqit=reqit+1
                reqit=mod(reqit-1,numreqlong)+1

                lpn=requests(mod(reqit, numreqlong)+1,1)
                isWrite=requests(mod(reqit, numreqlong)+1,2) /= 0
                p=FTL(lpn,1)
                bl=FTL(lpn,2)

                if(lpn <= maxnumhot) then
                    hotness=hotBlock
                else
                    hotness=coldBlock
                end if ! (lpn <= maxnumhot)

                if (isWrite) then
                    WFindex(hotness)=WFindex(hotness)+1
                    if (p /= 0) then ! Invalidate if valid page
                        ! Remove old page
                        SSD(p,bl) = 0
                        validPages(bl)=validPages(bl)-1
                    end if
                    ! Write update to WFE
                    FTL(lpn, 1:2) =(/ WFindex(hotness), WF(hotness)/)
                    SSD(WFindex(hotness),WF(hotness))=lpn
                    validPages(WF(hotness))=validPages(WF(hotness))+1

                    if(collectstats) then
                        intW=intW+1
                        extW=extW+1
                    end if ! (collectstats)
                    
                else if (p /= 0) then ! TRIM
                    SSD(p,bl) = 0
                    validPages(bl)=validPages(bl)-1
                    FTL(lpn, 1:2) =(/0, 0/)
                end if ! (isWrite)
            end do !(WFvalid < b)

            warmupdone=.not. collectstats .and. (sum(validPages) == maxnumvalid)

            !! GCA invocation
            gccalls=0
            failure=.true.

            do while(failure)
                if(WFindex(0) == b) then
                    same=0
                    other=1
                else
                    same=1
                    other=0
                end if ! (WFindex(0) == b)

                validPages(WF)=b+1
                victim=GC(rng,d,N,validPages)
                validPages(WF)=count(SSD(:,WF) > 0, 1)

                if(all(victim /= WF)) then
                    if (collectstats) then
                        if(PE(victim) == currentPE .and. currentPE < maxPE) then
                            currentPE=currentPE+1
    
                            fairness(currentPE)=sum(PE)/dble(N*currentPE)
                            endurance(currentPE)=dble(extW)/pageCount
                            hotpgf(currentPE)=count(FTL(1:maxnumhot,1) /= 0)
                            hotblf(currentPE)=count(blockhotness == 1)
                            coldpgf(currentPE)=count(FTL(maxnumhot+1:maxnumvalid,1) /= 0)
                            coldblf(currentPE)=N-hotblf(currentPE)
                        end if ! (PE(victim) == currentPE .and. currentPE < maxPE)

                        it=it+1

                        if(it == maxit) then
                            !! Stats at end of run
                            distL=minval(PE)
                            distU=maxval(PE)
                            if(allocated(dist)) then
                                deallocate(dist)
                            end if
                            allocate(dist(distL:distU))
                            dist(distL)=count(PE==distL)
                            do distit=distL+1,distU
                                dist(distit)=dist(distit-1)+count(PE == distit)
                            end do ! (distit=distL+1,distU)
                            dist=dist/N
    
                            do distit=0,b
                                validdist(distit)=count(validPages == distit)
                            end do ! (distit=0.b)
                            validdist=validdist/N
    
                            it=maxit+1 !make sure this does not happen again
                        end if !(it == maxit)

                        if(blockhotness(victim) == hotBlock) then
                            numhotgccalls = numhotgccalls +1
                            !Determine correct bin
                            binID=numhotgcbins-1
                            do distit=0,numhotgcbins-2
                                if(interhotvictim <= interhotvictimlim(distit)) then
                                    binID=distit
                                    exit ! Exit out of loop
                                end if ! (interhotvictim <= interhotvictimlim(distit))
                            end do ! (distit=0,numhotgcbins-2)
                            interhotvictimbins(binID) = interhotvictimbins(binID)+1
                            !Reset timer
                            interhotvictim=0
                        else ! (blockhotness(victim) == coldBlock)
                            !Not yet chosen hot victim, so increase "timer"
                            interhotvictim=interhotvictim+1
                        end if ! (blockhotness(victim) == hotBlock)

                        j=validPages(victim)
                        victimValids(j)=victimValids(j)+1
    
                        gccalls = gccalls+1
                        intW = intW + count(SSD(:,victim) /= 0)

                        !! PE cycle on victim
                        PE(victim)=PE(victim)+1
                        sumPE=sumPE+1
                    end if ! (collectstats)

                    failure = processGCHCWFTrace(N,b,SSD,FTL,maxnumvalid,validPages,PE,&
                                            blockhotness,victim,WF,WFindex,same,other)
                end if ! (victim /= WFE,WFI)                
            end do ! (failure)

            numgccalls=numgccalls+gccalls

        end do !(currentPE < maxPE .or. it < maxit)
        WA=dble(intW)/dble(extW)

        hotGCfreq=dble(numhotgccalls)/numgccalls


        if(present(initrandom) .and. initrandom) then
            write (WAfilename,        20)    b,d,rho,f,traceid,runit
            write (distfilename,      21)    b,d,rho,f,traceid,runit
            write (fairfilename,      22)    b,d,rho,f,traceid,runit
            write (endufilename,      23)    b,d,rho,f,traceid,runit
            write (victimfilename,    24)    b,d,rho,f,traceid,runit
            write (victimhotfilename, 25)    b,d,rho,f,traceid,runit
            write (hotvicfreqfilename, 26)   b,d,rho,f,traceid,runit
            write (interhotvicfilename,27)   b,d,rho,f,traceid,runit
            write (validfilename,      28)   b,d,rho,f,traceid,runit
            write (hotblffilename,     29)   b,d,rho,f,traceid,runit
            write (coldblffilename,    30)   b,d,rho,f,traceid,runit
            write (hotpgffilename,     31)   b,d,rho,f,traceid,runit
            write (coldpgffilename,    32)   b,d,rho,f,traceid,runit
        else
            write (WAfilename,         40)   b,d,rho,f,traceid,runit
            write (distfilename,       41)   b,d,rho,f,traceid,runit
            write (fairfilename,       42)   b,d,rho,f,traceid,runit
            write (endufilename,       43)   b,d,rho,f,traceid,runit
            write (victimfilename,     44)   b,d,rho,f,traceid,runit
            write (victimhotfilename,  45)   b,d,rho,f,traceid,runit
            write (hotvicfreqfilename, 46)   b,d,rho,f,traceid,runit
            write (interhotvicfilename,47)   b,d,rho,f,traceid,runit
            write (validfilename,      48)   b,d,rho,f,traceid,runit
            write (hotblffilename,     49)   b,d,rho,f,traceid,runit
            write (coldblffilename,    50)   b,d,rho,f,traceid,runit
            write (hotpgffilename,     51)   b,d,rho,f,traceid,runit
            write (coldpgffilename,    52)   b,d,rho,f,traceid,runit
        end if ! (present(initrandom) .and. initrandom)

        call PrintToFile (WAfilename,    (/WA/), 1, 1)
        call PrintToFile (distfilename,  dist, distL, distU)
        call PrintToFile (fairfilename,  fairness, 0, maxPE)
        call PrintToFile (endufilename,  endurance, 0, maxPE)
        call PrintInteger(victimfilename,  victimValids, 0, b)
        call PrintReal   (victimhotfilename,  victimhotness/dble(maxit), 0, b*(b+1))
        call PrintToFile (hotvicfreqfilename,  (/hotGCfreq/),    1, 1)
        call PrintInteger(interhotvicfilename,  interhotvictimbins, 0, numhotgcbins-1)
        call PrintReal   (validfilename, validdist, 0,b)
        call PrintToFile (hotblffilename,  hotblf,    0, maxPE)
        call PrintToFile (coldblffilename, coldblf, 0, maxPE)
        call PrintToFile (hotpgffilename, hotpgf, 0, maxPE)
        call PrintToFile (coldpgffilename, coldpgf, 0, maxPE)

        deallocate(FTL)
        deallocate(validPages)
        deallocate(PE)
        deallocate(SSD)
        deallocate(dist)

        20  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-WA.',I2,'.csv')
        21  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-dist.',I2,'.csv')
        22  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-fair.',I2,'.csv')
        23  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-end.',I2,'.csv')
        24  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-victim.',I2,'.csv')
        25  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-victimh.',I2,'.csv')
        26  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-hotGCf.',I2,'.csv')
        27  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-inter.',I2,'.csv')
        28  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-valid.',I2,'.csv')
        29  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-hotblf.',I2,'.csv')
        30  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-coldblf.',I2,'.csv')
        31  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-hotpgf.',I2,'.csv')
        32  format('hcwftrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-coldpgf.',I2,'.csv')

        40  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-WA.',I2,'.csv')
        41  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-dist.',I2,'.csv')
        42  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-fair.',I2,'.csv')
        43  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-end.',I2,'.csv')
        44  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-victim.',I2,'.csv')
        45  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-victimh.',I2,'.csv')
        46  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-hotGCf.',I2,'.csv')
        47  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-inter.',I2,'.csv')
        48  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-valid.',I2,'.csv')
        49  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-hotblf.',I2,'.csv')
        50  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-coldblf.',I2,'.csv')
        51  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-hotpgf.',I2,'.csv')
        52  format('hcwftrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-coldpgf.',I2,'.csv')

    end subroutine SSDHCWFTrace


    subroutine SSDHCORACLETrace(traceid,maxLBA, b,d,rho,f,nrFrames,maxPE,numreq,requests,isHotRequest,&
                                runit,rng, initrandom, initempty)
        use rng, only: rng_t, randi, rng_uniform

        integer, parameter :: hotsections=1000, testhotnesscount = 1000
        integer, parameter :: numhotgcbins=12
        integer, parameter :: hotBlock=1, coldBlock=0
        type(rng_t), intent(inout) :: rng
        character(4) :: traceid
        integer, intent(in):: maxLBA,b,d,maxPE,runit, numreq,nrFrames
        integer, intent(in), dimension(1:numreq,1:2) ::requests
        logical, intent(in), dimension(1:numreq) :: isHotRequest
        real(dp), intent(in) :: rho,f
        logical, intent(in), optional :: initrandom, initempty

        real(dp), allocatable, dimension(:) :: dist
        real(dp), dimension(0:maxPE) :: endurance, fairness, hotblf,coldblf,hotpgf,coldpgf
        real(dp), dimension(0:b) :: validdist
        integer(8), dimension(0:b*(b+1)) :: victimhotness, transientdist

        integer :: N,p,bl,it,hotness,i,j,jst,k,lpn,victim,maxnumvalid,kdiff,maxnumhot,&
                    distL, distU,distit,maxit,temphot,temphotindex,&
                    gccalls,numgccalls,currentPE,sumPE,testhotnessinterval,&
                    bi,bj,hotj,hotk,maxhotbl,same, other, binID,numhotgccalls,interhotvictim
        integer(8) :: intW, extW, numreqlong, reqit
        integer, allocatable, dimension(:):: PE,validPages, blockhotness
        integer, dimension(1:b) :: victimcontent
        integer, dimension(0:1) :: WF, WFindex
        integer, dimension(0:b) :: victimValids
        integer, dimension(0:numhotgcbins-1) :: interhotvictimlim,interhotvictimbins
        integer, allocatable, dimension(:,:):: FTL, SSD
        character(len=128) :: distfilename,endufilename, fairfilename,&
                        validfilename,WFEhfilename,WFIhfilename,victimfilename, &
                        victimhotfilename,transientdistfilename,WAfilename,&
                        hotvicfreqfilename, interhotvicfilename,coldpgffilename,&
                        hotblffilename, coldblffilename, hotpgffilename

        real(dp) :: pageCount, hotGCfreq, WA
        logical :: failure, metrics, isWrite, collectstats,warmupdone

        numreqlong=numreq

        N=ceiling(dble(maxLBA)/(b*rho))
        maxit=N*maxPE
        pageCount=dble(b*N)
        maxnumvalid=maxLBA
        testhotnessinterval=maxit/testhotnesscount

        !! Initialize
        allocate(FTL(1:maxnumvalid,1:2))
        allocate(PE(1:N))
        allocate(validPages(1:N))
        allocate(blockhotness(1:N))
        allocate(SSD(1:b,1:N))

        SSD=0
        FTL=0
        maxnumhot=floor(f*maxLBA)
        maxhotbl=ceiling(f*N)
        WF(coldBlock)=maxhotbl+1 !CWF
        WF(hotBlock)=1 !HWF
        WFindex=0
        victimValids=0
        validPages=0
        blockhotness(1:maxhotbl)=hotBlock
        blockhotness(maxhotbl+1:N)=coldBlock

        reqit=0
        if( present(initrandom) .and. initrandom) then ! Fill disk randomly
            call initHCWFTracerandom(N,b,SSD,FTL,maxnumvalid,maxhotbl,maxnumhot,WF,WFindex,hotBlock,coldBlock,rng)
            validPages = count(SSD > 0, 1)
            collectstats=.true.
            warmupdone=.true.
        else if( present(initempty) .and. initempty) then ! Start from empty disk, but collect stats
            collectstats=.true.
            warmupdone=.true.
        else ! Warmup with trace
            collectstats=.false.
            warmupdone=.false.
        end if

        !Reset metrics
        it=0
        currentPE=0
        intW=0
        extW=0
        PE=0
        sumPE=sum(PE)
        fairness=0.0_dp
        endurance=0.0_dp
        numgccalls =0
        victimhotness=0
        transientdist=0
        reqit=0
        hotpgf(currentPE)=count(FTL(1:maxnumhot,1) /= 0)
        hotblf(currentPE)=count(blockhotness == hotBlock)
        coldpgf(currentPE)=count(FTL(maxnumhot+1:maxnumvalid,1) /= 0)
        coldblf(currentPE)=N-hotblf(currentPE)

        numhotgccalls=0
        interhotvictim=0! Time between choosing Hot victim blocks
        !Upper limits of items in bin
        do distit=0,numhotgcbins-2
            interhotvictimlim(distit)=distit!5*distit
        end do ! (distit=0,numhotgcbins-2)
        interhotvictimlim(numhotgcbins-1)=0 !Actually infinity
        interhotvictimbins=0

        !! Simulation
        do while(currentPE < maxPE .or. it <= maxit)

            collectstats=collectstats .or. warmupdone
            
            do while(all(WFindex < b))
                reqit=reqit+1
                reqit=mod(reqit-1,numreqlong)+1

                lpn=requests(mod(reqit, numreqlong)+1,1)
                isWrite=requests(mod(reqit, numreqlong)+1,2) /= 0
                p=FTL(lpn,1)
                bl=FTL(lpn,2)

                if(lpn <= maxnumhot) then
                    hotness=hotBlock
                else
                    hotness=coldBlock
                end if ! (lpn <= maxnumhot)

                if (isWrite) then
                    WFindex(hotness)=WFindex(hotness)+1
                    if (p /= 0) then ! Invalidate if valid page
                        ! Remove old page
                        SSD(p,bl) = 0
                        validPages(bl)=validPages(bl)-1
                    end if
                    ! Write update to WFE
                    FTL(lpn, 1:2) =(/ WFindex(hotness), WF(hotness)/)
                    SSD(WFindex(hotness),WF(hotness))=lpn
                    validPages(WF(hotness))=validPages(WF(hotness))+1

                    if(collectstats) then
                        intW=intW+1
                        extW=extW+1
                    end if ! (collectstats)
                    
                else if (p /= 0) then ! TRIM
                    SSD(p,bl) = 0
                    validPages(bl)=validPages(bl)-1
                    FTL(lpn, 1:2) =(/0, 0/)
                end if ! (isWrite)
            end do !(WFvalid < b)

            warmupdone=.not. collectstats .and. (sum(validPages) == maxnumvalid)

            !! GCA invocation
            gccalls=0
            failure=.true.

            do while(failure)
                if(WFindex(0) == b) then
                    same=0
                    other=1
                else
                    same=1
                    other=0
                end if ! (WFindex(0) == b)

                validPages(WF)=b+1
                victim=GC(rng,d,N,validPages)
                validPages(WF)=count(SSD(:,WF) > 0, 1)

                if(all(victim /= WF)) then
                
                    if(collectstats .and. PE(victim) == currentPE .and. currentPE < maxPE) then
                        currentPE=currentPE+1

                        fairness(currentPE)=sum(PE)/dble(N*currentPE)
                        endurance(currentPE)=dble(extW)/pageCount
                        hotpgf(currentPE)=count(FTL(1:maxnumhot,1) /= 0)
                        hotblf(currentPE)=count(blockhotness == 1)
                        coldpgf(currentPE)=count(FTL(maxnumhot+1:maxnumvalid,1) /= 0)
                        coldblf(currentPE)=N-hotblf(currentPE)
                    end if ! (PE(victim) == currentPE .and. currentPE < maxPE)

                    it=it+1

                    if (collectstats) then
                        if(it == maxit) then
                            !! Stats at end of run
                            distL=minval(PE)
                            distU=maxval(PE)
                            if(allocated(dist)) then
                                deallocate(dist)
                            end if
                            allocate(dist(distL:distU))
                            dist(distL)=count(PE==distL)
                            do distit=distL+1,distU
                                dist(distit)=dist(distit-1)+count(PE == distit)
                            end do ! (distit=distL+1,distU)
                            dist=dist/N
    
                            do distit=0,b
                                validdist(distit)=count(validPages == distit)
                            end do ! (distit=0.b)
                            validdist=validdist/N
    
                            it=maxit+1 !make sure this does not happen again
                        end if !(it == maxit)

                        if(blockhotness(victim) == hotBlock) then
                            numhotgccalls = numhotgccalls +1
                            !Determine correct bin
                            binID=numhotgcbins-1
                            do distit=0,numhotgcbins-2
                                if(interhotvictim <= interhotvictimlim(distit)) then
                                    binID=distit
                                    exit ! Exit out of loop
                                end if ! (interhotvictim <= interhotvictimlim(distit))
                            end do ! (distit=0,numhotgcbins-2)
                            interhotvictimbins(binID) = interhotvictimbins(binID)+1
                            !Reset timer
                            interhotvictim=0
                        else ! (blockhotness(victim) == coldBlock)
                            !Not yet chosen hot victim, so increase "timer"
                            interhotvictim=interhotvictim+1
                        end if ! (blockhotness(victim) == hotBlock)

                        j=validPages(victim)
                        victimValids(j)=victimValids(j)+1
    
                        gccalls = gccalls+1
                        intW = intW + count(SSD(:,victim) /= 0)

                        !! PE cycle on victim
                        PE(victim)=PE(victim)+1
                        sumPE=sumPE+1
                    end if ! (collectstats)
                    failure = processGCHCWFTrace(N,b,SSD,FTL,maxnumvalid,validPages,PE,&
                                            blockhotness,victim,WF,WFindex,same,other)
                end if ! (victim /= WFE,WFI)
            end do ! (failure)

            numgccalls=numgccalls+gccalls

        end do !(currentPE < maxPE .or. it < maxit)
        WA=dble(intW)/dble(extW)

        hotGCfreq=dble(numhotgccalls)/numgccalls


        if(present(initrandom) .and. initrandom) then
            write (WAfilename,        20)    b,d,rho,f,nrFrames,traceid,runit
            write (distfilename,      21)    b,d,rho,f,nrFrames,traceid,runit
            write (fairfilename,      22)    b,d,rho,f,nrFrames,traceid,runit
            write (endufilename,      23)    b,d,rho,f,nrFrames,traceid,runit
            write (victimfilename,    24)    b,d,rho,f,nrFrames,traceid,runit
            write (victimhotfilename, 25)    b,d,rho,f,nrFrames,traceid,runit
            write (hotvicfreqfilename, 26)   b,d,rho,f,nrFrames,traceid,runit
            write (interhotvicfilename,27)   b,d,rho,f,nrFrames,traceid,runit
            write (validfilename,      28)   b,d,rho,f,nrFrames,traceid,runit
            write (hotblffilename,     29)   b,d,rho,f,nrFrames,traceid,runit
            write (coldblffilename,    30)   b,d,rho,f,nrFrames,traceid,runit
            write (hotpgffilename,     31)   b,d,rho,f,nrFrames,traceid,runit
            write (coldpgffilename,    32)   b,d,rho,f,nrFrames,traceid,runit
        else
            write (WAfilename,         40)   b,d,rho,f,nrFrames,traceid,runit
            write (distfilename,       41)   b,d,rho,f,nrFrames,traceid,runit
            write (fairfilename,       42)   b,d,rho,f,nrFrames,traceid,runit
            write (endufilename,       43)   b,d,rho,f,nrFrames,traceid,runit
            write (victimfilename,     44)   b,d,rho,f,nrFrames,traceid,runit
            write (victimhotfilename,  45)   b,d,rho,f,nrFrames,traceid,runit
            write (hotvicfreqfilename, 46)   b,d,rho,f,nrFrames,traceid,runit
            write (interhotvicfilename,47)   b,d,rho,f,nrFrames,traceid,runit
            write (validfilename,      48)   b,d,rho,f,nrFrames,traceid,runit
            write (hotblffilename,     49)   b,d,rho,f,nrFrames,traceid,runit
            write (coldblffilename,    50)   b,d,rho,f,nrFrames,traceid,runit
            write (hotpgffilename,     51)   b,d,rho,f,nrFrames,traceid,runit
            write (coldpgffilename,    52)   b,d,rho,f,nrFrames,traceid,runit
        end if ! (present(initrandom) .and. initrandom)

        call PrintToFile (WAfilename,    (/WA/), 1, 1)
        call PrintToFile (distfilename,  dist, distL, distU)
        call PrintToFile (fairfilename,  fairness, 0, maxPE)
        call PrintToFile (endufilename,  endurance, 0, maxPE)
        call PrintInteger(victimfilename,  victimValids, 0, b)
        call PrintReal   (victimhotfilename,  victimhotness/dble(maxit), 0, b*(b+1))
        call PrintToFile (hotvicfreqfilename,  (/hotGCfreq/),    1, 1)
        call PrintInteger(interhotvicfilename,  interhotvictimbins, 0, numhotgcbins-1)
        call PrintReal   (validfilename, validdist, 0,b)
        call PrintToFile (hotblffilename,  hotblf,    0, maxPE)
        call PrintToFile (coldblffilename, coldblf, 0, maxPE)
        call PrintToFile (hotpgffilename, hotpgf, 0, maxPE)
        call PrintToFile (coldpgffilename, coldpgf, 0, maxPE)

        deallocate(FTL)
        deallocate(validPages)
        deallocate(PE)
        deallocate(SSD)
        deallocate(dist)

        20  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-WA.',I2,'.csv')
        21  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-dist.',I2,'.csv')
        22  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-fair.',I2,'.csv')
        23  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-end.',I2,'.csv')
        24  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-victim.',I2,'.csv')
        25  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-victimh.',I2,'.csv')
        26  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-hotGCf.',I2,'.csv')
        27  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-inter.',I2,'.csv')
        28  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-valid.',I2,'.csv')
        29  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-hotblf.',I2,'.csv')
        30  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-coldblf.',I2,'.csv')
        31  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-hotpgf.',I2,'.csv')
        32  format('oracletrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-coldpgf.',I2,'.csv')

        40  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-WA.',I2,'.csv')
        41  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-dist.',I2,'.csv')
        42  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-fair.',I2,'.csv')
        43  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-end.',I2,'.csv')
        44  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-victim.',I2,'.csv')
        45  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-victimh.',I2,'.csv')
        46  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-hotGCf.',I2,'.csv')
        47  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-inter.',I2,'.csv')
        48  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-valid.',I2,'.csv')
        49  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-hotblf.',I2,'.csv')
        50  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-coldblf.',I2,'.csv')
        51  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-hotpgf.',I2,'.csv')
        52  format('oracletrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-fr',I6,'-',A4,'-coldpgf.',I2,'.csv')

    end subroutine SSDHCORACLETrace






    

    

    subroutine SSDCOLDDCHTrace(traceid,maxLBA, b,d,rho,f,maxPE,numreq,requests,runit,rng, initrandom)
        use rng, only: rng_t, randi, rng_uniform

        integer, parameter :: hotsections=1000, testhotnesscount = 1000
        integer, parameter :: numhotgcbins=12
        integer, parameter :: hotBlock=1, coldBlock=0
        type(rng_t), intent(inout) :: rng
        character(4) :: traceid
        integer, intent(in):: maxLBA,b,d,maxPE,runit, numreq
        integer, intent(in), dimension(1:numreq,1:2) ::requests
        real(dp), intent(in) :: rho,f
        logical, intent(in), optional :: initrandom

        real(dp), allocatable, dimension(:) :: dist
        real(dp), dimension(0:maxPE) :: endurance, fairness, hotblf,coldblf,hotpgf,coldpgf
        real(dp), dimension(0:b) :: validdist
        integer(8), dimension(0:b*(b+1)) :: victimhotness, transientdist

        integer :: N,p,bl,it,hotness,i,j,jst,k,lpn,victim,maxnumvalid,kdiff,maxnumhot,&
                    distL, distU,distit,maxit,temphot,temphotindex,&
                    gccalls,numgccalls,currentPE,sumPE,testhotnessinterval,&
                    bi,bj,hotj,hotk,maxhotbl,same, other, binID,numhotgccalls,interhotvictim
        integer(8) :: intW, extW, numreqlong, reqit
        integer, allocatable, dimension(:):: PE,validPages, blockhotness
        integer, dimension(1:b) :: victimcontent
        integer, dimension(0:1) :: WF, WFindex
        integer, dimension(0:b) :: victimValids
        integer, dimension(0:numhotgcbins-1) :: interhotvictimlim,interhotvictimbins
        integer, allocatable, dimension(:,:):: FTL, SSD
        character(len=64) :: distfilename,endufilename, fairfilename,&
                        validfilename,WFEhfilename,WFIhfilename,victimfilename, &
                        victimhotfilename,transientdistfilename,WAfilename,&
                        hotvicfreqfilename, interhotvicfilename,coldpgffilename,&
                        hotblffilename, coldblffilename, hotpgffilename

        real(dp) :: pageCount, hotGCfreq, WA
        logical :: failure, metrics, isWrite

        numreqlong=numreq

        N=ceiling(dble(maxLBA)/(b*rho))
        maxit=N*maxPE
        pageCount=dble(b*N)
        maxnumvalid=maxLBA
        testhotnessinterval=maxit/testhotnesscount

        !! Initialize
        allocate(FTL(1:maxnumvalid,1:2))
        allocate(PE(1:N))
        allocate(validPages(1:N))
        allocate(blockhotness(1:N))
        allocate(SSD(1:b,1:N))

        SSD=0
        FTL=0
        maxnumhot=floor(f*maxLBA)
        maxhotbl=ceiling(f*N)
        WF(0)=maxhotbl+1 !CWF
        WF(1)=1 !HWF
        WFindex=0
        victimValids=0
        validPages=0
        blockhotness(1:maxhotbl)=1
        blockhotness(maxhotbl+1:N)=0

        reqit=0
        !!TODO: Assume we initialize randomly; it doesn't really matter anyway
        !if( present(initrandom) .and. initrandom) then
            do it=1, maxnumvalid
                failure=.true.
                do while(failure)
                    p=randi(rng,b)
                    if(it <= maxnumhot) then
                        bl=randi(rng,maxhotbl)
                    else
                        bl=maxhotbl+randi(rng,N-maxhotbl)
                    end if
                    if(SSD(p,bl) <= 0) then
                        if(all(WF /= bl) ) then
                            SSD(p,bl) = it
                            FTL(it,:)=(/p,bl/)
                            failure=.false.
                        else
                            if(it <= maxnumhot) then
                                hotness=hotBlock
                            else
                                hotness=coldBlock
                            end if
                            if(WFindex(hotness) < b) then
                                WFindex(hotness)=WFindex(hotness)+1
                                FTL(it, :) =(/ WFindex(hotness), WF(hotness)/)
                                SSD(WFindex(hotness),WF(hotness))=it
                                failure=.false.
                            end if
                        end if
                    end if
                end do
            end do
            validPages = count(SSD > 0, 1)

        !Reset metrics
        it=0
        currentPE=0
        intW=0
        extW=0
        PE=0
        sumPE=sum(PE)
        fairness=0.0_dp
        endurance=0.0_dp
        numgccalls =0
        victimhotness=0
        transientdist=0
        reqit=0
        hotpgf(currentPE)=count(FTL(1:maxnumhot,1) /= 0)
        hotblf(currentPE)=count(blockhotness == 1)
        coldpgf(currentPE)=count(FTL(maxnumhot+1:maxnumvalid,1) /= 0)
        coldblf(currentPE)=N-hotblf(currentPE)

        numhotgccalls=0
        interhotvictim=0! Time between choosing Hot victim blocks
        !Upper limits of items in bin
        do distit=0,numhotgcbins-2
            interhotvictimlim(distit)=distit!5*distit
        end do ! (distit=0,numhotgcbins-2)
        interhotvictimlim(numhotgcbins-1)=0 !Actually infinity
        interhotvictimbins=0

        !! Simulation
        do while(currentPE < maxPE .or. it <= maxit)
            do while(all(WFindex < b))
                reqit=reqit+1
                reqit=mod(reqit-1,numreqlong)+1

                lpn=requests(mod(reqit, numreqlong)+1,1)
                isWrite=requests(mod(reqit, numreqlong)+1,2) /= 0
                p=FTL(lpn,1)
                bl=FTL(lpn,2)

                if(lpn <= maxnumhot) then
                    hotness=hotBlock
                else
                    hotness=coldBlock
                end if ! (lpn <= maxnumhot)

                if (isWrite) then
                    WFindex(hotness)=WFindex(hotness)+1
                    if (p /= 0) then ! Invalidate if valid page
                        ! Remove old page
                        SSD(p,bl) = 0
                        validPages(bl)=validPages(bl)-1
                    end if
                    ! Write update to WFE
                    FTL(lpn, 1:2) =(/ WFindex(hotness), WF(hotness)/)
                    SSD(WFindex(hotness),WF(hotness))=lpn
                    validPages(WF(hotness))=validPages(WF(hotness))+1

                    intW=intW+1
                    extW=extW+1
                else if (p /= 0) then ! TRIM
                    SSD(p,bl) = 0
                    validPages(bl)=validPages(bl)-1
                    FTL(lpn, 1:2) =(/0, 0/)
                end if ! (isWrite)
            end do !(WFvalid < b)

            !! GCA invocation
            gccalls=0
            failure=.true.

            do while(failure)
                if(WFindex(coldBlock) == b) then
                    same=coldBlock
                    other=hotBlock
                else
                    same=hotBlock
                    other=coldBlock
                end if ! (WFindex(0) == b)

                validPages(WF)=b+1
                victim=GCCOLD(rng, d, N, validPages, blockhotness, hotBlock, same == coldBlock)
                !!TODO: Bookkeep how many times we select a cold victim/convert a block?
                validPages(WF)=count(SSD(:,WF) > 0, 1)

                if(all(victim /= WF)) then
                    if(PE(victim) == currentPE .and. currentPE < maxPE) then
                        currentPE=currentPE+1

                        fairness(currentPE)=sum(PE)/dble(N*currentPE)
                        endurance(currentPE)=dble(extW)/pageCount
                        hotpgf(currentPE)=count(FTL(1:maxnumhot,1) /= 0)
                        hotblf(currentPE)=count(blockhotness == 1)
                        coldpgf(currentPE)=count(FTL(maxnumhot+1:maxnumvalid,1) /= 0)
                        coldblf(currentPE)=N-hotblf(currentPE)
                    end if ! (PE(victim) == currentPE .and. currentPE < maxPE)

                    it=it+1

                    if (it == maxit) then
                        !! Stats at end of run
                        distL=minval(PE)
                        distU=maxval(PE)
                        allocate(dist(distL:distU))
                        dist(distL)=count(PE==distL)
                        do distit=distL+1,distU
                            dist(distit)=dist(distit-1)+count(PE == distit)
                        end do ! (distit=distL+1,distU)
                        dist=dist/N

                        do distit=0,b
                            validdist(distit)=count(validPages == distit)
                        end do ! (distit=0.b)
                        validdist=validdist/N

                        it=maxit+1 !make sure this does not happen again
                    end if !(it == maxit)

                    if(blockhotness(victim) == 1) then
                        numhotgccalls = numhotgccalls +1
                        !Determine correct bin
                        binID=numhotgcbins-1
                        do distit=0,numhotgcbins-2
                            if(interhotvictim <= interhotvictimlim(distit)) then
                                binID=distit
                                exit ! Exit out of loop
                            end if ! (interhotvictim <= interhotvictimlim(distit))
                        end do ! (distit=0,numhotgcbins-2)
                        interhotvictimbins(binID)&
                                        =interhotvictimbins(binID)+1
                        !Reset timer
                        interhotvictim=0
                    else
                        !Not yet chosen hot victim, so increase "timer"
                        interhotvictim=interhotvictim+1
                    end if ! (blockhotness(victim) == 1)


                    j=validPages(victim)
                    victimValids(j)=victimValids(j)+1

                    jst=WFindex(other) ! Not equal to validPages(WF(other)), because incoming requests can invalidate in WFI
                    gccalls=gccalls+1

                    k=b-jst !Free in WFI
                    victimcontent=SSD(1:b,victim)

                    !! Erase victim
                    SSD(:,victim)=0
                    validPages(victim)=0

                    if(blockhotness(victim) == same) then
                        WF(same)=victim
                        WFindex(same)=0
                        do i=1,b
                            lpn=victimcontent(i)
                            if(lpn /= 0) then
                                WFindex(same)=WFindex(same)+1
                                FTL(lpn,:)=(/WFindex(same), WF(same)/)
                                SSD(WFindex(same),WF(same))=lpn
                                intW=intW+1
                            end if ! (lpn /= 0)
                        end do ! (i=1,b)

                        validPages(WF(same))=j

                        failure=.false.

                    elseif(j <= k) then ! Cold victim, sufficient space in CWF

                        !Copy to CWF
                        do i=1,b
                            lpn=victimcontent(i)
                            if(lpn /= 0) then
                                WFindex(other)=WFindex(other)+1
                                FTL(lpn,:)=(/WFindex(other), WF(other)/)
                                SSD(WFindex(other),WF(other))=lpn
                                intW=intW+1
                            end if
                        end do
                        validPages(WF(other))=validPages(WF(other))+j
                        !No modifications for (hot)validPages necessary here
                        validPages(victim)=0

                        ! HWF <- victim
                        WF(same) = victim
                        blockhotness(WF(same))=same
                        WFindex(same)=0

                        failure=.false.

                    else ! j > k

                        ! Copy k of j to CWF, rest to self
                        kdiff=k
                        if(kdiff == 0) then !Turns 0 for first time
                            WFindex(other) = 0 ! WFIindex remains between GCA invocations, only reset here
                        end if ! (kdiff == 0)

                        do i=1,b
                            lpn=victimcontent(i)
                            if(lpn /= 0) then
                                WFindex(other) = WFindex(other)+1
                                if (kdiff == 0) then ! No space on WFI, copy back to self
                                    SSD(WFindex(other),victim)=lpn
                                    FTL(lpn,:)=(/WFindex(other),victim/)
                                else ! (kdiff > 0) !Copy to WFI
                                    SSD(WFindex(other),WF(other))=lpn
                                    FTL(lpn,:)=(/WFindex(other), WF(other)/)

                                    kdiff=kdiff-1
                                    if(kdiff == 0) then !Turns 0 for first time
                                        WFindex(other) = 0 ! WFIindex remains between GCA invocations, only reset here
                                    end if ! (kdiff == 0)
                                end if ! (kdiff == 0)
                                intW=intW+1
                            end if ! (lpn /= 0)
                        end do ! (i=1,b)
                        validPages(victim)=j-k ! This is done local (during GC), so this always holds true (even if requests for pages in WFI and WFE)
                        validPages(WF(other))=validPages(WF(other))+k ! Old WF(other) filled completely during loop

                        WF(other) = victim
                        blockhotness(WF(other))=other
                    end if ! (blockhotness(victim) == same)

                    !! PE cycle on victim
                    PE(victim)=PE(victim)+1
                    sumPE=sumPE+1
                end if ! (victim /= WFE,WFI)
            end do ! (failure)

            numgccalls=numgccalls+gccalls

        end do !(currentPE < maxPE .or. it < maxit)
        WA=dble(intW)/dble(extW)

        hotGCfreq=dble(numhotgccalls)/numgccalls


        if(present(initrandom) .and. initrandom) then
            write (WAfilename,        20)    b,d,rho,f,traceid,runit
            write (distfilename,      21)    b,d,rho,f,traceid,runit
            write (fairfilename,      22)    b,d,rho,f,traceid,runit
            write (endufilename,      23)    b,d,rho,f,traceid,runit
            write (victimfilename,    24)    b,d,rho,f,traceid,runit
            write (victimhotfilename, 25)    b,d,rho,f,traceid,runit
            write (hotvicfreqfilename, 26)   b,d,rho,f,traceid,runit
            write (interhotvicfilename,27)   b,d,rho,f,traceid,runit
            write (validfilename,      28)   b,d,rho,f,traceid,runit
            write (hotblffilename,     29)   b,d,rho,f,traceid,runit
            write (coldblffilename,    30)   b,d,rho,f,traceid,runit
            write (hotpgffilename,     31)   b,d,rho,f,traceid,runit
            write (coldpgffilename,    32)   b,d,rho,f,traceid,runit
        else
            write (WAfilename,         40)   b,d,rho,f,traceid,runit
            write (distfilename,       41)   b,d,rho,f,traceid,runit
            write (fairfilename,       42)   b,d,rho,f,traceid,runit
            write (endufilename,       43)   b,d,rho,f,traceid,runit
            write (victimfilename,     44)   b,d,rho,f,traceid,runit
            write (victimhotfilename,  45)   b,d,rho,f,traceid,runit
            write (hotvicfreqfilename, 46)   b,d,rho,f,traceid,runit
            write (interhotvicfilename,47)   b,d,rho,f,traceid,runit
            write (validfilename,      48)   b,d,rho,f,traceid,runit
            write (hotblffilename,     49)   b,d,rho,f,traceid,runit
            write (coldblffilename,    50)   b,d,rho,f,traceid,runit
            write (hotpgffilename,     51)   b,d,rho,f,traceid,runit
            write (coldpgffilename,    52)   b,d,rho,f,traceid,runit
        end if ! (present(initrandom) .and. initrandom)

        call PrintToFile (WAfilename,    (/WA/), 1, 1)
        call PrintToFile (distfilename,  dist, distL, distU)
        call PrintToFile (fairfilename,  fairness, 0, maxPE)
        call PrintToFile (endufilename,  endurance, 0, maxPE)
        call PrintInteger(victimfilename,  victimValids, 0, b)
        call PrintReal   (victimhotfilename,  victimhotness/dble(maxit), 0, b*(b+1))
        call PrintToFile (hotvicfreqfilename,  (/hotGCfreq/),    1, 1)
        call PrintInteger(interhotvicfilename,  interhotvictimbins, 0, numhotgcbins-1)
        call PrintReal   (validfilename, validdist, 0,b)
        call PrintToFile (hotblffilename,  hotblf,    0, maxPE)
        call PrintToFile (coldblffilename, coldblf, 0, maxPE)
        call PrintToFile (hotpgffilename, hotpgf, 0, maxPE)
        call PrintToFile (coldpgffilename, coldpgf, 0, maxPE)

        deallocate(FTL)
        deallocate(validPages)
        deallocate(PE)
        deallocate(SSD)
        deallocate(dist)

        20  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-WA.',I2,'.csv')
        21  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-dist.',I2,'.csv')
        22  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-fair.',I2,'.csv')
        23  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-end.',I2,'.csv')
        24  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-victim.',I2,'.csv')
        25  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-victimh.',I2,'.csv')
        26  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-hotGCf.',I2,'.csv')
        27  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-inter.',I2,'.csv')
        28  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-valid.',I2,'.csv')
        29  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-hotblf.',I2,'.csv')
        30  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-coldblf.',I2,'.csv')
        31  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-hotpgf.',I2,'.csv')
        32  format('coldtrace-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-coldpgf.',I2,'.csv')

        40  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-WA.',I2,'.csv')
        41  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-dist.',I2,'.csv')
        42  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-fair.',I2,'.csv')
        43  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-end.',I2,'.csv')
        44  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-victim.',I2,'.csv')
        45  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-victimh.',I2,'.csv')
        46  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-hotGCf.',I2,'.csv')
        47  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-inter.',I2,'.csv')
        48  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-valid.',I2,'.csv')
        49  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-hotblf.',I2,'.csv')
        50  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-coldblf.',I2,'.csv')
        51  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-hotpgf.',I2,'.csv')
        52  format('coldtrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-f',F6.4,'-',A4,'-coldpgf.',I2,'.csv')
    end subroutine SSDCOLDDCHTrace

    subroutine SSDHCWF(N,b,d,rho,r,f,maxPE,runit, rng, initrandom)
        use rng, only: rng_t, randi, rng_uniform

        integer, parameter :: hotsections=1000, numhotgcbins=11
        integer, parameter :: hotBlock=1, coldBlock=0
        type(rng_t), intent(inout) :: rng
        integer, intent(in):: N,b,d,maxPE,runit
        real(dp), intent(in) :: rho,r,f
        logical, intent(in), optional :: initrandom

        real(dp), allocatable, dimension(:) :: dist
        real(dp), dimension(0:maxPE) :: endurance, fairness
        real(dp), dimension(0:b) :: validdist
        real(dp), dimension(0:hotsections) :: victimhotness

        integer :: p,bl,it,hotness,i,j,k,lpn,victim,maxnumvalid,&
                    distL, distU,distit,maxit,temphot,temphotindex,same,other,maxhotbl
        integer :: intW, extW, gcwrites, gccalls, numgccalls, binID,&
                    currentPE, sumPE,numhotgccalls,interhotvictim
        integer, dimension(1:b,1:N)::SSD
        integer, dimension(0:1):: WF, maxnumhot
        integer, dimension(1:N):: PE,blockhotness, validPages
        integer, dimension(0:b) :: victimValids
        integer, dimension(1:numhotgcbins) :: interhotvictimlim,interhotvictimbins
        integer, allocatable, dimension(:,:):: FTL
        character(len=64) :: distfilename,endufilename, fairfilename,&
                        validfilename,victimfilename,victimhotfilename,&
                        WAfilename, hotvicfreqfilename, interhotvicfilename

        real(dp) :: pageCount, rannr, hotGCfreq, WA
        logical :: failure

        maxit=N*maxPE
        pageCount=dble(b*N)
        maxnumvalid=ceiling(rho*pageCount)
        maxnumhot(hotBlock)=floor(f*rho*pageCount)
        maxnumhot(coldBlock)=maxnumvalid-maxnumhot(1)
        maxhotbl=ceiling(f*N)

        !! Initialize
        allocate(FTL(1:maxnumvalid,1:2))

        SSD=0
        FTL=0
        WF(hotBlock)=1 !HWF
        WF(coldBlock)=maxhotbl+1 !CWF
        if( present(initrandom) .and. initrandom) then
            call initHCWFrandom(N,b,SSD,FTL,maxnumvalid,maxhotbl, maxnumhot,&
                            WF, hotBlock,coldBlock,rng)
        end if ! (present(initrandom) .and. initrandom)
        validPages = count(SSD > 0, 1)
        blockhotness(1:maxhotbl)=hotBlock
        blockhotness(maxhotbl+1:N)=coldBlock

        !! Simulation
        it=0
        currentPE=0
        intW=0
        extW=0
        PE=0
        sumPE=sum(PE)
        fairness =0.0_dp
        endurance=0.0_dp
        victimValids=0
        numgccalls =0
        numhotgccalls=0
        interhotvictim=0 ! Time between choosing Hot victim blocks
        ! Upper limits of items in bin; counting in 5's (less than 5 in first bin, less than 10 in second,...)
        do distit=1,numhotgcbins-1
            interhotvictimlim(distit)=5*distit
        end do ! (distit=1,numhotgcbins-1)

        interhotvictimlim(numhotgcbins)=0
        interhotvictimbins=0

        do while(currentPE < maxPE .or. it <= maxit)

            do while(all(validPages(WF) < b))
                rannr=rng_uniform(rng)
                if(rannr < r) then
                    hotness=hotBlock
                else
                    hotness=coldBlock
                end if ! (rannr < r)

                failure=.true.
                do while (failure)
                    lpn=sum(maxnumhot(1:hotness-1))+randi(rng,maxnumhot(hotness))
                    p=FTL(lpn,1)
                    bl=FTL(lpn,2)

                    if(p /= 0) then
                        if(all(bl /= WF)) then
                            !Remove old page
                            SSD(p,bl) = 0
                            validPages(bl)=validPages(bl)-1
                            !Write update to correct WF
                            validPages(WF(hotness))=validPages(WF(hotness))+1
                            FTL(lpn, 1:2) =(/validPages(WF(hotness)), WF(hotness)/)
                            SSD(validPages(WF(hotness)),WF(hotness))=lpn

                            failure=.false.
                        end if ! (all(bl /= WF))
                    else ! Write to empty logical page
                        validPages(WF(hotness))=validPages(WF(hotness))+1
                        FTL(lpn, 1:2) =(/ validPages(WF(hotness)), WF(hotness) /)
                        SSD(validPages(WF(hotness)),WF(hotness))=lpn

                        failure=.false.
                    end if ! (p /= 0)
                end do ! (failure)
                extW=extW+1
            end do ! (WFEvalid < b)


            !! GCA invocation
            gccalls=0
            gcwrites=0
            if(validPages(WF(hotBlock)) == b) then !HWF is full
                same=hotBlock
                other=coldBlock
            else !CWF is full
                same=coldBlock
                other=hotBlock
            end if ! (validPages(WF(1)) == b)

            failure=.true.
            do while(failure)
                validPages(WF)=b+1 ! Do not let GCA select the WF
                victim=GC(rng,d,N,validPages)
                validPages(WF)=count(SSD(:,WF) > 0, 1)

                if( all(victim /= WF) ) then
                    if(PE(victim) == currentPE .and. currentPE < maxPE) then
                        currentPE=currentPE+1
                        fairness(currentPE)=sum(PE)/dble(N*currentPE)
                        endurance(currentPE)=extW/pageCount
                    end if ! (PE(victim) == currentPE .and. currentPE < maxPE)
                    it=it+1
                    PE(victim)=PE(victim)+1

                    if (it == maxit) then
                        !! Stats at end of run
                        distL=minval(PE)
                        distU=maxval(PE)
                        if(allocated(dist)) then
                            deallocate(dist)
                        end if ! (allocated(dist))
                        allocate(dist(distL:distU))
                        dist(distL)=count(PE==distL)
                        do distit=distL+1,distU
                            dist(distit)=dist(distit-1)+count(PE == distit)
                        end do ! (distit=distL+1,distU)
                        dist=dist/N

                        do distit=0,b
                            validdist(distit)=count(validPages == distit)
                        end do ! (distit=0,b)
                        validdist=validdist/N

                        it=maxit+1 ! Make sure this does not happen again
                    end if ! (it == maxit)

                    if(blockhotness(victim) == 1) then
                        numhotgccalls = numhotgccalls +1
                        ! Determine correct bin
                        binID=numhotgcbins
                        do distit=1,numhotgcbins-1
                            if(interhotvictim <= interhotvictimlim(distit)) then
                                binID=distit
                                exit ! Exit out of loop
                            end if ! (interhotvictim <= interhotvictimlim(distit))
                        end do ! (distit=1,numhotgcbins-1)
                        interhotvictimbins(binID)&
                                        =interhotvictimbins(binID)+1
                        ! Reset timer
                        interhotvictim=0
                    else
                        ! Not yet chosen hot victim, so increase "timer"
                        interhotvictim=interhotvictim+1
                    end if ! (blockhotness(victim) == 1)

                    j=validPages(victim)
                    k=b-validPages(WF(other))
                    victimValids(j)=victimValids(j)+1
                    
                    temphot=count(1 <= SSD(1:b,victim) .and. SSD(1:b,victim) <= maxnumhot(1))
                    temphotindex=nint(hotsections*real(temphot)/validPages(victim))
                    
                    victimhotness(temphotindex)=victimhotness(temphotindex)+1
                    gccalls=gccalls+1
                    gcwrites=gcwrites+b-j
                    failure = processGCHCWF(N,b,SSD,FTL,maxnumvalid,validPages,PE,&
                                blockhotness,victim,j,k,WF,same,other)

                end if ! (all(victim /= WF))
            end do ! (failure)

            intW=intW+gcwrites
            numgccalls=numgccalls+gccalls


        end do ! (currentPE < maxPE .or. it < maxit)
        WA=dble(intW)/numgccalls
        WA=dble(b)/WA
        hotGCfreq=dble(numhotgccalls)/numgccalls

        write (WAfilename,          20) b,d,rho,r,f,runit
        write (distfilename,        21) b,d,rho,r,f,runit
        write (fairfilename,        23) b,d,rho,r,f,runit
        write (endufilename,        24) b,d,rho,r,f,runit
        write (victimfilename,      25) b,d,rho,r,f,runit
        write (victimhotfilename,   26) b,d,rho,r,f,runit
        write (hotvicfreqfilename,  29) b,d,rho,r,f,runit
        write (interhotvicfilename, 30) b,d,rho,r,f,runit
        write (validfilename,       31) b,d,rho,r,f,runit

        call PrintToFile  (WAfilename,  (/WA/),    0, 0)
        call PrintToFile  (distfilename,  dist,      distL, distU)
        call PrintToFile  (fairfilename,  fairness,  0,     maxPE)
        call PrintToFile  (endufilename,  endurance, 0,    maxPE)
        call PrintInteger (victimfilename,  victimValids, 0, b)
        call PrintReal    (victimhotfilename,  victimhotness, 0, hotsections)
        call PrintToFile  (hotvicfreqfilename,  (/hotGCfreq/),    0, 0)
        call Print2ColReal(interhotvicfilename, dble(interhotvictimlim),&
                interhotvictimbins/dble(sum(interhotvictimbins)), 0, numhotgcbins-1)
        call PrintReal    (validfilename, validdist, 0,b)


        20  format('hcwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-WA.',I2,'.csv')
        21  format('hcwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-dist.',I2,'.csv')
        23  format('hcwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-fair.',I2,'.csv')
        24  format('hcwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-end.',I2,'.csv')
        25  format('hcwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-victim.',I2,'.csv')
        26  format('hcwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-victimh.',I2,'.csv')
        29  format('hcwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-hotGCf.',I2,'.csv')
        30  format('hcwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-inter.',I2,'.csv')
        31  format('hcwf-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-valid.',I2,'.csv')
    end subroutine SSDHCWF

    

    subroutine SSDHCWFCOLD(N,b,d,rho,r,f,maxPE,runit, rng, initrandom)
        use rng, only: rng_t, randi, rng_uniform
    
        integer, parameter :: hotsections=1000, numhotgcbins=11
        integer, parameter :: hotBlock = 1, coldBlock = 0
        type(rng_t), intent(inout) :: rng
        integer, intent(in):: N,b,d,maxPE,runit
        real(dp), intent(in) :: rho,r,f
        logical, intent(in), optional :: initrandom
    
        real(dp), allocatable, dimension(:) :: dist
        real(dp), dimension(0:maxPE) :: endurance, fairness
        real(dp), dimension(0:b) :: validdist
        real(dp), dimension(0:hotsections) :: victimhotness
    
        integer :: p,bl,it,hotness,i,j,k,lpn,victim,maxnumvalid,&
                    distL, distU,distit,maxit,temphot,temphotindex,same,other,maxhotbl
        integer :: intW, extW, gcwrites, gccalls, numgccalls, binID,&
                    currentPE, sumPE,numhotgccalls,interhotvictim
        integer, dimension(1:b,1:N)::SSD
        integer, dimension(0:1):: WF, maxnumhot
        integer, dimension(1:N):: PE,blockhotness, validPages
        integer, dimension(0:b) :: victimValids
        integer, dimension(1:numhotgcbins) :: interhotvictimlim,interhotvictimbins
        integer, allocatable, dimension(:,:):: FTL
        character(len=64) :: distfilename,endufilename, fairfilename,&
                        validfilename,victimfilename,victimhotfilename,&
                        WAfilename, hotvicfreqfilename, interhotvicfilename
    
        real(dp) :: pageCount, rannr, hotGCfreq, WA
        logical :: failure
    
        maxit=N*maxPE
        pageCount=dble(b*N)
        maxnumvalid=ceiling(rho*pageCount)
        maxnumhot(hotBlock)=floor(f*rho*pageCount)
        maxnumhot(coldBlock)=maxnumvalid-maxnumhot(1)
        maxhotbl=ceiling(f*N)
    
        !! Initialize
        allocate(FTL(1:maxnumvalid,1:2))
    
        SSD=0
        FTL=0
        WF(coldBlock)= maxhotbl+1 !CWF
        WF(hotBlock) = 1 !HWF
        if(present(initrandom) .and. initrandom) then
            call initHCWFrandom(N,b,SSD,FTL,maxnumvalid,maxhotbl, maxnumhot,&
                            WF, hotBlock,coldBlock,rng)
        end if ! (present(initrandom) .and. initrandom)
        validPages = count(SSD > 0, 1)
        blockhotness(1:maxhotbl)=hotBlock
        blockhotness(maxhotbl+1:N)=coldBlock
    
        !! Simulation
        it=0
        currentPE=0
        intW=0
        extW=0
        PE=0
        sumPE=sum(PE)
        fairness =0.0_dp
        endurance=0.0_dp
        victimValids=0
        numgccalls =0
        numhotgccalls=0
        interhotvictim=0 ! Time between choosing Hot victim blocks
        ! Upper limits of items in bin; counting in 5's (less than 5 in first bin, less than 10 in second,...)
        do distit=1,numhotgcbins-1
            interhotvictimlim(distit)=5*distit
        end do ! (distit=1,numhotgcbins-1)
    
        interhotvictimlim(numhotgcbins)=0
        interhotvictimbins=0
    
        do while(currentPE < maxPE .or. it <= maxit)
    
            do while(all(validPages(WF) < b))
                rannr=rng_uniform(rng)
                if(rannr < r) then
                    hotness=hotBlock
                else
                    hotness=coldBlock
                end if ! (rannr < r)
    
                failure=.true.
                do while (failure)
                    lpn=sum(maxnumhot(1:hotness-1))+randi(rng,maxnumhot(hotness))
                    p=FTL(lpn,1)
                    bl=FTL(lpn,2)
    
                    if(p /= 0) then
                        if(all(bl /= WF)) then
                            !Remove old page
                            SSD(p,bl) = 0
                            validPages(bl)=validPages(bl)-1
                            !Write update to correct WF
                            validPages(WF(hotness))=validPages(WF(hotness))+1
                            FTL(lpn, 1:2) =(/validPages(WF(hotness)), WF(hotness)/)
                            SSD(validPages(WF(hotness)),WF(hotness))=lpn
    
                            failure=.false.
                        end if ! (all(bl /= WF))
                    else ! Write to empty logical page
                        validPages(WF(hotness))=validPages(WF(hotness))+1
                        FTL(lpn, 1:2) =(/ validPages(WF(hotness)), WF(hotness) /)
                        SSD(validPages(WF(hotness)),WF(hotness))=lpn
    
                        failure=.false.
                    end if ! (p /= 0)
                end do ! (failure)
                extW=extW+1
            end do ! (WFEvalid < b)
    
    
            !! GCA invocation
            gccalls=0
            gcwrites=0
            if(validPages(WF(hotBlock)) == b) then ! HWF is full
                same=hotBlock
                other=coldBlock
            else ! CWF is full
                same=coldBlock
                other=hotBlock
            end if ! (validPages(WF(hotvalue)) == b)
    
            failure=.true.
            do while(failure)
            
                validPages(WF)=b+1 ! Do not let GCA select the WF
                victim=GCCOLD(rng, d, N, validPages, blockhotness, hotBlock, same == coldBlock)
                validPages(WF)=count(SSD(:,WF) > 0, 1)
    
                if( all(victim /= WF) ) then
                    if(PE(victim) == currentPE .and. currentPE < maxPE) then
                        currentPE=currentPE+1
                        fairness(currentPE)=sum(PE)/dble(N*currentPE)
                        endurance(currentPE)=extW/pageCount
                    end if ! (PE(victim) == currentPE .and. currentPE < maxPE)
                    it=it+1
                    PE(victim)=PE(victim)+1
    
                    if (it == maxit) then
                        !! Stats at end of run
                        distL=minval(PE)
                        distU=maxval(PE)
                        if(allocated(dist)) then
                            deallocate(dist)
                        end if ! (allocated(dist))
                        allocate(dist(distL:distU))
                        dist(distL)=count(PE==distL)
                        do distit=distL+1,distU
                            dist(distit)=dist(distit-1)+count(PE == distit)
                        end do ! (distit=distL+1,distU)
                        dist=dist/N
    
                        do distit=0,b
                            validdist(distit)=count(validPages == distit)
                        end do ! (distit=0,b)
                        validdist=validdist/N
    
                        it=maxit+1 ! Make sure this does not happen again
                    end if ! (it == maxit)
    
                    if(blockhotness(victim) == hotBlock) then
                        numhotgccalls = numhotgccalls +1
                        ! Determine correct bin
                        binID=numhotgcbins
                        do distit=1,numhotgcbins-1
                            if(interhotvictim <= interhotvictimlim(distit)) then
                                binID=distit
                                exit ! Exit out of loop
                            end if ! (interhotvictim <= interhotvictimlim(distit))
                        end do ! (distit=1,numhotgcbins-1)
                        interhotvictimbins(binID)&
                                        =interhotvictimbins(binID)+1
                        ! Reset timer
                        interhotvictim=0
                    else
                        ! Not yet chosen hot victim, so increase "timer"
                        interhotvictim=interhotvictim+1
                    end if ! (blockhotness(victim) == hotBlock)
    
                    j=validPages(victim)
                    k=b-validPages(WF(other))
                    victimValids(j)=victimValids(j)+1
                    
                    temphot=count(1 <= SSD(1:b,victim) .and. SSD(1:b,victim) <= maxnumhot(1))
                    temphotindex=nint(hotsections*real(temphot)/validPages(victim))
                    
                    victimhotness(temphotindex)=victimhotness(temphotindex)+1
                    gccalls=gccalls+1
                    gcwrites=gcwrites+b-j
                    failure = processGCHCWF(N,b,SSD,FTL,maxnumvalid,validPages,PE,&
                                blockhotness,victim,j,k,WF,same,other)
                    
                end if ! (all(victim /= WF))
            end do ! (failure)
    
            intW=intW+gcwrites
            numgccalls=numgccalls+gccalls
    
    
        end do ! (currentPE < maxPE .or. it < maxit)
        WA=dble(intW)/numgccalls
        WA=dble(b)/WA
        hotGCfreq=dble(numhotgccalls)/numgccalls
    
        write (WAfilename,          20) b,d,rho,r,f,runit
        write (distfilename,        21) b,d,rho,r,f,runit
        write (fairfilename,        23) b,d,rho,r,f,runit
        write (endufilename,        24) b,d,rho,r,f,runit
        write (victimfilename,      25) b,d,rho,r,f,runit
        write (victimhotfilename,   26) b,d,rho,r,f,runit
        write (hotvicfreqfilename,  29) b,d,rho,r,f,runit
        write (interhotvicfilename, 30) b,d,rho,r,f,runit
        write (validfilename,       31) b,d,rho,r,f,runit
    
        call PrintToFile  (WAfilename,  (/WA/),    0, 0)
        call PrintToFile  (distfilename,  dist,      distL, distU)
        call PrintToFile  (fairfilename,  fairness,  0,     maxPE)
        call PrintToFile  (endufilename,  endurance, 0,    maxPE)
        call PrintInteger (victimfilename,  victimValids, 0, b)
        call PrintReal    (victimhotfilename,  victimhotness, 0, hotsections)
        call PrintToFile  (hotvicfreqfilename,  (/hotGCfreq/),    0, 0)
        call Print2ColReal(interhotvicfilename, dble(interhotvictimlim),&
                interhotvictimbins/dble(sum(interhotvictimbins)), 0, numhotgcbins-1)
        call PrintReal    (validfilename, validdist, 0,b)
    
    
        20  format('cold-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-WA.',I2,'.csv')
        21  format('cold-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-dist.',I2,'.csv')
        23  format('cold-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-fair.',I2,'.csv')
        24  format('cold-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-end.',I2,'.csv')
        25  format('cold-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-victim.',I2,'.csv')
        26  format('cold-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-victimh.',I2,'.csv')
        29  format('cold-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-hotGCf.',I2,'.csv')
        30  format('cold-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-inter.',I2,'.csv')
        31  format('cold-b',I2,'-d',I3,'-rho',F4.2,'-r',F4.2,'-f',F4.2,'-valid.',I2,'.csv')
    end subroutine SSDHCWFCOLD

end module hcwfsim
