module hcwfutils
    use utils, only: dp,PrintInteger, PrintToFile, PrintReal, Print2ColReal
    implicit none

    private 
    public processGCHCWF,initHCWFrandom,processGCHCWFTrace,initHCWFTracerandom

    contains

    ! !!!!!!!!!!!!!! !
    ! Synthetic data !
    ! !!!!!!!!!!!!!! !
    subroutine initHCWFrandom(N,b,SSD,FTL,maxnumvalid,maxhotbl, maxnumhot,&
        WF, hotBlock,coldBlock, rng)

        use rng, only: rng_t, randi

        integer, intent(in) :: b,N,maxnumvalid,maxhotbl,hotBlock,coldBlock
        integer, intent(in), dimension(0:1) :: WF, maxnumhot
        integer, intent(inout), dimension(1:b,1:N) :: SSD
        integer, intent(inout), dimension(1:maxnumvalid,1:2) :: FTL
        type(rng_t), intent(inout) :: rng
        
        integer :: it, p, bl
        logical :: failure
        
        do it=1, maxnumvalid
            failure=.true.
            do while(failure)
                p=randi(rng,b)
                if(it < maxnumhot(hotBlock)) then
                    bl=randi(rng,maxhotbl)
                else
                    bl=maxhotbl+randi(rng,N-maxhotbl)
                end if ! (it < maxnumhot(1))
                if(SSD(p,bl) <= 0 .and. all(WF /= bl)) then
                    SSD(p,bl) = it
                    FTL(it,1)=p
                    FTL(it,2)=bl
                    failure=.false.
                end if ! (SSD(p,bl) <= 0 .and. all(WF /= bl))
            end do ! (failure)
        end do ! (it=1,maxnumvalid)
    end subroutine initHCWFrandom

    function processGCHCWF(N,b,SSD,FTL,maxnumvalid,validPages,PE,&
        blockhotness,victim,j,k,WF,same,other) result(failure)

        integer, intent(in) :: b,N, victim, j,k, same, other,maxnumvalid
        integer, intent(inout), dimension(1:b,1:N) :: SSD
        integer, intent(inout), dimension(1:maxnumvalid,1:2) :: FTL
        integer, intent(inout), dimension(0:1) :: WF
        integer, intent(inout), dimension(1:N) :: PE,blockhotness, validPages

        logical :: failure

        integer :: i,tmpvictimfill
        integer, dimension(1:b) :: valpvict


        failure=.true.!Assume failure to choose block with same hotness
        
        valpvict=SSD(:,victim)

        if(blockhotness(victim) == same) then ! Hot victim
            validPages(victim)= 0
            do i=1,b
                if(valpvict(i)>0) then
                    validPages(victim)=validPages(victim)+1
                    SSD(validPages(victim),victim)=valpvict(i)
                    FTL(valpvict(i),:)=(/validPages(victim),victim/)
                end if
            end do
            SSD(validPages(victim)+1:b,victim)=0

            WF(same)=victim
            failure=.false.

        elseif(j <= k) then ! Cold victim, sufficient space in CWF
            ! Copy to CWF
            do i=1,b
                if(valpvict(i)>0) then
                    validPages(WF(other))=validPages(WF(other))+1
                    SSD(validPages(WF(other)),WF(other))=valpvict(i)
                    FTL(valpvict(i),:)=(/ validPages(WF(other)),WF(other) /)
                end if
            end do
            ! HWF <- victim
            WF(same)=victim
            validPages(WF(same))=0
            SSD(:,WF(same))=0
            blockhotness(WF(same))=same
            failure=.false.

        else ! j > k
            ! Copy k of j to CWF, rest to self
            tmpvictimfill=0
            SSD(:,victim)=0
            do i=1,b
                if(valpvict(i)>0) then
                    validPages(WF(other))=validPages(WF(other))+1
                    if(validPages(WF(other)) <= b)then
                        !Copy to old CWF
                        SSD(validPages(WF(other)),WF(other))=valpvict(i)
                        FTL(valpvict(i),:)=(/validPages(WF(other)),WF(other)/)
                    else
                        ! Copy to self
                        tmpvictimfill=tmpvictimfill+1
                        SSD(tmpvictimfill,victim)=valpvict(i)
                        FTL(valpvict(i),:)=(/tmpvictimfill,victim/)
                    end if ! (validPages(WF(other)))
                end if ! (valpvict(i)>0)
            end do ! (i=1,b)
            !! CWF <- victim
            validPages(WF(other))=b ! Possibly went over b for validPages, so repair
            WF(other)=victim
            validPages(WF(other))=j-k
            SSD((j-k+1):b,WF(other))=0
        end if ! (blockhotness(victim) == same)

    end function processGCHCWF






    ! !!!!!!!!!!!!!!! !
    !   Trace-based   !
    ! !!!!!!!!!!!!!!! !
    subroutine initHCWFTracerandom(N,b,SSD,FTL,maxnumvalid,maxhotbl, maxnumhot,&
        WF, WFindex, hotBlock,coldBlock, rng)

        use rng, only: rng_t, randi

        integer, intent(in) :: b,N,maxnumvalid,maxhotbl,hotBlock,coldBlock,maxnumhot
        integer, intent(in), dimension(0:1) :: WF
        integer, intent(inout), dimension(0:1) :: WFindex
        integer, intent(inout), dimension(1:b,1:N) :: SSD
        integer, intent(inout), dimension(1:maxnumvalid,1:2) :: FTL
        type(rng_t), intent(inout) :: rng
        
        integer :: it, p, bl, hotness
        logical :: failure, isHot

        do it=1, maxnumvalid
            failure=.true.
            do while(failure)
                p=randi(rng,b)
                isHot= (it <= maxnumhot)
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

    end subroutine initHCWFTracerandom

    function processGCHCWFTrace(N,b,SSD,FTL,maxnumvalid,validPages,PE,&
        blockhotness,victim,WF,WFindex,same,other) result(failure)

        integer, intent(in) :: b,N, victim, same, other,maxnumvalid
        integer, intent(inout), dimension(1:b,1:N) :: SSD
        integer, intent(inout), dimension(1:maxnumvalid,1:2) :: FTL
        integer, intent(inout), dimension(0:1) :: WF, WFindex
        integer, intent(inout), dimension(1:N) :: PE,blockhotness, validPages

        logical :: failure

        integer :: i,j,jst,k,kdiff,tmpvictimfill,lpn
        integer, dimension(1:b) :: victimcontent, valpvict
        
        j=validPages(victim)
        !victimValids(j)=victimValids(j)+1

        jst=WFindex(other) ! Not equal to validPages(WF(other)), because incoming requests can invalidate in WFI

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
                    !intW=intW+1
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
                    !intW=intW+1
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
                    !intW=intW+1
                end if ! (lpn /= 0)
            end do ! (i=1,b)
            validPages(victim)=j-k ! This is done local (during GC), so this always holds true (even if requests for pages in WFI and WFE)
            validPages(WF(other))=validPages(WF(other))+k ! Old WF(other) filled completely during loop

            WF(other) = victim
            blockhotness(WF(other))=other
        end if ! (blockhotness(victim) == same)
    end function processGCHCWFTrace

end module hcwfutils
