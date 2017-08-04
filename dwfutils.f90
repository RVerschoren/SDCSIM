module dwfutils
    use utils, only: dp,PrintInteger, PrintToFile, PrintReal, Print2ColReal
    implicit none

    private 
    public processGCDWF,initDWFrandom,processGCDWFTrace

    contains

    
    subroutine initDWFrandom(N, b, SSD, FTL, maxnumvalid, WFE, WFI, rng)

        use rng, only: rng_t, randi

        integer, intent(in) :: b, N, maxnumvalid, WFE, WFI
        integer, intent(inout), dimension(1:b,1:N) :: SSD
        integer, intent(inout), dimension(1:maxnumvalid,1:2) :: FTL
        type(rng_t), intent(inout) :: rng
        
        integer :: it, p, bl
        logical :: failure
        
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
    end subroutine initDWFrandom

    ! !!!!!!!!!!!!!! !
    ! Synthetic data !
    ! !!!!!!!!!!!!!! !
    function processGCDWF(N,b,SSD,FTL,maxnumvalid,maxnumhot,validPages,hotValidPages,&
        victim,WFE,WFI) result(failure)

        integer, intent(in) :: b, N, victim, maxnumvalid
        integer, intent(inout) :: WFE, WFI
        integer, intent(inout), dimension(1:b,1:N) :: SSD
        integer, intent(inout), dimension(1:maxnumvalid,1:2) :: FTL
        integer, intent(inout), dimension(0:1) :: maxnumhot
        integer, intent(inout), dimension(1:N) :: validPages, hotValidPages!, PE

        logical :: failure

        integer :: i, tmpvictimfill, lpn, j, jst, k, kdiff
        integer, dimension(1:b) :: valpvict, victimcontent

        j=validPages(victim)
        jst=validPages(WFI)
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

            !temphot=count(0 < SSD(1:b,WFI) .and. SSD(1:b,WFI) <= maxnumhot(1))
            !WFIhotness(temphot)=WFIhotness(temphot)+1
            hotValidPages(WFI)=count(0 < SSD(1:b,WFI) .and. SSD(1:b,WFI) <= maxnumhot(1))!temphot

            !! Replace WFI with victim
            WFI=victim
            validPages(WFI)=j-k
            hotValidPages(WFI)=count(0 < SSD(1:b,WFI) .and. SSD(1:b,WFI) <= maxnumhot(1))
        end if !(j <=k)

    end function processGCDWF






    ! !!!!!!!!!!!!!!! !
    !   Trace-based   !
    ! !!!!!!!!!!!!!!! !

    function processGCDWFTrace(N,b,SSD,FTL,maxnumvalid,validPages,PE,&
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

    end function processGCDWFTrace

end module dwfutils
