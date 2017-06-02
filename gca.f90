module gca
    use utils, only : dp

    public GC, GCCOLD

    contains

    function GC(rng,d,N,validPages)
        use rng, only : rng_t, randid

        integer :: i,GC
        integer, save:: fifoCtr=1 !! Static variable
        type(rng_t), intent(inout) :: rng
        integer, intent(in)::d,N
        integer, dimension(1:N), intent(in) :: validPages
        integer,dimension(1:d):: dVec

        if(d == .0) then !! FIFO
            fifoCtr=mod(fifoCtr,N)+1
            GC=fifoCtr
        else if(d == N) then !! Greedy
            GC=minloc(validPages,1)
        else !! DChoices
            call randid(rng, N, dVec)
            i=minloc(validPages(dVec),1)
            GC=dVec(i)
        end if
    end function GC
    
    !Special GC for Hot/Cold data
    ! This tries to limit the amount of hot->cold block conversions.
    ! Rationale: Spread same amount of hot data, across as many blocks as possible.
    function GCCOLD(rng,d,N,validPages, hotness, hotvalue, replacingCWF)
        use rng, only : rng_t, randid

        integer :: i,GCCOLD
        !integer, save:: fifoCtr=1 !! Static variable
        type(rng_t), intent(inout) :: rng
        integer, intent(in)::d,N, hotvalue
        integer, dimension(1:N), intent(in) :: validPages, hotness
        integer,dimension(1:d):: dVec, dValid
        logical, intent(in) :: replacingCWF
        
        if(replacingCWF .and. d > 0 .and. d < N) then !Special behavior
            !! DChoices
            !!TODO: Implement greedy cold?
            call randid(rng, N, dVec)
            dValid=validPages(dVec)
            !Assume b<N, so validPages would never be as much as N; sidestepping a need to pass value of b to this function.
            ! Matlab equivalent: dValid(hotness(dVec) == hotvalue) = N
            do i=1,d
                if(hotness(dVec(i)) == hotValue) then
                    dValid(i) = N
                end if
            end do
            !Re-use i to find location of minimum
            i=minloc(dValid,1)
            if(dValid(i) == N) then ! No cold blocks in this batch :(
                GCCOLD=dVec(minloc(validPages(dVec),1))
            else ! Found cold block!
                GCCOLD=dVec(i)
            end if
        else !Replacing hot WF, no difference, so just pass everything
            GCCOLD = GC(rng,d,N,validPages)
        end if
    end function GCCOLD


end module gca
