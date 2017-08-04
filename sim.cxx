//module sim
//    use utils, only: dp,PrintInteger, PrintToFile, PrintReal, Print2ColReal
//    implicit none
//
//    private GC, SSDDWF, SSDHCWF, SSDDWFTrace, SSDHCWFTrace, SSDCOLDDCHTrace
//    public SSDDWFRuns, SSDHCWFRuns, SSDDWFTraceRuns, SSDHCWFTraceRuns, SSDCOLDDCHTraceRuns, SSDHCORACLETraceRuns
//
//    contains
//
#include <random>
#include "sim.hxx"

namespace rng{

class PRNG
{

public:

PRNG() : _rng(), unifintdist(), unifrealdist(0,1){
    //PRNG::result_type seed_val= 932117 + run_nr -1;
    _rng.seed(std::random_device()());
    //std::uniform_int_distribution<unsigned int> unifintdist();
    //std::uniform_real_distribution<double> unifrealdist(0,1)
}


    /// Generates integer number in [0,max_val[
    unsigned int randi(const unsigned int max_val) const{
        return unifintdist(_rng) % max_val;
    }

    /// Generates d integer numbers in [0, max_val[
    /// d equals the size of the output vector dVec
    void randid(std::vector<unsigned int> &dVec, const unsigned int max_val) const{
        for(unsigned int it=0; it < dVec.size(); it++)
        {
            dVec[it] = randi(max_val);
        }
    }

    /// Generates a real number in [0,1[
    double rand() const{
        return unifrealdist(_rng);
    }
}

}

namespace gc{
    //function GC(rng,d,N,validPages)
    //    use rng, only : rng_t, randid
    //
    //    integer :: i,GC
    //    integer, save:: fifoCtr=1 !! Static variable
    //    type(rng_t), intent(inout) :: rng
    //    integer, intent(in)::d,N
    //    integer, dimension(1:N), intent(in) :: validPages
    //    integer,dimension(1:d):: dVec
    //
    //    if(d.eq.0) then !! FIFO
    //        fifoCtr=mod(fifoCtr,N)+1
    //        GC=fifoCtr
    //    else if(d.eq.N) then !! Greedy
    //        GC=minloc(validPages,1)
    //    else !! DChoices
    //        call randid(rng, N, dVec)
    //        i=minloc(validPages(dVec),1)
    //        GC=dVec(i)
    //    end if
    //end function GC
    
    BlockNumber DChoices(const unsigned int d, const PRNG& prng, const std::vector<unsigned int> &validPages)
    {
        const unsigned int N = validPages.size();
        std::vector<unsigned int> dVec(d,0);
        prng.randid(dVec, N);
        BlockNumber minValidIt = dVec[0];
        for(unsigned int it=0; it < d; it++)
        {
            if(validPages[dVec[it]] < validPages[minValidIt]){
                minValidIt = dVec[it];
            }
        }
        return minValidIt;
    }

    BlockNumber Random(const PRNG& prng, const std::vector<unsigned int> &validPages)
    {
        return prng.randid(validPages.size());
    }
        
    

    //!Special GC for Hot/Cold data
    //! This tries to limit the amount of hot->cold block conversions.
    //! Rationale: Spread same amount of hot data, across as many blocks as possible.
    //function GCCOLD(rng,d,N,validPages, hotness, hotvalue, replacingCWF)
    //    use rng, only : rng_t, randid
    //
    //    integer :: i,GCCOLD
    //    !integer, save:: fifoCtr=1 !! Static variable
    //    type(rng_t), intent(inout) :: rng
    //    integer, intent(in)::d,N, hotvalue
    //    integer, dimension(1:N), intent(in) :: validPages, hotness
    //    integer,dimension(1:d):: dVec, dValid
    //    logical, intent(in) :: replacingCWF
    //    
    //    if(replacingCWF) then !Special behavior
    //        !! DChoices
    //        !!TODO: Assume DChoices for now, but enable in future
    //        call randid(rng, N, dVec)
    //        dValid=validPages(dVec)
    //        !Assume b<N, so validPages would never be as much as N; sidestepping a need to pass value of b to this function.
    //        ! Matlab equivalent: dValid(hotness(dVec) == hotvalue) = N
    //        do i=1,d
    //            if(hotness(dVec(i)) == hotValue) then
    //                dValid(i) = N
    //            end if
    //        end do
    //        !Re-use i to find location of minimum
    //        i=minloc(dValid,1)
    //        if(dValid(i) == N) then ! No cold blocks in this batch :(
    //            GCCOLD=dVec(minloc(validPages(dVec),1))
    //        else ! Found cold block!
    //            GCCOLD=dVec(i)
    //        end if
    //    else !Replacing hot WF, no difference, so just pass everything
    //        GCCOLD = GC(rng,d,N,validPages)
    //    end if
    //end function GCCOLD
}

/* * * * * * * * *
 *   FTL BEGIN   *
 * * * * * * * * */

FTL::FTL(const unsigned int numberOfEntries) : _ftl(numberOfEntries, PhysicalAddress(-1,-1));

void FTL::write(const LogicalPageNumber lpn, const PhysicalAddress &addr){
    (*this)[lpn] = addr;
}

void FTL::invalidate(const LogicalPageNumber lpn){
    PhysicalAddress& addr (*this)[lpn];
    addr.bl = -1;
    addr.p = -1;
}

PhysicalAddress const & read(const LogicalPageNumber lpn) const{
    return (*this)[lpn];
}

PhysicalAddress& FTL::operator[](const LogicalPageNumber lpn){
    ///@TODO Introduce fail-safe return element for out-of-bounds access?
    return _ftl.at(lpn-1);
}

/* * * * * * * * *
 *    FTL END    *
 * * * * * * * * */


namespace ssd{
    /* * * * * * * * *
     *  PAGE BEGIN   *
     * * * * * * * * */

    Page::Page() : _status(PageState::ERASED), _lpn(0){}

    void Page::erase(){
        _lpn = 0;
        _status = PageState::ERASED;
    }

    void Page::invalidate(){
        _lpn = 0;
        _status = PageState::INVALID;
    }

    void Page::write(const LogicalPageNumber newLpn){
        _lpn = newLpn;
        _status = PageState::VALID;
    }

    LogicalPageNumber Page::getLPN() const{
        return _lpn;
    }

    bool Page::isValid() const{
        return _state == PageState::VALID;
    }

    bool Page::isInvalidated() const{
        return _state == PageState::INVALID;
    }

    bool Page::isErased() const{
        return _state == PageState::ERASED;
    }
    
    /* * * * * * * * *
     *   PAGE END    *
     * * * * * * * * */
    

    /* * * * * * * * *
     *  BLOCK BEGIN  *
     * * * * * * * * */
    
    Block::Block(const unsigned int b) : _numErased(b), _block(b) {}
    
    Page& Block::operator[](const PageNumber pageNr){
        return _block.at(pageNr);
    }

    Page const & Block::operator[](const PageNumber pageNr) const{
        return _block.at(pageNr);
    }

    void Block::erase(){
        for(Page p : _block){
            p.erase();
        }
    }

    void invalidate(const PageNumber pageNr){
        (*this)[pageNr].invalidate();
    }

    unsigned int Block::getNumErased(){
        return _numErased;
    }

    /* * * * * * * * *
     *   BLOCK END   *
     * * * * * * * * */

    class SSD {
        public:
            SSD(const unsigned int N, const unsigned int b, const double rho,);
            //void read(const PhysicalAddress addr) const;
            void write(const LogicalPageNumber lpn);
            ///@TODO void trim(const LogicalPageNumber lpn);
            virtual ~SSD();
            bool isWF(const BlockNumber blockNr) const;
        protected:
        virtual void erase(const BlockNumber victim);
            Page& operator[](const PhysicalAddress &addr);
            Page const & operator[](const PhysicalAddress &addr) const;
        private:
            BlockNumber _wf;
            FTL _ftl;
            std::vector<Block> _ssd;
            BlockNumber (*_gc)();




    SSD::SSD(const unsigned int N, const unsigned int b, const double rho)
     : _wf(0), _ftl(std::ceil(rho*b*N)), _ssd(N, Block(b))
    {
        const unsigned int maxNumValid = std::ceil(rho*b*N);
        //Randomly fill SSD
        for(unsigned int it = 0; it < maxNumValid; it++){
            const LogicalPageNumber lpn = it + 1; 
            bool failure = true;
            while(failure){
                const PhysicalAddress addr(randi(N), randi(b));
                if(not (this->isWF(addr.bl) or _ssd[addr].isValid())){
                    (*this)[addr].write(lpn);
                    _ftl[lpn] = addr;
                    failure = false;
                }
            }
        }
    }

    void SSD::erase(const Blocknumber n){
        _ssd.at(n);
    }

    SSD::~SSD(){

    }
}

namespace sim{


    
/*    
    subroutine SSDDWF(N,b,d,rho,r,f,maxPE,runit, rng,initrandom)
        use rng, only: rng_t, randi, rng_uniform

        integer, parameter :: hotsections=1000, testhotnesscount = 1000
        type(rng_t), intent(inout) :: rng
        integer, intent(in):: N,b,d,maxPE,runit
        real(dp), intent(in) :: rho,r,f
        logical, intent(in), optional :: initrandom
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
                    if(hotness .eq. 1) then
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
                            if(hotness .eq. 1) then
                                hotValidPages(bl)=hotValidPages(bl)-1
                                hotValidPages(WFE)=hotValidPages(WFE)+1
                            end if
                            failure=.false.
                        end if
                    else ! Write to empty logical page
                        validPages(WFE)=validPages(WFE)+1
                        FTL(lpn, 1:2) =(/ validPages(WFE), WFE/)
                        SSD(validPages(WFE),WFE)=lpn
                        if(hotness .eq. 1) then
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

    subroutine SSDDWFRuns(nruns,startrun,N,b,d,rho,r,f,maxPE, initrandom)
            use utils, only : dp
            use rng, only : rng_seed, rng_t

            integer, intent(in) :: nruns,b,N,d,maxPE,startrun
            real(dp), intent(in) :: rho,r,f
            logical, intent(in) :: initrandom

            integer :: it
            type(rng_t), dimension(1:nruns) :: rng

            !!$OMP PARALLEL DO
            do it=1,nruns
                print *, it + startrun-1
                call rng_seed(rng(it), 932117 + it + startrun -1)
                call SSDDWF(N,b,d,rho,r,f,maxPE,it+startrun-1, rng(it), initrandom)
                print *, "done ", it + startrun -1
            end do
            !!$OMP END PARALLEL DO
    end subroutine SSDDWFRuns



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



    subroutine SSDHCWFTrace(traceid,maxLBA, b,d,rho,f,maxPE,numreq,requests,runit,rng, initrandom)
        use rng, only: rng_t, randi, rng_uniform

        integer, parameter :: hotsections=1000, testhotnesscount = 1000
        integer, parameter :: numhotgcbins=12
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
        if( present(initrandom) .and. initrandom) then
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
                                hotness=1
                            else
                                hotness=0
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
        else
            !! Warmup with trace
            do while(sum(validPages) <  maxnumvalid)
                do while(all(WFindex < b))
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
                        WFindex(hotness)=WFindex(hotness)+1
                        if(p /= 0) then ! Invalidate if valid page
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
                    end if
                end do ! (WFvalid < b)

                !! GCA invocation
                failure=.true.

                do while(failure)
                    if(WFindex(0) == b) then
                        same=0
                        other=1
                    else
                        same=1
                        other=0
                    end if

                    validPages(WF)=b+1
                    victim=GC(rng,d,N,validPages)
                    validPages(WF)=count(SSD(:,WF) > 0, 1)
                    if(all(victim /= WF)) then
                        j=validPages(victim)

                        jst=WFindex(other)! Not equal to validPages(WF(other)), because incoming requests can invalidate in WFI

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
                                end if
                            end do

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
                            end if

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
                                        end if
                                    end if
                                end if !(lpn /= 0)
                            end do !(i=1,b)
                            validPages(victim)=j-k !This is done local (during GC), so this always holds true (even if requests for pages in WFI and WFE)
                            validPages(WF(other))=validPages(WF(other))+k ! Old WF(other) filled completely during loop

                            WF(other) = victim
                            blockhotness(WF(other))=other
                        end if
                    end if !(victim /= WFE,WFI)
                end do !(failure)
            end do !(sum(validPages) < maxnumvalid)

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
                    hotness=1
                else
                    hotness=0
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

            !!$OMP PARALLEL DO
            do it=1,nruns
                call rng_seed(rng(it), 932117 + it +startrun-1)
                call SSDHCWFTrace(traceid, maxLBA, b,d,rho,f,maxPE,numreq,requests,it +startrun-1,rng(it), initrandom)
                print *, "done ", it +startrun-1
            end do
            !!$OMP END PARALLEL DO
    end subroutine SSDHCWFTraceRuns


    subroutine SSDHCORACLETrace(traceid,maxLBA, b,d,rho,r,f,maxPE,numreq,requests,isHotRequest,runit,rng, initrandom)
        use rng, only: rng_t, randi, rng_uniform

        integer, parameter :: hotsections=1000, testhotnesscount = 1000
        integer, parameter :: numhotgcbins=12
        integer, parameter :: hotvalue = 1, coldvalue = 0
        type(rng_t), intent(inout) :: rng
        character(4) :: traceid
        integer, intent(in):: maxLBA,b,d,maxPE,runit, numreq
        integer, intent(in), dimension(1:numreq,1:2) ::requests
        logical, intent(in), dimension(1:numreq) :: isHotRequest
        real(dp), intent(in) :: rho,r,f
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
        integer, allocatable, dimension(:):: PE,validPages, blockhotness, pagehotness
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
        allocate(pagehotness(1:maxnumvalid))
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
        !!TODO Re-add this
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
                                hotness=hotvalue
                            else
                                hotness=coldvalue
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
        !else
        !    !! Warmup with trace
        !    do while(sum(validPages) <  maxnumvalid)
        !        do while(all(WFindex < b))
        !            reqit=reqit+1
        !            reqit=mod(reqit-1,numreqlong)+1
        !            lpn=requests(mod(reqit, numreqlong)+1,1)
        !            isWrite=requests(mod(reqit, numreqlong)+1,2) /= 0
        !            p=FTL(lpn,1)
        !            bl=FTL(lpn,2)
        !
        !            if(lpn <= maxnumhot) then
        !                hotness=1
        !            else
        !                hotness=0
        !            end if
        !            if(isWrite) then
        !                WFindex(hotness)=WFindex(hotness)+1
        !                if(p /= 0) then ! Invalidate if valid page
        !                    ! Remove old page
        !                    SSD(p,bl) = 0
        !                    validPages(bl)=validPages(bl)-1
        !                end if
        !                ! Write update to WFE
        !                FTL(lpn, 1:2) =(/ WFindex(hotness), WF(hotness)/)
        !                SSD(WFindex(hotness),WF(hotness))=lpn
        !                validPages(WF(hotness))=validPages(WF(hotness))+1
        !
        !                intW=intW+1
        !                extW=extW+1
        !            else if (p /= 0) then ! TRIM
        !                SSD(p,bl) = 0
        !                validPages(bl)=validPages(bl)-1
        !                FTL(lpn, 1:2) =(/0, 0/)
        !            end if
        !        end do ! (WFvalid < b)
        !
        !        !! GCA invocation
        !        failure=.true.
        !
        !        do while(failure)
        !            if(WFindex(0) == b) then
        !                same=0
        !                other=1
        !            else
        !                same=1
        !                other=0
        !            end if
        !
        !            validPages(WF)=b+1
        !            victim=GC(rng,d,N,validPages)
        !            validPages(WF)=count(SSD(:,WF) > 0, 1)
        !            if(all(victim /= WF)) then
        !                j=validPages(victim)
        !
        !                jst=WFindex(other)! Not equal to validPages(WF(other)), because incoming requests can invalidate in WFI
        !
        !                k=b-jst !Free in WFI
        !                victimcontent=SSD(1:b,victim)
        !
        !                !! Erase victim
        !                SSD(:,victim)=0
        !                validPages(victim)=0
        !
        !                if(blockhotness(victim) == same) then
        !                    WF(same)=victim
        !                    WFindex(same)=0
        !                    do i=1,b
        !                        lpn=victimcontent(i)
        !                        if(lpn /= 0) then
        !                            WFindex(same)=WFindex(same)+1
        !                            FTL(lpn,:)=(/WFindex(same), WF(same)/)
        !                            SSD(WFindex(same),WF(same))=lpn
        !                        end if
        !                    end do
        !
        !                    validPages(WF(same))=j
        !                    failure=.false.
        !                elseif(j <= k) then ! Cold victim, sufficient space in CWF
        !                    !Copy to CWF
        !                    do i=1,b
        !                        lpn=victimcontent(i)
        !                        if(lpn /= 0) then
        !                            WFindex(other)=WFindex(other)+1
        !                            FTL(lpn,:)=(/WFindex(other), WF(other)/)
        !                            SSD(WFindex(other),WF(other))=lpn
        !                        end if
        !                    end do
        !                    validPages(WF(other))=validPages(WF(other))+j
        !                    !No modifications for (hot)validPages necessary here
        !                    validPages(victim)=0
        !
        !                    ! HWF <- victim
        !                    WF(same) = victim
        !                    blockhotness(WF(same))=same
        !                    WFindex(same)=0
        !                    failure=.false.
        !                else ! j > k
        !                    ! Copy k of j to CWF, rest to self
        !                    kdiff=k
        !                    if(kdiff == 0) then !Turns 0 for first time
        !                        WFindex(other) = 0 ! WFIindex remains between GCA invocations, only reset here
        !                    end if
        !
        !                    do i=1,b
        !                        lpn=victimcontent(i)
        !                        if(lpn /= 0) then
        !                            WFindex(other) = WFindex(other)+1
        !                            if (kdiff == 0) then ! No space on WFI, copy back to self
        !                                SSD(WFindex(other),victim)=lpn
        !                                FTL(lpn,:)=(/WFindex(other),victim/)
        !                            else ! (kdiff > 0) !Copy to WFI
        !                                SSD(WFindex(other),WF(other))=lpn
        !                                FTL(lpn,:)=(/WFindex(other), WF(other)/)
        !
        !                                kdiff=kdiff-1
        !                                if(kdiff == 0) then !Turns 0 for first time
        !                                    WFindex(other) = 0 ! WFIindex remains between GCA invocations, only reset here
        !                                end if
        !                            end if
        !                        end if !(lpn /= 0)
        !                    end do !(i=1,b)
        !                    validPages(victim)=j-k !This is done local (during GC), so this always holds true (even if requests for pages in WFI and WFE)
        !                    validPages(WF(other))=validPages(WF(other))+k ! Old WF(other) filled completely during loop
        !
        !                    WF(other) = victim
        !                    blockhotness(WF(other))=other
        !                end if
        !            end if !(victim /= WFE,WFI)
        !        end do !(failure)
        !    end do !(sum(validPages) < maxnumvalid)
        !
        !end if

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

                if(isHotRequest(reqit)) then
                    hotness=hotvalue
                    pagehotness(lpn)=hotvalue
                else
                    hotness=coldvalue
                    pagehotness(lpn)=coldvalue
                end if ! (isHotRequest(reqit))
        
                
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
                    if(PE(victim) == currentPE .and. currentPE < maxPE) then
                        currentPE=currentPE+1

                        fairness(currentPE)=sum(PE)/dble(N*currentPE)
                        endurance(currentPE)=dble(extW)/pageCount
                        !hotpgf(currentPE)=count(FTL(1:maxnumhot,1) /= 0)
                        hotpgf(currentPE)=count(pagehotness == hotvalue)
                        hotblf(currentPE)=count(blockhotness == 1)
                        !coldpgf(currentPE)=count(FTL(maxnumhot+1:maxnumvalid,1) /= 0)
                        coldpgf(currentPE) = maxnumvalid - hotpgf(currentPE)
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
            write (WAfilename,        20)    b,d,rho,r,f,traceid,runit
            write (distfilename,      21)    b,d,rho,r,f,traceid,runit
            write (fairfilename,      22)    b,d,rho,r,f,traceid,runit
            write (endufilename,      23)    b,d,rho,r,f,traceid,runit
            write (victimfilename,    24)    b,d,rho,r,f,traceid,runit
            write (victimhotfilename, 25)    b,d,rho,r,f,traceid,runit
            write (hotvicfreqfilename, 26)   b,d,rho,r,f,traceid,runit
            write (interhotvicfilename,27)   b,d,rho,r,f,traceid,runit
            write (validfilename,      28)   b,d,rho,r,f,traceid,runit
            write (hotblffilename,     29)   b,d,rho,r,f,traceid,runit
            write (coldblffilename,    30)   b,d,rho,r,f,traceid,runit
            write (hotpgffilename,     31)   b,d,rho,r,f,traceid,runit
            write (coldpgffilename,    32)   b,d,rho,r,f,traceid,runit
        else
            write (WAfilename,         40)   b,d,rho,r,f,traceid,runit
            write (distfilename,       41)   b,d,rho,r,f,traceid,runit
            write (fairfilename,       42)   b,d,rho,r,f,traceid,runit
            write (endufilename,       43)   b,d,rho,r,f,traceid,runit
            write (victimfilename,     44)   b,d,rho,r,f,traceid,runit
            write (victimhotfilename,  45)   b,d,rho,r,f,traceid,runit
            write (hotvicfreqfilename, 46)   b,d,rho,r,f,traceid,runit
            write (interhotvicfilename,47)   b,d,rho,r,f,traceid,runit
            write (validfilename,      48)   b,d,rho,r,f,traceid,runit
            write (hotblffilename,     49)   b,d,rho,r,f,traceid,runit
            write (coldblffilename,    50)   b,d,rho,r,f,traceid,runit
            write (hotpgffilename,     51)   b,d,rho,r,f,traceid,runit
            write (coldpgffilename,    52)   b,d,rho,r,f,traceid,runit
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

        20  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-WA.',I2,'.csv')
        21  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-dist.',I2,'.csv')
        22  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-fair.',I2,'.csv')
        23  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-end.',I2,'.csv')
        24  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-victim.',I2,'.csv')
        25  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-victimh.',I2,'.csv')
        26  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-hotGCf.',I2,'.csv')
        27  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-inter.',I2,'.csv')
        28  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-valid.',I2,'.csv')
        29  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-hotblf.',I2,'.csv')
        30  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-coldblf.',I2,'.csv')
        31  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-hotpgf.',I2,'.csv')
        32  format('hcortrace-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-coldpgf.',I2,'.csv')

        40  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-WA.',I2,'.csv')
        41  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-dist.',I2,'.csv')
        42  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-fair.',I2,'.csv')
        43  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-end.',I2,'.csv')
        44  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-victim.',I2,'.csv')
        45  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-victimh.',I2,'.csv')
        46  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-hotGCf.',I2,'.csv')
        47  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-inter.',I2,'.csv')
        48  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-valid.',I2,'.csv')
        49  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-hotblf.',I2,'.csv')
        50  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-coldblf.',I2,'.csv')
        51  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-hotpgf.',I2,'.csv')
        52  format('hcortrace-empty-b',I2,'-d',I3,'-rho',F4.2,'-r',F6.4,'-f',F6.4,'-',A4,'-coldpgf.',I2,'.csv')

    end subroutine SSDHCORACLETrace

    subroutine SSDHCORACLETraceRuns(traceid,startrun,nruns,b,d,rho,r,f,maxPE,numreq, requests,isHotRequest, initrandom)
            use utils, only : dp
            use rng, only : rng_seed, rng_t

            integer, intent(in) :: nruns,b,d,maxPE, numreq, startrun
            character(4), intent(in):: traceid
            real(dp), intent(in) :: rho,r,f
            logical, intent(in) :: initrandom
            integer, dimension(1:numreq,1:2), intent(in):: requests
            logical, dimension(1:numreq), intent(in):: isHotRequest

            integer :: it, maxLBA
            type(rng_t), dimension(1:nruns) :: rng

            maxLBA=maxval(requests)
            print *, maxLBA, ceiling(dble(maxLBA)/(b*rho))


            !!$OMP PARALLEL DO
            do it=1,nruns
                call rng_seed(rng(it), 932117 + it +startrun-1)
                call SSDHCORACLETrace(traceid, maxLBA, b,d,rho,r,f,maxPE,numreq,requests,isHotRequest,&
                    it +startrun-1,rng(it), initrandom)
                print *, "done ", it +startrun-1
            end do
            !!$OMP END PARALLEL DO
    end subroutine SSDHCORACLETraceRuns

    

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
        !else
        !    !! Warmup with trace
        !    do while(sum(validPages) <  maxnumvalid)
        !        do while(all(WFindex < b))
        !            reqit=reqit+1
        !            reqit=mod(reqit-1,numreqlong)+1
        !            lpn=requests(mod(reqit, numreqlong)+1,1)
        !            isWrite=requests(mod(reqit, numreqlong)+1,2) /= 0
        !            p=FTL(lpn,1)
        !            bl=FTL(lpn,2)
        !
        !            if(lpn <= maxnumhot) then
        !                hotness=hotBlock
        !            else
        !                hotness=coldBlock
        !            end if
        !            if(isWrite) then
        !                WFindex(hotness)=WFindex(hotness)+1
        !                if(p /= 0) then ! Invalidate if valid page
        !                    ! Remove old page
        !                    SSD(p,bl) = 0
        !                    validPages(bl)=validPages(bl)-1
        !                end if
        !                ! Write update to WFE
        !                FTL(lpn, 1:2) =(/ WFindex(hotness), WF(hotness)/)
        !                SSD(WFindex(hotness),WF(hotness))=lpn
        !                validPages(WF(hotness))=validPages(WF(hotness))+1
        !
        !                intW=intW+1
        !                extW=extW+1
        !            else if (p /= 0) then ! TRIM
        !                SSD(p,bl) = 0
        !                validPages(bl)=validPages(bl)-1
        !                FTL(lpn, 1:2) =(/0, 0/)
        !            end if
        !        end do ! (WFvalid < b)
        !
        !        !! GCA invocation
        !        failure=.true.
        !
        !        do while(failure)
        !            if(WFindex(0) == b) then
        !                same=0
        !                other=1
        !            else
        !                same=1
        !                other=0
        !            end if
        !
        !            validPages(WF)=b+1
        !            victim=GC(rng,d,N,validPages)
        !            validPages(WF)=count(SSD(:,WF) > 0, 1)
        !            if(all(victim /= WF)) then
        !                j=validPages(victim)
        !
        !                jst=WFindex(other)! Not equal to validPages(WF(other)), because incoming requests can invalidate in WFI
        !
        !                k=b-jst !Free in WFI
        !                victimcontent=SSD(1:b,victim)
        !
        !                !! Erase victim
        !                SSD(:,victim)=0
        !                validPages(victim)=0
        !
        !                if(blockhotness(victim) == same) then
        !                    WF(same)=victim
        !                    WFindex(same)=0
        !                    do i=1,b
        !                        lpn=victimcontent(i)
        !                        if(lpn /= 0) then
        !                            WFindex(same)=WFindex(same)+1
        !                            FTL(lpn,:)=(/WFindex(same), WF(same)/)
        !                            SSD(WFindex(same),WF(same))=lpn
        !                        end if
        !                    end do
        !
        !                    validPages(WF(same))=j
        !                    failure=.false.
        !                elseif(j <= k) then ! Cold victim, sufficient space in CWF
        !                    !Copy to CWF
        !                    do i=1,b
        !                        lpn=victimcontent(i)
        !                        if(lpn /= 0) then
        !                            WFindex(other)=WFindex(other)+1
        !                            FTL(lpn,:)=(/WFindex(other), WF(other)/)
        !                            SSD(WFindex(other),WF(other))=lpn
        !                        end if
        !                    end do
        !                    validPages(WF(other))=validPages(WF(other))+j
        !                    !No modifications for (hot)validPages necessary here
        !                    validPages(victim)=0
        !
        !                    ! HWF <- victim
        !                    WF(same) = victim
        !                    blockhotness(WF(same))=same
        !                    WFindex(same)=0
        !                    failure=.false.
        !                else ! j > k
        !                    ! Copy k of j to CWF, rest to self
        !                    kdiff=k
        !                    if(kdiff == 0) then !Turns 0 for first time
        !                        WFindex(other) = 0 ! WFIindex remains between GCA invocations, only reset here
        !                    end if
        !
        !                    do i=1,b
        !                        lpn=victimcontent(i)
        !                        if(lpn /= 0) then
        !                            WFindex(other) = WFindex(other)+1
        !                            if (kdiff == 0) then ! No space on WFI, copy back to self
        !                                SSD(WFindex(other),victim)=lpn
        !                                FTL(lpn,:)=(/WFindex(other),victim/)
        !                            else ! (kdiff > 0) !Copy to WFI
        !                                SSD(WFindex(other),WF(other))=lpn
        !                                FTL(lpn,:)=(/WFindex(other), WF(other)/)
        !
        !                                kdiff=kdiff-1
        !                                if(kdiff == 0) then !Turns 0 for first time
        !                                    WFindex(other) = 0 ! WFIindex remains between GCA invocations, only reset here
        !                                end if
        !                            end if
        !                        end if !(lpn /= 0)
        !                    end do !(i=1,b)
        !                    validPages(victim)=j-k !This is done local (during GC), so this always holds true (even if requests for pages in WFI and WFE)
        !                    validPages(WF(other))=validPages(WF(other))+k ! Old WF(other) filled completely during loop
        !
        !                    WF(other) = victim
        !                    blockhotness(WF(other))=other
        !                end if
        !            end if !(victim /= WFE,WFI)
        !        end do !(failure)
        !    end do !(sum(validPages) < maxnumvalid)
        !
        !end if

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
                !victim=GC(rng,d,N,validPages)
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

    subroutine SSDCOLDDCHTraceRuns(traceid,startrun,nruns,b,d,rho,f,maxPE,numreq, requests, initrandom)
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
                call rng_seed(rng(it), 932117 + it +startrun-1)
                call SSDCOLDDCHTrace(traceid, maxLBA, b,d,rho,f,maxPE,numreq,requests,it +startrun-1,rng(it), initrandom)
                print *, "done ", it +startrun-1
            end do
            !!$OMP END PARALLEL DO
    end subroutine SSDCOLDDCHTraceRuns



    subroutine SSDHCWF(N,b,d,rho,r,f,maxPE,runit, rng,initrandom)
        use rng, only: rng_t, randi, rng_uniform

        integer, parameter :: hotsections=1000, numhotgcbins=11
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
                    currentPE, sumPE,tmpvictimfill,numhotgccalls,interhotvictim
        integer, dimension(1:b,1:N)::SSD
        integer, dimension(1:2):: WF
        integer, dimension(1:N):: PE,blockhotness, validPages
        integer, dimension(1:b) :: valpvict
        integer, dimension(1:2) :: maxnumhot
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
        maxnumhot(1)=floor(f*rho*pageCount)
        maxnumhot(2)=maxnumvalid-maxnumhot(1)
        maxhotbl=ceiling(f*N)

        !! Initialize
        allocate(FTL(1:maxnumvalid,1:2))

        SSD=0
        FTL=0
        WF(1)=1 !HWF
        WF(2)=2 !CWF
        if( present(initrandom) .and. initrandom) then
            do it=1, maxnumvalid
                failure=.true.
                do while(failure)
                    p=randi(rng,b)
                    if(it < maxnumhot(1)) then
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
        end if ! (present(initrandom) .and. initrandom)
        validPages = count(SSD > 0, 1)
        blockhotness(1:maxhotbl)=1
        blockhotness(maxhotbl+1:N)=2

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
                    hotness=1
                else
                    hotness=2
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
            if(validPages(WF(1)) == b) then
                same=1
                other=2
            else
                same=2
                other=1
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

    subroutine SSDHCWFRuns(nruns,startrun,N,b,d,rho,r,f,maxPE, initrandom)
            use utils, only : dp
            use rng, only : rng_seed, rng_t

            integer, intent(in) :: nruns,b,N,d,maxPE,startrun
            real(dp), intent(in) :: rho,r,f
            logical, intent(in) :: initrandom

            integer :: it
            type(rng_t), dimension(1:nruns) :: rng

            !!$OMP PARALLEL DO
            do it=1,nruns
                print *, it + startrun-1
                call rng_seed(rng(it), 932117 + it + startrun-1)
                call SSDHCWF(N,b,d,rho,r,f,maxPE,it+startrun-1, rng(it), initrandom)
                print *, "done ", it + startrun -1
            end do
            !!$OMP END PARALLEL DO
    end subroutine SSDHCWFRuns
*/
}
