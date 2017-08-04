#ifndef SDCSIM_SIM_HXX
#define SDCSIM_SIM_HXX

#include <vector>

using BlockNumber=int;
using PageNumber=int;
using LogicalPageNumber=unsigned int;




struct PhysicalAddress{
	BlockNumber bl;
	PageNumber p;
    PhysicalAddress() : bl(-1), p(-1) {}
    PhysicalAddress(const BlockNumber blockNr, const PageNumber pageNr) : bl(blockNr), p(pageNr) {}
	PhysicalAddress(PhysicalAddress const &addr) : bl(addr.bl), p(addr.p) {}
};

class FTL{
    public:
        FTL(const unsigned int numberOfEntries);
        void write(const LogicalPageNumber lpn, const PhysicalAddress &addr);
        void invalidate(const LogicalPageNumber lpn);
        //PhysicalAddress const & read(const LogicalPageNumber lpn) const;
        PhysicalAddress& operator[](const LogicalPageNumber lpn);
    private:
        //PhysicalAddress& operator[](const LogicalPageNumber lpn);
        std::vector<PhysicalAddress> _ftl;
};



namespace ssd{

    enum class PageState : std::int8_t
    {
        ERASED = 1, INVALID = 2, VALID = 3
    };

    class Block;

    class Page{
        public:
            Page();
            ///@TODO Only for access from Blocks (friend class)
            
            ///Sets logical page number associated with this physical page
            void write(const LogicalPageNumber newLpn);
            ///Gets logical page number associated with this physical page
            LogicalPageNumber getLPN() const;

            bool isValid() const;
            bool isInvalidated() const;
            bool isErased() const;
            //PageStatus const & getStatus() const;
        private:
            void erase();
            void invalidate();
            PageStatus _state;
            LogicalPageNumber _lpn;
        friend class Block;
    };

    class Block{
        public:
            Block(const unsigned int b); 
            //Page& operator[](const PageNumber pageNr);
            Page const & operator[](const PageNumber pageNr) const;
            void erase();
            void invalidate(const PageNumber pageNr);
            //void write(const PageNumber pageNr, const LogicalPageNumber newlpn);
            ///Gets number of pages in erased state on this block.
            unsigned int getNumErased();
        private:
            unsigned int _numErased;
            std::vector<Page> _block;
    };


    class SSD {
        public:
            SSD(const unsigned int N, const unsigned int b, const double rho);
            //void read(const PhysicalAddress addr) const;
            void write(const LogicalPageNumber lpn);
            virtual void erase(const BlockNumber victim);
            bool isWF(const BlockNumber bl) const;
            virtual ~SSD();
        protected:
            Page& operator[](const PhysicalAddress &addr);
            Page const & operator[](const PhysicalAddress &addr) const;
        private:
            BlockNumber _wf;
            FTL _ftl;
            std::vector<Block> _ssd;
    };
}

#endif /* SDCSIM_SIM_HXX */