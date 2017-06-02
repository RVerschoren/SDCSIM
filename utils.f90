module utils
    implicit none
    integer, parameter:: dp=kind(0.0d0)

    private
    public dp, PrintToFile, PrintReal, Print2ColReal, PrintInteger!, PrintDoubleToFile

    contains

    !subroutine PrintDoubleToFile(filename,doubleValue)
    !    real(dp), intent(in) :: doubleValue
    !    character(len=64), intent(in) :: filename
    !
    !    logical :: exists
    !    integer :: i
    !
    !    inquire(file=filename, EXIST=exists)
    !    if(exists) then
    !        open(UNIT=77,FILE=filename, status='old')
    !    else
    !        open(UNIT=77,FILE=filename, status='new')
    !    end if
    !
    !    write (77, 21) i,doubleValue
    !
    !    close(77)
    !
    !21  format(I8,',', 1X, F35.10)
    !end subroutine PrintDoubleToFile

    subroutine PrintToFile(filename,values,L,U)
        real(dp), dimension(:), intent(in) :: values
        integer, intent(in) :: L,U
        character(len=64), intent(in) :: filename
        real(dp) :: realval

        logical :: exists
        integer :: i

        exists=.false.
        inquire(file=filename, EXIST=exists)
        if(exists) then
            open(UNIT=77,FILE=filename, status='old')
        else
            open(UNIT=77,FILE=filename, status='new')
        end if

        i=0
        realval=0.0_dp
        do i=L,U
            realval=values(i-L+1)
            write (77, 21) i,realval
        end do
        close(77)

    21  format(I8,',', 1X, F20.10)
    end subroutine PrintToFile

    subroutine PrintReal(filename,values,L,U)
        real(dp), dimension(:), intent(in) :: values
        integer, intent(in) :: L,U
        character(len=64), intent(in) :: filename

        real(dp) :: realval
        logical :: exists
        integer :: i

        exists=.false.
        inquire(file=filename, EXIST=exists)
        if(exists) then
            open(UNIT=77,FILE=filename, status='old')
        else
            open(UNIT=77,FILE=filename, status='new')
        end if


        i=0
        realval=0.0_dp
        do i=L,U
            realval=values(i-L+1)
            write (77, 21) i,realval
        end do
        close(77)

    21  format(I8,',', 1X, F65.10)
    end subroutine PrintReal

    subroutine Print2ColReal(filename,values1,values2, L,U)
        real(dp), dimension(:), intent(in) :: values1, values2
        integer, intent(in) :: L, U
        character(len=64), intent(in) :: filename

        real(dp) :: realval1, realval2
        logical :: exists
        integer :: i

        exists=.false.
        inquire(file=filename, EXIST=exists)
        if(exists) then
            open(UNIT=77,FILE=filename, status='old')
        else
            open(UNIT=77,FILE=filename, status='new')
        end if


        i=0
        realval1=0.0_dp
        realval2=0.0_dp
        do i=L,U
            realval1=values1(i-L+1)
            realval2=values2(i-L+1)
            write (77, 21) realval1,realval2
        end do
        close(77)

    21  format(F65.10, ',', 1X, F65.10)
    end subroutine Print2ColReal


    subroutine PrintInteger(filename,values,L,U)
        integer, dimension(:), intent(in) :: values
        integer, intent(in) :: L,U
        character(len=64), intent(in) :: filename

        logical :: exists
        integer :: i

        inquire(file=filename, EXIST=exists)
        if(exists) then
            open(UNIT=77,FILE=filename, status='old')
        else
            open(UNIT=77,FILE=filename, status='new')
        end if

        do i=L,U
            write (77, 21) i,values(i-L+1)
        end do
        close(77)

    21  format(I8,',',I20)
    end subroutine PrintInteger
end module utils

module rng !Based on example at http://jblevins.org/log/openmp
    implicit none

    private
    public :: rng_t, rng_seed, rng_uniform, randi, randid

    ! Dimension of the state
    integer, parameter :: ns = 4

    ! Default seed vector
    integer, parameter, dimension(ns) :: default_seed &
        = (/ 521288629, 362436069, 16163801, 1131199299 /)

    ! A data type for storing the state of the RNG
    type :: rng_t
        integer, dimension(ns) :: state = default_seed
    end type rng_t

    contains

        ! Seeds the RNG using a single integer and a default seed vector.
        subroutine rng_seed(self, seed)
            type(rng_t), intent(inout) :: self
            integer, intent(in) :: seed

            self%state(1) = seed
            self%state(2:ns) = default_seed(2:ns)
        end subroutine rng_seed

        ! Draws a uniform real number on [0,1].
        function rng_uniform(self) result(u)
            use utils, only : dp

            type(rng_t), intent(inout) :: self
            real(dp) :: u
            integer :: imz

            imz = self%state(1) - self%state(3)

            if (imz < 0) imz = imz + 2147483579

            self%state(1) = self%state(2)
            self%state(2) = self%state(3)
            self%state(3) = imz
            self%state(4) = 69069 * self%state(4) + 1013904243
            imz = imz + self%state(4)
            u = 0.5d0 + 0.23283064d-9 * imz
        end function rng_uniform

        function randi(self, upperlimit) result(i)
            type(rng_t), intent(inout) :: self
            integer, intent(in) :: upperlimit
            integer :: i

            i=floor(dble(upperlimit)*rng_uniform(self))+1
        end function randi

        subroutine randid(self, upperlimit, vec)
            type(rng_t), intent(inout) :: self
            integer, intent(in) :: upperlimit
            integer, dimension(:), intent(out) :: vec
            integer :: it

            do it=lbound(vec,1), ubound(vec,1)
                vec(it)=randi(self,upperlimit)
            end do
        end subroutine randid

end module rng
