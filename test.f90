program TestCondor
    implicit none

    character(len=64) :: filename
    integer :: stat,b
    logical :: exists
    character(len=32) :: arg

    call get_command_argument(1, arg)
    read (arg, *) b
    write (filename, 21)         b
    21  format('testoutput',I2,'.txt')
    
    exists=.false.
    inquire(file=filename, EXIST=exists)
    if(exists) then
        open(UNIT=77,FILE=filename, status='old')
    else
        open(UNIT=77,FILE=filename, status='new')
    end if
    write (77, 21) b
    close(77)
end program TestCondor
