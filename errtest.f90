!$Id$
! Copyright (c) 2016 Anton Shterenlikht, The University of Bristol, UK
!
! Simple tests of error functions in module errfun.

program errtest
use errfun
implicit none

! Use np points along x and y.
integer, parameter :: np = 1000
real( kind=rkind ), parameter :: xmin = -10.0_rkind,                   &
  xmax = -xmin, ymin= xmin, ymax = xmax, delta = (xmax-xmin)/(np-1)
complex( kind=rkind ) :: z(np,np), res(np,np)
real( kind=rkind ) :: x(np), y(np), err = 0.06447_rkind * eps0,  &
  re(np,np), im(np,np)
real :: time2, time1

integer :: i,j,funit


! Generate the coordinate arrays
do concurrent( i=1:np )
  x(i) = xmin + delta * (i-1)
  y(i) = ymin + delta * (i-1)
end do

!write (*,*) "delta:", delta
!write (*,*) "xmax, ymax:", x(np), y(np)

! Generate the array of z
do concurrent (i=1:np, j=1:np)
  z(i,j) = cmplx( x(i), y(j), kind=rkind ) 
end do

! Time the calculation
call cpu_time( time1 )
res = fad( z, err )
call cpu_time( time2 )
write (*,*) "CPU time, s:", time2-time1

writeout: if (.false.) then
  ! Write the data out
  re = real( res )
  im = aimag( res )
  
  open(newunit=funit, file="out-fortran", form="formatted",              &
       status="replace")
  do j=1,np
  do i=1,np
    write (funit, "(4(es25.16,tr1))") x(i), y(j), re(i,j), im(i,j)
  end do
  end do
  close(funit)

end if writeout

end program errtest
