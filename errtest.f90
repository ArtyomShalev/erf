program errtest
use errfun
implicit none

complex( kind=rkind ) :: z
real( kind=rkind ) :: err = eps0

z = cmplx( -0.0_rkind, 1.0_rkind, kind=rkind )
write (*,*) fad( z, err )

z = cmplx( 6.3_rkind, 1.0e-20_rkind, kind=rkind )
write (*,*) "z, w(z):", z, fad( z, err )
write (*,*) "z, erf_cmplx(z):", z, erf_cmplx( z, err )

end program errtest
