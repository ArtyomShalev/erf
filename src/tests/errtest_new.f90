program errtest
    implicit none
    call errftest

end program errtest

    subroutine errftest()
        !!-------------------------------------------------------------------------
        !   DESCRIPTION
        !!-------------------------------------------------------------------------
        use errfun
        implicit none
        integer, parameter :: array_size = 34
        integer :: i
        real (kind = rkind) :: err = 1.0e-16, x(array_size), y(array_size), y_constant_sequence(10),                             &
                               calculation_accuracy = 1.0e-11, calc_result_real(array_size), calc_result_imag(array_size)
        complex( kind=rkind ), dimension(array_size) :: z, result_wzag, result_wpop, result_erf_zag, result_erf_pop,             &
                                                        pympmath_faddeyeva_array, pympmath_errfun_array

        logical :: test_is_passed = .true. , compare_complex


        ! Generating the x array
        x(1:10) = 6.3e-02_rkind
        x(11:20) = 6.3_rkind
        x(21:30) = 6.3e+02_rkind
        x(31) = 1.0_rkind
        x(32) = 5.5_rkind
        x(33) = 3.9e+04_rkind
        x(34) = 1.0_rkind

        ! Generating the y array
        y_constant_sequence = (/ 1.0e-20_rkind, 1.0e-14_rkind, 1.0e-12_rkind, 1.0e-10_rkind, &
        1.0e-06_rkind, 1.0e-02_rkind, 1.0e+01_rkind, 1.2e+01_rkind, 1.5e+01_rkind, 2.0e+02_rkind /)
        y(1:10) = y_constant_sequence
        y(11:20) = y_constant_sequence
        y(21:30) = y_constant_sequence
        y(31) = 1.0e-20_rkind
        y(32) = 1.0e-14_rkind
        y(33) = 1.0_rkind
        y(34) = 2.8e+04_rkind

        ! Generating the z array
        do i = 1, array_size
            z(i) = cmplx( x(i), y(i), kind=rkind )
        end do

        ! Determination of the python mpmath values array of Faddeyeva function
        pympmath_faddeyeva_array = (/   cmplx(0.9960388660702479200362 ,0.07090008726353683707177_rkind , kind = rkind),        &
        cmplx(0.9960388660702367255899_rkind ,0.07090008726353558206405_rkind , kind = rkind),          &
        cmplx(0.9960388660691284742913_rkind ,0.0709000872634113361759_rkind , kind = rkind),           &
        cmplx(0.9960388659583033444373_rkind ,0.07090008725098674736124_rkind , kind = rkind),          &
        cmplx(0.9960377466254799517377_rkind ,0.07089996176278113729699_rkind , kind = rkind),          &
        cmplx(0.9849424862549036674213_rkind ,0.06965909657459020618798_rkind , kind = rkind),          &
        cmplx(0.05613881832823899178986_rkind ,0.000350223233333299277369_rkind , kind = rkind),        &
        cmplx(0.04685295149211580280635_rkind ,0.0002442987772965738846208_rkind , kind = rkind),       &
        cmplx(0.03752895161491528271622_rkind ,0.0001569287266610624892983_rkind , kind = rkind),       &
        cmplx(0.002820912377331288953022_rkind ,8.885651855673831697366e-7_rkind , kind = rkind),       &
        cmplx(5.792460778844116016363e-18_rkind ,0.09072765968412737108689_rkind , kind = rkind),       &
        cmplx(1.536857621303168868241e-16_rkind ,0.09072765968412737108689_rkind , kind = rkind),       &
        cmplx(1.479513723737757200158e-14_rkind ,0.09072765968412737108689_rkind , kind = rkind),       &
        cmplx(1.478940284762106694756e-12_rkind ,0.09072765968412737108596_rkind , kind = rkind),       &
        cmplx(1.478934493028421039178e-8_rkind ,0.09072765968412509510392_rkind , kind = rkind),        &
        cmplx(0.0001478930389133949051505_rkind ,0.09072741516349306682764_rkind , kind = rkind),       &
        cmplx(0.04040671157393869231447_rkind ,0.02527577277549407320506_rkind , kind = rkind),         &
        cmplx(0.03684277239564807699018_rkind ,0.01923808857910919404091_rkind , kind = rkind),         &
        cmplx(0.03194834330452632123238_rkind ,0.01336797114261587799005_rkind , kind = rkind),         &
        cmplx(0.002818116555665662604749_rkind ,0.0000887684545745620215339_rkind , kind = rkind),      &
        cmplx(1.421495882573231873622e-26_rkind ,0.0008955401496757104415007_rkind , kind = rkind),     &
        cmplx(1.421495882622521424285e-20_rkind ,0.0008955401496757104415007_rkind , kind = rkind),     &
        cmplx(1.421495882597586292511e-18_rkind ,0.0008955401496757104415007_rkind , kind = rkind),     &
        cmplx(1.421495882689343887519e-16_rkind ,0.0008955401496757104414917_rkind , kind = rkind),     &
        cmplx(1.42149588248463703316e-12_rkind ,0.0008955401496748148990947_rkind , kind = rkind),      &
        cmplx(1.421495882175688556969e-8_rkind ,0.0008955401494543289092053_rkind , kind = rkind),      &
        cmplx(0.00001421137820009847167673_rkind ,0.0008953145713915760760959_rkind , kind = rkind),    &
        cmplx(0.000017051763955417068696_rkind ,0.000895215352944587476679_rkind , kind = rkind),       &
        cmplx(0.00002131035743074597724708_rkind ,0.0008950327582962093743453_rkind , kind = rkind),    &
        cmplx(0.0002582702147491469382615_rkind ,0.0008135493143556982138108_rkind , kind = rkind),     &
        cmplx(0.3678794411714423215964_rkind ,0.6071577058413937291077_rkind , kind = rkind),           &
        cmplx(7.307386729528773115628e-14_rkind ,0.1043674364367812078792_rkind , kind = rkind),        &
        cmplx(3.709333226385423722871e-10_rkind ,0.00001446639957339204167654_rkind , kind = rkind),    &
        cmplx(0.00002014962794529685482252_rkind ,7.196295685569928173655e-10_rkind , kind = rkind)     &
        /)


        result_wzag = wzag(z, err)
        result_wpop = wpop(z)


        if ( compare_complex(result_wzag, pympmath_faddeyeva_array, array_size, calculation_accuracy) .eqv. .false. ) then
                        print *,  "----------- Calculation of function wzag is FAILED --------------"
                        test_is_passed = .false.
        else
            print *,  "----------- Test of function wzag PASSED --------------"
        end if

        if ( compare_complex(result_wpop, pympmath_faddeyeva_array, array_size, calculation_accuracy) .eqv. .false. ) then
            print *,  "----------- Calculation of function wpop is FAILED --------------"
            test_is_passed = .false.
        else
            print *,  "----------- Test of function wpop PASSED --------------"
        end if


        if (test_is_passed .eqv. .false.) then
            stop 1
        end if

    end subroutine errftest

    function compare_complex(calc_values, ref_values, array_size, calculation_accuracy) result(comparison_result)

        use errfun
        implicit none
        integer :: i, array_size
        real (kind = rkind) :: calculation_accuracy
        complex (kind = rkind) :: calc_values(array_size), ref_values(array_size), calc, ref
        logical :: comparison_result

        comparison_result = .true.
        do i = 1, array_size
            calc = calc_values(i)
            ref = ref_values(i)
            if (    abs( real(calc ) - real( ref ) ) / real( ref ) > calculation_accuracy .and.     &
                    abs( aimag( calc ) - aimag( ref ) ) / aimag (ref ) > calculation_accuracy ) then
                comparison_result = .false.

            end if
        end do

    end function
