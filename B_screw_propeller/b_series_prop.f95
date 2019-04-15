program b_series_prop
    implicit none
    integer, parameter :: N=9, M=20
    real :: D, BAR
    real :: r_R(N), cZ_DBAR(N), a_c(N), b_c(N), Ar(N), Br(N), c(N), a(N), b(N), &
            T(N), P(M), P_temp(M+1), V1(N, M), V2(N, M), V1_temp(N, M+1), &
            V2_temp(N, M+1), Yface(N, M), Yback(N, M), r(N), X(N, M), Tte(N), Tle(N)
    integer :: Z, i, j

    ! N : number of sections
    ! c : chord length
    ! D : propeller diameter
    ! BAR : blade area ratio
    ! Tte, Tle : extrapolated blade section thickness at the trailing and leading edges
    ! Z : number of blades
    ! r_R : r/R
    ! cZ_DBAR : (c/D)*(Z/BAR)
    ! c : chord length of blade section ar radius r
    ! a : distance between leading edge and generator line at r
    ! b : distance between leading edge and location of maximum thickness
    ! T : maximum thickness at r
    ! P : non-dimensional coordinate along pitch line from position of
    !     maximum thickness to leading edge (where P =1), and from position of
    !     maximum thickness to trailing edge (where P=−1)
    ! V1 : v1 table
    ! V2 : v2 table
    ! r : section position along radius
    ! X : cord-wise position

    write (*, *) "Enter propeller diameter(D), blade number(Z) and blade area ratio(BAR):"
    read (*, *) D, Z, BAR

    ! open propeller data files
    open(1, file="prop_data.csv")
    ! open input files
    open(2, file="input_files/r_R.txt")
    open(3, file="input_files/cZ_DBAR.txt")
    open(4, file="input_files/a_c.txt")
    open(5, file="input_files/b_c.txt")
    open(7, file="input_files/Ar.txt")
    open(8, file="input_files/Br.txt")
    open(9, file="input_files/v1.csv")
    open(10, file="input_files/v2.csv")

    write (*, *) "Reading input files."
    do i = 1, N
        read(2, *) r_R(i)
    end do
    do i = 1, N
        read(3, *) cZ_DBAR(i)
    end do
    do i = 1, N
        read(4, *) a_c(i)
    end do
    do i = 1, N
        read(5, *) b_c(i)
    end do
    do i = 1, N
        read(7, *) Ar(i)
    end do
    do i = 1, N
        read(8, *) Br(i)
    end do
    do i = 1, N+1
        if (i == 1) then
            read (9, *) P_temp
            P = P_temp(2:)
        else
            read (9, *) V1_temp(i-1, :)
            V1(i-1, :) = V1_temp(i-1, 2:)
        end if
    end do
    do i = 1, N+1
        if (i ==1) then
            read (10, *) P_temp  ! dummy
        else
            read (10, *) V2_temp(i-1, :)
            V2(i-1, :) = V2_temp(i-1, 2:)
        end if
    end do

    ! propeller creation loop
    write (*, *) "Entering propeller creation loop."
    do i = 1, N
        c(i) = cZ_DBAR(i) * BAR * D / Z
        a(i) = a_c(i) * c(i)
        b(i) = b_c(i) * c(i)
        T(i) = D * (Ar(i) - Br(i)*Z)  ! max thickness
        r(i) = r_R(i) * D/2.0
        Tle(i) = T(i) * 0.01
        Tte(i) = T(i) * 0.02

        do j = 1, M
            if (P(j) < 0) then
                X(i, j) = (c(i)-b(i)) * P(j)
                Yface(i, j) = V1(i, j) * (T(i) - Tte(i))
                Yback(i, j) = (V1(i, j) + V2(i, j)) * (T(i) - Tte(i)) + Tte(i)
            else
                X(i, j) = b(i) * P(j)
                Yface(i, j) = V1(i, j) * (T(i) - Tle(i))
                Yback(i, j) = (V1(i, j) + V2(i, j)) * (T(i) - Tle(i)) + Tle(i)
            end if
        end do
    end do

    ! write to csv data file
    do i = 1, N
        do j = 1, M
            write (1, *) X(i, j), ",", Yface(i, j), ",", r(i)
            write (1, *) X(i, j), ",", Yback(i, j), ",", r(i)
        end do
    end do

    write (*, *) "B-screw series propeller creation finished."

    ! close all opened files
    close(1)
    close(2)
    close(3)
    close(4)
    close(5)
    close(7)
    close(8)
    close(9)
    close(10)
end program
