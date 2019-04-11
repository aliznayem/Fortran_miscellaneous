program wigley_hull
    implicit none
    real :: L, B, T, a, X, Y, Z, Cb, dX, dZ
    integer :: NX, NZ, iX, iZ

    ! L : length
    ! B : breadth
    ! a : hull form parameter; must be in range (-1, 1)
    ! X : x-coordinate
    ! Y : y-coordinate
    ! Z : z-coordinate
    ! Cb : block coefficient
    ! dX : x-spacing
    ! dZ : z-spacing
    ! NX : number of x-spacing (length)
    ! NZ : number of z-spacing (half breadth)

    write (*, *) "Enter length(L), Breadth(B) and Draft(T):"
    read (*, *) L, B, T
    write (*, *) "Enter hull form parameter; must be in range (-1, 1):"
    read (*, *) a
    write (*, *) "Enter NX and NZ:"
    read (*, *) NX, NZ

    ! inputs
    !L = 80
    !B = 12
    !T = 3
    !a = 0.7
    !NX = 100
    !NZ = 10
    dX = L/NX
    dZ = T/NZ

    Cb = (4.0/9.0) * (1.0 + a/5.0)
    write (*, *) "Block coefficient for corresponding hull form parameter: ", Cb

    ! open data file
    open(1, file='data.csv')

    write (*, *) "Entering Wigley hull creation loop."
    ! hull creation loop
    do iX = 0, NX
        X = iX * dX
        ! positive half
        do iZ = 0, NZ
            Z = iZ * dZ
            ! Wigley hull function
            Y = (B/2.0) * ((Z/T) * (2-Z/T) * (4*X/L) * (1-X/L) * (1 + a*(1-(2*X/L))**2))
            write (1, *) X, ",", Y, ",", Z
        end do
        ! negative half
        do iZ = 0, NZ
            Z = iZ * dZ
            Y = -(B/2.0) * ((Z/T) * (2-Z/T) * (4*X/L) * (1-X/L) * (1 + a*(1-(2*X/L))**2))
            write (1, *) X, ",", Y, ",", Z
        end do
    end do

    write (*, *) "Wigley hull creation finished."

    ! close data file
    close(1)
end program
