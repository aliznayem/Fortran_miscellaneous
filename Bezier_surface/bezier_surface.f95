function fact(n) result(f)
    implicit none
    integer, intent(in) :: n
    integer :: i, f

    f = 1
    do i = 1, n
        f = f*i
    end do
end function

program bezier_surface
    implicit none
    real :: Qx, Qy, Qz, u, w, du, dw
    integer :: m1, n1, m, n, i, j, Nu, Nw, fact
    real, dimension (:,:), allocatable :: Bx, By, Bz

    ! m : row number
    ! n : column number
    ! Nu : number of spacing x direction
    ! Nw : number of spacing z direction

    write (*, *) "Enter base surface matrix dimension m and n:"
    read (*, *) m, n
    write (*, *) "Enter surface resolution Nu and Nw:"
    read (*, *) Nu, Nw

    allocate(Bx(m, n))
    allocate(By(m, n))
    allocate(Bz(m, n))

    ! x spacing
    du = 1.0/Nu
    ! z spacing
    dw = 1.0/Nw

    ! open output file
    open(10, file="data.csv")
    ! open input files
    open(20, file="Bx.txt")
    open(30, file="By.txt")
    open(40, file="Bz.txt")

    ! read Bezier base surface matrix
    do i = 1, m
        read (20, *) Bx(i, :)
    end do
    do i = 1, m
        read (30, *) By(i, :)
    end do
    do i = 1, m
        read (40, *) Bz(i, :)
    end do

    ! Bezier surface creation loop
    write (*, *) "Entering Bezier surface creation loop."
    do u = 0, 1, du
        do w = 0, 1, dw
            Qx = 0
            do i = 1, n
                do j = 1, m
                    Qx = Qx + Bx(i, j) * (fact(n-1)/(fact(i-1)*fact(n-i))) * &
                    u**(i-1) * (1-u)**(n-i) * (fact(m-1)/(fact(j-1)*fact(m-j))) * w**(j-1) * (1-w)**(m-j)
                end do
            end do

            Qy = 0
            do i = 1, n
                do j = 1, m
                    Qy = Qy + By(i, j) * (fact(n-1)/(fact(i-1)*fact(n-i))) * &
                    u**(i-1) * (1-u)**(n-i) * (fact(m-1)/(fact(j-1)*fact(m-j))) * w**(j-1) * (1-w)**(m-j)
                end do
            end do

            Qz = 0
            do i = 1, n
                do j = 1, m
                    Qz = Qz + Bz(i, j) * (fact(n-1)/(fact(i-1)*fact(n-i))) * &
                    u**(i-1) * (1-u)**(n-i) * (fact(m-1)/(fact(j-1)*fact(m-j))) * w**(j-1) * (1-w)**(m-j)
                end do
            end do
            write (10, *) Qx, ",", Qy, ",", Qz
        end do
    end do
    write (*, *) "Bezier surface creation finished."

    ! close all opened files
    close(10)
    close(20)
    close(30)
    close(40)
end program

