module hello_interface
    use, intrinsic :: iso_c_binding
    implicit none

    interface
        subroutine hello() bind(C, name="hello")
            ! Declare the procedure interface for the C function
        end subroutine hello
    end interface
end module hello_interface

program main
    use hello_interface
    implicit none

    ! Call the C function
    call hello()

end program main
