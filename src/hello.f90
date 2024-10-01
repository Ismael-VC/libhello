module hello_interface
    use, intrinsic :: iso_c_binding
    implicit none

    interface
        subroutine hello() bind(C, name="hello")
        end subroutine hello
    end interface
end module hello_interface

program main
    use hello_interface
    implicit none

    call hello()

end program main
