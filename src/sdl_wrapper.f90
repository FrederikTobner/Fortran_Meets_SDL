! filepath: /D:/Projects/Fortran/Fortran_Meets_SDL/sdl_wrapper.f90
module sdl_wrapper
    use, intrinsic :: iso_c_binding
    implicit none

    ! SDL Constants
    integer(c_int), parameter :: SDL_INIT_VIDEO = int(z'00000020')
    integer(c_int), parameter :: SDL_QUIT_EVENT = int(z'100')

    ! SDL Types
    type, bind(C) :: SDL_Event
        integer(c_int) :: type
        character(c_char) :: padding(124)
    end type

    ! SDL Function interfaces
    interface
        function SDL_Init(flags) bind(C, name='SDL_Init')
            import :: c_int
            integer(c_int), value :: flags
            integer(c_int) :: SDL_Init
        end function

        function SDL_CreateWindow(title, x, y, w, h) bind(C, name='SDL_CreateWindow')
            import :: c_ptr, c_char, c_int
            character(kind=c_char), dimension(*) :: title
            integer(c_int), value :: x, y, w, h
            type(c_ptr) :: SDL_CreateWindow
        end function

        function SDL_PollEvent(event) bind(C, name='SDL_PollEvent')
            import :: c_int, SDL_Event
            type(SDL_Event) :: event
            integer(c_int) :: SDL_PollEvent
        end function

        subroutine SDL_Delay(ms) bind(C, name='SDL_Delay')
            import :: c_int
            integer(c_int), value :: ms
        end subroutine

        subroutine SDL_DestroyWindow(window) bind(C, name='SDL_DestroyWindow')
            import :: c_ptr
            type(c_ptr), value :: window
        end subroutine

        subroutine SDL_Quit() bind(C, name='SDL_Quit')
        end subroutine
    end interface

end module sdl_wrapper