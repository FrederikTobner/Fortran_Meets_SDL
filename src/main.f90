! filepath: /D:/Projects/Fortran/Fortran_Meets_SDL/main.f90
program fortran_meets_sdl
    use sdl_wrapper
    use, intrinsic :: iso_c_binding
    implicit none

    ! Local variables
    type(c_ptr) :: window
    type(SDL_Event) :: event
    integer(c_int) :: init_status, running
    character(len=*), parameter :: title = "Fortran Meets SDL"//c_null_char

    ! Initialize SDL
    init_status = SDL_Init(SDL_INIT_VIDEO)
    if (init_status .lt. 0) then
        print *, "SDL initialization failed"
        stop
    end if

    ! Create window
    window = SDL_CreateWindow(title, &
                            1600, 900, &
                            1600, 900)
    if (.not. c_associated(window)) then
        print *, "Window creation failed"
        call SDL_Quit()
        stop
    end if

    ! Main loop
    running = 1
    do while (running == 1)
        do while (SDL_PollEvent(event) .ne. 0)
            write(*, '(Z3)') event%type
            if (event%type == SDL_QUIT_EVENT) then
                print *, "Quit event received"
                running = 0
                exit
            end if
        end do
    end do

    ! Cleanup
    call SDL_DestroyWindow(window)
    call SDL_Quit()

end program fortran_meets_sdl