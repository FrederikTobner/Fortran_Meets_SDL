! filepath: /D:/Projects/Fortran/Fortran_Meets_SDL/main.f90
program fortran_meets_sdl
    use sdl_wrapper
    use, intrinsic :: iso_c_binding
    implicit none

    ! Local variables
    type(c_ptr) :: window, renderer
    type(SDL_Event) :: event
    integer(c_int) :: init_status, running
    character(len=*), parameter :: title = "Fortran Meets SDL"//c_null_char
    type(SDL_FRect) :: rectangle = SDL_FRect(750.0, 400.0, 100.0, 100.0)

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
        print *, "Window creation failed"//' '//get_sdl_error()
        call SDL_Quit()
        stop
    end if

    print *, "Window created"
    
    ! Create renderer
    renderer = SDL_CreateRenderer(window, C_NULL_PTR)
    if (.not. c_associated(renderer)) then
        print *, "Renderer creation failed"//' '//get_sdl_error()
        call SDL_DestroyWindow(window)
        call SDL_Quit()
        stop
    end if

    ! Set draw color (red)
    init_status = SDL_SetRenderDrawColor(renderer, int(z'FF', c_int8_t), &
                                                 int(z'FF', c_int8_t), &
                                                 int(z'FF', c_int8_t), &
                                                 int(z'FF', c_int8_t))

    print *, "Draw color set"                                             
    
    ! Main loop
    running = 1
    do while (running .eq. 1)
        do while (SDL_PollEvent(event) .ne. 0)
            if (event%type .eq. SDL_QUIT_EVENT) then
                running = 0
                exit
            end if
        end do

        ! Clear screen with current draw color
        call SDL_RenderClear(renderer)
        ! Draw a rectangle
        init_status =  SDL_SetRenderDrawColor(renderer, int(z'00', c_int8_t), &
                                                 int(z'00', c_int8_t), &
                                                 int(z'00', c_int8_t), &
                                                 int(z'00', c_int8_t))
        init_status =  SDL_RenderFillRect(renderer, c_loc_rect(rectangle))
        init_status =  SDL_SetRenderDrawColor(renderer, int(z'FF', c_int8_t), &
                                                 int(z'FF', c_int8_t), &
                                                 int(z'FF', c_int8_t), &
                                                 int(z'FF', c_int8_t))
        ! Present the rendered frame
        call SDL_RenderPresent(renderer)
    end do

    ! Cleanup
    call SDL_DestroyRenderer(renderer)
    call SDL_DestroyWindow(window)
    call SDL_Quit()

end program fortran_meets_sdl