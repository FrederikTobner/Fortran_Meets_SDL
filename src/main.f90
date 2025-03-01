!> @file main.f90
!> @brief Main program file for Space Invaders game
!> @details This file contains the main program logic for the Space Invaders game

!> @program space_invaders
!> @brief Main program for Space Invaders game
program space_invaders
    use sdl_wrapper
    use, intrinsic :: iso_c_binding
    use game_renderer
    use game_types
    use game_state
    implicit none

    ! Local variables
    type(c_ptr) :: window, renderer
    type(SDL_Event) :: event
    integer(c_int) :: init_status, running
    character(len=*), parameter :: title = "Space Invaders"//c_null_char
    type(GameState) :: game

    ! Initialize SDL
    init_status = SDL_Init(SDL_INIT_VIDEO)
    if (init_status .lt. 0) then
        print *, "SDL initialization failed"
        stop
    end if

    ! Create window and renderer
    window = SDL_CreateWindow(title, &
    0, 0, &
    SDL_WINDOW_FULLSCREEN) 
    if (.not. c_associated(window)) then
        print *, "Window creation failed"
        call SDL_Quit()
        stop
    end if

    renderer = SDL_CreateRenderer(window, C_NULL_PTR)
    if (.not. c_associated(renderer)) then
        print *, "Renderer creation failed"
        call SDL_DestroyWindow(window)
        call SDL_Quit()
        stop
    end if

    ! Initialize game state
    call game%init(window)

    ! Main game loop
    running = 1
    do while (running .eq. 1)
        ! Handle events
        do while (SDL_PollEvent(event) .ne. 0)
            call game%handle_input(event, running)
        end do

        ! Update game state
        call game%update()

        ! Render game
        call render_game(renderer, game)
        
        ! Check for game over
        if (game%game_over) then
            call game%display_final_score()
            running = 0
        end if
    end do

    ! Cleanup
    call SDL_DestroyRenderer(renderer)
    call SDL_DestroyWindow(window)
    call SDL_Quit()

end program space_invaders
