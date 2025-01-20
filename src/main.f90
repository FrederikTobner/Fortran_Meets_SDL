! filepath: /D:/Projects/Fortran/Fortran_Meets_SDL/main.f90
program space_invaders
    use sdl_wrapper
    use, intrinsic :: iso_c_binding
    implicit none

    ! Game constants
    integer, parameter :: PLAYER_WIDTH = 100
    integer, parameter :: PLAYER_HEIGHT = 20
    real, parameter :: PLAYER_SPEED = 0.5
    integer, parameter :: BULLET_SIZE = 10

    ! Player type
    type :: Player
        real :: x
        real :: y
        logical :: moving_left
        logical :: moving_right
    end type Player

    ! Ball type
    type :: Bullet
        real :: x
        real :: y
    end type Bullet

    ! Local variables
    type(c_ptr) :: window, renderer
    type(SDL_Event) :: event
    integer(c_int) :: init_status, running
    character(len=*), parameter :: title = "Space Invaders"//c_null_char
    type(Player) :: player_obj
    type(SDL_FRect) :: player_rect
    integer :: screen_width = 800
    integer :: screen_height = 600

    ! Initialize SDL
    init_status = SDL_Init(SDL_INIT_VIDEO)
    if (init_status < 0) then
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

    init_status = SDL_GetWindowSize(window, screen_width, SCREEN_HEIGHT)
    print *, "Window size: ", screen_width, SCREEN_HEIGHT

    ! Initialize player
    player_obj%x = (screen_width - PLAYER_WIDTH) / 2.0
    player_obj%y = screen_height - PLAYER_HEIGHT - 10.0
    player_obj%moving_left = .false.
    player_obj%moving_right = .false.

    ! Main game loop
    running = 1
    do while (running == 1)
        ! Handle events
        do while (SDL_PollEvent(event) /= 0)
            select case (event%type)
                case (SDL_QUIT_EVENT)
                    running = 0
                case (SDL_KEYDOWN)
                    print *, "Key pressed + ", event%scancode
                    select case (event%scancode)
                        case (SDL_SCANCODE_LEFT, SDL_SCANCODE_A)
                            player_obj%moving_left = .true.
                        case (SDL_SCANCODE_RIGHT, SDL_SCANCODE_D)
                            player_obj%moving_right = .true.
                    end select
                case (SDL_KEYUP)
                    print *, "Key released + ", event%scancode
                    select case (event%scancode)
                        case (SDL_SCANCODE_LEFT, SDL_SCANCODE_A)
                            player_obj%moving_left = .false.
                        case (SDL_SCANCODE_RIGHT, SDL_SCANCODE_D)
                            player_obj%moving_right = .false.
                    end select
            end select
        end do

        ! Update player position
        if (player_obj%moving_left .and. player_obj%x > 0) then
            print *, "Moving left"
            player_obj%x = player_obj%x - PLAYER_SPEED
        end if
        if (player_obj%moving_right .and. player_obj%x < screen_width - PLAYER_WIDTH) then
            print *, "Moving right"
            player_obj%x = player_obj%x + PLAYER_SPEED
        end if

        ! Clear screen
        init_status = SDL_SetRenderDrawColor(renderer, int(z'00', c_int8_t), &
                                                     int(z'00', c_int8_t), &
                                                     int(z'00', c_int8_t), &
                                                     int(z'FF', c_int8_t))
        call SDL_RenderClear(renderer)

        ! Draw player
        init_status = SDL_SetRenderDrawColor(renderer, int(z'FF', c_int8_t), &
                                                     int(z'FF', c_int8_t), &
                                                     int(z'FF', c_int8_t), &
                                                     int(z'FF', c_int8_t))
        player_rect%x = player_obj%x
        player_rect%y = player_obj%y
        player_rect%w = PLAYER_WIDTH
        player_rect%h = PLAYER_HEIGHT
        init_status = SDL_RenderFillRect(renderer, c_loc_rect(player_rect))

        ! Present render
        call SDL_RenderPresent(renderer)
    end do

    ! Cleanup
    call SDL_DestroyRenderer(renderer)
    call SDL_DestroyWindow(window)
    call SDL_Quit()

end program space_invaders