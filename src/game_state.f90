! filepath: /D:/Projects/Fortran/Fortran_Meets_SDL/src/game_state.f90
module game_state
    use game_types
    use sdl_wrapper
    use, intrinsic :: iso_c_binding
    implicit none

    type :: GameState
        type(Player) :: player
        type(Bullet), dimension(MAX_BULLETS) :: bullets
        integer :: bullet_count = 0
        integer :: screen_width = 800
        integer :: screen_height = 600
        integer(c_int64_t) :: current_time, last_time
        real(c_double) :: delta_time
    contains
        procedure :: init => init_game_state
        procedure :: handle_input => handle_game_input
        procedure :: update => update_game_state
        procedure :: fire_bullet => fire_bullet
    end type GameState

contains
    subroutine init_game_state(this, window)
        class(GameState), intent(inout) :: this
        type(c_ptr), intent(in) :: window
        integer(c_int) :: status

        status = SDL_GetWindowSize(window, this%screen_width, this%screen_height)
        call this%player%init(this%screen_width, this%screen_height)
        this%last_time = SDL_GetTicks()
    end subroutine

    subroutine handle_game_input(this, event, running)
        class(GameState), intent(inout) :: this
        type(SDL_Event), intent(in) :: event
        integer, intent(inout) :: running
        
        select case (event%type)
            case (SDL_QUIT_EVENT)
                running = 0
            case (SDL_KEYDOWN)
                print *, "Keycode: ", event%scancode
                select case (event%scancode)                   
                    case (SDL_SCANCODE_LEFT, SDL_SCANCODE_A)
                        this%player%moving_left = .true.
                    case (SDL_SCANCODE_RIGHT, SDL_SCANCODE_D)
                        this%player%moving_right = .true.
                    case (SDL_SCANCODE_SPACE)
                        call this%fire_bullet()
                end select
            case (SDL_KEYUP)
                select case (event%scancode)
                    case (SDL_SCANCODE_LEFT, SDL_SCANCODE_A)
                        this%player%moving_left = .false.
                    case (SDL_SCANCODE_RIGHT, SDL_SCANCODE_D)
                        this%player%moving_right = .false.
                end select
        end select
    end subroutine

    subroutine fire_bullet(this)
        class(GameState), intent(inout) :: this
        integer :: i

        print *, "Firing bullet!"

        ! Find first inactive bullet
        do i = 1, MAX_BULLETS
            if (.not. this%bullets(i)%active) then
                this%bullets(i)%active = .true.
                this%bullets(i)%x = this%player%x + (PLAYER_WIDTH - BULLET_WIDTH) / 2.0
                this%bullets(i)%y = this%player%y
                return
            end if
        end do
    end subroutine

    subroutine update_game_state(this)
        class(GameState), intent(inout) :: this
        integer :: i
        
        ! Update delta time
        this%current_time = SDL_GetTicks()
        this%delta_time = real(this%current_time - this%last_time, c_double) / 1000.0
        this%last_time = this%current_time

        ! Update player position
        if (this%player%moving_left .and. this%player%x .gt. 0) then
            this%player%x = this%player%x - PLAYER_SPEED * this%delta_time
        end if
        if (this%player%moving_right .and. this%player%x .lt. this%screen_width - PLAYER_WIDTH) then
            this%player%x = this%player%x + PLAYER_SPEED * this%delta_time
        end if

        ! Update bullets
        do i = 1, MAX_BULLETS
            if (this%bullets(i)%active) then
                this%bullets(i)%y = this%bullets(i)%y - BULLET_SPEED * this%delta_time
                
                ! Deactivate bullets that go off screen
                if (this%bullets(i)%y + BULLET_HEIGHT < 0) then
                    this%bullets(i)%active = .false.
                end if
            end if
        end do
    end subroutine
end module game_state
