! filepath: /D:/Projects/Fortran/Fortran_Meets_SDL/src/game_state.f90
module game_state
    use game_types
    use sdl_wrapper
    use, intrinsic :: iso_c_binding
    use collision_utils
    implicit none

    type :: GameState
        type(Player) :: player
        type(Bullet), dimension(MAX_BULLETS) :: bullets
        integer :: bullet_count = 0
        integer :: screen_width = 800
        integer :: screen_height = 600
        integer(c_int64_t) :: current_time, last_time
        real(c_double) :: delta_time
        type(Enemy), dimension(MAX_ENEMIES) :: enemies
        real(c_double) :: spawn_timer = 0.0
        real(c_double) :: difficulty = 1.0
        logical :: game_over = .false.
    contains
        procedure :: init => init_game_state
        procedure :: handle_input => handle_game_input
        procedure :: update => update_game_state
        procedure :: fire_bullet => fire_bullet
        procedure :: spawn_enemies => spawn_enemies
        procedure :: random_int => random_int
        procedure :: check_bullet_enemy_collisions => check_bullet_enemy_collisions
    end type GameState

contains
    subroutine init_game_state(this, window)
        class(GameState), intent(inout) :: this
        type(c_ptr), intent(in) :: window
        integer(c_int) :: status

        status = SDL_GetWindowSize(window, this%screen_width, this%screen_height)
        call this%player%init(this%screen_width, this%screen_height)
        this%last_time = SDL_GetTicks()

        ! Initialize random number generator
        call random_seed()
        
        ! Spawn initial wave
        call this%spawn_enemies()
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

    subroutine spawn_enemies(this)
        class(GameState), intent(inout) :: this
        integer :: i, count, spawn_count
        real :: x_pos
        
        ! Calculate spawn count based on difficulty
        spawn_count = this%random_int(MIN_SPAWN_COUNT, &
                                    min(MAX_SPAWN_COUNT, &
                                        MIN_SPAWN_COUNT + floor(this%difficulty)))

        ! Find inactive enemies and spawn them
        count = 0
        do i = 1, MAX_ENEMIES
            if (.not. this%enemies(i)%active) then
                call random_number(x_pos)
                this%enemies(i)%active = .true.
                this%enemies(i)%x = x_pos * (this%screen_width - ENEMY_WIDTH)
                this%enemies(i)%y = -ENEMY_HEIGHT  ! Start above screen
                
                count = count + 1
                if (count >= spawn_count) exit
            end if
        end do
        
        ! Increase difficulty
        this%difficulty = this%difficulty + DIFFICULTY_INCREASE_RATE
    end subroutine

    function random_int(this, min_val, max_val) result(rand_int)
        class(GameState), intent(in) :: this
        integer, intent(in) :: min_val, max_val
        integer :: rand_int
        real :: r
        
        call random_number(r)
        rand_int = min_val + floor(r * (max_val - min_val + 1))
    end function

    subroutine update_game_state(this)
        class(GameState), intent(inout) :: this
        integer :: i
        real(c_double) :: bullet_old_y
        
        ! Don't update if game is over
        if (this%game_over) return

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

        ! Update spawn timer
        this%spawn_timer = this%spawn_timer + this%delta_time
        if (this%spawn_timer >= SPAWN_INTERVAL) then
            this%spawn_timer = 0.0
            call this%spawn_enemies()
        end if

        ! Update enemies
        do i = 1, MAX_ENEMIES
            if (this%enemies(i)%active) then
                this%enemies(i)%y = this%enemies(i)%y + ENEMY_SPEED * this%delta_time
                
                ! Check if enemy reached bottom
                if (this%enemies(i)%y + ENEMY_HEIGHT >= this%screen_height) then
                    this%game_over = .true.
                    return
                end if
            end if
        end do

        ! Update bullets
        do i = 1, MAX_BULLETS
            if (this%bullets(i)%active) then
                ! Store old position for collision detection
                bullet_old_y = this%bullets(i)%y
                
                ! Update bullet position
                this%bullets(i)%y = this%bullets(i)%y - BULLET_SPEED * this%delta_time
                
                ! Deactivate bullets that go off screen
                if (this%bullets(i)%y + BULLET_HEIGHT < 0) then
                    this%bullets(i)%active = .false.
                end if
            end if
        end do

        ! Check for collisions
        call check_bullet_enemy_collisions(this)
    end subroutine

    subroutine check_bullet_enemy_collisions(this)
        class(GameState), intent(inout) :: this
        integer :: i, j
        type(Rectangle) :: enemy_rect
        real(c_double) :: bullet_old_y

        do i = 1, MAX_BULLETS
            if (this%bullets(i)%active) then
                ! Store bullet's previous position for raycast
                bullet_old_y = this%bullets(i)%y + BULLET_HEIGHT
                
                do j = 1, MAX_ENEMIES
                    if (this%enemies(j)%active) then
                        ! Set up enemy rectangle
                        enemy_rect%x = this%enemies(j)%x
                        enemy_rect%y = this%enemies(j)%y
                        enemy_rect%w = ENEMY_WIDTH
                        enemy_rect%h = ENEMY_HEIGHT

                        ! Check for collision using raycast
                        if (line_rect_intersection( &
                            this%bullets(i)%x + BULLET_WIDTH/2, bullet_old_y, &
                            this%bullets(i)%x + BULLET_WIDTH/2, this%bullets(i)%y, &
                            enemy_rect)) then
                            ! Collision detected - deactivate both bullet and enemy
                            this%bullets(i)%active = .false.
                            this%enemies(j)%active = .false.
                            exit  ! Bullet can only hit one enemy
                        end if
                    end if
                end do
            end if
        end do
    end subroutine
end module game_state
