!> @file game_types.f90
!> @brief Game types module
!> @details Provides types for the game objects

!> @module game_types
!> @brief Module for game types
module game_types
    use, intrinsic :: iso_c_binding
    implicit none

    ! Game constants
    integer, parameter :: PLAYER_WIDTH = 50
    integer, parameter :: PLAYER_HEIGHT = 50
    real(c_double), parameter :: PLAYER_SPEED = 600.0
    integer, parameter :: BULLET_SIZE = 10
    integer, parameter :: BULLET_WIDTH = 4
    integer, parameter :: BULLET_HEIGHT = 10
    real(c_double), parameter :: BULLET_SPEED = 500.0
    integer, parameter :: MAX_BULLETS = 100

    ! Enemy constants
    integer, parameter :: ENEMY_WIDTH = 40
    integer, parameter :: ENEMY_HEIGHT = 40
    real(c_double), parameter :: ENEMY_SPEED = 100.0
    integer, parameter :: MAX_ENEMIES = 100
    integer, parameter :: MIN_SPAWN_COUNT = 3
    integer, parameter :: MAX_SPAWN_COUNT = 8
    real(c_double), parameter :: SPAWN_INTERVAL = 3.0  ! Seconds between spawns
    real(c_double), parameter :: DIFFICULTY_INCREASE_RATE = 0.1  ! Increase per spawn

 ! Score constants
    integer, parameter :: ENEMY_SCORE = 100     ! Points for destroying an enemy
    integer, parameter :: SCORE_MULTIPLIER = 10  ! Score multiplier based on difficulty


    !> @brief Player type
    !> @details Represents the player in the game
    type :: Player
        real(c_double) :: x
        real(c_double) :: y
        logical :: moving_left
        logical :: moving_right
    contains
        procedure :: init => init_player
    end type Player

    !> @brief Bullet type
    !> @details Represents a bullet in the game
    type :: Bullet
        real(c_double) :: x
        real(c_double) :: y
        logical :: active = .false.
    end type Bullet

    !> @brief Enemy type
    !> @details Represents an enemy in the game
    type :: Enemy
        real(c_double) :: x
        real(c_double) :: y
        logical :: active = .false.
    end type Enemy

contains
    !> @brief Initialize the player
    !> @details Initializes the player at the center of the screen
    !> @param this The player to initialize
    !> @param screen_width The width of the screen
    !> @param screen_height The height of the screen
    subroutine init_player(this, screen_width, screen_height)
        class(Player), intent(inout) :: this
        integer, intent(in) :: screen_width, screen_height
        
        this%x = (screen_width - PLAYER_WIDTH) / 2.0
        this%y = screen_height - PLAYER_HEIGHT - 10.0
        this%moving_left = .false.
        this%moving_right = .false.
    end subroutine
end module game_types