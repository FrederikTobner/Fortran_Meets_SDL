! filepath: /D:/Projects/Fortran/Fortran_Meets_SDL/src/game_types.f90
module game_types
    use, intrinsic :: iso_c_binding
    implicit none

    ! Game constants
    integer, parameter :: PLAYER_WIDTH = 50
    integer, parameter :: PLAYER_HEIGHT = 50
    real(c_double), parameter :: PLAYER_SPEED = 600.0
    integer, parameter :: BULLET_SIZE = 10

    ! Player type
    type :: Player
        real(c_double) :: x
        real(c_double) :: y
        logical :: moving_left
        logical :: moving_right
    contains
        procedure :: init => init_player
    end type Player

    ! Bullet type
    type :: Bullet
        real(c_double) :: x
        real(c_double) :: y
    end type Bullet

contains
    subroutine init_player(this, screen_width, screen_height)
        class(Player), intent(inout) :: this
        integer, intent(in) :: screen_width, screen_height
        
        this%x = (screen_width - PLAYER_WIDTH) / 2.0
        this%y = screen_height - PLAYER_HEIGHT - 10.0
        this%moving_left = .false.
        this%moving_right = .false.
    end subroutine
end module game_types