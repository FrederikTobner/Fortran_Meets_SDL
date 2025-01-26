! filepath: /D:/Projects/Fortran/Fortran_Meets_SDL/src/game_renderer.f90
module game_renderer
    use game_types
    use game_state
    use sdl_wrapper
    use, intrinsic :: iso_c_binding
    implicit none

contains
    subroutine render_game(renderer, game)
        type(c_ptr), intent(in) :: renderer
        type(GameState), intent(in) :: game
        type(SDL_FRect) :: player_rect, bullet_rect
        integer(c_int) :: status
        integer :: i

        ! Clear screen
        status = SDL_SetRenderDrawColor(renderer, int(z'00', c_int8_t), &
                                                int(z'00', c_int8_t), &
                                                int(z'00', c_int8_t), &
                                                int(z'FF', c_int8_t))
        call SDL_RenderClear(renderer)

        ! Draw player
        status = SDL_SetRenderDrawColor(renderer, int(z'00', c_int8_t), &
                                                int(z'FF', c_int8_t), &
                                                int(z'00', c_int8_t), &
                                                int(z'FF', c_int8_t))
        player_rect%x = real(game%player%x)
        player_rect%y = real(game%player%y)
        player_rect%w = PLAYER_WIDTH
        player_rect%h = PLAYER_HEIGHT
        status = SDL_RenderFillRect(renderer, c_loc_rect(player_rect))

        ! Draw bullets
        status = SDL_SetRenderDrawColor(renderer, int(z'00', c_int8_t), &
                                                int(z'FF', c_int8_t), &
                                                int(z'FF', c_int8_t), &
                                                int(z'FF', c_int8_t))
        do i = 1, MAX_BULLETS
            if (game%bullets(i)%active) then
                bullet_rect%x = real(game%bullets(i)%x)
                bullet_rect%y = real(game%bullets(i)%y)
                bullet_rect%w = BULLET_WIDTH
                bullet_rect%h = BULLET_HEIGHT
                status = SDL_RenderFillRect(renderer, c_loc_rect(bullet_rect))
            end if
        end do

        call SDL_RenderPresent(renderer)
    end subroutine
end module game_renderer
