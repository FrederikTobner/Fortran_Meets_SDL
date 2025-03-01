!> @file sdl_wrapper.f90
!> @brief SDL3 Fortran wrapper module
!> @details Provides Fortran bindings for SDL3 library functions

!> @module sdl_wrapper
!> @brief Wrapper module for SDL3 functionality
module sdl_wrapper
    use, intrinsic :: iso_c_binding
    implicit none

    ! SDL Constants
    integer(c_int), parameter :: SDL_INIT_VIDEO = int(z'00000020')
    integer(c_int), parameter :: SDL_WINDOW_FULLSCREEN = int(z'01')
    
    ! SDL Events
    integer(c_int), parameter :: SDL_QUIT_EVENT = int(z'100')
    integer(c_int), parameter :: SDL_KEYDOWN = int(z'300')
    integer(c_int), parameter :: SDL_KEYUP = int(z'301')

    ! SDL Renderer
    integer(c_int), parameter :: SDL_RENDERER_ACCELERATED = int(z'02')
    
    ! SDL Keycodes
    integer(c_int), parameter :: SDL_SCANCODE_LEFT = int(z'40000050')
    integer(c_int), parameter :: SDL_SCANCODE_A = int(z'61')
    integer(c_int), parameter :: SDL_SCANCODE_RIGHT = int(z'4000004F')
    integer(c_int), parameter :: SDL_SCANCODE_D = int(z'64')
    integer(c_int), parameter :: SDL_SCANCODE_SPACE = int(z'20')


    ! SDL Types

    !> @brief SDL_Event is a union of all event types
    type, bind(C) :: SDL_Event
        integer(c_int) :: type
        integer(c_int) :: timestamp
        integer(c_int) :: windowID
        integer(c_int) :: which
        integer(c_int) :: state  ! pressed/released
        integer(c_int) :: repeat ! non-zero if this is a repeat
        integer(c_int) :: padding
        integer(c_int) :: scancode
        integer(c_int) :: keycode
        integer(c_int) :: padding2
        character(kind=c_char) :: padding3(96)
    end type

    !> @brief SDL_FRect is a float version of SDL_Rect
    type, bind(C) :: SDL_FRect ! SDL_FRect is a float version of SDL_Rect
        real(c_float) :: x
        real(c_float) :: y
        real(c_float) :: w
        real(c_float) :: h
    end type

    ! SDL Function interfaces
    interface
        !> @brief Initialize SDL.
        !> @param flags The flags to initialize SDL with.
        function SDL_Init(flags) bind(C, name='SDL_Init')
            import :: c_int
            integer(c_int), value :: flags
            integer(c_int) :: SDL_Init
        end function

        !> @brief Poll for currently pending events.
        !> @param event The event to store the next event in.
        function SDL_PollEvent(event) bind(C, name='SDL_PollEvent')
            import :: c_int, SDL_Event
            type(SDL_Event) :: event
            integer(c_int) :: SDL_PollEvent
        end function

        !> @brief Delay execution for a given number of milliseconds.
        !> @param ms The number of milliseconds to delay.
        subroutine SDL_Delay(ms) bind(C, name='SDL_Delay')
            import :: c_int
            integer(c_int), value :: ms
        end subroutine

        !> @brief Get the number of milliseconds since SDL library initialization.
        !> @return Returns an unsigned 64-bit value representing the number of milliseconds since the SDL library initialized.
        function SDL_GetTicks() bind(C, name='SDL_GetTicks')
            import :: c_int64_t
            integer(c_int64_t) :: SDL_GetTicks
        end function

        !> @brief Create a window.
        !> @param title The title of the window.
        !> @param x The x position of the window.
        !> @param y The y position of the window.
        !> @param w The width of the window.
        !> @param h The height of the window.
        !> @param flags The flags to create the window with.
        !> @return Returns a pointer to the created window.
        function SDL_CreateWindow(title, w, h, flags) bind(C, name='SDL_CreateWindow')
            import :: c_ptr, c_char, c_int
            character(kind=c_char), dimension(*) :: title
            integer(c_int), value :: w, h, flags
            type(c_ptr) :: SDL_CreateWindow
        end function

        !> @brief Get the size of a window.
        !> @param window The window to get the size of.
        !> @param w The width of the window.
        !> @param h The height of the window.
        !> @return Returns 0 on success, -1 on error.
        function SDL_GetWindowSize(window, w, h) bind(C, name='SDL_GetWindowSize')
            import :: c_ptr, c_int
            type(c_ptr), value :: window
            integer(c_int), intent(out) :: w, h
            integer(c_int) :: SDL_GetWindowSize
        end function

        !> @brief Destroy a window.
        !> @param window The window to destroy.
        subroutine SDL_DestroyWindow(window) bind(C, name='SDL_DestroyWindow')
            import :: c_ptr
            type(c_ptr), value :: window
        end subroutine

        !> @brief Quit SDL and clean up all subsystems.
        !> @details This should be called before the program exits.
        subroutine SDL_Quit() bind(C, name='SDL_Quit')
        end subroutine

        !> @brief Get the last error message as a string.
        !> @return Returns a pointer to the error message string.
        function SDL_GetError() bind(C, name='SDL_GetError')
            import :: c_ptr
            type(c_ptr) :: SDL_GetError
        end function

        !> @brief Create a 2D rendering context for a window.
        !> @param window The window where rendering is displayed.
        !> @param name The name of the rendering driver to initialize, or NULL to let SDL choose one.
        !> @return Returns a pointer to the created renderer.
        function SDL_CreateRenderer(window, name) bind(C, name='SDL_CreateRenderer')
            import :: c_ptr, c_char
            type(c_ptr), value :: window
            type(c_ptr), value :: name
            type(c_ptr) :: SDL_CreateRenderer
        end function

        !> @brief Set the color used for drawing operations.
        !> @param renderer The renderer to set the draw color for.
        !> @param r The red value to use in the draw color.
        !> @param g The green value to use in the draw color.
        !> @param b The blue value to use in the draw color.
        !> @param a The alpha value to use in the draw color.
        !> @return Returns 0 on success, -1 on error.
        function SDL_SetRenderDrawColor(renderer, r, g, b, a) bind(C, name='SDL_SetRenderDrawColor')
            import :: c_ptr, c_int8_t, c_int
            type(c_ptr), value :: renderer
            integer(c_int8_t), value :: r, g, b, a
            integer(c_int) :: SDL_SetRenderDrawColor
        end function

        !> @brief Draw a rectangle on the current rendering target.
        !> @param renderer The renderer to draw on.
        !> @param rect The rectangle to draw.
        !> @return Returns 0 on success, -1 on error.
        function SDL_RenderRect(renderer, rect) bind(C, name='SDL_RenderRect')
            import :: c_ptr, c_int
            type(c_ptr), value :: renderer
            type(c_ptr), value :: rect
            integer(c_int) :: SDL_RenderRect
        end function

        !> @brief Fill a rectangle on the current rendering target with the drawing color.
        !> @param renderer The renderer to draw on.
        !> @param rect The rectangle to fill.
        !> @return Returns 0 on success, -1 on error.
        function SDL_RenderFillRect(renderer, rect) bind(C, name='SDL_RenderFillRect')
            import :: c_ptr, c_int
            type(c_ptr), value :: renderer
            type(c_ptr), value :: rect
            integer(c_int) :: SDL_RenderFillRect
        end function

        !> @brief Clear the current rendering target with the drawing color.
        !> @param renderer The renderer to clear.
        subroutine SDL_RenderClear(renderer) bind(C, name='SDL_RenderClear')
            import :: c_ptr
            ! The renderer to clear.
            type(c_ptr), value :: renderer
        end subroutine

        !> @brief Copy the renderer's present target to the window.
        !> @param renderer The renderer to present.
        subroutine SDL_RenderPresent(renderer) bind(C, name='SDL_RenderPresent')
            import :: c_ptr
            ! The renderer to present.
            type(c_ptr), value :: renderer
        end subroutine

        !> @brief Destroy the rendering context for a window and free all associated textures.
        !> @param renderer The rendering context to destroy.
        subroutine SDL_DestroyRenderer(renderer) bind(C, name='SDL_DestroyRenderer')
            import :: c_ptr
            type(c_ptr), value :: renderer
        end subroutine
    end interface
  
    contains
    !> @brief Convert SDL_FRect to C pointer
    !> @param rect The SDL_FRect to convert
    !> @return Returns a C pointer to the SDL_FRect
    function c_loc_rect(rect) result(rect_ptr)
        type(SDL_FRect), target, intent(in) :: rect
        type(c_ptr) :: rect_ptr
        rect_ptr = c_loc(rect)
    end function
    
    !> @brief Get the last SDL error message as a string.
    !> @return Returns the last SDL error message as a string.
    function get_sdl_error()
        use iso_c_binding
        character(len=:), allocatable :: get_sdl_error
        type(c_ptr) :: error_ptr
        character(kind=c_char), pointer :: error_chars(:)
        integer :: i, length

        error_ptr = SDL_GetError()
        if (.not. c_associated(error_ptr)) then
            get_sdl_error = "No error message available"
            return
        end if

        call c_f_pointer(error_ptr, error_chars, [huge(0)])
        length = 0
        do while (error_chars(length + 1) .ne. c_null_char)
            length = length + 1
        end do

        allocate(character(len=length) :: get_sdl_error)
        do i = 1, length
            get_sdl_error(i:i) = error_chars(i)
        end do
    end function

end module sdl_wrapper