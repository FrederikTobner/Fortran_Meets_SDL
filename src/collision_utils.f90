module collision_utils
    use, intrinsic :: iso_c_binding
    implicit none

    ! Rectangle type for collision detection
    type :: Rectangle
        real(c_double) :: x, y, w, h
    end type Rectangle

contains
    ! Check if a point is inside a rectangle
    function point_in_rect(px, py, rect) result(collision)
        real(c_double), intent(in) :: px, py
        type(Rectangle), intent(in) :: rect
        logical :: collision
        
        collision = px >= rect%x .and. px <= rect%x + rect%w .and. &
                   py >= rect%y .and. py <= rect%y + rect%h
    end function

    ! Line segment intersection with rectangle
    function line_rect_intersection(x1, y1, x2, y2, rect) result(collision)
        real(c_double), intent(in) :: x1, y1, x2, y2
        type(Rectangle), intent(in) :: rect
        logical :: collision
        real(c_double) :: tx1, ty1, tx2, ty2, t, x, y
        
        collision = .false.

        ! Check if start or end points are inside rectangle
        if (point_in_rect(x1, y1, rect) .or. point_in_rect(x2, y2, rect)) then
            collision = .true.
            return
        end if

        ! Check each edge of the rectangle for intersection
        ! Top edge
        if (line_intersection(x1, y1, x2, y2, rect%x, rect%y, rect%x + rect%w, rect%y, t)) then
            if (t >= 0.0 .and. t <= 1.0) then
                collision = .true.
                return
            end if
        end if
        
        ! Bottom edge
        if (line_intersection(x1, y1, x2, y2, rect%x, rect%y + rect%h, &
                            rect%x + rect%w, rect%y + rect%h, t)) then
            if (t >= 0.0 .and. t <= 1.0) then
                collision = .true.
                return
            end if
        end if
        
        ! Left edge
        if (line_intersection(x1, y1, x2, y2, rect%x, rect%y, rect%x, rect%y + rect%h, t)) then
            if (t >= 0.0 .and. t <= 1.0) then
                collision = .true.
                return
            end if
        end if
        
        ! Right edge
        if (line_intersection(x1, y1, x2, y2, rect%x + rect%w, rect%y, &
                            rect%x + rect%w, rect%y + rect%h, t)) then
            if (t >= 0.0 .and. t <= 1.0) then
                collision = .true.
                return
            end if
        end if
    end function

    ! Line segment intersection test
    function line_intersection(x1, y1, x2, y2, x3, y3, x4, y4, t) result(intersects)
        real(c_double), intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(c_double), intent(out) :: t
        logical :: intersects
        real(c_double) :: denominator, t1
        
        denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        
        if (abs(denominator) < 1.0e-10) then
            intersects = .false.
            t = 0.0
            return
        end if
        
        t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denominator
        t1 = ((x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)) / denominator
        
        intersects = t1 >= 0.0 .and. t1 <= 1.0
    end function
end module collision_utils