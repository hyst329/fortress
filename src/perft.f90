MODULE perft_module

  IMPLICIT NONE

CONTAINS
  RECURSIVE FUNCTION perft(b, depth) result(res)
    USE board_module
    USE move_module
    TYPE(board), INTENT(in) :: b
    INTEGER, INTENT(in) :: depth
    INTEGER :: res
    INTEGER :: i
    TYPE(move), ALLOCATABLE :: moves(:)
    TYPE(board) :: child
    iF (depth == 0) then
      res = 0
      return
    end if
    moves = generate_moves(b, .FALSE.)
    iF (depth == 1) then
      res = size(moves)
      return
    end if
    res = 0
    do i = 1, size(moves)!lbound(moves, 1), ubound(moves, 1)
      print *, moves(i)
      child = b%make_move(moves(i))
      print *, child
      res = res + perft(child, depth - 1)
    end do
  END FUNCTION perft

END MODULE perft_module
