MODULE board_module
  IMPLICIT NONE

  INTEGER(1), PARAMETER :: NONE = 0
  INTEGER(1), PARAMETER :: white = 1, black = -1
  INTEGER(1), PARAMETER :: pawn = 1, knight = 2, bishop = 3, rook = 4, queen = 5, king = 6
  INTEGER(1), PARAMETER :: kingside = 1, queenside = 2, both = 3
  INTEGER(8) :: zobrist_tables(0:63, -6:6)
  TYPE board
     INTEGER(1) :: mailbox(0:63)
     INTEGER(8) :: bitboards(-6:6)
     INTEGER(1) :: side_to_move
     INTEGER(1) :: castling_rights ! MSB white, LSB black
     INTEGER(1) :: en_passant
     INTEGER :: move_index
     INTEGER(8) :: hash
     INTEGER(1) :: halfmove_counter
     INTEGER(2) :: move_number
   CONTAINS
     PROCEDURE :: make_move
     PROCEDURE :: set_piece
  END TYPE board

CONTAINS
  FUNCTION generate_random_int64() RESULT(res)
    INTEGER(8) :: res
    INTEGER :: i
    REAL(8) :: temp(4)
    CALL random_NUMBER(temp)
    res = 0
    DO i = 1, 4
       res = ISHFT(res, 16)
       res = res + INT(temp(i)*65536)
    END DO
  END FUNCTION generate_random_int64

  SUBROUTINE initialise_zobrist_tables
    INTEGER :: i, j
    DO i = 0, 63
       DO j = -6, 6
          zobrist_tables(i, j) = generate_random_int64()
       END DO
    END DO
  END SUBROUTINE initialise_zobrist_tables

  FUNCTION make_move(this, m) RESULT(child)
    USE move_module
    CLASS(board), VALUE :: this
    CLASS(move), INTENT(in) :: m
    CLASS(board), ALLOCATABLE :: child
    INTEGER(1) :: piece, rook_square, rook_piece, new_rook_square, actual_pawn_square
    INTEGER(1) :: promoted_piece, captured_piece
    ALLOCATE(child, source=this)
    piece = child%mailbox(m%from_square)
    IF (ABS(piece) == KING .AND. ABS(m%from_square - m%to_square) == 16) THEN
       rook_square = MERGE(56_1 + IAND(m%from_square, 7_1), IAND(m%from_square, 7_1), ISHFT(m%from_square, -3) > 4)
       CALL child%set_piece(m%from_square, NONE)
       CALL child%set_piece(m%to_square, piece)
       rook_piece = child%mailbox(rook_square)
       new_rook_square = MERGE(rook_square - 16_1, rook_square + 24_1, ISHFT(m%to_square, -3) > 4)
       CALL child%set_piece(rook_square, NONE)
       CALL child%set_piece(new_rook_square, rook_piece)
       child%castling_rights = IAND(child%castling_rights, MERGE(3_1, 12_1, child%side_to_move == white))
       child%side_to_move = -child%side_to_move
       child%hash = IEOR(child%hash, ISHFT(1_8, 63))
       IF (child%side_to_move == white) THEN
          child%move_number = child%move_number + 1_2
       END IF
       child%halfmove_counter = child%halfmove_counter + 1_1
       child%en_passant = 0_1
    ELSE
       promoted_piece = m%promoted_piece
       captured_piece = m%captured_piece
       CALL child%set_piece(m%from_square, NONE)
       CALL child%set_piece(m%to_square, MERGE(promoted_piece, piece, promoted_piece /= 0))
       IF (ABS(piece) == king) THEN
          child%castling_rights = IAND(child%castling_rights, MERGE(3_1, 12_1, child%side_to_move == white))
       END IF
       IF ((ABS(piece) == rook .AND. m%from_square == 0) .OR. m%to_square == 0) THEN
          child%castling_rights = IAND(child%castling_rights, 7_1)
       END IF
       IF ((ABS(piece) == rook .AND. m%from_square == 7) .OR. m%to_square == 7) THEN
          child%castling_rights = IAND(child%castling_rights, 13_1)
       END IF
       IF ((ABS(piece) == rook .AND. m%from_square == 56) .OR. m%to_square == 56) THEN
          child%castling_rights = IAND(child%castling_rights, 11_1)
       END IF
       IF ((ABS(piece) == rook .AND. m%from_square == 63) .OR. m%to_square == 63) THEN
          child%castling_rights = IAND(child%castling_rights, 14_1)
       END IF
       child%side_to_move = -child%side_to_move
       child%hash = IEOR(child%hash, ISHFT(1_8, 63))
       IF (child%side_to_move == white) THEN
          child%move_number = child%move_number + 1_2
       END IF
       IF (m%en_passant) THEN
          actual_pawn_square = m%to_square + child%side_to_move
          CALL child%set_piece(actual_pawn_square, NONE)
       END IF
       IF (ABS(piece) == pawn .AND. ABS(m%from_square - m%to_square) == 2) THEN
          child%en_passant = ISHFT(m%from_square, -3) + 1_1
       ELSE
          child%en_passant = 0
       END IF
       IF (ABS(piece) == pawn .OR. captured_piece /= NONE) THEN
          child%halfmove_counter = 0
       ELSE
          child%halfmove_counter = child%halfmove_counter + 1_1
       END IF
    END IF
  END FUNCTION make_move

  SUBROUTINE set_piece(this, square, piece)
    CLASS(board), INTENT(inout) :: this
    INTEGER(1) :: square, piece
    INTEGER(1) :: old_piece
    old_piece = this%mailbox(square)
    this%bitboards(old_piece) = IAND(this%bitboards(old_piece), NOT(ISHFT(1_8, square)))
    this%bitboards(NONE) = IAND(this%bitboards(NONE), NOT(ISHFT(1_8, square)))
    this%bitboards(piece) = IOR(this%bitboards(NONE), ISHFT(1_8, square))
    this%hash = IEOR(this%hash, zobrist_tables(square, old_piece))
    this%hash = IEOR(this%hash, zobrist_tables(square, piece))
    this%mailbox(square) = piece
  END SUBROUTINE set_piece

END MODULE board_module
