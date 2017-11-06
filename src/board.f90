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
     PROCEDURE :: combined_bitboard
     PROCEDURE :: write_board
     GENERIC :: WRITE(formatted) => write_board
  END TYPE board

  INTERFACE board
     MODULE PROCEDURE create_empty_board
     MODULE PROCEDURE create_board
  END INTERFACE board

  TYPE move
     INTEGER(1) :: from_square, to_square, captured_piece, promoted_piece
     LOGICAL :: en_passant
  END TYPE move

  INTERFACE WRITE(formatted)
     MODULE PROCEDURE write_move
  END INTERFACE WRITE(formatted)

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

  FUNCTION create_empty_board() RESULT(new_board)
    TYPE(board) :: new_board
    INTEGER :: i
    new_board%hash = 0
    DO i = 0, 63
       new_board%mailbox(i) = 0
       new_board%hash = IEOR(new_board%hash, zobrist_tables(i, 0))
    END DO
    new_board%hash = IEOR(new_board%hash, ISHFT(1_8, 63))
    DO i = -6, 6
       new_board%bitboards(i) = 0_8
    END DO
    new_board%bitboards(0) = -1_8
    new_board%move_number = 1
    new_board%halfmove_counter = 0
    new_board%side_to_move = white
    new_board%castling_rights = 0
    new_board%en_passant = 0
  END FUNCTION create_empty_board

  FUNCTION create_board(fen) RESULT(new_board)
    CHARACTER(*), INTENT(in) :: fen
    CHARACTER(128) :: tfen, dfen
    CHARACTER(1) :: color
    CHARACTER(4) :: castling_rights
    CHARACTER(2) :: en_passant
    CHARACTER(3) :: halfmove_counter_str
    CHARACTER(4) :: move_number_str
    INTEGER :: halfmove_counter, move_number
    TYPE(board) :: new_board
    INTEGER :: index, i
    INTEGER(1) :: file, rank
    new_board = create_empty_board()
    ! Parse FEN
    tfen = fen(1:INDEX(fen, ' ') - 1)
    dfen = fen(INDEX(fen, ' '):)
    READ (dfen, *) color, castling_rights, en_passant, halfmove_counter_str, move_number_str
    READ (halfmove_counter_str, *) halfmove_counter
    READ (move_number_str, *) move_number
    file = 1
    rank = 8
    DO i = 1, LEN(tfen)
       SELECT CASE (tfen(i:i))
       CASE ("K")
          CALL new_board%set_piece(INT(file * 8 + rank - 9, 1), INT(white * king, 1))
          file = file + 1
       CASE ("Q")
          CALL new_board%set_piece(INT(file * 8 + rank - 9, 1), INT(white * queen, 1))
          file = file + 1
       CASE ("R")
          CALL new_board%set_piece(INT(file * 8 + rank - 9, 1), INT(white * rook, 1))
          file = file + 1
       CASE ("B")
          CALL new_board%set_piece(INT(file * 8 + rank - 9, 1), INT(white * bishop, 1))
          file = file + 1
       CASE ("N")
          CALL new_board%set_piece(INT(file * 8 + rank - 9, 1), INT(white * knight, 1))
          file = file + 1
       CASE ("P")
          CALL new_board%set_piece(INT(file * 8 + rank - 9, 1), INT(white * pawn, 1))
          file = file + 1
       CASE ("k")
          CALL new_board%set_piece(INT(file * 8 + rank - 9, 1), INT(black * king, 1))
          file = file + 1
       CASE ("q")
          CALL new_board%set_piece(INT(file * 8 + rank - 9, 1), INT(black * queen, 1))
          file = file + 1
       CASE ("r")
          CALL new_board%set_piece(INT(file * 8 + rank - 9, 1), INT(black * rook, 1))
          file = file + 1
       CASE ("b")
          CALL new_board%set_piece(INT(file * 8 + rank - 9, 1), INT(black * bishop, 1))
          file = file + 1
       CASE ("n")
          CALL new_board%set_piece(INT(file * 8 + rank - 9, 1), INT(black * knight, 1))
          file = file + 1
       CASE ("p")
          CALL new_board%set_piece(INT(file * 8 + rank - 9, 1), INT(black * pawn, 1))
          file = file + 1
       CASE ("/")
          file = 1
          rank = rank - 1
       CASE ("1":"8")
          file = file + ICHAR(tfen(i:i)) - ICHAR("0")
       END SELECT
    END DO
    SELECT CASE (color)
    CASE ("w")
       new_board%side_to_move = white
    CASE ("b")
       new_board%side_to_move = black
    END SELECT
    DO i = 1, LEN(castling_rights)
       SELECT CASE (castling_rights(i:i))
       CASE ("K")
          new_board%castling_rights = IOR(new_board%castling_rights, 4_1)
       CASE ("Q")
          new_board%castling_rights = IOR(new_board%castling_rights, 8_1)
       CASE ("k")
          new_board%castling_rights = IOR(new_board%castling_rights, 1_1)
       CASE ("q")
          new_board%castling_rights = IOR(new_board%castling_rights, 2_1)
       CASE ("-")
          new_board%castling_rights = 0
       END SELECT
    END DO
    SELECT CASE (en_passant(1:1))
    CASE ("a":"h")
       new_board%en_passant = ICHAR(en_passant(1:1)) - ICHAR("a") + 1
    CASE ("-")
       new_board%en_passant = 0
    END SELECT
    new_board%halfmove_counter = halfmove_counter
    new_board%move_number = move_number
  END FUNCTION create_board

  SUBROUTINE write_board(dtv, unit, iotype, v_list, iostat, iomsg)
    CLASS(board), INTENT(in) :: dtv
    INTEGER, INTENT(in) :: unit
    CHARACTER(*), INTENT(in) :: iotype
    INTEGER, INTENT(in) :: v_list(:)
    INTEGER, INTENT(out) :: iostat
    CHARACTER(*), INTENT(inout) :: iomsg
    CHARACTER(*), PARAMETER :: pieces = "kqrbnp.PNBRQK"
    INTEGER :: rank, file
    INTEGER(1) :: p
    WRITE (unit, fmt=*, iostat=iostat, iomsg=iomsg) "side to move = ", dtv%side_to_move, &
         " en passant = ", dtv%en_passant, "castling = ", dtv%castling_rights, NEW_LINE("0")
    DO rank = 8, 1, -1
       DO file = 1, 8, 1
          p = dtv%mailbox(8 * file + rank - 9) + 7
          WRITE (unit, fmt="(A2)", iostat=iostat, iomsg=iomsg) pieces(p:p)
       END DO
       WRITE (unit, fmt=*, iostat=iostat, iomsg=iomsg) NEW_LINE("0")
    END DO
  END SUBROUTINE write_board

  SUBROUTINE write_move(dtv, unit, iotype, v_list, iostat, iomsg)
    CLASS(move), INTENT(in) :: dtv
    INTEGER, INTENT(in) :: unit
    CHARACTER(*), INTENT(in) :: iotype
    INTEGER, INTENT(in) :: v_list(:)
    INTEGER, INTENT(out) :: iostat
    CHARACTER(*), INTENT(inout) :: iomsg
    CHARACTER(1) :: file1, rank1, file2, rank2
    file1 = achar(ichar("a") + ISHFT(dtv%from_square, -3_1))
    rank1 = achar(ichar("1") + IAND(dtv%from_square, 7_1))
    file2 = achar(ichar("a") + ISHFT(dtv%to_square, -3_1))
    rank2 = achar(ichar("1") + IAND(dtv%to_square, 7_1))
    WRITE (unit, fmt="(4A1)", iostat=iostat, iomsg=iomsg) file1, rank1, file2, rank2
  END SUBROUTINE write_move

  FUNCTION make_move(this, m) RESULT(child)
    CLASS(board), VALUE :: this
    TYPE(move), INTENT(in) :: m
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

  FUNCTION combined_bitboard(this, color) RESULT(res)
    CLASS(board), INTENT(in) :: this
    INTEGER(1) :: color, i
    INTEGER(8) :: res
    res = 0
    DO i = pawn, king
       res = IOR(res, this%bitboards(color * king))
    END DO
  END FUNCTION combined_bitboard

END MODULE board_module
