MODULE move_module
  IMPLICIT NONE

  INTEGER(8), PARAMETER :: king_patterns(0:63) = [ &
       Z"0000000000000302", Z"0000000000000705", Z"0000000000000e0a", &
       Z"0000000000001c14", Z"0000000000003828", Z"0000000000007050", &
       Z"000000000000e0a0", Z"000000000000c040", Z"0000000000030203", &
       Z"0000000000070507", Z"00000000000e0a0e", Z"00000000001c141c", &
       Z"0000000000382838", Z"0000000000705070", Z"0000000000e0a0e0", &
       Z"0000000000c040c0", Z"0000000003020300", Z"0000000007050700", &
       Z"000000000e0a0e00", Z"000000001c141c00", Z"0000000038283800", &
       Z"0000000070507000", Z"00000000e0a0e000", Z"00000000c040c000", &
       Z"0000000302030000", Z"0000000705070000", Z"0000000e0a0e0000", &
       Z"0000001c141c0000", Z"0000003828380000", Z"0000007050700000", &
       Z"000000e0a0e00000", Z"000000c040c00000", Z"0000030203000000", &
       Z"0000070507000000", Z"00000e0a0e000000", Z"00001c141c000000", &
       Z"0000382838000000", Z"0000705070000000", Z"0000e0a0e0000000", &
       Z"0000c040c0000000", Z"0003020300000000", Z"0007050700000000", &
       Z"000e0a0e00000000", Z"001c141c00000000", Z"0038283800000000", &
       Z"0070507000000000", Z"00e0a0e000000000", Z"00c040c000000000", &
       Z"0302030000000000", Z"0705070000000000", Z"0e0a0e0000000000", &
       Z"1c141c0000000000", Z"3828380000000000", Z"7050700000000000", &
       Z"e0a0e00000000000", Z"c040c00000000000", Z"0203000000000000", &
       Z"0507000000000000", Z"0a0e000000000000", Z"141c000000000000", &
       Z"2838000000000000", Z"5070000000000000", Z"a0e0000000000000", &
       Z"40c0000000000000"]

  INTEGER(8), PARAMETER :: knight_patterns(0:63) = [ &
       Z"0000000000020400", Z"0000000000050800", Z"00000000000a1100", &
       Z"0000000000142200", Z"0000000000284400", Z"0000000000508800", &
       Z"0000000000a01000", Z"0000000000402000", Z"0000000002040004", &
       Z"0000000005080008", Z"000000000a110011", Z"0000000014220022", &
       Z"0000000028440044", Z"0000000050880088", Z"00000000a0100010", &
       Z"0000000040200020", Z"0000000204000402", Z"0000000508000805", &
       Z"0000000a1100110a", Z"0000001422002214", Z"0000002844004428", &
       Z"0000005088008850", Z"000000a0100010a0", Z"0000004020002040", &
       Z"0000020400040200", Z"0000050800080500", Z"00000a1100110a00", &
       Z"0000142200221400", Z"0000284400442800", Z"0000508800885000", &
       Z"0000a0100010a000", Z"0000402000204000", Z"0002040004020000", &
       Z"0005080008050000", Z"000a1100110a0000", Z"0014220022140000", &
       Z"0028440044280000", Z"0050880088500000", Z"00a0100010a00000", &
       Z"0040200020400000", Z"0204000402000000", Z"0508000805000000", &
       Z"0a1100110a000000", Z"1422002214000000", Z"2844004428000000", &
       Z"5088008850000000", Z"a0100010a0000000", Z"4020002040000000", &
       Z"0400040200000000", Z"0800080500000000", Z"1100110a00000000", &
       Z"2200221400000000", Z"4400442800000000", Z"8800885000000000", &
       Z"100010a000000000", Z"2000204000000000", Z"0004020000000000", &
       Z"0008050000000000", Z"00110a0000000000", Z"0022140000000000", &
       Z"0044280000000000", Z"0088500000000000", Z"0010a00000000000", &
       Z"0020400000000000"]

  INTEGER, PARAMETER :: rook_bits(0:63) = [ &
       12, 11, 11, 11, 11, 11, 11, 12, &
       11, 10, 10, 10, 10, 10, 10, 11, &
       11, 10, 10, 10, 10, 10, 10, 11, &
       11, 10, 10, 10, 10, 10, 10, 11, &
       11, 10, 10, 10, 10, 10, 10, 11, &
       11, 10, 10, 10, 10, 10, 10, 11, &
       11, 10, 10, 10, 10, 10, 10, 11, &
       12, 11, 11, 11, 11, 11, 11, 12]

  INTEGER, PARAMETER :: bishop_bits(0:63) = [&
       6, 5, 5, 5, 5, 5, 5, 6, &
       5, 5, 5, 5, 5, 5, 5, 5, &
       5, 5, 7, 7, 7, 7, 5, 5, &
       5, 5, 7, 9, 9, 7, 5, 5, &
       5, 5, 7, 9, 9, 7, 5, 5, &
       5, 5, 7, 7, 7, 7, 5, 5, &
       5, 5, 5, 5, 5, 5, 5, 5, &
       6, 5, 5, 5, 5, 5, 5, 6]

  INTEGER(8) :: rook_attacks(0:63, 0:4095), bishop_attacks(0:63, 0:511)

CONTAINS
  SUBROUTINE read_attacks()
    INTEGER :: i
    OPEN(unit=28, file="fortress_rattacks.dat", access="stream", form="unformatted", status="old")
    OPEN(unit=29, file="fortress_battacks.dat", access="stream", form="unformatted", status="old")
    DO I = 0, 63
       READ (28) rook_attacks(i, :)
       READ (29) bishop_attacks(i, :)
    END DO
    CLOSE(28)
    CLOSE(29)
  END SUBROUTINE read_attacks

  SUBROUTINE generate_moves_pawn(b, square, buffer, index, captures_only)
    USE board_module
    TYPE(board), INTENT(in) :: b
    INTEGER(1), INTENT(in) :: square
    TYPE(move), INTENT(inout) :: buffer(256)
    INTEGER, INTENT(inout) :: index
    LOGICAL :: captures_only
    INTEGER(1) :: forward, double_forward, left_forward, right_forward
    INTEGER(1) :: color, piece, en_passant
    color = b%side_to_move
    forward = square + color
    double_forward = square + 2 * color
    en_passant = MERGE(b%en_passant * 8 - MERGE(3, 6, color == white), -1, b%en_passant /= 0)
    IF (.NOT. captures_only) THEN
       IF (b%mailbox(forward) == NONE) THEN
          IF (ISHFT(forward, 3) == MERGE(7, 0, color == white)) THEN
             buffer(index) = move(square, forward, NONE, queen, .FALSE.)
             buffer(index + 1) = move(square, forward, NONE, rook, .FALSE.)
             buffer(index + 2) = move(square, forward, NONE, bishop, .FALSE.)
             buffer(index + 3) = move(square, forward, NONE, knight, .FALSE.)
             index = index + 4
          ELSE
             buffer(index) = move(square, forward, NONE, NONE, .FALSE.)
             index = index + 1
          END IF
       ENDIF
    END IF
    left_forward = MERGE(forward - 8, -1, forward .GE. 8)
    right_forward = MERGE(forward + 8, -1, forward .LE. 55)
    IF (left_forward >= 0) THEN
       piece = b%mailbox(left_forward)
       IF (piece * color < 0) THEN
          IF (ISHFT(left_forward, 3) == MERGE(7, 0, color == white)) THEN
             buffer(index) = move(square, left_forward, piece, queen, .FALSE.)
             buffer(index + 1) = move(square, left_forward, piece, rook, .FALSE.)
             buffer(index + 2) = move(square, left_forward, piece, bishop, .FALSE.)
             buffer(index + 3) = move(square, left_forward, piece, knight, .FALSE.)
             index = index + 4
          ELSE
             buffer(index) = move(square, left_forward, piece, NONE, .FALSE.)
             index = index + 1
          END IF
       END IF
    END IF
    IF (right_forward >= 0) THEN
       piece = b%mailbox(right_forward)
       IF (piece * color < 0) THEN
          IF (ISHFT(right_forward, 3) == MERGE(7, 0, color == white)) THEN
             buffer(index) = move(square, right_forward, piece, queen, .FALSE.)
             buffer(index + 1) = move(square, right_forward, piece, rook, .FALSE.)
             buffer(index + 2) = move(square, right_forward, piece, bishop, .FALSE.)
             buffer(index + 3) = move(square, right_forward, piece, knight, .FALSE.)
             index = index + 4
          ELSE
             buffer(index) = move(square, right_forward, piece, NONE, .FALSE.)
             index = index + 1
          END IF
       END IF
    END IF
    ! En passant captures
    IF (en_passant >= 0) THEN
       IF (left_forward == en_passant) THEN
          buffer(index) = move(square, left_forward, -color * pawn, NONE, .TRUE.)
          index = index + 1
       END IF
       IF (right_forward == en_passant) THEN
          buffer(index) = move(square, right_forward, -color * pawn, NONE, .TRUE.)
          index = index + 1
       END IF
    END IF
    ! Double-square push
    IF (ISHFT(square, -3) == MERGE(1, 6, color == white) .AND..NOT. captures_only) THEN
       IF (b%mailbox(forward) == NONE .AND. b%mailbox(double_forward) == NONE) THEN
          buffer(index) = move(square, double_forward, NONE, NONE, .FALSE.)
          index = index + 1
       END IF
    END IF
  END SUBROUTINE generate_moves_pawn

  SUBROUTINE generate_moves_knight(b, square, buffer, index, captures_only)
    USE board_module
    TYPE(board), INTENT(in) :: b
    INTEGER(1), INTENT(in) :: square
    TYPE(move), INTENT(inout) :: buffer(256)
    INTEGER, INTENT(inout) :: index
    LOGICAL :: captures_only
    INTEGER(8) :: needed_squares, pattern
    INTEGER(1) :: i
    needed_squares = b%combined_bitboard(-b%side_to_move)
    IF (.NOT. captures_only) needed_squares = IOR(needed_squares, b%bitboards(NONE))
    pattern = IAND(knight_patterns(square), needed_squares)
    DO i = 0, 63
       IF (BTEST(pattern, i)) THEN
          buffer(index) = move(square, i, b%mailbox(i), NONE, .FALSE.)
          index = index + 1
       END IF
    END DO
  END SUBROUTINE generate_moves_knight

  SUBROUTINE generate_moves_bishop(b, square, buffer, index, captures_only)
    USE board_module
    USE magic_module
    TYPE(board), INTENT(in) :: b
    INTEGER(1), INTENT(in) :: square
    TYPE(move), INTENT(inout) :: buffer(256)
    INTEGER, INTENT(inout) :: index
    LOGICAL :: captures_only
    INTEGER(8) :: occupied, pattern
    INTEGER(1) :: i
    occupied = NOT(b%bitboards(NONE))
    occupied = IAND(occupied, bishop_magic(square)%mask)
    occupied = occupied * bishop_magic(square)%magic
    occupied = ISHFT(occupied, bishop_bits(square) - 64)
    pattern = bishop_attacks(square, occupied)
    pattern = IAND(pattern, NOT(b%combined_bitboard(b%side_to_move)))
    IF (captures_only) pattern = IAND(pattern, NOT(b%bitboards(NONE)))
    DO i = 0, 63
       IF (BTEST(pattern, i)) THEN
          buffer(index) = move(square, i, b%mailbox(i), NONE, .FALSE.)
          index = index + 1
       END IF
    END DO
  END SUBROUTINE generate_moves_bishop

  SUBROUTINE generate_moves_rook(b, square, buffer, index, captures_only)
    USE board_module
    USE magic_module
    TYPE(board), INTENT(in) :: b
    INTEGER(1), INTENT(in) :: square
    TYPE(move), INTENT(inout) :: buffer(256)
    INTEGER, INTENT(inout) :: index
    LOGICAL :: captures_only
    INTEGER(8) :: occupied, pattern
    INTEGER(1) :: i
    occupied = NOT(b%bitboards(NONE))
    occupied = IAND(occupied, rook_magic(square)%mask)
    occupied = occupied * rook_magic(square)%magic
    occupied = ISHFT(occupied, rook_bits(square) - 64)
    pattern = rook_attacks(square, occupied)
    pattern = IAND(pattern, NOT(b%combined_bitboard(b%side_to_move)))
    IF (captures_only) pattern = IAND(pattern, NOT(b%bitboards(NONE)))
    DO i = 0, 63
       IF (BTEST(pattern, i)) THEN
          buffer(index) = move(square, i, b%mailbox(i), NONE, .FALSE.)
          index = index + 1
       END IF
    END DO
  END SUBROUTINE generate_moves_rook

  SUBROUTINE generate_moves_queen(b, square, buffer, index, captures_only)
    USE board_module
    USE magic_module
    TYPE(board), INTENT(in) :: b
    INTEGER(1), INTENT(in) :: square
    TYPE(move), INTENT(inout) :: buffer(256)
    INTEGER, INTENT(inout) :: index
    LOGICAL :: captures_only
    INTEGER(8) :: occupied_rook, occupied_bishop
    INTEGER(8) :: pattern_rook, pattern_bishop, pattern
    INTEGER(1) :: i
    occupied_rook = NOT(b%bitboards(NONE))
    occupied_rook = IAND(occupied_rook, rook_magic(square)%mask)
    occupied_rook = occupied_rook * rook_magic(square)%magic
    occupied_rook = ISHFT(occupied_rook, rook_bits(square) - 64)
    pattern_rook = rook_attacks(square, occupied_rook)
    occupied_bishop = NOT(b%bitboards(NONE))
    occupied_bishop = IAND(occupied_bishop, bishop_magic(square)%mask)
    occupied_bishop = occupied_bishop * bishop_magic(square)%magic
    occupied_bishop = ISHFT(occupied_bishop, bishop_bits(square) - 64)
    pattern_bishop = bishop_attacks(square, occupied_bishop)
    pattern = IOR(pattern_rook, pattern_bishop)
    pattern = IAND(pattern, NOT(b%combined_bitboard(b%side_to_move)))
    IF (captures_only) pattern = IAND(pattern, NOT(b%bitboards(NONE)))
    DO i = 0, 63
       IF (BTEST(pattern, i)) THEN
          buffer(index) = move(square, i, b%mailbox(i), NONE, .FALSE.)
          index = index + 1
       END IF
    END DO
  END SUBROUTINE generate_moves_queen

  SUBROUTINE generate_moves_king(b, square, buffer, index, captures_only)
    USE board_module
    TYPE(board), INTENT(in) :: b
    INTEGER(1), INTENT(in) :: square
    TYPE(move), INTENT(inout) :: buffer(256)
    INTEGER, INTENT(inout) :: index
    LOGICAL :: captures_only
    INTEGER(8) :: needed_squares, pattern
    INTEGER(1) :: i
    needed_squares = b%combined_bitboard(-b%side_to_move)
    IF (.NOT. captures_only) needed_squares = IOR(needed_squares, b%bitboards(NONE))
    pattern = IAND(king_patterns(square), needed_squares)
    DO i = 0, 63
       IF (BTEST(pattern, i)) THEN
          buffer(index) = move(square, i, b%mailbox(i), NONE, .FALSE.)
          index = index + 1
       END IF
    END DO
  END SUBROUTINE generate_moves_king

  FUNCTION generate_moves(b, captures_only) RESULT(res)
    USE board_module
    TYPE(board), INTENT(in) :: b
    INTEGER(1) :: square
    INTEGER :: index, legal_index, i
    INTEGER(8) :: own_pieces
    TYPE(move) :: buffer(256), legal_buffer(256), m
    TYPE(move), ALLOCATABLE :: res(:)
    TYPE(board) :: child
    LOGICAL :: captures_only
    INTEGER(1) :: king_square, target_square
    INTEGER(8) :: mask, free_squares
    LOGICAL :: legal, checked, kingside_through_check, queenside_through_check, illegal_castling
    index = 1
    own_pieces = b%combined_bitboard(b%side_to_move)
    print "(B64.64)", own_pieces
    free_squares = b%bitboards(NONE)
    DO square = 0, 63
       IF (BTEST(own_pieces, square)) THEN
          SELECT CASE (ABS(b%mailbox(square)))
          CASE (pawn)
             CALL generate_moves_pawn(b, square, buffer, index, captures_only)
             print *, square, "pawn, index =", index
          CASE (knight)
             CALL generate_moves_knight(b, square, buffer, index, captures_only)
             print *, square, "knight, index =", index
          CASE (bishop)
             CALL generate_moves_bishop(b, square, buffer, index, captures_only)
             print *, square, "bishop, index =", index
          CASE (rook)
             CALL generate_moves_rook(b, square, buffer, index, captures_only)
             print *, square, "rook, index =", index
          CASE (queen)
             CALL generate_moves_queen(b, square, buffer, index, captures_only)
             print *, square, "queen, index =", index
          CASE (king)
             CALL generate_moves_king(b, square, buffer, index, captures_only)
             print *, square, "king, index =", index
          END SELECT
       END IF
    END DO
    ! Castling
    IF (.NOT. captures_only) THEN
       IF (b%side_to_move == white) THEN
          IF (BTEST(b%castling_rights, 3)) THEN
             ! White can castle queenside
             king_square = 32
             target_square = 16
             mask = INT(Z"0000000001010100")
             IF (IAND(free_squares, mask) == mask) THEN
                buffer(index) = move(king_square, target_square, NONE, NONE, .FALSE.)
                index = index + 1
             END IF
          END IF
          IF (BTEST(b%castling_rights, 2)) THEN
             ! White can castle kingside
             king_square = 32
             target_square = 48
             mask = INT(Z"0001010000000000")
             IF (IAND(free_squares, mask) == mask) THEN
                buffer(index) = move(king_square, target_square, NONE, NONE, .FALSE.)
                index = index + 1
             END IF
          END IF
       ELSE
          IF (BTEST(b%castling_rights, 1)) THEN
             ! Black can castle queenside
             king_square = 39
             target_square = 23
             mask = INT(Z"0000000080808000")
             IF (IAND(free_squares, mask) == mask) THEN
                buffer(index) = move(king_square, target_square, NONE, NONE, .FALSE.)
                index = index + 1
             END IF
          END IF
          IF (BTEST(b%castling_rights, 0)) THEN
             ! Black can castle kingside
             king_square = 39
             target_square = 55
             mask = INT(Z"0080800000000000")
             IF (IAND(free_squares, mask) == mask) THEN
                buffer(index) = move(king_square, target_square, NONE, NONE, .FALSE.)
                index = index + 1
             END IF
          END IF
       END IF
    END IF
    ! Check for legality
    index = index - 1 ! Number of pseudo-legal moves
    print *, index, "moves generated"
    legal_index = 1
    kingside_through_check = .FALSE.
    queenside_through_check = .FALSE.
    checked = checking_pieces(b) /= 0
    DO i = 1, index
       m = buffer(i)
       child = b%make_move(m)
       child%side_to_move = -child%side_to_move
       legal = (checking_pieces(child) /= 0)
       child%side_to_move = -child%side_to_move
       kingside_through_check = kingside_through_check .OR. &
            (buffer(i)%from_square == MERGE(32, 39, b%side_to_move == white) .AND. &
            buffer(i)%to_square == MERGE(40, 47, b%side_to_move == white) .AND. &
            BTEST(b%castling_rights, MERGE(2, 0, b%side_to_move == white)) .AND. &
            .NOT. legal)
       queenside_through_check = queenside_through_check .OR. &
            (buffer(i)%from_square == MERGE(32, 39, b%side_to_move == white) .AND. &
            buffer(i)%to_square == MERGE(24, 31, b%side_to_move == white) .AND. &
            BTEST(b%castling_rights, MERGE(3, 1, b%side_to_move == white)) .AND. &
            .NOT. legal)
       illegal_castling = &
            (buffer(i)%from_square == MERGE(32, 39, b%side_to_move == white) .AND. &
            buffer(i)%to_square == MERGE(48, 55, b%side_to_move == white) .AND. &
            ABS(b%mailbox(buffer(i)%from_square)) == king .AND. &
            (kingside_through_check .or. checked))
       if (illegal_castling) legal = .FALSE.
       illegal_castling = &
            (buffer(i)%from_square == MERGE(32, 39, b%side_to_move == white) .AND. &
            buffer(i)%to_square == MERGE(16, 23, b%side_to_move == white) .AND. &
            ABS(b%mailbox(buffer(i)%from_square)) == king .AND. &
            (queenside_through_check .or. checked))
       if (illegal_castling) legal = .FALSE.
       if (legal) THEN
         legal_buffer(legal_index) = buffer(i)
         legal_index = legal_index + 1
       end if
    END DO
    legal_index = legal_index - 1 ! Number of legal moves
    allocate(res(1:legal_index))
    res = legal_buffer(1:legal_index)
  END FUNCTION generate_moves

  FUNCTION checking_pieces(b)
    USE board_module
    USE magic_module
    TYPE(board), INTENT(in) :: b
    INTEGER(8) :: checking_pieces
    INTEGER(8) :: king_bitboard
    INTEGER(1) :: king_square, enemy_king_square, color
    INTEGER(8) :: enemy_queens_bitboard, enemy_rooks_bitboard, enemy_bishops_bitboard
    INTEGER(8) :: enemy_knights_bitboard, enemy_pawns_bitboard, enemy_king_bitboard
    INTEGER(8) :: occupied
    INTEGER(1) :: backward, left_backward, right_backward
    INTEGER(8) :: left_backward_bitboard, right_backward_bitboard
    checking_pieces = 0
    color = b%side_to_move
    king_bitboard = b%bitboards(color * king)
    king_square = TRAILZ(king_bitboard)
    enemy_king_bitboard = b%bitboards(-color * king)
    enemy_king_square = TRAILZ(enemy_king_bitboard)
    occupied = 0
    enemy_rooks_bitboard = b%bitboards(-color * rook)
    enemy_bishops_bitboard = b%bitboards(-color * bishop)
    enemy_queens_bitboard = b%bitboards(-color * queen)
    occupied = NOT(b%bitboards(NONE))
    occupied = IAND(occupied, rook_magic(king_square)%mask)
    occupied = occupied * rook_magic(king_square)%magic
    occupied = ISHFT(occupied, rook_bits(king_square) - 64)
    checking_pieces = IOR(checking_pieces, IAND(rook_attacks(king_square, occupied), &
         IOR(enemy_rooks_bitboard, enemy_queens_bitboard)))
    occupied = IAND(occupied, bishop_magic(king_square)%mask)
    occupied = occupied * bishop_magic(king_square)%magic
    occupied = ISHFT(occupied, bishop_bits(king_square) - 64)
    checking_pieces = IOR(checking_pieces, IAND(bishop_attacks(king_square, occupied), &
         IOR(enemy_bishops_bitboard, enemy_queens_bitboard)))
    checking_pieces = IOR(checking_pieces, IAND(king_patterns(enemy_king_square), king_bitboard))
    checking_pieces = IOR(checking_pieces, IAND(knight_patterns(king_square), enemy_knights_bitboard))
    backward = king_square + color
    left_backward = MERGE(backward - 8, -1, backward .GE. 8)
    right_backward = MERGE(backward + 8, -1, backward .LE. 55)
    left_backward_bitboard = MERGE(ISHFT(1_8, left_backward), 0_8, left_backward .GE. 0)
    right_backward_bitboard = MERGE(ISHFT(1_8, right_backward), 0_8, right_backward .GE. 0)
    checking_pieces = IOR(checking_pieces, IAND(IOR(left_backward_bitboard, right_backward_bitboard), enemy_pawns_bitboard))
  END FUNCTION checking_pieces

END MODULE move_module
