MODULE move_module
  IMPLICIT NONE

  INTEGER(8), PARAMETER :: king_patterns(64) = [ &
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

  INTEGER(8), PARAMETER :: knight_patterns(64) = [ &
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

  INTEGER, PARAMETER :: rook_bits(64) = [ &
       12, 11, 11, 11, 11, 11, 11, 12, &
       11, 10, 10, 10, 10, 10, 10, 11, &
       11, 10, 10, 10, 10, 10, 10, 11, &
       11, 10, 10, 10, 10, 10, 10, 11, &
       11, 10, 10, 10, 10, 10, 10, 11, &
       11, 10, 10, 10, 10, 10, 10, 11, &
       11, 10, 10, 10, 10, 10, 10, 11, &
       12, 11, 11, 11, 11, 11, 11, 12]

  INTEGER, PARAMETER :: bishop_bits(64) = [&
       6, 5, 5, 5, 5, 5, 5, 6, &
       5, 5, 5, 5, 5, 5, 5, 5, &
       5, 5, 7, 7, 7, 7, 5, 5, &
       5, 5, 7, 9, 9, 7, 5, 5, &
       5, 5, 7, 9, 9, 7, 5, 5, &
       5, 5, 7, 7, 7, 7, 5, 5, &
       5, 5, 5, 5, 5, 5, 5, 5, &
       6, 5, 5, 5, 5, 5, 5, 6]

  INTEGER(8) :: rook_attacks(0:63, 0:4095), bishop_attacks(0:63, 0:511)

  TYPE move
     INTEGER(1) :: from_square, to_square, captured_piece, promoted_piece
     LOGICAL :: en_passant
  END TYPE move

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

  SUBROUTINE generate_moves_pawn(b, square, buffer, index)
    USE board_module
    TYPE(board), INTENT(in) :: b
    INTEGER, INTENT(in) :: square
    TYPE(move), INTENT(inout) :: buffer(256)
    INTEGER, INTENT(inout) :: index
  END SUBROUTINE generate_moves_pawn
  SUBROUTINE generate_moves_knight(b, square, buffer, index)
    USE board_module
    TYPE(board), INTENT(in) :: b
    INTEGER, INTENT(in) :: square
    TYPE(move), INTENT(inout) :: buffer(256)
    INTEGER, INTENT(inout) :: index
  END SUBROUTINE generate_moves_knight
  SUBROUTINE generate_moves_bishop(b, square, buffer, index)
    USE board_module
    TYPE(board), INTENT(in) :: b
    INTEGER, INTENT(in) :: square
    TYPE(move), INTENT(inout) :: buffer(256)
    INTEGER, INTENT(inout) :: index
  END SUBROUTINE generate_moves_bishop
  SUBROUTINE generate_moves_rook(b, square, buffer, index)
    USE board_module
    TYPE(board), INTENT(in) :: b
    INTEGER, INTENT(in) :: square
    TYPE(move), INTENT(inout) :: buffer(256)
    INTEGER, INTENT(inout) :: index
  END SUBROUTINE generate_moves_rook
  SUBROUTINE generate_moves_queen(b, square, buffer, index)
    USE board_module
    TYPE(board), INTENT(in) :: b
    INTEGER, INTENT(in) :: square
    TYPE(move), INTENT(inout) :: buffer(256)
    INTEGER, INTENT(inout) :: index
  END SUBROUTINE generate_moves_queen
  SUBROUTINE generate_moves_king(b, square, buffer, index)
    USE board_module
    TYPE(board), INTENT(in) :: b
    INTEGER, INTENT(in) :: square
    TYPE(move), INTENT(inout) :: buffer(256)
    INTEGER, INTENT(inout) :: index
  END SUBROUTINE generate_moves_king

  FUNCTION generate_moves(b) RESULT(res)
    USE board_module
    TYPE(board), INTENT(in) :: b
    INTEGER :: square, index
    INTEGER(8) :: own_pieces
    TYPE(move) :: buffer(256)
    TYPE(move), ALLOCATABLE :: res(:)
    index = 1
    own_pieces = b%combined_bitboard(b%side_to_move)
    DO square = 0, 63
       IF (BTEST(own_pieces, square)) THEN
          SELECT CASE (ABS(b%mailbox(square)))
          CASE (pawn)
             CALL generate_moves_pawn(b, square, buffer, index)
          CASE (knight)
             CALL generate_moves_knight(b, square, buffer, index)
          CASE (bishop)
             CALL generate_moves_bishop(b, square, buffer, index)
          CASE (rook)
             CALL generate_moves_rook(b, square, buffer, index)
          CASE (queen)
             CALL generate_moves_queen(b, square, buffer, index)
          CASE (king)
             CALL generate_moves_king(b, square, buffer, index)
          END SELECT
       END  IF
    END DO
  END FUNCTION generate_moves

END MODULE move_module
