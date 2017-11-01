MODULE move_module

  TYPE move
     INTEGER(1) :: from_square, to_square, captured_piece, promoted_piece
     LOGICAL :: en_passant
  END TYPE move

END MODULE move_module
