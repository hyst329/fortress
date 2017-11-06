PROGRAM hello
  USE init_module
  USE board_module
  USE perft_module
  TYPE(board) :: b
  INTEGER :: p, depth
  CALL initialise
  b = board("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  PRINT *, b
  PRINT "(A)", "Fortress v0.0 by trolley.813"
  DO depth = 1, 6
     p = perft(b, depth)
     PRINT *, "depth = ", depth, " p = ", p
  END DO
END PROGRAM hello
