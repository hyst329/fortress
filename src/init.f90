module init_module

  implicit none

CONTAINS
  SUBROUTINE initialise
    use board_module
    use move_module
    CALL initialise_zobrist_tables
    CALL read_attacks
  END SUBROUTINE initialise

end module init_module
