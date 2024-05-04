       IDENTIFICATION DIVISION.
       PROGRAM-ID. TICTACTOE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "COPYBOOK/INPUT/DATA".
       01  COUNTS PIC 9.
       01  BOARD OCCURS 9 TIMES PIC X VALUE SPACE.
           
       01  PLAYERS OCCURS 2 TIMES PIC X VALUE SPACE.
           
       01  CURRENT PIC 9 VALUE 1.

       01  WINNER PIC X VALUE ' '.
       01  INPUT_TEXT PIC 9(9).
       01  INPUT_NUMBER PIC 9 VALUE 0.
       01  EMPTY PIC 9 VALUE 1.

       PROCEDURE DIVISION.
       PERFORM GAME-INIT.
       PERFORM PLAY UNTIL WINNER NOT = ' ' OR EMPTY < 1.
       PERFORM DRAW-BOARD.
       IF EMPTY = 0 AND WINNER = ' '
           DISPLAY "DRAW"
       ELSE
           DISPLAY 'WINNER IS ' WINNER
       END-IF.

       STOP RUN.

       COPY "COPYBOOK/INPUT/FUNCTIONS".

       GAME-INIT SECTION.
           PERFORM VARYING COUNTS FROM 0 BY 1 UNTIL COUNTS > 8
               ADD 1 TO COUNTS
               MOVE COUNTS TO BOARD(COUNTS)
               SUBTRACT 1 FROM COUNTS
           END-PERFORM.
           MOVE "X" TO PLAYERS(1).
           MOVE "O" TO PLAYERS(2).

       DRAW-BOARD SECTION.
           CALL "timeout" USING BY VALUE 0.

           PERFORM VARYING COUNTS FROM 0 BY 3 UNTIL COUNTS > 8
                ADD 1 TO COUNTS
                DISPLAY BOARD(COUNTS) " | " BOARD(COUNTS + 1) " | ",
                 BOARD(COUNTS + 2)
                IF COUNTS < 7
                    DISPLAY "---+---+---"
                END-IF
                SUBTRACT 1 FROM COUNTS
               
           END-PERFORM.


       PLAY SECTION.
           PERFORM DRAW-BOARD.
           PERFORM PLACE-INPUT UNTIL INPUT_NUMBER NOT = 0.
           MOVE 0 TO INPUT_NUMBER.
           PERFORM CHECK-WINNER.

       CHECK-WINNER SECTION.
           IF BOARD(1) = BOARD(2) AND BOARD(2) = BOARD(3)
               MOVE BOARD(1) TO WINNER
           ELSE IF BOARD(4) = BOARD(5) AND BOARD(5) = BOARD(6)
               MOVE BOARD(4) TO WINNER
           ELSE IF BOARD(7) = BOARD(8) AND BOARD(8) = BOARD(9)
               MOVE BOARD(7) TO WINNER
           ELSE IF BOARD(1) = BOARD(4) AND BOARD(4) = BOARD(7)
               MOVE BOARD(1) TO WINNER
           ELSE IF BOARD(2) = BOARD(5) AND BOARD(5) = BOARD(8)
               MOVE BOARD(2) TO WINNER
           ELSE IF BOARD(3) = BOARD(6) AND BOARD(6) = BOARD(9)
               MOVE BOARD(3) TO WINNER
           ELSE IF BOARD(1) = BOARD(5) AND BOARD(5) = BOARD(9)
               MOVE BOARD(1) TO WINNER
           ELSE IF BOARD(3) = BOARD(5) AND BOARD(5) = BOARD(7)
               MOVE BOARD(3) TO WINNER
           ELSE
               MOVE ' ' TO WINNER
           END-IF.
           
           MOVE 0 TO EMPTY
           PERFORM VARYING COUNTS FROM 0 BY 1 UNTIL COUNTS > 8
               ADD 1 TO COUNTS
                IF BOARD(COUNTS) NOT = "X" AND BOARD(COUNTS) NOT = "O"
                     ADD 1 TO EMPTY
                END-IF
                SUBTRACT 1 FROM COUNTS
           END-PERFORM.

       PLACE-INPUT SECTION.
           STRING "PLAYER " PLAYERS(CURRENT) " ENTER YOUR MOVE (1-9): "
               DELIMITED BY SIZE INTO INPUT_DATA.
           PERFORM INPUT_FUNCTION.
           ACCEPT INPUT_TEXT.

           IF INPUT_TEXT > 0 AND INPUT_TEXT < 10
               MOVE INPUT_TEXT TO INPUT_NUMBER
               DISPLAY "INPUT NUMBER IS " INPUT_NUMBER
               IF BOARD(INPUT_NUMBER) = "X"
                   OR BOARD(INPUT_NUMBER) = "O"
                   DISPLAY "NUMBER ALREADY USED"
                   MOVE 0 TO INPUT_NUMBER
               ELSE
                   PERFORM UPDATE-BOARD
               END-IF
           ELSE
                DISPLAY "INVALID NUMBER"
           END-IF.

       UPDATE-BOARD SECTION.
           MOVE PLAYERS(CURRENT) TO BOARD(INPUT_NUMBER)
           IF CURRENT = 1
               MOVE 2 TO CURRENT
           ELSE
               MOVE 1 TO CURRENT
           END-IF.
