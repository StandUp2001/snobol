       IDENTIFICATION DIVISION.
       PROGRAM-ID. SNOBOL.
      *A SNAKE GAME IN COBOL
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ASCII-W PIC 9(3) USAGE IS COMP VALUE 119.
       01 ASCII-A PIC 9(3) USAGE IS COMP VALUE 97.
       01 ASCII-S PIC 9(3) USAGE IS COMP VALUE 115.
       01 ASCII-D PIC 9(3) USAGE IS COMP VALUE 100.
       01 ASCII-Q PIC 9(3) USAGE IS COMP VALUE 113.

       01 DIR-UP PIC 9(1) VALUE 1.
       01 DIR-LEFT PIC 9(1) VALUE 2.
       01 DIR-DOWN PIC 9(1) VALUE 3.
       01 DIR-RIGHT PIC 9(1) VALUE 4.
       
       01 VIS-SNAKE PIC X(1) VALUE "O".
       01 VIS-FOOD PIC X(1) VALUE "#".
       01 VIS-BLANK PIC X(1) VALUE ".".

       01 INPUT-CHAR PIC 9(8) USAGE IS COMP.
       01 OLD-DIRECTION PIC 9(1) VALUE 3.
       01 CUR-DIRECTION PIC 9(1) VALUE 3.

       01 ROW_LEN PIC 9 USAGE IS COMP VALUE 9.

       01 SNAKE.
             05 SNAKE-PART OCCURS 81 TIMES INDEXED BY SNAKE-INDEX.
             10 SNAKE-X PIC 9(2).
             10 SNAKE-Y PIC 9(2).
       01 SNAKE-LEN PIC 9(2) USAGE IS COMP VALUE 1.
       01 NEXT-SNAKE-POS.
             10 NEXT-SNAKE-X PIC 9(2).
             10 NEXT-SNAKE-Y PIC 9(2).

       01 FOOD.
             05 FOOD-X PIC 9(2).
             05 FOOD-Y PIC 9(2).

       01 GAME-SCREEN.
           05 SCREEN-ROW OCCURS 9 TIMES.
           10 SCREEN-PIXEL PIC X(1) VALUE '.' OCCURS 9 TIMES.
           10 SCREEN-NL PIC 9(1) USAGE IS COMP VALUE 9.
           05 SCREEN-NULL PIC 9(1) USAGE IS COMP VALUE 0.

       01 CREATE-MORE-FOOD PIC 9(1) VALUE 1.
       01 SNAKE-GREW PIC 9(1) VALUE 1.

       01 MS-COUNT PIC 9(3) USAGE IS COMP.
       01 MS-MOVE-TIME PIC 9(3) USAGE IS COMP VALUE 350.

       PROCEDURE DIVISION.
       
       MAIN.
           CALL "initscr".
           CALL "noecho".
           CALL "timeout" USING BY VALUE 0.

           MOVE VIS-SNAKE TO SCREEN-PIXEL(1, 1).
           MOVE 1 TO SNAKE-X(1), SNAKE-Y(1).
           PERFORM GAME-LOOP.
      *    PERFORM GAME-LOOP WITH TEST AFTER UNTIL INPUT-CHAR = ASCII-Q.

      *    PERFORM GAME-OVER.
              STOP RUN.

       GAME-OVER.
           CALL "endwin".
           DISPLAY "GAME OVER MAN, GAME OVER! SCORE: "SNAKE-LEN.
           STOP RUN.

       GAME-LOOP.
           IF CREATE-MORE-FOOD = 1 THEN
                 PERFORM CREATE-FOOD
           END-IF.

           CALL "clear".
           PERFORM DRAW.
           PERFORM INPUT-PARA.

           IF MS-COUNT > MS-MOVE-TIME THEN
                 PERFORM HANDLE-MOVE
                 MOVE 0 TO MS-COUNT
           END-IF.

           CALL "usleep" USING BY VALUE 1.
           ADD 1 TO MS-COUNT.

       INPUT-PARA.
           CALL "getch" RETURNING INPUT-CHAR.

           EVALUATE TRUE
               WHEN INPUT-CHAR = ASCII-W AND NOT OLD-DIRECTION =
                     DIR-DOWN
                   MOVE DIR-UP TO CUR-DIRECTION
               WHEN INPUT-CHAR = ASCII-A AND NOT OLD-DIRECTION =
                     DIR-RIGHT
                   MOVE DIR-LEFT TO CUR-DIRECTION
               WHEN INPUT-CHAR = ASCII-S AND NOT OLD-DIRECTION =
                     DIR-UP
                   MOVE DIR-DOWN TO CUR-DIRECTION
               WHEN INPUT-CHAR = ASCII-D AND NOT OLD-DIRECTION =
                     DIR-LEFT
                   MOVE DIR-RIGHT TO CUR-DIRECTION
           END-EVALUATE.

       CREATE-FOOD.
           PERFORM GENERATE-FOOD-LOCATION UNTIL SCREEN-PIXEL(FOOD-Y,
           FOOD-X) = VIS-BLANK.
           MOVE VIS-FOOD TO SCREEN-PIXEL(FOOD-Y, FOOD-X).
           MOVE 0 TO CREATE-MORE-FOOD.

       GENERATE-FOOD-LOCATION.
           COMPUTE FOOD-X = FUNCTION RANDOM * 9 + 1.
           COMPUTE FOOD-Y = FUNCTION RANDOM * 9 + 1.

       DRAW.
           CALL "printw" USING GAME-SCREEN.
           CALL "printw" USING "SCORE: %D", BY VALUE SNAKE-LEN.



       SHIFT-SNAKE.
           COMPUTE SNAKE-X(SNAKE-INDEX) = SNAKE-X(SNAKE-INDEX - 1).
           COMPUTE SNAKE-Y(SNAKE-INDEX) = SNAKE-Y(SNAKE-INDEX - 1).

       HANDLE-MOVE.
           PERFORM GET-NEXT-POS.

           MOVE 0 TO SNAKE-GREW.

           IF SCREEN-PIXEL(NEXT-SNAKE-Y, NEXT-SNAKE-X) = "O" THEN
                 PERFORM GAME-OVER
           ELSE
                 IF NEXT-SNAKE-X = FOOD-X AND NEXT-SNAKE-Y = FOOD-Y THEN
                       ADD 1 TO SNAKE-LEN
                       COMPUTE SNAKE-X(SNAKE-LEN) = SNAKE-X(
                             SNAKE-LEN - 1)
                       COMPUTE SNAKE-Y(SNAKE-LEN) = SNAKE-Y(
                             SNAKE-LEN - 1)
                       MOVE 1 TO CREATE-MORE-FOOD
                       MOVE 1 TO SNAKE-GREW
                  END-IF
           END-IF.

           MOVE VIS-SNAKE TO SCREEN-PIXEL(NEXT-SNAKE-Y, NEXT-SNAKE-X).
           IF SNAKE-GREW = 0 THEN
                 MOVE VIS-BLANK TO SCREEN-PIXEL(SNAKE-Y(SNAKE-LEN),
                       SNAKE-X(SNAKE-LEN))
           END-IF.

           PERFORM SHIFT-SNAKE VARYING SNAKE-INDEX FROM SNAKE-LEN BY -1
                 UNTIL SNAKE-INDEX = 1.
      
           MOVE NEXT-SNAKE-X TO SNAKE-X(1).
           MOVE NEXT-SNAKE-Y TO SNAKE-Y(1).
           
           MOVE CUR-DIRECTION TO OLD-DIRECTION.

       GET-NEXT-POS.
           MOVE SNAKE-X(1) TO NEXT-SNAKE-X.
           MOVE SNAKE-Y(1) TO NEXT-SNAKE-Y.
           EVALUATE TRUE
               WHEN CUR-DIRECTION = DIR-UP
                   PERFORM GET-NEXT-POS-UP
               WHEN CUR-DIRECTION = DIR-LEFT
                   PERFORM GET-NEXT-POS-LEFT
               WHEN CUR-DIRECTION = DIR-DOWN
                   PERFORM GET-NEXT-POS-DOWN
               WHEN CUR-DIRECTION = DIR-RIGHT
                   PERFORM GET-NEXT-POS-RIGHT
           END-EVALUATE.

       GET-NEXT-POS-UP.
           IF SNAKE-Y(1) = 1 THEN
                 MOVE 9 TO NEXT-SNAKE-Y
           ELSE
                 SUBTRACT 1 FROM SNAKE-Y(1) GIVING NEXT-SNAKE-Y
           END-IF.

       GET-NEXT-POS-LEFT.
           IF SNAKE-X(1) = 1 THEN
                 MOVE 9 TO NEXT-SNAKE-X
           ELSE
                 SUBTRACT 1 FROM SNAKE-X(1) GIVING NEXT-SNAKE-X
           END-IF.

       GET-NEXT-POS-DOWN.
           IF SNAKE-Y(1) = 9 THEN
                 MOVE 1 TO NEXT-SNAKE-Y
           ELSE
                 ADD 1 TO SNAKE-Y(1) GIVING NEXT-SNAKE-Y
           END-IF.

       GET-NEXT-POS-RIGHT.
           IF SNAKE-X(1) = 9 THEN
                 MOVE 1 TO NEXT-SNAKE-X
           ELSE
                 ADD 1 TO SNAKE-X(1) GIVING NEXT-SNAKE-X
           END-IF.
