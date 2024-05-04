       IDENTIFICATION DIVISION.
       PROGRAM-ID. SNOBOL.
       DATA DIVISION.
           WORKING-STORAGE SECTION.
               COPY "INPUT/DATA".
               77 X PIC 999.
               77 Y PIC 999.
               77 Z PIC 9999.
               77 FUNCTION_CALL PIC X(3).
               77 TOKEN PIC X.

       PROCEDURE DIVISION.

       MOVE "Enter the first number:" TO INPUT-DATA.
       PERFORM INPUT_FUNCTION.
       ACCEPT X.

       MOVE "Enter the second number:" TO INPUT-DATA.
       PERFORM INPUT_FUNCTION.
       ACCEPT Y.

       DISPLAY "Enter the function you want to perform"
       MOVE "(ADD, MUL, DIV, SUB): " TO INPUT-DATA.
       PERFORM INPUT_FUNCTION.
       ACCEPT FUNCTION_CALL.
 
       EVALUATE FUNCTION_CALL
              WHEN "ADD" PERFORM ADDITION
              WHEN "MUL" PERFORM MULTIPLICATION
              WHEN "DIV" PERFORM DIVIDER
              WHEN "SUB" PERFORM SUBTRACTION
              WHEN OTHER 
               DISPLAY "Invalid function call"
               EXIT PROGRAM
       END-EVALUATE.

       IF Z NOT EQUAL TO ZERO
           DISPLAY "X " TOKEN " Y = " Z
       END-IF.

       DISPLAY     "Do you want to continue? (Y/N)"

       STOP RUN.


       ADDITION SECTION.
           ADD X Y GIVING Z.
           MOVE "+" TO TOKEN.
       
       MULTIPLICATION SECTION.
           MULTIPLY X BY Y GIVING Z.
           MOVE "*" TO TOKEN.

       DIVIDER SECTION.
           DIVIDE X BY Y GIVING Z.
           MOVE "/" TO TOKEN.

       SUBTRACTION SECTION.
           SUBTRACT Y FROM X GIVING Z.
           MOVE "-" TO TOKEN.
       
       COPY "INPUT/FUNCTIONS".
       