       IDENTIFICATION DIVISION.
       PROGRAM-ID. SNOBOL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY "COPYBOOK/INPUT/DATA".
           01 X PIC 999.
           01 Y PIC 999.
           01 Z PIC 9999.
           01 FUNCTION_CALL PIC X(3).
           01 TOKEN PIC X.

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

       DISPLAY "X " TOKEN " Y = " Z

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
       
       COPY "COPYBOOK/INPUT/FUNCTIONS".
       