# snobol-run needs to be run after snobol-build
snobol-build: ; cobc -O -x -o snobol snobol.cbl
snobol-run: ; cobc -O -x -o snobol snobol.cbl ; ./snobol
# tic-run needs to be run after tic-build
tic-build: ; cobc -O -x -o tictactoe tictactoe.cbl
tic-run: ; cobc -O -x -o tictactoe tictactoe.cbl ; ./tictactoe

clean: ; rm -f snobol tictactoe


