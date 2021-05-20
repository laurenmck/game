B01213242 & B01573046

1. Instructions For Use:
Human player vs. Human player: After the Referee.re file is run, the board will be displayed in the terminal and Player 1 will be prompted to make a move. To make a move, players enter the column number in which s/he would like to place his/her game piece. To move in the first column, enter a 1, to move in the second column, enter a 2, etc. Game pieces are represented by the numbers 1 and 2, for Player 1 and Player 2, respectively. If the move is not valid, "invalid move" will be outputted and the player will be asked to try another move. The game board is then updated with the inputted move and Player 2 will be prompted to make a move. This cycle will continue until a player wins or there is a draw. When the game ends, a message will appear in the terminal telling the users which player won or if it is a tie.

Human Player vs. AI: The human player moves in the same way as described above. When the AI is prompted to make a move, it will automatically do so. The board will be updated after the AI makes its move and the human player will be prompted to make another move. This cycle will continue until a player wins or there is a draw. If the AI player is Player 1, it will make its move first, and then the human player will be prompted to enter a move; if the human player is Player 1, s/he will enter a move before the AI. When the game ends, a message will appear in the terminal telling the users which player won or if it is a tie.

To switch between two human players, one human and one AI player, or two AI players, edit the Ref module at the bottom of the Referee.re file, which appears like so:

module Ref = Referee(Connect4, (HumanPlayer(Connect4)), (AIPlayer(Connect4)));

Currently Player 1 is a Human player and Player 2 is an AI player. To change Player 1 and Player 2 the user needs to substitute the labels "HumanPlayer" and "AIPlayer" for whichever players they would prefer to be Player 1 and Player 2.

To begin playing the game, navigate to the correct directory and run the command "node Referee.bs.js" in the terminal. If the user has chosen to change who is Player 1 or Player 2 before starting the game, run the command "npm run build" before "node Referee.bs.js". To change the size of the board, navigate to "initialRows" and "initialCols" in the Connect4.re file and change those numbers to change the number of rows and number of columns, respectively.


2. There are four main files to the game Connect4: the Referee, the AI player, the Human Player, and the main Connect4.re file. The Referee file is responsible for the user friendly side of the game. The Referee prompts each player to make moves, returns the moves made by each player, prints out the updated version of the board, and starts the games. The AIPlayer.re file includes all the functions that the AI player uses to decide its move given the status of the game; it draws on multiple functions from the Connect4.re file to understand the status of the game and the values of different game states, and uses that information to make moves that maximize the value of the board if it's Player 1 and minimize the value of the board if it's Player 2. This use of the minimax algorithm allows the AI to make the best move given a game state. The Human Player file checks to see if the human player's input is legal, and if so, executes that move. The Connect4.re file includes all of the functions that create, update, and evaluate the state of the game, check for legal moves, and all other functions necessary for the Referee module to execute the game.

3. We are not aware of any bugs in our program.

4. n/a

5. n/a