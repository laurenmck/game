open CS17SetupGame;
open SigGame;

/* Data Definitions and Example Data Key:

   whichPlayer:
   Definition: P1 or P2, which represent Player 1 and Player 2, respectively.
   Example Data: P1, P2

   status:
   Definition: the status of the game: whether someone has won, and if so, who won,
   whether it is a tie, or if the game is ongoing, and if so, whose turn it is.
   Example Data: Win(P2), Draw, Ongoing(P1)

   move:
   Definition: a valid move in Connect4, represented by Move(int), where the int
   represents the column into which the player drops their token.
   Example Data: Move(1), Move(6)

   board:
   Definition: a list of lists of ints. The inner lists represent each column, with
   each element within those lists representing a row in that column, so that
   the head of the list is the first row.
   Example Data: [[0, 0, 0, 0], [1, 1, 0, 0], [2, 0, 0, 0]]

   state:
   Definition: a description of the status and the board, represented as
   State(status, board).
   Example Data: State(Ongoing(P2), [[0, 0, 0, 0], [1, 1, 0, 0], [2, 0, 0, 0]]),
   State(Win(P1), [[0, 0, 0, 0], [1, 1, 1, 1], [2, 0, 0, 0]])

   initialState:
   Definition: the initial state of the game, so that the status is Ongoing(P1)
   and the board has the appropriate dimensions as defined in initialRows and
   initialCols, and is filled exclusively with zeroes.

   All other data types used within this file are atomic data types or lists
   of atomic data types and therefore do not require data definitions or example
   data.
   */

module Connect4 = {
  /* sets the number of rows in the board for initialBoard procedure */
  let initialRows = 5;
  /* sets the number of columns in the board for initialBoard procedure */
  let initialCols = 7;

  /* Data Definitions and Example Data: refer to key */
  type whichPlayer =
    | P1
    | P2;

  /* Data Definitions and Example Data: refer to key */
  type status =
    | Win(whichPlayer)
    | Draw
    | Ongoing(whichPlayer);

  /* Data Definitions and Example Data: refer to key */
  type move =
    | Move(int);

  /* Data Definitions and Example Data: refer to key */
  type board = list(list(int));

  /* Data Definitions and Example Data: refer to key */
  type state =
    | State(status, board);

  /* Data Definitions and Example Data: refer to key

     initialBoard : (int, int) => board

     Input: an (int, int) tuple, (n, m), where n is the number of rows and m
     is the number of columns
     Output: a board containing n amount of initialRows and m amount of
     initialColumns

     RD:
     OI: (4, 4)
     RI: (4, 3)
     RO: [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
     ID: cons the result of calling initialRow(n) to recursive output
     00: [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]] */

  let rec initialBoard: (int, int) => board =
    (n, m) => {
      let rec initialRow: int => list(int) =
        n =>
          switch (n) {
          | 0 => []
          | _ => [0, ...initialRow(n - 1)]
          };
      switch (m) {
      | 0 => []
      | _ => [initialRow(n), ...initialBoard(n, m - 1)]
      };
    };

  /* Data Definition and Example Data: refer to key
     initialState represents the original state of the game;
     in this case representing what player goes first and what the initialBoard
     looks like when the game is started. In our implementation of connect 4,
     player 1 always goes first and the board is initially filled with all zeros.
     To fill the board, we call initialBoard with initial rows and initial cols
     as inputs to return */

  let initialState =
    State(Ongoing(P1), initialBoard(initialRows, initialCols));

  /* Data Definition and Example Data: refer to key

     stringOfPlayer: whichPlayer => string
     input: a player of type whichPlayer, player
     output: a string representing the player
     */

  let stringOfPlayer: whichPlayer => string =
    player =>
      switch (player) {
      | P1 => "Player 1"
      | P2 => "Player 2"
      };

  /* Data Definition and Example Data: refer to key

     stringOfStatus: status => string
     input: a game status, s
     output: s string, s, representing the status of the game in readable terms
     */

  let stringOfStatus: status => string =
    s =>
      switch (s) {
      | Win(a) => stringOfPlayer(a) ++ "wins!"
      | Draw => "Draw"
      | Ongoing(b) => stringOfPlayer(b) ++ "'s turn"
      };

  /* Data Definition and Example Data: refer to key

     transpose: board => board
     input: a game board
     output: a game board, b, the input board trnsposed, that is returns a board
     whose rows are the columns of the original.

     RD:
     OI: [[1, 2], [3, 4], [5, 6], [7, 8]]
     RI: [[3, 4], [5, 6], [7, 8]]
     RO: [[3, 5, 7], [4, 6, 8]]
     I: adds the items of the first list to the hd of the new cols
     OO: [[1, 3, 5, 7], [2, 4, 6, 8]]
     */

  let rec transpose: board => board =
    b =>
      switch (b) {
      | [] => []
      | [[], ..._] =>
        failwith("will not reach this 0-dimensional board case")
      | [[_], ..._] => [List.flatten(b)]
      | [[_, ..._], ..._] => [
          List.map(List.hd, b),
          ...transpose(List.map(List.tl, b)),
        ]
      };

  /* Data Definition and Example Data: refer to key

     printListString: list(int) => string
     input: a list of ints, lst
     output: a string of the input list

     RD
     OI: [4, 5, 6]
     RI: [5, 6]
     RO: "5, 6"
     I: adds the first int in the list to the RO string
     OO: "4, 5, 6"
     */

  let rec printListString: list(int) => string =
    lst =>
      switch (lst) {
      | [] => ""
      | [hd, ...tl] => string_of_int(hd) ++ " " ++ printListString(tl)
      };

  /* Data Definition and Example Data: refer to key

     printBoardString: board => list(string)
     input: a board, b
     output: a board reprented as a list of strings
     (i.e in a userfriendly/readable way)
     */

  let printBoardString: board => list(string) =
    b => List.map(printListString, b);

  /* Data Definition and Example Data: refer to key

     printBoard: board => string
     input: a board, b
     output: a string representing the input board
     */

  let printBoard: board => string =
    b => {
      let rec printBoardHelper: list(string) => string =
        lst =>
          switch (lst) {
          | [] => ""
          | [hd, ...tl] => "\n" ++ hd ++ "\n" ++ printBoardHelper(tl)
          };

      printBoardHelper(printBoardString(List.rev(transpose(b))));
    };

  /* Data Definition and Example Data: refer to key

     stringOfState: State => string
     input: a state, State(s, b)
     output: a string representing the current state of the game,
     i.e the current status and the board
     */

  let stringOfState = (State(s, b): state): string =>
    stringOfStatus(s) ++ printBoard(b);

  /* Data Definition and Example Data: refer to key

     stringOfMove: move => string
     input: a move, Move(i)
     output: the col(int) of the move move represented as a string
     */

  let stringOfMove = (Move(i): move): string => string_of_int(i);

  /* Data Definition and Example Data: refer to key

     moveOfString: string => move
     input: a string, str
     output: the input string represented as type move
     */

  let moveOfString = (str: string): move => Move(int_of_string(str));

  /* Data Definition and Example Data: refer to key

     legalMoves= State(s, b) => list(move)
     input: takes in a game state, the game status, s, and a board, b
     output: a list of moves a plauer can legally make

     legalMovesHelper
     input: a board, b, extravted from the legal moves state input and an int, i,
     that keeps track of the cols in the board
     output: a list of moves a player can legally make

     RD for legalMovesHelper
     OI: [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]], 1
     RI: [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]], 2
     RO: [Move(2), Move(3), Move(4)]
     I: the board is empty so the player can place a "token" in all 4 cols
     OO: [Move(1), Move(2), Move(3), Move(4)]

     OI: [[2, 0, 0, 0], [1, 2, 1, 1], [2, 0, 0, 0], [0, 0, 0, 0]], 1
     RI: [[1, 2, 1, 1], [2, 0, 0, 0], [0, 0, 0, 0]], 2
     RO: [Move(3), Move(4)]
     I: col 2 is full, therefore it is not a leagal move and is not added to the
     output list
     OO: [Move(1), Move(3), Move(4)]
     */

  let legalMoves = (State(_, b): state): list(move) => {
    let rec legalMovesHelper: (board, int) => list(move) =
      (b, i) =>
        switch (b) {
        | [[]] => []
        | [[hd, ...tl]] =>
          if (List.mem(0, [hd, ...tl])) {
            [Move(i)];
          } else {
            legalMovesHelper([tl], i);
          }
        | [[hd, ...tl], ...tl1] =>
          if (List.mem(0, [hd, ...tl])) {
            [Move(i), ...legalMovesHelper(tl1, i + 1)];
          } else {
            legalMovesHelper(tl1, i + 1);
          }
        | _ => failwith("will not reach this legalMovesHelper case")
        };

    legalMovesHelper(b, 1);
  };

  /* Data Definition and Example Data: refer to key

     gameStatus: state => status
     input: a game status, State(s, _)
     output: returns the game status from the given gams state
     */

  let gameStatus = (State(s, _): state): status => s;

  /* Data Definition and Example Data: refer to key
        correctColumn : (board, int) => list(int)

        Input: a board, b, and an int, i, representing the integer in a move
        Output: the column, represented by a list of ints, which the move wants
        to insert in.

     correctColumn RD
     OI: [[1, 1], [0, 0], [2, 2]], 2
     RI: [[0, 0], [2, 2]], 1
     RO: [0, 0]
     I: returns [0, 0] becasue it is the first col in the board when the input
     int is 1 therefore it is the col that the move it refering to
     OO: [0, 0]

     OI: [[1, 1, 1], [0, 0, 0]], 1
     RI: n/a
     RO: n/a
     I: the input i is one so no recursion takes place
     and the first col of the board is returned
     OO: [1, 1, 1]
     */

  let rec correctColumn: (board, int) => list(int) =
    (b, i) =>
      switch (b) {
      | [] => []
      | [[hd, ...tl], ...tl2] =>
        if (i == 1) {
          [hd, ...tl];
        } else {
          correctColumn(tl2, i - 1);
        }
      | _ => failwith("will not reach this correctColumn case")
      };

  /* Data Definition and Example Data: refer to key

     replacer: (list(int), status) => list(int)

     Input: a list of ints, lst, and a status, s
     Output: a column, represented by a list of ints, with a move inserted

     replacer RD:
     OI: [0, 0, 0], Ongoing(P1)
     RI: n/a
     RO: n/a
     I: the hd of the list is zero - meaning there are no items in the col,
     so P1's token is placed at the hd of the list
     OO: [1, 0, 0]

     OI: [1, 0, 0], Ongoing(P2)
     RI: [0, 0], Ongoing(P2)
     RO: [2, 0]
     I: the list is recurred through until a zero,
     the first zero found is replaced by a 2
     OO:  [1, 2, 0]
     */

  let rec replacer: (list(int), status) => list(int) =
    (lst, s) =>
      switch (lst) {
      | [] => []
      | [hd, ...tl] =>
        switch (hd, s) {
        | (0, Ongoing(P1)) => [1, ...tl]
        | (_, Ongoing(P1)) => [hd, ...replacer(tl, s)]
        | (0, Ongoing(P2)) => [2, ...tl]
        | (_, Ongoing(P2)) => [hd, ...replacer(tl, s)]
        | (_, Draw) => [hd, ...tl]
        | (_, Win(_)) => [hd, ...tl]
        }
      };

  /* Data Definitions and Example Data: refer to key

     drop : board => board

     Input: a board, b
     Output: a board with its final column removed

     Drop RD:
     OI: [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
     RI: [[0, 0, 0], [0, 0, 0]]
     RO: [0, 0, 0]
     I:  returns the origional board without the tail
     OO: [[0, 0, 0], [0, 0, 0]]

     OI: [[2, 2], [1, 1]]
     RI: [[1, 1]]
     RO: []
     I:  returns the origional board without the tail
     OO: [[2, 2]]
     */

  let rec drop: board => board =
    b =>
      switch (b) {
      | [] => []
      | [_] => []
      | [hd, ...tl] => [hd, ...drop(tl)]
      };

  /* Data Definition and Example Data: refer to key

     updateBoard: (board, move, status) => board
     input: the current game board, b, a move made by a player, Move(i),
     and a game status, s
     output: the input board updated to include the input move and a game status, s
     */

  let updateBoard: (board, move, status) => board =
    (b, Move(i), s) => {
      let rec updateBoardHelper: (board, int, status) => board =
        (b, i, s) =>
          switch (i) {
          | 0 => failwith("invalid move, not a column")
          | 1 => [replacer(correctColumn(b, i), s), ...List.tl(b)]
          | _ =>
            if (i == List.length(b)) {
              List.append(drop(b), [replacer(correctColumn(b, i), s)]);
            } else {
              [List.hd(b), ...updateBoardHelper(List.tl(b), i - 1, s)];
            }
          };

      updateBoardHelper(b, i, s);
    };

  /*
   Data Definition and Example Data: refer to key

   checkForZeroes: board => bool

   Input: a board, b
   Output: a boolean: true if there is a zero (representing an open space) in the
   board, and false otherwise

   recursion diagrams
   OI: [[[1, 1, 1], [0, 0, 0], [2, 2, 2]]
   RI: [[0, 0, 0], [2, 2, 2]]
   RO: true
   I: finds zeros in the second list and returns true
   OO: true

   OI: [[0, 0], [1, 1]]
   RI: n/a
   RO: n/a
   I: finds zeros in the first list so short cit=rcuits and returns true
   OO: true
   */

  let rec checkForZeroes: board => bool =
    b =>
      switch (b) {
      | [] => false
      | [hd, ...tl] => List.mem(0, hd) || checkForZeroes(tl)
      };

  /* Data Definition and Example Data: refer to key

     oppositePlayer: status => status
     input: s status, s
     output: a status with the opposite player than the player in the input status
     */

  let oppositePlayer: status => status =
    s =>
      if (s == Ongoing(P1)) {
        Ongoing(P2);
      } else {
        Ongoing(P1);
      };

  /* Data Definition and Example Data: refer to key

     vertCount4: board => (bool, int)
     input: the current game board, b
     output: a bool, int touple - the bool represents if one of the players has a
     vertical run of 4 and the int represnets what player has the run.
     Returns 0 if the bool is false. */

  let vertCount4: board => ((bool, int), (bool, int)) =
    b => {
      let vertCount4Helper1: board => list((bool, int)) =
        b => {
          let rec vertCount4Helper2: list(int) => (bool, int) =
            lst =>
              switch (lst) {
              | [] => (false, 0)
              | [hd1, hd2, hd3, hd4, ...tl] =>
                if ((hd1 == hd2 && hd2 == hd3) && hd3 == hd4 && hd1 != 0) {
                  (true, hd1);
                } else {
                  vertCount4Helper2([hd2, hd3, hd4, ...tl]);
                }
              | _ => (false, 0)
              };

          List.map(vertCount4Helper2, b);
        };

      switch (
        List.mem((true, 1), vertCount4Helper1(b)),
        List.mem((true, 2), vertCount4Helper1(b)),
      ) {
      | (true, true) => ((true, 1), (true, 2))
      | (true, false) => ((true, 1), (false, 2))
      | (false, true) => ((false, 1), (true, 2))
      | (false, false) => ((false, 1), (false, 2))
      };
    };

  /* Data Definition and Example Data: refer to key

     vertCount3: board => ((bool, int), (bool, int))
     input: a board, b
     output: two bool int touples, signifying if if there are 3 in a row vertically
     with an open space following (i.e. it could become 4 with an additional move).
     Int represnets what player has the run. returns 0 if the bool is false.
     */
  let vertCount3: board => ((bool, int), (bool, int)) =
    b => {
      let vertCount3Helper1: board => list((bool, int)) =
        b => {
          let rec vertCount3Helper2: list(int) => (bool, int) =
            lst =>
              switch (lst) {
              | [] => (false, 0)
              | [hd1, hd2, hd3, hd4, ...tl] =>
                if ((hd1 == hd2 && hd2 == hd3) && hd4 == 0 && hd1 != 0) {
                  (true, hd1);
                } else {
                  vertCount3Helper2([hd2, hd3, hd4, ...tl]);
                }
              | _ => (false, 0)
              };

          List.map(vertCount3Helper2, b);
        };

      switch (
        List.mem((true, 1), vertCount3Helper1(b)),
        List.mem((true, 2), vertCount3Helper1(b)),
      ) {
      | (true, true) => ((true, 1), (true, 2))
      | (true, false) => ((true, 1), (false, 2))
      | (false, true) => ((false, 1), (true, 2))
      | (false, false) => ((false, 1), (false, 2))
      };
    };

  /* Data Definition and Example Data: refer to key
     vertCount3Hole2: board => ((bool, int), (bool, int))
     input: a board, b
     output: two bool int touples checking for a list like [1, 0, 1, 1],
     where there are 3 in a row with an open space in the second slot,
     the int represents what player has the run, if false a 0 is returned */

  let vertCount3Hole2: board => ((bool, int), (bool, int)) =
    b => {
      let vertCount3Hole2Helper1: board => list((bool, int)) =
        b => {
          let rec vertCount3Hole2Helper2: list(int) => (bool, int) =
            lst =>
              switch (lst) {
              | [] => (false, 0)
              | [hd1, hd2, hd3, hd4, ...tl] =>
                if ((hd1 == hd3 && hd2 == 0) && hd3 == hd4 && hd1 != 0) {
                  (true, hd1);
                } else {
                  vertCount3Hole2Helper2([hd2, hd3, hd4, ...tl]);
                }
              | _ => (false, 0)
              };

          List.map(vertCount3Hole2Helper2, b);
        };

      switch (
        List.mem((true, 1), vertCount3Hole2Helper1(b)),
        List.mem((true, 2), vertCount3Hole2Helper1(b)),
      ) {
      | (true, true) => ((true, 1), (true, 2))
      | (true, false) => ((true, 1), (false, 2))
      | (false, true) => ((false, 1), (true, 2))
      | (false, false) => ((false, 1), (false, 2))
      };
    };

  /*
   Data Definition and Example Data: refer to key
   input: a board, b
   vertCount3Hole3: board => ((bool, int), (bool, int))
   input: a bord, b
   output: two bool int touples checking for a list like [1, 1, 0, 1],
   where there are 3 in a row with an open space in the third slot,
   the int represents what player has the run, if false a 0 is returned */

  let vertCount3Hole3: board => ((bool, int), (bool, int)) =
    b => {
      let vertCount3Hole3Helper1: board => list((bool, int)) =
        b => {
          let rec vertCount3Hole3Helper2: list(int) => (bool, int) =
            lst =>
              switch (lst) {
              | [] => (false, 0)
              | [hd1, hd2, hd3, hd4, ...tl] =>
                if ((hd1 == hd2 && hd3 == 0) && hd2 == hd4 && hd1 != 0) {
                  (true, hd1);
                } else {
                  vertCount3Hole3Helper2([hd2, hd3, hd4, ...tl]);
                }
              | _ => (false, 0)
              };

          List.map(vertCount3Hole3Helper2, b);
        };

      switch (
        List.mem((true, 1), vertCount3Hole3Helper1(b)),
        List.mem((true, 2), vertCount3Hole3Helper1(b)),
      ) {
      | (true, true) => ((true, 1), (true, 2))
      | (true, false) => ((true, 1), (false, 2))
      | (false, true) => ((false, 1), (true, 2))
      | (false, false) => ((false, 1), (false, 2))
      };
    };

  /*
   Data Definition and Example Data: refer to key

   vertCount2: board => ((bool, int), (bool, int))
   input: a board, b
   output: two bool int touples representing if there are 2 in a row vertically
   with an open space following (i.e. it could become 3 with an additional move),
   the int represents what player has the run, if false a 0 is returned */

  let vertCount2: board => ((bool, int), (bool, int)) =
    b => {
      let vertCount2Helper1: board => list((bool, int)) =
        b => {
          let rec vertCount2Helper2: list(int) => (bool, int) =
            lst =>
              switch (lst) {
              | [] => (false, 0)
              | [hd1, hd2, hd3, ...tl] =>
                if (hd1 == hd2 && hd3 == 0 && hd1 != 0) {
                  (true, hd1);
                } else {
                  vertCount2Helper2([hd2, hd3, ...tl]);
                }
              | _ => (false, 0)
              };

          List.map(vertCount2Helper2, b);
        };

      switch (
        List.mem((true, 1), vertCount2Helper1(b)),
        List.mem((true, 2), vertCount2Helper1(b)),
      ) {
      | (true, true) => ((true, 1), (true, 2))
      | (true, false) => ((true, 1), (false, 2))
      | (false, true) => ((false, 1), (true, 2))
      | (false, false) => ((false, 1), (false, 2))
      };
    };

  /* Data Definitions and Example Data: refer to key

     horzCount4: board => ((bool, int), (bool, int))

     Input: a board, b
     Output: a ((bool, int), (bool, int)) tuple, the first tuple checking to see if
     P1 has 4 in a row horizontally, and the second checking for P2. */
  let horzCount4: board => ((bool, int), (bool, int)) =
    b => vertCount4(transpose(b));

  /* Data Definitions and Example Data: refer to key

     horzCount3: board => ((bool, int), (bool, int))

     Input: a board, b
     Output: a ((bool, int), (bool, int)) tuple, the first tuple checking to see if
     P1 has 3 in a row horizontally with an open space following
     (i.e. it could become 4 with an additional move),
     and the second checking for P2. */
  let horzCount3: board => ((bool, int), (bool, int)) =
    b => vertCount3(transpose(b));

  /* Data Definitions and Example Data: refer to key

     horzCount2: board => ((bool, int), (bool, int))

     Input: a board, b
     Output: a ((bool, int), (bool, int)) tuple, the first tuple checking to see if
     P1 has 2 in a row horizontally with an open space following
     (i.e. it could become 4 with an additional move),
     and the second checking for P2. */
  let horzCount2: board => ((bool, int), (bool, int)) =
    b => vertCount2(transpose(b));

  /* Data Definitions and Example Data: refer to key
     diag: board => list(list(int))
     input: the current game board
     output: a list of lists of the diagonals of the input board */
  let diag: board => list(list(int)) =
    l => {
      let intlst: int => list(int) = i => [i];
      let rec helper: (board, board) => board =
        (nl, ol) => {
          let rec helper2: (board, list(int)) => board =
            (nl, ohd) =>
              switch (nl, ohd) {
              | ([], _) => List.map(intlst, ohd)
              | ([hdn, ...tln], [hdo, ...tlo]) => [
                  [hdo, ...hdn],
                  ...helper2(tln, tlo),
                ]
              | _ => failwith("will not reach this diaghelper2 case")
              };
          switch (nl, ol) {
          | (_, []) => nl
          | ([hdn, ...tln], [hdo, ...tlo]) => [
              hdn,
              ...helper(helper2(tln, hdo), tlo),
            ]
          | _ => failwith("will not reach this diag helper case")
          };
        };
      switch (l) {
      | [] => failwith("will not reach this diag empty board case")
      | [hd, ...tl2] => helper(List.map(intlst, hd), tl2)
      };
    };

  /*  Data Definition and Example Data: refer to key

      diagCount4Up: board => ((bool, int), (bool, int))

      input: a board, s
      output: two bool int touples checking the upward diagonals for 4,
      the int represemts what player has the diagonal, returns 0 if false */
  let diagCount4Up: board => ((bool, int), (bool, int)) =
    b => vertCount4(diag(b));

  /*  Data Definition and Example Data: refer to key

      diagCount4Down: board => ((bool, int), (bool, int))

      input: a board, b
      output: two bool int touples checking the downward diagonals for 4,
      the int represemts what player has the diagonal, returns 0 if false */
  let diagCount4Down: board => ((bool, int), (bool, int)) =
    b => vertCount4(diag(List.rev(b)));

  /*  Data Definition and Example Data: refer to key

      diagCount3Up: board => ((bool, int), (bool, int))

      input: a board, b
      output: two bool int touples checking the up diagonals for 3,
      the int represemts what player has the run, returns 0 if false */
  let diagCount3Up: board => ((bool, int), (bool, int)) =
    b => vertCount3(diag(b));

  /* Data Definition and Example Data: refer to key

     diagCount3Down: board => ((bool, int), (bool, int))

     input: a board, b
     output: two bool int touples checking the dowm diagonals for 3, the int
     represemts what player has the run, returns 0 if false */
  let diagCount3Down: board => ((bool, int), (bool, int)) =
    b => vertCount3(diag(List.rev(b)));

  /* Data Definition and Example Data: refer to key

     diagCount2Up: board => ((bool, int), (bool, int))

     input: a board, b
     output: two bool int touples checking the up diagonals for 2, the int
     represemts what player has the run, returns 0 if false */
  let diagCount2Up: board => ((bool, int), (bool, int)) =
    b => vertCount2(diag(b));

  /* Data Definition and Example Data: refer to key

     diagCount2Down: board => ((bool, int), (bool, int))

     input: a board, b
     output: two bool int touples checking the down diagonals for 2, the int
     represemts what player has the run, returns 0 if false */
  let diagCount2Down: board => ((bool, int), (bool, int)) =
    b => vertCount2(diag(List.rev(b)));

  /* Data Definition and Example Data: refer to key

     horzCount3Hole2: board => ((bool, int), (bool, int))

     input: a board, b
     output: two bool int touples checking for a row t like 1, 0, 1, 1 where there
     are 3 in a row with an open space in the second slot, the int represents what
     player has the run, if false a 0 is returned */
  let horzCount3Hole2: board => ((bool, int), (bool, int)) =
    b => vertCount3Hole2(transpose(b));

  /* Data Definition and Example Data: refer to key

     horzCount3Hole3: board => ((bool, int), (bool, int))

     input: a board, b
     output: two bool int touples checking for a row 1, 1, 0, 1 where there are 3 in
     a row with an open space in the third slot, the int represents what player has
     the run, if false a 0 is returned */
  let horzCount3Hole3: board => ((bool, int), (bool, int)) =
    b => vertCount3Hole3(transpose(b));

  /* Data Definitions and Example Data: refer to key

     diagCount3UpHole2: board => ((bool, int), (bool, int))

     input: a board, b
     output: two bool int touples checking for an upwards diagonal run like
     1, 0, 1, 1 where there are 3 in a row with an open space in the second slot,
     the int represents what player has the run, if false a 0 is returned */
  let diagCount3UpHole2: board => ((bool, int), (bool, int)) =
    b => vertCount3Hole2(diag(b));

  /* Data Definitions and Example Data: refer to key

     diagCount3DownHole2: board => ((bool, int), (bool, int))

     input: a board, b
     output: two bool int touples checking for a reverse down diagonal run like
     1, 0,1, 1 where there are 3 in a row with an open space in the second slot,
     the int represents what player has the run, if false a 0 is returned */
  let diagCount3DownHole2: board => ((bool, int), (bool, int)) =
    b => vertCount3Hole2(diag(List.rev(b)));

  /* Data Definitions and Example Data: refer to key

     diagCount3UpHole3: board => ((bool, int), (bool, int))

     input: a borad, b
     output: two bool, int touples checking for an upward diagonal like
     1, 1, 0, 1 where there are 3 in a row with an open space in the third slot,
     the int represents what player has the run, if false a 0 is returned */
  let diagCount3UpHole3: board => ((bool, int), (bool, int)) =
    b => vertCount3Hole3(diag(b));

  /* Data Definitions and Example Data: refer to key

     diagCount3DownHole3: board => ((bool, int), (bool, int))

     input: a board, b
     output: two bool, int touples checking for a reverse downward diagonal run like
     1, 1, 0, 1 where there are 3 in a row with an open space in the third slot,
     the int represents what player has the run, if false a 0 is returned */
  let diagCount3DownHole3: board => ((bool, int), (bool, int)) =
    b => vertCount3Hole3(diag(List.rev(b)));

  /* Data Definition and Example Data: refer to key

     updateStatus : (status, move, board) => status

     input: the current game status, s, the current game borad b, and move, m
     output: the updated game status after the move is made */
  let updateStatus: (status, move, board) => status =
    (s, m, b) =>
      switch (s) {
      | Win(x) => Win(x)
      | Draw => Draw
      | Ongoing(_) =>
        switch (updateBoard(b, m, s)) {
        | [] => failwith("will not reach this empty updateStatus case")
        | [hd, ...tl] =>
          switch (vertCount4([hd, ...tl])) {
          | ((true, 1), _) => Win(P1)
          | (_, (true, 2)) => Win(P2)
          | ((false, 1), (false, 2)) =>
            switch (horzCount4([hd, ...tl])) {
            | ((true, 1), _) => Win(P1)
            | (_, (true, 2)) => Win(P2)
            | ((false, 1), (false, 2)) =>
              switch (diagCount4Up([hd, ...tl])) {
              | ((true, 1), _) => Win(P1)
              | (_, (true, 2)) => Win(P2)
              | ((false, 1), (false, 2)) =>
                switch (diagCount4Down([hd, ...tl])) {
                | ((true, 1), _) => Win(P1)
                | (_, (true, 2)) => Win(P2)
                | ((false, 1), (false, 2)) =>
                  if (checkForZeroes([hd, ...tl])) {
                    oppositePlayer(s);
                  } else {
                    Draw;
                  }
                | _ => failwith("will not reach this updateStatus case")
                }
              | _ => failwith("will not reach this updateStatus case")
              }
            | _ => failwith("will not reach this updateStatus case")
            }
          | _ => failwith("will not reach this updateStatus case")
          }
        }
      };

  /* Data Definitions and Example Data: refer to key

     nextState: (state, move) => state

     Input: a state, State(s, b), and a move, m
     Output: a state whose status and board have been updated given the input move */

  let nextState: (state, move) => state =
    (State(s, b), m) =>
      State(updateStatus(s, m, b), updateBoard(b, m, s));

  /*  Data Definitions and Example Data: refer to key

      howManyPieces : board => int

      Input: a board, b
      Output: the number of pieces in the board, i.e. the number of moves that
      have been made

      Recursion diagrams:

      OI: [[0, 0], [0, 1]]
      RI: [[0, 0]] && [[0, 1]]
      RO: 0 && 1
      I: checks both individual lists for numbers other than zero and adds on
      the count each time a one is foundm, makes two recursive calls
      OO: 1

      OI: [[1, 0, 0, 0]]
      RI: [0, 0, 0]
      RO: 0
      I: checks each item in the list and adds one when a non zero val is found
      OO: 1 */

  let rec howManyPieces: board => int =
    b =>
      switch (b) {
      | [[]] => 0
      | [[hd, ...tl]] =>
        if (hd == 0) {
          howManyPieces([tl]);
        } else {
          1 + howManyPieces([tl]);
        }
      | [[hd, ...tl], ...tl1] =>
        howManyPieces([[hd, ...tl]]) + howManyPieces(tl1)
      | _ => failwith("will not reach this howManyPieces case")
      };

  /* Data Definitions and Example Data: refer to key

     result2 : board => list(((bool, int), (bool, int)))

     Input: a board, b
     Output: a list of ((bool, int), (bool, int)) tuples which compile the results
     of horzCount2, vertCount2, diagCount2Up, and diagCount2Down into a single
     list. */

  let result2: board => list(((bool, int), (bool, int))) =
    b => [
      vertCount2(b),
      horzCount2(b),
      diagCount2Up(b),
      diagCount2Down(b),
    ];

  /* Data Definitions and Example Data: refer to key

     count1Trues2 : board => int

     Input: a board, b
     Output: an int, which is the number of times that the functions within result2
     returned true for P1. */

  let count1Trues2: board => int =
    b => {
      let rec count1Trues2Helper: list(((bool, int), (bool, int))) => int =
        lst =>
          switch (lst) {
          | [] => 0
          | [hd, ...tl] =>
            switch (hd) {
            | ((true, 1), _) => 1 + count1Trues2Helper(tl)
            | ((false, 1), _) => count1Trues2Helper(tl)
            | _ => failwith("will not reach this count1Trues2 case")
            }
          };

      count1Trues2Helper(result2(b));
    };

  /* Data Definitions and Example Data: refer to key

     count1Trues2 : board => int

     Input: a board, b
     Output: an int, which is the number of times that the functions within result2
     returned true for P2. */

  let count2Trues2: board => int =
    b => {
      let rec count2Trues2Helper: list(((bool, int), (bool, int))) => int =
        lst =>
          switch (lst) {
          | [] => 0
          | [hd, ...tl] =>
            switch (hd) {
            | (_, (true, 2)) => 1 + count2Trues2Helper(tl)
            | (_, (false, 2)) => count2Trues2Helper(tl)
            | _ => failwith("will not reach this count2Trues2 case")
            }
          };

      count2Trues2Helper(result2(b));
    };

  /* Data Definitions and Example Data: refer to key

     result3 : board => list(((bool, int), (bool, int)))

     Input: a board, b
     Output: a list of ((bool, int), (bool, int)) tuples which compile the results
     of vertCount3, horzCount3, horzCount3Hole2, horzCount3Hole3,
     diagCount3Up, diagCount3Down, diagCount3UpHole2, diagCount3UpHole3,
     diagCount3DownHole2, and diagCount3DownHole3 into a single list. */

  let result3: board => list(((bool, int), (bool, int))) =
    b => [
      vertCount3(b),
      horzCount3(b),
      horzCount3Hole2(b),
      horzCount3Hole3(b),
      diagCount3Up(b),
      diagCount3Down(b),
      diagCount3UpHole2(b),
      diagCount3UpHole3(b),
      diagCount3DownHole2(b),
      diagCount3DownHole3(b),
    ];

  /* Data Definitions and Example Data: refer to key

     count1Trues3 : board => int

     Input: a board, b
     Output: an int, which is the number of times that the functions within result3
     returned true for P1. */

  let count1Trues3: board => int =
    b => {
      let rec count1Trues3Helper: list(((bool, int), (bool, int))) => int =
        lst =>
          switch (lst) {
          | [] => 0
          | [hd, ...tl] =>
            switch (hd) {
            | ((true, 1), _) => 1 + count1Trues3Helper(tl)
            | ((false, 1), _) => count1Trues3Helper(tl)
            | _ => failwith("will not reach this count1Trues3 case")
            }
          };

      count1Trues3Helper(result3(b));
    };

  /* Data Definitions and Example Data: refer to key

     count2Trues3 : board => int

     Input: a board, b
     Output: an int, which is the number of times that the functions within result3
     returned true for P2. */

  let count2Trues3: board => int =
    b => {
      let rec count2Trues3Helper: list(((bool, int), (bool, int))) => int =
        lst =>
          switch (lst) {
          | [] => 0
          | [hd, ...tl] =>
            switch (hd) {
            | (_, (true, 2)) => 1 + count2Trues3Helper(tl)
            | (_, (false, 2)) => count2Trues3Helper(tl)
            | _ => failwith("will not reach this count2Trues3 case")
            }
          };

      count2Trues3Helper(result3(b));
    };

  /* Data Definitions and Example Data: refer to key

     estimateValue : state => float

     Input: a state, State(s, b)
     Output: a float that represents the value of that state: the highest numbers
     are most valuable to P1, and the lowest numbers are most valuable to P2. */

  let estimateValue: state => float =
    (State(s, b)) =>
      switch (s, b) {
      | (Win(P1), _) => 100000000000000000000.0
      | (Win(P2), _) => (-100000000000000000000.0)
      | (Draw, _) => 0.0
      | (Ongoing(_), b) =>
        switch (howManyPieces(b)) {
        | 0 => failwith("will not reach this estimateValue case")
        | 1 => 0.0
        | 2 => 0.0
        | 3 =>
          switch (count1Trues2(b), count2Trues2(b)) {
          | (0, 0) => 0.
          | (0, 1) => (-10.)
          | (0, 2) => (-20.)
          | (0, 3) => (-30.)
          | (0, 4) => (-40.)
          | (1, 0) => 10.
          | (1, 1) => 0.
          | (1, 2) => (-10.)
          | (1, 3) => (-20.)
          | (1, 4) => (-30.)
          | (2, 0) => 20.
          | (2, 1) => 10.
          | (2, 2) => 0.
          | (2, 3) => (-10.)
          | (2, 4) => (-20.)
          | (3, 0) => 30.
          | (3, 1) => 20.
          | (3, 2) => 10.
          | (3, 3) => 0.
          | (3, 4) => (-10.)
          | (4, 0) => 40.
          | (4, 1) => 30.
          | (4, 2) => 20.
          | (4, 3) => 10.
          | (4, 4) => 0.
          | _ => failwith("will not reach this estimateValue case")
          }
        | 4 =>
          switch (count1Trues2(b), count2Trues2(b)) {
          | (0, 0) => 0.
          | (0, 1) => (-10.)
          | (0, 2) => (-20.)
          | (0, 3) => (-30.)
          | (0, 4) => (-40.)
          | (1, 0) => 10.
          | (1, 1) => 0.
          | (1, 2) => (-10.)
          | (1, 3) => (-20.)
          | (1, 4) => (-30.)
          | (2, 0) => 20.
          | (2, 1) => 10.
          | (2, 2) => 0.
          | (2, 3) => (-10.)
          | (2, 4) => (-20.)
          | (3, 0) => 30.
          | (3, 1) => 20.
          | (3, 2) => 10.
          | (3, 3) => 0.
          | (3, 4) => (-10.)
          | (4, 0) => 40.
          | (4, 1) => 30.
          | (4, 2) => 20.
          | (4, 3) => 10.
          | (4, 4) => 0.
          | _ => failwith("will not reach this estimateValue case")
          }
        | _ =>
          switch (count1Trues3(b), count2Trues3(b)) {
          | (0, 1) => (-10000.)
          | (0, 2) => (-20000.)
          | (0, 3) => (-30000.)
          | (0, 4) => (-40000.)
          | (0, 5) => (-50000.)
          | (0, 6) => (-60000.)
          | (0, 7) => (-70000.)
          | (0, 8) => (-80000.)
          | (0, 9) => (-90000.)
          | (0, 10) => (-100000.)
          | (1, 0) => 10000.
          | (1, 2) => (-10000.)
          | (1, 3) => (-20000.)
          | (1, 4) => (-30000.)
          | (1, 5) => (-40000.)
          | (1, 6) => (-50000.)
          | (1, 7) => (-60000.)
          | (1, 8) => (-70000.)
          | (1, 9) => (-80000.)
          | (1, 10) => (-90000.)
          | (2, 0) => 20000.
          | (2, 1) => 10000.
          | (2, 3) => (-10000.)
          | (2, 4) => (-20000.)
          | (2, 5) => (-30000.)
          | (2, 6) => (-40000.)
          | (2, 7) => (-50000.)
          | (2, 8) => (-60000.)
          | (2, 9) => (-70000.)
          | (2, 10) => (-80000.)
          | (3, 0) => 30000.
          | (3, 1) => 20000.
          | (3, 2) => 10000.
          | (3, 4) => (-10000.)
          | (3, 5) => (-20000.)
          | (3, 6) => (-30000.)
          | (3, 7) => (-40000.)
          | (3, 8) => (-50000.)
          | (3, 9) => (-60000.)
          | (3, 10) => (-70000.)
          | (4, 0) => 40000.
          | (4, 1) => 30000.
          | (4, 2) => 20000.
          | (4, 3) => 10000.
          | (4, 5) => (-10000.)
          | (4, 6) => (-20000.)
          | (4, 7) => (-30000.)
          | (4, 8) => (-40000.)
          | (4, 9) => (-50000.)
          | (4, 10) => (-60000.)
          | (5, 1) => 40000.
          | (5, 2) => 30000.
          | (5, 3) => 20000.
          | (5, 4) => 10000.
          | (5, 6) => (-10000.)
          | (5, 7) => (-20000.)
          | (5, 8) => (-30000.)
          | (5, 9) => (-40000.)
          | (5, 10) => (-500000.)
          | (6, 0) => 60000.
          | (6, 1) => 50000.
          | (6, 2) => 40000.
          | (6, 3) => 30000.
          | (6, 4) => 20000.
          | (6, 5) => 10000.
          | (6, 7) => (-10000.)
          | (6, 8) => (-20000.)
          | (6, 9) => (-30000.)
          | (6, 10) => (-40000.)
          | (7, 0) => 70000.
          | (7, 1) => 60000.
          | (7, 2) => 50000.
          | (7, 3) => 40000.
          | (7, 4) => 30000.
          | (7, 5) => 20000.
          | (7, 6) => 10000.
          | (7, 8) => (-10000.)
          | (7, 9) => (-20000.)
          | (7, 10) => (-30000.)
          | (8, 0) => 80000.
          | (8, 1) => 70000.
          | (8, 2) => 60000.
          | (8, 4) => 40000.
          | (8, 5) => 30000.
          | (8, 6) => 20000.
          | (8, 7) => 10000.
          | (8, 9) => (-10000.)
          | (8, 10) => (-20000.)
          | (9, 0) => 90000.
          | (9, 1) => 80000.
          | (9, 2) => 70000.
          | (9, 3) => 60000.
          | (9, 4) => 50000.
          | (9, 5) => 40000.
          | (9, 6) => 30000.
          | (9, 7) => 20000.
          | (9, 8) => 10000.
          | (9, 10) => (-10000.)
          | (10, 0) => 100000.
          | (10, 1) => 90000.
          | (10, 2) => 80000.
          | (10, 3) => 70000.
          | (10, 4) => 60000.
          | (10, 5) => 50000.
          | (10, 6) => 40000.
          | (10, 7) => 30000.
          | (10, 8) => 20000.
          | (10, 9) => 10000.
          | _ =>
            switch (count1Trues2(b), count2Trues2(b)) {
            | (0, 0) => 0.
            | (0, 1) => (-10.)
            | (0, 2) => (-20.)
            | (0, 3) => (-30.)
            | (0, 4) => (-40.)
            | (1, 0) => 10.
            | (1, 1) => 0.
            | (1, 2) => (-10.)
            | (1, 3) => (-20.)
            | (1, 4) => (-30.)
            | (2, 0) => 20.
            | (2, 1) => 10.
            | (2, 2) => 0.
            | (2, 3) => (-10.)
            | (2, 4) => (-20.)
            | (3, 0) => 30.
            | (3, 1) => 20.
            | (3, 2) => 10.
            | (3, 3) => 0.
            | (3, 4) => (-10.)
            | (4, 0) => 40.
            | (4, 1) => 30.
            | (4, 2) => 20.
            | (4, 3) => 10.
            | (4, 4) => 0.
            | _ => failwith("will not reach this estimateValue case")
            }
          }
        }
      };
};

module MyGame: Game = Connect4;
open Connect4 /* [1, 2, 1, 2]])), 0., "estimateValue Draw")*/;

/*initial board*/
// checkExpect(initialBoard(4, 4), [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0],
// [0, 0, 0, 0]], "initialBoard checkexpect 4row4col");
// checkExpect(initialBoard(7, 6), [[0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
// [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0],
// [0, 0, 0, 0, 0, 0, 0]], "initialBoard checkexpect 6row7col");

/*stringOfPlayer*/
// checkExpect(stringOfPlayer(P1), "Player 1", "string of player player 1");
// checkExpect(stringOfPlayer(P2), "Player 2", "string of player player 2");

/*stringOfStatus*/
// checkExpect(stringOfStatus(Win(P1)), "Player 1wins!", "stringofstatus p1 win");
// checkExpect(stringOfStatus(Draw), "Draw", "stringofstatus draw");

/*transpose*/
// checkExpect(transpose([[0, 0, 1, 2], [0, 0, 2, 2]]),
// [[0, 0], [0, 0], [1, 2], [2, 2]], "transpose check1");
// checkExpect(transpose([[1, 2], [3, 4]]), [[1, 3], [2, 4]], "transpose check2");

/*printListString*/
// checkExpect(printListString([]), "", "printListString base case test");
// checkExpect(printListString([0, 2, 1]), "0 2 1 ", "printListString list test");

/*printBoardString*/
// checkExpect(printBoardString([[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 1, 2],
// [0, 0, 2, 1]]), ["0 0 0 0 ", "0 0 0 0 ", "0 0 1 2 ", "0 0 2 1 "],
// "print board string test");

/*printBoard*/
// checkExpect(printBoard([[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 1, 2],
// [0, 0, 2, 1]]), "\n0 0 2 1 \n\n0 0 1 2 \n\n0 0 0 0 \n\n0 0 0 0 \n",
// "print board check");

/*stringOfState*/
// checkExpect(stringOfState(State(Ongoing(P2), [[0, 0, 0, 0], [0, 0, 0, 0],
// [0, 0, 1, 2], [0, 0, 2, 1]])),
//  "Player 2's turn\n0 0 2 1 \n\n0 0 1 2 \n\n0 0 0 0 \n\n0 0 0 0 \n",
//  "string of state");

/*stringOfMove*/
// checkExpect(stringOfMove(Move(5)), "5", "string of move test1");
// checkExpect(stringOfMove(Move(1)), "1", "string of move test2");

/*moveOfString*/
// checkExpect(moveOfString("5"), Move(5), "move of string test1");
// checkExpect(moveOfString("1"), Move(1), "move of string test2");

/*legalmoves*/
// checkExpect(legalMoves(State(Ongoing(P1), [[0, 0, 0, 0], [0, 0, 0, 0],
// [0, 0, 1, 2], [0, 0, 2, 1]])), [Move(1), Move(2), Move(3), Move(4)],
// "legal moves check1");
// checkExpect(legalMoves(State(Ongoing(P2), [[2, 1, 2, 1], [0, 0, 0, 0],
// [0, 0, 1, 2]])), [Move(2), Move(3)], "legal moves check2");

/*gameStatus*/
// checkExpect(gameStatus(State(Ongoing(P2), [[0, 0, 0, 0], [0, 0, 0, 0],
// [0, 0, 1, 2], [0, 0, 2, 1]])), Ongoing(P2), "game status test1")
// checkExpect(gameStatus(State(Win(P2), [[2, 2, 2, 2], [0, 0, 0, 0], [0, 0, 1, 1],
// [0, 0, 1, 1]])), Win(P2), "game status test2");

/*correctColumn*/
// checkExpect(correctColumn([[1, 1], [0, 0], [2, 2]], 2), [0, 0],
// "correct col test1");
// checkExpect(correctColumn([[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 1, 2],
// [0, 0, 2, 1]], 3), [0, 0, 1, 2], "correct col test2");

/*replacer*/
// checkExpect(replacer([0, 0, 0], Ongoing(P1)), [1, 0, 0], "replacer test 1")
// checkExpect(replacer([1, 0, 0, 0], Ongoing(P2)), [1, 2, 0, 0],
// "replacer test 2");

/*drop*/
// checkExpect(drop([[0, 0, 0], [0, 0, 0], [0, 0, 0]]), [[0, 0, 0], [0, 0, 0]],
// "drop test1");
// checkExpect(drop([[2, 2], [1, 1]]), [[2, 2]], "drop test2");

/*checkForZeroes*/
// checkExpect(checkForZeroes([[1, 1, 1], [0, 0, 0], [2, 2, 2]]),
// true, "check for zeros test1");
// checkExpect(checkForZeroes([[1, 1, 1], [1, 2, 1], [2, 2, 2]]),
// false, "check for zeros test2");

// /*oppositePlayer*/
// checkExpect(oppositePlayer(Ongoing(P1)), Ongoing(P2), "oppositePlayer test1")
// checkExpect(oppositePlayer(Ongoing(P2)), Ongoing(P1), "oppositePlayer test1")

// /* horizontals */
// checkExpect(horzCount4([[1, 1, 1, 0], [2, 1, 1, 0], [2, 1, 2, 1],
// [1, 1, 2, 2]]), ((true, 1), (false, 2)), "horzCount4 true1");
// checkExpect(horzCount4([[1, 2, 1, 0], [2, 2, 1, 0], [2, 2, 2, 1], [1, 2, 2, 2],
// [2, 1, 1, 1]]), ((false, 1), (true, 2)), "horzCount4 true2");
// checkExpect(horzCount4([[1, 2, 1, 0], [2, 2, 1, 0], [2, 1, 2, 1], [1, 2, 2, 2],
// [2, 1, 1, 1]]), ((false, 1), (false, 2)), "horzCount4 false");

// checkExpect(horzCount3([[1, 1, 1, 0], [2, 1, 1, 0], [2, 1, 2, 1],
// [1, 0, 0, 0]]), ((true, 1), (false, 2)), "horzCount3 true1");
// checkExpect(horzCount3([[1, 2, 1, 0], [2, 2, 1, 0], [2, 2, 2, 1], [1, 0, 0, 0],
// [2, 1, 1, 1]]), ((false, 1), (true, 2)), "horzCount3 true2");
// checkExpect(horzCount3([[1, 2, 1, 0], [2, 2, 1, 0], [2, 1, 2, 1], [1, 2, 2, 2],
// [2, 1, 1, 1]]), ((false, 1), (false, 2)), "horzCount3 false");

// checkExpect(horzCount3Hole2([[1, 1, 1, 0], [2, 0, 0, 0], [2, 1, 1, 0],
// [1, 1, 2, 2], [1, 1, 2, 2]]), ((true, 1), (false, 2)), "horzCount3Hole2 true1");
// checkExpect(horzCount3Hole2([[1, 2, 2, 0], [2, 0, 0, 0], [2, 1, 2, 0],
// [1, 1, 2, 2], [1, 1, 2, 2]]), ((false, 1), (true, 2)), "horzCount3Hole2 true2");
// checkExpect(horzCount3Hole2([[2, 2, 1, 0], [2, 0, 0, 0], [2, 1, 2, 0],
// [1, 1, 2, 2], [1, 1, 2, 2]]), ((false, 1), (false, 2)),
// "horzCount3Hole2 false");

// checkExpect(horzCount3Hole3([[1, 1, 1, 0], [2, 2, 1, 1], [2, 1, 0, 0],
// [1, 1, 1, 2], [1, 1, 2, 2]]), ((true, 1), (false, 2)), "horzCount3Hole2 true1");
// checkExpect(horzCount3Hole3([[1, 2, 2, 0], [2, 2, 0, 0], [2, 0, 0, 0],
// [1, 2, 2, 2], [1, 1, 2, 2]]), ((false, 1), (true, 2)), "horzCount3Hole2 true2");
// checkExpect(horzCount3Hole3([[2, 2, 1, 0], [2, 0, 0, 0], [2, 1, 2, 0],
// [1, 1, 2, 2], [1, 1, 2, 2]]), ((false, 1), (false, 2)),
// "horzCount3Hole2 false");

// /* verticals */
// checkExpect(vertCount4([[1, 2, 1, 0], [2, 2, 1, 0], [2, 2, 2, 1], [1, 2, 2, 2],
// [1, 1, 1, 1]]), ((true, 1), (false, 2)), "vertCount4 true1");
// checkExpect(vertCount4([[1, 1, 1, 0], [2, 1, 1, 0], [2, 2, 2, 2],
// [1, 1, 2, 2]]), ((false, 1), (true, 2)), "vertCount4 true2");
// checkExpect(vertCount4([[1, 2, 1, 0], [2, 2, 1, 0], [2, 1, 2, 1], [1, 2, 2, 2],
// [2, 1, 1, 1]]), ((false, 1), (false, 2)), "vertCount4 false");

// checkExpect(vertCount3([[1, 1, 1, 0], [2, 1, 1, 0], [2, 1, 2, 1],
// [1, 2, 2, 2]]), ((true, 1), (false, 2)), "vertCount3 true1");
// checkExpect(vertCount3([[2, 2, 2, 0], [2, 2, 1, 0], [2, 2, 2, 1], [2, 2, 2, 2],
// [2, 1, 1, 1]]), ((false, 1), (true, 2)), "vertCount3 true2");
// checkExpect(vertCount3([[1, 2, 1, 0], [2, 2, 1, 0], [2, 1, 2, 1], [1, 2, 2, 2],
// [2, 1, 1, 1]]), ((false, 1), (false, 2)), "vertCount3 false");

// checkExpect(vertCount3Hole2([[1, 0, 1, 1], [2, 1, 1, 0], [2, 1, 2, 1],
// [1, 2, 2, 2]]), ((true, 1), (false, 2)), "vertCount3Hole2 true1");
// checkExpect(vertCount3Hole2([[1, 1, 1, 0], [2, 2, 1, 0], [2, 0, 2, 2],
// [2, 2, 2, 2], [2, 1, 1, 1]]), ((false, 1), (true, 2)), "vertCount3Hole2 true2");
// checkExpect(vertCount3Hole2([[1, 2, 1, 0], [2, 2, 1, 0], [2, 1, 2, 1],
// [1, 2, 2, 2], [2, 1, 1, 1]]), ((false, 1), (false, 2)),
// "vertCount3Hole2 false");

// checkExpect(vertCount3Hole3([[1, 1, 0, 1], [2, 1, 1, 0], [2, 1, 2, 1],
// [1, 2, 2, 2]]), ((true, 1), (false, 2)), "vertCount3Hole3 true1");
// checkExpect(vertCount3Hole3([[1, 1, 1, 0], [2, 2, 1, 0], [2, 2, 0, 2],
// [2, 2, 2, 2], [2, 1, 1, 1]]), ((false, 1), (true, 2)), "vertCount3Hole3 true2");
// checkExpect(vertCount3Hole3([[1, 2, 1, 0], [2, 2, 1, 0], [2, 1, 2, 1],
// [1, 2, 2, 2], [2, 1, 1, 1]]), ((false, 1), (false, 2)),
// "vertCount3Hole3 false");

// checkExpect(vertCount2([[1, 1, 1, 0], [2, 1, 1, 0], [2, 1, 2, 1],
// [1, 2, 2, 2]]), ((true, 1), (false, 2)), "vertCount2 true1");
// checkExpect(vertCount2([[1, 2, 1, 0], [2, 2, 1, 0], [2, 2, 2, 1], [2, 2, 2, 0],
// [2, 1, 1, 1]]), ((false, 1), (true, 2)), "vertCount2 true2");
// checkExpect(vertCount2([[1, 2, 1, 0], [2, 2, 1, 0], [2, 1, 2, 1], [1, 2, 2, 2],
// [2, 1, 1, 1]]), ((false, 1), (false, 2)), "vertCount2 false");

// /* diagonals */
// checkExpect(diagCount2Up([[1, 2, 0, 0], [2, 1, 0, 0], [1, 0, 0, 0],
// [2, 1, 0, 0]]), ((true, 1), (false, 2)), "diagCount2Up true 1");
// checkExpect(diagCount2Down([[1, 2, 0, 0], [2, 0, 0, 0], [1, 2, 0, 0],
// [2, 1, 0, 0]]), ((false, 1), (true, 2)), "diagCount2Down true 2");

// checkExpect(diagCount3Down([[1, 2, 1, 2], [1, 1, 2, 2], [2, 2, 1, 2],
// [2, 2, 2, 0]]), ((true, 1), (false, 2)), "diagCount3Down true1");
// checkExpect(diagCount3Down([[2, 2, 1, 2], [1, 2, 1, 2], [2, 1, 2, 1],
// [2, 2, 2, 0]]), ((false, 1), (true, 2)), "diagCount3Down true2");
// checkExpect(diagCount3Down([[1, 2, 1, 2], [1, 2, 2, 2], [2, 1, 1, 2],
// [2, 2, 2, 2], [1, 1, 2, 0]]), ((false, 1), (false, 2)), "diagCount3Down false");

// checkExpect(diagCount3Up([[1, 2, 1, 0], [1, 1, 1, 2], [2, 1, 1, 2],
// [1, 2, 1, 2]]), ((true, 1), (false, 2)), "diagCount3Up true1");
// checkExpect(diagCount3Up([[1, 2, 1, 0], [1, 1, 2, 2], [2, 2, 1, 2],
// [2, 2, 1, 2]]), ((false, 1), (true, 2)), "diagCount3Up true2");
// checkExpect(diagCount3Up([[1, 2, 1, 2], [1, 2, 2, 2], [2, 1, 1, 2],
// [2, 2, 2, 2], [1, 1, 2, 0]]), ((false, 1), (false, 2)), "diagCount3Up false");

// checkExpect(diagCount3DownHole2([[1, 2, 1, 2], [1, 0, 0, 0], [2, 2, 1, 2],
// [2, 2, 1, 1]]), ((true, 1), (false, 2)), "diagCount3DownHole2 true1");
// checkExpect(diagCount3DownHole2([[2, 2, 1, 2], [1, 0, 0, 0], [2, 2, 2, 2],
// [2, 2, 1, 2]]), ((false, 1), (true, 2)), "diagCount3DownHole2 true2");
// checkExpect(diagCount3DownHole2([[1, 2, 1, 2], [1, 2, 2, 2], [2, 1, 1, 2],
// [2, 2, 2, 2], [1, 1, 2, 0]]), ((false, 1), (false, 2)),
// "diagCount3DownHole2 false");

// checkExpect(diagCount3UpHole2([[1, 1, 2, 1], [2, 2, 1, 1], [2, 0, 0, 0],
// [1, 1, 2, 1]]), ((true, 1), (false, 2)), "diagCount3UpHole2 true1");
// checkExpect(diagCount3UpHole2([[2, 2, 2, 2], [1, 1, 2, 1], [2, 0, 0, 0],
// [2, 2, 1, 1]]), ((false, 1), (true, 2)), "diagCount3UpHole2 true2");
// checkExpect(diagCount3UpHole2([[1, 2, 1, 2], [1, 2, 2, 2], [2, 1, 1, 2],
// [2, 2, 2, 2], [1, 1, 2, 0]]), ((false, 1), (false, 2)),
// "diagCount3UpHole2 false");

// checkExpect(diagCount3DownHole3([[1, 2, 1, 2], [2, 1, 0, 0], [1, 1, 0, 0],
// [2, 2, 1, 1]]), ((true, 1), (false, 2)), "diagCount3DownHole3 true1");
// checkExpect(diagCount3DownHole3([[2, 2, 1, 2], [2, 2, 2, 2], [1, 0, 0, 0],
// [2, 2, 1, 2]]), ((false, 1), (true, 2)), "diagCount3DownHole3 true2");
// checkExpect(diagCount3DownHole3([[1, 2, 1, 2], [1, 2, 2, 2], [2, 1, 1, 2],
// [2, 2, 2, 2], [1, 1, 2, 0]]), ((false, 1), (false, 2)),
// "diagCount3DownHole3 false");

// checkExpect(diagCount3UpHole3([[1, 1, 1, 1], [1, 0, 0, 0], [1, 1, 0, 0],
// [1, 1, 1, 1]]), ((true, 1), (false, 2)), "diagCount3UpHole3 true1");
// checkExpect(diagCount3UpHole3([[2, 2, 2, 2], [2, 0, 0, 0], [2, 2, 0, 0],
// [2, 2, 1, 1]]), ((false, 1), (true, 2)), "diagCount3UpHole3 true2");
// checkExpect(diagCount3UpHole3([[1, 2, 1, 2], [1, 2, 2, 2], [2, 1, 1, 2],
// [2, 2, 2, 2], [1, 1, 2, 0]]), ((false, 1), (false, 2)),
// "diagCount3UpHole3 false");

// checkExpect(diagCount4Down([[1, 2, 1, 2], [1, 1, 2, 2], [2, 2, 1, 2],
// [2, 2, 2, 1]]), ((true, 1), (false, 2)), "diagCount4Down true1");
// checkExpect(diagCount4Down([[2, 2, 1, 2], [1, 2, 1, 2], [2, 1, 2, 1],
// [2, 2, 2, 2]]), ((false, 1), (true, 2)), "diagCount4Down true2");
// checkExpect(diagCount4Down([[1, 2, 1, 2], [1, 2, 2, 1], [2, 1, 1, 2],
// [2, 1, 1, 1], [1, 1, 2, 2]]), ((false, 1), (false, 2)), "diagCount4Down false");

// checkExpect(diagCount4Up([[1, 2, 2, 2], [1, 2, 2, 1], [2, 1, 1, 2],
// [2, 1, 1, 1], [1, 1, 1, 2]]), ((true, 1), (false, 2)), "diagCount4Up true1");
// checkExpect(diagCount4Up([[1, 2, 1, 2], [1, 1, 2, 2], [2, 2, 1, 2],
// [2, 2, 2, 1]]), ((false, 1), (true, 2)), "diagCount4Up true2");
// checkExpect(diagCount4Up([[2, 2, 1, 2], [1, 2, 1, 2], [2, 1, 2, 1],
// [2, 2, 2, 2]]), ((false, 1), (false, 2)), "diagCount4Up false");

// /* updateStatus */
// checkExpect(updateStatus(Ongoing(P2), Move(4), [[2, 2, 1, 0, 0],
// [1, 1, 2, 0, 0], [2, 2, 1, 0, 0], [2, 1, 0, 0, 0], [1, 1, 1, 0, 0],
// [2, 2, 1, 1, 1]]), Ongoing(P1), "updateStatus");
// checkExpect(updateStatus(Ongoing(P1), Move(2), [[0, 0, 0, 0, 0, 0],
// [1, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0],
// [0, 0, 0, 0, 0, 0], [2, 2, 2, 0, 0, 0]]), Ongoing(P2), "updateStatus");
// checkExpect(updateStatus(Win(P2), Move(4), [[2, 2, 2, 2, 0],
// [1, 1, 2, 0, 0], [2, 2, 1, 0, 0], [2, 1, 0, 0, 0], [1, 1, 1, 0, 0],
// [2, 2, 1, 1, 1]]), Win(P2), "updateStatus");

// /* nextState */
// checkExpect(nextState(State(Ongoing(P2), [[2, 2, 1, 0, 0], [1, 1, 2, 0, 0],
// [2, 2, 1, 0, 0], [2, 1, 0, 0, 0], [1, 1, 1, 0, 0], [2, 2, 1, 1, 1]]), Move(1)),
// State(Win(P2), [[2, 2, 1, 2, 0], [1, 1, 2, 0, 0], [2, 2, 1, 0, 0],
// [2, 1, 0, 0, 0], [1, 1, 1, 0, 0], [2, 2, 1, 1, 1]]), "nextState Win P2");
// checkExpect(nextState(State(Ongoing(P2), [[2, 1, 2, 1], [1, 2, 0, 0],
// [1, 1, 1, 2], [1, 2, 0, 0]]), Move(4)), State(Ongoing(P1), [[2, 1, 2, 1],
// [1, 2, 0, 0], [1, 1, 1, 2], [1, 2, 2, 0]]), "nextState Ongoing P1");
// checkExpect(nextState(State(Ongoing(P1), [[1, 1, 1, 0], [2, 2, 1, 1],
// [2, 2, 1, 1]]), Move(1)), State(Win(P1), [[1, 1, 1, 1], [2, 2, 1, 1],
// [2, 2, 1, 1]]), "nextState Win P1");
// checkExpect(nextState(State(Ongoing(P2), [[1, 1, 1, 0], [2, 2, 1, 1],
// [2, 2, 1, 1]]), Move(1)), State(Draw, [[1, 1, 1, 2], [2, 2, 1, 1],
// [2, 2, 1, 1]]), "nextState Draw");

// /* updateBoard */
//  checkExpect(updateBoard([[0, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0],
//  [1, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0],
//  [2, 2, 2, 0, 0, 0]], Move(5), Ongoing(P1)), [[0, 0, 0, 0, 0, 0],
//  [1, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0],
//  [0, 0, 0, 0, 0, 0], [2, 2, 2, 0, 0, 0]], "updateBoard P1");
//  checkExpect(updateBoard([[2, 2, 1, 0, 0], [1, 1, 2, 0, 0],
//  [2, 2, 1, 0, 0], [2, 1, 0, 0, 0], [1, 1, 1, 0, 0], [2, 2, 1, 1, 1]], Move(1),
//  Ongoing(P2)), [[2, 2, 1, 2, 0], [1, 1, 2, 0, 0], [2, 2, 1, 0, 0],
//  [2, 1, 0, 0, 0], [1, 1, 1, 0, 0], [2, 2, 1, 1, 1]], "updateBoard P2");
//  checkError(() => updateBoard([[0, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0],
//  [1, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0],
//  [2, 2, 2, 0, 0, 0]], Move(0), Ongoing(P1)), "invalid move, not a column");

// /* howManyPieces */
// checkExpect(howManyPieces([[2, 1, 0, 1], [0, 0, 1, 1]]), 5, "howManyPieces 5");
// checkExpect(howManyPieces([[1, 1, 1, 0], [2, 2, 1, 1], [2, 2, 1, 1]]), 11,
// "howManyPieces 11");

// /* result3 */
// checkExpect(result3([[1, 2, 2, 2, 0, 0], [1, 1, 1, 0, 0, 0],
//  [1, 2, 1, 1, 1, 0], [1, 2, 1, 2, 1, 2], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0],
//  [2, 2, 2, 0, 0, 0]]), [((true, 1), (true, 2)), ((true, 1), (false, 2)),
//  ((false, 1), (false, 2)), ((false, 1), (false, 2)), ((false, 1), (false, 2)),
//  ((true, 1), (false, 2)), ((false, 1), (false, 2)), ((false, 1), (false, 2)),
//  ((false, 1), (false, 2)), ((false, 1), (false, 2))], "result3");
// checkExpect(result3([[2, 2, 1, 0, 0], [1, 1, 2, 0, 0],
//  [2, 2, 1, 0, 0], [2, 1, 0, 0, 0], [1, 1, 1, 0, 0], [2, 2, 1, 1, 1]]),
//  [((true, 1), (false, 2)), ((false, 1), (false, 2)),
//  ((true, 1), (false, 2)), ((false, 1), (false, 2)), ((true, 1), (true, 2)),
//  ((false, 1), (false, 2)), ((false, 1), (false, 2)), ((false, 1), (false, 2)),
//  ((false, 1), (false, 2)), ((false, 1), (false, 2))], "result3");

//  /* result2 */
//  checkExpect(result2([[1, 2, 2, 2, 0, 0], [1, 1, 1, 0, 0, 0],
//  [1, 2, 1, 1, 1, 0], [1, 2, 1, 2, 1, 2], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0],
//  [2, 2, 2, 0, 0, 0]]), [((true, 1), (true, 2)), ((true, 1), (true, 2)),
//  ((true, 1), (false, 2)), ((true, 1), (false, 2))], "result2");
//  checkExpect(result2([[2, 2, 1, 0, 0], [1, 1, 2, 0, 0],
//  [2, 2, 1, 0, 0], [2, 1, 0, 0, 0], [1, 1, 1, 0, 0], [2, 2, 1, 1, 1]]),
//  [((true, 1), (false, 2)), ((false, 1), (false, 2)),
//  ((true, 1), (true, 2)), ((true, 1), (true, 2))], "result2");

// /* count1Trues2 */
// checkExpect(count1Trues2([[1, 2, 0, 0, 0, 0], [1, 1, 0, 0, 0, 0],
// [1, 0, 0, 0, 0, 0], [1, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0],
// [1, 1, 1, 0, 0, 0]]), 4, "count1Trues2");
// checkExpect(count1Trues2([[2, 2, 1, 0, 0], [1, 1, 2, 0, 0],
// [2, 2, 1, 0, 0], [2, 1, 0, 0, 0], [1, 1, 1, 0, 0], [2, 2, 1, 1, 1]]), 3,
// "count1Trues2");

// /* count2Trues2 */
// checkExpect(count2Trues2([[0, 0, 0, 0, 0, 0],
// [1, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0, 0],
// [0, 0, 0, 0, 0, 0], [2, 2, 2, 0, 0, 0]]), 1, "count2Trues2");
// checkExpect(count2Trues2([[1, 2, 1, 2], [1, 1, 2, 2], [2, 2, 1, 2],
// [2, 2, 2, 1]]), 0, "count2Trues2");

// /* count1Trues 3 */
// checkExpect(count1Trues3([[2, 2, 1, 0, 0], [1, 1, 2, 0, 0],
// [2, 2, 1, 0, 0], [2, 1, 0, 0, 0], [1, 1, 1, 0, 0], [2, 2, 1, 1, 1]]), 3,
// "count1Trues3");
// checkExpect(count1Trues3([[1, 2, 1, 2], [1, 1, 1, 2], [2, 2, 1, 2],
// [2, 2, 2, 0]]), 1, "count1Trues3");

// /* count2Trues3 */
// checkExpect(count2Trues3([[2, 2, 1, 0, 0], [1, 1, 2, 0, 0],
// [2, 2, 1, 0, 0], [2, 1, 0, 0, 0], [1, 1, 1, 0, 0], [2, 2, 1, 1, 1]]), 1,
// "count2Trues3");
// checkExpect(count2Trues3([[1, 2, 1, 2], [2, 2, 2, 0], [2, 2, 2, 0],
// [2, 2, 2, 1]]), 1, "count2Trues3");

//  /* estimateValue */
// checkExpect(estimateValue(State(Ongoing(P1),
// [[1, 2, 1, 0], [2, 2, 1, 0], [2, 2, 2, 1], [1, 1, 2, 2]])), 0.,
// "estimateValue Ongoing P1");
// checkExpect(estimateValue(State(Win(P2),
// [[2, 2, 2, 2], [1, 2, 1, 2], [1, 1, 0, 0]])), -100000000000000000000.,
// "estimateValue Win P2");
// checkExpect(estimateValue(State(Ongoing(P1), [[1, 1, 1, 0], [2, 1, 1, 0],
// [2, 2, 1, 0], [1, 2, 2, 1], [2, 2, 2, 1]])), 10000.,
// "estimateValue Ongoing P2");
// checkExpect(estimateValue(State(Draw, [[1, 2, 1, 2], [2, 2, 2, 1],
