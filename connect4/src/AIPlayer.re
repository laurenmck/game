open! CS17SetupGame;
open SigGame;
open Connect4;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;

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

     All other data types used within this file are atomic data types or lists
     of atomic data types and therefore do not require data definitions or example
     data.
     */

  /* Data Definitions and Example Data: refer to key

     maxValue : list(float) => float

     Input: a list of floats, lst
     Output: the maximum float in the list of floats

     RD:
     OI: [1., 2., 3.]
     RI: [2., 3.]
     RO: 3.
     ID: if the first float in the list is not the maximum, recursive output
     equals original output
     OO: 3. */

  let rec maxValue: list(float) => float =
    lst =>
      switch (lst) {
      | [] => failwith("will never reach this maxValue case")
      | [hd] => hd
      | [hd, ...tl] =>
        if (hd > maxValue(tl)) {
          hd;
        } else {
          maxValue(tl);
        }
      };

  /* Data Definitions and Example Data: refer to key

     maxPlace : (float, list(float)) => int

     Input: a float, maxValue, and a list of floats, lst
     Output: the place in the list where the maximum float is, represented by an int:
     0 if it's the first element of the list, 1 if i's the second element, etc.

     RD:
     OI: 3., [1., 2., 3.]
     RI: 3., [2., 3.]
     RO: 2
     ID: if the maximum value is not at the head of the list, add 1 to
     the recursive output
     OO: 3 */

  let rec maxPlace: (float, list(float)) => int =
    (maxValue, lst) =>
      if (maxValue == List.hd(lst)) {
        0;
      } else {
        1 + maxPlace(maxValue, List.tl(lst));
      };

  /* Data Definitions and Example Data: refer to key

     minValue : list(float) => float

     Input: a list of floats, lst
     Output: the minimum float in the list of floats

     RD:
     OI: [1., 2., 3.]
     RI: [2., 3.]
     RO: 2.
     ID: if the first float in the list is not the minimum, recursive output
     equals original output
     OO: 2. */

  let rec minValue: list(float) => float =
    lst =>
      switch (lst) {
      | [] => failwith("will never reach this maxValue case")
      | [hd] => hd
      | [hd, ...tl] =>
        if (hd < minValue(tl)) {
          hd;
        } else {
          minValue(tl);
        }
      };

  /* Data Definitions and Example Data: refer to key

     minPlace : (float, list(float)) => int

     Input: a float, minValue, and a list of floats, lst
     Output: the place in the list where the minimum float is, represented by an int:
     0 if it's the first element of the list, 1 if i's the second element, etc.

     RD:
     OI: 3., [5., 4., 3.]
     RI: 3., [4., 3.]
     RO: 2
     ID: if the minimum value is not at the head of the list, add 1 to
     the recursive output
     OO: 3 */

  let rec minPlace: (float, list(float)) => int =
    (minValue, lst) =>
      if (minValue == List.hd(lst)) {
        0;
      } else {
        1 + minPlace(minValue, List.tl(lst));
      };

  /* Data Definitions and Example Data: refer to key

     minimax : (state, int) => float

     Input: a state, s, and an int, i, which represents the depth that has been
     reached at a given point in the recursion
     Output: a float that corresponds to the value of the ideal move for the AI

     RD:
     OI: (State(Ongoing(P1), [[1, 0, 0, 0], [1, 1, 2, 0], [0, 0, 0, 0]]), 5)
     RI: (State(Ongoing(P1), [[1, 0, 0, 0], [1, 1, 2, 0], [0, 0, 0, 0]]), 4)
     RO: ?????? DO THIS IN SKETCH TO GET ANSWER AND THEN MAKE IT A CHECK EXPECT
     ID: ??????
     OO: ????? */

  let rec minimax: (PlayerGame.state, int) => float =
    (s, i) =>
      switch (PlayerGame.gameStatus(s), i) {
      | (Win(_), _) => PlayerGame.estimateValue(s)
      | (Draw, _) => PlayerGame.estimateValue(s)
      | (Ongoing(_), 0) => PlayerGame.estimateValue(s)
      | (Ongoing(P1), _) =>
        maxValue(
          List.map(
            s => minimax(s, i - 1),
            List.map(
              m => PlayerGame.nextState(s, m),
              PlayerGame.legalMoves(s),
            ),
          ),
        )
      | (Ongoing(P2), _) =>
        minValue(
          List.map(
            s => minimax(s, i - 1),
            List.map(
              m => PlayerGame.nextState(s, m),
              PlayerGame.legalMoves(s),
            ),
          ),
        )
      };

  let nextMove: PlayerGame.state => PlayerGame.move =
    s => {
      let minimaxList =
        List.map(
          s => minimax(s, 4),
          List.map(
            m => PlayerGame.nextState(s, m),
            PlayerGame.legalMoves(s),
          ),
        );

      switch (PlayerGame.gameStatus(s)) {
      | Ongoing(P1) =>
        List.nth(
          PlayerGame.legalMoves(s),
          maxPlace(maxValue(minimaxList), minimaxList),
        )
      | Ongoing(P2) =>
        List.nth(
          PlayerGame.legalMoves(s),
          minPlace(minValue(minimaxList), minimaxList),
        )
      | _ => failwith("should not reach this nextMove case")
      };
    };
};

module TestGame = Connect4;
module TestAIPlayer = AIPlayer(TestGame);
open TestAIPlayer /* [-100000000000000000000., -20000., 100.]), 0, "minPlace")*/;

/* maxValue */
// checkExpect(maxValue([-100., 10000., -200.]), 10000., "maxValue");
// checkExpect(maxValue([-200., 1000., 50000.]), 50000., "maxValue");
// checkExpect(maxValue([100000000000000000000., -20000., 100.]),
// 100000000000000000000., "maxValue");

/* maxPlace */
// checkExpect(maxPlace(10000., [-100., 10000., -200.]), 1, "maxPlace");
// checkExpect(maxPlace(50000., [-200., 1000., 50000.]), 2, "maxPlace");
// checkExpect(maxPlace(100000000000000000000.,
// [100000000000000000000., -20000., 100.]), 0, "maxPlace");

/* minValue */
// checkExpect(minValue([-100., 10000., -200.]), -200., "minValue");
// checkExpect(minValue([-200., 1000., 50000.]), -200., "minValue");
// checkExpect(minValue([-100000000000000000000., -20000., 100.]),
// -100000000000000000000., "minValue");

/* maxPlace */
// checkExpect(minPlace(-200., [-100., 10000., -200.]), 2, "minPlace");
// checkExpect(minPlace(-200., [-200., 1000., 50000.]), 0, "minPlace");
// checkExpect(minPlace(-100000000000000000000.,
