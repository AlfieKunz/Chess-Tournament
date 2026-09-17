
Partial Public Class AI

    Private PieceHeatMap(,,,) As Integer 'Calls & Constructs PieceHeatMaps - producing a hashed array containing the
    '"ideal" locations for each piece on the board (used to evaluate a board position).


    'Function which generates the data for the Piece Heat Maps - 2D arrays that represent the 'ideal' positions for each piece.
    'Used by the evaluation function to determine how favourable a position is for white.
    Private Function GeneratePieceHeatSquares() As Integer(,,,)
        'Array containing all the PieceHeatMap data - hashed so that a specific piece's index can be easily retrieved using its symbol.
        Dim HeatSqaures(9, 7, 7, 16) As Integer
        Dim EndgameLerpWeight As Double

        'Below are the Heat Maps for each individual piece: the greater the number, the more favourable a position is that has that
        'specific piece on that index of the board (favoured around white; 7-YIndex is used for black's POV).
        Dim PawnHeatSquares As Double(,) = {
            {0, 0, 0, 0, 0, 0, 0, 0},
            {50, 50, 50, 50, 50, 50, 50, 50},
            {10, 10, 20, 30, 30, 20, 10, 10},
            {4, 5, 10, 25, 25, 10, 5, 4},
            {0, 0, 0, 20, 20, 0, 0, 0},
            {5, -5, -10, 0, 0, -10, -5, 5},
            {4, 10, 10, -20, -20, 10, 10, 4},
            {0, 0, 0, 0, 0, 0, 0, 0}}

        Dim PawnEndgameHeatSquares As Double(,) = {
            {0, 0, 0, 0, 0, 0, 0, 0},
            {90, 90, 90, 90, 90, 90, 90, 90},
            {50, 50, 50, 50, 50, 50, 50, 50},
            {20, 20, 20, 20, 20, 20, 20, 20},
            {10, 10, 10, 10, 10, 10, 10, 10},
            {5, 5, 5, 5, 5, 5, 5, 5},
            {6, 6, 6, 6, 6, 6, 6, 6},
            {0, 0, 0, 0, 0, 0, 0, 0}}

        Dim KnightHeatSquares As Double(,) = {
            {-50, -40, -30, -30, -30, -30, -40, -50},
            {-40, -20, 0, 0, 0, 0, -20, -40},
            {-30, 0, 10, 15, 15, 10, 0, -30},
            {-30, 5, 15, 20, 20, 15, 5, -30},
            {-30, 0, 15, 20, 20, 15, 0, -30},
            {-30, 5, 10, 15, 15, 10, 5, -30},
            {-40, -20, 0, 5, 5, 0, -20, -40},
            {-50, -40, -30, -30, -30, -30, -40, -50}}

        Dim KnightEndgameHeatSquares As Double(,) = {
            {-30, -25, -20, -20, -20, -20, -25, -30},
            {-25, -10, 0, 0, 0, 0, -10, -25},
            {-20, 0, 10, 10, 10, 10, 0, -20},
            {-20, 5, 10, 15, 15, 10, 5, -20},
            {-20, 0, 10, 15, 15, 10, 0, -20},
            {-20, 5, 10, 10, 10, 10, 5, -20},
            {-25, -10, 0, 5, 5, 0, -10, -25},
            {-30, -25, -20, -20, -20, -20, -25, -30}}

        Dim BishopHeatSquares As Double(,) = {
            {-20, -10, -10, -10, -10, -10, -10, -20},
            {-10, 0, 0, 0, 0, 0, 0, -10},
            {-10, 0, 5, 10, 10, 5, 0, -10},
            {-10, 5, 5, 10, 10, 5, 5, -10},
            {-10, 0, 10, 10, 10, 10, 0, -10},
            {-10, 10, 10, 10, 10, 10, 10, -10},
            {-10, 5, 0, 0, 0, 0, 5, -10},
            {-20, -10, -10, -10, -10, -10, -10, -20}}

        Dim BishopEndgameHeatSquares As Double(,) = {
            {-10, -5, -5, -5, -5, -5, -5, -10},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {-5, 0, 3, 5, 5, 3, 0, -5},
            {-5, 3, 3, 5, 5, 3, 3, -5},
            {-5, 0, 5, 5, 5, 5, 0, -5},
            {-5, 5, 5, 5, 5, 5, 5, -5},
            {-5, 3, 0, 0, 0, 0, 3, -5},
            {-10, -5, -5, -5, -5, -5, -5, -10}}

        Dim RookHeatSquares As Double(,) = {
            {4, 7, 7, 7, 7, 7, 7, 4},
            {5, 10, 12, 15, 15, 12, 10, 5},
            {3, 6, 6, 6, 6, 6, 6, 3},
            {-5, 0, 2, 3, 3, 2, 0, -5},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {-5, -2, -2, -2, -2, -2, -2, -5},
            {-8, -8, -4, -3, -3, -4, -8, -8},
            {0, 0, 0, 5, 5, 0, 0, 0}}

        Dim RookEndgameHeatSquares As Double(,) = {
            {8, 8, 8, 8, 8, 8, 8, 8},
            {5, 5, 5, 5, 5, 5, 5, 5},
            {2, 0, 0, 0, 0, 0, 0, 2},
            {0, 0, 0, 0, 0, 0, 0, 0},
            {0, -3, -5, -5, -5, -5, -3, 0},
            {-5, -5, -5, -5, -5, -5, -5, -5},
            {-5, -5, -5, -5, -5, -5, -5, -5},
            {-5, -5, -5, -5, -5, -5, -5, -5}}

        Dim QueenHeatSquares As Double(,) = {
            {-20, -10, -10, -5, -5, -10, -10, -20},
            {-10, 0, 0, 0, 0, 0, 0, -10},
            {-10, 0, 5, 5, 5, 5, 0, -10},
            {-5, 0, 5, 5, 5, 5, 0, -5},
            {0, 0, 5, 5, 5, 5, 0, -5},
            {-10, 5, 5, 5, 5, 5, 0, -10},
            {-10, 0, 5, 0, 0, 0, 0, -10},
            {-20, -10, -10, -5, -5, -10, -10, -20}}

        Dim QueenEndgameHeatSquares As Double(,) = {
            {0, 3, 5, 5, 5, 5, 3, 0},
            {0, 5, 8, 8, 8, 8, 5, 0},
            {5, 8, 10, 10, 10, 10, 8, 5},
            {5, 8, 10, 10, 10, 10, 8, 5},
            {0, 5, 8, 8, 8, 8, 5, 0},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {-10, -8, -5, -5, -5, -5, -8, -10},
            {-20, -10, -10, -5, -5, -10, -10, -20}}

        Dim KingHeatSquares As Double(,) = {
            {-30, -40, -40, -50, -50, -40, -40, -30},
            {-30, -40, -40, -50, -50, -40, -40, -30},
            {-30, -40, -40, -50, -50, -40, -40, -30},
            {-30, -40, -40, -50, -50, -40, -40, -30},
            {-20, -30, -30, -40, -40, -30, -30, -20},
            {-10, -20, -20, -20, -20, -20, -20, -10},
            {20, 20, 0, 0, 0, 0, 20, 20},
            {20, 30, 10, 0, 0, 10, 30, 20}}

        Dim KingEndgameHeatSquares As Double(,) = {
            {-50, -40, -30, -20, -20, -30, -40, -50},
            {-30, -20, -10, 0, 0, -10, -20, -30},
            {-30, -10, 20, 30, 30, 20, -10, -30},
            {-30, -10, 30, 40, 40, 30, -10, -30},
            {-30, -10, 30, 40, 40, 30, -10, -30},
            {-30, -10, 20, 30, 30, 20, -10, -30},
            {-30, -30, 0, 0, 0, 0, -30, -30},
            {-50, -30, -30, -30, -30, -30, -30, -50}}

        For x = 0 To 7
            For y = 0 To 7
                'From testing, I am using the "Aggressive-Endgame-Endgame Hybrid" model of my PHM values, which prioritises heavy piece activity
                'and strong piece activity in the endgame phase. I presume this aggressive behaviour works well so that the AI does not have to
                'face 'defensive' positions, which may expose its poor understanding of king safety.

                'Most chess engines value the bishop as worth 3.25 points of material, but my chess engine uses 3.
                'For PHM, add this change.
                BishopHeatSquares(x, y) += 25
                BishopEndgameHeatSquares(x, y) += 25

                'Multiples each Heat Map index by the piece multiplier (so that they reflect the weight of each piece).
                BishopHeatSquares(x, y) *= 1.28
                KnightHeatSquares(x, y) *= 1.18
                PawnHeatSquares(x, y) *= 1.42
                QueenHeatSquares(x, y) *= 2.55
                RookHeatSquares(x, y) *= 1.98
                KingHeatSquares(x, y) *= 1.33

                'Corresponding values for the endgame phase.
                BishopEndgameHeatSquares(x, y) *= 1.35
                KnightEndgameHeatSquares(x, y) *= 0.92
                PawnEndgameHeatSquares(x, y) *= 1.65
                QueenEndgameHeatSquares(x, y) *= 2.9
                RookEndgameHeatSquares(x, y) *= 2.25
                KingEndgameHeatSquares(x, y) *= 1.65

                For m = 0 To 16
                    EndgameLerpWeight = Math.Min(2 - 0.125 * m, 1)

                    'Combines each Piece Heat Map into one big Heat Map, so that the correct Heat Map can be called by hashing the name of the required piece.
                    HeatSqaures(0, x, y, m) = CInt((1 - EndgameLerpWeight) * BishopHeatSquares(x, y) + EndgameLerpWeight * BishopEndgameHeatSquares(x, y))
                    HeatSqaures(1, x, y, m) = CInt((1 - EndgameLerpWeight) * KnightHeatSquares(x, y) + EndgameLerpWeight * KnightEndgameHeatSquares(x, y))
                    HeatSqaures(3, x, y, m) = CInt((1 - EndgameLerpWeight) * PawnHeatSquares(x, y) + EndgameLerpWeight * PawnEndgameHeatSquares(x, y))
                    HeatSqaures(4, x, y, m) = CInt((1 - EndgameLerpWeight) * QueenHeatSquares(x, y) + EndgameLerpWeight * QueenEndgameHeatSquares(x, y))
                    HeatSqaures(5, x, y, m) = CInt((1 - EndgameLerpWeight) * RookHeatSquares(x, y) + EndgameLerpWeight * RookEndgameHeatSquares(x, y))
                    HeatSqaures(9, x, y, m) = CInt((1 - EndgameLerpWeight) * KingHeatSquares(x, y) + EndgameLerpWeight * KingEndgameHeatSquares(x, y))
                Next
            Next
        Next

        Return HeatSqaures
    End Function







    Private EndgameEvalLookupTable(63, 63, 14) As Int16 'Lookup Table that calculates the evaluation of simple endgame positions,
    'taking into account the king positions, and the player's material count (value scales as material count decreases).
    '(a,b,c), where a = position of the player's king, b = position of the opposition's king, c = player's material count
    '(which is divided by 100 in the main NegaMax code).

    'Algorithm which forms the Lookup Table for simple endgame evaluations.
    Private Sub PopulateEndgameEvalLookupTable()
        Dim KingDeltaX, KingDeltaY, KingDistance As Double 'Variables which represent the distances between the two kings.
        Dim KingCentreDistanceX, KingCentreDistanceY, KingCentreDistance As Double

        'For each possible configuration of king positions...
        For MeKPos As UInt16 = 0 To 63
            For EnemyKPos As UInt16 = 0 To 63
                If MeKPos = EnemyKPos Then Continue For 'The two kings cannot be on the same square.

                'Finds distances between kings.
                KingDeltaX = Math.Abs(((MeKPos And 56) >> 3) - ((EnemyKPos And 56) >> 3))
                KingDeltaY = Math.Abs((MeKPos And 7) - (EnemyKPos And 7))
                KingDistance = (KingDeltaX + KingDeltaY) / 2

                'Calculates the distance from the player's king to the edges of the board.
                KingCentreDistanceX = Math.Max(((MeKPos And 56) >> 3) - 4, 3 - ((MeKPos And 56) >> 3))
                KingCentreDistanceY = Math.Max((MeKPos And 7) - 4, 3 - (MeKPos And 7))
                KingCentreDistance = KingCentreDistanceX + KingCentreDistanceY

                'Uses the above data (along with the player's material count), to calculate the penalty that should be applied to that player,
                'for each possible material count of the player. This value is then stored in the Lookup Table.
                For c = 0 To 14
                    'Heuristic becomes more prevelant as the opponent has fewer and fewer pieces (exponential curve).
                    EndgameEvalLookupTable(MeKPos, EnemyKPos, c) = CShort((KingCentreDistance * 1.5 + (7 - KingDistance)) * (1.25 ^ (12 - c)))
                Next
            Next
        Next
    End Sub


    Dim EvalPastPawnBonus() As Integer = {0, 0, 10, 15, 35, 60, 90, 120, 0}
    Dim EvalIsolatedPawnPenalty() As Integer = {0, 0, 22, 22, 22, 18, 10, 5, 0}
    Dim EvalDoubledPawnPenalty As Integer = 35

End Class
