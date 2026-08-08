
Partial Public Class AI

    Private PieceHeatMap(,,) As SByte 'Calls & Constructs PieceHeatMaps - producing a hashed array containing the
    '"ideal" locations for each piece on the board (used to evaluate a board position).


    'Function which generates the data for the Piece Heat Maps - 2D arrays that represent the 'ideal' positions for each piece.
    'Used by the evaluation function to determine how favourable a position is for white.
    Private Function GeneratePieceHeatSquares() As SByte(,,)
        'Array containing all the PieceHeatMap data - hashed so that a specific piece's index can be easily retrieved using its symbol.
        Dim HeatSqaures(9, 7, 7) As SByte

        'Below are the Heat Maps for each individual piece: the greater the number, the more favourable a position is that has that
        'specific piece on that index of the board (favoured around white; 7-YIndex is used for black's POV).
        Dim PawnHeatSquares As SByte(,) = {
            {0, 0, 0, 0, 0, 0, 0, 0},
            {50, 50, 50, 50, 50, 50, 50, 50},
            {10, 10, 20, 30, 30, 20, 10, 10},
            {5, 5, 10, 25, 25, 10, 5, 5},
            {0, 0, 0, 20, 20, 0, 0, 0},
            {5, -5, -10, 0, 0, -10, -5, 5},
            {5, 10, 10, -20, -20, 10, 10, 5},
            {0, 0, 0, 0, 0, 0, 0, 0}}

        Dim KnightHeatSquares As SByte(,) = {
            {-50, -40, -30, -30, -30, -30, -40, -50},
            {-40, -20, 0, 0, 0, 0, -20, -40},
            {-30, 0, 10, 15, 15, 10, 0, -30},
            {-30, 5, 15, 20, 20, 15, 5, -30},
            {-30, 0, 15, 20, 20, 15, 0, -30},
            {-30, 5, 10, 15, 15, 10, 5, -30},
            {-40, -20, 0, 5, 5, 0, -20, -40},
            {-50, -40, -30, -30, -30, -30, -40, -50}}

        Dim BishopHeatSquares As SByte(,) = {
            {-20, -10, -10, -10, -10, -10, -10, -20},
            {-10, 0, 0, 0, 0, 0, 0, -10},
            {-10, 0, 5, 10, 10, 5, 0, -10},
            {-10, 5, 5, 10, 10, 5, 5, -10},
            {-10, 0, 10, 10, 10, 10, 0, -10},
            {-10, 10, 10, 10, 10, 10, 10, -10},
            {-10, 5, 0, 0, 0, 0, 5, -10},
            {-20, -10, -10, -10, -10, -10, -10, -20}}

        Dim RookHeatSquares As SByte(,) = {
            {0, 0, 0, 0, 0, 0, 0, 0},
            {5, 10, 10, 10, 10, 10, 10, 5},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {0, 0, 0, 5, 5, 0, 0, 0}}

        Dim QueenHeatSquares As SByte(,) = {
            {-20, -10, -10, -5, -5, -10, -10, -20},
            {-10, 0, 0, 0, 0, 0, 0, -10},
            {-10, 0, 5, 5, 5, 5, 0, -10},
            {-5, 0, 5, 5, 5, 5, 0, -5},
            {0, 0, 5, 5, 5, 5, 0, -5},
            {-10, 5, 5, 5, 5, 5, 0, -10},
            {-10, 0, 5, 0, 0, 0, 0, -10},
            {-20, -10, -10, -5, -5, -10, -10, -20}}

        Dim KingHeatSquares As SByte(,) = {
            {-30, -40, -40, -50, -50, -40, -40, -30},
            {-30, -40, -40, -50, -50, -40, -40, -30},
            {-30, -40, -40, -50, -50, -40, -40, -30},
            {-30, -40, -40, -50, -50, -40, -40, -30},
            {-20, -30, -30, -40, -40, -30, -30, -20},
            {-10, -20, -20, -20, -20, -20, -20, -10},
            {20, 20, 0, 0, 0, 0, 20, 20},
            {20, 30, 10, 0, 0, 10, 30, 20}}

        For x = 0 To 7
            For y = 0 To 7
                'Mutliples each Heat Map index by the piece multiplier (so that they reflect the weight of each piece).
                BishopHeatSquares(x, y) *= 2
                KnightHeatSquares(x, y) *= 1
                PawnHeatSquares(x, y) *= 1.4
                QueenHeatSquares(x, y) *= 2.4
                RookHeatSquares(x, y) *= 2
                KingHeatSquares(x, y) *= 1.6

                'Combines each Piece Heat Map into one big Heat Map, so that the correct Heat Map can be called by hashing the name of the required piece.
                HeatSqaures(0, x, y) = BishopHeatSquares(x, y)
                HeatSqaures(1, x, y) = KnightHeatSquares(x, y)
                HeatSqaures(3, x, y) = PawnHeatSquares(x, y)
                HeatSqaures(4, x, y) = QueenHeatSquares(x, y)
                HeatSqaures(5, x, y) = RookHeatSquares(x, y)
                HeatSqaures(9, x, y) = KingHeatSquares(x, y)
            Next
        Next

        Return HeatSqaures
    End Function






    Private EndgameEvalLookupTable(63, 63, 12) As Int16 'Lookup Table that calculates the evaluation of simple endgame positions,
    'taking into account the king positions, and the player's material count (value scales as material count decreases).
    '(a,b,c), where a = position of the player's king, b = position of the opposition's king, c = player's material count
    '(which is divided by 100 in the main MiniMax code).

    'Algorithm which forms the Lookup Table for simple endgame evaluations.
    Private Sub PopulateEndgameEvalLookupTable()
        Dim KingDeltaX, KingDeltaY, KingDistance As Decimal 'Variables which represent the distances between the two kings.
        Dim KingCentreDistanceX, KingCentreDistanceY, KingCentreDistance As Decimal

        'For each possible configuration of king positions...
        For MeKPos As Byte = 0 To 63
            For EnemyKPos As Byte = 0 To 63
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
                For c = 0 To 12
                    'Heuristic becomes more prevelant as the opponent has fewer and fewer pieces (exponential curve).
                    EndgameEvalLookupTable(MeKPos, EnemyKPos, c) = Math.Round((KingCentreDistance * 1.5 + (7 - KingDistance)) * (1.25 ^ (12 - c)))
                Next
            Next
        Next
    End Sub

End Class
