'Class that holds the data for the Piece Heat Maps - 2D arrays that represent the 'ideal' positions for each piece.
'Used by the evaluation function to determine how favourable a position is for white.
Public Class PieceHeatSqaures
    Public ReadOnly HeatSqaures(9, 7, 7) As Decimal

    Private PawnHeatSquares As Decimal(,) = {
            {0, 0, 0, 0, 0, 0, 0, 0},
            {50, 50, 50, 50, 50, 50, 50, 50},
            {10, 10, 20, 30, 30, 20, 10, 10},
            {5, 5, 10, 25, 25, 10, 5, 5},
            {0, 0, 0, 20, 20, 0, 0, 0},
            {5, -5, -10, 0, 0, -10, -5, 5},
            {5, 10, 10, -20, -20, 10, 10, 5},
            {0, 0, 0, 0, 0, 0, 0, 0}}

    Private KnightHeatSquares As Decimal(,) = {
            {-50, -40, -30, -30, -30, -30, -40, -50},
            {-40, -20, 0, 0, 0, 0, -20, -40},
            {-30, 0, 10, 15, 15, 10, 0, -30},
            {-30, 5, 15, 20, 20, 15, 5, -30},
            {-30, 0, 15, 20, 20, 15, 0, -30},
            {-30, 5, 10, 15, 15, 10, 5, -30},
            {-40, -20, 0, 5, 5, 0, -20, -40},
            {-50, -40, -30, -30, -30, -30, -40, -50}}

    Private BishopHeatSquares As Decimal(,) = {
            {-20, -10, -10, -10, -10, -10, -10, -20},
            {-10, 0, 0, 0, 0, 0, 0, -10},
            {-10, 0, 5, 10, 10, 5, 0, -10},
            {-10, 5, 5, 10, 10, 5, 5, -10},
            {-10, 0, 10, 10, 10, 10, 0, -10},
            {-10, 10, 10, 10, 10, 10, 10, -10},
            {-10, 5, 0, 0, 0, 0, 5, -10},
            {-20, -10, -10, -10, -10, -10, -10, -20}}

    Private RookHeatSquares As Decimal(,) = {
            {0, 0, 0, 0, 0, 0, 0, 0},
            {5, 10, 10, 10, 10, 10, 10, 5},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {-5, 0, 0, 0, 0, 0, 0, -5},
            {0, 0, 0, 5, 5, 0, 0, 0}}

    Private QueenHeatSquares As Decimal(,) = {
            {-20, -10, -10, -5, -5, -10, -10, -20},
            {-10, 0, 0, 0, 0, 0, 0, -10},
            {-10, 0, 5, 5, 5, 5, 0, -10},
            {-5, 0, 5, 5, 5, 5, 0, -5},
            {0, 0, 5, 5, 5, 5, 0, -5},
            {-10, 5, 5, 5, 5, 5, 0, -10},
            {-10, 0, 5, 0, 0, 0, 0, -10},
            {-20, -10, -10, -5, -5, -10, -10, -20}}

    Private KingHeatSquares As Decimal(,) = {
            {-30, -40, -40, -50, -50, -40, -40, -30},
            {-30, -40, -40, -50, -50, -40, -40, -30},
            {-30, -40, -40, -50, -50, -40, -40, -30},
            {-30, -40, -40, -50, -50, -40, -40, -30},
            {-20, -30, -30, -40, -40, -30, -30, -20},
            {-10, -20, -20, -20, -20, -20, -20, -10},
            {20, 20, 0, 0, 0, 0, 20, 20},
            {20, 30, 10, 0, 0, 10, 30, 20}}


    Public Sub New()
        For x = 0 To 7
            For y = 0 To 7
                'Mutliples each Heat Map index by the piece multiplier (so that they reflect the weight of each piece).
                BishopHeatSquares(x, y) *= 2
                KnightHeatSquares(x, y) *= 1
                PawnHeatSquares(x, y) *= 1.5
                QueenHeatSquares(x, y) *= 3
                RookHeatSquares(x, y) *= 2
                KingHeatSquares(x, y) *= 1.5

                'Scales each Heat Map index so that it will not overshadow the effect of the material count in the evaluation function.
                BishopHeatSquares(x, y) /= 100
                KnightHeatSquares(x, y) /= 100
                PawnHeatSquares(x, y) /= 100
                QueenHeatSquares(x, y) /= 100
                RookHeatSquares(x, y) /= 100
                KingHeatSquares(x, y) /= 100

                'Combines each Piece Heat Map into one big Heat Map, so that the correct Heat Map can be called by hashing the name of the required piece.
                HeatSqaures(0, x, y) = BishopHeatSquares(x, y)
                HeatSqaures(1, x, y) = KnightHeatSquares(x, y)
                HeatSqaures(3, x, y) = PawnHeatSquares(x, y)
                HeatSqaures(4, x, y) = QueenHeatSquares(x, y)
                HeatSqaures(5, x, y) = RookHeatSquares(x, y)
                HeatSqaures(9, x, y) = KingHeatSquares(x, y)
            Next
        Next
    End Sub

End Class
