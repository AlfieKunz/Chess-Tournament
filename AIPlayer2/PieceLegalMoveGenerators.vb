Option Strict On
Imports System.Data
Imports System.Diagnostics.Metrics
Imports System.IO
Imports System.Numerics
Imports System.Runtime
Imports System.Runtime.CompilerServices


'Condensed Magic and Mask Info for efficient OOP storage. The exported data is a serialised array of 'MagicInfo', one structure for each square.
Public Structure MagicInfo
    Public MovementMask As UInt64 'Holds the move map of a piece with no blockers, but cutting the final square off for each edge. Allows for easy ANDing with the occupancy mask. 
    Public Magic As UInt64 '64-bit unsigned magic number, with 87.5% set sparsity and always containing at least 6 set bits.
    Public Shift As Integer 'Number of places to shift the "Magic * Blocker Mask" key, to produce an entry in the array of legal moves.
    Public MoveMap() As UInt64 'Hashed array of legal moves, containing the movement map for a rook at each location with specific blocker pattern.
End Structure


'This class contains the code for generating the pseudo-legal moves for a given piece on the board. I have broken these
'algorithms into two main components: one which will be called by the Move Generation algorithms, and will return a set
'of moves that the input piece can make. The second set will be called by the TFTable Fixer algorithms, and will only
'update the TFTable squares of the required player (however, the core of each of these algorithms remain the same - to
'determine the pseudo-legal moves that a piece can make. Originally, these two sets were combined into a singular set
'(which were called by both algorithms), but are now separated to improve efficiency (from v6.1 and onwards).
Partial Public Class CoreMethods

    'Code for methods that precompute all movement bitmaps for all pieces at any square, by casting rays. Crutially, for sliding
    'pieces we cut the search 1 cell from each edge, as to better fit with occupancy masks.
    Private Shared MagicRookInfo(63) As MagicInfo
    Private Shared MagicBishopInfo(63) As MagicInfo
    Protected Shared RayMap(4095) As UInt64
    Protected Sub PrecomputeAllPieceMaps()
        PrecomputeKingMap()
        PrecomputeKingDangerMaps()
        PrecomputePawnMaps()
        PrecomputeKnightMap()
        PrecomputeBishopMap()
        PrecomputeRookMap()

        'Loads magic numbers, shift values, and move maps for all 64 squares for rooks & bishops.
        Using FS As New FileStream(GlobalConstants.StartupPath & "\Assets\MagicData.bit", FileMode.Open, FileAccess.Read)
            Using BR As New BinaryReader(FS)
                For Each MagicData In {MagicRookInfo, MagicBishopInfo}
                    For n = 0 To 63
                        Dim TempMagic As MagicInfo
                        TempMagic.MovementMask = BR.ReadUInt64()
                        TempMagic.Magic = BR.ReadUInt64()
                        TempMagic.Shift = BR.ReadInt32()
                        TempMagic.MoveMap = New UInt64(BR.ReadInt32() - 1) {}
                        For m = 0 To TempMagic.MoveMap.Length - 1
                            TempMagic.MoveMap(m) = BR.ReadUInt64()
                        Next
                        MagicData(n) = TempMagic
                    Next
                Next
            End Using
        End Using

        'Computes the Ray Map, telling us all the squares bewteen two specific squares. Used in pin detection, restricting pin movement,
        'check evasion via blocking, checking empty squares in castling, etc.
        For Square1 = 0 To 63
            For Square2 = 0 To 63
                'Skip computation if the squares are equal, or don't share the same row / diagonal.
                Dim dx As Integer = (Square2 Mod 8) - (Square1 Mod 8)
                Dim dy As Integer = (Square2 \ 8) - (Square1 \ 8)
                If dx = 0 OrElse dy = 0 OrElse Math.Abs(dx) = Math.Abs(dy) Then
                    Dim Ofset As Integer = (Math.Sign(dy) * 8) + Math.Sign(dx)
                    Dim TempSquare As Integer = Square1 + Ofset
                    Dim Ray As UInt64 = 0UL
                    While TempSquare <> Square2
                        Ray = Ray Or (1UL << TempSquare)
                        TempSquare += Ofset
                    End While
                    RayMap(64 * Square1 + Square2) = Ray
                End If
            Next
        Next
    End Sub



    'Precomputed Pawn Data
    Protected Shared PawnWhiteAttackMap(63) As UInt64
    Protected Shared PawnBlackAttackMap(63) As UInt64
    Protected Sub PrecomputePawnMaps()
        Dim TempMoveMap As UInt64
        For y As UInt16 = 0 To 7
            For x As UInt16 = 0 To 7
                TempMoveMap = If(x > 0US, 1UL << Flatten2DBoardIndex(x - 1US, y - 1US), 0UL)
                If x < 7US Then TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(x + 1US, y - 1US))
                PawnWhiteAttackMap(Flatten2DBoardIndex(x, y)) = TempMoveMap

                TempMoveMap = If(x > 0US, 1UL << Flatten2DBoardIndex(x - 1US, y + 1US), 0UL)
                If x < 7US Then TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(x + 1US, y + 1US))
                PawnBlackAttackMap(Flatten2DBoardIndex(x, y)) = TempMoveMap
            Next
        Next
    End Sub

    'Precomputed Knight Data
    Protected Shared KnightMoveMap(63) As UInt64
    Protected Sub PrecomputeKnightMap()
        For y As Int16 = 0 To 7
            For x As Int16 = 0 To 7
                Dim TempMoveMap As UInt64 = 0UL
                If x >= 2 Then
                    For Delta As Int16 = -1S To 1S Step 2S
                        Dim CoorY As Int16 = y + Delta
                        If CoorY >= 0 AndAlso CoorY <= 7 Then TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(x - 2S, CoorY))
                    Next
                End If
                If x <= 5 Then
                    For Delta As Int16 = -1S To 1S Step 2S
                        Dim CoorY As Int16 = y + Delta
                        If CoorY >= 0 AndAlso CoorY <= 7 Then TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(x + 2S, CoorY))
                    Next
                End If
                If y >= 2 Then
                    For Delta As Int16 = -1S To 1S Step 2S
                        Dim CoorX As Int16 = x + Delta
                        If CoorX >= 0 AndAlso CoorX <= 7 Then TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(CoorX, y - 2S))
                    Next
                End If
                If y <= 5 Then
                    For Delta As Int16 = -1S To 1S Step 2S
                        Dim CoorX As Int16 = x + Delta
                        If CoorX >= 0 AndAlso CoorX <= 7 Then TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(CoorX, y + 2S))
                    Next
                End If
                KnightMoveMap(Flatten2DBoardIndex(x, y)) = TempMoveMap
            Next
        Next
    End Sub

    'Precomputed Bishop Data. Note we DO NOT stop 1 cell from the edges - this data is held in magic info.
    Private Shared BishopMoveMap(63) As UInt64
    Protected Sub PrecomputeBishopMap()
        For y As Int16 = 0 To 7
            For x As Int16 = 0 To 7
                Dim TempMoveMap As UInt64 = 0
                Dim n, m As Int16
                n = x + 1S
                m = y + 1S
                Do While n <= 7 AndAlso m <= 7
                    TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(n, m))
                    n += 1S
                    m += 1S
                Loop
                n = x + 1S
                m = y - 1S
                Do While n <= 7 AndAlso m >= 0
                    TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(n, m))
                    n += 1S
                    m -= 1S
                Loop
                n = x - 1S
                m = y - 1S
                Do While n >= 0 AndAlso m >= 0
                    TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(n, m))
                    n -= 1S
                    m -= 1S
                Loop
                n = x - 1S
                m = y + 1S
                Do While n >= 0 AndAlso m <= 7
                    TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(n, m))
                    n -= 1S
                    m += 1S
                Loop
                BishopMoveMap(Flatten2DBoardIndex(x, y)) = TempMoveMap
            Next
        Next
    End Sub

    'Precomputed Rook Data. Note we stop 1 cell from the edges, as the occupancy map does not care about this.
    Private Shared RookMoveMap(63) As UInt64
    Protected Sub PrecomputeRookMap()
        For y As UInt16 = 0 To 7
            For x As UInt16 = 0 To 7
                Dim TempMoveMap As UInt64 = 0
                For n = x + 1 To 7
                    TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(CUShort(n), y))
                Next
                For n = x - 1 To 0 Step -1
                    TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(CUShort(n), y))
                Next
                For n = y + 1 To 7
                    TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(x, CUShort(n)))
                Next
                For n = y - 1 To 0 Step -1
                    TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(x, CUShort(n)))
                Next
                RookMoveMap(Flatten2DBoardIndex(x, y)) = TempMoveMap
            Next
        Next
    End Sub

    'Precomputed King Data. The latter 3 structures produce a mask that tells if a given piece could influence the king's motion.
    Private Shared KingMoveMap(63) As UInt64
    Protected Shared KingDangerMapKnight(63) As UInt64
    Protected Shared KingDangerMapBishop(63) As UInt64
    Protected Shared KingDangerMapRook(63) As UInt64
    Protected Sub PrecomputeKingMap()
        For y As Int16 = 0 To 7
            For x As Int16 = 0 To 7
                Dim TempMoveMap As UInt64 = 0UL
                For CoorY As Int16 = Math.Max(0S, y - 1S) To Math.Min(7S, y + 1S)
                    For CoorX As Int16 = Math.Max(0S, x - 1S) To Math.Min(7S, x + 1S)
                        TempMoveMap = TempMoveMap Or (1UL << Flatten2DBoardIndex(CoorX, CoorY))
                    Next
                Next
                KingMoveMap(Flatten2DBoardIndex(x, y)) = TempMoveMap
            Next
        Next
    End Sub
    Protected Sub PrecomputeKingDangerMaps()
        For KingY As Int16 = 0 To 7
            For KingX As Int16 = 0 To 7
                Dim FlatKingIndex As Int16 = Flatten2DBoardIndex(KingX, KingY)
                Dim KingInStartPos As Boolean = FlatKingIndex = 4 OrElse FlatKingIndex = 60

                Dim TempKnightMap As UInt64 = 0
                Dim TempBishopMap As UInt64 = 0
                Dim TempRookMap As UInt64 = 0
                For y As Int16 = 0 To 7
                    For x As Int16 = 0 To 7
                        'Logic taken directly from old FixTFTable sub.
                        Dim PieceSquareIndex As Int16 = Flatten2DBoardIndex(x, y)
                        Dim dx As Int16 = Math.Abs(x - KingX)
                        Dim dy As Int16 = Math.Abs(y - KingY)

                        Dim KnightCanStopCastle As Boolean = (FlatKingIndex = 4 AndAlso PieceSquareIndex = 8) OrElse (FlatKingIndex = 60 AndAlso PieceSquareIndex = 48)
                        If (Math.Max(dx, dy) <= 3 AndAlso dx + dy <= 5) OrElse KnightCanStopCastle Then TempKnightMap = TempKnightMap Or (1UL << PieceSquareIndex)
                        If Math.Abs(dx - dy) <= 2 Then TempBishopMap = TempBishopMap Or (1UL << PieceSquareIndex)
                        If dx <= If(KingInStartPos, 2, 1) OrElse dy <= 1 Then TempRookMap = TempRookMap Or (1UL << PieceSquareIndex)
                    Next
                Next
                KingDangerMapKnight(FlatKingIndex) = TempKnightMap
                KingDangerMapBishop(FlatKingIndex) = TempBishopMap
                KingDangerMapRook(FlatKingIndex) = TempRookMap
            Next
        Next
    End Sub




    'Converts Legal Move Maps into legal moves. TODO: ADD FLAGS!!!!!!
    Private LegalMoveArray(GlobalConstants.MaxPieceLegalMoves + 1) As UInt16
    Private Sub PopulateLegalMoveArray(ByVal Square As UInt16, ByVal EnemyPieceMask As UInt64, ByVal OccupancyMask As UInt64, ByVal MoveMap As UInt64, ByVal IncludeNonCaptures As Boolean, Optional ByVal n As UInt16 = 0)
        Dim StartValue As UInt16 = Square << 6
        Dim TempMap As UInt64 = MoveMap And EnemyPieceMask
        While TempMap <> 0UL
            n += 1US
            LegalMoveArray(n) = 32768US Or StartValue Or CUShort(BitOperations.TrailingZeroCount(TempMap))
            TempMap = TempMap And (TempMap - 1UL)
        End While
        If IncludeNonCaptures Then
            TempMap = MoveMap And Not OccupancyMask 'Looks at non-capture moves only.
            While TempMap <> 0UL
                n += 1US
                LegalMoveArray(n) = StartValue Or CUShort(BitOperations.TrailingZeroCount(TempMap))
                TempMap = TempMap And (TempMap - 1UL)
            End While
        End If
        LegalMoveArray(0) = n
    End Sub


    Public Function WhitePawnLegalMoves(ByVal Square As UInt16, ByVal EnemyPieceMask As UInt64, ByVal OccupancyMask As UInt64, ByVal PinInfoStraight As UInt64, ByVal PinInfoDiag As UInt64, ByVal MeKPos As UInt16, ByVal EnPassant As UInt16, Optional ByVal IncludeNonCaptures As Boolean = True) As UInt16()
        Dim n As UInt16
        Dim StartValue As UInt16 = Square << 6
        Dim PieceMap As UInt64 = 1UL << Square

        'Piece must not be pinned straight to capture. This also covers EnPassant pins.
        If (PinInfoStraight And PieceMap) = 0UL Then
            If EnPassant <> 0US Then EnemyPieceMask = EnemyPieceMask Or (1UL << EnPassant)
            Dim AttackMap As UInt64 = PawnWhiteAttackMap(Square) And EnemyPieceMask
            If (PinInfoDiag And PieceMap) <> 0UL Then AttackMap = AttackMap And BishopMoveMap(MeKPos) 'Only allows the pinned piece to move along the king's ray.
            'Handles EnPassant captures. Crutically, we don't count this as a capture move (but still feed it through into quiescence), so that MakeMove doesn't get confused.
            If EnPassant <> 0US AndAlso (AttackMap And (1UL << EnPassant)) <> 0UL Then
                n += 1US
                LegalMoveArray(n) = 12288US Or StartValue Or EnPassant
                AttackMap = AttackMap Xor (1UL << EnPassant)
            End If
            While AttackMap <> 0UL
                Dim EndSquare As UInt16 = CUShort(BitOperations.TrailingZeroCount(AttackMap))
                Dim MoveBase As UInt16 = StartValue Or EndSquare
                n += 1US
                If EndSquare < 8US Then
                    'We're promoting a pawn! Run for both queen & knight.
                    LegalMoveArray(n) = 36864US Or MoveBase
                    n += 1US
                    LegalMoveArray(n) = 61440US Or MoveBase
                Else
                    LegalMoveArray(n) = 32768US Or MoveBase
                End If
                AttackMap = AttackMap And (AttackMap - 1UL)
            End While
        End If

        If IncludeNonCaptures Then
            'Must not be pinned anything other than vertically.
            If (PinInfoDiag And PieceMap) = 0UL AndAlso ((PinInfoStraight And PieceMap) = 0UL OrElse (MeKPos Mod 8) = (Square Mod 8)) Then
                Dim EndSquare As UInt16 = Square - 8US
                If ((1UL << EndSquare) And OccupancyMask) = 0UL Then
                    Dim MoveBase As UInt16 = StartValue Or EndSquare
                    If Square < 16US Then
                        'Adds flags for pawn promotion.
                        n += 1US
                        LegalMoveArray(n) = 4096US Or MoveBase
                        n += 1US
                        LegalMoveArray(n) = 28672US Or MoveBase
                    Else
                        n += 1US
                        LegalMoveArray(n) = MoveBase
                        If Square > 47US Then
                            'Adds flags for double pawn pushes (later EnPassant creation).
                            Dim DoubleSquare As UInt16 = Square - 16US
                            If ((1UL << DoubleSquare) And OccupancyMask) = 0UL Then
                                n += 1US
                                LegalMoveArray(n) = 8192US Or StartValue Or DoubleSquare
                            End If
                        End If
                    End If
                End If
            End If
        End If

        LegalMoveArray(0) = n
        Return LegalMoveArray
    End Function
    Public Function BlackPawnLegalMoves(ByVal Square As UInt16, ByVal EnemyPieceMask As UInt64, ByVal OccupancyMask As UInt64, ByVal PinInfoStraight As UInt64, ByVal PinInfoDiag As UInt64, ByVal MeKPos As UInt16, ByVal EnPassant As UInt16, Optional ByVal IncludeNonCaptures As Boolean = True) As UInt16()
        Dim n As UInt16 = 0
        Dim StartValue As UInt16 = Square << 6
        Dim PieceMap As UInt64 = 1UL << Square
        If (PinInfoStraight And PieceMap) = 0UL Then
            If EnPassant <> 0US Then EnemyPieceMask = EnemyPieceMask Or (1UL << EnPassant)
            Dim AttackMap As UInt64 = PawnBlackAttackMap(Square) And EnemyPieceMask
            If (PinInfoDiag And PieceMap) <> 0UL Then AttackMap = AttackMap And BishopMoveMap(MeKPos) 'Only allows the pinned piece to move along the king's ray.
            If EnPassant <> 0US AndAlso (AttackMap And (1UL << EnPassant)) <> 0UL Then
                n += 1US
                LegalMoveArray(n) = 12288US Or StartValue Or EnPassant
                AttackMap = AttackMap Xor (1UL << EnPassant)
            End If
            While AttackMap <> 0UL
                Dim EndSquare As UInt16 = CUShort(BitOperations.TrailingZeroCount(AttackMap))
                n += 1US
                If EndSquare > 55US Then
                    LegalMoveArray(n) = 36864US Or StartValue Or EndSquare
                    n += 1US
                    LegalMoveArray(n) = 61440US Or StartValue Or EndSquare
                Else
                    LegalMoveArray(n) = 32768US Or StartValue Or EndSquare
                End If
                AttackMap = AttackMap And (AttackMap - 1UL)
            End While
        End If
        If IncludeNonCaptures Then
            If (PinInfoDiag And PieceMap) = 0UL AndAlso ((PinInfoStraight And PieceMap) = 0UL OrElse (MeKPos Mod 8) = (Square Mod 8)) Then
                Dim EndSquare As UInt16 = Square + 8US
                If ((1UL << EndSquare) And OccupancyMask) = 0UL Then
                    Dim MoveBase As UInt16 = StartValue Or EndSquare
                    If Square > 47US Then
                        n += 1US
                        LegalMoveArray(n) = 4096US Or MoveBase
                        n += 1US
                        LegalMoveArray(n) = 28672US Or MoveBase
                    Else
                        n += 1US
                        LegalMoveArray(n) = MoveBase
                        If Square < 16US Then
                            Dim DoubleSquare As UInt16 = Square + 16US
                            If ((1UL << DoubleSquare) And OccupancyMask) = 0UL Then
                                n += 1US
                                LegalMoveArray(n) = 8192US Or StartValue Or DoubleSquare
                            End If
                        End If
                    End If
                End If
            End If
        End If
        LegalMoveArray(0) = n
        Return LegalMoveArray
    End Function

    Public Function KingLegalMoves(ByVal Square As UInt16, ByVal EnemyPieceMask As UInt64, ByVal OccupancyMask As UInt64, ByVal TFTable As UInt64, ByVal MeCanCastle As CanCastle, ByVal MeInCheck As UInt16, Optional ByVal IncludeNonCaptures As Boolean = True) As UInt16()
        Dim n As UInt16 = 0
        Dim StartValue As UInt16 = Square << 6
        'Note that kings cannot be pinned (that would be funny though). Calculaes movement mask through the TFTable, saying which squares are not protected.
        Dim MoveMap As UInt64 = (KingMoveMap(Square) And TFTable)
        Dim TempMap As UInt64 = MoveMap And EnemyPieceMask
        While TempMap <> 0UL
            n += 1US
            LegalMoveArray(n) = 32768US Or StartValue Or CUShort(BitOperations.TrailingZeroCount(TempMap))
            TempMap = TempMap And (TempMap - 1UL)
        End While
        If IncludeNonCaptures Then
            TempMap = MoveMap And Not OccupancyMask 'Looks at non-capture moves only.
            While TempMap <> 0UL
                n += 1US
                LegalMoveArray(n) = StartValue Or CUShort(BitOperations.TrailingZeroCount(TempMap))
                TempMap = TempMap And (TempMap - 1UL)
            End While
            'Code for castling. Note that we cannot castle out of check. To keep things universal for each piece, we shift left and right (taking
            'advantage of the fact that MakeMove removes castling privileges if we move from the starting position. As all rook handling with
            'castling rights is made by MakeMove, we assume that KS = True implies the rook is present in the board's corner.
            If MeInCheck = 0US Then
                If MeCanCastle.KS Then
                    Dim isWhite As Boolean = Square > 7US
                    'We create a mask for all the squares that need to be empty for the king to be able to castle.
                    'For KS castling, this is the same as all the squares that cannot be attacked for the king to be able to castle.
                    Dim CastleMask As UInt64 = If(isWhite, &H6000000000000000UL, 96UL)
                    If (CastleMask And (Not OccupancyMask) And TFTable) = CastleMask Then
                        n += 1US
                        LegalMoveArray(n) = If(isWhite, 24382US, 20742US)  'King-side castle move.
                    End If
                End If
                If MeCanCastle.QS Then
                    Dim isWhite As Boolean = Square > 7US
                    'Unlike KS castling, QS castling requires an extra square to be empty, but not necessarily attacked by the enemy.
                    Dim CastleEmptyMask As UInt64 = If(isWhite, &HE00000000000000UL, 14UL)
                    Dim CastleAttackMask As UInt64 = If(isWhite, &HC00000000000000UL, 12UL)
                    If (CastleEmptyMask And (Not OccupancyMask)) = CastleEmptyMask AndAlso (CastleAttackMask And TFTable) = CastleAttackMask Then
                        n += 1US
                        LegalMoveArray(n) = If(isWhite, 28474US, 24834US)  'Queen-side castle move.
                    End If
                End If
            End If
        End If
        LegalMoveArray(0) = n
        Return LegalMoveArray
    End Function

    Public Function KnightLegalMoves(ByVal Square As UInt16, ByVal EnemyPieceMask As UInt64, ByVal OccupancyMask As UInt64, ByVal PinInfo As UInt64, Optional ByVal IncludeNonCaptures As Boolean = True) As UInt16()
        'Knights cannot move at all if they are pinned.
        If (PinInfo And (1UL << Square)) = 0UL Then PopulateLegalMoveArray(Square, EnemyPieceMask, OccupancyMask, KnightMoveMap(Square), IncludeNonCaptures) Else LegalMoveArray(0) = 0
        Return LegalMoveArray
    End Function

    Public Function BishopLegalMoves(ByVal Square As UInt16, ByVal EnemyPieceMask As UInt64, ByVal OccupancyMask As UInt64, ByVal PinInfoStraight As UInt64, ByVal PinInfoDiag As UInt64, ByVal MeKPos As UInt16, Optional ByVal IncludeNonCaptures As Boolean = True) As UInt16()
        Dim PieceMap As UInt64 = 1UL << Square
        'Bishops cannot move at all if they are pinned straight.
        If (PinInfoStraight And PieceMap) = 0UL Then
            Dim MoveMap As UInt64 = BishopMagicLookup(Square, OccupancyMask)
            If (PinInfoDiag And PieceMap) <> 0UL Then MoveMap = MoveMap And BishopMoveMap(MeKPos) 'Only allows the pinned piece to move along the king's ray.
            PopulateLegalMoveArray(Square, EnemyPieceMask, OccupancyMask, MoveMap, IncludeNonCaptures)
        Else
            LegalMoveArray(0) = 0
        End If
        Return LegalMoveArray
    End Function

    Public Function RookLegalMoves(ByVal Square As UInt16, ByVal EnemyPieceMask As UInt64, ByVal OccupancyMask As UInt64, ByVal PinInfoStraight As UInt64, ByVal PinInfoDiag As UInt64, ByVal MeKPos As UInt16, Optional ByVal IncludeNonCaptures As Boolean = True) As UInt16()
        Dim PieceMap As UInt64 = 1UL << Square
        'Rooks cannot move at all if they are pinned diagonally.
        If (PinInfoDiag And PieceMap) = 0UL Then
            Dim MoveMap As UInt64 = RookMagicLookup(Square, OccupancyMask)
            If (PinInfoStraight And PieceMap) <> 0UL Then MoveMap = MoveMap And RookMoveMap(MeKPos) 'Only allows the pinned piece to move along the king's ray.
            PopulateLegalMoveArray(Square, EnemyPieceMask, OccupancyMask, MoveMap, IncludeNonCaptures)
        Else
            LegalMoveArray(0) = 0
        End If
        Return LegalMoveArray
    End Function

    Public Function QueenLegalMoves(ByVal Square As UInt16, ByVal EnemyPieceMask As UInt64, ByVal OccupancyMask As UInt64, ByVal PinInfoStraight As UInt64, ByVal PinInfoDiag As UInt64, ByVal MeKPos As UInt16, Optional ByVal IncludeNonCaptures As Boolean = True) As UInt16()
        Dim PieceMap As UInt64 = 1UL << Square
        Dim MoveMap As UInt64
        LegalMoveArray(0) = 0

        'Restrict straight queen moves if is pinned diagonally.
        If (PinInfoDiag And PieceMap) = 0UL Then
            MoveMap = RookMagicLookup(Square, OccupancyMask)
            If (PinInfoStraight And PieceMap) <> 0UL Then MoveMap = MoveMap And RookMoveMap(MeKPos) 'Only allows the pinned piece to move along the king's ray.
            PopulateLegalMoveArray(Square, EnemyPieceMask, OccupancyMask, MoveMap, IncludeNonCaptures)
        End If
        'Restrict diagonal queen moves if is pinned straight.
        If (PinInfoStraight And PieceMap) = 0UL Then
            MoveMap = BishopMagicLookup(Square, OccupancyMask)
            If (PinInfoDiag And PieceMap) <> 0UL Then MoveMap = MoveMap And BishopMoveMap(MeKPos) 'Only allows the pinned piece to move along the king's ray.
            PopulateLegalMoveArray(Square, EnemyPieceMask, OccupancyMask, MoveMap, IncludeNonCaptures, LegalMoveArray(0))
        End If
        Return LegalMoveArray
    End Function


    Public Function BishopMagicLookup(ByVal Square As UInt16, ByVal OccupancyMask As UInt64) As UInt64
        Dim BlockerMap As UInt64 = MagicBishopInfo(Square).MovementMask And OccupancyMask
        Return MagicBishopInfo(Square).MoveMap(CInt((BlockerMap * MagicBishopInfo(Square).Magic) >> MagicBishopInfo(Square).Shift))
    End Function
    Public Function RookMagicLookup(ByVal Square As UInt16, ByVal OccupancyMask As UInt64) As UInt64
        Dim BlockerMap As UInt64 = MagicRookInfo(Square).MovementMask And OccupancyMask
        Return MagicRookInfo(Square).MoveMap(CInt((BlockerMap * MagicRookInfo(Square).Magic) >> MagicRookInfo(Square).Shift))
    End Function

End Class
