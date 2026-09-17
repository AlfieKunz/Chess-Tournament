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
    Private Shared PawnWhiteAttackMap(63) As UInt64
    Private Shared PawnBlackAttackMap(63) As UInt64
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
    Private Shared KnightMoveMap(63) As UInt64
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

    'Precomputed King Data
    Private Shared KingMoveMap(63) As UInt64
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
            If EnPassant <> 0S Then EnemyPieceMask = EnemyPieceMask Or (1UL << EnPassant)
            Dim AttackMap As UInt64 = PawnWhiteAttackMap(Square) And EnemyPieceMask
            If (PinInfoDiag And PieceMap) <> 0UL Then AttackMap = AttackMap And BishopMoveMap(MeKPos) 'Only allows the pinned piece to move along the king's ray.
            'Handles EnPassant captures.
            If EnPassant <> 0S AndAlso (AttackMap And (1UL << EnPassant)) <> 0UL Then
                n += 1US
                LegalMoveArray(n) = 45056US Or StartValue Or EnPassant
                AttackMap = AttackMap Xor (1UL << EnPassant)
            End If
            While AttackMap <> 0UL
                Dim EndSquare As UInt16 = CUShort(BitOperations.TrailingZeroCount(AttackMap))
                n += 1US
                If EndSquare < 8US Then
                    'We're promoting a pawn! Run for both queen & knight.
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
            'Must not be pinned anything other than vertically.
            If (PinInfoDiag And PieceMap) = 0UL AndAlso ((PinInfoStraight And PieceMap) = 0UL OrElse (MeKPos Mod 8) = (Square Mod 8)) Then
                Dim EndSquare As UInt16 = Square - 8US
                If ((1UL << EndSquare) And OccupancyMask) = 0UL Then
                    n += 1US
                    LegalMoveArray(n) = StartValue Or EndSquare
                    If Square \ 8US = 6US Then
                        'Adds flags for double pawn pushes (later EnPassant creation).
                        If ((1UL << (Square - 16)) And OccupancyMask) = 0UL Then
                            n += 1US
                            LegalMoveArray(n) = 8192US Or StartValue Or (Square - 16US)
                        End If
                    ElseIf Square < 16US Then
                        'Adds flags for pawn promotion.
                        n += 1US
                        LegalMoveArray(n) = 4096US Or StartValue Or EndSquare
                        n += 1US
                        LegalMoveArray(n) = 28672US Or StartValue Or EndSquare
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
            If EnPassant <> 0S Then EnemyPieceMask = EnemyPieceMask Or (1UL << EnPassant)
            Dim AttackMap As UInt64 = PawnBlackAttackMap(Square) And EnemyPieceMask
            If (PinInfoDiag And PieceMap) <> 0UL Then AttackMap = AttackMap And BishopMoveMap(MeKPos) 'Only allows the pinned piece to move along the king's ray.
            If EnPassant <> 0S AndAlso (AttackMap And (1UL << EnPassant)) <> 0UL Then
                n += 1US
                LegalMoveArray(n) = 45056US Or StartValue Or EnPassant
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
                    n += 1US
                    LegalMoveArray(n) = StartValue Or EndSquare
                    If Square \ 8US = 1US Then
                        If ((1UL << (Square + 16)) And OccupancyMask) = 0UL Then
                            n += 1US
                            LegalMoveArray(n) = 8192US Or StartValue Or (Square + 16US)
                        End If
                    ElseIf Square > 47US Then
                        n += 1US
                        LegalMoveArray(n) = 4096US Or StartValue Or EndSquare
                        n += 1US
                        LegalMoveArray(n) = 28672US Or StartValue Or EndSquare
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
                        LegalMoveArray(n) = If(isWhite, 24382US, 41222US)  'King-side castle move.
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
        Dim Magic As MagicInfo = MagicBishopInfo(Square)
        Dim BlockerMap As UInt64 = Magic.MovementMask And OccupancyMask
        Return Magic.MoveMap(CInt((BlockerMap * Magic.Magic) >> Magic.Shift))
    End Function
    Public Function RookMagicLookup(ByVal Square As UInt16, ByVal OccupancyMask As UInt64) As UInt64
        Dim Magic As MagicInfo = MagicRookInfo(Square)
        Dim BlockerMap As UInt64 = Magic.MovementMask And OccupancyMask
        Return Magic.MoveMap(CInt((BlockerMap * Magic.Magic) >> Magic.Shift))
    End Function






















    'Attribute that contains all the legal moves for a knight, placed anywhere on the board. When the program initally boots,
    'we simulate a knight being placed on each square on the board, and log its pseudo-legal moves into the KnightLegalMoveArray
    'structure. This is so that, when we want to determine the pseudo-legal moves of a knight on the board, we just look up
    'the information in KnightLegalMoveArray.
    Private Shared KnightLegalMoveArray(7, 7, 8) As UInt16
    Protected Sub PrecomputeKnightLegalMoveArray()
        Dim LegalMoveArray(8) As UInt16
        Dim Counter As UInt16

        For CoorY As UInt16 = 0 To 7
            For CoorX As UInt16 = 0 To 7
                'Resets the LegalMoveArray structure.
                For n As Byte = 0 To 8
                    LegalMoveArray(n) = CoorX << 9 Or CoorY << 6
                Next

                Counter = 1

                'Calculates the pseudo-legal moves for a knight placed at the given square.
                If CoorX >= 2 Then
                    For x = -1S To 1S Step 2S
                        If (CoorY + x) >= 0 AndAlso (CoorY + x) <= 7 Then
                            LegalMoveArray(Counter) = LegalMoveArray(Counter) Or (CoorX - 2US) << 3US Or CUShort(CoorY + x)
                            Counter += 1US
                        End If
                    Next
                End If
                If CoorX <= 5 Then
                    For x = -1S To 1S Step 2S
                        If (CoorY + x) >= 0 AndAlso (CoorY + x) <= 7 Then
                            LegalMoveArray(Counter) = LegalMoveArray(Counter) Or (CoorX + 2US) << 3US Or CUShort(CoorY + x)
                            Counter += 1US
                        End If
                    Next
                End If
                If CoorY >= 2 Then
                    For y = -1S To 1S Step 2S
                        If (CoorX + y) >= 0 AndAlso (CoorX + y) <= 7 Then
                            LegalMoveArray(Counter) = LegalMoveArray(Counter) Or CUShort(CoorX + y) << 3US Or (CoorY - 2US)
                            Counter += 1US
                        End If
                    Next
                End If
                If CoorY <= 5 Then
                    For y = -1S To 1S Step 2S
                        If (CoorX + y) >= 0 AndAlso (CoorX + y) <= 7 Then
                            LegalMoveArray(Counter) = LegalMoveArray(Counter) Or CUShort(CoorX + y) << 3US Or (CoorY + 2US)
                            Counter += 1US
                        End If
                    Next
                End If

                'Stores the total number of moves into the first index of LegalMoveArray, then copies this array
                'into the corresponding index in KnightLegalMoveArray.
                LegalMoveArray(0) = Counter - 1US
                For n = 0 To Counter - 1US
                    KnightLegalMoveArray(CoorX, CoorY, n) = LegalMoveArray(n)
                Next
            Next
        Next
    End Sub



    'The next 2 functions (WhitePieceLegalMoves and BlackPieceLegalMoves) receive a single piece on the board,
    'and determines all its posible moves - assigning them to the array LegalMoveArray. Alongside this, the overloads
    'for this sub also builds to one of the two TrueFalse Tables using the squares which a piece attacks as markers. In
    'this table, the program checks for pinned pieces using the Enemy King's Position - extending a piece's line of sight
    'if the King is on the same axis as it. If it finds a King, then the Pinned Piece is labelled from 0-3, which
    'represent which direction it is pinned from (0 = vertical, 1 = btm left to top right diagonal, 2 = horizontal,
    '3 = top left to btm right diagonal). This is useful as a pinned pawn pinned vertically can still move
    'upwardly, but cannot take an enemy piece diagonally (which would expose the king), so I have also implemented
    'the logic for pieces moving depending on if / where they are pinned from.

    'Note: to help reduce unnecessary commenting, and due to the fact that much of this code is somewhat similar
    'for each type of piece, comments will only be included for the first appearance of a particular method / technique.
    'For more in-depth commenting & info, please see the section on Pseudo-legal Move Generation in my Project Report (Design). 
    Public Function WhitePieceLegalMoves(ByRef Board(,) As Char, ByVal CoorX As UInt16, ByVal CoorY As UInt16, ByRef WhiteTFTable(,) As Char, ByVal WInCheck As UInt16, ByRef WCanCastle As CanCastle, ByVal EnPassant As Int16) As UInt16()
        Dim n As UInt16 = 1
        Dim StartValue As UInt16 = CoorX << 9 Or CoorY << 6
        Dim TFTValue As Char = WhiteTFTable(CoorX, CoorY)

        If Board(CoorX, CoorY) = "P"c Then 'Legal Moves for the Pawn (along with First Moves).
            If TFTValue <> "2"c Then

                For PPCount As UInt16 = 4096 To 28672 Step 24576
                    If CoorY = 1 Then StartValue = StartValue Or PPCount 'All moves with this pawn will result in a promotion -
                    'add promotion flag to all moves (first iteration for queen promotions, the next for knight promotions.

                    'If the pawn is not pinned horizontally...
                    If TFTValue <> "1"c AndAlso TFTValue <> "3"c Then
                        If Board(CoorX, CoorY - 1) = " "c Then
                            LegalMoveArray(n) = StartValue Or CoorX << 3US Or (CoorY - 1US)
                            n += 1US
                            If CoorY = 6 AndAlso Board(CoorX, 4) = " "c Then 'Pawns can move two squares on their first move.
                                LegalMoveArray(n) = StartValue Or 8192US Or CoorX << 3 Or 4US
                                n += 1US
                            End If
                        End If
                    End If
                    If TFTValue <> "0"c Then
                        'Code for pawn left captures.
                        If CoorX > 0 AndAlso TFTValue <> "1"c Then
                            If Char.IsLower(Board(CoorX - 1, CoorY - 1)) Then
                                LegalMoveArray(n) = StartValue Or (CoorX - 1US) << 3 Or (CoorY - 1US)
                                n += 1US
                            ElseIf EnPassant <> 0 AndAlso CoorY = 3 AndAlso (CoorX - 1) = (EnPassant >> 3) AndAlso TFTValue <> "4"c Then
                                'En-Passant capture.
                                LegalMoveArray(n) = StartValue Or 12288US Or CUShort(EnPassant)
                                n += 1US
                            End If
                        End If
                        'Code for pawn right captures.
                        If CoorX < 7 AndAlso TFTValue <> "3"c Then
                            If Char.IsLower(Board(CoorX + 1, CoorY - 1)) Then
                                LegalMoveArray(n) = StartValue Or (CoorX + 1US) << 3 Or (CoorY - 1US)
                                n += 1US
                            ElseIf EnPassant <> 0 AndAlso CoorY = 3 AndAlso (CoorX + 1) = (EnPassant >> 3) AndAlso TFTValue <> "4"c Then
                                LegalMoveArray(n) = StartValue Or 12288US Or CUShort(EnPassant)
                                n += 1US
                            End If
                        End If
                    End If

                    If CoorY <> 1 Then Exit For
                Next
            End If


        ElseIf Board(CoorX, CoorY) = "N"c Then 'Legal Moves for the Knight. (If pinned, then it cannot move at all.)
            If TFTValue >= "5"c Then
                Dim TestMove As UInt16
                'Retrieves moves from the KnightLegalMove lookup table.
                For m = 1 To KnightLegalMoveArray(CoorX, CoorY, 0)
                    TestMove = KnightLegalMoveArray(CoorX, CoorY, m)
                    If Not Char.IsUpper(Board((TestMove And 56) >> 3, TestMove And 7)) Then
                        LegalMoveArray(n) = TestMove
                        n += 1US
                    End If
                Next
            End If


        ElseIf Board(CoorX, CoorY) = "K"c Then 'Legal Moves for the King (along with Castling).
            'Sets coordinate bounds. Start & End Coords, in this case, represents the 8 squares around the king.
            For y = If(CoorY = 0US, 0US, CoorY - 1US) To If(CoorY = 7US, 7US, CoorY + 1US)
                For x = If(CoorX = 0US, 0US, CoorX - 1US) To If(CoorX = 7US, 7US, CoorX + 1US)
                    If Not Char.IsUpper(Board(x, y)) AndAlso WhiteTFTable(x, y) = "T"c Then
                        'Therefore is a legal move...
                        LegalMoveArray(n) = StartValue Or x << 3US Or y
                        n += 1US
                    End If
                Next
            Next
            If WInCheck < 128 Then
                If WCanCastle.KS Then
                    If Board(5, 7) = " "c AndAlso Board(6, 7) = " "c AndAlso WhiteTFTable(5, 7) = "T"c AndAlso WhiteTFTable(6, 7) = "T"c Then
                        'As all rook handling with castling rights is made by MakeMove, we assume that KS = True implies Board(7, 7) = "R".
                        LegalMoveArray(n) = 23031  'King-side castle flag.
                        n += 1US
                    End If
                End If
                If WCanCastle.QS Then
                    If Board(3, 7) = " "c AndAlso Board(2, 7) = " "c AndAlso Board(1, 7) = " "c AndAlso WhiteTFTable(3, 7) = "T"c AndAlso WhiteTFTable(2, 7) = "T"c Then
                        LegalMoveArray(n) = 27095 'Square king would go to to castle queen-side.
                        n += 1US
                    End If
                End If
            End If


        Else
            If Board(CoorX, CoorY) = "R"c OrElse Board(CoorX, CoorY) = "Q"c Then 'Legal Moves for the Rook / part of Queen.
                If TFTValue <> "1"c AndAlso TFTValue <> "3"c Then
                    If TFTValue <> "0"c Then 'if rook is not pinned vertically...
                        'Right Movement.
                        For x = (CoorX + 1US) To 7US
                            If Char.IsUpper(Board(x, CoorY)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = StartValue Or x << 3 Or CoorY
                                n += 1US
                                If Board(x, CoorY) <> " "c Then Exit For
                            End If
                        Next
                        'Left Movement
                        For x = CShort(CoorX) - 1S To 0S Step -1S
                            If Char.IsUpper(Board(x, CoorY)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = StartValue Or CUShort(x) << 3 Or CoorY
                                n += 1US
                                If Board(x, CoorY) <> " "c Then Exit For
                            End If
                        Next
                    End If

                    'Down Movement.
                    If TFTValue <> "2"c Then
                        For y = (CoorY + 1US) To 7US
                            If Char.IsUpper(Board(CoorX, y)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = StartValue Or CoorX << 3 Or y
                                n += 1US
                                If Board(CoorX, y) <> " "c Then Exit For
                            End If
                        Next

                        'Up Movement.
                        For y = CShort(CoorY) - 1S To 0S Step -1S
                            If Char.IsUpper(Board(CoorX, y)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = StartValue Or CoorX << 3 Or CUShort(y)
                                n += 1US
                                If Board(CoorX, y) <> " "c Then Exit For
                            End If
                        Next
                    End If
                End If
            End If


            If Board(CoorX, CoorY) = "B"c OrElse Board(CoorX, CoorY) = "Q"c Then 'Legal Moves for the Biship / Part of Queen.
                If TFTValue <> "0"c AndAlso TFTValue <> "2"c Then
                    Dim StartX, StartY As UInt16 'Pointers for the coordinate bounds.
                    If TFTValue <> "1"c Then
                        'Right-Down Movement.
                        StartX = CoorX
                        StartY = CoorY
                        Do Until StartX = 7 OrElse StartY = 7
                            StartX += 1US
                            StartY += 1US
                            If Char.IsUpper(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartValue Or StartX << 3 Or StartY
                                n += 1US
                                If Board(StartX, StartY) <> " "c Then Exit Do
                            End If
                        Loop

                        'Left-Up Movement.
                        StartX = CoorX
                        StartY = CoorY
                        Do Until StartX = 0 OrElse StartY = 0
                            StartX -= 1US
                            StartY -= 1US
                            If Char.IsUpper(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartValue Or StartX << 3 Or StartY
                                n += 1US
                                If Board(StartX, StartY) <> " "c Then Exit Do
                            End If
                        Loop

                    End If
                    If TFTValue <> "3"c Then
                        'Right-Up Movement.
                        StartX = CoorX
                        StartY = CoorY
                        Do Until StartX = 7 OrElse StartY = 0
                            StartX += 1US
                            StartY -= 1US
                            If Char.IsUpper(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartValue Or StartX << 3 Or StartY
                                n += 1US
                                If Board(StartX, StartY) <> " "c Then Exit Do
                            End If
                        Loop

                        'Left-Down Movement.
                        StartX = CoorX
                        StartY = CoorY
                        Do Until StartX = 0 OrElse StartY = 7
                            StartX -= 1US
                            StartY += 1US
                            If Char.IsUpper(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartValue Or StartX << 3 Or StartY
                                n += 1US
                                If Board(StartX, StartY) <> " "c Then Exit Do
                            End If
                        Loop
                    End If
                End If
            End If
        End If

        'Shrinks the array to the correct size, so that there are no blank entries.
        LegalMoveArray(0) = n - 1US
        Return LegalMoveArray
    End Function


    'As the below subroutine is very similar to its equivilant 'WhitePieceLegalMoves' function, commenting will be limited.
    Public Function BlackPieceLegalMoves(ByRef Board(,) As Char, ByVal CoorX As UInt16, ByVal CoorY As UInt16, ByRef BlackTFTable(,) As Char, ByVal BInCheck As UInt16, ByRef BCanCastle As CanCastle, ByVal EnPassant As Int16) As UInt16()
        Dim n As UInt16 = 1
        Dim StartValue As UInt16 = CoorX << 9 Or CoorY << 6
        Dim TFTValue As Char = BlackTFTable(CoorX, CoorY)

        If Board(CoorX, CoorY) = "p"c Then 'Legal Moves for the Pawn (along with First Moves.)
            If TFTValue <> "2"c Then

                For PPCount As UInt16 = 4096 To 28672 Step 24576
                    If CoorY = 6 Then StartValue = StartValue Or PPCount

                    If TFTValue <> "1"c AndAlso TFTValue <> "3"c Then
                        If Board(CoorX, CoorY + 1) = " "c Then
                            LegalMoveArray(n) = StartValue Or CoorX << 3US Or (CoorY + 1US)
                            n += 1US
                            If CoorY = 1 AndAlso Board(CoorX, 3) = " "c Then
                                LegalMoveArray(n) = StartValue Or 8192US Or CoorX << 3 Or 3US
                                n += 1US
                            End If
                        End If
                    End If
                    If TFTValue <> "0"c Then
                        If CoorX > 0 AndAlso TFTValue <> "3"c Then
                            If Char.IsUpper(Board(CoorX - 1, CoorY + 1)) Then
                                LegalMoveArray(n) = StartValue Or (CoorX - 1US) << 3 Or (CoorY + 1US)
                                n += 1US
                            ElseIf EnPassant <> 0 AndAlso CoorY = 4 AndAlso (CoorX - 1) = (EnPassant >> 3) AndAlso TFTValue <> "4"c Then
                                LegalMoveArray(n) = StartValue Or 12288US Or CUShort(EnPassant)
                                n += 1US
                            End If
                        End If
                        If CoorX < 7 AndAlso TFTValue <> "1"c Then
                            If Char.IsUpper(Board(CoorX + 1, CoorY + 1)) Then
                                LegalMoveArray(n) = StartValue Or (CoorX + 1US) << 3 Or (CoorY + 1US)
                                n += 1US
                            ElseIf EnPassant <> 0 AndAlso CoorY = 4 AndAlso (CoorX + 1) = (EnPassant >> 3) AndAlso TFTValue <> "4"c Then
                                LegalMoveArray(n) = StartValue Or 12288US Or CUShort(EnPassant)
                                n += 1US
                            End If
                        End If
                    End If

                    If CoorY <> 6 Then Exit For
                Next
            End If


        ElseIf Board(CoorX, CoorY) = "n"c Then 'Legal Moves for the Knight. (If pinned, then it cannot move at all.)
            If TFTValue >= "5"c Then
                Dim TestMove As UInt16
                For m = 1 To KnightLegalMoveArray(CoorX, CoorY, 0)
                    TestMove = KnightLegalMoveArray(CoorX, CoorY, m)
                    If Not Char.IsLower(Board((TestMove And 56) >> 3, TestMove And 7)) Then
                        LegalMoveArray(n) = TestMove
                        n += 1US
                    End If
                Next
            End If


        ElseIf Board(CoorX, CoorY) = "k"c Then 'Legal Moves for the King (Along with Castling).
            For y = If(CoorY = 0US, 0US, CoorY - 1US) To If(CoorY = 7US, 7US, CoorY + 1US)
                For x = If(CoorX = 0US, 0US, CoorX - 1US) To If(CoorX = 7US, 7US, CoorX + 1US)
                    If Not Char.IsLower(Board(x, y)) AndAlso BlackTFTable(x, y) = "T"c Then
                        LegalMoveArray(n) = StartValue Or x << 3 Or y
                        n += 1US
                    End If
                Next
            Next
            If BInCheck < 128 Then
                If BCanCastle.KS Then
                    If Board(5, 0) = " "c AndAlso Board(6, 0) = " "c AndAlso BlackTFTable(5, 0) = "T"c AndAlso BlackTFTable(6, 0) = "T"c Then
                        LegalMoveArray(n) = StartValue Or 22576US
                        n += 1US
                    End If
                End If
                If BCanCastle.QS Then
                    If Board(3, 0) = " "c AndAlso Board(2, 0) = " "c AndAlso Board(1, 0) = " "c AndAlso BlackTFTable(3, 0) = "T"c AndAlso BlackTFTable(2, 0) = "T"c Then
                        LegalMoveArray(n) = StartValue Or 26640US
                        n += 1US
                    End If
                End If
            End If


        Else
            If Board(CoorX, CoorY) = "r"c OrElse Board(CoorX, CoorY) = "q"c Then 'Legal Moves for the Rook / Part of Queen.
                If TFTValue <> "1"c AndAlso TFTValue <> "3"c Then
                    If TFTValue <> "0"c Then
                        For x = (CoorX + 1US) To 7US
                            If Char.IsLower(Board(x, CoorY)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = StartValue Or x << 3 Or CoorY
                                n += 1US
                                If Board(x, CoorY) <> " "c Then Exit For
                            End If
                        Next
                        For x = CShort(CoorX) - 1S To 0S Step -1S
                            If Char.IsLower(Board(x, CoorY)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = StartValue Or CUShort(x) << 3 Or CoorY
                                n += 1US
                                If Board(x, CoorY) <> " "c Then Exit For
                            End If
                        Next
                    End If

                    If TFTValue <> "2"c Then
                        For y = (CoorY + 1US) To 7US
                            If Char.IsLower(Board(CoorX, y)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = StartValue Or CoorX << 3 Or y
                                n += 1US
                                If Board(CoorX, y) <> " "c Then Exit For
                            End If
                        Next

                        For y = CShort(CoorY) - 1S To 0S Step -1S
                            If Char.IsLower(Board(CoorX, y)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = StartValue Or CoorX << 3 Or CUShort(y)
                                n += 1US
                                If Board(CoorX, y) <> " "c Then Exit For
                            End If
                        Next
                    End If
                End If
            End If


            If Board(CoorX, CoorY) = "b"c OrElse Board(CoorX, CoorY) = "q"c Then 'Legal Moves for the Bishop / Part of Queen.
                If TFTValue <> "0"c AndAlso TFTValue <> "2"c Then
                    Dim StartX, StartY As UInt16 'Pointers for the coordinate bounds.
                    If TFTValue <> "1"c Then
                        StartX = CoorX
                        StartY = CoorY
                        Do Until StartX = 7 OrElse StartY = 7
                            StartX += 1US
                            StartY += 1US
                            If Char.IsLower(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartValue Or StartX << 3 Or StartY
                                n += 1US
                                If Board(StartX, StartY) <> " "c Then Exit Do
                            End If
                        Loop

                        StartX = CoorX
                        StartY = CoorY
                        Do Until StartX = 0 OrElse StartY = 0
                            StartX -= 1US
                            StartY -= 1US
                            If Char.IsLower(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartValue Or StartX << 3 Or StartY
                                n += 1US
                                If Board(StartX, StartY) <> " "c Then Exit Do
                            End If
                        Loop
                    End If

                    If TFTValue <> "3"c Then
                        StartX = CoorX
                        StartY = CoorY
                        Do Until StartX = 7 OrElse StartY = 0
                            StartX += 1US
                            StartY -= 1US
                            If Char.IsLower(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartValue Or StartX << 3 Or StartY
                                n += 1US
                                If Board(StartX, StartY) <> " "c Then Exit Do
                            End If
                        Loop

                        StartX = CoorX
                        StartY = CoorY
                        Do Until StartX = 0 OrElse StartY = 7
                            StartX -= 1US
                            StartY += 1US
                            If Char.IsLower(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartValue Or StartX << 3 Or StartY
                                n += 1US
                                If Board(StartX, StartY) <> " "c Then Exit Do
                            End If
                        Loop
                    End If
                End If
            End If
        End If

        LegalMoveArray(0) = n - 1US
        Return LegalMoveArray
    End Function




    Public Sub WhitePieceLegalMoves(ByRef Board(,) As Char, ByVal CoorX As UInt16, ByVal CoorY As UInt16, ByRef BlackTFTable(,) As Char, ByVal BKPos As UInt16, ByRef BInCheck As UInt16, ByVal EnPassant As UInt16)
        Dim CheckingPiece As UInt16 = 65535US
        Dim AlreadyInCheck As Boolean = (BInCheck >= 128US)

        If Board(CoorX, CoorY) = "P"c Then 'Legal Moves for the Pawn (along with First Moves).
            If CoorX > 0 Then
                If BlackTFTable(CoorX - 1, CoorY - 1) = "T"c Then BlackTFTable(CoorX - 1, CoorY - 1) = "F"c
                If Board(CoorX - 1, CoorY - 1) = "k"c Then
                    BInCheck = BInCheck Or 128US
                    CheckingPiece = (CoorX << 3) Or CoorY
                End If
            End If
            'Code for pawn right captures.
            If CoorX < 7 Then
                If BlackTFTable(CoorX + 1, CoorY - 1) = "T"c Then BlackTFTable(CoorX + 1, CoorY - 1) = "F"c
                If Board(CoorX + 1, CoorY - 1) = "k"c Then
                    BInCheck = BInCheck Or 128US
                    CheckingPiece = (CoorX << 3) Or CoorY
                End If
            End If


        ElseIf Board(CoorX, CoorY) = "N"c Then 'Legal Moves for the Knight. (If pinned, then it cannot move at all.)
            Dim XPos, YPos As UInt16
            For m = 1US To KnightLegalMoveArray(CoorX, CoorY, 0)
                XPos = (KnightLegalMoveArray(CoorX, CoorY, m) And 56US) >> 3
                YPos = KnightLegalMoveArray(CoorX, CoorY, m) And 7US
                If BlackTFTable(XPos, YPos) = "T"c Then BlackTFTable(XPos, YPos) = "F"c
                If Board(XPos, YPos) = "k"c Then
                    BInCheck = BInCheck Or 128US
                    CheckingPiece = (CoorX << 3) Or CoorY
                End If
            Next


        ElseIf Board(CoorX, CoorY) = "K"c Then
            For y = If(CoorY = 0US, 0US, CoorY - 1US) To If(CoorY = 7US, 7US, CoorY + 1US)
                For x = If(CoorX = 0US, 0US, CoorX - 1US) To If(CoorX = 7US, 7US, CoorX + 1US)
                    If BlackTFTable(x, y) = "T"c Then BlackTFTable(x, y) = "F"c
                Next
            Next


        Else
            If Board(CoorX, CoorY) = "R"c OrElse Board(CoorX, CoorY) = "Q"c Then 'Legal Moves for the Rook / part of Queen.
                For x = (CoorX + 1US) To 7US
                    If BlackTFTable(x, CoorY) = "T"c Then BlackTFTable(x, CoorY) = "F"c
                    If Char.IsUpper(Board(x, CoorY)) Then
                        If EnPassant <> 0 AndAlso Board(x, CoorY) = "P"c AndAlso (x << 3 Or (CoorY + 1)) = EnPassant AndAlso ((BKPos And 56) >> 3) > x AndAlso (BKPos And 7) = CoorY Then
                            'Initiate pin checks (inc friendly-first EnPassant pins).
                            For m = x + 2US To 7US
                                If Board(m, CoorY) = "k"c Then
                                    BlackTFTable(x + 1, CoorY) = "4"c
                                    Exit For
                                End If
                                'There is another piece in the way - cancel the search.
                                If Board(m, CoorY) <> " "c Then Exit For
                            Next
                        End If
                        Exit For
                    Else
                        If Board(x, CoorY) = "k"c Then
                            BInCheck = BInCheck Or 128US
                            CheckingPiece = (CoorX << 3) Or CoorY
                            'Marks the next square as illegal for the king to move to.
                            If BlackTFTable(Math.Min(x + 1, 7), CoorY) = "T"c Then BlackTFTable(Math.Min(x + 1, 7), CoorY) = "F"c
                            Exit For
                        End If
                        If Char.IsLower(Board(x, CoorY)) AndAlso ((BKPos And 56) >> 3) > x AndAlso (BKPos And 7) = CoorY Then
                            'Initiate pin checks.
                            For m = x + 1US To 7US
                                If Board(m, CoorY) = "k"c Then
                                    'Enemy king spotted - mark piece as pinned.
                                    BlackTFTable(x, CoorY) = "2"c
                                    Exit For
                                End If
                                'Checks for enemy-first EnPassant pins.
                                If Board(m, CoorY) <> " "c Then
                                    If EnPassant <> 0 AndAlso Board(m, CoorY) = "P"c AndAlso (m << 3 Or (CoorY + 1)) = EnPassant Then
                                        'Pawn should be stopped from taking En-Passant.
                                        For a = m + 1US To 7US
                                            If Board(a, CoorY) = "k"c Then
                                                BlackTFTable(x, CoorY) = "4"c
                                                Exit For
                                            End If
                                            If Board(a, CoorY) <> " "c Then Exit For
                                        Next
                                    End If
                                    Exit For
                                End If
                            Next
                        End If

                        If Board(x, CoorY) <> " "c Then Exit For
                    End If
                Next

                For x = CShort(CoorX) - 1S To 0S Step -1S
                    If BlackTFTable(x, CoorY) = "T"c Then BlackTFTable(x, CoorY) = "F"c
                    If Char.IsUpper(Board(x, CoorY)) Then
                        If EnPassant <> 0 AndAlso Board(x, CoorY) = "P" AndAlso (x << 3 Or (CoorY + 1)) = EnPassant AndAlso ((BKPos And 56) >> 3) < x AndAlso (BKPos And 7) = CoorY Then
                            For m = x - 2S To 0S Step -1S
                                If Board(m, CoorY) = "k" Then
                                    BlackTFTable(x - 1, CoorY) = "4"c
                                    Exit For
                                End If
                                If Board(m, CoorY) <> " "c Then Exit For
                            Next
                        End If
                        Exit For
                    Else
                        If Board(x, CoorY) = "k"c Then
                            BInCheck = BInCheck Or 128US
                            CheckingPiece = (CoorX << 3) Or CoorY
                            If BlackTFTable(Math.Max(x - 1, 0), CoorY) = "T"c Then BlackTFTable(Math.Max(x - 1, 0), CoorY) = "F"c
                            Exit For
                        End If
                        If Char.IsLower(Board(x, CoorY)) AndAlso ((BKPos And 56) >> 3) < x AndAlso (BKPos And 7) = CoorY Then
                            For m = x - 1S To 0S Step -1S
                                If Board(m, CoorY) = "k"c Then
                                    BlackTFTable(x, CoorY) = "2"c
                                    Exit For
                                End If
                                If Board(m, CoorY) <> " "c Then
                                    If EnPassant <> 0 AndAlso Board(m, CoorY) = "P"c AndAlso (m << 3 Or (CoorY + 1)) = EnPassant Then
                                        For a = m - 1S To 0S Step -1S
                                            If Board(a, CoorY) = "k"c Then
                                                BlackTFTable(x, CoorY) = "4"c
                                                Exit For
                                            End If
                                            If Board(a, CoorY) <> " "c Then Exit For
                                        Next
                                    End If
                                    Exit For
                                End If
                            Next
                        End If

                        If Board(x, CoorY) <> " "c Then Exit For
                    End If
                Next

                For y = (CoorY + 1US) To 7US
                    If BlackTFTable(CoorX, y) = "T"c Then BlackTFTable(CoorX, y) = "F"c
                    If Char.IsUpper(Board(CoorX, y)) Then
                        Exit For
                    Else
                        If Board(CoorX, y) = "k"c Then
                            BInCheck = BInCheck Or 128US
                            CheckingPiece = (CoorX << 3) Or CoorY
                            If BlackTFTable(CoorX, Math.Min(y + 1, 7)) = "T"c Then BlackTFTable(CoorX, Math.Min(y + 1, 7)) = "F"c
                            Exit For
                        End If
                        If Char.IsLower(Board(CoorX, y)) AndAlso ((BKPos And 56) >> 3) = CoorX AndAlso (BKPos And 7) > y Then
                            For m = y + 1US To 7US
                                If Board(CoorX, m) = "k" Then
                                    BlackTFTable(CoorX, y) = "0"c
                                    Exit For
                                End If
                                If Board(CoorX, m) <> " "c Then Exit For
                            Next
                        End If
                        If Board(CoorX, y) <> " " Then Exit For
                    End If
                Next

                For y = CShort(CoorY) - 1S To 0S Step -1S
                    If BlackTFTable(CoorX, y) = "T"c Then BlackTFTable(CoorX, y) = "F"c
                    If Char.IsUpper(Board(CoorX, y)) Then
                        Exit For
                    Else
                        If Board(CoorX, y) = "k"c Then
                            BInCheck = BInCheck Or 128US
                            CheckingPiece = (CoorX << 3) Or CoorY
                            If BlackTFTable(CoorX, Math.Max(y - 1, 0)) = "T"c Then BlackTFTable(CoorX, Math.Max(y - 1, 0)) = "F"c
                            Exit For
                        End If
                        If Char.IsLower(Board(CoorX, y)) AndAlso ((BKPos And 56) >> 3) = CoorX AndAlso (BKPos And 7) < y Then
                            For m = y - 1S To 0S Step -1S
                                If Board(CoorX, m) = "k"c Then
                                    BlackTFTable(CoorX, y) = "0"c
                                    Exit For
                                End If
                                If Board(CoorX, m) <> " "c Then Exit For
                            Next
                        End If

                        If Board(CoorX, y) <> " "c Then Exit For
                    End If
                Next
            End If


            If Board(CoorX, CoorY) = "B"c OrElse Board(CoorX, CoorY) = "Q"c Then 'Legal Moves for the Biship / Part of Queen.
                Dim StartX, StartY, o, p As UInt16 'These represent temporary x and y coordinates, used for pin detection.
                StartX = CoorX
                StartY = CoorY
                Do Until StartX = 7 OrElse StartY = 7
                    StartX += 1US
                    StartY += 1US
                    If BlackTFTable(StartX, StartY) = "T"c Then BlackTFTable(StartX, StartY) = "F"c
                    If Char.IsUpper(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "k"c Then
                            BInCheck = BInCheck Or 128US
                            CheckingPiece = (CoorX << 3) Or CoorY
                            If StartX < 7 AndAlso StartY < 7 Then
                                If BlackTFTable(StartX + 1, StartY + 1) = "T"c Then BlackTFTable(StartX + 1, StartY + 1) = "F"c
                            End If
                            Exit Do
                        End If
                        If Char.IsLower(Board(StartX, StartY)) AndAlso (((BKPos And 56) >> 3) - CoorX = (BKPos And 7) - CoorY) AndAlso ((BKPos And 56) >> 3) > CoorX Then
                            o = StartX
                            p = StartY
                            Do Until o = 7 OrElse p = 7 'until the end of the board is reached.
                                o += 1US
                                p += 1US
                                If Board(o, p) = "k"c Then
                                    BlackTFTable(StartX, StartY) = "3"c
                                    Exit Do
                                End If
                                If Board(o, p) <> " "c Then Exit Do
                            Loop
                        End If
                        If Board(StartX, StartY) <> " "c Then Exit Do
                    End If
                Loop

                StartX = CoorX
                StartY = CoorY
                Do Until StartX = 0 OrElse StartY = 0
                    StartX -= 1US
                    StartY -= 1US
                    If BlackTFTable(StartX, StartY) = "T"c Then BlackTFTable(StartX, StartY) = "F"c
                    If Char.IsUpper(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "k"c Then
                            BInCheck = BInCheck Or 128US
                            CheckingPiece = (CoorX << 3) Or CoorY
                            If StartX > 0 AndAlso StartY > 0 Then
                                If BlackTFTable(StartX - 1, StartY - 1) = "T"c Then BlackTFTable(StartX - 1, StartY - 1) = "F"c
                            End If
                            Exit Do
                        End If
                        If Char.IsLower(Board(StartX, StartY)) AndAlso (((BKPos And 56) >> 3) - CoorX = (BKPos And 7) - CoorY) AndAlso ((BKPos And 56) >> 3) < CoorX Then
                            o = StartX
                            p = StartY
                            Do Until o = 0 OrElse p = 0
                                o -= 1US
                                p -= 1US
                                If Board(o, p) = "k"c Then
                                    BlackTFTable(StartX, StartY) = "3"c
                                    Exit Do
                                End If
                                If Board(o, p) <> " "c Then Exit Do
                            Loop
                        End If
                        If Board(StartX, StartY) <> " "c Then Exit Do
                    End If
                Loop


                StartX = CoorX
                StartY = CoorY
                Do Until StartX = 7 OrElse StartY = 0
                    StartX += 1US
                    StartY -= 1US
                    If BlackTFTable(StartX, StartY) = "T"c Then BlackTFTable(StartX, StartY) = "F"c
                    If Char.IsUpper(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "k"c Then
                            BInCheck = BInCheck Or 128US
                            CheckingPiece = (CoorX << 3) Or CoorY
                            If StartX < 7 AndAlso StartY > 0 Then
                                If BlackTFTable(StartX + 1, StartY - 1) = "T"c Then BlackTFTable(StartX + 1, StartY - 1) = "F"c
                            End If
                            Exit Do
                        End If
                        If Char.IsLower(Board(StartX, StartY)) AndAlso (((BKPos And 56) >> 3) - CoorX = CoorY - (BKPos And 7)) AndAlso ((BKPos And 56) >> 3) > CoorX Then
                            o = StartX
                            p = StartY
                            Do Until o = 7 OrElse p = 0
                                o += 1US
                                p -= 1US
                                If Board(o, p) = "k"c Then
                                    BlackTFTable(StartX, StartY) = "1"c
                                    Exit Do
                                End If
                                If Board(o, p) <> " "c Then Exit Do
                            Loop
                        End If

                        If Board(StartX, StartY) <> " "c Then Exit Do
                    End If
                Loop

                StartX = CoorX
                StartY = CoorY
                Do Until StartX = 0 OrElse StartY = 7
                    StartX -= 1US
                    StartY += 1US
                    If BlackTFTable(StartX, StartY) = "T"c Then BlackTFTable(StartX, StartY) = "F"c
                    If Char.IsUpper(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "k"c Then
                            BInCheck = BInCheck Or 128US
                            CheckingPiece = (CoorX << 3) Or CoorY
                            If StartX > 0 AndAlso StartY < 7 Then
                                If BlackTFTable(StartX - 1, StartY + 1) = "T"c Then BlackTFTable(StartX - 1, StartY + 1) = "F"c
                            End If
                            Exit Do
                        End If
                        If Char.IsLower(Board(StartX, StartY)) AndAlso (((BKPos And 56) >> 3) - CoorX = CoorY - (BKPos And 7)) AndAlso ((BKPos And 56) >> 3) < CoorX Then
                            o = StartX
                            p = StartY
                            Do Until o = 0 OrElse p = 7
                                o -= 1US
                                p += 1US
                                If Board(o, p) = "k"c Then
                                    BlackTFTable(StartX, StartY) = "1"c
                                    Exit Do
                                End If
                                If Board(o, p) <> " "c Then Exit Do
                            Loop
                        End If
                        If Board(StartX, StartY) <> " "c Then Exit Do
                    End If
                Loop
            End If
        End If

        'At the end of the function, the program checks to see if any piece is attacking the enemy king.
        'If it is, then the other player is signalled to be in check, and the attacking piece is recorded.
        If CheckingPiece < 65535US Then
            If Not AlreadyInCheck Then
                BInCheck = BInCheck Or CheckingPiece
            ElseIf (BInCheck And 63US) <> CheckingPiece Then
                BInCheck = BInCheck Or 64US
            End If
        End If
    End Sub

    'As the below subroutine is very similar to its equivilant 'WhitePieceLegalMoves' function, commenting will be limited.
    Public Sub BlackPieceLegalMoves(ByRef Board(,) As Char, ByVal CoorX As UInt16, ByVal CoorY As UInt16, ByRef WhiteTFTable(,) As Char, ByVal WKPos As UInt16, ByRef WInCheck As UInt16, ByVal EnPassant As UInt16)
        Dim CheckingPiece As UInt16 = 65535US
        Dim AlreadyInCheck As Boolean = (WInCheck >= 128US)

        If Board(CoorX, CoorY) = "p"c Then 'Legal Moves for the Pawn (along with First Moves.)
            If CoorX > 0 Then
                If WhiteTFTable(CoorX - 1, CoorY + 1) = "T"c Then WhiteTFTable(CoorX - 1, CoorY + 1) = "F"c
                If Board(CoorX - 1, CoorY + 1) = "K"c Then
                    WInCheck = WInCheck Or 128US
                    CheckingPiece = CoorX << 3 Or CoorY
                End If
            End If
            If CoorX < 7 Then
                If WhiteTFTable(CoorX + 1, CoorY + 1) = "T"c Then WhiteTFTable(CoorX + 1, CoorY + 1) = "F"c
                If Board(CoorX + 1, CoorY + 1) = "K"c Then
                    WInCheck = WInCheck Or 128US
                    CheckingPiece = CoorX << 3 Or CoorY
                End If
            End If


        ElseIf Board(CoorX, CoorY) = "n"c Then 'Legal Moves for the Knight. (If pinned, then it cannot move at all.)
            Dim XPos, YPos As UInt16
            For m = 1US To KnightLegalMoveArray(CoorX, CoorY, 0)
                XPos = (KnightLegalMoveArray(CoorX, CoorY, m) And 56US) >> 3
                YPos = KnightLegalMoveArray(CoorX, CoorY, m) And 7US
                If WhiteTFTable(XPos, YPos) = "T"c Then WhiteTFTable(XPos, YPos) = "F"c
                If Board(XPos, YPos) = "K"c Then
                    WInCheck = WInCheck Or 128US
                    CheckingPiece = CoorX << 3 Or CoorY
                End If
            Next


        ElseIf Board(CoorX, CoorY) = "k"c Then 'Legal Moves for the King (Along with Castling).
            For y = If(CoorY = 0US, 0US, CoorY - 1US) To If(CoorY = 7US, 7US, CoorY + 1US)
                For x = If(CoorX = 0US, 0US, CoorX - 1US) To If(CoorX = 7US, 7US, CoorX + 1US)
                    If WhiteTFTable(x, y) = "T"c Then WhiteTFTable(x, y) = "F"c
                Next
            Next


        Else
            If Board(CoorX, CoorY) = "r"c OrElse Board(CoorX, CoorY) = "q"c Then 'Legal Moves for the Rook / Part of Queen.
                For x = (CoorX + 1US) To 7US
                    If WhiteTFTable(x, CoorY) = "T"c Then WhiteTFTable(x, CoorY) = "F"c
                    If Char.IsLower(Board(x, CoorY)) Then
                        If EnPassant <> 0 AndAlso Board(x, CoorY) = "p"c AndAlso (x << 3 Or (CoorY - 1)) = EnPassant AndAlso ((WKPos And 56) >> 3) > x AndAlso (WKPos And 7) = CoorY Then
                            For m = x + 2US To 7US
                                If Board(m, CoorY) = "K"c Then
                                    WhiteTFTable(x + 1, CoorY) = "4"c
                                    Exit For
                                End If
                                If Board(m, CoorY) <> " "c Then Exit For
                            Next
                        End If
                        Exit For
                    Else
                        If Board(x, CoorY) = "K"c Then
                            WInCheck = WInCheck Or 128US
                            CheckingPiece = CoorX << 3 Or CoorY
                            If WhiteTFTable(Math.Min(x + 1, 7), CoorY) = "T" Then WhiteTFTable(Math.Min(x + 1, 7), CoorY) = "F"c
                            Exit For
                        End If
                        If Char.IsUpper(Board(x, CoorY)) AndAlso ((WKPos And 56) >> 3) > x AndAlso (WKPos And 7) = CoorY Then
                            For m = x + 1US To 7US
                                If Board(m, CoorY) = "K"c Then
                                    WhiteTFTable(x, CoorY) = "2"c
                                    Exit For
                                End If
                                If Board(m, CoorY) <> " "c Then
                                    If EnPassant <> 0 AndAlso Board(m, CoorY) = "p"c AndAlso (m << 3 Or (CoorY - 1)) = EnPassant Then
                                        For a = m + 1US To 7US
                                            If Board(a, CoorY) = "K"c Then
                                                WhiteTFTable(x, CoorY) = "4"c
                                                Exit For
                                            End If
                                            If Board(a, CoorY) <> " "c Then Exit For
                                        Next
                                    End If
                                    Exit For
                                End If
                            Next
                        End If
                        If Board(x, CoorY) <> " "c Then Exit For
                    End If
                Next

                For x = CShort(CoorX) - 1S To 0S Step -1S
                    If WhiteTFTable(x, CoorY) = "T"c Then WhiteTFTable(x, CoorY) = "F"c
                    If Char.IsLower(Board(x, CoorY)) Then
                        If EnPassant <> 0 AndAlso Board(x, CoorY) = "p"c AndAlso (x << 3 Or (CoorY - 1)) = EnPassant AndAlso ((WKPos And 56) >> 3) < x AndAlso (WKPos And 7) = CoorY Then
                            For m = x - 2S To 0S Step -1S
                                If Board(m, CoorY) = "K"c Then
                                    WhiteTFTable(x - 1, CoorY) = "4"c
                                    Exit For
                                End If
                                If Board(m, CoorY) <> " "c Then Exit For
                            Next
                        End If
                        Exit For
                    Else
                        If Board(x, CoorY) = "K"c Then
                            WInCheck = WInCheck Or 128US
                            CheckingPiece = CoorX << 3 Or CoorY
                            If WhiteTFTable(Math.Max(x - 1, 0), CoorY) = "T"c Then WhiteTFTable(Math.Max(x - 1, 0), CoorY) = "F"c
                            Exit For
                        End If
                        If Char.IsUpper(Board(x, CoorY)) AndAlso ((WKPos And 56) >> 3) < x AndAlso (WKPos And 7) = CoorY Then
                            For m = x - 1S To 0S Step -1S
                                If Board(m, CoorY) = "K"c Then
                                    WhiteTFTable(x, CoorY) = "2"c
                                    Exit For
                                End If
                                If Board(m, CoorY) <> " "c Then
                                    If EnPassant <> 0 AndAlso Board(m, CoorY) = "p"c AndAlso (m << 3 Or (CoorY - 1)) = EnPassant Then
                                        For a = m - 1S To 0S Step -1S
                                            If Board(a, CoorY) = "K" Then
                                                WhiteTFTable(x, CoorY) = "4"c
                                                Exit For
                                            End If
                                            If Board(a, CoorY) <> " "c Then Exit For
                                        Next
                                    End If
                                    Exit For
                                End If
                            Next
                        End If
                        If Board(x, CoorY) <> " "c Then Exit For
                    End If
                Next

                For y = (CoorY + 1US) To 7US
                    If WhiteTFTable(CoorX, y) = "T"c Then WhiteTFTable(CoorX, y) = "F"c
                    If Char.IsLower(Board(CoorX, y)) Then
                        Exit For
                    Else
                        If Board(CoorX, y) = "K"c Then
                            WInCheck = WInCheck Or 128US
                            CheckingPiece = CoorX << 3 Or CoorY
                            If WhiteTFTable(CoorX, Math.Min(y + 1, 7)) = "T"c Then WhiteTFTable(CoorX, Math.Min(y + 1, 7)) = "F"c
                            Exit For
                        End If
                        If Char.IsUpper(Board(CoorX, y)) AndAlso ((WKPos And 56) >> 3) = CoorX AndAlso (WKPos And 7) > y Then
                            For m = y + 1US To 7US
                                If Board(CoorX, m) = "K"c Then
                                    WhiteTFTable(CoorX, y) = "0"c
                                    Exit For
                                End If
                                If Board(CoorX, m) <> " "c Then Exit For
                            Next
                        End If
                        If Board(CoorX, y) <> " "c Then Exit For
                    End If
                Next

                For y = CShort(CoorY) - 1S To 0S Step -1S
                    If WhiteTFTable(CoorX, y) = "T"c Then WhiteTFTable(CoorX, y) = "F"c
                    If Char.IsLower(Board(CoorX, y)) Then
                        Exit For
                    Else
                        If Board(CoorX, y) = "K"c Then
                            WInCheck = WInCheck Or 128US
                            CheckingPiece = CoorX << 3 Or CoorY
                            If WhiteTFTable(CoorX, Math.Max(y - 1, 0)) = "T"c Then WhiteTFTable(CoorX, Math.Max(y - 1, 0)) = "F"c
                            Exit For
                        End If
                        If Char.IsUpper(Board(CoorX, y)) AndAlso ((WKPos And 56) >> 3) = CoorX AndAlso (WKPos And 7) < y Then
                            For m = y - 1S To 0S Step -1S
                                If Board(CoorX, m) = "K"c Then
                                    WhiteTFTable(CoorX, y) = "0"c
                                    Exit For
                                End If
                                If Board(CoorX, m) <> " "c Then Exit For
                            Next
                        End If
                        If Board(CoorX, y) <> " "c Then Exit For
                    End If
                Next
            End If


            If Board(CoorX, CoorY) = "b"c OrElse Board(CoorX, CoorY) = "q"c Then 'Legal Moves for the Bishop / Part of Queen.
                Dim StartX, StartY, o, p As UInt16 'These represent temporary x and y coordinates, used for pin detection.
                StartX = CoorX
                StartY = CoorY
                Do Until StartX = 7 OrElse StartY = 7
                    StartX += 1US
                    StartY += 1US
                    If WhiteTFTable(StartX, StartY) = "T"c Then WhiteTFTable(StartX, StartY) = "F"c
                    If Char.IsLower(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "K"c Then
                            WInCheck = WInCheck Or 128US
                            CheckingPiece = CoorX << 3 Or CoorY
                            If StartX < 7 AndAlso StartY < 7 Then
                                If WhiteTFTable(StartX + 1, StartY + 1) = "T"c Then WhiteTFTable(StartX + 1, StartY + 1) = "F"c
                            End If
                            Exit Do
                        End If
                        If Char.IsUpper(Board(StartX, StartY)) AndAlso (((WKPos And 56) >> 3) - CoorX = (WKPos And 7) - CoorY) AndAlso ((WKPos And 56) >> 3) > CoorX Then
                            o = StartX
                            p = StartY
                            Do Until o = 7 OrElse p = 7
                                o += 1US
                                p += 1US
                                If Board(o, p) = "K"c Then
                                    WhiteTFTable(StartX, StartY) = "3"c
                                    Exit Do
                                End If
                                If Board(o, p) <> " "c Then Exit Do
                            Loop
                        End If
                        If Board(StartX, StartY) <> " "c Then Exit Do
                    End If
                Loop

                StartX = CoorX
                StartY = CoorY
                Do Until StartX = 0 OrElse StartY = 0
                    StartX -= 1US
                    StartY -= 1US
                    If WhiteTFTable(StartX, StartY) = "T"c Then WhiteTFTable(StartX, StartY) = "F"c
                    If Char.IsLower(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "K"c Then
                            WInCheck = WInCheck Or 128US
                            CheckingPiece = CoorX << 3 Or CoorY
                            If StartX > 0 AndAlso StartY > 0 Then
                                If WhiteTFTable(StartX - 1, StartY - 1) = "T"c Then WhiteTFTable(StartX - 1, StartY - 1) = "F"c
                            End If
                            Exit Do
                        End If
                        If Char.IsUpper(Board(StartX, StartY)) AndAlso (((WKPos And 56) >> 3) - CoorX = (WKPos And 7) - CoorY) AndAlso ((WKPos And 56) >> 3) < CoorX Then
                            o = StartX
                            p = StartY
                            Do Until o = 0 OrElse p = 0
                                o -= 1US
                                p -= 1US
                                If Board(o, p) = "K"c Then
                                    WhiteTFTable(StartX, StartY) = "3"c
                                    Exit Do
                                End If
                                If Board(o, p) <> " "c Then Exit Do
                            Loop
                        End If
                        If Board(StartX, StartY) <> " "c Then Exit Do
                    End If
                Loop

                StartX = CoorX
                StartY = CoorY
                Do Until StartX = 7 OrElse StartY = 0
                    StartX += 1US
                    StartY -= 1US
                    If WhiteTFTable(StartX, StartY) = "T"c Then WhiteTFTable(StartX, StartY) = "F"c
                    If Char.IsLower(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "K"c Then
                            WInCheck = WInCheck Or 128US
                            CheckingPiece = CoorX << 3 Or CoorY
                            If StartX < 7 AndAlso StartY > 0 Then
                                If WhiteTFTable(StartX + 1, StartY - 1) = "T"c Then WhiteTFTable(StartX + 1, StartY - 1) = "F"c
                            End If
                            Exit Do
                        End If
                        If Char.IsUpper(Board(StartX, StartY)) AndAlso (((WKPos And 56) >> 3) - CoorX = CoorY - (WKPos And 7)) AndAlso ((WKPos And 56) >> 3) > CoorX Then
                            o = StartX
                            p = StartY
                            Do Until o = 7 OrElse p = 0
                                o += 1US
                                p -= 1US
                                If Board(o, p) = "K"c Then
                                    WhiteTFTable(StartX, StartY) = "1"c
                                    Exit Do
                                End If
                                If Board(o, p) <> " "c Then Exit Do
                            Loop
                        End If
                        If Board(StartX, StartY) <> " "c Then Exit Do
                    End If
                Loop

                StartX = CoorX
                StartY = CoorY
                Do Until StartX = 0 OrElse StartY = 7
                    StartX -= 1US
                    StartY += 1US
                    If WhiteTFTable(StartX, StartY) = "T"c Then WhiteTFTable(StartX, StartY) = "F"c
                    If Char.IsLower(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "K"c Then
                            WInCheck = WInCheck Or 128US
                            CheckingPiece = CoorX << 3 Or CoorY
                            If StartX > 0 AndAlso StartY < 7 Then
                                If WhiteTFTable(StartX - 1, StartY + 1) = "T"c Then WhiteTFTable(StartX - 1, StartY + 1) = "F"c
                            End If
                            Exit Do
                        End If
                        If Char.IsUpper(Board(StartX, StartY)) AndAlso (((WKPos And 56) >> 3) - CoorX = CoorY - (WKPos And 7)) AndAlso ((WKPos And 56) >> 3) < CoorX Then
                            o = StartX
                            p = StartY
                            Do Until o = 0 OrElse p = 7
                                o -= 1US
                                p += 1US
                                If Board(o, p) = "K"c Then
                                    WhiteTFTable(StartX, StartY) = "1"c
                                    Exit Do
                                End If
                                If Board(o, p) <> " "c Then Exit Do
                            Loop
                        End If
                        If Board(StartX, StartY) <> " "c Then Exit Do
                    End If
                Loop
            End If
        End If

        If CheckingPiece < 65535US Then
            If Not AlreadyInCheck Then
                WInCheck = WInCheck Or CheckingPiece
            ElseIf (WInCheck And 63US) <> CheckingPiece Then
                WInCheck = WInCheck Or 64US
            End If
        End If
    End Sub

End Class
