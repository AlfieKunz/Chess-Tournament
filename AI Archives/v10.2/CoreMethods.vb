Option Strict On

'This class contains most of the primary algorithms I will be using in my project, and will link to both my Chess class and
'my AI class (either via instavintiation or by inheritance). It will contain the algorithms that will be used by both my Chess
'& AI classes, such as the ‘TFTable’ Generator, ‘DoesMoveResolveCheck’, 'Move Converters', and others.
Imports System.Data.OleDb
Imports System.Numerics
Imports System.Runtime.CompilerServices
Imports System.Xml
Imports System.Xml.XPath
Imports Chess_AI.GlobalConstants

Partial Public Class CoreMethods
    Protected Shared LegacyPieceIndexConverter(9) As Integer 'Methods using the Board(,) structure use "Asc(PIECE) Mod 11" or "(Asc(piece) + 1) Mod 11"
    'for indexing into PieceValue, MVVLVAValues, ZobristHashTable. We convert this to the structure that bitboards use (indexing through unique
    'PieceIndex.Piece value) by storing said indices in this array. TODO: clear these from AI.vb
    Protected Shared ReadOnly PieceValue(5) As Integer 'Array Containing the Value or Weight of each Piece.
    Protected Shared MVVLVAValues(29) As UInt16 'Array Containing the score associated with each possible capture configuration in chess.
    'This is used for move ordering, and represents the premise of encouraging high captures, and capturing _with_ low material.

    Protected Shared ReadOnly ZobristHashTable(767) As UInt64 '(a, b, c), where a = piece type, b = piece colour, c = square.
    'a is Similar to PieceValue: use (Asc(UCase(PieceName)) Mod 11) to calculate - [2] used for EnPassant square.
    Protected Shared ReadOnly ZobristHashConstants(12) As UInt64 '0-7 = EnPassant Square information, 8 = Player Turn, 9 = WhiteKSCastle, 10 = WhiteQSCastle, 11 = BlackKSCastle, 12 = BlackQSCastle.
    Public Sub New()
        LegacyPieceIndexConverter = {2, 1, -1, 0, 4, 3, -1, -1, -1, 5}
        PieceValue(GlobalConstants.PieceIndex.Pawn) = GlobalConstants.PieceWeight.Pawn 'Pawn Weight
        PieceValue(GlobalConstants.PieceIndex.Knight) = GlobalConstants.PieceWeight.Knight 'Knight Weight
        PieceValue(GlobalConstants.PieceIndex.Bishop) = GlobalConstants.PieceWeight.Bishop 'Bishop Weight
        PieceValue(GlobalConstants.PieceIndex.Rook) = GlobalConstants.PieceWeight.Rook 'Rook Weight
        PieceValue(GlobalConstants.PieceIndex.Queen) = GlobalConstants.PieceWeight.Queen 'Queen Weight
        PieceValue(GlobalConstants.PieceIndex.King) = GlobalConstants.PieceWeight.King 'Queen Weight

        'Loads the appropriate values into MVA-LVA. For more info, see rustic-chess.org/search/ordering/mvv_lva.html
        MVVLVAValues = {
            15, 14, 13, 12, 11, 10, ' Victim: Pawn   (P, N, B, R, Q, K)
            25, 24, 23, 22, 21, 20, ' Victim: Knight (P, N, B, R, Q, K)
            35, 34, 33, 32, 31, 30, ' Victim: Bishop (P, N, B, R, Q, K)
            45, 44, 43, 42, 41, 40, ' Victim: Rook   (P, N, B, R, Q, K)
            55, 54, 53, 52, 51, 50 ' Victim: Queen  (P, N, B, R, Q, K)
        }

        'Fills ZobristHasTable with pseudo-random 64-bit numbers
        Static RND As New Random()
        Dim Buffer(7) As Byte
        For PieceIndex = 0 To 5
            For Turn = 0 To 1
                For Square = 0 To 63
                    RND.NextBytes(Buffer)
                    ZobristHashTable((128 * PieceIndex) + (64 * Turn) + Square) = BitConverter.ToUInt64(Buffer, 0)
                Next
            Next
        Next
        'Fills HasConstants with random 64-bit numbers.
        For n As Byte = 0 To 12
            RND.NextBytes(Buffer)
            ZobristHashConstants(n) = BitConverter.ToUInt64(Buffer, 0)
        Next
    End Sub
    <MethodImpl(MethodImplOptions.AggressiveInlining)>
    Public Function GetZobristHashTableValue(ByVal Piece As Integer, ByVal Colour As Integer, ByVal Square As Integer) As UInt64
        Return ZobristHashTable((128 * Piece) + (64 * Colour) + Square)
    End Function
    <MethodImpl(MethodImplOptions.AggressiveInlining)>
    Public Function GetMVVLVAValue(ByVal VictimPiece As Integer, ByVal AttackingPiece As Integer) As UInt16
        Return MVVLVAValues(6 * VictimPiece + AttackingPiece)
    End Function




    Private ZeroAsc As Integer = AscW("0"c)
    Public Function CCharInt(ByVal Chr As Char) As Integer
        Return AscW(Chr) - ZeroAsc
    End Function



    'Functions that convert between positions (eg: 53) and the standard chess coordinate notation (eg: f5)
    Public Function CoorToPGNConverter(ByVal Position As String) As String '53 --> f5
        If Position.Length <> 2 Then Return Position
        Return Chr(Val(Position(0)) + 97) & 8 - Val(Position(1))
    End Function
    Public Function PGNtoCoorConverter(ByVal Position As String) As String 'f5 --> 53
        Return Asc(Position(0)) - 97 & 8 - Val(Position(1))
    End Function
    Public Function SquareToPGNConverter(ByVal Square As Integer) As String
        Return Chr((Square Mod 8) + 97) & 8 - (Square \ 8)
    End Function




    'Function which puts the required pieces in a FEN board position into an 8x8 board array.
    Public Function FENConverter(ByVal FEN As String, ByRef WCanCastle As CanCastle, ByRef BCanCastle As CanCastle, ByRef WKPos As String, ByRef BKPos As String, ByRef EnPassant As String, ByRef IsWhite As Boolean) As Char(,)
        'Resets castling & EnPassant rules
        WCanCastle.CannotCastle()
        BCanCastle.CannotCastle()
        EnPassant = "-"
        Dim x, y As Integer
        Dim tempArray(7, 7) As Char
        Dim SpaceLocation As Integer
        For n = 0 To Len(FEN) - 1
            Select Case FEN(n)
                Case "/"c '= end of board row. Reset column index and increment row index.
                    y += 1
                    If x <> 8 Then Throw New Exception($"Incorrect Length on Row {y}.")
                    x = 0
                Case "A"c To "Z"c, "a"c To "z"c '= name of piece - add name of piece to board index.
                    tempArray(x, y) = FEN(n)
                    If FEN(n) = "K"c Then
                        WKPos = x & y
                    ElseIf FEN(n) = "k"c Then
                        BKPos = x & y
                    End If
                    x += 1
                Case "0"c To "8"c 'Set of empty squares.
                    For m = 1 To Integer.Parse(FEN(n))
                        tempArray(x, y) = " "c
                        x += 1
                    Next
                Case " "c
                    'Checks for pawns incorrectly placed on the 1st or 8th rank. If we detect any, then we provoke an
                    'intentional crash, which our encapsulating Try-Catch detects and promptly reverses.
                    For m As Byte = 0 To 7
                        If UCase(tempArray(m, 0)) = "P" OrElse UCase(tempArray(m, 7)) = "P" Then Throw New Exception("Invalid Pawn Placements.")
                    Next
                    'Once the SPACE has been reached In the FEN, we know that we have read the entire board. We can then move onto
                    'info about castling, who's turn it is, En Passant And more. Because of this, we must have reached the h1 square.
                    If x <> 8 Then Throw New Exception($"Incorrect Length on Row {y}.")
                    SpaceLocation = FEN.IndexOf(" "c) 'Following characters revolve around location of SPACE.
                    If FEN(SpaceLocation + 1) = "w" Then
                        IsWhite = True
                    Else
                        IsWhite = False
                    End If
                    FEN = Right(FEN, Len(FEN) - SpaceLocation - 3)
                    'Creates castling privileges (as long as the pieces are in the correct space).
                    For m = 0 To Len(FEN) - 1
                        If FEN(m) = "K" AndAlso (tempArray(4, 7) = "K" AndAlso tempArray(7, 7) = "R") Then
                            WCanCastle.KS = True
                        ElseIf FEN(m) = "Q" AndAlso (tempArray(4, 7) = "K" AndAlso tempArray(0, 7) = "R") Then
                            WCanCastle.QS = True
                        ElseIf FEN(m) = "k" AndAlso (tempArray(4, 0) = "k" AndAlso tempArray(7, 0) = "r") Then
                            BCanCastle.KS = True
                        ElseIf FEN(m) = "q" AndAlso (tempArray(4, 0) = "k" AndAlso tempArray(0, 0) = "r") Then
                            BCanCastle.QS = True
                        ElseIf FEN(m) >= "a" AndAlso FEN(m) <= "h" Then
                            'converts the a-h coordinate To the more computer-friendly index from 0-7 (eg: h3 --> 75)
                            EnPassant = PGNtoCoorConverter(FEN.Substring(m, 2))
                            If Not CheckEnPassantSquareIsLegal(tempArray, EnPassant, IsWhite) Then Throw New Exception("Invalid En-Passant Square.")
                        End If
                    Next
                    Exit For
            End Select
        Next
        Return tempArray
    End Function

    'Overloads of the above subroutine, but for Bitvalues for KPos, and EnPassant.
    Public Function FENConverter(ByVal FEN As String, ByRef WCanCastle As CanCastle, ByRef BCanCastle As CanCastle, ByRef WKPos As UInt16, ByRef BKPos As UInt16, ByRef EnPassant As UInt16, ByRef IsWhite As Boolean) As Char(,)
        Dim TempWKPos As String = ""
        Dim TempBKPos As String = ""
        Dim TempEnPassant As String = ""
        Dim TempBoard(,) As Char
        TempBoard = FENConverter(FEN, WCanCastle, BCanCastle, TempWKPos, TempBKPos, TempEnPassant, IsWhite)
        WKPos = ConvertStringToBitCoor(TempWKPos)
        BKPos = ConvertStringToBitCoor(TempBKPos)
        EnPassant = ConvertStringToBitCoor(TempEnPassant)
        Return TempBoard
    End Function

    'Function which converts the current board position into its FEN counterpart.
    Public Function ConvertToFEN(ByVal Board(,) As Char, ByVal WCanCastle As CanCastle, ByVal BCanCastle As CanCastle, ByVal EnPassant As UInt16, ByVal isWhite As Boolean) As String
        Dim Counter As Integer = 0 '= the number of blank spaces in a row on the board.
        ConvertToFEN = ""
        For y As Byte = 0 To 7
            For x As Byte = 0 To 7
                If Board(x, y) = " "c Then
                    Counter += 1
                Else 'If necessary, add Counter to the FEN & add the piece name to the FEN.
                    If Counter > 0 Then
                        ConvertToFEN &= Counter
                        Counter = 0
                    End If
                    ConvertToFEN &= Board(x, y)
                End If
            Next
            If Counter > 0 Then ConvertToFEN &= Counter
            ConvertToFEN &= "/" 'Creates new row break, and begins on the next one.
            Counter = 0
        Next
        ConvertToFEN = ConvertToFEN.TrimEnd("/"c) 'Removes the last character from the FEN ("/").
        If isWhite Then
            ConvertToFEN &= " w "
        Else
            ConvertToFEN &= " b "
        End If
        If WCanCastle.KS Then ConvertToFEN &= "K"
        If WCanCastle.QS Then ConvertToFEN &= "Q"
        If BCanCastle.KS Then ConvertToFEN &= "k"
        If BCanCastle.QS Then ConvertToFEN &= "q"
        If ConvertToFEN.EndsWith(" "c) Then ConvertToFEN &= "-" '= therefore no castling privileges
        If EnPassant <> 0 Then
            'converts the computer-friendly index from 0-7 into the more human-friendly a-h coordinate (eg: 75 -> h3)
            ConvertToFEN &= " " & SquareToPGNConverter(EnPassant) & " 0 1"
        Else
            ConvertToFEN &= " - 0 1" 'represents the move numbers for a standard position.
        End If
        Return ConvertToFEN
    End Function

    'Function which removes all the Move Counts (ie: the full-move and half-move counts) from a FEN.
    'Eg: 5B2/NR6/1np5/p7/p1kp4/K4Q2/3P4/8 w - - 2 16  -->  5B2/NR6/1np5/p7/p1kp4/K4Q2/3P4/8 w - -
    Public Function StripFENOfMoveCounts(ByVal FEN As String) As String
        Dim FENFullCutOff As String = FEN.Substring(0, FEN.LastIndexOf(" "c))
        Return FENFullCutOff.Substring(0, FENFullCutOff.LastIndexOf(" "c))
    End Function


    'Function which chcecks if the EnPassant square (given by a FEN) is valid on the board.
    Public Function CheckEnPassantSquareIsLegal(ByVal Board(,) As Char, ByVal EnPassant As String, ByVal isWhite As Boolean) As Boolean
        If EnPassant = "-" OrElse EnPassant = Nothing Then Return True
        Dim XCoor As Integer = CCharInt(EnPassant(0))
        Dim YCoor As Integer = CCharInt(EnPassant(1))
        'Checks that the row is correct, and that there is an enemy pawn behind the square.
        If isWhite AndAlso YCoor = 2 AndAlso Board(XCoor, YCoor + 1) = "p" Then
            'Checks that there is a friendly pawn next to that enemy pawn.
            Return (Board(Math.Min(XCoor + 1, 7), YCoor + 1) = "P" OrElse Board(Math.Max(XCoor - 1, 0), YCoor + 1) = "P")
        ElseIf Not isWhite AndAlso YCoor = 5 AndAlso Board(XCoor, YCoor - 1) = "P" Then
            'Similar code for black en-passant.
            Return (Board(Math.Min(XCoor + 1, 7), YCoor - 1) = "p" OrElse Board(Math.Max(XCoor - 1, 0), YCoor - 1) = "p")
        End If
        Return False
    End Function



    'Subroutine which outputs a given board to the console.
    Public Sub OutputBoardToConsole(ByRef Board(,) As Char)
        For y As Byte = 0 To 7
            For x As Byte = 0 To 7
                If Board(x, y) = " " Then
                    If (x + y) Mod 2 = 0 Then
                        'Square is a light-coloured square.
                        Console.ForegroundColor = ConsoleColor.White
                    Else 'Square is a dark-coloured square.
                        Console.ForegroundColor = ConsoleColor.Gray
                    End If
                    Console.Write(ChrW(&H25AB)) 'Empty cell symbol.
                Else
                    'Colours piece depending if it is white's or black's piece.
                    If Char.IsUpper(Board(x, y)) Then
                        Console.ForegroundColor = ConsoleColor.White
                    Else
                        Console.ForegroundColor = ConsoleColor.DarkGray
                    End If
                    Console.Write(Board(x, y))
                End If
            Next
            Console.WriteLine()
        Next
        Console.ForegroundColor = ConsoleColor.White
        Console.WriteLine()
    End Sub
    Public Sub OutputTFTableToConsole(ByVal TFTable As UInt64, Optional ByVal PinInfo As UInt64 = 0UL, Optional ByVal InCheck As UInt16 = 0US, Optional ByVal MeKPos As UInt16 = UInt16.MaxValue)
        Console.ForegroundColor = ConsoleColor.DarkCyan
        Console.WriteLine("Denary: " & TFTable)
        Dim BinaryMask As String = StrReverse(String.Join("", BitConverter.GetBytes(CULng(TFTable)).Reverse().Select(Function(b) Convert.ToString(b, 2).PadLeft(8, "0"c))))
        Dim Counter As Integer
        For i = 0 To 63
            If i = MeKPos Then
                Console.ForegroundColor = ConsoleColor.DarkYellow
            ElseIf InCheck <> 0US AndAlso i = (InCheck And 63US) Then
                Console.ForegroundColor = ConsoleColor.White
            ElseIf (PinInfo And (1UL << i)) <> 0UL Then
                Console.ForegroundColor = ConsoleColor.Blue
            Else
                Console.ForegroundColor = If(BinaryMask(i) = "1"c, ConsoleColor.Green, ConsoleColor.Red)
            End If
            Console.Write(If(BinaryMask(i) = "1"c, "T"c, "F"c))
            Counter += 1
            If Counter = 8 Then Counter = 0 : Console.WriteLine()
        Next
        Console.ForegroundColor = ConsoleColor.White
        Console.WriteLine()
    End Sub


    'Subroutine which outputs a given BitMove (as used by my AI) to the console.
    Protected Sub OutputBitMoveToConsole(ByVal Move As UInt16, Optional ByVal PrecedingText As String = "")
        'Converts the BitMove to binary (base 2).
        Dim BinaryString As String = (Convert.ToString(Move, 2)).PadLeft(16, "0"c)
        Dim OldMove, NewMove As Integer
        If PrecedingText <> "" Then Console.Write(PrecedingText)
        For n As Byte = 0 To 15
            Select Case n
                Case 0
                    'Capture Flag
                    Console.ForegroundColor = ConsoleColor.White
                Case 1 To 3
                    'Other Flags
                    Console.ForegroundColor = ConsoleColor.Magenta
                Case 4 To 9
                    'OldPos
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                Case 10 To 15
                    'NewPos
                    Console.ForegroundColor = ConsoleColor.Blue
            End Select
            Console.Write(BinaryString(n))
        Next
        'Outputs the denary equivilant of the move.
        Console.ForegroundColor = ConsoleColor.Gray
        Console.Write((" (" & Move.ToString("N0") & ")").PadRight(10) & "=  ")

        'Converts the BitMove into the more user-friendly coor system, then outputs each digit in their respective colour.
        OldMove = (Move And 4032) >> 6
        NewMove = Move And 63
        Console.ForegroundColor = ConsoleColor.DarkYellow
        Console.Write($"{OldMove:D2} ")
        Console.ForegroundColor = ConsoleColor.Blue
        Console.Write($"{NewMove:D2} ")
        Console.ForegroundColor = ConsoleColor.Gray

        'Outputs the PGN equivilent of the move (in the format a1b2, where a1 = start position, and b2 = end position).
        Console.WriteLine("(" & SquareToPGNConverter(OldMove) & SquareToPGNConverter(NewMove) & ").")
    End Sub

    Protected Sub OutputBitMaskToConsole(ByVal Mask As ULong, Optional ByVal PawnPosition As Integer = -1, Optional ByVal EnemyPawnMask As ULong = 0UL)
        Console.ForegroundColor = ConsoleColor.DarkCyan
        Console.WriteLine("Denary: " & Mask)
        Dim BinaryMask As String = String.Join("", BitConverter.GetBytes(CULng(Mask)).Reverse().Select(Function(b) Convert.ToString(b, 2).PadLeft(8, "0"c)))
        Dim BinaryEnemyMask As String = String.Join("", BitConverter.GetBytes(CULng(EnemyPawnMask)).Reverse().Select(Function(b) Convert.ToString(b, 2).PadLeft(8, "0"c)))
        Dim Counter As Integer
        For i = 63 To 0 Step -1
            If i = PawnPosition Then
                Console.ForegroundColor = ConsoleColor.Cyan
            ElseIf BinaryMask(i) = "1"c Then
                Console.ForegroundColor = If(BinaryMask(i) = BinaryEnemyMask(i), ConsoleColor.DarkYellow, ConsoleColor.Green)
            Else
                Console.ForegroundColor = ConsoleColor.Red
            End If
            Console.Write(BinaryMask(i))
            Counter += 1
            If Counter = 8 Then Counter = 0 : Console.WriteLine()
        Next
        Console.WriteLine()
    End Sub


    'Subroutine which constructs the coordinates of a Move structure, from an AI's BitMove.
    Protected Function ConvertBitMoveToMove(ByVal BitMove As UInt16) As Move
        Dim TempMove As New Move
        ConvertBitMoveToMove(TempMove, BitMove)
        Return TempMove
    End Function
    Protected Sub ConvertBitMoveToMove(ByRef TempMove As Move, ByVal BitMove As UInt16)
        Dim OldSquare As UInt16 = (BitMove And 4032US) >> 6
        Dim NewSquare As UInt16 = BitMove And 63US
        TempMove.OldMoveX = CStr(OldSquare Mod 8US)
        TempMove.OldMoveY = CStr(OldSquare \ 8US)
        TempMove.NewMoveX = CStr(NewSquare Mod 8US)
        TempMove.NewMoveY = CStr(NewSquare \ 8US)
        If (BitMove And 28672) = 4096 Then
            TempMove.Code = "Q"c
        ElseIf (BitMove And 28672) = 28672 Then
            TempMove.Code = "N"c
        Else
            TempMove.Code = "f"c
        End If
        'Saves a copy of the BitMove inside the Move object, in case it needs to be called later (eg: iterative deepening, or altering the board afterwards).
        TempMove.BitMove = BitMove
    End Sub

    'Function which converts a string coordinate (eg: "54") to its BitMove counterpart (eg: "00101100")
    Public Function ConvertStringToBitCoor(ByVal MoveString As String) As UInt16
        If MoveString = "-" OrElse MoveString = Nothing Then Return 0 'For blank En-Passant.
        Return Flatten2DBoardIndex(CUShort(Val(MoveString(0))), CUShort(Val(MoveString(1))))
    End Function


    'Creates and handles bitboards.
    Public Sub ConvertBoardtoBitboards(ByVal Board(,) As Char, ByRef State As BoardState, Optional ByVal StateNeedsCleaning As Boolean = False)
        If StateNeedsCleaning Then State.ClearBitboards()
        For y As UInt16 = 0 To 7
            For x As UInt16 = 0 To 7
                Dim Piece As Char = Board(x, y)
                If Piece <> " " Then
                    Dim Square As UInt16 = Flatten2DBoardIndex(x, y)
                    If Char.IsUpper(Piece) Then
                        Select Case Piece
                            Case "P"c : State.BitboardPawnWhite = State.BitboardPawnWhite Or (1UL << Square)
                            Case "N"c : State.BitboardKnightWhite = State.BitboardKnightWhite Or (1UL << Square)
                            Case "B"c : State.BitboardBishopWhite = State.BitboardBishopWhite Or (1UL << Square)
                            Case "R"c : State.BitboardRookWhite = State.BitboardRookWhite Or (1UL << Square)
                            Case "Q"c : State.BitboardQueenWhite = State.BitboardQueenWhite Or (1UL << Square)
                        End Select
                    Else
                        Select Case Piece
                            Case "p"c : State.BitboardPawnBlack = State.BitboardPawnBlack Or (1UL << Square)
                            Case "n"c : State.BitboardKnightBlack = State.BitboardKnightBlack Or (1UL << Square)
                            Case "b"c : State.BitboardBishopBlack = State.BitboardBishopBlack Or (1UL << Square)
                            Case "r"c : State.BitboardRookBlack = State.BitboardRookBlack Or (1UL << Square)
                            Case "q"c : State.BitboardQueenBlack = State.BitboardQueenBlack Or (1UL << Square)
                        End Select
                    End If
                End If
            Next
        Next
    End Sub
    Public Function GetPieceIndexFromSquare(ByVal Square As UInt16, ByRef State As BoardState, ByVal PieceIsWhite As Boolean) As Integer
        Dim NewPieceMap As UInt64 = 1UL << Square
        If PieceIsWhite Then
            If (NewPieceMap And State.BitboardPawnWhite) <> 0UL Then
                Return GlobalConstants.PieceIndex.Pawn
            ElseIf (NewPieceMap And State.BitboardKnightWhite) <> 0UL Then
                Return GlobalConstants.PieceIndex.Knight
            ElseIf (NewPieceMap And State.BitboardBishopWhite) <> 0UL Then
                Return GlobalConstants.PieceIndex.Bishop
            ElseIf (NewPieceMap And State.BitboardRookWhite) <> 0UL Then
                Return GlobalConstants.PieceIndex.Rook
            ElseIf (NewPieceMap And State.BitboardQueenWhite) <> 0UL Then
                Return GlobalConstants.PieceIndex.Queen
            Else 'The piece must be the king.
                Return GlobalConstants.PieceIndex.King
            End If
        Else
            If (NewPieceMap And State.BitboardPawnBlack) <> 0UL Then
                Return GlobalConstants.PieceIndex.Pawn
            ElseIf (NewPieceMap And State.BitboardKnightBlack) <> 0UL Then
                Return GlobalConstants.PieceIndex.Knight
            ElseIf (NewPieceMap And State.BitboardBishopBlack) <> 0UL Then
                Return GlobalConstants.PieceIndex.Bishop
            ElseIf (NewPieceMap And State.BitboardRookBlack) <> 0UL Then
                Return GlobalConstants.PieceIndex.Rook
            ElseIf (NewPieceMap And State.BitboardQueenBlack) <> 0UL Then
                Return GlobalConstants.PieceIndex.Queen
            Else 'The piece must be the king.
                Return GlobalConstants.PieceIndex.King
            End If
        End If
    End Function
    Public Function ConvertBitboardstoBoard(ByRef State As BoardState) As Char(,)
        Dim Board(7, 7) As Char
        Dim BoardMap As (Bitboard As UInt64, Symbol As Char)() = {
            (State.BitboardPawnWhite, "P"c),
            (State.BitboardKnightWhite, "N"c),
            (State.BitboardBishopWhite, "B"c),
            (State.BitboardRookWhite, "R"c),
            (State.BitboardQueenWhite, "Q"c),
            (State.BitboardPawnBlack, "p"c),
            (State.BitboardKnightBlack, "n"c),
            (State.BitboardBishopBlack, "b"c),
            (State.BitboardRookBlack, "r"c),
            (State.BitboardQueenBlack, "q"c)}
        For Each Map In BoardMap
            While Map.Bitboard > 0UL
                Dim BoardCoords = Unwrap1DBoardIndex(CUShort(BitOperations.TrailingZeroCount(Map.Bitboard)))
                If Board(BoardCoords.x, BoardCoords.y) = " " Then
                    Console.ForegroundColor = ConsoleColor.DarkRed
                    Console.WriteLine("Experienced a Collision Error When Converting Bitboards into Board.")
                Else
                    Board(BoardCoords.x, BoardCoords.y) = Map.Symbol
                End If
                Map.Bitboard = Map.Bitboard And (Map.Bitboard - 1UL)
            End While
        Next
        Return Board
    End Function
    <MethodImpl(MethodImplOptions.AggressiveInlining)>
    Public Function Flatten2DBoardIndex(ByVal x As UInt16, y As UInt16) As UInt16
        Return 8US * y + x
    End Function
    <MethodImpl(MethodImplOptions.AggressiveInlining)>
    Public Function Flatten2DBoardIndex(ByVal x As Int16, y As Int16) As Int16
        Return 8S * y + x
    End Function
    <MethodImpl(MethodImplOptions.AggressiveInlining)>
    Public Function Unwrap1DBoardIndex(ByVal Square As UInt16) As (x As UInt16, y As UInt16)
        Return (Square Mod 8US, Square \ 8US)
    End Function


    'Method which creates the TrueFalse Table of the selected player (controlled by the Variable FixWhite).
    'This is done by generating all the legal moves of the pieces that could influence the enemy king's motion.
    'This creates a 'field' around the king (stating where its legal moves are), along with creating pinned pieces
    'and checks.
    'This method returns true if the player to move contains at least 1 piece (ie: anything other than pawns). This will be useful for detecting Zugzwang in Null Move Pruning.
    Public Function CalibrateForMoveGeneration(ByRef Board As BoardState, ByVal MeKPos As UInt16, ByVal EnemyKPos As UInt16, ByVal isWhite As Boolean, Optional ByRef PieceInPos As Boolean = False) As NegaMaxSearchTools
        Dim TFTable, PinInfoStraight, PinInfoDiag, OccupancyMask, EnemyPieceMask As UInt64
        Dim CheckInfo As UInt16
        'We construct the full bitboards of all pieces, minus the kings (allows rooks to 'see' through them so the king cannot move backwards when in check).
        Dim FriendlyPieceMask As UInt64
        Dim MeKingMask As UInt64 = 1UL << MeKPos

        'Calibrates TFTable by checking all pieces which could influence the king. For all heavy pieces, we AND the bitboard with a pre-computed
        '"danger" map of where these pieces need to be to influence the king - this allows for fewer computation of a piece's legal moves.
        'TODO: Try relax initially first and see what kinda difference that makes.
        Dim TempMask As UInt64
        If isWhite Then
            FriendlyPieceMask = Board.BitboardKnightWhite Or Board.BitboardBishopWhite Or Board.BitboardRookWhite Or Board.BitboardQueenWhite
            If FriendlyPieceMask <> 0UL Then PieceInPos = True 'Our position contains at least one minor / major piece, and so we are _probably_ not in Zugzwang.
            FriendlyPieceMask = FriendlyPieceMask Or Board.BitboardPawnWhite

            EnemyPieceMask = Board.BitboardPawnBlack Or Board.BitboardKnightBlack Or Board.BitboardBishopBlack Or Board.BitboardRookBlack Or Board.BitboardQueenBlack Or (1UL << EnemyKPos)
            OccupancyMask = FriendlyPieceMask Or EnemyPieceMask

            'Shifts all the enemy pawns at once to generate the full attack map instantly. If we intersect the king, place a friendly pawn
            'at the king's location and intersect to find the (single) attacking pawn.
            TempMask = ((Board.BitboardPawnBlack And &HFEFEFEFEFEFEFEFEUL) << 7) Or ((Board.BitboardPawnBlack And &H7F7F7F7F7F7F7F7FUL) << 9)
            TFTable = TFTable Or TempMask
            'Check for checks! (time for thyme?) Double checks must incorporate at least one sliding piece - can't have happened yet.
            If (TempMask And MeKingMask) <> 0UL Then
                TempMask = Board.BitboardPawnBlack And PawnWhiteAttackMap(MeKPos)
                CheckInfo = 128US Or CUShort(BitOperations.TrailingZeroCount(TempMask))
            End If

            TempMask = Board.BitboardKnightBlack And KingDangerMapKnight(MeKPos)
            While TempMask <> 0UL
                Dim Square As Integer = BitOperations.TrailingZeroCount(TempMask)
                TFTable = TFTable Or KnightMoveMap(Square)
                If (KnightMoveMap(Square) And MeKingMask) <> 0UL Then CheckInfo = 128US Or CUShort(Square)
                TempMask = TempMask And (TempMask - 1UL)
            End While

            'Enemy king influence.
            TFTable = TFTable Or KingMoveMap(EnemyKPos)

            'Sliding piece influence: rooks & bishops (counting queen twice, once for each movement type).
            TempMask = (Board.BitboardBishopBlack Or Board.BitboardQueenBlack) And KingDangerMapBishop(MeKPos)
            While TempMask <> 0UL
                TFTable = TFTable Or BishopMagicLookup(CUShort(BitOperations.TrailingZeroCount(TempMask)), OccupancyMask)
                TempMask = TempMask And (TempMask - 1UL)
            End While
            TempMask = (Board.BitboardRookBlack Or Board.BitboardQueenBlack) And KingDangerMapRook(MeKPos)
            While TempMask <> 0UL
                TFTable = TFTable Or RookMagicLookup(CUShort(BitOperations.TrailingZeroCount(TempMask)), OccupancyMask)
                TempMask = TempMask And (TempMask - 1UL)
            End While

        Else 'Identical code but for the white pieces (fixing the Black TFTable).
            FriendlyPieceMask = Board.BitboardKnightBlack Or Board.BitboardBishopBlack Or Board.BitboardRookBlack Or Board.BitboardQueenBlack
            If FriendlyPieceMask <> 0UL Then PieceInPos = True
            FriendlyPieceMask = FriendlyPieceMask Or Board.BitboardPawnBlack

            EnemyPieceMask = Board.BitboardPawnWhite Or Board.BitboardKnightWhite Or Board.BitboardBishopWhite Or Board.BitboardRookWhite Or Board.BitboardQueenWhite Or (1UL << EnemyKPos)
            OccupancyMask = FriendlyPieceMask Or EnemyPieceMask

            TempMask = ((Board.BitboardPawnWhite And &HFEFEFEFEFEFEFEFEUL) >> 9) Or ((Board.BitboardPawnWhite And &H7F7F7F7F7F7F7F7FUL) >> 7)
            TFTable = TFTable Or TempMask
            If (TempMask And MeKingMask) <> 0UL Then
                TempMask = Board.BitboardPawnWhite And PawnBlackAttackMap(MeKPos)
                CheckInfo = 128US Or CUShort(BitOperations.TrailingZeroCount(TempMask))
            End If

            TempMask = Board.BitboardKnightWhite And KingDangerMapKnight(MeKPos)
            While TempMask <> 0UL
                Dim Square As Integer = BitOperations.TrailingZeroCount(TempMask)
                TFTable = TFTable Or KnightMoveMap(Square)
                If (KnightMoveMap(Square) And MeKingMask) <> 0UL Then CheckInfo = 128US Or CUShort(Square)
                TempMask = TempMask And (TempMask - 1UL)
            End While

            TFTable = TFTable Or KingMoveMap(EnemyKPos)

            TempMask = (Board.BitboardBishopWhite Or Board.BitboardQueenWhite) And KingDangerMapBishop(MeKPos)
            While TempMask <> 0UL
                TFTable = TFTable Or BishopMagicLookup(CUShort(BitOperations.TrailingZeroCount(TempMask)), OccupancyMask)
                TempMask = TempMask And (TempMask - 1UL)
            End While
            TempMask = (Board.BitboardRookWhite Or Board.BitboardQueenWhite) And KingDangerMapRook(MeKPos)
            While TempMask <> 0UL
                TFTable = TFTable Or RookMagicLookup(CUShort(BitOperations.TrailingZeroCount(TempMask)), OccupancyMask)
                TempMask = TempMask And (TempMask - 1UL)
            End While
        End If
        OccupancyMask = OccupancyMask Or MeKingMask
        TFTable = Not TFTable


        'Calibrates check and pin data by treating the king as a queen, casting rays to detect enemy pieces, and removing immediate blockers to detect pins.
        'Double checks must incorporate at least one sliding piece (and, if two sliding pieces are used, one of each type) - impossible to have achieved one at this point.
        Dim PossiblePinners As UInt64 = BishopMoveMap(MeKPos) And If(isWhite, Board.BitboardBishopBlack Or Board.BitboardQueenBlack, Board.BitboardBishopWhite Or Board.BitboardQueenWhite)
        While PossiblePinners <> 0UL
            'Uses the RayMap to find all the piece between the pinning candidate and the king. If there is just one friendly piece, itsapin.
            Dim PinnerSquare As Integer = BitOperations.TrailingZeroCount(PossiblePinners)
            Dim CandidatePins As UInt64 = OccupancyMask And RayMap(64 * MeKPos + PinnerSquare)
            If CandidatePins = 0UL Then
                'No pieces in te way - it's a check! Add data (or double check flag, depending on if we've already flagged this state as a check).
                CheckInfo = If(CheckInfo = 0US, 128US Or CUShort(PinnerSquare), CheckInfo Or 64US)
            ElseIf (CandidatePins And (CandidatePins - 1UL)) = 0UL AndAlso (CandidatePins And FriendlyPieceMask) <> 0UL Then
                'Flags the pin.
                PinInfoDiag = PinInfoDiag Or CandidatePins
            End If
            PossiblePinners = PossiblePinners And (PossiblePinners - 1UL)
        End While

        'Same code for straight pins and checks.
        PossiblePinners = RookMoveMap(MeKPos) And If(isWhite, Board.BitboardRookBlack Or Board.BitboardQueenBlack, Board.BitboardRookWhite Or Board.BitboardQueenWhite)
        While PossiblePinners <> 0UL
            Dim PinnerSquare As Integer = BitOperations.TrailingZeroCount(PossiblePinners)
            Dim CandidatePins As UInt64 = OccupancyMask And RayMap(64 * MeKPos + PinnerSquare)
            If CandidatePins = 0UL Then
                If (CheckInfo And 64US) = 0US Then CheckInfo = If(CheckInfo = 0US, 128US Or CUShort(PinnerSquare), CheckInfo Or 64US)
            ElseIf (CandidatePins And (CandidatePins - 1UL)) = 0UL AndAlso (CandidatePins And FriendlyPieceMask) <> 0UL Then
                PinInfoStraight = PinInfoStraight Or CandidatePins
            End If
            PossiblePinners = PossiblePinners And (PossiblePinners - 1UL)
        End While

        Return New NegaMaxSearchTools With {
            .TFTable = TFTable,
            .PinInfoStraight = PinInfoStraight,
            .PinInfoDiag = PinInfoDiag,
            .CheckInfo = CheckInfo,
            .OccupancyMask = OccupancyMask,
            .EnemyPieceMask = EnemyPieceMask
        }
    End Function




    'Algorithm that returns the weight / value of a given piece. Links to the array of hashed values PieceValue. Aggressive Inlining
    'replaces all references of the function (which appears many times in my program) with the function itself, to reduce on overhead.
    <MethodImpl(MethodImplOptions.AggressiveInlining)>
    Public Function ReturnPieceValue(ByVal Piece As Char) As Integer
        Return PieceValue(LegacyPieceIndexConverter(Asc(UCase(Piece)) Mod 11))
    End Function


    'Function which counts up all the material (and their values) on the board. Stores this information in two
    'variables - one for white's total material count, and the other for black's total material count.
    Public Function CountMaterial(ByVal Board(,) As Char) As Integer()
        Dim MaterialCount(1) As Integer
        For y = 0 To 7
            For x = 0 To 7
                If Not (Board(x, y) = " "c OrElse UCase(Board(x, y)) = "K"c) Then
                    'Add the value of the piece to either White or Black's total.
                    If Char.IsUpper(Board(x, y)) Then
                        MaterialCount(0) += ReturnPieceValue(Board(x, y))
                    Else
                        MaterialCount(1) += ReturnPieceValue(Board(x, y))
                    End If
                End If
            Next
        Next
        Return MaterialCount
    End Function

    'Function that hashes a chess position (including its details) into a 64-bit number using the 'Zobrist Hash' algorithm.
    Public Function ZobristHashPosition(ByVal Board(,) As Char, ByVal isWhite As Boolean, ByVal WCanCastle As CanCastle, ByVal BCanCastle As CanCastle, ByVal EnPassant As UInt16) As UInt64
        ZobristHashPosition = 0
        For y As Byte = 0 To 7
            For x As Byte = 0 To 7
                'If the square contains a piece, xor the required entry in ZobristHashTable into the key.
                If Board(x, y) <> " " Then
                    If Char.IsUpper(Board(x, y)) Then
                        ZobristHashPosition = ZobristHashPosition Xor GetZobristHashTableValue(LegacyPieceIndexConverter(Asc(Board(x, y)) Mod 11), 0, Flatten2DBoardIndex(x, y))
                    Else
                        ZobristHashPosition = ZobristHashPosition Xor GetZobristHashTableValue(LegacyPieceIndexConverter((Asc(Board(x, y)) + 1) Mod 11), 1, Flatten2DBoardIndex(x, y))
                    End If
                End If
            Next
        Next
        'Adds board meta-data to key, such as castling priviledges & en passant.
        If Not isWhite Then ZobristHashPosition = ZobristHashPosition Xor ZobristHashConstants(8)
        If WCanCastle.KS Then ZobristHashPosition = ZobristHashPosition Xor ZobristHashConstants(9)
        If WCanCastle.QS Then ZobristHashPosition = ZobristHashPosition Xor ZobristHashConstants(10)
        If BCanCastle.KS Then ZobristHashPosition = ZobristHashPosition Xor ZobristHashConstants(11)
        If BCanCastle.QS Then ZobristHashPosition = ZobristHashPosition Xor ZobristHashConstants(12)
        If EnPassant <> 0 Then ZobristHashPosition = ZobristHashPosition Xor ZobristHashConstants((EnPassant And 56S) >> 3)
    End Function


    'Subroutine that converts a Move into standard PGN chess notation (eg: e4, Nf4, Ka2).
    Public Function MoveConverter(ByRef State As BoardState, ByVal TempMove As Move, ByVal isWhite As Boolean, ByVal KPos As UInt16, ByVal OccupancyMask As UInt64, ByVal EnemyPieceMask As UInt64) As String
        Dim OldMoveX As Integer = Integer.Parse(TempMove.OldMoveX)
        Dim OldMoveY As Integer = Integer.Parse(TempMove.OldMoveY)
        Dim NewMoveX As Integer = Integer.Parse(TempMove.NewMoveX)
        Dim NewMoveY As Integer = Integer.Parse(TempMove.NewMoveY)
        Dim OldSquare As UInt16 = CUShort(OldMoveY * 8 + OldMoveX)
        Dim NewSquare As UInt16 = CUShort(NewMoveY * 8 + NewMoveX)
        Dim IsCapture As Boolean = (EnemyPieceMask And (1UL << NewSquare)) <> 0UL
        Dim PGNMove As String = ""
        Dim AmbiguityAttackers As UInt64

        Dim Piece As Integer = GetPieceIndexFromSquare(OldSquare, State, isWhite)
        Select Case Piece
            Case GlobalConstants.PieceIndex.Pawn
                'Pawns operate differently with standard chess notation - when moving a pawn, we give its column (a-h), then add its file
                'that it is moving to. For all other pieces, we state the name of the piece, then its end coordinates. Treat this as a separate case.
                If IsCapture OrElse (State.EnPassant <> 0US AndAlso State.EnPassant = NewSquare) Then
                    'Is a capture move - add an "x" followed by the coordinates of the captured piece.
                    PGNMove = Chr(OldMoveX + 97) & "x" & SquareToPGNConverter(NewSquare)
                Else
                    PGNMove = SquareToPGNConverter(NewSquare)
                End If
                If NewMoveY = 0 OrElse NewMoveY = 7 Then PGNMove &= "=" & TempMove.Code 'Code for pawn promotions.
                Return PGNMove 'Pawn moves are unambiguous

            Case GlobalConstants.PieceIndex.Knight
                PGNMove = "N"
                AmbiguityAttackers = KnightMoveMap(NewSquare) And If(isWhite, State.BitboardKnightWhite, State.BitboardKnightBlack)
            Case GlobalConstants.PieceIndex.Bishop
                PGNMove = "B"
                AmbiguityAttackers = BishopMagicLookup(NewSquare, OccupancyMask) And If(isWhite, State.BitboardBishopWhite, State.BitboardBishopBlack)
            Case GlobalConstants.PieceIndex.Rook
                PGNMove = "R"
                AmbiguityAttackers = RookMagicLookup(NewSquare, OccupancyMask) And If(isWhite, State.BitboardRookWhite, State.BitboardRookBlack)
            Case GlobalConstants.PieceIndex.Queen
                PGNMove = "Q"
                AmbiguityAttackers = (BishopMagicLookup(NewSquare, OccupancyMask) Or RookMagicLookup(NewSquare, OccupancyMask)) And If(isWhite, State.BitboardQueenWhite, State.BitboardQueenBlack)
            Case Else
                'We assume this is a king, but GetPieceIndexFromSquare would return this for an empty square. Verify.
                If OldSquare = KPos Then
                    PGNMove = "K"
                    'Code for detecting castling.
                    If OldMoveX = 4 AndAlso (NewMoveY = 0 OrElse NewMoveY = 7) Then
                        If NewMoveX = 6 Then Return "O-O" 'Notation for KS castling.
                        If NewMoveX = 2 Then Return "O-O-O" 'Notation for QS castling.
                    End If
                Else 'Invalid move.
                    Return "ERROR"
                End If
        End Select

        'Once the move has been generated, see if the move can be interpreted in multiple ways.
        'If it can, we need to add constraint(s) to the move.
        If BitOperations.PopCount(AmbiguityAttackers) > 1 Then
            AmbiguityAttackers = AmbiguityAttackers And Not (1UL << OldSquare) 'Isolates the colliding pieces.
            Dim NeedsFileConstraint, NeedsRankConstraint As Boolean
            While AmbiguityAttackers <> 0UL
                Dim ColSquare As Integer = BitOperations.TrailingZeroCount(AmbiguityAttackers)
                If (ColSquare Mod 8) = OldMoveX Then NeedsRankConstraint = True
                If (ColSquare \ 8) = OldMoveY Then NeedsFileConstraint = True
                AmbiguityAttackers = AmbiguityAttackers And (AmbiguityAttackers - 1UL)
            End While
            If Not NeedsFileConstraint AndAlso Not NeedsRankConstraint Then NeedsFileConstraint = True 'We have no area as to the ambigiousness :O
            Dim StartPGN As String = SquareToPGNConverter(OldSquare)
            If NeedsFileConstraint Then PGNMove &= StartPGN(0)
            If NeedsRankConstraint Then PGNMove &= StartPGN(1)
        End If

        'Is a capture move - add an "x" followed by the coordinates of the captured piece.
        If IsCapture Then PGNMove &= "x"
        PGNMove &= SquareToPGNConverter(NewSquare)
        Return PGNMove
    End Function


    'Function that converts a standard chess move (eg: e4, Nf4, Ka2) into a Move.
    Public Function ConvertToMove(ByVal InputMove As String, ByRef State As BoardState, ByVal isWhite As Boolean, ByVal KPos As UInt16, ByVal OccupancyMask As UInt64) As Move
        'Removes extra data from move (that is not useful to my system, ie: checks & pawn promotion tags).
        Dim FormattedMove As String = InputMove.Replace("+", "").Replace("#", "")
        Dim ResultMove As New Move With {.Code = "o"c} 'Denotes normal move that has no constraints.
        If FormattedMove.Contains("=") Then
            ResultMove.Code = FormattedMove.Last() 'Sets promotion piece.
            FormattedMove = FormattedMove.Substring(0, FormattedMove.Length - 2)
        End If
        'Sets end position to the last 2 characters of the move.
        Dim TempEndPosition As String = PGNtoCoorConverter(FormattedMove.Substring(FormattedMove.Length - 2, 2))
        ResultMove.NewMoveX = TempEndPosition(0)
        ResultMove.NewMoveY = TempEndPosition(1)
        Dim NewSquare As UInt16 = ConvertStringToBitCoor(TempEndPosition)
        Dim Candidates As UInt64

        ' 3. Find the Source Square using Reverse Ray-Casting
        If Char.IsLower(FormattedMove(0)) Then 'is a pawn! No ambiguity
            If FormattedMove.Length = 2 Then 'No Capture - pawn is moving 1 or 2 squares.
                'Searches 1 and 2 squares behind the end square, looking for TempPiece.
                Dim SearchSquare As Integer = If(isWhite, NewSquare + 8, NewSquare - 8)
                If (If(isWhite, State.BitboardPawnWhite, State.BitboardPawnBlack) And (1UL << SearchSquare)) <> 0UL Then
                    ResultMove.OldMoveX = CStr(SearchSquare Mod 8)
                    ResultMove.OldMoveY = CStr(SearchSquare \ 8)
                    Return ResultMove
                Else
                    SearchSquare = If(isWhite, NewSquare + 16, NewSquare - 16)
                    If (If(isWhite, State.BitboardPawnWhite, State.BitboardPawnBlack) And (1UL << SearchSquare)) <> 0UL Then
                        ResultMove.OldMoveX = CStr(SearchSquare Mod 8)
                        ResultMove.OldMoveY = CStr(SearchSquare \ 8)
                        Return ResultMove
                    End If
                End If
            Else 'is a pawn capture move - set starting coordinates accordingly.
                ResultMove.OldMoveX = CStr(Asc(FormattedMove(0)) - 97) 'Converts the rank index to a number.
                ResultMove.OldMoveY = CStr(If(isWhite, Integer.Parse(ResultMove.NewMoveY) + 1, Integer.Parse(ResultMove.NewMoveY) - 1))
                Return ResultMove
            End If
        Else 'Is a piece. Constraint is used to specify which piece should move to the square (if there are multiple to choose from).
            Dim Constraint As String = FormattedMove.Substring(1, FormattedMove.Length - 3).Replace("x", "")
            Select Case FormattedMove(0)
                Case "N"c
                    Candidates = KnightMoveMap(NewSquare) And If(isWhite, State.BitboardKnightWhite, State.BitboardKnightBlack)
                Case "B"c
                    Candidates = BishopMagicLookup(NewSquare, OccupancyMask) And If(isWhite, State.BitboardBishopWhite, State.BitboardBishopBlack)
                Case "R"c
                    Candidates = RookMagicLookup(NewSquare, OccupancyMask) And If(isWhite, State.BitboardRookWhite, State.BitboardRookBlack)
                Case "Q"c
                    Candidates = (BishopMagicLookup(NewSquare, OccupancyMask) Or RookMagicLookup(NewSquare, OccupancyMask)) And If(isWhite, State.BitboardQueenWhite, State.BitboardQueenBlack)
                Case "K"c, "O"c
                    'User is attempting to castle... Note there cannot be ambiguity in this move.
                    ResultMove.Code = "o"c
                    ResultMove.OldMoveX = CStr(KPos Mod 8US)
                    ResultMove.OldMoveY = CStr(KPos \ 8US)
                    If FormattedMove = "O-O" Then
                        ResultMove.NewMoveX = "6"
                        ResultMove.NewMoveY = ResultMove.OldMoveY
                    ElseIf FormattedMove = "O-O-O" Then
                        ResultMove.NewMoveX = "2"
                        ResultMove.NewMoveY = ResultMove.OldMoveY
                    End If
                    Return ResultMove
            End Select

            If BitOperations.PopCount(Candidates) > 1 AndAlso Constraint.Length > 0 Then
                If Constraint.Length = 2 Then
                    'Constraint length of 2 specifies the exact starting coordinates - retrieve these.
                    TempEndPosition = PGNtoCoorConverter(Constraint)
                    ResultMove.OldMoveX = TempEndPosition(0)
                    ResultMove.OldMoveY = TempEndPosition(1)
                Else
                    If Char.IsLetter(Constraint(0)) Then 'File constraint.
                        Candidates = Candidates And (&H101010101010101UL << (Asc(Constraint(0)) - 97))
                    Else 'Row constraint.
                        Candidates = Candidates And (&HFFUL << ((8 - Integer.Parse(Constraint(0))) * 8))
                    End If
                End If
            End If
            If Candidates <> 0UL Then
                Dim OldSquare As Integer = BitOperations.TrailingZeroCount(Candidates)
                ResultMove.OldMoveX = CStr(OldSquare Mod 8)
                ResultMove.OldMoveY = CStr(OldSquare \ 8)
                Return ResultMove
            End If
        End If

        'No move could be found.
        Console.ForegroundColor = ConsoleColor.DarkRed
        Console.WriteLine($"Unable to interpret move {InputMove} given constraints.")
        Console.ForegroundColor = ConsoleColor.White
        ResultMove.Code = "a"c
        Return ResultMove
    End Function

End Class
