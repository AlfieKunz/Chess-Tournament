'This class contains most of the primary algorithms I will be using in my project, and will link to both my Chess class and
'my AI class (either via instavintiation or by inheritance). It will contain the algorithms that will be used by both my Chess
'& AI classes, such as the ‘TFTable’ Generator, ‘DoesMoveResolveCheck’, 'Move Converters', and others.
Imports System.Data.OleDb
Imports System.Runtime.CompilerServices
Imports System.Xml.XPath

Partial Public Class CoreMethods
    'TrueTables are just TrueFalse Tables containing only the letter T. Very useful for resetting TrueFalse Tables
    'and debugging.
    Public MasterTrueTable(7, 7), TrueTable(7, 7) As Char
    Public CannotCastle As New CanCastle
    Private Shared ReadOnly PieceValue(9) As Integer 'Array Containing the Value or Weight of each Piece.
    Protected Shared MVVLVAValues(9, 9) As UInt16 'Array Containing the score associated with each possible capture configuration in chess.
    'This is used for move ordering, and represents the premise of encouraging high captures, and capturing _with_ low material.

    Protected Shared ReadOnly ZobristHashTable(9, 1, 7, 7) As UInt64 '(a, b, c, d), where a = piece type, b = piece colour, c = x-coor, d = y-coor.
    'a is Similar to PieceValue: use (Asc(UCase(PieceName)) Mod 11) to calculate - [2] used for EnPassant square.
    Protected Shared ReadOnly HashConstants(4) As UInt64 '0 = Player Turn, 1 = WhiteKSCastle, 2 = WhiteQSCastle, 3 = BlackKSCastle, 4 = BlackQSCastle.
    Public Sub New()
        PopulateKnightLegalMoveArray()
        'Sets PieceValues variables using a Hash Function (Upper Case letter --> ASCII, then MOD 11). This
        'creates a unique index / row in the PieceValue array for each piece and its corresponding weight,
        'so its value can be searched up quickly. PieceValue(Asc(UCase(Board(x, y))) Mod 11)
        PieceValue(0) = GlobalConstants.PieceWeight.Bishop 'Bishop Weight
        PieceValue(1) = GlobalConstants.PieceWeight.Knight 'Knight Weight
        PieceValue(3) = GlobalConstants.PieceWeight.Pawn 'Pawn Weight
        PieceValue(4) = GlobalConstants.PieceWeight.Queen 'Queen Weight
        PieceValue(5) = GlobalConstants.PieceWeight.Rook 'Rook Weight
        PieceValue(9) = GlobalConstants.PieceWeight.King 'King Weight

        'Loads the appropriate values into MVA-LVA. For more info, see rustic-chess.org/search/ordering/mvv_lva.html
        MVVLVAValues = {
            {33, 34, 0, 35, 31, 32, 0, 0, 0, 30}, 'Victim = Bishop.
            {23, 24, 0, 25, 21, 22, 0, 0, 0, 20}, 'Victim = Knight.
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {13, 14, 0, 15, 11, 12, 0, 0, 0, 10}, 'Victim = Pawn.
            {53, 54, 0, 55, 51, 52, 0, 0, 0, 50}, 'Victim = Queen.
            {43, 44, 0, 45, 41, 42, 0, 0, 0, 40}, 'Victim = Rook.
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
            {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}} 'Victim = King.

        'Creates MasterTrueTable and TrueTable.
        For x As Byte = 0 To 7
            For y As Byte = 0 To 7
                MasterTrueTable(x, y) = "T"c
                TrueTable(x, y) = "T"c
            Next
        Next

        'Fills ZobristHasTable with pseudo-random 64-bit numbers
        Static RND As New Random()
        Dim RNDOne, RNDTwo As UInt64
        For w As Byte = 0 To 9
            Select Case w
                Case 0, 1, 2, 3, 4, 5, 9
                    For x As Byte = 0 To 1
                        For y As Byte = 0 To 7
                            For z As Byte = 0 To 7
                                'Produce two random 32-bit numbers
                                RNDOne = CULng(RND.Next())
                                RNDTwo = CULng(RND.Next())
                                'Combine these numbers together into a 64-bit number by applying a 32-bit left shift to RNDOne,
                                'then combining this with RNDTwo via a bitwise OR operation.
                                ZobristHashTable(w, x, y, z) = (RNDOne << 32) Or RNDTwo
                            Next
                        Next
                    Next
            End Select
        Next
        'Fills HasConstants with random 64-bit numbers.
        For n As Byte = 0 To 4
            RNDOne = CULng(RND.Next())
            RNDTwo = CULng(RND.Next())
            HashConstants(n) = (RNDOne << 32) Or RNDTwo
        Next
    End Sub




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
                    ' Once the SPACE has been reached In the FEN, we know that we have read the entire board. We can then move onto
                    ' info about castling, who's turn it is, En Passant And more.
                    SpaceLocation = FEN.IndexOf(" ") 'Following characters revolve around location of SPACE.
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
    Public Function FENConverter(ByVal FEN As String, ByRef WCanCastle As CanCastle, ByRef BCanCastle As CanCastle, ByRef WKPos As Int16, ByRef BKPos As Int16, ByRef EnPassant As Int16, ByRef IsWhite As Boolean) As Char(,)
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
    Public Function ConvertToFEN(ByVal Board(,) As Char, ByVal WCanCastle As CanCastle, ByVal BCanCastle As CanCastle, ByVal EnPassant As Int16, ByVal isWhite As Boolean) As String
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
        If ConvertToFEN.EndsWith(" ") Then ConvertToFEN &= "-" '= therefore no castling privileges
        If EnPassant <> 0 Then
            'converts the computer-friendly index from 0-7 into the more human-friendly a-h coordinate (eg: 75 -> h3)
            ConvertToFEN &= " " & Chr(((EnPassant And 56) >> 3) + 97) & 8 - (EnPassant And 7) & " 0 1"
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

    'Subroutine which outputs a given BitMove (as used by my AI) to the console.
    Protected Sub OutputBitMoveToConsole(ByVal Move As UInt16, Optional ByVal PrecedingText As String = "")
        'Converts the BitMove to binary (base 2).
        Dim BinaryString As String = (Convert.ToString(Move, 2)).PadLeft(16, "0"c)
        Dim OldMove, NewMove As String
        If PrecedingText <> "" Then Console.Write(PrecedingText)
        For n As Byte = 0 To 15
            Select Case n
                Case 0
                    'Capture Flag
                    Console.ForegroundColor = ConsoleColor.White
                Case 1 To 3
                    'Other Flags
                    Console.ForegroundColor = ConsoleColor.Magenta
                Case 4 To 6
                    'OldPosX
                    Console.ForegroundColor = ConsoleColor.Red
                Case 7 To 9
                    'OldPosY
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                Case 10 To 12
                    'NewPosX
                    Console.ForegroundColor = ConsoleColor.Green
                Case 13 To 15
                    'NewPosY
                    Console.ForegroundColor = ConsoleColor.Blue
            End Select
            Console.Write(BinaryString(n))
        Next
        'Outputs the denary equivilant of the move.
        Console.ForegroundColor = ConsoleColor.Gray
        Console.Write((" (" & Move.ToString("N0") & ")").PadRight(10) & "=  ")

        'Converts the BitMove into the more user-friendly coor system, then outputs each digit in their respective colour.
        OldMove = ((Move And 3584) >> 9) & ((Move And 448) >> 6)
        NewMove = ((Move And 56) >> 3) & (Move And 7)
        Console.ForegroundColor = ConsoleColor.Red
        Console.Write(OldMove(0))
        Console.ForegroundColor = ConsoleColor.DarkYellow
        Console.Write(OldMove(1) & " ")
        Console.ForegroundColor = ConsoleColor.Green
        Console.Write(NewMove(0))
        Console.ForegroundColor = ConsoleColor.Blue
        Console.Write(NewMove(1))
        Console.ForegroundColor = ConsoleColor.Gray

        'Outputs the PGN equivilent of the move (in the format a1b2, where a1 = start position, and b2 = end position).
        Console.WriteLine(" (" & CoorToPGNConverter(OldMove) & CoorToPGNConverter(NewMove) & ").")
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
        TempMove.OldMoveX = CStr((BitMove And 3584US) >> 9)
        TempMove.OldMoveY = CStr((BitMove And 448US) >> 6)
        TempMove.NewMoveX = CStr((BitMove And 56US) >> 3)
        TempMove.NewMoveY = CStr(BitMove And 7)
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
    Public Function ConvertStringToBitCoor(ByVal MoveString As String) As Int16
        If MoveString = "-" OrElse MoveString = Nothing Then Return 0 'For blank En-Passant.
        Return CShort((CCharInt(MoveString(0)) << 3) Or Val(MoveString(1)))
    End Function



    'Method which creates the TrueFalse Table of the selected player (controlled by the Variable FixWhite).
    'This is done by generating all the legal moves of the pieces that could influence the enemy king's motion.
    'This creates a 'field' around the king (stating where its legal moves are), along with creating pinned pieces
    'and checks.
    'This method returns true if the player to move contains at least 1 piece (ie: anything other than pawns). This will be useful for detecting Zugzwang in Null Move Pruning.
    Public Sub FixTFTable(ByRef Board(,) As Char, ByVal FixWhite As Boolean, ByRef TFTableToFix(,) As Char, ByRef KPos As Int16, ByRef InCheck As UInt16, ByVal CanICastle As Boolean, ByVal EnPassant As Int16, Optional ByRef CheckForPiece As Boolean = False)
        Dim dx, dy As Int16
        Dim PieceInfluenceKing As Boolean
        'Resets TFTables.
        Array.Copy(MasterTrueTable, TFTableToFix, 64)
        If FixWhite Then
            For y = 0S To 7S
                For x = 0S To 7S
                    If Char.IsLower(Board(x, y)) Then
                        'Calculates distances between piece and the enemy king.
                        dx = Math.Abs(((KPos And 56S) >> 3) - x)
                        dy = Math.Abs((KPos And 7S) - y)
                        Select Case Board(x, y)
                            Case "p"c
                                'If the king hasn't castled, a pawn on a2 needs to prevent a castling attempt.
                                PieceInfluenceKing = (KPos And 7) >= y AndAlso (Math.Max(dx, dy) <= 2 OrElse (CanICastle AndAlso y = 6))
                            Case "b"c
                                PieceInfluenceKing = Math.Abs(dx - dy) <= 2
                            Case "n"c
                                PieceInfluenceKing = Math.Max(dx, dy) <= 3 AndAlso dx + dy <= 5
                            Case "r"c
                                'If the king can castle, the rook might be able to cut off the king's motion - give the rook one more move of sight.
                                PieceInfluenceKing = Math.Min(dx, dy) <= If(CanICastle, 2, 1)
                            Case "q"c
                                PieceInfluenceKing = Math.Min(dx, dy) <= 1 OrElse Math.Abs(dx - dy) <= 2
                            Case Else
                                PieceInfluenceKing = Math.Max(dx, dy) <= 2 'is a king.
                        End Select
                        If PieceInfluenceKing Then BlackPieceLegalMoves(Board, CUShort(x), CUShort(y), TFTableToFix, CUShort(KPos), InCheck, CUShort(EnPassant))
                    ElseIf CheckForPiece AndAlso Board(x, y) <> " " Then
                        'Our position contains at least one minor / major piece, and so we are _probably_ not in Zugzwang.
                        If Board(x, y) = "B" OrElse Board(x, y) = "N" OrElse Board(x, y) = "R" OrElse Board(x, y) = "Q" Then CheckForPiece = False
                    End If
                Next
            Next
        Else 'Identical code but for the white pieces (fixing the Black TFTable).
            For y = 0S To 7S
                For x = 0S To 7S
                    If Char.IsUpper(Board(x, y)) Then
                        dx = Math.Abs(((KPos And 56S) >> 3) - x)
                        dy = Math.Abs((KPos And 7S) - y)
                        Select Case Board(x, y)
                            Case "P"c
                                PieceInfluenceKing = (KPos And 7) <= y AndAlso (Math.Max(dx, dy) <= 2 OrElse (CanICastle AndAlso y = 1))
                            Case "B"c
                                PieceInfluenceKing = Math.Abs(dx - dy) <= 2
                            Case "N"c
                                PieceInfluenceKing = Math.Max(dx, dy) <= 3 AndAlso dx + dy <= 5
                            Case "R"c
                                PieceInfluenceKing = Math.Min(dx, dy) <= If(CanICastle, 2, 1)
                            Case "Q"c
                                PieceInfluenceKing = Math.Min(dx, dy) <= 1 OrElse Math.Abs(dx - dy) <= 2
                            Case Else
                                PieceInfluenceKing = Math.Max(dx, dy) <= 2 'is a king.
                        End Select
                        If PieceInfluenceKing Then WhitePieceLegalMoves(Board, CUShort(x), CUShort(y), TFTableToFix, CUShort(KPos), InCheck, CUShort(EnPassant))
                    ElseIf CheckForPiece AndAlso Board(x, y) <> " " Then
                        If Board(x, y) = "b" OrElse Board(x, y) = "n" OrElse Board(x, y) = "r" OrElse Board(x, y) = "q" Then CheckForPiece = False
                    End If
                Next
            Next
        End If
    End Sub




    'Algorithm that returns the weight / value of a given piece. Links to the array of hashed values PieceValue. Aggressive Inlining
    'replaces all references of the function (which appears many times in my program) with the function itself, to reduce on overhead.
    <MethodImpl(MethodImplOptions.AggressiveInlining)>
    Public Function ReturnPieceValue(ByVal Piece As Char) As Integer
        Return PieceValue(Asc(UCase(Piece)) Mod 11)
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
    Public Function ZobristHashPosition(ByVal Board(,) As Char, ByVal isWhite As Boolean, ByVal WCanCastle As CanCastle, ByVal BCanCastle As CanCastle, ByVal EnPassant As Int16) As UInt64
        ZobristHashPosition = 0
        For y As Byte = 0 To 7
            For x As Byte = 0 To 7
                'If the square contains a piece, xor the required entry in ZobristHashTable into the key.
                If Board(x, y) <> " " Then
                    If Char.IsUpper(Board(x, y)) Then
                        ZobristHashPosition = ZobristHashPosition Xor ZobristHashTable(Asc(Board(x, y)) Mod 11, 0, x, y)
                    Else
                        ZobristHashPosition = ZobristHashPosition Xor ZobristHashTable((Asc(Board(x, y)) + 1) Mod 11, 1, x, y)
                    End If
                End If
            Next
        Next
        'Adds board meta-data to key, such as castling priviledges & en passant.
        If Not isWhite Then ZobristHashPosition = ZobristHashPosition Xor HashConstants(0)
        If WCanCastle.KS Then ZobristHashPosition = ZobristHashPosition Xor HashConstants(1)
        If WCanCastle.QS Then ZobristHashPosition = ZobristHashPosition Xor HashConstants(2)
        If BCanCastle.KS Then ZobristHashPosition = ZobristHashPosition Xor HashConstants(3)
        If BCanCastle.QS Then ZobristHashPosition = ZobristHashPosition Xor HashConstants(4)
        If EnPassant <> 0 Then ZobristHashPosition = ZobristHashPosition Xor ZobristHashTable(2, 0, (EnPassant And 56) >> 3, EnPassant And 7)
    End Function


    'Subroutine that converts a Move into standard PGN chess notation (eg: e4, Nf4, Ka2).
    Public Function MoveConverter(ByVal Board(,) As Char, ByVal TempMove As Move, ByVal EnPassant As Int16) As String
        'Overload function for no constraints being added to the move.
        Return MoveConverter(Board, TempMove, True, 255, EnPassant, Nothing)
    End Function
    Public Function MoveConverter(ByVal Board(,) As Char, ByVal TempMove As Move, ByVal isWhite As Boolean, ByVal KPos As Int16, ByVal EnPassant As Int16, ByVal TFTable(,) As Char) As String
        Dim MovedPiece As Char = UCase(Board(Integer.Parse(TempMove.OldMoveX), Integer.Parse(TempMove.OldMoveY)))
        'Pawns operate differently with standard chess notation - when moving a pawn, we give its column (a-h),
        'then add its file that it is moving to. For all other pieces, we state the name of the piece, then its
        'end coordinates.
        If MovedPiece <> " " Then
            If MovedPiece = "P" Then
                MoveConverter = Chr(Integer.Parse(TempMove.OldMoveX) + 97)
                'Code for detecting castling.
            ElseIf MovedPiece = "K" AndAlso TempMove.OldMoveX = "4" AndAlso (TempMove.NewMoveY = "0" OrElse TempMove.NewMoveY = "7") Then
                If TempMove.NewMoveX = "6" Then 'Is king-side castling.
                    Return "O-O" 'Notation for KS castling.
                ElseIf TempMove.NewMoveX = "2" Then ''Is queen-side castling.
                    Return "O-O-O" 'Notation for QS castling.
                Else
                    MoveConverter = MovedPiece
                End If
            Else
                MoveConverter = MovedPiece
            End If
            If Board(Integer.Parse(TempMove.NewMoveX), Integer.Parse(TempMove.NewMoveY)) <> " " OrElse (UCase(MovedPiece) = "P" AndAlso EnPassant = ConvertStringToBitCoor(TempMove.NewMoveX & TempMove.NewMoveY)) Then
                'Is a capture move - add an "x" followed by the coordinates of the captured piece.
                MoveConverter &= "x" & CoorToPGNConverter(TempMove.NewMoveX & TempMove.NewMoveY)
            Else
                If MovedPiece <> "P" Then MoveConverter &= Chr(Integer.Parse(TempMove.NewMoveX) + 97)
                MoveConverter &= 8 - Integer.Parse(TempMove.NewMoveY)
            End If
            'Code for pawn promotions.
            If MovedPiece = "P" AndAlso (TempMove.NewMoveY = "0" OrElse TempMove.NewMoveY = "7") Then MoveConverter &= "=" & TempMove.Code

            If KPos < 255 Then
                'Once the move has been generated, run it through ConvertToMove, to see if the move can be interpreted in multiple ways.
                'If it can, we need to add constraint(s) to the move.
                Dim TestMove As Move
                Dim TestPGN As String = MoveConverter
                TestMove = ConvertToMove(TestPGN, Board, isWhite, KPos, TFTable)
                Do Until TestMove.Code = "o" OrElse TestMove.Code = "Q" OrElse TestMove.Code = "N"
                    If TestMove.Code = "a" Then
                        'Invalid Move - return error state.
                        Return "ERROR"
                    ElseIf TestMove.Code = "c" Then
                        'Adds a constraint to MoveConverter.
                        Select Case TestPGN.Length
                            Case MoveConverter.Length
                                'Add column constraint.
                                TestPGN = TestPGN.Insert(1, Chr(Integer.Parse(TempMove.OldMoveX) + 97))
                            Case MoveConverter.Length + 1
                                If Char.IsLetter(TestPGN(1)) Then
                                    'Add row constraint to replace column constraint.
                                    TestPGN = TestPGN(0) & (8 - Integer.Parse(TempMove.OldMoveY)) & TestPGN.Substring(2)
                                Else
                                    'Add column constraint to row constraint.
                                    TestPGN = TestPGN.Insert(1, Chr(Integer.Parse(TempMove.OldMoveX) + 97))
                                End If
                            Case Else
                                'No more constraints can be added. Return collision error.
                                Return "ERROR"
                        End Select
                        TestMove = ConvertToMove(TestPGN, Board, isWhite, KPos, TFTable)
                    End If
                Loop
                MoveConverter = TestPGN

            End If
        Else
            Return "ERROR"
        End If
    End Function


    'Function that converts a standard chess move (eg: e4, Nf4, Ka2) into a Move.
    Public Function ConvertToMove(ByVal InputMove As String, ByVal Board(,) As Char, ByVal isWhite As Boolean, ByVal KPos As Int16, ByVal TFTable(,) As Char) As Move
        'Removes extra data from move (that is not useful to my system, ie: checks & pawn promotion tags).
        Dim FormattedMove As String = InputMove.TrimEnd(CChar("+"))
        Dim ResultMove As New Move With {.Code = "f"c} 'Denotes normal move.
        ResultMove.Code = "o"c
        If FormattedMove(FormattedMove.Length - 2) = "=" Then
            ResultMove.Code = FormattedMove(FormattedMove.Length - 1) 'Sets promotion piece.
            FormattedMove = FormattedMove.Substring(0, FormattedMove.Length - 2)
        End If
        'Sets end position to the last 2 characters of the move.
        Dim TempEndPosition As String = PGNtoCoorConverter(FormattedMove.Substring(FormattedMove.Length - 2, 2))
        ResultMove.NewMoveX = TempEndPosition(0)
        ResultMove.NewMoveY = TempEndPosition(1)

        Select Case FormattedMove(0)
            Case "B"c, "N"c, "R"c, "Q"c
                'Constraint is used to specify which piece should move to the square (if there are multiple to choose from).
                Dim Constraint As String = FormattedMove.Substring(1, FormattedMove.Length - 3)
                Constraint = Constraint.TrimEnd(CChar("x"))
                If Constraint.Length = 2 Then 'Constraint length of 2 specifies the exact starting coordinates - retrieve these.
                    TempEndPosition = PGNtoCoorConverter(Constraint)
                    ResultMove.OldMoveX = TempEndPosition(0)
                    ResultMove.OldMoveY = TempEndPosition(1)
                Else
                    Dim TempPiece As Char 'Represents the piece we are looking for.
                    If isWhite Then TempPiece = FormattedMove(0) Else TempPiece = LCase(FormattedMove(0))
                    'Creates temporary, empty board that holds TempPiece at the ending coordinates.
                    Dim EmptyBoard(7, 7) As Char
                    For y As Byte = 0 To 7
                        For x As Byte = 0 To 7
                            EmptyBoard(x, y) = " "c
                        Next
                    Next
                    EmptyBoard(Integer.Parse(ResultMove.NewMoveX), Integer.Parse(ResultMove.NewMoveY)) = TempPiece
                    Dim LegalMovesTemp() As UInt16
                    'Calculate the legal moves of TempPiece on EmptyBoard. This produces a set of coordinates that that piece can move to (one of which will be the starting coordinates).
                    If isWhite Then
                        LegalMovesTemp = WhitePieceLegalMoves(EmptyBoard, UInt16.Parse(ResultMove.NewMoveX), UInt16.Parse(ResultMove.NewMoveY), MasterTrueTable, 0, CannotCastle, 0)
                    Else
                        LegalMovesTemp = BlackPieceLegalMoves(EmptyBoard, UInt16.Parse(ResultMove.NewMoveX), UInt16.Parse(ResultMove.NewMoveY), MasterTrueTable, 0, CannotCastle, 0)
                    End If
                    Dim LegalMoves(LegalMovesTemp(0) - 1) As UInt16
                    Array.Copy(LegalMovesTemp, 1, LegalMoves, 0, LegalMovesTemp(0))

                    'Checks if any of these moves contains TempPiece on Board. If so then that piece is the one that is moving, and hence we set that to be the starting coordinates.
                    Dim MatchedMove As Integer = -1
                    For n = 0 To LegalMoves.Length - 1 'for each move...
                        If Board((LegalMoves(n) And 56) >> 3, LegalMoves(n) And 7) = TempPiece Then
                            Dim TestMoves() As UInt16
                            'Matching piece found - check if it agrees with any Constraints.
                            Dim ConstraintMatches As Boolean
                            If Constraint = "" Then
                                ConstraintMatches = True
                            Else
                                'Constraint must be one character long, and hence either specifies the exact
                                'rank that the piece is on, or the exact rile that the piece is on.
                                Select Case Constraint
                                    Case "a" To "h"
                                        'Row constraint - only accept move if its file agrees with the constraint's.
                                        ConstraintMatches = (Asc(Constraint) - 97 = (LegalMoves(n) And 56) >> 3)
                                    Case Else
                                        'Rank constraint - only accept move if its rank agrees with the constraint's.
                                        ConstraintMatches = ((8 - Integer.Parse(Constraint)) = (LegalMoves(n) And 7))
                                End Select
                            End If

                            If ConstraintMatches Then
                                'Tests if move is valid by playing it on the original Board.
                                If isWhite Then
                                    TestMoves = WhitePieceLegalMoves(Board, (LegalMoves(n) And 56US) >> 3, LegalMoves(n) And 7US, TFTable, 0, CannotCastle, 0)
                                Else
                                    TestMoves = BlackPieceLegalMoves(Board, (LegalMoves(n) And 56US) >> 3, LegalMoves(n) And 7US, TFTable, 0, CannotCastle, 0)
                                End If
                                If TestMoves IsNot Nothing Then
                                    For m = 1 To TestMoves(0)
                                        If (TestMoves(m) And 56) >> 3 = Integer.Parse(ResultMove.NewMoveX) AndAlso (TestMoves(m) And 7) = Integer.Parse(ResultMove.NewMoveY) Then
                                            'Move is a match! And is therefore valid.
                                            'Flags that there are multiple moves which satisfy then input move.
                                            If MatchedMove > -1 Then ResultMove.Code = "c"c : Exit For
                                            MatchedMove = n
                                        End If
                                    Next
                                    If ResultMove.Code = "c" Then Exit For
                                End If
                            End If

                        End If
                        If ResultMove.Code = "c" Then Exit For
                    Next

                    If MatchedMove = -1 Then 'No Move Found.
                        Console.ForegroundColor = ConsoleColor.DarkRed
                        Console.WriteLine($"Unable to interpret move {InputMove} given constraints.")
                        Console.ForegroundColor = ConsoleColor.White
                        ResultMove.Code = "a"c
                    Else 'Sets starting coordinates.
                        ResultMove.OldMoveX = CStr((LegalMoves(MatchedMove) And 56) >> 3)
                        ResultMove.OldMoveY = CStr(LegalMoves(MatchedMove) And 7)
                    End If
                End If

            Case "K"c, "O"c 'As there is only one king for each player, we can easily retrieve the starting coordinates.
                'Sets the start move to be the initial position of the king.
                ResultMove.OldMoveX = CStr((KPos And 56) >> 3)
                ResultMove.OldMoveY = CStr(KPos And 7)
                'User is attempting to castle...
                If FormattedMove = "O-O" Then
                    ResultMove.NewMoveX = "6"
                    ResultMove.NewMoveY = CStr(7 - (7 * (CInt(isWhite) + 1)))
                ElseIf FormattedMove = "O-O-O" Then
                    ResultMove.NewMoveX = "2"
                    ResultMove.NewMoveY = CStr(7 - (7 * (CInt(isWhite) + 1)))
                End If

            Case Else 'Is a pawn.
                ResultMove.OldMoveX = CStr(Asc(FormattedMove(0)) - 97) 'Converts the rank index to a number.
                If FormattedMove.Length = 2 Then 'No Capture - pawn is moving 1 or 2 squares.
                    Dim TempPiece As Char 'Represents the piece we are looking for.
                    If isWhite Then TempPiece = "P"c Else TempPiece = "p"c
                    For n As Byte = 1 To 2
                        'Searches 1 and 2 squares behind the end square, looking for TempPiece.
                        Dim TestY As Integer = Integer.Parse(ResultMove.NewMoveY) - n * (2 * CInt(isWhite) + 1)
                        If Board(Integer.Parse(ResultMove.OldMoveX), TestY) = TempPiece Then
                            'Pawn found - set starting coordinates.
                            ResultMove.OldMoveY = CStr(TestY)
                            Exit Select
                        End If
                    Next
                    'No pawn found.
                    Console.ForegroundColor = ConsoleColor.DarkRed
                    Console.WriteLine("Unable to interpret move given constraints.")
                    Console.ForegroundColor = ConsoleColor.White
                    ResultMove.Code = "a"c
                Else 'is a pawn capture move - set starting coordinates accordingly.
                    ResultMove.OldMoveY = CStr(Integer.Parse(ResultMove.NewMoveY) - (2 * Val(isWhite) + 1))
                End If
        End Select

        Return ResultMove
    End Function

End Class
