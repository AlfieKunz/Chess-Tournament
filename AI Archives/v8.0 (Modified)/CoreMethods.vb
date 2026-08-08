'This class contains most of the primary algorithms I will be using in my project, and will link to both my Chess class and
'my AI class (either via instavintiation or by inheritance). It will contain the algorithms that will be used by both my Chess
'& AI classes, such as the ‘TFTable’ Generator, ‘DoesMoveResolveCheck’, 'Move Converters', and others.
Partial Public Class CoreMethods
    'TrueTables are just TrueFalse Tables containing only the letter T. Very useful for resetting TrueFalse Tables
    'and debugging.
    Public MasterTrueTable(7, 7), TrueTable(7, 7) As Char
    Public CannotCastle As New CanCastle
    Public NotInCheck As New InCheck
    Private Shared ReadOnly PieceValue(9) As Int16 'Array Containing the Value or Weight of each Piece.

    Protected Shared ReadOnly ZobristHashTable(9, 1, 7, 7) As UInt64 '(a, b, c, d), where a = piece type, b = piece colour, c = x-coor, d = y-coor.
    'a is Similar to PieceValue: use (Asc(UCase(PieceName)) Mod 11) to calculate - [2] used for EnPassant square.
    Protected Shared ReadOnly HashConstants(4) As UInt64 '0 = Player Turn, 1 = WhiteKSCastle, 2 = WhiteQSCastle, 3 = BlackKSCastle, 4 = BlackQSCastle.
    Public Sub New()
        PopulateKnightLegalMoveArray()
        'Sets PieceValues variables using a Hash Function (Upper Case letter --> ASCII, then MOD 11). This
        'creates a unique index / row in the PieceValue array for each piece and its corresponding weight,
        'so its value can be searched up quickly. PieceValue(Asc(UCase(Board(x, y))) Mod 11)
        PieceValue(0) = 300 'Bishop Weight
        PieceValue(1) = 300 'Knight Weight
        PieceValue(3) = 100 'Pawn Weight
        PieceValue(4) = 900 'Queen Weight
        PieceValue(5) = 500 'Rook Weight
        PieceValue(9) = 0 'King Weight

        'Creates MasterTrueTable and TrueTable.
        For x = 0 To 7
            For y = 0 To 7
                MasterTrueTable(x, y) = "T"
                TrueTable(x, y) = "T"
            Next
        Next

        'Fills ZobristHasTable with pseudo-random 64-bit numbers
        Static RND As New Random()
        Dim RNDOne, RNDTwo As UInt64
        For w = 0 To 9
            Select Case w
                Case 0, 1, 3, 4, 5, 9
                    For x = 0 To 1
                        For y = 0 To 7
                            For z = 0 To 7
                                'Produce two random 32-bit numbers
                                RNDOne = RND.Next()
                                RNDTwo = RND.Next()
                                'Combine these numbers together into a 64-bit number by applying a 32-bit left shift to RNDOne,
                                'then combining this with RNDTwo via a bitwise OR operation.
                                ZobristHashTable(w, x, y, z) = (RNDOne << 32) Or RNDTwo
                            Next
                        Next
                    Next
            End Select
        Next
        'Fills HasConstants with random 64-bit numbers.
        For n = 0 To 4
            RNDOne = RND.Next()
            RNDTwo = RND.Next()
            HashConstants(n) = (RNDOne << 32) Or RNDTwo
        Next
    End Sub




    'Functions that convert between positions (eg: 53) and the standard chess coordinate notation (eg: f5)
    Public Function PosToCoorConverter(ByVal Position As String) As String 'f5 --> 53
        If Position.Length <> 2 Then Return Position
        Return Chr(Val(Position(0)) + 97) & 8 - Val(Position(1))
    End Function
    Public Function CoorToPosConverter(ByVal Position As String) As String '53 --> f5
        Return Asc(Position(0)) - 97 & 8 - Val(Position(1))
    End Function


    'Subroutine which sets the default settings of the AISearchSettings structure.
    Public Sub SetDefaultAISearchSettings(ByRef SearchSettings As AISearchSettings)
        SearchSettings.UseQuiescence = True
        SearchSettings.UsePieceHeatMaps = True
        SearchSettings.OutputToConsole = True
        SearchSettings.OutputPath = True
        SearchSettings.StableSearch = True
        SearchSettings.ReturnBestMove = True
        SearchSettings.UpdateLifetimeStats = True
    End Sub


    'Function which puts the required pieces in a FEN board position into an 8x8 board array.
    Public Function FENConverter(ByVal FEN As String, ByRef WCanCastle As CanCastle, ByRef BCanCastle As CanCastle, ByRef WKPos As String, ByRef BKPos As String, ByRef EnPassant As String, ByRef IsWhite As Boolean) As Char(,)
        'Resets castling & EnPassant rules
        WCanCastle.CannotCastle()
        BCanCastle.CannotCastle()
        EnPassant = "-"
        Dim x As Byte = 0
        Dim y As Byte = 0
        Dim tempArray(7, 7) As Char
        Dim SpaceLocation As Byte
        For n = 0 To Len(FEN) - 1
            Select Case FEN(n)
                Case "/" '= end of board row. Reset column index and increment row index.
                    y += 1
                    x = 0
                Case "A" To "Z", "a" To "z" '= name of piece - add name of piece to board index.
                    tempArray(x, y) = FEN(n)
                    If FEN(n) = "K" Then
                        WKPos = x & y
                    ElseIf FEN(n) = "k" Then
                        BKPos = x & y
                    End If
                    x += 1
                Case "0" To "8" 'Set of empty squares.
                    For m = 1 To Val(FEN(n))
                        tempArray(x, y) = " "
                        x += 1
                    Next
                Case " "
                    'Checks for pawns incorrectly placed on the 1st or 8th rank. If we detect any, then we provoke an
                    'intentional crash, which our encapsulating Try-Catch detects and promptly reverses.
                    For x = 0 To 7
                        If UCase(tempArray(x, 0)) = "P" OrElse UCase(tempArray(x, 7)) = "P" Then FEN = ""
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
                            EnPassant = CoorToPosConverter(FEN.Substring(m, 2))
                        End If
                    Next
                    Exit For
            End Select
        Next
        Return tempArray
    End Function

    'Function which converts the current board position into its FEN counterpart.
    Public Function ConvertToFEN(ByVal Board(,) As Char, ByVal WCanCastle As CanCastle, ByVal BCanCastle As CanCastle, ByVal EnPassant As String, ByVal isWhite As Boolean) As String
        Dim Counter As Byte = 0 '= the number of blank spaces in a row on the board.
        ConvertToFEN = ""
        For y = 0 To 7
            For x = 0 To 7
                If Board(x, y) = " " Then
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
        ConvertToFEN = ConvertToFEN.TrimEnd("/") 'Removes the last character from the FEN ("/").
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
        If EnPassant <> "-" Then
            'converts the computer-friendly index from 0-7 into the more human-friendly a-h coordinate (eg: 75 -> h3)
            ConvertToFEN &= " " & Chr(Val(EnPassant(0)) + 97) & 8 - Val(EnPassant(1)) & " 0 1"
        Else
            ConvertToFEN &= " - 0 1" 'represents the move numbers for a standard position.
        End If
        Return ConvertToFEN
    End Function



    'Subroutine which creates the TrueFalse Table of the selected player (controlled by the Variable FixWhite).
    'This is done by generating all the legal moves of the pieces that could influence the enemy king's motion.
    'This creates a 'field' around the king (stating where its legal moves are), along with creating pinned pieces
    'and checks.
    Public Sub FixTFTables(ByVal Board(,) As Char, ByVal FixWhite As Boolean, ByRef TrueFalseTable(,) As Char, ByRef KPos As String, ByRef InCheck As InCheck, ByVal EnPassant As String)
        Dim dx, dy As SByte
        'Resets TFTables.
        Array.Copy(MasterTrueTable, TrueFalseTable, 64)
        If FixWhite Then
            For y = 0 To 7
                For x = 0 To 7
                    If Char.IsLower(Board(x, y)) Then
                        'Calculates distances between piece and the enemy king.
                        dx = Math.Abs(Val(KPos(0)) - x)
                        dy = Math.Abs(Val(KPos(1)) - y)
                        If Board(x, y) = "p" Then
                            If Math.Max(dx, dy) <= 2 AndAlso Val(KPos(1)) >= y Then
                                'Pawn could influence king motion - calculate legal moves.
                                BlackPieceLegalMoves(Board, x, y, TrueFalseTable, KPos, InCheck, EnPassant)
                            End If
                        ElseIf Board(x, y) = "b" Then
                            If Math.Abs(dx - dy) <= 2 Then
                                'Piece could influence king motion - calculate legal moves.
                                BlackPieceLegalMoves(Board, x, y, TrueFalseTable, KPos, InCheck, EnPassant)
                            End If
                        ElseIf Board(x, y) = "n" Then
                            If Math.Max(dx, dy) <= 3 AndAlso dx + dy <= 5 Then
                                'Piece could influence king motion - calculate legal moves.
                                BlackPieceLegalMoves(Board, x, y, TrueFalseTable, KPos, InCheck, EnPassant)
                            End If
                        ElseIf Board(x, y) = "r" Then
                            If Math.Min(dx, dy) <= 1 Then
                                'Piece could influence king motion - calculate legal moves.
                                BlackPieceLegalMoves(Board, x, y, TrueFalseTable, KPos, InCheck, EnPassant)
                            End If
                        ElseIf Board(x, y) = "q" Then
                            If Math.Min(dx, dy) <= 1 OrElse Math.Abs(dx - dy) <= 2 Then
                                'Piece could influence king motion - calculate legal moves.
                                BlackPieceLegalMoves(Board, x, y, TrueFalseTable, KPos, InCheck, EnPassant)
                            End If
                        ElseIf Math.Max(dx, dy) <= 2 Then 'is a king.
                            'Piece could influence king motion - calculate legal moves.
                            BlackPieceLegalMoves(Board, x, y, TrueFalseTable, KPos, InCheck, EnPassant)
                        End If
                    End If
                Next
            Next
        Else 'Identical code but for the white pieces (fixing the Black TFTable).
            For y = 0 To 7
                For x = 0 To 7
                    If Char.IsUpper(Board(x, y)) Then
                        dx = Math.Abs(Val(KPos(0)) - x)
                        dy = Math.Abs(Val(KPos(1)) - y)
                        If Board(x, y) = "P" Then
                            If Math.Max(dx, dy) <= 2 AndAlso Val(KPos(1)) <= y Then
                                WhitePieceLegalMoves(Board, x, y, TrueFalseTable, KPos, InCheck, EnPassant)
                            End If
                        ElseIf Board(x, y) = "B" Then
                            If Math.Abs(dx - dy) <= 2 Then
                                WhitePieceLegalMoves(Board, x, y, TrueFalseTable, KPos, InCheck, EnPassant)
                            End If
                        ElseIf Board(x, y) = "N" Then
                            If Math.Max(dx, dy) <= 3 AndAlso dx + dy <= 5 Then
                                WhitePieceLegalMoves(Board, x, y, TrueFalseTable, KPos, InCheck, EnPassant)
                            End If
                        ElseIf Board(x, y) = "R" Then
                            If Math.Min(dx, dy) <= 1 Then
                                WhitePieceLegalMoves(Board, x, y, TrueFalseTable, KPos, InCheck, EnPassant)
                            End If
                        ElseIf Board(x, y) = "Q" Then
                            If Math.Min(dx, dy) <= 1 OrElse Math.Abs(dx - dy) <= 2 Then
                                WhitePieceLegalMoves(Board, x, y, TrueFalseTable, KPos, InCheck, EnPassant)
                            End If
                        ElseIf Math.Max(dx, dy) <= 2 Then 'is a king.
                            WhitePieceLegalMoves(Board, x, y, TrueFalseTable, KPos, InCheck, EnPassant)
                        End If
                    End If
                Next
            Next
        End If
    End Sub



    'Algorithm that returns the weight / value of a given piece. Links to the array of hashed values PieceValue.
    'Note that the input of this function must be uppercase!!
    Public Function ReturnPieceValue(ByVal Piece As Char) As Int16
        Return PieceValue(Asc(Piece) Mod 11)
    End Function


    'Function which counts up all the material (and their values) on the board. Stores this information in two
    'variables - one for white's total material count, and the other for black's total material count.
    Public Function CountMaterial(ByVal Board(,) As Char) As Int16()
        Dim MaterialCount(1) As Int16
        For y = 0 To 7
            For x = 0 To 7
                If Board(x, y) <> " " Then
                    'Add the value of the piece to either White or Black's total.
                    If Char.IsUpper(Board(x, y)) Then
                        MaterialCount(0) += ReturnPieceValue(Board(x, y))
                    Else
                        MaterialCount(1) += ReturnPieceValue(UCase(Board(x, y)))
                    End If
                End If
            Next
        Next
        Return MaterialCount
    End Function

    'Function that hashes a chess position (including its details) into a 64-bit number using the 'Zobrist Hash' algorithm.
    Public Function ZobristHashPosition(ByVal Board(,) As Char, ByVal isWhite As Boolean, ByVal WCanCastle As CanCastle, ByVal BCanCastle As CanCastle, ByVal EnPassant As String) As UInt64
        ZobristHashPosition = 0
        For y = 0 To 7
            For x = 0 To 7
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
        If EnPassant <> "-" Then ZobristHashPosition = ZobristHashPosition Xor ZobristHashTable(2, 0, Val(EnPassant(0)), Val(EnPassant(1)))
    End Function


    'Subroutine that converts a Move into standard PGN chess notation (eg: e4, Nf4, Ka2).
    Public Function MoveConverter(ByVal Board(,) As Char, ByVal TempMove As Move, ByVal EnPassant As String) As String
        Dim MovedPiece As Char = UCase(Board(TempMove.OldMoveX, TempMove.OldMoveY))
        'Pawns operate differently with standard chess notation - when moving a pawn, we give its column (a-h),
        'then add its file that it is moving to. For all other pieces, we state the name of the piece, then its
        'end coordinates.
        If MovedPiece <> " " Then
            If MovedPiece = "P" Then
                MoveConverter = Chr(Val(TempMove.OldMoveX) + 97)
                'Code for detecting castling.
            ElseIf MovedPiece = "K" AndAlso TempMove.OldMoveX = 4 AndAlso (TempMove.NewMoveY = 0 OrElse TempMove.NewMoveY = 7) Then
                If TempMove.NewMoveX = 6 Then 'Is king-side castling.
                    Return "O-O" 'Notation for KS castling.
                ElseIf TempMove.NewMoveX = 2 Then ''Is queen-side castling.
                    Return "O-O-O" 'Notation for QS castling.
                Else
                    MoveConverter = MovedPiece
                End If
            Else
                MoveConverter = MovedPiece
            End If
            If Board(TempMove.NewMoveX, TempMove.NewMoveY) <> " " OrElse (UCase(MovedPiece) = "P" AndAlso EnPassant = TempMove.NewMoveX & TempMove.NewMoveY) Then
                'Is a capture move - add an "x" followed by the coordinates of the captured piece.
                MoveConverter &= "x" & PosToCoorConverter(TempMove.NewMoveX & TempMove.NewMoveY)
            Else
                If MovedPiece <> "P" Then MoveConverter &= Chr(Val(TempMove.NewMoveX) + 97)
                MoveConverter &= 8 - TempMove.NewMoveY
            End If
            'Code for pawn promotions.
            If MovedPiece = "P" AndAlso (TempMove.NewMoveY = 0 OrElse TempMove.NewMoveY = 7) Then MoveConverter &= "=Q"
        Else
            Return "ERROR"
        End If
    End Function

    'Function that converts a standard chess move (eg: e4, Nf4, Ka2) into a Move.
    Public Function ConvertToMove(ByVal InputMove As String, ByVal Board(,) As Char, ByVal isWhite As Boolean, ByVal KPos As String, ByVal TFTable(,) As Char)
        'Removes extra data from move (that is not useful to my system, ie: checks & pawn promotion tags).
        InputMove = InputMove.TrimEnd(CChar("+"))
        If InputMove(InputMove.Length - 2) = "=" Then InputMove = InputMove.Substring(0, InputMove.Length - 2)
        Dim ResultMove As New Move
        'Sets end position to the last 2 characters of the move.
        Dim TempEndPosition As String = CoorToPosConverter(InputMove.Substring(InputMove.Length - 2, 2))
        ResultMove.NewMoveX = TempEndPosition(0)
        ResultMove.NewMoveY = TempEndPosition(1)
        Select Case InputMove(0)
            Case "B", "N", "R", "Q"
                'Constraint is used to specify which piece should move to the square (if there are multiple to choose from).
                Dim Constraint As String = InputMove.Substring(1, InputMove.Length - 3)
                Constraint = Constraint.TrimEnd(CChar("x"))
                If Constraint.Length = 2 Then 'Constraint length of 2 specifies the exact starting coordinates - retrieve these.
                    TempEndPosition = CoorToPosConverter(Constraint)
                    ResultMove.OldMoveX = TempEndPosition(0)
                    ResultMove.OldMoveY = TempEndPosition(1)
                Else
                    Dim TempPiece As Char 'Represents the piece we are looking for.
                    If isWhite Then TempPiece = InputMove(0) Else TempPiece = LCase(InputMove(0))
                    'Creates temporary, empty board that holds TempPiece at the ending coordinates.
                    Dim EmptyBoard(7, 7) As Char
                    For y = 0 To 7
                        For x = 0 To 7
                            EmptyBoard(x, y) = " "
                        Next
                    Next
                    EmptyBoard(ResultMove.NewMoveX, ResultMove.NewMoveY) = TempPiece
                    Dim LegalMoves() As String
                    Array.Copy(MasterTrueTable, TrueTable, 64)
                    'Calculate the legal moves of TempPiece on EmptyBoard. This produces a set of coordinates that that piece can move to (one of which will be the starting coordinates).
                    If isWhite Then
                        LegalMoves = WhitePieceLegalMoves(EmptyBoard, ResultMove.NewMoveX, ResultMove.NewMoveY, TrueTable, NotInCheck, CannotCastle, "-")
                    Else
                        LegalMoves = BlackPieceLegalMoves(EmptyBoard, ResultMove.NewMoveX, ResultMove.NewMoveY, TrueTable, NotInCheck, CannotCastle, "-")
                    End If

                    'Checks if any of these moves contains TempPiece on Board. If so then that piece is the one that is moving, and hence we set that to be the starting coordinates.
                    Dim MatchedMove As SByte = -1
                    For n = 0 To LegalMoves.Length - 1
                        If Board(Val(LegalMoves(n)(0)), Val(LegalMoves(n)(1))) = TempPiece Then
                            Dim TestMoves() As String
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
                                        If Asc(Constraint) - 97 = Val(LegalMoves(n)(0)) Then
                                            ConstraintMatches = True
                                        End If
                                    Case Else
                                        'Rank constraint - only accept move if its rank agrees with the constraint's.
                                        If 8 - Constraint = Val(LegalMoves(n)(1)) Then
                                            ConstraintMatches = True
                                        End If
                                End Select
                            End If

                            If ConstraintMatches Then
                                'Tests if move is valid by playing it on the original Board.
                                If isWhite Then
                                    TestMoves = WhitePieceLegalMoves(Board, Val(LegalMoves(n)(0)), Val(LegalMoves(n)(1)), TFTable, NotInCheck, CannotCastle, "-")
                                Else
                                    TestMoves = BlackPieceLegalMoves(Board, Val(LegalMoves(n)(0)), Val(LegalMoves(n)(1)), TFTable, NotInCheck, CannotCastle, "-")
                                End If
                                For m = 0 To TestMoves.Length - 1
                                    If Val(TestMoves(m)(0)) = ResultMove.NewMoveX AndAlso Val(TestMoves(m)(1)) = ResultMove.NewMoveY Then
                                        'Move is a match! And is therefore valid.
                                        MatchedMove = n
                                        Exit For
                                    End If
                                Next
                                If MatchedMove > -1 Then Exit For
                            End If

                        End If
                    Next
                    If MatchedMove = -1 Then 'No Move Found.
                        Console.WriteLine("Unable to interpret move given constraints.")
                        Return Nothing
                    Else 'Sets starting coordinates.
                        ResultMove.OldMoveX = LegalMoves(MatchedMove)(0)
                        ResultMove.OldMoveY = LegalMoves(MatchedMove)(1)
                    End If
                End If

            Case "K", "O" 'As there is only one king for each player, we can easily retrieve the starting coordinates.
                'Sets the start move to be the initial position of the king.
                ResultMove.OldMoveX = Val(KPos(0))
                ResultMove.OldMoveY = Val(KPos(1))
                'User is attempting to castle...
                If InputMove = "O-O" Then
                    ResultMove.NewMoveX = 6
                    ResultMove.NewMoveY = 7 - (7 * (Val(isWhite) + 1))
                ElseIf InputMove = "O-O-O" Then
                    ResultMove.NewMoveX = 2
                    ResultMove.NewMoveY = 7 - (7 * (Val(isWhite) + 1))
                End If

            Case Else 'Is a pawn.
                ResultMove.OldMoveX = Asc(InputMove(0)) - 97 'Converts the rank index to a number.
                If InputMove.Length = 2 Then 'No Capture - pawn is moving 1 or 2 squares.
                    Dim TempPiece As Char 'Represents the piece we are looking for.
                    If isWhite Then TempPiece = "P" Else TempPiece = "p"
                    For n = 1 To 2
                        'Searches 1 and 2 squares behind the end square, looking for TempPiece.
                        If Board(ResultMove.OldMoveX, (ResultMove.NewMoveY - n * (2 * Val(isWhite) + 1))) = TempPiece Then
                            'Pawn found - set starting coordinates.
                            ResultMove.OldMoveY = (ResultMove.NewMoveY - n * (2 * Val(isWhite) + 1))
                            Exit Select
                        End If
                    Next
                    'No pawn found.
                    Console.WriteLine("Unable to interpret move given constraints.")
                    Return Nothing
                Else 'is a pawn capture move - set starting coordinates accordingly.
                    ResultMove.OldMoveY = ResultMove.NewMoveY - (2 * Val(isWhite) + 1)
                End If
        End Select

        Return ResultMove
    End Function

End Class
