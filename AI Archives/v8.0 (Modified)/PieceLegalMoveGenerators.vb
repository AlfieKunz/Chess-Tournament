'This class contains the code for generating the pseudo-legal moves for a given piece on the board. I have broken these
'algorithms into two main components: one which will be called by the Move Generation algorithms, and will return a set
'of moves that the input piece can make. The second set will be called by the TFTable Fixer algorithms, and will only
'update the TFTable squares of the required player (however, the core of each of these algorithms remain the same - to
'determine the pseudo-legal moves that a piece can make. Originally, these two sets were combined into a singular set
'(which were called by both algorithms), but are now separated to improve efficiency (from v6.1 and onwards).
Partial Public Class CoreMethods
    'Attribute that contains all the legal moves for a knight, placed anywhere on the board. When the program initally boots,
    'we simulate a knight being placed on each square on the board, and log its pseudo-legal moves into the KnightLegalMoveArray
    'structure. This is so that, when we want to determine the pseudo-legal moves of a knight on the board, we just look up
    'the information in KnightLegalMoveArray.
    Private Shared KnightLegalMoveArray(7, 7, 8) As String

    'Sub that populates KnightLegalMoveArray
    Public Sub PopulateKnightLegalMoveArray()
        Dim LegalMoveArray(27) As String
        Dim Counter As SByte

        For CoorY = 0 To 7
            For CoorX = 0 To 7
                'Resets the LegalMoveArray structure.
                For n = 0 To 27
                    LegalMoveArray(n) = Nothing
                Next
                Counter = 1

                'Calculates the pseudo-legal moves for a knight placed at the given square.
                If CoorX >= 2 Then
                    For x = -1 To 1 Step 2
                        If (CoorY + x) >= 0 AndAlso (CoorY + x) <= 7 Then
                            LegalMoveArray(Counter) = (CoorX - 2) & (CoorY + x)
                            Counter += 1
                        End If
                    Next
                End If
                If CoorX <= 5 Then
                    For x = -1 To 1 Step 2
                        If (CoorY + x) >= 0 AndAlso (CoorY + x) <= 7 Then
                            LegalMoveArray(Counter) = (CoorX + 2) & (CoorY + x)
                            Counter += 1
                        End If
                    Next
                End If
                If CoorY >= 2 Then
                    For x = -1 To 1 Step 2
                        If (CoorX + x) >= 0 AndAlso (CoorX + x) <= 7 Then
                            LegalMoveArray(Counter) = (CoorX + x) & (CoorY - 2)
                            Counter += 1
                        End If
                    Next
                End If
                If CoorY <= 5 Then
                    For x = -1 To 1 Step 2
                        If (CoorX + x) >= 0 AndAlso (CoorX + x) <= 7 Then
                            LegalMoveArray(Counter) = (CoorX + x) & (CoorY + 2)
                            Counter += 1
                        End If
                    Next
                End If

                'Stores the total number of moves into the first index of LegalMoveArray, then copies this array
                'into the corresponding index in KnightLegalMoveArray.
                LegalMoveArray(0) = Counter
                For n = 0 To Val(LegalMoveArray(0)) - 1
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
    Public Function WhitePieceLegalMoves(ByVal Board(,) As Char, ByVal CoorX As SByte, ByVal CoorY As SByte, ByRef WhiteTFTable(,) As Char, ByVal WInCheck As InCheck, ByRef WCanCastle As CanCastle, ByVal EnPassant As String) As String()
        Dim LegalMoveArray(27) As String
        Dim n As Byte

        If Board(CoorX, CoorY) = "P" Then 'Legal Moves for the Pawn (along with First Moves).
            If WhiteTFTable(CoorX, CoorY) <> "2" Then
                'If the pawn is not pinned horizontally...
                If WhiteTFTable(CoorX, CoorY) <> "1" AndAlso WhiteTFTable(CoorX, CoorY) <> "3" Then
                    If Board(CoorX, CoorY - 1) = " " Then
                        LegalMoveArray(n) = CoorX & CoorY - 1
                        n += 1
                        If CoorY = 6 AndAlso Board(CoorX, 4) = " " Then 'Pawns can move two squares on their first move.
                            LegalMoveArray(n) = CoorX & 4
                            n += 1
                        End If
                    End If
                End If
                If WhiteTFTable(CoorX, CoorY) <> "0" Then
                    'Code for pawn left captures.
                    If CoorX > 0 AndAlso WhiteTFTable(CoorX, CoorY) <> "1" Then
                        If Char.IsLower(Board(CoorX - 1, CoorY - 1)) OrElse (CoorX - 1 & CoorY - 1 = EnPassant AndAlso WhiteTFTable(CoorX, CoorY) <> "4") Then
                            LegalMoveArray(n) = CoorX - 1 & CoorY - 1
                            n += 1
                        End If
                    End If
                    'Code for pawn right captures.
                    If CoorX < 7 AndAlso WhiteTFTable(CoorX, CoorY) <> "3" Then
                        If Char.IsLower(Board(CoorX + 1, CoorY - 1)) OrElse (CoorX + 1 & CoorY - 1 = EnPassant AndAlso WhiteTFTable(CoorX, CoorY) <> "4") Then
                            LegalMoveArray(n) = CoorX + 1 & CoorY - 1
                            n += 1
                        End If
                    End If
                End If
            End If


        ElseIf Board(CoorX, CoorY) = "N" Then 'Legal Moves for the Knight. (If pinned, then it cannot move at all.)
            If WhiteTFTable(CoorX, CoorY) >= "5" Then
                Dim TestMove As String
                'Retrieves moves from the KnightLegalMove lookup table.
                For m = 1 To Val(KnightLegalMoveArray(CoorX, CoorY, 0)) - 1
                    TestMove = KnightLegalMoveArray(CoorX, CoorY, m)
                    If Not Char.IsUpper(Board(Val(TestMove(0)), Val(TestMove(1)))) Then
                        LegalMoveArray(n) = KnightLegalMoveArray(CoorX, CoorY, m)
                        n += 1
                    End If
                Next
            End If


        ElseIf Board(CoorX, CoorY) = "K" Then 'Legal Moves for the King (along with Castling).
            'Sets coordinate bounds. Start & End Coords, in this case, represents the 8 squares around the king.
            For y = Math.Max(CoorY - 1, 0) To Math.Min(CoorY + 1, 7)
                For x = Math.Max(CoorX - 1, 0) To Math.Min(CoorX + 1, 7)
                    If Not Char.IsUpper(Board(x, y)) AndAlso WhiteTFTable(x, y) = "T" Then
                        'Therefore is a legal move...
                        LegalMoveArray(n) = x & y
                        n += 1
                    End If
                Next
            Next
            If Not WInCheck.IsInCheck Then
                If WCanCastle.KS Then
                    If Board(5, 7) = " " AndAlso Board(6, 7) = " " AndAlso WhiteTFTable(5, 7) = "T" AndAlso WhiteTFTable(6, 7) = "T" Then
                        If Board(7, 7) = "R" Then
                            LegalMoveArray(n) = "67" 'Square king would go to to castle king-side.
                            n += 1
                        Else
                            'The rook is no longer at the required square (eg: it has been captured) - stop the player
                            'from castling in this direction.
                            WCanCastle.KS = False
                        End If
                    End If
                End If
                If WCanCastle.QS Then
                    If Board(3, 7) = " " AndAlso Board(2, 7) = " " AndAlso Board(1, 7) = " " AndAlso WhiteTFTable(3, 7) = "T" AndAlso WhiteTFTable(2, 7) = "T" Then
                        If Board(0, 7) = "R" Then
                            LegalMoveArray(n) = "27" 'Square king would go to to castle queen-side.
                            n += 1
                        Else
                            WCanCastle.QS = False
                        End If
                    End If
                End If
            End If


        Else
            If Board(CoorX, CoorY) = "R" OrElse Board(CoorX, CoorY) = "Q" Then 'Legal Moves for the Rook / part of Queen.
                If WhiteTFTable(CoorX, CoorY) <> "1" AndAlso WhiteTFTable(CoorX, CoorY) <> "3" Then
                    If WhiteTFTable(CoorX, CoorY) <> "0" Then 'if rook is not pinned vertically...
                        'Right Movement.
                        For x = (CoorX + 1) To 7
                            If Char.IsUpper(Board(x, CoorY)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = x & CoorY
                                n += 1
                                If Board(x, CoorY) <> " " Then Exit For
                            End If
                        Next
                        'Left Movement
                        For x = (CoorX - 1) To 0 Step -1
                            If Char.IsUpper(Board(x, CoorY)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = x & CoorY
                                n += 1
                                If Board(x, CoorY) <> " " Then Exit For
                            End If
                        Next
                    End If

                    'Down Movement.
                    If WhiteTFTable(CoorX, CoorY) <> "2" Then
                        For y = (CoorY + 1) To 7
                            If Char.IsUpper(Board(CoorX, y)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = CoorX & y
                                n += 1
                                If Board(CoorX, y) <> " " Then Exit For
                            End If
                        Next

                        'Up Movement.
                        For y = (CoorY - 1) To 0 Step -1
                            If Char.IsUpper(Board(CoorX, y)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = CoorX & y
                                n += 1
                                If Board(CoorX, y) <> " " Then Exit For
                            End If
                        Next
                    End If
                End If
            End If


            If Board(CoorX, CoorY) = "B" OrElse Board(CoorX, CoorY) = "Q" Then 'Legal Moves for the Biship / Part of Queen.
                If WhiteTFTable(CoorX, CoorY) <> "0" AndAlso WhiteTFTable(CoorX, CoorY) <> "2" Then
                    Dim StartX, StartY As SByte 'Pointers for the coordinate bounds.
                    If WhiteTFTable(CoorX, CoorY) <> "1" Then
                        'Right-Down Movement.
                        StartX = CoorX + 1
                        StartY = CoorY + 1
                        Do Until StartX > 7 OrElse StartY > 7
                            If Char.IsUpper(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartX & StartY
                                n += 1
                                If Board(StartX, StartY) <> " " Then Exit Do
                            End If
                            StartX += 1
                            StartY += 1
                        Loop

                        'Left-Up Movement.
                        StartX = CoorX - 1
                        StartY = CoorY - 1
                        Do Until StartX < 0 OrElse StartY < 0
                            If Char.IsUpper(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartX & StartY
                                n += 1
                                If Board(StartX, StartY) <> " " Then Exit Do
                            End If
                            StartX -= 1
                            StartY -= 1
                        Loop

                    End If
                    If WhiteTFTable(CoorX, CoorY) <> "3" Then
                        'Right-Up Movement.
                        StartX = CoorX + 1
                        StartY = CoorY - 1
                        Do Until StartX > 7 OrElse StartY < 0
                            If Char.IsUpper(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartX & StartY
                                n += 1
                                If Board(StartX, StartY) <> " " Then Exit Do
                            End If
                            StartX += 1
                            StartY -= 1
                        Loop

                        'Left-Down Movement.
                        StartX = CoorX - 1
                        StartY = CoorY + 1
                        Do Until StartX < 0 OrElse StartY > 7
                            If Char.IsUpper(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartX & StartY
                                n += 1
                                If Board(StartX, StartY) <> " " Then Exit Do
                            End If
                            StartX -= 1
                            StartY += 1
                        Loop
                    End If
                End If
            End If
        End If

        'Shrinks the array to the correct size, so that there are no blank entries.
        If n = 0 Then Return Nothing
        Array.Resize(LegalMoveArray, n)
        Return LegalMoveArray
    End Function


    'As the below subroutine is very similar to its equivilant 'WhitePieceLegalMoves' function, commenting will be limited.
    Public Function BlackPieceLegalMoves(ByVal Board(,) As Char, ByVal CoorX As SByte, ByVal CoorY As SByte, ByRef BlackTFTable(,) As Char, ByVal BInCheck As InCheck, ByRef BCanCastle As CanCastle, ByVal EnPassant As String) As String()
        Dim LegalMoveArray(27) As String
        Dim n As Byte

        If Board(CoorX, CoorY) = "p" Then 'Legal Moves for the Pawn (along with First Moves.)
            If BlackTFTable(CoorX, CoorY) <> "2" Then
                If BlackTFTable(CoorX, CoorY) <> "1" AndAlso BlackTFTable(CoorX, CoorY) <> "3" Then
                    If Board(CoorX, CoorY + 1) = " " Then
                        LegalMoveArray(n) = CoorX & CoorY + 1
                        n += 1
                        If CoorY = 1 AndAlso Board(CoorX, 3) = " " Then
                            LegalMoveArray(n) = CoorX & 3
                            n += 1
                        End If
                    End If
                End If
                If BlackTFTable(CoorX, CoorY) <> "0" Then
                    If CoorX > 0 AndAlso BlackTFTable(CoorX, CoorY) <> "3" Then
                        If Char.IsUpper(Board(CoorX - 1, CoorY + 1)) OrElse (CoorX - 1 & CoorY + 1 = EnPassant AndAlso BlackTFTable(CoorX, CoorY) <> "4") Then
                            LegalMoveArray(n) = CoorX - 1 & CoorY + 1
                            n += 1
                        End If
                    End If
                    If CoorX < 7 AndAlso BlackTFTable(CoorX, CoorY) <> "1" Then
                        If Char.IsUpper(Board(CoorX + 1, CoorY + 1)) OrElse (CoorX + 1 & CoorY + 1 = EnPassant AndAlso BlackTFTable(CoorX, CoorY) <> "4") Then
                            LegalMoveArray(n) = CoorX + 1 & CoorY + 1
                            n += 1
                        End If
                    End If
                End If
            End If


        ElseIf Board(CoorX, CoorY) = "n" Then 'Legal Moves for the Knight. (If pinned, then it cannot move at all.)
            If BlackTFTable(CoorX, CoorY) >= "5" Then
                Dim TestMove As String
                For m = 1 To Val(KnightLegalMoveArray(CoorX, CoorY, 0)) - 1
                    TestMove = KnightLegalMoveArray(CoorX, CoorY, m)
                    If Not Char.IsLower(Board(Val(TestMove(0)), Val(TestMove(1)))) Then
                        LegalMoveArray(n) = KnightLegalMoveArray(CoorX, CoorY, m)
                        n += 1
                    End If
                Next
            End If


        ElseIf Board(CoorX, CoorY) = "k" Then 'Legal Moves for the King (Along with Castling).
            For y = Math.Max(CoorY - 1, 0) To Math.Min(CoorY + 1, 7)
                For x = Math.Max(CoorX - 1, 0) To Math.Min(CoorX + 1, 7)
                    If Not Char.IsLower(Board(x, y)) AndAlso BlackTFTable(x, y) = "T" Then
                        LegalMoveArray(n) = x & y
                        n += 1
                    End If
                Next
            Next
            If Not BInCheck.IsInCheck Then
                If BCanCastle.KS Then
                    If Board(5, 0) = " " AndAlso Board(6, 0) = " " AndAlso BlackTFTable(5, 0) = "T" AndAlso BlackTFTable(6, 0) = "T" Then
                        If Board(7, 0) = "r" Then
                            LegalMoveArray(n) = "60"
                            n += 1
                        Else
                            BCanCastle.KS = False
                        End If
                    End If
                End If
                If BCanCastle.QS Then
                    If Board(3, 0) = " " AndAlso Board(2, 0) = " " AndAlso Board(1, 0) = " " AndAlso BlackTFTable(3, 0) = "T" AndAlso BlackTFTable(2, 0) = "T" Then
                        If Board(0, 0) = "r" Then
                            LegalMoveArray(n) = "20"
                            n += 1
                        Else
                            BCanCastle.QS = False
                        End If
                    End If
                End If
            End If


        Else
            If Board(CoorX, CoorY) = "r" OrElse Board(CoorX, CoorY) = "q" Then 'Legal Moves for the Rook / Part of Queen.
                If BlackTFTable(CoorX, CoorY) <> "1" AndAlso BlackTFTable(CoorX, CoorY) <> "3" Then
                    If BlackTFTable(CoorX, CoorY) <> "0" Then
                        For x = (CoorX + 1) To 7
                            If Char.IsLower(Board(x, CoorY)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = x & CoorY
                                n += 1
                                If Board(x, CoorY) <> " " Then Exit For
                            End If
                        Next
                        For x = (CoorX - 1) To 0 Step -1
                            If Char.IsLower(Board(x, CoorY)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = x & CoorY
                                n += 1
                                If Board(x, CoorY) <> " " Then Exit For
                            End If
                        Next
                    End If

                    If BlackTFTable(CoorX, CoorY) <> "2" Then
                        For y = (CoorY + 1) To 7
                            If Char.IsLower(Board(CoorX, y)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = CoorX & y
                                n += 1
                                If Board(CoorX, y) <> " " Then Exit For
                            End If
                        Next

                        For y = (CoorY - 1) To 0 Step -1
                            If Char.IsLower(Board(CoorX, y)) Then
                                Exit For
                            Else
                                LegalMoveArray(n) = CoorX & y
                                n += 1
                                If Board(CoorX, y) <> " " Then Exit For
                            End If
                        Next
                    End If
                End If
            End If


            If Board(CoorX, CoorY) = "b" OrElse Board(CoorX, CoorY) = "q" Then 'Legal Moves for the Bishop / Part of Queen.
                If BlackTFTable(CoorX, CoorY) <> "0" AndAlso BlackTFTable(CoorX, CoorY) <> "2" Then
                    Dim StartX, StartY As SByte 'Pointers for the coordinate bounds.
                    If BlackTFTable(CoorX, CoorY) <> "1" Then
                        StartX = CoorX + 1
                        StartY = CoorY + 1
                        Do Until StartX > 7 OrElse StartY > 7
                            If Char.IsLower(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartX & StartY
                                n += 1
                                If Board(StartX, StartY) <> " " Then Exit Do
                            End If
                            StartX += 1
                            StartY += 1
                        Loop

                        StartX = CoorX - 1
                        StartY = CoorY - 1
                        Do Until StartX < 0 OrElse StartY < 0
                            If Char.IsLower(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartX & StartY
                                n += 1
                                If Board(StartX, StartY) <> " " Then Exit Do
                            End If
                            StartX -= 1
                            StartY -= 1
                        Loop
                    End If

                    If BlackTFTable(CoorX, CoorY) <> "3" Then
                        StartX = CoorX + 1
                        StartY = CoorY - 1
                        Do Until StartX > 7 OrElse StartY < 0
                            If Char.IsLower(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartX & StartY
                                n += 1
                                If Board(StartX, StartY) <> " " Then Exit Do
                            End If
                            StartX += 1
                            StartY -= 1
                        Loop

                        StartX = CoorX - 1
                        StartY = CoorY + 1
                        Do Until StartX < 0 OrElse StartY > 7
                            If Char.IsLower(Board(StartX, StartY)) Then
                                Exit Do
                            Else
                                LegalMoveArray(n) = StartX & StartY
                                n += 1
                                If Board(StartX, StartY) <> " " Then Exit Do
                            End If
                            StartX -= 1
                            StartY += 1
                        Loop
                    End If
                End If
            End If
        End If

        If n = 0 Then Return Nothing
        Array.Resize(LegalMoveArray, n)
        Return LegalMoveArray
    End Function




    Public Sub WhitePieceLegalMoves(ByVal Board(,) As Char, ByVal CoorX As SByte, ByVal CoorY As SByte, ByRef BlackTFTable(,) As Char, ByVal BKPos As String, ByRef BInCheck As InCheck, ByVal EnPassant As String)
        Dim CheckingPiece As String = " "
        Dim PiecePutsPlayerInCheck As Boolean

        If Board(CoorX, CoorY) = "P" Then 'Legal Moves for the Pawn (along with First Moves).
            If CoorX > 0 Then
                If BlackTFTable(CoorX - 1, CoorY - 1) = "T" Then BlackTFTable(CoorX - 1, CoorY - 1) = "F"
                If Board(CoorX - 1, CoorY - 1) = "k" Then
                    BInCheck.IsInCheck = True
                    CheckingPiece = CoorX & CoorY
                End If
            End If
            'Code for pawn right captures.
            If CoorX < 7 Then
                If BlackTFTable(CoorX + 1, CoorY - 1) = "T" Then BlackTFTable(CoorX + 1, CoorY - 1) = "F"
                If Board(CoorX + 1, CoorY - 1) = "k" Then
                    BInCheck.IsInCheck = True
                    CheckingPiece = CoorX & CoorY
                End If
            End If


        ElseIf Board(CoorX, CoorY) = "N" Then 'Legal Moves for the Knight. (If pinned, then it cannot move at all.)
            Dim TestMove As String
            For m = 1 To Val(KnightLegalMoveArray(CoorX, CoorY, 0)) - 1
                TestMove = KnightLegalMoveArray(CoorX, CoorY, m)
                If BlackTFTable(Val(TestMove(0)), Val(TestMove(1))) = "T" Then BlackTFTable(Val(TestMove(0)), Val(TestMove(1))) = "F"
                If Board(Val(TestMove(0)), Val(TestMove(1))) = "k" Then
                    BInCheck.IsInCheck = True
                    CheckingPiece = CoorX & CoorY
                End If
            Next


        ElseIf Board(CoorX, CoorY) = "K" Then
            For y = Math.Max(CoorY - 1, 0) To Math.Min(CoorY + 1, 7)
                For x = Math.Max(CoorX - 1, 0) To Math.Min(CoorX + 1, 7)
                    If BlackTFTable(x, y) = "T" Then BlackTFTable(x, y) = "F"
                Next
            Next


        Else
            If Board(CoorX, CoorY) = "R" OrElse Board(CoorX, CoorY) = "Q" Then 'Legal Moves for the Rook / part of Queen.
                For x = (CoorX + 1) To 7
                    If BlackTFTable(x, CoorY) = "T" Then BlackTFTable(x, CoorY) = "F"
                    If Char.IsUpper(Board(x, CoorY)) Then
                        If Board(x, CoorY) = "P" AndAlso x & CoorY + 1 = EnPassant AndAlso Val(BKPos(0)) > x AndAlso Val(BKPos(1)) = CoorY Then
                            'Initiate pin checks (inc friendly-first EnPassant pins).
                            For m = x + 2 To 7
                                If Board(m, CoorY) = "k" Then
                                    BlackTFTable(x + 1, CoorY) = "4"
                                    Exit For
                                End If
                                'There is another piece in the way - cancel the search.
                                If Board(m, CoorY) <> " " Then Exit For
                            Next
                        End If
                        Exit For
                    Else
                        If Board(x, CoorY) = "k" Then
                            BInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            'Marks the next square as illegal for the king to move to.
                            If BlackTFTable(Math.Min(x + 1, 7), CoorY) = "T" Then BlackTFTable(Math.Min(x + 1, 7), CoorY) = "F"
                            Exit For
                        End If
                        If Char.IsLower(Board(x, CoorY)) AndAlso Val(BKPos(0)) > x AndAlso Val(BKPos(1)) = CoorY Then
                            'Initiate pin checks.
                            For m = x + 1 To 7
                                If Board(m, CoorY) = "k" Then
                                    'Enemy king spotted - mark piece as pinned.
                                    BlackTFTable(x, CoorY) = "2"
                                    Exit For
                                End If
                                'Checks for enemy-first EnPassant pins.
                                If Board(m, CoorY) <> " " Then
                                    If Board(m, CoorY) = "P" AndAlso m & CoorY + 1 = EnPassant Then
                                        'Pawn should be stopped from taking En-Passant.
                                        For a = m + 1 To 7
                                            If Board(a, CoorY) = "k" Then
                                                BlackTFTable(x, CoorY) = "4"
                                                Exit For
                                            End If
                                            If Board(a, CoorY) <> " " Then Exit For
                                        Next
                                    End If
                                    Exit For
                                End If
                            Next
                        End If

                        If Board(x, CoorY) <> " " Then Exit For
                    End If
                Next

                For x = (CoorX - 1) To 0 Step -1
                    If BlackTFTable(x, CoorY) = "T" Then BlackTFTable(x, CoorY) = "F"
                    If Char.IsUpper(Board(x, CoorY)) Then
                        If Board(x, CoorY) = "P" AndAlso x & CoorY + 1 = EnPassant AndAlso Val(BKPos(0)) < x AndAlso Val(BKPos(1)) = CoorY Then
                            For m = x - 2 To 0 Step -1
                                If Board(m, CoorY) = "k" Then
                                    BlackTFTable(x - 1, CoorY) = "4"
                                    Exit For
                                End If
                                If Board(m, CoorY) <> " " Then Exit For
                            Next
                        End If
                        Exit For
                    Else
                        If Board(x, CoorY) = "k" Then
                            BInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If BlackTFTable(Math.Max(x - 1, 0), CoorY) = "T" Then BlackTFTable(Math.Max(x - 1, 0), CoorY) = "F"
                            Exit For
                        End If
                        If Char.IsLower(Board(x, CoorY)) AndAlso Val(BKPos(0)) < x AndAlso Val(BKPos(1)) = CoorY Then
                            For m = x - 1 To 0 Step -1
                                If Board(m, CoorY) = "k" Then
                                    BlackTFTable(x, CoorY) = "2"
                                    Exit For
                                End If
                                If Board(m, CoorY) <> " " Then
                                    If Board(m, CoorY) = "P" AndAlso m & CoorY + 1 = EnPassant Then
                                        For a = m - 1 To 0 Step -1
                                            If Board(a, CoorY) = "k" Then
                                                BlackTFTable(x, CoorY) = "4"
                                                Exit For
                                            End If
                                            If Board(a, CoorY) <> " " Then Exit For
                                        Next
                                    End If
                                    Exit For
                                End If
                            Next
                        End If

                        If Board(x, CoorY) <> " " Then Exit For
                    End If
                Next

                For y = (CoorY + 1) To 7
                    If BlackTFTable(CoorX, y) = "T" Then BlackTFTable(CoorX, y) = "F"
                    If Char.IsUpper(Board(CoorX, y)) Then
                        Exit For
                    Else
                        If Board(CoorX, y) = "k" Then
                            BInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If BlackTFTable(CoorX, Math.Min(y + 1, 7)) = "T" Then BlackTFTable(CoorX, Math.Min(y + 1, 7)) = "F"
                            Exit For
                        End If
                        If Char.IsLower(Board(CoorX, y)) AndAlso Val(BKPos(0)) = CoorX AndAlso Val(BKPos(1)) > y Then
                            For m = y + 1 To 7
                                If Board(CoorX, m) = "k" Then
                                    BlackTFTable(CoorX, y) = "0"
                                    Exit For
                                End If
                                If Board(CoorX, m) <> " " Then Exit For
                            Next
                        End If
                        If Board(CoorX, y) <> " " Then Exit For
                    End If
                Next

                For y = (CoorY - 1) To 0 Step -1
                    If BlackTFTable(CoorX, y) = "T" Then BlackTFTable(CoorX, y) = "F"
                    If Char.IsUpper(Board(CoorX, y)) Then
                        Exit For
                    Else
                        If Board(CoorX, y) = "k" Then
                            BInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If BlackTFTable(CoorX, Math.Max(y - 1, 0)) = "T" Then BlackTFTable(CoorX, Math.Max(y - 1, 0)) = "F"
                            Exit For
                        End If
                        If Char.IsLower(Board(CoorX, y)) AndAlso Val(BKPos(0)) = CoorX AndAlso Val(BKPos(1)) < y Then
                            For m = y - 1 To 0 Step -1
                                If Board(CoorX, m) = "k" Then
                                    BlackTFTable(CoorX, y) = "0"
                                    Exit For
                                End If
                                If Board(CoorX, m) <> " " Then Exit For
                            Next
                        End If

                        If Board(CoorX, y) <> " " Then Exit For
                    End If
                Next
            End If


            If Board(CoorX, CoorY) = "B" OrElse Board(CoorX, CoorY) = "Q" Then 'Legal Moves for the Biship / Part of Queen.
                Dim StartX, StartY, o, p As SByte 'These represent temporary x and y coordinates, used for pin detection.
                StartX = CoorX + 1
                StartY = CoorY + 1
                Do Until StartX > 7 OrElse StartY > 7
                    If BlackTFTable(StartX, StartY) = "T" Then BlackTFTable(StartX, StartY) = "F"
                    If Char.IsUpper(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "k" Then
                            BInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If StartX < 7 AndAlso StartY < 7 Then
                                If BlackTFTable(StartX + 1, StartY + 1) = "T" Then BlackTFTable(StartX + 1, StartY + 1) = "F"
                            End If
                            Exit Do
                        End If
                        If Char.IsLower(Board(StartX, StartY)) AndAlso (Val(BKPos(0)) - CoorX = Val(BKPos(1)) - CoorY) AndAlso Val(BKPos(0)) > CoorX Then
                            o = StartX + 1
                            p = StartY + 1
                            Do Until o > 7 OrElse p > 7 'until the end of the board is reached.
                                If Board(o, p) = "k" Then
                                    BlackTFTable(StartX, StartY) = "3"
                                    Exit Do
                                End If
                                If Board(o, p) <> " " Then Exit Do
                                o += 1
                                p += 1
                            Loop
                        End If
                        If Board(StartX, StartY) <> " " Then Exit Do
                    End If
                    StartX += 1
                    StartY += 1
                Loop

                StartX = CoorX - 1
                StartY = CoorY - 1
                Do Until StartX < 0 OrElse StartY < 0
                    If BlackTFTable(StartX, StartY) = "T" Then BlackTFTable(StartX, StartY) = "F"
                    If Char.IsUpper(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "k" Then
                            BInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If StartX > 0 AndAlso StartY > 0 Then
                                If BlackTFTable(StartX - 1, StartY - 1) = "T" Then BlackTFTable(StartX - 1, StartY - 1) = "F"
                            End If
                            Exit Do
                        End If
                        If Char.IsLower(Board(StartX, StartY)) AndAlso (Val(BKPos(0)) - CoorX = Val(BKPos(1)) - CoorY) AndAlso Val(BKPos(0)) < CoorX Then
                            o = StartX - 1
                            p = StartY - 1
                            Do Until o < 0 OrElse p < 0
                                If Board(o, p) = "k" Then
                                    BlackTFTable(StartX, StartY) = "3"
                                    Exit Do
                                End If
                                If Board(o, p) <> " " Then Exit Do
                                o -= 1
                                p -= 1
                            Loop
                        End If
                        If Board(StartX, StartY) <> " " Then Exit Do
                    End If
                    StartX -= 1
                    StartY -= 1
                Loop


                StartX = CoorX + 1
                StartY = CoorY - 1
                Do Until StartX > 7 OrElse StartY < 0
                    If BlackTFTable(StartX, StartY) = "T" Then BlackTFTable(StartX, StartY) = "F"
                    If Char.IsUpper(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "k" Then
                            BInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If StartX < 7 AndAlso StartY > 0 Then
                                If BlackTFTable(StartX + 1, StartY - 1) = "T" Then BlackTFTable(StartX + 1, StartY - 1) = "F"
                            End If
                            Exit Do
                        End If
                        If Char.IsLower(Board(StartX, StartY)) AndAlso (Val(BKPos(0)) - CoorX = CoorY - Val(BKPos(1))) AndAlso Val(BKPos(0)) > CoorX Then
                            o = StartX + 1
                            p = StartY - 1
                            Do Until o > 7 OrElse p < 0
                                If Board(o, p) = "k" Then
                                    BlackTFTable(StartX, StartY) = "1"
                                    Exit Do
                                End If
                                If Board(o, p) <> " " Then Exit Do
                                o += 1
                                p -= 1
                            Loop
                        End If

                        If Board(StartX, StartY) <> " " Then Exit Do
                    End If
                    StartX += 1
                    StartY -= 1
                Loop

                StartX = CoorX - 1
                StartY = CoorY + 1
                Do Until StartX < 0 OrElse StartY > 7
                    If BlackTFTable(StartX, StartY) = "T" Then BlackTFTable(StartX, StartY) = "F"
                    If Char.IsUpper(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "k" Then
                            BInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If StartX > 0 AndAlso StartY < 7 Then
                                If BlackTFTable(StartX - 1, StartY + 1) = "T" Then BlackTFTable(StartX - 1, StartY + 1) = "F"
                            End If
                            Exit Do
                        End If
                        If Char.IsLower(Board(StartX, StartY)) AndAlso (Val(BKPos(0)) - CoorX = CoorY - Val(BKPos(1))) AndAlso Val(BKPos(0)) < CoorX Then
                            o = StartX - 1
                            p = StartY + 1
                            Do Until o < 0 OrElse p > 7
                                If Board(o, p) = "k" Then
                                    BlackTFTable(StartX, StartY) = "1"
                                    Exit Do
                                End If
                                If Board(o, p) <> " " Then Exit Do
                                o -= 1
                                p += 1
                            Loop
                        End If
                        If Board(StartX, StartY) <> " " Then Exit Do
                    End If
                    StartX -= 1
                    StartY += 1
                Loop
            End If
        End If

        'At the end of the function, the program checks to see if any piece is attacking the enemy king.
        'If it is, then the other player is signalled to be in check, and the attacking piece is recorded.
        If CheckingPiece <> " " Then
            If BInCheck.Piece = " " OrElse BInCheck.Piece = CheckingPiece Then
                BInCheck.Piece = CheckingPiece
            Else
                BInCheck.DoubleCheck = True
            End If
        End If
    End Sub

    'As the below subroutine is very similar to its equivilant 'WhitePieceLegalMoves' function, commenting will be limited.
    Public Sub BlackPieceLegalMoves(ByVal Board(,) As Char, ByVal CoorX As SByte, ByVal CoorY As SByte, ByRef WhiteTFTable(,) As Char, ByVal WKPos As String, ByRef WInCheck As InCheck, ByVal EnPassant As String)
        Dim CheckingPiece As String = " "

        If Board(CoorX, CoorY) = "p" Then 'Legal Moves for the Pawn (along with First Moves.)
            If CoorX > 0 Then
                If WhiteTFTable(CoorX - 1, CoorY + 1) = "T" Then WhiteTFTable(CoorX - 1, CoorY + 1) = "F"
                If Board(CoorX - 1, CoorY + 1) = "K" Then
                    WInCheck.IsInCheck = True
                    CheckingPiece = CoorX & CoorY
                End If
            End If
            If CoorX < 7 Then
                If WhiteTFTable(CoorX + 1, CoorY + 1) = "T" Then WhiteTFTable(CoorX + 1, CoorY + 1) = "F"
                If Board(CoorX + 1, CoorY + 1) = "K" Then
                    WInCheck.IsInCheck = True
                    CheckingPiece = CoorX & CoorY
                End If
            End If


        ElseIf Board(CoorX, CoorY) = "n" Then 'Legal Moves for the Knight. (If pinned, then it cannot move at all.)
            Dim TestMove As String
            For m = 1 To Val(KnightLegalMoveArray(CoorX, CoorY, 0)) - 1
                TestMove = KnightLegalMoveArray(CoorX, CoorY, m)
                If WhiteTFTable(Val(TestMove(0)), Val(TestMove(1))) = "T" Then WhiteTFTable(Val(TestMove(0)), Val(TestMove(1))) = "F"
                If Board(Val(TestMove(0)), Val(TestMove(1))) = "K" Then
                    WInCheck.IsInCheck = True
                    CheckingPiece = CoorX & CoorY
                End If
            Next


        ElseIf Board(CoorX, CoorY) = "k" Then 'Legal Moves for the King (Along with Castling).
            For y = Math.Max(CoorY - 1, 0) To Math.Min(CoorY + 1, 7)
                For x = Math.Max(CoorX - 1, 0) To Math.Min(CoorX + 1, 7)
                    If WhiteTFTable(x, y) = "T" Then WhiteTFTable(x, y) = "F"
                Next
            Next


        Else
            If Board(CoorX, CoorY) = "r" OrElse Board(CoorX, CoorY) = "q" Then 'Legal Moves for the Rook / Part of Queen.
                For x = (CoorX + 1) To 7
                    If WhiteTFTable(x, CoorY) = "T" Then WhiteTFTable(x, CoorY) = "F"
                    If Char.IsLower(Board(x, CoorY)) Then
                        If Board(x, CoorY) = "p" AndAlso x & CoorY - 1 = EnPassant Then
                            For m = x + 2 To 7
                                If Board(m, CoorY) = "K" Then
                                    WhiteTFTable(x + 1, CoorY) = "4"
                                    Exit For
                                End If
                                If Board(m, CoorY) <> " " Then Exit For
                            Next
                        End If
                        Exit For
                    Else
                        If Board(x, CoorY) = "K" Then
                            WInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If WhiteTFTable(Math.Min(x + 1, 7), CoorY) = "T" Then WhiteTFTable(Math.Min(x + 1, 7), CoorY) = "F"
                            Exit For
                        End If
                        If Char.IsUpper(Board(x, CoorY)) AndAlso Val(WKPos(0)) > x AndAlso Val(WKPos(1)) = CoorY Then
                            For m = x + 1 To 7
                                If Board(m, CoorY) = "K" Then
                                    WhiteTFTable(x, CoorY) = "2"
                                    Exit For
                                End If
                                If Board(m, CoorY) <> " " Then
                                    If Board(m, CoorY) = "p" AndAlso m & CoorY - 1 = EnPassant Then
                                        For a = m + 1 To 7
                                            If Board(a, CoorY) = "K" Then
                                                WhiteTFTable(x, CoorY) = "4"
                                                Exit For
                                            End If
                                            If Board(a, CoorY) <> " " Then Exit For
                                        Next
                                    End If
                                    Exit For
                                End If
                            Next
                        End If
                        If Board(x, CoorY) <> " " Then Exit For
                    End If
                Next

                For x = (CoorX - 1) To 0 Step -1
                    If WhiteTFTable(x, CoorY) = "T" Then WhiteTFTable(x, CoorY) = "F"
                    If Char.IsLower(Board(x, CoorY)) Then
                        If Board(x, CoorY) = "p" AndAlso x & CoorY - 1 = EnPassant Then
                            For m = x - 2 To 0 Step -1
                                If Board(m, CoorY) = "K" Then
                                    WhiteTFTable(x - 1, CoorY) = "4"
                                    Exit For
                                End If
                                If Board(m, CoorY) <> " " Then Exit For
                            Next
                        End If
                        Exit For
                    Else
                        If Board(x, CoorY) = "K" Then
                            WInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If WhiteTFTable(Math.Max(x - 1, 0), CoorY) = "T" Then WhiteTFTable(Math.Max(x - 1, 0), CoorY) = "F"
                            Exit For
                        End If
                        If Char.IsUpper(Board(x, CoorY)) AndAlso Val(WKPos(0)) < x AndAlso Val(WKPos(1)) = CoorY Then
                            For m = x - 1 To 0 Step -1
                                If Board(m, CoorY) = "K" Then
                                    WhiteTFTable(x, CoorY) = "2"
                                    Exit For
                                End If
                                If Board(m, CoorY) <> " " Then
                                    If Board(m, CoorY) = "p" AndAlso m & CoorY - 1 = EnPassant Then
                                        For a = m - 1 To 0 Step -1
                                            If Board(a, CoorY) = "K" Then
                                                WhiteTFTable(x, CoorY) = "4"
                                                Exit For
                                            End If
                                            If Board(a, CoorY) <> " " Then Exit For
                                        Next
                                    End If
                                    Exit For
                                End If
                            Next
                        End If
                        If Board(x, CoorY) <> " " Then Exit For
                    End If
                Next

                For y = (CoorY + 1) To 7
                    If WhiteTFTable(CoorX, y) = "T" Then WhiteTFTable(CoorX, y) = "F"
                    If Char.IsLower(Board(CoorX, y)) Then
                        Exit For
                    Else
                        If Board(CoorX, y) = "K" Then
                            WInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If WhiteTFTable(CoorX, Math.Min(y + 1, 7)) = "T" Then WhiteTFTable(CoorX, Math.Min(y + 1, 7)) = "F"
                            Exit For
                        End If
                        If Char.IsUpper(Board(CoorX, y)) AndAlso Val(WKPos(0)) = CoorX AndAlso Val(WKPos(1)) > y Then
                            For m = y + 1 To 7
                                If Board(CoorX, m) = "K" Then
                                    WhiteTFTable(CoorX, y) = "0"
                                    Exit For
                                End If
                                If Board(CoorX, m) <> " " Then Exit For
                            Next
                        End If
                        If Board(CoorX, y) <> " " Then Exit For
                    End If
                Next

                For y = (CoorY - 1) To 0 Step -1
                    If WhiteTFTable(CoorX, y) = "T" Then WhiteTFTable(CoorX, y) = "F"
                    If Char.IsLower(Board(CoorX, y)) Then
                        Exit For
                    Else
                        If Board(CoorX, y) = "K" Then
                            WInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If WhiteTFTable(CoorX, Math.Max(y - 1, 0)) = "T" Then WhiteTFTable(CoorX, Math.Max(y - 1, 0)) = "F"
                            Exit For
                        End If
                        If Char.IsUpper(Board(CoorX, y)) AndAlso Val(WKPos(0)) = CoorX AndAlso Val(WKPos(1)) < y Then
                            For m = y - 1 To 0 Step -1
                                If Board(CoorX, m) = "K" Then
                                    WhiteTFTable(CoorX, y) = "0"
                                    Exit For
                                End If
                                If Board(CoorX, m) <> " " Then Exit For
                            Next
                        End If
                        If Board(CoorX, y) <> " " Then Exit For
                    End If
                Next
            End If


            If Board(CoorX, CoorY) = "b" OrElse Board(CoorX, CoorY) = "q" Then 'Legal Moves for the Bishop / Part of Queen.
                Dim StartX, StartY, o, p As SByte 'These represent temporary x and y coordinates, used for pin detection.
                StartX = CoorX + 1
                StartY = CoorY + 1
                Do Until StartX > 7 OrElse StartY > 7
                    If WhiteTFTable(StartX, StartY) = "T" Then WhiteTFTable(StartX, StartY) = "F"
                    If Char.IsLower(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "K" Then
                            WInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If StartX < 7 AndAlso StartY < 7 Then
                                If WhiteTFTable(StartX + 1, StartY + 1) = "T" Then WhiteTFTable(StartX + 1, StartY + 1) = "F"
                            End If
                            Exit Do
                        End If
                        If Char.IsUpper(Board(StartX, StartY)) AndAlso (Val(WKPos(0)) - CoorX = Val(WKPos(1)) - CoorY) AndAlso Val(WKPos(0)) > CoorX Then
                            o = StartX + 1
                            p = StartY + 1
                            Do Until o > 7 OrElse p > 7
                                If Board(o, p) = "K" Then
                                    WhiteTFTable(StartX, StartY) = "3"
                                    Exit Do
                                End If
                                If Board(o, p) <> " " Then Exit Do
                                o += 1
                                p += 1
                            Loop
                        End If
                        If Board(StartX, StartY) <> " " Then Exit Do
                    End If
                    StartX += 1
                    StartY += 1
                Loop

                StartX = CoorX - 1
                StartY = CoorY - 1
                Do Until StartX < 0 OrElse StartY < 0
                    If WhiteTFTable(StartX, StartY) = "T" Then WhiteTFTable(StartX, StartY) = "F"
                    If Char.IsLower(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "K" Then
                            WInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If StartX > 0 AndAlso StartY > 0 Then
                                If WhiteTFTable(StartX - 1, StartY - 1) = "T" Then WhiteTFTable(StartX - 1, StartY - 1) = "F"
                            End If
                            Exit Do
                        End If
                        If Char.IsUpper(Board(StartX, StartY)) AndAlso (Val(WKPos(0)) - CoorX = Val(WKPos(1)) - CoorY) AndAlso Val(WKPos(0)) < CoorX Then
                            o = StartX - 1
                            p = StartY - 1
                            Do Until o < 0 OrElse p < 0
                                If Board(o, p) = "K" Then
                                    WhiteTFTable(StartX, StartY) = "3"
                                    Exit Do
                                End If
                                If Board(o, p) <> " " Then Exit Do
                                o -= 1
                                p -= 1
                            Loop
                        End If
                        If Board(StartX, StartY) <> " " Then Exit Do
                    End If
                    StartX -= 1
                    StartY -= 1
                Loop

                StartX = CoorX + 1
                StartY = CoorY - 1
                Do Until StartX > 7 OrElse StartY < 0
                    If WhiteTFTable(StartX, StartY) = "T" Then WhiteTFTable(StartX, StartY) = "F"
                    If Char.IsLower(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "K" Then
                            WInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If StartX < 7 AndAlso StartY > 0 Then
                                If WhiteTFTable(StartX + 1, StartY - 1) = "T" Then WhiteTFTable(StartX + 1, StartY - 1) = "F"
                            End If
                            Exit Do
                        End If
                        If Char.IsUpper(Board(StartX, StartY)) AndAlso (Val(WKPos(0)) - CoorX = CoorY - Val(WKPos(1))) AndAlso Val(WKPos(0)) > CoorX Then
                            o = StartX + 1
                            p = StartY - 1
                            Do Until o > 7 OrElse p < 0
                                If Board(o, p) = "K" Then
                                    WhiteTFTable(StartX, StartY) = "1"
                                    Exit Do
                                End If
                                If Board(o, p) <> " " Then Exit Do
                                o += 1
                                p -= 1
                            Loop
                        End If
                        If Board(StartX, StartY) <> " " Then Exit Do
                    End If
                    StartX += 1
                    StartY -= 1
                Loop

                StartX = CoorX - 1
                StartY = CoorY + 1
                Do Until StartX < 0 OrElse StartY > 7
                    If WhiteTFTable(StartX, StartY) = "T" Then WhiteTFTable(StartX, StartY) = "F"
                    If Char.IsLower(Board(StartX, StartY)) Then
                        Exit Do
                    Else
                        If Board(StartX, StartY) = "K" Then
                            WInCheck.IsInCheck = True
                            CheckingPiece = CoorX & CoorY
                            If StartX > 0 AndAlso StartY < 7 Then
                                If WhiteTFTable(StartX - 1, StartY + 1) = "T" Then WhiteTFTable(StartX - 1, StartY + 1) = "F"
                            End If
                            Exit Do
                        End If
                        If Char.IsUpper(Board(StartX, StartY)) AndAlso (Val(WKPos(0)) - CoorX = CoorY - Val(WKPos(1))) AndAlso Val(WKPos(0)) < CoorX Then
                            o = StartX - 1
                            p = StartY + 1
                            Do Until o < 0 OrElse p > 7
                                If Board(o, p) = "K" Then
                                    WhiteTFTable(StartX, StartY) = "1"
                                    Exit Do
                                End If
                                If Board(o, p) <> " " Then Exit Do
                                o -= 1
                                p += 1
                            Loop
                        End If
                        If Board(StartX, StartY) <> " " Then Exit Do
                    End If
                    StartX -= 1
                    StartY += 1
                Loop
            End If
        End If

        'At the end of the function, the program checks to see if any piece is attacking the enemy king.
        'If it is, then the other player is signalled to be in check, and the attacking piece is recorded.
        If CheckingPiece <> " " Then
            If WInCheck.Piece = " " OrElse WInCheck.Piece = CheckingPiece Then
                WInCheck.Piece = CheckingPiece
            Else
                WInCheck.DoubleCheck = True
            End If
        End If
    End Sub

End Class
