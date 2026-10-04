'This class contains my Chess AI, which involves the MiniMax algorithm with Alpha-Beta Pruning, Move ordering,
'Quiescence (optional), along with the ability to check for end states. This class is modular from my Chess class - being
'constructed only from the FEN position, and only returning a Move (see the structure below). Some other interacting is done,
'however, such as allowing the AI to be remotely aborted.
Imports System.Formats.Tar

Public Class AI 'i shall thy the Alfie Alphafish (bit optimistic, I know).
    Inherits CoreMethods
    Private Shared HasBeenInstantiated As Boolean 'The AI will not perform methods if it has not been fully instantiated with a FEN.
    'Below are the details that the AI requires for a search. Please see their counterparts in the Chess form for their info.
    Private Shared PrimaryBoard(7, 7), PrimaryTFTable(7, 7) As Char
    Private Shared PrimaryMeCanCastle, PrimaryEnemyCanCastle As New CanCastle
    Private Shared PrimaryMeInCheck As New InCheck
    Private Shared PrimaryMeKPos, PrimaryEnemyKPos As String
    Private Shared PrimaryEnPassant As String
    Private Shared PrimaryMaterialCount(1) As SByte
    Private Shared PlayerTurn As Boolean

    Private Shared KingSymbol As Char '"K" for white, "k" for black. Used for helping to resolve checks.
    Private UseQuiescence As Boolean 'Set by the user - determines whether the AI will use the Quiescence algorithm.
    Private TotalPositionsSearched As UInt32
    Public ABORT As Boolean 'Controlled by the thread handlers - an AI is aborted if another AI has
    'found a mating pattern, or if the AI has ran out of time.

    'Constructor methods.
    Public Sub New()
        'AI variables instantiated but without a FEN. Used when the program is first booted up.
    End Sub
    Public Sub New(ByVal FEN As String)
        'Configures the AI using a given FEN.
        Reconfigure(FEN)
    End Sub

    'Subroutine which converts a user's input FEN into all the details needed to conduct a MiniMax search.
    Public Sub Reconfigure(ByVal FEN As String, Optional ByVal ResetTT As Boolean = False)
        'Converts the user's FEN into a board position, then resets checking rules.
        PrimaryBoard = FENConverter(FEN, PrimaryMeCanCastle, PrimaryEnemyCanCastle, PrimaryMeKPos, PrimaryEnemyKPos, PrimaryEnPassant, PlayerTurn)
        PrimaryMeInCheck.NotInCheck()
        If PlayerTurn Then
            'Creates the TFTable for the white pieces, then creates king symbol.
            FixTFTables(PrimaryBoard, True, PrimaryTFTable, PrimaryMeKPos, PrimaryMeInCheck, NotInCheck, PrimaryEnPassant)
            KingSymbol = "K"
        Else
            'Swaps the Primary & Enemy Castling privileges, along with the Primary & Enemy King privileges.
            'This is because, for the AI, all variables are in context of which player the AI is controlling,
            'so 'me' and 'enemy' can refer to different colours depending on the board position.
            Dim TempCanCastle As New CanCastle
            TempCanCastle.CopyFrom(PrimaryMeCanCastle)
            PrimaryMeCanCastle.CopyFrom(PrimaryEnemyCanCastle)
            PrimaryEnemyCanCastle.CopyFrom(TempCanCastle)
            Dim TempKPos As String = PrimaryMeKPos
            PrimaryMeKPos = PrimaryEnemyKPos
            PrimaryEnemyKPos = TempKPos
            'Creates the TFTable for the black pieces, then creates king symbol.
            FixTFTables(PrimaryBoard, False, PrimaryTFTable, PrimaryMeKPos, NotInCheck, PrimaryMeInCheck, PrimaryEnPassant)
            KingSymbol = "k"
        End If
        'Finds the material count of the board.
        PrimaryMaterialCount = CountMaterial(PrimaryBoard)
        HasBeenInstantiated = True
    End Sub


    'Subroutine that adds the Board History to the Transposition Table.
    Public Sub AddBoardHistory(ByVal BoardHistory() As UInt64)
    End Sub
    Public Function GetVersion() As String
        Return GlobalConstants.ProgramVersion
    End Function
    Public Function GetBoard() As Char(,)
        Return PrimaryBoard
    End Function
    Public Function GetMaterialCount() As Integer()
        Return New Integer() {PrimaryMaterialCount(0), PrimaryMaterialCount(1)}
    End Function
    Public Function GetZobristValue() As ULong
    End Function
    Public Function GetWhiteCanCastle() As CanCastle
        Return If(PlayerTurn, PrimaryMeCanCastle, PrimaryEnemyCanCastle)
    End Function
    Public Function GetBlackCanCastle() As CanCastle
        Return If(PlayerTurn, PrimaryEnemyCanCastle, PrimaryMeCanCastle)
    End Function
    Public Function GetWhiteKPos() As String
        Return If(PlayerTurn, PrimaryMeKPos, PrimaryEnemyKPos)
    End Function
    Public Function GetBlackKPos() As String
        Return If(PlayerTurn, PrimaryEnemyKPos, PrimaryMeKPos)
    End Function
    Public Function GetMeInCheck() As InCheck
        Return PrimaryMeInCheck
    End Function
    Public Function GetEnPassant() As String
        Return PrimaryEnPassant
    End Function
    Public Sub ABORTSearch()
        ABORT = True
    End Sub
    Public Function GetABORTState() As Boolean
        Return ABORT
    End Function
    Public Function OutputMoveInfo(ByVal BestMove As Move, Optional ByVal OnlyReturnPGN As Boolean = False) As String
        If HasBeenInstantiated Then
            Dim PGNMove As String = MoveConverter(PrimaryBoard, BestMove, PlayerTurn)
            If OnlyReturnPGN Then Return PGNMove
            Console.Write("Move = " & PGNMove)
            If Math.Abs(BestMove.Score) = 299.99 Then Console.Write("#")
            Console.Write(", with Evaluation: ")
            'Colours BestMove.Score depending on whether the AI thinks it is winning, drawing, or losing.
            Select Case BestMove.Score
                Case > 0
                    Console.ForegroundColor = ConsoleColor.Green
                    'As NegaMax moves are relative to the player to move, reformat them in an asymmetric way (easier for the user to interpret).
                    Console.Write(If(PlayerTurn, "+", "-"))
                Case = 0
                    Console.ForegroundColor = ConsoleColor.White
                    Console.Write("0.")
                Case < 0
                    Console.ForegroundColor = ConsoleColor.Red
                    Console.Write(If(PlayerTurn, "-", "+"))
            End Select
            Console.WriteLine(Math.Abs(BestMove.Score))
            Console.ForegroundColor = ConsoleColor.White
        End If
        Return Nothing
    End Function
    Public Sub ConfigureSettings(ByVal UserSearchSettings As AISearchSettings, ByVal ResetTT As Boolean)
    End Sub
    'Function that accepts a move, and returns the FEN of the position that would be after making that move on the board.
    Public Function ReturnFENAfterMove(ByVal TempMove As Move) As String
        If HasBeenInstantiated Then
            'Creates temporary variables of each of the main board controls, so that we can make this temporary move.
            Dim TempBoard(7, 7) As Char
            Dim TempMeCanCastle As New CanCastle
            Dim TempEnemyCanCastle As New CanCastle
            Dim TempEnPassant As String = PrimaryEnPassant
            'Copies primary assets to temporary assets, then makes the move on the temporary board.
            Array.Copy(PrimaryBoard, TempBoard, 64)
            TempMeCanCastle.CopyFrom(PrimaryMeCanCastle)
            TempEnemyCanCastle.CopyFrom(PrimaryEnemyCanCastle)
            Try
                MakeMove(TempBoard, TempMove.OldMoveX, TempMove.OldMoveY, TempMove.NewMoveX, TempMove.NewMoveY, TempMeCanCastle, "00", {0, 0}, TempEnPassant)
                'Returns this new FEN.
                Return ConvertToFEN(TempBoard, If(PlayerTurn, TempMeCanCastle, TempEnemyCanCastle), If(PlayerTurn, TempEnemyCanCastle, TempMeCanCastle), TempEnPassant, Not PlayerTurn)
            Catch ex As Exception
                'The move is not valid on this position - there must be some error.
                Console.ForegroundColor = ConsoleColor.DarkRed
                Console.WriteLine("Error: Move is invalid in this Position.")
                Console.ForegroundColor = ConsoleColor.White
            End Try
        End If
        Return Nothing
    End Function




    'Algorithm that finds the AI's 'Best Move' for a given position, using the MiniMax algorithm.
    Public Function Search(ByVal Depth As Integer, Optional ByVal UserQuiescence As Boolean = True) As Move
        Dim BestMove As New Move
        BestMove.Score = Integer.MinValue
        If HasBeenInstantiated AndAlso Depth > 0 Then
            UseQuiescence = UserQuiescence
            'Creates alpha (white's best move) and beta (black's best move).
            Dim CurrentScore As Decimal
            Dim Alpha As Decimal = Integer.MinValue
            Dim Beta As Decimal = Integer.MaxValue
            ABORT = False
            TotalPositionsSearched = 0
            'Creates the pseudo-legal moves for the chosen player.
            Dim PieceMoves(200, 2) As String
            If PlayerTurn Then
                PieceMoves = CreateMoves(PrimaryBoard, True, PrimaryTFTable, PrimaryEnemyKPos, PrimaryMeInCheck, NotInCheck, PrimaryMeCanCastle, PrimaryEnPassant, False)
            Else
                PieceMoves = CreateMoves(PrimaryBoard, False, PrimaryTFTable, PrimaryEnemyKPos, NotInCheck, PrimaryMeInCheck, PrimaryMeCanCastle, PrimaryEnPassant, False)
            End If
            If PieceMoves(0, 0) > 0 Then 'If any move exists...
                'Creates temp variables.
                Dim TempBoard(7, 7) As Char
                Dim TempMaterialCount(1) As SByte
                Dim TempMeKPos As String
                Dim TempMeCanCastle As New CanCastle
                Dim TempMeInCheck As New InCheck
                Dim TempEnPassant As String
                For n = 1 To CInt(PieceMoves(0, 0)) 'for each move...
                    If ABORT Then Exit For
                    Array.Copy(PrimaryBoard, TempBoard, 64)
                    'If the user is in check, eliminate the moves that do not escape check. King moves are not considered
                    'as the player's TFTable will ensure that all king moves are legal.
                    If PrimaryMeInCheck.IsInCheck AndAlso TempBoard(PieceMoves(n, 0), PieceMoves(n, 1)) <> KingSymbol Then
                        'Runs move through the DoesMoveResolveCheck algorithm.
                        TempMeInCheck.CopyFrom(PrimaryMeInCheck)
                        If PlayerTurn AndAlso DoesMoveResolveCheck(TempBoard, PieceMoves(n, 0), PieceMoves(n, 1), PieceMoves(n, 2)(0), PieceMoves(n, 2)(1), TempMeInCheck, NotInCheck, PrimaryEnPassant) Then
                            'Move has resolved check.
                            TempMeInCheck.NotInCheck()
                        ElseIf Not PlayerTurn AndAlso DoesMoveResolveCheck(TempBoard, PieceMoves(n, 0), PieceMoves(n, 1), PieceMoves(n, 2)(0), PieceMoves(n, 2)(1), NotInCheck, TempMeInCheck, PrimaryEnPassant) Then
                            'Move has resolved check.
                            TempMeInCheck.NotInCheck()
                        End If
                    Else
                        TempMeInCheck.IsInCheck = False
                    End If
                    If Not TempMeInCheck.IsInCheck Then 'Therefore, is a legal move.
                        'Copies board info to temp variables.
                        Array.Copy(PrimaryMaterialCount, TempMaterialCount, 2)
                        TempMeKPos = PrimaryMeKPos
                        TempMeCanCastle.CopyFrom(PrimaryMeCanCastle)
                        TempEnPassant = PrimaryEnPassant
                        'Makes move on temp board, then calls MiniMax for this new position.
                        MakeMove(TempBoard, PieceMoves(n, 0), PieceMoves(n, 1), PieceMoves(n, 2)(0), PieceMoves(n, 2)(1), TempMeCanCastle, TempMeKPos, TempMaterialCount, TempEnPassant)
                        If Depth = 1 AndAlso Not UseQuiescence Then
                            'We have reached a leaf position - return the evaluation for this position.
                            TotalPositionsSearched += 1
                            If PlayerTurn Then
                                CurrentScore = Evaluate(TempMaterialCount, TempMeKPos, PrimaryEnemyKPos)
                            Else
                                CurrentScore = Evaluate(TempMaterialCount, PrimaryEnemyKPos, TempMeKPos)
                            End If
                        ElseIf PlayerTurn Then
                            CurrentScore = MiniMax(TempBoard, Depth - 1, False, TempMeCanCastle, PrimaryEnemyCanCastle, TempMeKPos, PrimaryEnemyKPos, TempEnPassant, TempMaterialCount, Alpha, Beta)
                        Else
                            'Alpha & Beta are replaced with -Beta & -Alpha so that all moves' scores are given in context
                            'of the player to move. This is so that the largest number will always be the best score.
                            CurrentScore = -MiniMax(TempBoard, Depth - 1, True, PrimaryEnemyCanCastle, TempMeCanCastle, PrimaryEnemyKPos, TempMeKPos, TempEnPassant, TempMaterialCount, -Beta, -Alpha)
                        End If
                        Alpha = Math.Max(Alpha, CurrentScore)
                        If CurrentScore > BestMove.Score Then
                            'Move has been beaten - replace it.
                            BestMove.Score = CurrentScore
                            BestMove.OldMoveX = PieceMoves(n, 0)
                            BestMove.OldMoveY = PieceMoves(n, 1)
                            BestMove.NewMoveX = PieceMoves(n, 2)(0)
                            BestMove.NewMoveY = PieceMoves(n, 2)(1)
                        End If
                    End If
                Next
            End If
            If ABORT Then
                'Search has been terminated / no move found - make a note on the Move.
                BestMove.Code = "a"
            Else 'Search successfully completed.
                If Not PlayerTurn Then BestMove.Score = -BestMove.Score
                'Console.WriteLine("Depth Of " & Depth & " Completed. Move = " & MoveConverter(PrimaryBoard, BestMove, PrimaryEnPassant) & ", with Evaluation = " & BestMove.Score & vbCrLf & "Positions searched = " & TotalPositionsSearched & vbCr)
            End If
        Else 'AI not correctly instantiated.
            Console.WriteLine("Error when Attempting Search - FEN Position not set / Depth set too low.")
            BestMove.Code = "a"
        End If
        Return BestMove
    End Function
    Public Function Search(ByVal Depth As Integer, ByVal PreviousBestMove As Move) As Move
        Return Search(Depth)
    End Function

    'Function that returns all the legal moves of a given piece on the board.
    'Used for when the user is attempting to move a piece on the GUI.
    Public Function ReturnPiecesLegalMoves(ByVal CoorX As String, ByVal CoorY As String) As String()
        Dim LegalMoves(27) As String
        If HasBeenInstantiated Then
            Dim TotalMoves As Byte = 0
            'Creates the pseudo-legal moves for the chosen player.
            Dim PieceMoves(27) As String
            If PlayerTurn AndAlso Char.IsUpper(PrimaryBoard(CoorX, CoorY)) Then
                PieceMoves = WhitePieceLegalMoves(PrimaryBoard, CoorX, CoorY, PrimaryTFTable, TrueTable, PrimaryEnemyKPos, NotInCheck, PrimaryMeInCheck, PrimaryMeCanCastle, PrimaryEnPassant)
            ElseIf Not (PlayerTurn OrElse Char.IsUpper(PrimaryBoard(CoorX, CoorY))) Then
                PieceMoves = BlackPieceLegalMoves(PrimaryBoard, CoorX, CoorY, PrimaryTFTable, TrueTable, PrimaryEnemyKPos, NotInCheck, PrimaryMeInCheck, PrimaryMeCanCastle, PrimaryEnPassant)
            Else
                Console.WriteLine("Error - Illegal to Move Piece.")
                Return Nothing
            End If
            If PieceMoves(0) IsNot Nothing Then 'If any move exists...
                Dim TempMeInCheck As New InCheck 'Creates temp check class.
                For n = 1 To CInt(PieceMoves(0)) - 1 'for each move...
                    'If the user is in check, eliminate the moves that do not escape check. King moves are not considered
                    'as the player's TFTable will ensure that all king moves are legal.
                    If PrimaryMeInCheck.IsInCheck AndAlso PrimaryBoard(CoorX, CoorY) <> KingSymbol Then
                        'Runs move through the DoesMoveResolveCheck algorithm.
                        TempMeInCheck.CopyFrom(PrimaryMeInCheck)
                        If PlayerTurn AndAlso DoesMoveResolveCheck(PrimaryBoard, CoorX, CoorY, CStr(PieceMoves(n)(0)), CStr(PieceMoves(n)(1)), TempMeInCheck, NotInCheck, PrimaryEnPassant) Then
                            'Move has resolved check.
                            TempMeInCheck.NotInCheck()
                        ElseIf Not PlayerTurn AndAlso DoesMoveResolveCheck(PrimaryBoard, CoorX, CoorY, CStr(PieceMoves(n)(0)), CStr(PieceMoves(n)(1)), NotInCheck, TempMeInCheck, PrimaryEnPassant) Then
                            'Move has resolved check.
                            TempMeInCheck.NotInCheck()
                        End If
                    Else
                        TempMeInCheck.IsInCheck = False
                    End If
                    If Not TempMeInCheck.IsInCheck Then
                        'The move is legal - update the list of the piece's legal moves.
                        TotalMoves += 1
                        LegalMoves(TotalMoves) = PieceMoves(n)
                    End If
                Next
            End If
            'Make the first index of the array to be the total amount of legal moves.
            'This helps know how many indexes of the array to check when looking for legal moves.
            LegalMoves(0) = TotalMoves
        Else 'AI not correctly instantiated.
            Console.WriteLine("Error when Attempting Search - FEN Position not set.")
        End If
        Return LegalMoves
    End Function

    'Function that scans the position for End States - positions where the game must terminate.
    Public Function CheckForEndState() As Move
        Dim CurrentMove As New Move
        If HasBeenInstantiated Then
            CurrentMove.Code = "c" 'Result defaults to checkmate unless proven otherwise.
            Dim TotalMoves As Byte = 0
            'Creates the pseudo-legal moves for the chosen player.
            Dim PieceMoves(200, 2) As String
            If PlayerTurn Then
                PieceMoves = CreateMoves(PrimaryBoard, True, PrimaryTFTable, PrimaryEnemyKPos, PrimaryMeInCheck, NotInCheck, PrimaryMeCanCastle, PrimaryEnPassant, False)
            Else
                PieceMoves = CreateMoves(PrimaryBoard, False, PrimaryTFTable, PrimaryEnemyKPos, NotInCheck, PrimaryMeInCheck, PrimaryMeCanCastle, PrimaryEnPassant, False)
            End If
            If PieceMoves(0, 0) > 0 Then 'If any move exists...
                Dim TempMeInCheck As New InCheck 'Creates temp check class.
                For n = 1 To CInt(PieceMoves(0, 0)) 'for each move...
                    'If the user is in check, eliminate the moves that do not escape check. King moves are not considered
                    'as the player's TFTable will ensure that all king moves are legal.
                    If PrimaryMeInCheck.IsInCheck AndAlso PrimaryBoard(PieceMoves(n, 0), PieceMoves(n, 1)) <> KingSymbol Then
                        'Runs move through the DoesMoveResolveCheck algorithm.
                        TempMeInCheck.CopyFrom(PrimaryMeInCheck)
                        If PlayerTurn AndAlso DoesMoveResolveCheck(PrimaryBoard, PieceMoves(n, 0), PieceMoves(n, 1), PieceMoves(n, 2)(0), PieceMoves(n, 2)(1), TempMeInCheck, NotInCheck, PrimaryEnPassant) Then
                            'Move has resolved check.
                            TempMeInCheck.NotInCheck()
                        ElseIf Not PlayerTurn AndAlso DoesMoveResolveCheck(PrimaryBoard, PieceMoves(n, 0), PieceMoves(n, 1), PieceMoves(n, 2)(0), PieceMoves(n, 2)(1), NotInCheck, TempMeInCheck, PrimaryEnPassant) Then
                            'Move has resolved check.
                            TempMeInCheck.NotInCheck()
                        End If
                    Else
                        TempMeInCheck.IsInCheck = False
                    End If
                    If Not TempMeInCheck.IsInCheck Then
                        'Therefore is a legal move. This means that there is no checkmate / stalemate.
                        TotalMoves += 1
                        If TotalMoves > 1 Then
                            'Multiple moves detected/
                            CurrentMove.Code = "f"
                            Return CurrentMove
                        Else 'Only one (forced) move detected - note Move info.
                            CurrentMove.OldMoveX = PieceMoves(n, 0)
                            CurrentMove.OldMoveY = PieceMoves(n, 1)
                            CurrentMove.NewMoveX = PieceMoves(n, 2)(0)
                            CurrentMove.NewMoveY = PieceMoves(n, 2)(1)
                            CurrentMove.Code = "o"
                        End If
                    End If
                Next
            ElseIf Not PrimaryMeInCheck.IsInCheck Then
                'Position is a stalemate.
                CurrentMove.Code = "s"
            End If
        Else 'AI not correctly instantiated.
            Console.WriteLine("Error when Attempting Search - FEN Position not set.")
            CurrentMove.Code = "a"
        End If
        Return CurrentMove
    End Function



    'Function which creates and orders all the pseudo-legal moves a player can make, given certain criteria.
    Private Function CreateMoves(ByVal Board(,) As Char, ByVal isWhite As Boolean, ByVal TrueFalseTable(,) As Char, ByVal KPos As String, ByVal WInCheck As InCheck, ByVal BInCheck As InCheck, ByVal CanCastle As CanCastle, ByVal EnPassant As String, ByVal OnlyCaptures As Boolean) As String(,)
        Dim PieceMoves(27) As String
        'Creates category arrays, along with their lengths variables.
        Dim AmazingCaptureMoves(200, 2) As String
        Dim CaptureMoves(19, 2) As String
        Dim OtherCaptureMoves(19, 2) As String
        Dim PawnPromotionMoves(7, 2) As String
        Dim GoodMoves(49, 2) As String
        Dim OtherMoves(99, 2) As String
        Dim BadMoves(49, 2) As String
        Dim TerribleMoves(24, 2) As String

        Dim ArrLens(7) As Byte 'Represents the number of moves in each move category array.
        ArrLens(0) = 1

        'dx and dy represent the horizontal & vertical distances between the piece and the enemy king.
        Dim dx, dy As Byte
        Dim PieceValueDif As SByte 'Difference in weight between the capturing piece, and the piece being captured.
        If isWhite Then
            For y = 0 To 7
                For x = 0 To 7
                    If Char.IsUpper(Board(x, y)) Then
                        'Generates the moves that the piece can make.
                        PieceMoves = WhitePieceLegalMoves(Board, x, y, TrueFalseTable, TrueTable, KPos, BInCheck, WInCheck, CanCastle, EnPassant)
                        If PieceMoves(0) IsNot Nothing Then 'If there are any moves...
                            For n = 1 To CInt(PieceMoves(0)) - 1 'for each move...
                                If Board(CStr(PieceMoves(n)(0)), CStr(PieceMoves(n)(1))) <> " " Then '= capture move.
                                    'Gets the difference in weight between the capturing piece, and the captured piece.
                                    PieceValueDif = ReturnPieceValue(Board(CStr(PieceMoves(n)(0)), CStr(PieceMoves(n)(1)))) - ReturnPieceValue(Board(x, y))
                                    If PieceValueDif >= 0 Then 'Capturing piece weighs less than captured piece.
                                        'Ammend move list.
                                        AmazingCaptureMoves(ArrLens(0), 0) = x
                                        AmazingCaptureMoves(ArrLens(0), 1) = y
                                        AmazingCaptureMoves(ArrLens(0), 2) = PieceMoves(n)
                                        ArrLens(0) += 1
                                    ElseIf PieceValueDif = 0 Then 'Capturing piece weighs the same as captured piece.
                                        'Ammend move list.
                                        CaptureMoves(ArrLens(1), 0) = x
                                        CaptureMoves(ArrLens(1), 1) = y
                                        CaptureMoves(ArrLens(1), 2) = PieceMoves(n)
                                        ArrLens(1) += 1
                                    Else 'Capturing piece weighs more than captured piece.
                                        'Ammend move list.
                                        OtherCaptureMoves(ArrLens(2), 0) = x
                                        OtherCaptureMoves(ArrLens(2), 1) = y
                                        OtherCaptureMoves(ArrLens(2), 2) = PieceMoves(n)
                                        ArrLens(2) += 1
                                    End If
                                ElseIf Not OnlyCaptures Then 'Non-Capture moves are not considered when Quiescence mode has been activated.
                                    'Finds distances between piece & enemy piece.
                                    dx = Math.Abs(CStr(KPos(0)) - CStr(PieceMoves(n)(0)))
                                    dy = Math.Abs(CStr(KPos(1)) - CStr(PieceMoves(n)(1)))
                                    If PieceMoves(n)(1) = "0" AndAlso Board(x, y) = "P" Then 'User is promoting a pawn.
                                        'Ammend move list.
                                        PawnPromotionMoves(ArrLens(3), 0) = x
                                        PawnPromotionMoves(ArrLens(3), 1) = y
                                        PawnPromotionMoves(ArrLens(3), 2) = PieceMoves(n)
                                        ArrLens(3) += 1
                                    ElseIf Board(Math.Max(CStr(PieceMoves(n)(0)) - 1, 0), Math.Max(CStr(PieceMoves(n)(1)) - 1, 0)) = "p" OrElse Board(Math.Min(CStr(PieceMoves(n)(0)) + 1, 7), Math.Max(CStr(PieceMoves(n)(1)) - 1, 0)) = "p" Then
                                        'New square is controlled by an enemy pawn - ammend move list.
                                        TerribleMoves(ArrLens(7), 0) = x
                                        TerribleMoves(ArrLens(7), 1) = y
                                        TerribleMoves(ArrLens(7), 2) = PieceMoves(n)
                                        ArrLens(7) += 1
                                    ElseIf TrueFalseTable(CStr(PieceMoves(n)(0)), CStr(PieceMoves(n)(1))) = "F" Then
                                        'Piece is positioned on a "False" on the TFTable, meaning the square is controlled by an enemy piece.
                                        'Ammend move list.
                                        BadMoves(ArrLens(6), 0) = x
                                        BadMoves(ArrLens(6), 1) = y
                                        BadMoves(ArrLens(6), 2) = PieceMoves(n)
                                        ArrLens(6) += 1
                                    ElseIf (Board(x, y) = "R" OrElse Board(x, y) = "B" OrElse Board(x, y) = "Q") AndAlso (dx = 0 OrElse dy = 0 OrElse dx = dy) Then
                                        'Piece may influece king's motion - leading to a possible check. Ammend move list.
                                        GoodMoves(ArrLens(4), 0) = x
                                        GoodMoves(ArrLens(4), 1) = y
                                        GoodMoves(ArrLens(4), 2) = PieceMoves(n)
                                        ArrLens(4) += 1
                                    Else 'Is a regular move. Ammend move list.
                                        OtherMoves(ArrLens(5), 0) = x
                                        OtherMoves(ArrLens(5), 1) = y
                                        OtherMoves(ArrLens(5), 2) = PieceMoves(n)
                                        ArrLens(5) += 1
                                    End If
                                End If
                            Next
                        End If
                    End If
                Next
            Next
        Else 'Identical code for the black pieces.
            For y = 7 To 0 Step -1
                For x = 0 To 7
                    If Char.IsLower(Board(x, y)) Then
                        PieceMoves = BlackPieceLegalMoves(Board, x, y, TrueFalseTable, TrueTable, KPos, WInCheck, BInCheck, CanCastle, EnPassant)
                        If PieceMoves(0) IsNot Nothing Then
                            For n = 1 To CInt(PieceMoves(0)) - 1
                                If Board(CStr(PieceMoves(n)(0)), CStr(PieceMoves(n)(1))) <> " " Then
                                    PieceValueDif = ReturnPieceValue(Board(CStr(PieceMoves(n)(0)), CStr(PieceMoves(n)(1)))) - ReturnPieceValue(Board(x, y))
                                    If PieceValueDif >= 0 Then
                                        AmazingCaptureMoves(ArrLens(0), 0) = x
                                        AmazingCaptureMoves(ArrLens(0), 1) = y
                                        AmazingCaptureMoves(ArrLens(0), 2) = PieceMoves(n)
                                        ArrLens(0) += 1
                                    ElseIf PieceValueDif = 0 Then
                                        CaptureMoves(ArrLens(1), 0) = x
                                        CaptureMoves(ArrLens(1), 1) = y
                                        CaptureMoves(ArrLens(1), 2) = PieceMoves(n)
                                        ArrLens(1) += 1
                                    Else
                                        OtherCaptureMoves(ArrLens(2), 0) = x
                                        OtherCaptureMoves(ArrLens(2), 1) = y
                                        OtherCaptureMoves(ArrLens(2), 2) = PieceMoves(n)
                                        ArrLens(2) += 1
                                    End If
                                ElseIf Not OnlyCaptures Then
                                    dx = Math.Abs(CStr(KPos(0)) - CStr(PieceMoves(n)(0)))
                                    dy = Math.Abs(CStr(KPos(1)) - CStr(PieceMoves(n)(1)))
                                    If PieceMoves(n)(1) = "7" AndAlso Board(x, y) = "p" Then
                                        PawnPromotionMoves(ArrLens(3), 0) = x
                                        PawnPromotionMoves(ArrLens(3), 1) = y
                                        PawnPromotionMoves(ArrLens(3), 2) = PieceMoves(n)
                                        ArrLens(3) += 1
                                    ElseIf Board(Math.Max(CStr(PieceMoves(n)(0)) - 1, 0), Math.Min(CStr(PieceMoves(n)(1)) + 1, 7)) = "P" OrElse Board(Math.Min(CStr(PieceMoves(n)(0)) + 1, 7), Math.Min(CStr(PieceMoves(n)(1)) + 1, 7)) = "P" Then
                                        TerribleMoves(ArrLens(7), 0) = x
                                        TerribleMoves(ArrLens(7), 1) = y
                                        TerribleMoves(ArrLens(7), 2) = PieceMoves(n)
                                        ArrLens(7) += 1
                                    ElseIf TrueFalseTable(CStr(PieceMoves(n)(0)), CStr(PieceMoves(n)(1))) = "F" Then
                                        BadMoves(ArrLens(6), 0) = x
                                        BadMoves(ArrLens(6), 1) = y
                                        BadMoves(ArrLens(6), 2) = PieceMoves(n)
                                        ArrLens(6) += 1
                                    ElseIf (Board(x, y) = "r" OrElse Board(x, y) = "b" OrElse Board(x, y) = "q") AndAlso (dx = 0 OrElse dy = 0 OrElse dx = dy) Then
                                        GoodMoves(ArrLens(4), 0) = x
                                        GoodMoves(ArrLens(4), 1) = y
                                        GoodMoves(ArrLens(4), 2) = PieceMoves(n)
                                        ArrLens(4) += 1
                                    Else
                                        OtherMoves(ArrLens(5), 0) = x
                                        OtherMoves(ArrLens(5), 1) = y
                                        OtherMoves(ArrLens(5), 2) = PieceMoves(n)
                                        ArrLens(5) += 1
                                    End If
                                End If
                            Next
                        End If
                    End If
                Next
            Next
        End If

        'At the end of the function, we merge all the category arrays into one - producing a huge, tiered,
        'list of a player's total pseudo-legal moves. If the user is in Quiescence mode, all non-capture moves
        'are discounted, and are not merged.
        Array.Copy(CaptureMoves, 0, AmazingCaptureMoves, 3 * ArrLens(0), 3 * ArrLens(1))
        Array.Copy(OtherCaptureMoves, 0, AmazingCaptureMoves, 3 * (ArrLens(0) + ArrLens(1)), 3 * ArrLens(2))
        ArrLens(0) += ArrLens(1) + ArrLens(2)
        If Not OnlyCaptures Then 'Copies all the non-capture moves to the main array.
            If ArrLens(3) > 0 Then Array.Copy(PawnPromotionMoves, 0, AmazingCaptureMoves, 3 * ArrLens(0), 3 * ArrLens(3))
            Array.Copy(GoodMoves, 0, AmazingCaptureMoves, 3 * (ArrLens(0) + ArrLens(3)), 3 * ArrLens(4))
            Array.Copy(OtherMoves, 0, AmazingCaptureMoves, 3 * (ArrLens(0) + ArrLens(3) + ArrLens(4)), 3 * ArrLens(5))
            ArrLens(0) += ArrLens(3) + ArrLens(4) + ArrLens(5)
            Array.Copy(BadMoves, 0, AmazingCaptureMoves, 3 * ArrLens(0), 3 * ArrLens(6))
            Array.Copy(TerribleMoves, 0, AmazingCaptureMoves, 3 * (ArrLens(0) + ArrLens(6)), 3 * ArrLens(7))
            ArrLens(0) += ArrLens(6) + ArrLens(7)
        End If
        'Make the first index of the array to be the total amount of pseudo-legal moves.
        'This helps know how many indexes of the array to check when looking for moves.
        AmazingCaptureMoves(0, 0) = ArrLens(0) - 1
        Return AmazingCaptureMoves
    End Function


    'Function which receives a game position and a possible move. The function makes this move on the board (using
    'shortcuts that can only be made on a virtual board), and then generates the possible moves of the attacking
    'piece(s). If the king is no longer being threatened, then the check as been resolved.
    Private Function DoesMoveResolveCheck(ByVal Board(,) As Char, ByVal OldPosX As String, ByVal OldPosY As String, ByVal NewPosX As String, ByVal NewPosY As String, ByRef WInCheck As InCheck, ByRef BInCheck As InCheck, ByVal EnPassant As String) As Boolean
        'If the player is in a double check, then only king moves could be legal. Therefore, as this part is done in the
        'TFTable generation, we skip this here.
        If WInCheck.IsInCheck AndAlso Not WInCheck.DoubleCheck Then
            If NewPosX & NewPosY = WInCheck.Piece OrElse (NewPosX & NewPosY = EnPassant AndAlso Board(OldPosX, OldPosY) = "P") Then
                'Move captures the attacking piece. Therefore we assume it is legal.
                Return True
            ElseIf Board(NewPosX, NewPosY) = " " Then
                WInCheck.IsInCheck = False
                Board(NewPosX, NewPosY) = "O" 'Move made on temporary board.
                'Calculate the legal moves of the attacking piece. If the king is still in check, then WInCheck.IsInCheck
                'will flag from False to True - therefore it is illegal.
                BlackPieceLegalMoves(Board, CStr(WInCheck.Piece(0)), CStr(WInCheck.Piece(1)), TrueTable, TrueTable, "00", WInCheck, BInCheck, CannotCastle, "-")
                Board(NewPosX, NewPosY) = " " 'Move unmade on temporary board.
                If WInCheck.IsInCheck = False Then Return True
            Else 'Move is a capture move, but not capturing the attacking piece. Therefore, it has to be illegal.
                Return False
            End If
        ElseIf BInCheck.IsInCheck AndAlso Not BInCheck.DoubleCheck Then
            'Identical code but for the black pieces.
            If NewPosX & NewPosY = BInCheck.Piece OrElse (NewPosX & NewPosY = EnPassant AndAlso Board(OldPosX, OldPosY) = "p") Then
                Return True
            ElseIf Board(NewPosX, NewPosY) = " " Then
                BInCheck.IsInCheck = False
                Board(NewPosX, NewPosY) = "o"
                WhitePieceLegalMoves(Board, CStr(BInCheck.Piece(0)), CStr(BInCheck.Piece(1)), TrueTable, TrueTable, "00", BInCheck, WInCheck, CannotCastle, "-")
                Board(NewPosX, NewPosY) = " "
                If BInCheck.IsInCheck = False Then Return True
            Else
                Return False
            End If
        End If
        Return False
    End Function


    'Subroutine that makes a move on the board, given coordinates. Includes castling (& rights) and pawn promotion.
    Private Sub MakeMove(ByVal Board(,) As Char, ByVal OldCoorX As String, ByVal OldCoorY As String, ByVal NewCoorX As String, ByVal NewCoorY As String, ByRef CanCastle As CanCastle, ByRef KPos As String, ByRef MaterialCount() As SByte, ByRef EnPassant As String)
        Dim TempPiece As Char = Board(OldCoorX, OldCoorY)
        Dim HasEnPassanted As Boolean
        If Char.IsUpper(TempPiece) Then
            If TempPiece = "P" Then
                'Code for Promoting Pawns and En Passant. Also increments the material count.
                If NewCoorY = 0 Then
                    TempPiece = "Q"
                    MaterialCount(0) += ReturnPieceValue("Q") - ReturnPieceValue("P") '+ 9 for a new queen, - 1 for losing the pawn in the process.
                ElseIf NewCoorX & NewCoorY = EnPassant Then
                    Board(NewCoorX, NewCoorY + 1) = " "
                    MaterialCount(1) -= ReturnPieceValue("P")
                ElseIf OldCoorY = 6 AndAlso NewCoorY = 4 AndAlso (Board(Math.Max(NewCoorX - 1, 0), 4) = "p" OrElse Board(Math.Min(NewCoorX + 1, 7), 4) = "p") Then
                    'EnPassant creation.
                    EnPassant = NewCoorX & 5
                    HasEnPassanted = True
                End If
                'If piece is a Rook, part of Castling is disabled (depending on which Rook has moved).
            ElseIf TempPiece = "R" AndAlso CanCastle.CanICastle Then
                If OldCoorX = 0 AndAlso OldCoorY = 7 Then
                    CanCastle.QS = False
                ElseIf OldCoorX = 7 AndAlso OldCoorY = 7 Then
                    CanCastle.KS = False
                End If
            ElseIf TempPiece = "K" Then
                KPos = NewCoorX & NewCoorY
                'Code for Castling.
                If CanCastle.KS AndAlso NewCoorX = 6 AndAlso NewCoorY = 7 Then
                    Board(5, 7) = "R"
                    Board(7, 7) = " "
                ElseIf CanCastle.QS AndAlso NewCoorX = 2 AndAlso NewCoorY = 7 Then
                    Board(0, 7) = " "
                    Board(3, 7) = "R"
                End If
                CanCastle.CannotCastle()
            End If
        Else
            'Identical Code for the Black Pieces.
            If TempPiece = "p" Then
                If NewCoorY = 7 Then
                    TempPiece = "q"
                    MaterialCount(1) += ReturnPieceValue("Q") - ReturnPieceValue("P")
                ElseIf NewCoorX & NewCoorY = EnPassant Then
                    Board(NewCoorX, NewCoorY - 1) = " "
                    MaterialCount(0) -= ReturnPieceValue("P")
                ElseIf OldCoorY = 1 AndAlso NewCoorY = 3 AndAlso (Board(Math.Max(NewCoorX - 1, 0), 3) = "P" OrElse Board(Math.Min(NewCoorX + 1, 7), 3) = "P") Then
                    EnPassant = NewCoorX & 2
                    HasEnPassanted = True
                End If
            ElseIf TempPiece = "r" AndAlso CanCastle.CanICastle Then
                If OldCoorX = 0 AndAlso OldCoorY = 0 Then
                    CanCastle.QS = False
                ElseIf OldCoorX = 7 AndAlso OldCoorY = 0 Then
                    CanCastle.KS = False
                End If
            ElseIf TempPiece = "k" Then
                KPos = NewCoorX & NewCoorY
                If CanCastle.KS AndAlso NewCoorX = 6 AndAlso NewCoorY = 0 Then
                    Board(5, 0) = "r"
                    Board(7, 0) = " "
                ElseIf CanCastle.QS AndAlso NewCoorX = 2 AndAlso NewCoorY = 0 Then
                    Board(0, 0) = " "
                    Board(3, 0) = "r"
                End If
                CanCastle.CannotCastle()
            End If
        End If

        If Not (EnPassant = "-" OrElse HasEnPassanted) Then EnPassant = "-" 'Removal of EnPassant (if required).
        'At the end of the subroutine, the Piece is placed at the new coordinates, and the old position is cleared.
        'If the new position contains a piece, then the material count is updated for only that piece.
        If Board(NewCoorX, NewCoorY) <> " " Then
            If Char.IsUpper(Board(NewCoorX, NewCoorY)) Then
                MaterialCount(0) -= ReturnPieceValue(Board(NewCoorX, NewCoorY))
            Else
                MaterialCount(1) -= ReturnPieceValue(Board(NewCoorX, NewCoorY))
            End If
        End If
        Board(NewCoorX, NewCoorY) = TempPiece
        Board(OldCoorX, OldCoorY) = " "
    End Sub



    'This function contains my MiniMax algorithm using Alpha-Beta Pruning and Quiescence (optional).
    Private Function MiniMax(ByVal Board(,) As Char, ByVal depth As SByte, ByVal isWhite As Boolean, ByVal WCanCastle As CanCastle, ByVal BCanCastle As CanCastle, ByVal WKPos As String, ByVal BKPos As String, ByVal EnPassant As String, ByVal MaterialCount() As SByte, ByVal Alpha As Decimal, ByVal Beta As Decimal) As Decimal
        If ABORT Then Return 0
        TotalPositionsSearched += 1
        'Creates moves & checking rules.
        Dim CurrentMove, BestMove As Decimal
        Dim WInCheck As New InCheck
        Dim BInCheck As New InCheck

        If isWhite Then
            'Creates and forms the TFTable for the player to move.
            Dim WhiteTFTable(7, 7) As Char
            FixTFTables(Board, True, WhiteTFTable, WKPos, WInCheck, NotInCheck, EnPassant)
            If Not (depth > 0 OrElse WInCheck.IsInCheck) Then 'Quiescence mode activated.
                'Evaluation of board is the current move to beat.
                BestMove = Evaluate(MaterialCount, WKPos, BKPos)
                Alpha = Math.Max(Alpha, BestMove)
                If Beta <= Alpha Then 'ABP.
                    Return BestMove
                End If
            Else
                BestMove = Integer.MinValue
            End If
            'Creates the pseudo-legal moves for the chosen player.
            Dim PieceMoves(200, 2) As String
            PieceMoves = CreateMoves(Board, True, WhiteTFTable, BKPos, WInCheck, BInCheck, WCanCastle, EnPassant, Not (depth > 0 OrElse WInCheck.IsInCheck)) 'If Quiescence mode is activated then use capture moves only.
            If PieceMoves(0, 0) > 0 Then 'If any move exists...
                'Creates temp variables.
                Dim TempBoard(7, 7) As Char
                Dim TempMaterialCount(1) As SByte
                Dim TempWKPos As String
                Dim TempWCanCastle As New CanCastle
                Dim TempWInCheck As New InCheck
                Dim TempEnPassant As String
                For n = 1 To CInt(PieceMoves(0, 0)) 'for each move...
                    'If the user is in check, eliminate the moves that do not escape check. King moves are not considered
                    'as the player's TFTable will ensure that all king moves are legal.
                    If WInCheck.IsInCheck AndAlso Board(PieceMoves(n, 0), PieceMoves(n, 1)) <> "K" Then
                        'Runs move through the DoesMoveResolveCheck algorithm.
                        TempWInCheck.CopyFrom(WInCheck)
                        If DoesMoveResolveCheck(Board, PieceMoves(n, 0), PieceMoves(n, 1), PieceMoves(n, 2)(0), PieceMoves(n, 2)(1), TempWInCheck, BInCheck, EnPassant) Then
                            'Move has resolved check.
                            TempWInCheck.NotInCheck()
                        End If
                    Else
                        TempWInCheck.IsInCheck = False
                    End If
                    If Not TempWInCheck.IsInCheck Then 'Therefore, is a legal move.
                        'Copies board info to temp variables.
                        Array.Copy(Board, TempBoard, 64)
                        TempMaterialCount(0) = MaterialCount(0)
                        TempMaterialCount(1) = MaterialCount(1)
                        TempWKPos = WKPos
                        TempWCanCastle.CopyFrom(WCanCastle)
                        TempEnPassant = EnPassant
                        'Makes move on temp board, then calls MiniMax for this new position.
                        MakeMove(TempBoard, PieceMoves(n, 0), PieceMoves(n, 1), PieceMoves(n, 2)(0), PieceMoves(n, 2)(1), TempWCanCastle, TempWKPos, TempMaterialCount, TempEnPassant)
                        If Not UseQuiescence AndAlso depth = 1 Then
                            'We have reached a leaf position - return the evaluation for this position.
                            TotalPositionsSearched += 1
                            CurrentMove = Evaluate(TempMaterialCount, TempWKPos, BKPos) 'Evaluate position for opponent.
                        Else 'No leaf node (or are using Quiescence) - put position through MiniMax recursively.
                            CurrentMove = MiniMax(TempBoard, depth - 1, False, TempWCanCastle, BCanCastle, TempWKPos, BKPos, TempEnPassant, TempMaterialCount, Alpha, Beta)
                        End If
                        If CurrentMove > BestMove Then
                            BestMove = CurrentMove 'Best Move has been beaten - replace it.
                            Alpha = Math.Max(Alpha, BestMove) 'Alpha = best move found for white.
                            'Move was too strong for player; opponent will not choose this branch.
                            If Beta <= Alpha Then
                                Return BestMove 'Alpha-Beta Pruning - return best move.
                            End If
                        End If
                    End If
                Next
            End If
        Else 'Near-identical code for the black side.
            Dim BlackTFTable(7, 7) As Char
            FixTFTables(Board, False, BlackTFTable, BKPos, NotInCheck, BInCheck, EnPassant)
            If Not (depth > 0 OrElse BInCheck.IsInCheck) Then
                BestMove = Evaluate(MaterialCount, WKPos, BKPos)
                Beta = Math.Min(Beta, BestMove)
                If Beta <= Alpha Then
                    Return BestMove
                End If
            Else
                BestMove = Integer.MaxValue
            End If
            Dim PieceMoves(200, 2) As String
            PieceMoves = CreateMoves(Board, False, BlackTFTable, WKPos, WInCheck, BInCheck, BCanCastle, EnPassant, Not (depth > 0 OrElse BInCheck.IsInCheck))
            If PieceMoves(0, 0) > 0 Then
                Dim TempBoard(7, 7) As Char
                Dim TempMaterialCount(1) As SByte
                Dim TempBKPos As String
                Dim TempBCanCastle As New CanCastle
                Dim TempBInCheck As New InCheck
                Dim TempEnPassant As String
                For n = 1 To CInt(PieceMoves(0, 0))
                    If BInCheck.IsInCheck AndAlso Board(PieceMoves(n, 0), PieceMoves(n, 1)) <> "k" Then
                        TempBInCheck.CopyFrom(BInCheck)
                        If DoesMoveResolveCheck(Board, PieceMoves(n, 0), PieceMoves(n, 1), PieceMoves(n, 2)(0), PieceMoves(n, 2)(1), WInCheck, TempBInCheck, EnPassant) Then
                            TempBInCheck.NotInCheck()
                        End If
                    Else
                        TempBInCheck.IsInCheck = False
                    End If
                    If Not TempBInCheck.IsInCheck Then
                        Array.Copy(Board, TempBoard, 64)
                        TempMaterialCount(0) = MaterialCount(0)
                        TempMaterialCount(1) = MaterialCount(1)
                        TempBKPos = BKPos
                        TempBCanCastle.CopyFrom(BCanCastle)
                        TempEnPassant = EnPassant
                        MakeMove(TempBoard, PieceMoves(n, 0), PieceMoves(n, 1), PieceMoves(n, 2)(0), PieceMoves(n, 2)(1), TempBCanCastle, TempBKPos, TempMaterialCount, TempEnPassant)
                        If Not UseQuiescence AndAlso depth = 1 Then
                            TotalPositionsSearched += 1
                            CurrentMove = Evaluate(TempMaterialCount, WKPos, TempBKPos)
                        Else
                            CurrentMove = MiniMax(TempBoard, depth - 1, True, WCanCastle, TempBCanCastle, WKPos, TempBKPos, TempEnPassant, TempMaterialCount, Alpha, Beta)
                        End If
                        If CurrentMove < BestMove Then
                            BestMove = CurrentMove
                            Beta = Math.Min(Beta, BestMove) 'Beta = best move found for white.
                            If Beta <= Alpha Then
                                Return BestMove
                            End If
                        End If
                    End If
                Next
            End If
        End If

        If BestMove = Integer.MinValue OrElse BestMove = Integer.MaxValue Then
            'No legal move found for the player.
            If WInCheck.IsInCheck Then
                'Checkmate for white: return -(1000 + depth) so that the quickest path to checkmate is chosen by the AI.
                Return -(1000 + depth)
            ElseIf BInCheck.IsInCheck Then
                'Checkmate for black: return (1000 + depth) so that the quickest path to checkmate is chosen by the AI.
                Return (1000 + depth)
            Else 'Stalemate: return 0
                Return 0
            End If
        End If
        'Otherwise, return the best move's score found this iteration.
        Return BestMove
    End Function

    'This algorithm is used to condense a board position into an evaluation score, used to determine best moves.
    'We take into account the difference in material between the two sides, along with a heuristic to help
    'the AI find checkmated in simple endgame positions.
    Private Function Evaluate(ByVal MaterialCount() As SByte, ByVal WKPos As String, ByVal BKPos As String) As Decimal
        'Finds difference in material between both sides.
        Dim Score As Decimal = MaterialCount(0) - MaterialCount(1)
        'If the opponent has little material left, we try to find positions where the opponent king is close to
        'the edge / corner of the board, and where the kings are closer together. This can help find a checkmate.
        If MaterialCount(0) < 10 OrElse MaterialCount(1) < 10 Then
            'Finds distances between kings.
            Dim KingDistance As Byte = Math.Max(Math.Abs(CStr(WKPos(0)) - CStr(BKPos(0))), Math.Abs(CStr(WKPos(1)) - CStr(BKPos(1))))
            Dim KingCentreDistance As Byte
            If MaterialCount(1) < 10 Then
                'Finds distance from opponent's king to the centre of the board.
                KingCentreDistance = Math.Max(CStr(BKPos(0)) - 4, 3 - CStr(BKPos(0))) + Math.Max(CStr(BKPos(1)) - 4, 3 - CStr(BKPos(1)))
                'Heuristic becomes more prevelant as the opponent has fewer and fewer pieces (exponential curve).
                Score += (KingCentreDistance * 1.5 + (7 - KingDistance)) * (1.3 ^ (9 - MaterialCount(1))) / 100
            End If
            If MaterialCount(0) < 10 Then 'Similar code for the white pieces.
                KingCentreDistance = Math.Max(CStr(WKPos(0)) - 4, 3 - CStr(WKPos(0))) + Math.Max(CStr(WKPos(1)) - 4, 3 - CStr(WKPos(1)))
                Score -= (KingCentreDistance * 1.5 + (7 - KingDistance)) * (1.3 ^ (9 - MaterialCount(0))) / 100
            End If
        End If
        Return Math.Round(Score, 2) 'Prevents huge floating point numbers from entering the system.
    End Function

End Class
