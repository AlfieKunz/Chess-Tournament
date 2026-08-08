'This class contains my Chess AI, which involves the MiniMax algorithm with Alpha-Beta Pruning.
'This class is modular from my Chess class - being constructed only from the FEN position, and only returning a Move (see the structure below).
'Some other interacting is done, however, such as allowing the AI to be remotely aborted.
Imports System.IO
Imports System.Text

Public Class AI 'i shall thy the Alfie Alphafish (bit optimistic, I know).
    Inherits CoreMethods
    Private PieceHeatMap As New PieceHeatSqaures() 'Calls & Constructs PieceHeatMaps - producing a hashed array containing the
    '"ideal" locations for each piece on the board (used to evaluate a board position).

    Private HasBeenInstantiated As Boolean 'The AI will not perform methods if it has not been fully instantiated with a FEN.
    'Below are the details that the AI requires for a search. Please see their counterparts in the Chess form for their info.
    Private PrimaryBoard(7, 7), PrimaryTFTable(7, 7) As Char
    Private PrimaryMeCanCastle, PrimaryEnemyCanCastle As New CanCastle
    Private PrimaryMeInCheck As New InCheck
    Private PrimaryMeKPos, PrimaryEnemyKPos As String
    Private PrimaryEnPassant As String
    Private PrimaryMaterialCount(1) As Int16
    Private PlayerTurn As Boolean
    Private BasePieceMoves(,) As String 'Represents the current legal moves in the position (used so that the moves don't have to
    'be recomputed every time we call the AI).
    Private BaseZobristValue As UInt64 'Represents the Zorbist Hash Key of the position to be searched on.

    Private KingSymbol As Char '"K" for white, "k" for black. Used for helping to resolve checks.
    Private SearchSettings As New AISearchSettings 'Settings of the current search.
    Private TotalPositionsSearched, TranspositionsFound, CheckmatesFound As UInt32 'Numbers showing the stats of the current search.
    Private LifetimePositions, LifetimeTranspositions, LifetimeCheckmates As UInt32 'Numbers showing the lifetime stats of the AI (persists
    'across multiple boot-ups).
    Private HighestQuiescenceDepth, HighestSuccessfulQuiescenceDepth As SByte 'Shows the maximum reached depth of a search that has not been ABORTed.
    Private ABORT As Boolean 'Controlled by the thread handlers - an AI is aborted if another AI has
    'found a mating pattern, or if the AI has ran out of time. If ABORT is set to True, then the AI finishes its search ASAP.
    Private MasterDepth As SByte 'The Surface Depth of the search, as set by the user.

    Private KillerMoves(255, 1) As String 'Array containing Killer Moves: non-capture moves which caused an alpha-beta cut off.
    'If we detect killer moves in sibling positions (ie: positions of the same depth), we search the Killer Move(s) first.

    'Arrays that the CreateMoves function uses (delared before to save processing time in the search process).
    Private AmazingCaptureMoves(19, 1) As String 'Min size = 19
    Private CaptureMoves(19, 1) As String 'Min size = 19
    Private OtherCaptureMoves(19, 1) As String 'Min size = 19
    Private PawnPromotionMoves(7, 1) As String 'Min size = 7
    Private GoodMoves(99, 1) As String 'Min size = 49
    Private OtherMoves(99, 1) As String 'Min size = 99
    Private BadMoves(49, 1) As String 'Min size = 49
    Private TerribleMoves(49, 1) As String 'Min size = 49

    'Structures for the TranspositionTable - a large table containing the basic details of a position - stored via their Zobrist Hash.
    'Stored as a list so that the overall size of the Table can be reduced (would need to be 2^64 elements large otherwise).
    'Therefore, multiple positions will be stored in the same 'list' entry in the Table, so we then need to perform a linear search
    'on the list to find the exact position we need.
    Private TranspositionTable((2 ^ 20) - 1) As List(Of TTEntry)
    Private Structure TTEntry
        Dim IsPopulated As Boolean
        Dim Key As UInt64 'Zobrist Key of position - used to pinpoint the correct board.
        Dim Depth As SByte
        Dim Flag As Byte 'Represents additional information about the move:
        '0 = Score is exact (no ambiguity), 1 = Lower Bound (score could be higher), 2 = Upper Bound (score could be lower),
        '4 = Position is currently being searched on (for 3RF), 5 = End State (ie: checkmate, stalemate).
        Dim Score As Int16 'Stores the evaluation of the position
        Dim BestMoveOld As String 'Stores the best move found from this position when previously computed.
        Dim BestMoveNew As String
    End Structure



    'Constructor method.
    Public Sub New()
        'AI variables instantiated but without a FEN. Used when the program is first booted up.
    End Sub
    Public Sub New(ByVal FEN As String)
        'Configures the AI using a given FEN.
        Reconfigure(FEN, True)
        'Loads the Lifetime stats file, and stores the results into their appropriate variables.
        Try
            Using SR As New StreamReader(AppDomain.CurrentDomain.BaseDirectory & "\Assets\AIStats.txt", Encoding.UTF8, True)
                LifetimePositions = SR.ReadLine()
                LifetimeTranspositions = SR.ReadLine()
                LifetimeCheckmates = SR.ReadLine()
            End Using
        Catch ex As Exception
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Unable to retrieve AI lifetime stats. Resetting...")
            Console.ResetColor()
        End Try
    End Sub
    Public Function GetVersion() As String
        Return GlobalConstants.ProgramVersion
    End Function
    Public Function GetBoard() As Char(,)
        Return PrimaryBoard
    End Function
    Public Function GetZobristValue() As UInt64
        Return BaseZobristValue
    End Function
    Public Function GetMaterialCount() As Integer()
        Return New Integer() {PrimaryMaterialCount(0), PrimaryMaterialCount(1)}
    End Function
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
                MakeMove(TempBoard, TempMove.OldMoveX, TempMove.OldMoveY, TempMove.NewMoveX, TempMove.NewMoveY, TempMeCanCastle, "", {10000, 10000}, TempEnPassant, 0UL)
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


    'Subroutine which converts a user's input FEN into all the details needed to conduct a MiniMax search.
    Public Sub Reconfigure(ByVal FEN As String, ByVal ResetTT As Boolean)
        'Converts the user's FEN into a board position, then resets checking rules.
        HighestSuccessfulQuiescenceDepth = 1
        PrimaryBoard = FENConverter(FEN, PrimaryMeCanCastle, PrimaryEnemyCanCastle, PrimaryMeKPos, PrimaryEnemyKPos, PrimaryEnPassant, PlayerTurn)
        PrimaryMeInCheck.NotInCheck()
        If PlayerTurn Then
            'Creates the TFTable for the white pieces, then creates king symbol.
            FixTFTables(PrimaryBoard, True, PrimaryTFTable, PrimaryMeKPos, PrimaryMeInCheck, PrimaryEnPassant)
            KingSymbol = "K"
            BasePieceMoves = CreateMoves(PrimaryBoard, True, PrimaryTFTable, PrimaryEnemyKPos, PrimaryMeInCheck, PrimaryMeCanCastle, PrimaryEnPassant, False, 0, "", "")
            'Creates the Zobrist Value for the position.
            BaseZobristValue = ZobristHashPosition(PrimaryBoard, True, PrimaryMeCanCastle, PrimaryEnemyCanCastle, PrimaryEnPassant)
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
            FixTFTables(PrimaryBoard, False, PrimaryTFTable, PrimaryMeKPos, PrimaryMeInCheck, PrimaryEnPassant)
            KingSymbol = "k"
            BasePieceMoves = CreateMoves(PrimaryBoard, False, PrimaryTFTable, PrimaryEnemyKPos, PrimaryMeInCheck, PrimaryMeCanCastle, PrimaryEnPassant, False, 0, "", "")
            BaseZobristValue = ZobristHashPosition(PrimaryBoard, False, PrimaryEnemyCanCastle, PrimaryMeCanCastle, PrimaryEnPassant)
        End If
        'Finds the material count of the board.
        PrimaryMaterialCount = CountMaterial(PrimaryBoard)
        'Resets KillerMoves.
        For n = 0 To 255
            KillerMoves(n, 0) = Nothing
            KillerMoves(n, 1) = Nothing
        Next
        If ResetTT Then ResetTranspositionTable()
        HasBeenInstantiated = True
    End Sub

    Public Sub ResetTranspositionTable()
        ReDim TranspositionTable(2 ^ 20 - 1)
    End Sub


    'Subroutines that adds / removes the Board History to / from the Transposition Table, so that MiniMax can flag these positions
    'as being 3-fold repetition (if they are encountered in a search).
    Public Sub AddBoardHistory(ByVal BoardHistory() As UInt64)
        'Calibrates this new entry in the Transpositon Table.
        Dim TempTTEntry As New TTEntry
        TempTTEntry.IsPopulated = True
        TempTTEntry.Depth = 100 'Ensures that the depth will always be greater than the current search, meaning that the TTEntry
        'will never be ignored by a MiniMax branch.
        TempTTEntry.Flag = 4 'Represents a repeated position.
        Dim EntryInTT As UInt32
        Dim FoundInTT As Boolean

        'Adds each position to the Transposition Table.
        For n = 0 To BoardHistory.Length - 1
            'Hashes Zobrist key to find the location of the Entry in the Table.
            TempTTEntry.Key = BoardHistory(n)
            EntryInTT = BoardHistory(n) >> 44
            FoundInTT = False
            If TranspositionTable(EntryInTT) Is Nothing Then
                TranspositionTable(EntryInTT) = New List(Of TTEntry)
            Else
                'Searches for position in Transposition Table.
                For m = 0 To TranspositionTable(EntryInTT).Count - 1
                    If TranspositionTable(EntryInTT)(m).Key = TempTTEntry.Key Then
                        'Position has been found - replace this with the repeated position entry.
                        TranspositionTable(EntryInTT)(m) = TempTTEntry
                        FoundInTT = True
                        Exit For
                    End If
                Next
            End If
            If Not FoundInTT Then TranspositionTable(EntryInTT).Add(TempTTEntry) 'Adds the Entry to the Transposition Table.
        Next
    End Sub
    Public Sub RemoveBoardHistory(ByVal BoardHistory() As UInt64)
        Dim EntryInTT As UInt32
        'Finds, then removes each position to the Transposition Table.
        For n = 0 To BoardHistory.Length - 1
            'Hashes Zobrist key to find the location of the Entry in the Table.
            EntryInTT = BoardHistory(n) >> 44
            If TranspositionTable(EntryInTT) IsNot Nothing Then
                For m = 0 To TranspositionTable(EntryInTT).Count - 1
                    If TranspositionTable(EntryInTT)(m).Key = BoardHistory(n) Then
                        'Entry has been found - remove from the Table.
                        TranspositionTable(EntryInTT).RemoveAt(m)
                        Exit For
                    End If
                Next
            End If
        Next
    End Sub

    'Subroutine which copies a parameter's settings to the AI's SearchSettings.
    Public Sub ConfigureSettings(ByVal UserSearchSettings As AISearchSettings, ByVal ResetTT As Boolean)
        SearchSettings.UseQuiescence = UserSearchSettings.UseQuiescence
        SearchSettings.UsePieceHeatMaps = UserSearchSettings.UsePieceHeatMaps
        SearchSettings.OutputToConsole = UserSearchSettings.OutputToConsole
        SearchSettings.OutputPath = UserSearchSettings.OutputPath
        SearchSettings.StableSearch = UserSearchSettings.StableSearch
        SearchSettings.ReturnBestMove = UserSearchSettings.ReturnBestMove
        SearchSettings.UpdateLifetimeStats = UserSearchSettings.UpdateLifetimeStats
    End Sub


    'Setter & Getter Functions for the ABORT attribute.
    Public Sub ABORTSearch()
        ABORT = True
    End Sub
    Public Function GetABORTState() As Boolean
        Return ABORT
    End Function


    'Algorithm that finds the AI's 'Best Move' for a given position, using the MiniMax algorithm.
    Public Function Search(ByVal Depth As Integer) As Move
        Dim BestMove As New Move
        If SearchSettings.ReturnBestMove Then BestMove.Score = -32767 Else BestMove.Score = 32767 '+ or - infinity.

        If HasBeenInstantiated AndAlso Depth > 0 Then
            'Creates alpha (white's best move) and beta (black's best move).
            Dim CurrentScore As Int16
            Dim Alpha As Int16 = -32767
            Dim Beta As Int16 = 32767
            'Resets debug variables.
            ABORT = False
            TotalPositionsSearched = 0
            TranspositionsFound = 0
            CheckmatesFound = 0
            HighestQuiescenceDepth = 1
            If BasePieceMoves IsNot Nothing Then 'If any move exists...
                'Creates temp variables.
                MasterDepth = Depth
                Dim TempBoard(7, 7) As Char
                Dim TempMaterialCount(1) As Int16
                Dim TempMeKPos As String
                Dim TempMeCanCastle As New CanCastle
                Dim TempMeInCheck As New InCheck
                Dim TempZobristValue As UInt64
                Dim TempEnPassant As String

                For n = 0 To BasePieceMoves.GetUpperBound(0) 'for each move...
                    'If the user is in check, eliminate the moves that do not escape check. King moves are not considered
                    'as the player's TFTable will ensure that all king moves are legal.
                    If PrimaryMeInCheck.IsInCheck AndAlso PrimaryBoard(Val(BasePieceMoves(n, 0)(0)), Val(BasePieceMoves(n, 0)(1))) <> KingSymbol Then
                        'Runs move through the DoesMoveResolveCheck algorithm.
                        TempMeInCheck.CopyFrom(PrimaryMeInCheck)
                        If DoesMoveResolveCheck(PrimaryBoard, PlayerTurn, BasePieceMoves(n, 0)(0), BasePieceMoves(n, 0)(1), BasePieceMoves(n, 1)(0), BasePieceMoves(n, 1)(1), TempMeInCheck, PrimaryEnPassant) Then
                            'Move has resolved check.
                            TempMeInCheck.NotInCheck()
                        End If
                    Else
                        TempMeInCheck.IsInCheck = False
                    End If
                    If Not TempMeInCheck.IsInCheck Then 'Therefore, is a legal move.
                        'Copies board info to temp variables.
                        Array.Copy(PrimaryBoard, TempBoard, 64)
                        Array.Copy(PrimaryMaterialCount, TempMaterialCount, 2)
                        TempMeKPos = PrimaryMeKPos
                        TempMeCanCastle.CopyFrom(PrimaryMeCanCastle)
                        TempEnPassant = PrimaryEnPassant
                        TempZobristValue = BaseZobristValue
                        'Makes move on temp board, then calls MiniMax for this new position.
                        MakeMove(TempBoard, BasePieceMoves(n, 0)(0), BasePieceMoves(n, 0)(1), BasePieceMoves(n, 1)(0), BasePieceMoves(n, 1)(1), TempMeCanCastle, TempMeKPos, TempMaterialCount, TempEnPassant, TempZobristValue)
                        If TempMaterialCount(0) + TempMaterialCount(1) = 0 Then
                            'Enforce draw by repetition.
                            TotalPositionsSearched += 1
                            CurrentScore = 0
                        ElseIf Depth = 1 AndAlso Not SearchSettings.UseQuiescence Then
                            'We have reached a leaf position - return the evaluation for this position.
                            TotalPositionsSearched += 1
                            If PlayerTurn Then
                                CurrentScore = Evaluate(TempBoard, TempMaterialCount, TempMeKPos, PrimaryEnemyKPos)
                            Else
                                CurrentScore = -Evaluate(TempBoard, TempMaterialCount, PrimaryEnemyKPos, TempMeKPos)
                            End If
                        ElseIf PlayerTurn Then
                            CurrentScore = MiniMax(TempBoard, Depth - 1, False, TempMeCanCastle, PrimaryEnemyCanCastle, TempMeKPos, PrimaryEnemyKPos, TempEnPassant, TempMaterialCount, TempZobristValue, Alpha, Beta)
                        Else
                            'Alpha & Beta are replaced with -Beta & -Alpha so that all moves' scores are given in context
                            'of the player to move. This is so that the largest number will always be the best score.
                            CurrentScore = -MiniMax(TempBoard, Depth - 1, True, PrimaryEnemyCanCastle, TempMeCanCastle, PrimaryEnemyKPos, TempMeKPos, TempEnPassant, TempMaterialCount, TempZobristValue, -Beta, -Alpha)
                        End If

                        'Code for selecting the best move in the position.
                        If ABORT Then
                            'Latest, unfinished move will not be considered.
                            Exit For
                        Else
                            If SearchSettings.ReturnBestMove Then
                                Alpha = Math.Max(Alpha, CurrentScore)
                                If CurrentScore > BestMove.Score Then
                                    'Move has been beaten (better) - replace it.
                                    BestMove.Score = CurrentScore
                                    BestMove.OldMoveX = BasePieceMoves(n, 0)(0)
                                    BestMove.OldMoveY = BasePieceMoves(n, 0)(1)
                                    BestMove.NewMoveX = BasePieceMoves(n, 1)(0)
                                    BestMove.NewMoveY = BasePieceMoves(n, 1)(1)
                                End If
                            Else
                                Beta = Math.Min(Beta, CurrentScore)
                                If CurrentScore < BestMove.Score Then
                                    'Move has been beaten (worse) - replace it.
                                    BestMove.Score = CurrentScore
                                    BestMove.OldMoveX = BasePieceMoves(n, 0)(0)
                                    BestMove.OldMoveY = BasePieceMoves(n, 0)(1)
                                    BestMove.NewMoveX = BasePieceMoves(n, 1)(0)
                                    BestMove.NewMoveY = BasePieceMoves(n, 1)(1)
                                End If
                            End If
                        End If

                    End If
                Next
            End If

            'Formats the score inside the correct range, then flips score if it is black to move.
            BestMove.Score /= 100
            If Not PlayerTurn Then BestMove.Score = -BestMove.Score
            HighestSuccessfulQuiescenceDepth = Math.Abs(HighestQuiescenceDepth) + MasterDepth

            If Math.Abs(BestMove.Score) = 327.67 Then
                'Search either had 0 legal moves, or has been terminated so early that no move could be successfully searched.
                BestMove.Code = "a" 'Note for aborted search.
                If SearchSettings.OutputToConsole Then
                    Console.ForegroundColor = ConsoleColor.DarkGreen
                    Console.WriteLine("Depth Of " & Depth & " Incomplete.")
                    Console.ResetColor()
                End If
                If Not ABORT Then Return BestMove
            ElseIf SearchSettings.OutputToConsole Then
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.Write("Depth Of " & Depth)
                If SearchSettings.UseQuiescence Then Console.Write("-" & GetHighestQuiescenceDepth()) 'Retrieves the maximum reached depth via Quiescence (if enabled).
                If ABORT Then Console.Write(" Incomplete. Predicted ") Else Console.Write(" Completed. ")
                OutputMoveInfo(BestMove) 'Outputs the diagnostics for the current search.
                If SearchSettings.OutputPath Then Console.WriteLine("Path = " & GenerateBestMoveLine(BestMove)) 'Generates the path of 'best moves' leading from this position.
            End If
            If SearchSettings.OutputToConsole Then Console.WriteLine("Positions searched = " & TotalPositionsSearched.ToString("N0") & vbCrLf & "Transpositions Found: " & TranspositionsFound.ToString("N0") & vbCrLf & "Checkmates Found: " & CheckmatesFound.ToString("N0") & vbCr)
            If SearchSettings.UpdateLifetimeStats Then
                'Increments lifetime stats.
                LifetimePositions += TotalPositionsSearched
                LifetimeTranspositions += TranspositionsFound
                If Math.Abs(BestMove.Score) = 299.99 Then LifetimeCheckmates += 1
            End If
        Else 'AI not correctly instantiated.
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when Attempting Search - FEN Position not set / Depth set too low.")
            Console.ResetColor()
            BestMove.Code = "a"
        End If
        Return BestMove
    End Function

    'Search function with added 'PreviousBestMove' attribute - ammends 'BasePieceMoves' so that PreviousBestMove will be searched first.
    Public Function Search(ByVal Depth As Integer, ByVal PreviousBestMove As Move) As Move
        Dim OldCoors As String = PreviousBestMove.OldMoveX & PreviousBestMove.OldMoveY
        Dim NewCoors As String = PreviousBestMove.NewMoveX & PreviousBestMove.NewMoveY
        Dim LocationInMoves As Byte
        If Not (BasePieceMoves(0, 0) = OldCoors AndAlso BasePieceMoves(0, 1) = NewCoors) Then
            'Finds the location of PreviousBestMove in BasePieceMoves.
            For n = 1 To BasePieceMoves.GetUpperBound(0)
                If BasePieceMoves(n, 0) = OldCoors AndAlso BasePieceMoves(n, 1) = NewCoors Then
                    LocationInMoves = n
                    Exit For
                End If
            Next
            If LocationInMoves = 0 Then
                Console.ForegroundColor = ConsoleColor.Red
                Console.WriteLine("Unable to find Input Move in the current position - performing search as normal...")
                Console.ResetColor()
            Else
                'Shifts all BasePieceMoves variables one index down, then adds PreviousBestMove to the start of the array.
                Array.Copy(BasePieceMoves, 0, BasePieceMoves, 2, LocationInMoves * 2)
                BasePieceMoves(0, 0) = OldCoors
                BasePieceMoves(0, 1) = NewCoors
            End If
        End If
        'Performs the search as normal.
        Return Search(Depth)
    End Function


    Public Function GetHighestQuiescenceDepth() As Byte
        Return HighestSuccessfulQuiescenceDepth
    End Function

    'Subroutine that returns the diagnostics of the current search.
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

    'Function that generates the line containing the best sequence of moves, as found by the AI, generated by following the TranspositionTable 'best move' trail.
    Private Function GenerateBestMoveLine(ByVal BestMove As Move) As String
        Dim BestLine As String = MoveConverter(PrimaryBoard, BestMove, PrimaryEnPassant)
        Dim TempMove As Move = BestMove

        'Copies primary board attributes to their temporary counterparts.
        Dim isWhite As Boolean = PlayerTurn
        Dim TempBoard(7, 7) As Char
        Array.Copy(PrimaryBoard, TempBoard, 64)
        Dim TempWCanCastle, TempBCanCastle As New CanCastle
        Dim TempWKPos, TempBKPos As String
        If PlayerTurn Then
            TempWCanCastle.CopyFrom(PrimaryMeCanCastle)
            TempBCanCastle.CopyFrom(PrimaryEnemyCanCastle)
            TempWKPos = PrimaryMeKPos
            TempBKPos = PrimaryEnemyKPos
        Else
            TempWCanCastle.CopyFrom(PrimaryEnemyCanCastle)
            TempBCanCastle.CopyFrom(PrimaryMeCanCastle)
            TempWKPos = PrimaryEnemyKPos
            TempBKPos = PrimaryMeKPos
        End If
        Dim TempMaterialCount(1) As Int16
        Dim TempEnPassant As String = PrimaryEnPassant
        Dim TempZobristValue As UInt64 = BaseZobristValue


        Dim TempTTEntry As TTEntry
        Dim EntryInTT As UInt32

        'Caps the maximum amount of moves being displaced to prevent the system from getting stuck in a loop.
        For i = 1 To 20
            'Makes the AI's calculated Best Move (from this position) on the temporary board.
            If isWhite Then
                MakeMove(TempBoard, TempMove.OldMoveX, TempMove.OldMoveY, TempMove.NewMoveX, TempMove.NewMoveY, TempWCanCastle, TempWKPos, TempMaterialCount, TempEnPassant, TempZobristValue)
            Else
                MakeMove(TempBoard, TempMove.OldMoveX, TempMove.OldMoveY, TempMove.NewMoveX, TempMove.NewMoveY, TempBCanCastle, TempBKPos, TempMaterialCount, TempEnPassant, TempZobristValue)
            End If

            'Hashes the current position, then finds the TranspositionTable entry containing that move.
            TempTTEntry = New TTEntry
            EntryInTT = TempZobristValue >> 44
            If TranspositionTable(EntryInTT) IsNot Nothing Then
                For n = 0 To TranspositionTable(EntryInTT).Count - 1
                    If (TranspositionTable(EntryInTT))(n).Key = TempZobristValue Then
                        'Position found in transposition table - store this.
                        TempTTEntry = (TranspositionTable(EntryInTT))(n)
                        Exit For
                    End If
                Next
            Else
                'No position was found from the previous move - must be end of sequence.
                Exit For
            End If

            If TempTTEntry.IsPopulated AndAlso TempTTEntry.BestMoveOld IsNot Nothing Then
                'We have stored a move in this position - retrieve this move, then add the PGN version of it to BestLine.
                TempMove.OldMoveX = TempTTEntry.BestMoveOld(0)
                TempMove.OldMoveY = TempTTEntry.BestMoveOld(1)
                TempMove.NewMoveX = TempTTEntry.BestMoveNew(0)
                TempMove.NewMoveY = TempTTEntry.BestMoveNew(1)
                BestLine &= "," & MoveConverter(TempBoard, TempMove, TempEnPassant)
                isWhite = Not isWhite
            Else
                'This position is empty - exit the loop.
                Exit For
            End If

            If i = 20 Then BestLine &= ".." 'Move limit reached.
        Next

        Return BestLine & "."
    End Function


    'Subroutine which outputs the lifetime stats of the AI to the AIStats file.
    Public Sub OutputStatsToFile()
        Using SR As New StreamWriter(AppDomain.CurrentDomain.BaseDirectory & "\Assets\AIStats.txt")
            SR.WriteLine(LifetimePositions)
            SR.WriteLine(LifetimeTranspositions)
            SR.WriteLine(LifetimeCheckmates)
        End Using
    End Sub




    'Function that returns all the legal moves of a given piece on the board.
    'Used for when the user is attempting to move a piece on the GUI.
    Public Function ReturnPiecesLegalMoves(ByVal CoorX As String, ByVal CoorY As String) As String()
        Dim LegalMoves(27) As String
        If HasBeenInstantiated Then
            Dim TotalMoves As Byte = 0
            'Creates the pseudo-legal moves for the chosen player.
            Dim PieceMoves() As String
            If PlayerTurn AndAlso Char.IsUpper(PrimaryBoard(CoorX, CoorY)) Then
                PieceMoves = WhitePieceLegalMoves(PrimaryBoard, CoorX, CoorY, PrimaryTFTable, PrimaryMeInCheck, PrimaryMeCanCastle, PrimaryEnPassant)
            ElseIf Not (PlayerTurn OrElse Char.IsUpper(PrimaryBoard(CoorX, CoorY))) Then
                PieceMoves = BlackPieceLegalMoves(PrimaryBoard, CoorX, CoorY, PrimaryTFTable, PrimaryMeInCheck, PrimaryMeCanCastle, PrimaryEnPassant)
            Else
                Console.ForegroundColor = ConsoleColor.DarkRed
                Console.WriteLine("Error - Illegal to Move Piece.")
                Console.ResetColor()
                LegalMoves(0) = TotalMoves
                Return LegalMoves
            End If
            If PieceMoves IsNot Nothing Then 'If any move exists...
                Dim TempMeInCheck As New InCheck 'Creates temp check class.
                For n = 0 To PieceMoves.Length - 1 'for each move...
                    'If the user is in check, eliminate the moves that do not escape check. King moves are not considered
                    'as the player's TFTable will ensure that all king moves are legal.
                    If PrimaryMeInCheck.IsInCheck AndAlso PrimaryBoard(CoorX, CoorY) <> KingSymbol Then
                        'Runs move through the DoesMoveResolveCheck algorithm.
                        TempMeInCheck.CopyFrom(PrimaryMeInCheck)
                        If DoesMoveResolveCheck(PrimaryBoard, PlayerTurn, CoorX, CoorY, PieceMoves(n)(0), PieceMoves(n)(1), TempMeInCheck, PrimaryEnPassant) Then
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
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when Attempting Search - FEN Position not set.")
            Console.ResetColor()
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
            If BasePieceMoves IsNot Nothing Then 'If any move exists...
                Dim TempMeInCheck As New InCheck 'Creates temp check class.
                For n = 0 To BasePieceMoves.GetUpperBound(0) 'for each move...
                    'If the user is in check, eliminate the moves that do not escape check. King moves are not considered
                    'as the player's TFTable will ensure that all king moves are legal.
                    If PrimaryMeInCheck.IsInCheck AndAlso PrimaryBoard(Val(BasePieceMoves(n, 0)(0)), Val(BasePieceMoves(n, 0)(1))) <> KingSymbol Then
                        'Runs move through the DoesMoveResolveCheck algorithm.
                        TempMeInCheck.CopyFrom(PrimaryMeInCheck)
                        If DoesMoveResolveCheck(PrimaryBoard, PlayerTurn, BasePieceMoves(n, 0)(0), BasePieceMoves(n, 0)(1), BasePieceMoves(n, 1)(0), BasePieceMoves(n, 1)(1), TempMeInCheck, PrimaryEnPassant) Then
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
                            CurrentMove.OldMoveX = BasePieceMoves(n, 0)(0)
                            CurrentMove.OldMoveY = BasePieceMoves(n, 0)(1)
                            CurrentMove.NewMoveX = BasePieceMoves(n, 1)(0)
                            CurrentMove.NewMoveY = BasePieceMoves(n, 1)(1)
                            CurrentMove.Code = "o"
                        End If
                    End If
                Next
            ElseIf Not PrimaryMeInCheck.IsInCheck Then
                'Position is a stalemate.
                CurrentMove.Code = "s"
            End If
        Else 'AI not correctly instantiated.
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when Attempting Search - FEN Position not set.")
            Console.ResetColor()
            CurrentMove.Code = "a"
        End If
        Return CurrentMove
    End Function



    'Function which creates and orders all the pseudo-legal moves a player can make, given certain criteria.
    Public Function CreateMoves(ByVal Board(,) As Char, ByVal isWhite As Boolean, ByVal TrueFalseTable(,) As Char, ByVal KPos As String, ByVal InCheck As InCheck, ByVal CanCastle As CanCastle, ByVal EnPassant As String, ByVal OnlyCaptures As Boolean, ByVal KillerDepth As SByte, ByVal TTMoveOld As String, ByVal TTMoveNew As String) As String(,)
        Dim ArrLens(7) As Byte 'Represents the number of moves in each move category array.

        'Creates variables for finding the Transposition Table's Best Move from the current position.
        Dim UseTTMove As Boolean = (TTMoveOld <> "")
        Dim PieceMayBeTTMove As Boolean
        Dim TTMoveFoundInPosition As Byte

        Dim TempKillerMoves(1, 1) As String
        'Variables that handle the KillerMoves array - first determines if the correct KillerMove index is empty.
        Dim PieceMayBeKillerOne, PieceMayBeKillerTwo As Boolean
        Dim PieceKillerOneFull As Boolean = KillerMoves(KillerDepth, 0) IsNot Nothing
        Dim PieceKillerTwoFull As Boolean = KillerMoves(KillerDepth, 1) IsNot Nothing
        Dim KillerMoveCount As Byte


        Dim PieceValueDif As Int16 'Difference in weight between the capturing piece, and the piece being captured.
        If isWhite Then
            For y = 0 To 7
                For x = 0 To 7
                    If Char.IsUpper(Board(x, y)) Then
                        'Generates the moves that the piece can make.
                        Dim PieceMoves() As String = WhitePieceLegalMoves(Board, x, y, TrueFalseTable, InCheck, CanCastle, EnPassant)

                        If PieceMoves IsNot Nothing Then 'If there are any moves...
                            If UseTTMove AndAlso x & y = TTMoveOld Then PieceMayBeTTMove = True Else PieceMayBeTTMove = False
                            'Determines if the piece that is moving could be a match in KillerMoves() - if the coordinates of
                            'the moving piece match the first two digits of the required index in KillerMoves() then one of
                            'the piece's moves may be a match. If not, we don't involve killer moves in the move ordering algorithm.
                            If PieceKillerOneFull AndAlso x & y = KillerMoves(KillerDepth, 0).Substring(0, 2) Then PieceMayBeKillerOne = True Else PieceMayBeKillerOne = False
                            If PieceKillerTwoFull AndAlso x & y = KillerMoves(KillerDepth, 1).Substring(0, 2) Then PieceMayBeKillerTwo = True Else PieceMayBeKillerTwo = False
                            For n = 0 To PieceMoves.Length - 1 'for each move...

                                'Checks if the move is the stored move in the Transposition Table.
                                If PieceMayBeTTMove AndAlso PieceMoves(n) = TTMoveNew Then
                                    'Move found - stop any other moves from being checked against TTMove.
                                    TTMoveFoundInPosition = 1
                                    UseTTMove = False
                                    PieceMayBeTTMove = False
                                Else
                                    If Board(Val(PieceMoves(n)(0)), Val(PieceMoves(n)(1))) <> " " Then '= capture move.
                                        'Gets the difference in weight between the capturing piece, and the captured piece.
                                        PieceValueDif = ReturnPieceValue(UCase(Board(Val(PieceMoves(n)(0)), Val(PieceMoves(n)(1))))) - ReturnPieceValue(Board(x, y))
                                        If PieceValueDif >= 0 Then 'Capturing piece weighs less than captured piece.
                                            'Ammend move list.
                                            AmazingCaptureMoves(ArrLens(0), 0) = x & y
                                            AmazingCaptureMoves(ArrLens(0), 1) = PieceMoves(n)
                                            ArrLens(0) += 1
                                        ElseIf PieceValueDif = 0 Then 'Capturing piece weighs the same as captured piece.
                                            'Ammend move list.
                                            CaptureMoves(ArrLens(1), 0) = x & y
                                            CaptureMoves(ArrLens(1), 1) = PieceMoves(n)
                                            ArrLens(1) += 1
                                        Else 'Capturing piece weighs more than captured piece.
                                            'Ammend move list.
                                            OtherCaptureMoves(ArrLens(2), 0) = x & y
                                            OtherCaptureMoves(ArrLens(2), 1) = PieceMoves(n)
                                            ArrLens(2) += 1
                                        End If
                                    ElseIf Not OnlyCaptures Then 'Non-Capture moves are not considered when Quiescence mode has been activated.
                                        'Determines if the move is a direct match with the required index in KillerMoves(). If there is one,
                                        'the move is added to the TempKillerMoves
                                        If PieceMayBeKillerOne AndAlso x & y & PieceMoves(n) = KillerMoves(KillerDepth, 0) Then
                                            TempKillerMoves(0, 0) = x & y
                                            TempKillerMoves(0, 1) = PieceMoves(n)
                                            'Prevents more moves from being added to this index.
                                            PieceKillerOneFull = False
                                            PieceMayBeKillerOne = False
                                            KillerMoveCount += 1
                                        ElseIf PieceMayBeKillerTwo AndAlso x & y & PieceMoves(n) = KillerMoves(KillerDepth, 1) Then
                                            TempKillerMoves(1, 0) = x & y
                                            TempKillerMoves(1, 1) = PieceMoves(n)
                                            PieceKillerTwoFull = False
                                            PieceMayBeKillerTwo = False
                                            KillerMoveCount += 1
                                        Else
                                            If Board(x, y) = "P" AndAlso Val(PieceMoves(n)(1)) <= 1 Then 'User is promoting a pawn (or is very close to).
                                                'Ammend move list.
                                                PawnPromotionMoves(ArrLens(3), 0) = x & y
                                                PawnPromotionMoves(ArrLens(3), 1) = PieceMoves(n)
                                                ArrLens(3) += 1
                                            ElseIf Val(PieceMoves(n)(1)) >= 2 AndAlso (Board(Math.Max(Val(PieceMoves(n)(0)) - 1, 0), Val(PieceMoves(n)(1)) - 1) = "p" OrElse Board(Math.Min(Val(PieceMoves(n)(0)) + 1, 7), Val(PieceMoves(n)(1)) - 1) = "p") Then
                                                'New square is controlled by an enemy pawn - ammend move list.
                                                TerribleMoves(ArrLens(7), 0) = x & y
                                                TerribleMoves(ArrLens(7), 1) = PieceMoves(n)
                                                ArrLens(7) += 1
                                            ElseIf Math.Max(Math.Abs(Val(KPos(0)) - Val(PieceMoves(n)(0))), Math.Abs(Val(KPos(1)) - Val(PieceMoves(n)(1)))) <= 3 Then
                                                'Piece moves to a location close to the enemy king - leading to a possible check / attack.
                                                GoodMoves(ArrLens(4), 0) = x & y
                                                GoodMoves(ArrLens(4), 1) = PieceMoves(n)
                                                ArrLens(4) += 1
                                            ElseIf TrueFalseTable(Val(PieceMoves(n)(0)), Val(PieceMoves(n)(1))) = "F" Then
                                                'Piece is positioned on a "False" on the TFTable, meaning the square is controlled by an enemy piece.
                                                'Ammend move list.
                                                BadMoves(ArrLens(6), 0) = x & y
                                                BadMoves(ArrLens(6), 1) = PieceMoves(n)
                                                ArrLens(6) += 1
                                            Else 'Is a regular move. Ammend move list.
                                                OtherMoves(ArrLens(5), 0) = x & y
                                                OtherMoves(ArrLens(5), 1) = PieceMoves(n)
                                                ArrLens(5) += 1
                                            End If
                                        End If
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
                        Dim PieceMoves() As String = BlackPieceLegalMoves(Board, x, y, TrueFalseTable, InCheck, CanCastle, EnPassant)

                        If PieceMoves IsNot Nothing Then
                            If UseTTMove AndAlso x & y = TTMoveOld Then PieceMayBeTTMove = True Else PieceMayBeTTMove = False
                            If PieceKillerOneFull AndAlso x & y = KillerMoves(KillerDepth, 0).Substring(0, 2) Then PieceMayBeKillerOne = True Else PieceMayBeKillerOne = False
                            If PieceKillerTwoFull AndAlso x & y = KillerMoves(KillerDepth, 1).Substring(0, 2) Then PieceMayBeKillerTwo = True Else PieceMayBeKillerTwo = False
                            For n = 0 To PieceMoves.Length - 1

                                If PieceMayBeTTMove AndAlso PieceMoves(n) = TTMoveNew Then
                                    TTMoveFoundInPosition = 1
                                    UseTTMove = False
                                    PieceMayBeTTMove = False
                                Else
                                    If Board(Val(PieceMoves(n)(0)), Val(PieceMoves(n)(1))) <> " " Then
                                        PieceValueDif = ReturnPieceValue(Board(Val(PieceMoves(n)(0)), Val(PieceMoves(n)(1)))) - ReturnPieceValue(UCase(Board(x, y)))
                                        If PieceValueDif >= 0 Then
                                            AmazingCaptureMoves(ArrLens(0), 0) = x & y
                                            AmazingCaptureMoves(ArrLens(0), 1) = PieceMoves(n)
                                            ArrLens(0) += 1
                                        ElseIf PieceValueDif = 0 Then
                                            CaptureMoves(ArrLens(1), 0) = x & y
                                            CaptureMoves(ArrLens(1), 1) = PieceMoves(n)
                                            ArrLens(1) += 1
                                        Else
                                            OtherCaptureMoves(ArrLens(2), 0) = x & y
                                            OtherCaptureMoves(ArrLens(2), 1) = PieceMoves(n)
                                            ArrLens(2) += 1
                                        End If
                                    ElseIf Not OnlyCaptures Then
                                        If PieceMayBeKillerOne AndAlso x & y & PieceMoves(n) = KillerMoves(KillerDepth, 0) Then
                                            TempKillerMoves(0, 0) = x & y
                                            TempKillerMoves(0, 1) = PieceMoves(n)
                                            PieceKillerOneFull = False
                                            PieceMayBeKillerOne = False
                                            KillerMoveCount += 1
                                        ElseIf PieceMayBeKillerTwo AndAlso x & y & PieceMoves(n) = KillerMoves(KillerDepth, 1) Then
                                            TempKillerMoves(1, 0) = x & y
                                            TempKillerMoves(1, 1) = PieceMoves(n)
                                            PieceKillerTwoFull = False
                                            PieceMayBeKillerTwo = False
                                            KillerMoveCount += 1
                                        Else
                                            If Board(x, y) = "p" AndAlso Val(PieceMoves(n)(1)) >= 6 Then
                                                PawnPromotionMoves(ArrLens(3), 0) = x & y
                                                PawnPromotionMoves(ArrLens(3), 1) = PieceMoves(n)
                                                ArrLens(3) += 1
                                            ElseIf Val(PieceMoves(n)(1)) <= 5 AndAlso (Board(Math.Max(Val(PieceMoves(n)(0)) - 1, 0), Val(PieceMoves(n)(1)) + 1) = "P" OrElse Board(Math.Min(Val(PieceMoves(n)(0)) + 1, 7), Val(PieceMoves(n)(1)) + 1) = "P") Then
                                                TerribleMoves(ArrLens(7), 0) = x & y
                                                TerribleMoves(ArrLens(7), 1) = PieceMoves(n)
                                                ArrLens(7) += 1
                                            ElseIf Math.Max(Math.Abs(Val(KPos(0)) - Val(PieceMoves(n)(0))), Math.Abs(Val(KPos(1)) - Val(PieceMoves(n)(1)))) <= 3 Then
                                                GoodMoves(ArrLens(4), 0) = x & y
                                                GoodMoves(ArrLens(4), 1) = PieceMoves(n)
                                                ArrLens(4) += 1
                                            ElseIf TrueFalseTable(Val(PieceMoves(n)(0)), Val(PieceMoves(n)(1))) = "F" Then
                                                BadMoves(ArrLens(6), 0) = x & y
                                                BadMoves(ArrLens(6), 1) = PieceMoves(n)
                                                ArrLens(6) += 1
                                            Else
                                                OtherMoves(ArrLens(5), 0) = x & y
                                                OtherMoves(ArrLens(5), 1) = PieceMoves(n)
                                                ArrLens(5) += 1
                                            End If
                                        End If
                                    End If
                                End If
                            Next
                        End If
                    End If
                Next
            Next
        End If

        'Creates a new array that represents the total move count.
        Dim TotalMoveCount As Byte = TTMoveFoundInPosition + ArrLens(0) + ArrLens(1) + ArrLens(2) + KillerMoveCount + ArrLens(3) + ArrLens(4) + ArrLens(5) + ArrLens(6) + ArrLens(7)
        If TotalMoveCount = 0 Then Return Nothing 'No moves found in the position.
        'At the end of the function, we merge all the category arrays into one - producing a huge, tiered,
        'list of a player's total pseudo-legal moves. If the user is in Quiescence mode, all non-capture moves
        'are discounted, and are not merged.
        Dim AllMoves(TotalMoveCount - 1, 1) As String

        If TTMoveFoundInPosition = 1 Then
            'Store TTMove at the start, meaning that it will be searched first.
            AllMoves(0, 0) = TTMoveOld
            AllMoves(0, 1) = TTMoveNew
            Array.Copy(AmazingCaptureMoves, 0, AllMoves, 2, 2 * ArrLens(0))
            ArrLens(0) += 1 'Makes sure that all further moves will be stored below AmazingCaptureMoves, as index 0 is left for TT moves.
        Else
            Array.Copy(AmazingCaptureMoves, 0, AllMoves, 0, 2 * ArrLens(0))
        End If

        'Copies all other move arrays to AllMoves(,)
        Array.Copy(CaptureMoves, 0, AllMoves, 2 * ArrLens(0), 2 * ArrLens(1))
        Array.Copy(OtherCaptureMoves, 0, AllMoves, 2 * (ArrLens(0) + ArrLens(1)), 2 * ArrLens(2))
        ArrLens(0) += ArrLens(1) + ArrLens(2)

        If Not OnlyCaptures Then 'Copies all the non-capture moves to the main array.

            'Copies KillerMoves to AllMoves, if any have been found.
            If TempKillerMoves(0, 0) IsNot Nothing Then
                Array.Copy(TempKillerMoves, 0, AllMoves, 2 * ArrLens(0), 2)
                ArrLens(0) += 1
            End If
            If TempKillerMoves(1, 0) IsNot Nothing Then
                Array.Copy(TempKillerMoves, 2, AllMoves, 2 * ArrLens(0), 2)
                ArrLens(0) += 1
            End If


            If ArrLens(3) > 0 Then Array.Copy(PawnPromotionMoves, 0, AllMoves, 2 * ArrLens(0), 2 * ArrLens(3))
            Array.Copy(GoodMoves, 0, AllMoves, 2 * (ArrLens(0) + ArrLens(3)), 2 * ArrLens(4))
            Array.Copy(OtherMoves, 0, AllMoves, 2 * (ArrLens(0) + ArrLens(3) + ArrLens(4)), 2 * ArrLens(5))
            ArrLens(0) += ArrLens(3) + ArrLens(4) + ArrLens(5)
            Array.Copy(BadMoves, 0, AllMoves, 2 * ArrLens(0), 2 * ArrLens(6))
            Array.Copy(TerribleMoves, 0, AllMoves, 2 * (ArrLens(0) + ArrLens(6)), 2 * ArrLens(7))
        End If

        Return AllMoves
    End Function


    'Function which receives a game position and a possible move. The function makes this move on the board (using
    'shortcuts that can only be made on a virtual board), and then generates the possible moves of the attacking
    'piece(s). If the king is no longer being threatened, then the check as been resolved.
    Private Function DoesMoveResolveCheck(ByVal Board(,) As Char, ByVal PlayerTurn As Boolean, ByVal OldPosX As String, ByVal OldPosY As String, ByVal NewPosX As String, ByVal NewPosY As String, ByRef InCheck As InCheck, ByVal EnPassant As String) As Boolean
        'If the player is in a double check, then only king moves could be legal. Therefore, as this part is done in the
        'TFTable generation, we skip this here.
        If Not InCheck.DoubleCheck Then
            If PlayerTurn Then
                If NewPosX & NewPosY = InCheck.Piece OrElse (NewPosX & NewPosY = EnPassant AndAlso Board(OldPosX, OldPosY) = "P") Then
                    'Move captures the attacking piece. Therefore we assume it is legal.
                    Return True
                ElseIf Board(NewPosX, NewPosY) = " " Then
                    InCheck.IsInCheck = False
                    Board(NewPosX, NewPosY) = "O" 'Move made on temporary board.
                    'Calculate the legal moves of the attacking piece. If the king is still in check, then WInCheck.IsInCheck
                    'will flag from False to True - therefore it is illegal.
                    BlackPieceLegalMoves(Board, Val(InCheck.Piece(0)), Val(InCheck.Piece(1)), TrueTable, "00", InCheck, "-")
                    Board(NewPosX, NewPosY) = " " 'Move un-made on temporary board.
                    If InCheck.IsInCheck = False Then Return True
                End If
                'Otherwise, the move is a capture move, but not capturing the attacking piece. Therefore, it has to be illegal.
            Else
                'Identical code but for the black pieces.
                If NewPosX & NewPosY = InCheck.Piece OrElse (NewPosX & NewPosY = EnPassant AndAlso Board(OldPosX, OldPosY) = "p") Then
                    Return True
                ElseIf Board(NewPosX, NewPosY) = " " Then
                    InCheck.IsInCheck = False
                    Board(NewPosX, NewPosY) = "o"
                    WhitePieceLegalMoves(Board, Val(InCheck.Piece(0)), Val(InCheck.Piece(1)), TrueTable, "00", InCheck, "-")
                    Board(NewPosX, NewPosY) = " "
                    If InCheck.IsInCheck = False Then Return True
                End If
            End If
        End If
        Return False
    End Function


    'Subroutine that makes a move on the board, given coordinates. Includes castling (& rights), pawn promotion, and manipuation of ZobristValue.
    Private Sub MakeMove(ByVal Board(,) As Char, ByVal OldCoorX As String, ByVal OldCoorY As String, ByVal NewCoorX As String, ByVal NewCoorY As String, ByRef CanCastle As CanCastle, ByRef KPos As String, ByRef MaterialCount() As Int16, ByRef EnPassant As String, ByRef ZobristValue As UInt64)
        Dim TempPiece As Char = Board(OldCoorX, OldCoorY)
        Dim HasEnPassanted As Boolean
        If Char.IsUpper(TempPiece) Then
            'Removes the piece from the board's Zobrist Value.
            ZobristValue = ZobristValue Xor ZobristHashTable(Asc(TempPiece) Mod 11, 0, OldCoorX, OldCoorY)
            If TempPiece = "P" Then
                'Code for Promoting Pawns and En Passant. Also increments the material count.
                If NewCoorY = 0 Then
                    TempPiece = "Q"
                    MaterialCount(0) += ReturnPieceValue("Q") - ReturnPieceValue("P") '+ 9 for a new queen, - 1 for losing the pawn in the process.
                ElseIf NewCoorX & NewCoorY = EnPassant Then
                    Board(NewCoorX, 3) = " "
                    ZobristValue = ZobristValue Xor ZobristHashTable(3, 1, NewCoorX, 3) 'Ammended for a capture of a pawn.
                    MaterialCount(1) -= ReturnPieceValue("P")
                ElseIf OldCoorY = 6 AndAlso NewCoorY = 4 AndAlso (Board(Math.Max(NewCoorX - 1, 0), 4) = "p" OrElse Board(Math.Min(NewCoorX + 1, 7), 4) = "p") Then
                    'EnPassant creation.
                    If EnPassant <> "-" Then ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, Val(EnPassant(0)), 2)
                    EnPassant = NewCoorX & 5
                    ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, NewCoorX, 5) 'Ammended for a capture of a pawn.
                    HasEnPassanted = True
                End If
                'If piece is a Rook, part of Castling is disabled (depending on which Rook has moved).
            ElseIf TempPiece = "R" AndAlso CanCastle.CanICastle Then
                If CanCastle.KS AndAlso OldCoorX = 7 AndAlso OldCoorY = 7 Then
                    'Rook has been moved - the player can no longer castle that side of the board.
                    CanCastle.KS = False
                    ZobristValue = ZobristValue Xor HashConstants(1) 'Ammends the Zobrist Value for that player no longer being able to castle.
                ElseIf CanCastle.QS AndAlso OldCoorX = 0 AndAlso OldCoorY = 7 Then
                    CanCastle.QS = False
                    ZobristValue = ZobristValue Xor HashConstants(2)
                End If
            ElseIf TempPiece = "K" Then
                KPos = NewCoorX & NewCoorY
                'Code for Castling.
                If CanCastle.KS Then
                    If NewCoorX = 6 AndAlso NewCoorY = 7 Then
                        'Moves elements about on the board, and the Zobrist value.
                        Board(5, 7) = "R"
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 0, 5, 7)
                        Board(7, 7) = " "
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 0, 7, 7)
                    End If
                    'Player can no longer castle.
                    CanCastle.KS = False
                    ZobristValue = ZobristValue Xor HashConstants(1)
                End If
                If CanCastle.QS Then
                    If NewCoorX = 2 AndAlso NewCoorY = 7 Then
                        Board(0, 7) = " "
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 0, 0, 7)
                        Board(3, 7) = "R"
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 0, 3, 7)
                    End If
                    ZobristValue = ZobristValue Xor HashConstants(2)
                    CanCastle.QS = False
                End If
            End If
            'Places the piece on the board's new position using its Zobrist value.
            ZobristValue = ZobristValue Xor ZobristHashTable(Asc(TempPiece) Mod 11, 0, NewCoorX, NewCoorY)
        Else
            'Near-identical Code for the Black Pieces.
            ZobristValue = ZobristValue Xor ZobristHashTable((Asc(TempPiece) + 1) Mod 11, 1, OldCoorX, OldCoorY)
            If TempPiece = "p" Then
                If NewCoorY = 7 Then
                    TempPiece = "q"
                    MaterialCount(1) += ReturnPieceValue("Q") - ReturnPieceValue("P")
                ElseIf NewCoorX & NewCoorY = EnPassant Then
                    Board(NewCoorX, 4) = " "
                    ZobristValue = ZobristValue Xor ZobristHashTable(3, 0, NewCoorX, 4)
                    MaterialCount(0) -= ReturnPieceValue("P")
                ElseIf OldCoorY = 1 AndAlso NewCoorY = 3 AndAlso (Board(Math.Max(NewCoorX - 1, 0), 3) = "P" OrElse Board(Math.Min(NewCoorX + 1, 7), 3) = "P") Then
                    If EnPassant <> "-" Then ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, Val(EnPassant(0)), 5)
                    EnPassant = NewCoorX & 2
                    ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, NewCoorX, 2)
                    HasEnPassanted = True
                End If
            ElseIf TempPiece = "r" AndAlso CanCastle.CanICastle Then
                If CanCastle.KS AndAlso OldCoorX = 7 AndAlso OldCoorY = 0 Then
                    CanCastle.KS = False
                    ZobristValue = ZobristValue Xor HashConstants(3)
                ElseIf CanCastle.QS AndAlso OldCoorX = 0 AndAlso OldCoorY = 0 Then
                    CanCastle.QS = False
                    ZobristValue = ZobristValue Xor HashConstants(4)
                End If
            ElseIf TempPiece = "k" Then
                KPos = NewCoorX & NewCoorY
                If CanCastle.KS Then
                    If NewCoorX = 6 AndAlso NewCoorY = 7 Then
                        Board(5, 0) = "r"
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 1, 5, 0)
                        Board(7, 0) = " "
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 1, 7, 0)
                    End If
                    CanCastle.KS = False
                    ZobristValue = ZobristValue Xor HashConstants(3)
                End If
                If CanCastle.QS Then
                    If NewCoorX = 2 AndAlso NewCoorY = 7 Then
                        Board(0, 0) = " "
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 1, 0, 0)
                        Board(3, 0) = "r"
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 1, 3, 0)
                    End If
                    ZobristValue = ZobristValue Xor HashConstants(4)
                    CanCastle.QS = False
                End If
            End If
            ZobristValue = ZobristValue Xor ZobristHashTable((Asc(TempPiece) + 1) Mod 11, 1, NewCoorX, NewCoorY)
        End If

        If Not (EnPassant = "-" OrElse HasEnPassanted) Then
            'Removal of EnPassant.
            ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, Val(EnPassant(0)), Val(EnPassant(1)))
            EnPassant = "-"
        End If
        'At the end of the subroutine, the Piece is placed at the new coordinates, and the old position is cleared.
        'If the new position contains a piece, then the material count is updated for only that piece.
        If Board(NewCoorX, NewCoorY) <> " " Then
            Dim CapturedPiece As Char = Board(NewCoorX, NewCoorY)
            If Char.IsUpper(CapturedPiece) Then
                MaterialCount(0) -= ReturnPieceValue(CapturedPiece)
                'Removes the captured piece from the Zobrist Key.
                ZobristValue = ZobristValue Xor ZobristHashTable(Asc(CapturedPiece) Mod 11, 0, NewCoorX, NewCoorY)
            Else
                MaterialCount(1) -= ReturnPieceValue(UCase(CapturedPiece))
                ZobristValue = ZobristValue Xor ZobristHashTable((Asc(CapturedPiece) + 1) Mod 11, 1, NewCoorX, NewCoorY)
            End If
        End If
        Board(NewCoorX, NewCoorY) = TempPiece
        Board(OldCoorX, OldCoorY) = " "
        'Changes the player to move on the Zobrist Key.
        ZobristValue = ZobristValue Xor HashConstants(0)
    End Sub




    'This function contains my MiniMax algorithm using Alpha-Beta Pruning, Transpotition Tables, and Quiescence (optional).
    Private Function MiniMax(ByVal Board(,) As Char, ByVal depth As SByte, ByVal isWhite As Boolean, ByVal WCanCastle As CanCastle, ByVal BCanCastle As CanCastle, ByVal WKPos As String, ByVal BKPos As String, ByVal EnPassant As String, ByVal MaterialCount() As Int16, ByVal ZobristValue As UInt64, ByVal Alpha As Int16, ByVal Beta As Int16) As Int16
        If ABORT Then Return 0
        TotalPositionsSearched += 1
        HighestQuiescenceDepth = Math.Min(HighestQuiescenceDepth, depth)
        'Creates moves & checking rules.
        Dim CurrentMove, BestMove As Int16
        Dim WInCheck, BInCheck As New InCheck

        Dim TempTTEntry As New TTEntry
        'Hashes Zobrist key to find the location of the Entry in the Table.
        Dim EntryInTT As UInt32 = ZobristValue >> 44
        Dim IndexInTT As Byte
        'Searches for position in TranspositionTable.
        If TranspositionTable(EntryInTT) IsNot Nothing Then
            For n = 0 To TranspositionTable(EntryInTT).Count - 1
                If (TranspositionTable(EntryInTT))(n).Key = ZobristValue Then
                    'Entry has been found - copy this to TempTTEntry.
                    IndexInTT = n
                    TempTTEntry = (TranspositionTable(EntryInTT))(n)
                    Exit For
                End If
            Next
        ElseIf depth > 0 Then
            'Instantiates that index of the Transposition Table.
            TranspositionTable(EntryInTT) = New List(Of TTEntry)
        End If


        If TempTTEntry.IsPopulated Then
            'Code for exiting the MiniMax branch early if it can successfully retrieve a correct score from that entry in TT.
            'Note that the depth of the stored position must be greater than that of the current position, as otherwise the
            'evalutation of the stored entry will be less accurate than performing the search.
            If TempTTEntry.Flag = 5 Then CheckmatesFound += 1 : Return TempTTEntry.Score 'Position has already been calculated as an end-state - return this position.
            If TempTTEntry.Depth >= depth Then
                If TempTTEntry.Flag = 4 Then TranspositionsFound += 1 : Return 0 'Position has been repeated - return draw score.

                If TempTTEntry.Flag = 0 Then 'Value stored is exact evaluation - return this score.
                    TranspositionsFound += 1
                    Return TempTTEntry.Score
                    'Otherwise, is either a lower or upper bound - therefore must be within the bounds of Alpha & Beta to be assumed valid.
                ElseIf TempTTEntry.Flag = 1 Then
                    If TempTTEntry.Score >= Beta Then TranspositionsFound += 1 : Return TempTTEntry.Score
                ElseIf TempTTEntry.Flag = 2 Then
                    If TempTTEntry.Score <= Alpha Then TranspositionsFound += 1 : Return TempTTEntry.Score
                End If

            End If
            If depth > 0 Then
                TempTTEntry.Flag = 4 'flags that the position is currently being searched on - if child nodes also detect this position,
                'then we return a draw (three-fold repetition).
                TempTTEntry.Depth = depth
                TranspositionTable(EntryInTT)(IndexInTT) = TempTTEntry 'Replaces entry.
            Else
                'Make sure recommended move isn't passed into the move generation code (as move may not be a capture move).
                TempTTEntry.BestMoveOld = ""
                TempTTEntry.BestMoveNew = ""
            End If

        ElseIf depth > 0 Then
            'Creates a new TTEntry for the current position, as it could not be found in the Transposition Table.
            TempTTEntry.IsPopulated = True
            TempTTEntry.Key = ZobristValue
            TempTTEntry.Flag = 4
            TempTTEntry.Depth = depth
            IndexInTT = TranspositionTable(EntryInTT).Count
            TranspositionTable(EntryInTT).Add(TempTTEntry)
        End If


        If isWhite Then
            'Creates and forms the TFTable for the player to move.
            Dim WhiteTFTable(7, 7) As Char
            FixTFTables(Board, True, WhiteTFTable, WKPos, WInCheck, EnPassant)
            If Not (depth > 0 OrElse WInCheck.IsInCheck) Then 'Quiescence mode activated.
                'Evaluation of board is the current move to beat.
                BestMove = Evaluate(Board, MaterialCount, WKPos, BKPos)
                Alpha = Math.Max(Alpha, BestMove)
                If Beta <= Alpha Then 'ABP.
                    Return BestMove
                End If
            Else
                BestMove = -32767
            End If

            'Assumes Flag to be Upper bound, unless proven otherwise.
            TempTTEntry.Flag = 2
            'Creates the pseudo-legal moves for the chosen player.
            Dim PieceMoves(,) As String = CreateMoves(Board, True, WhiteTFTable, BKPos, WInCheck, WCanCastle, EnPassant, Not (depth > 0 OrElse WInCheck.IsInCheck), MasterDepth - depth, TempTTEntry.BestMoveOld, TempTTEntry.BestMoveNew) 'If Quiescence mode is activated then use capture moves only.
            If PieceMoves IsNot Nothing Then 'If any move exists...
                'Creates temp variables.
                Dim TempBoard(7, 7) As Char
                Dim TempMaterialCount(1) As Int16
                Dim TempWKPos As String
                Dim TempWCanCastle As New CanCastle
                Dim TempWInCheck As New InCheck
                Dim TempEnPassant As String
                Dim TempZobristValue As UInt64

                For n = 0 To PieceMoves.GetUpperBound(0) 'for each move...
                    'If the user is in check, eliminate the moves that do not escape check. King moves are not considered
                    'as the player's TFTable will ensure that all king moves are legal.
                    If WInCheck.IsInCheck AndAlso Board(Val(PieceMoves(n, 0)(0)), Val(PieceMoves(n, 0)(1))) <> "K" Then
                        'Runs move through the DoesMoveResolveCheck algorithm.
                        TempWInCheck.CopyFrom(WInCheck)
                        If DoesMoveResolveCheck(Board, True, PieceMoves(n, 0)(0), PieceMoves(n, 0)(1), PieceMoves(n, 1)(0), PieceMoves(n, 1)(1), TempWInCheck, EnPassant) Then
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
                        TempZobristValue = ZobristValue
                        'Makes move on temp board, then calls MiniMax for this new position.
                        MakeMove(TempBoard, PieceMoves(n, 0)(0), PieceMoves(n, 0)(1), PieceMoves(n, 1)(0), PieceMoves(n, 1)(1), TempWCanCastle, TempWKPos, TempMaterialCount, TempEnPassant, TempZobristValue)
                        If TempMaterialCount(0) + TempMaterialCount(1) = 0 Then
                            'Enforce draw by repetition.
                            TotalPositionsSearched += 1
                            HighestQuiescenceDepth = Math.Min(HighestQuiescenceDepth, depth - 1)
                            CurrentMove = 0
                        ElseIf Not SearchSettings.UseQuiescence AndAlso depth = 1 Then
                            'We have reached a leaf position - return the evaluation for this position.
                            TotalPositionsSearched += 1
                            HighestQuiescenceDepth = Math.Min(HighestQuiescenceDepth, depth - 1)
                            CurrentMove = Evaluate(TempBoard, TempMaterialCount, TempWKPos, BKPos) 'Evaluate position for opponent.
                        Else 'No leaf node or drawn position (or are using Quiescence) - put position through MiniMax recursively.
                            CurrentMove = MiniMax(TempBoard, depth - 1, False, TempWCanCastle, BCanCastle, TempWKPos, BKPos, TempEnPassant, TempMaterialCount, TempZobristValue, Alpha, Beta)
                        End If

                        If ABORT Then
                            'Removes the current position's entry in the Transposition Table (to prevent leftover "4"
                            'entries from affecting future searches), then exits search immediately.
                            If depth > 0 Then TranspositionTable(EntryInTT).RemoveAt(IndexInTT)
                            Return 0
                        End If

                        If CurrentMove > BestMove Then
                            BestMove = CurrentMove 'Best Move has been beaten - replace it.

                            If depth > 0 Then
                                'Updates the best move in the Transposition Table entry to match this new, best move.
                                TempTTEntry.BestMoveOld = PieceMoves(n, 0)
                                TempTTEntry.BestMoveNew = PieceMoves(n, 1)
                                'We now have proof that the move is not an upper bound, as the move was able to produce
                                'a valid evaluation - replace it with 'exact'.
                                If Not SearchSettings.StableSearch Then TempTTEntry.Flag = 0
                            End If

                            Alpha = Math.Max(Alpha, BestMove) 'Alpha = best move found for white.
                            'Move was too strong for player; opponent will not choose this branch.
                            If Beta <= Alpha Then
                                If Board(Val(PieceMoves(n, 1)(0)), Val(PieceMoves(n, 1)(1))) = " " AndAlso (KillerMoves(MasterDepth - depth, 0) <> PieceMoves(n, 0) & PieceMoves(n, 1)) Then
                                    'The pruned move is not a capture move - add move to KillerMoves(), in the hope that the move
                                    'is also possible in sibling positions. If this move is detected, it is searched earlier.
                                    KillerMoves(MasterDepth - depth, 1) = KillerMoves(MasterDepth - depth, 0)
                                    KillerMoves(MasterDepth - depth, 0) = PieceMoves(n, 0) & PieceMoves(n, 1)
                                End If

                                If depth > 0 Then
                                    'Store position & its detail in the Transposition Table.
                                    TempTTEntry.Score = BestMove
                                    TempTTEntry.Flag = 1
                                    'Caused an Alpha-beta cutoff, so the move may be even better than its score suggests - mark as lower bound.
                                    TranspositionTable(EntryInTT)(IndexInTT) = TempTTEntry 'Replaces entry.
                                End If

                                Return BestMove 'Alpha-Beta Pruning - return best move.
                            End If
                        End If
                    End If
                Next
            End If
        Else 'Near-identical code for the black side.

            Dim BlackTFTable(7, 7) As Char
            FixTFTables(Board, False, BlackTFTable, BKPos, BInCheck, EnPassant)
            If Not (depth > 0 OrElse BInCheck.IsInCheck) Then
                BestMove = Evaluate(Board, MaterialCount, WKPos, BKPos)
                Beta = Math.Min(Beta, BestMove)
                If Beta <= Alpha Then
                    Return BestMove
                End If
            Else
                BestMove = 32767
            End If

            TempTTEntry.Flag = 1
            Dim PieceMoves(,) As String = CreateMoves(Board, False, BlackTFTable, WKPos, BInCheck, BCanCastle, EnPassant, Not (depth > 0 OrElse BInCheck.IsInCheck), MasterDepth - depth, TempTTEntry.BestMoveOld, TempTTEntry.BestMoveNew)
            If PieceMoves IsNot Nothing Then
                Dim TempBoard(7, 7) As Char
                Dim TempMaterialCount(1) As Int16
                Dim TempBKPos As String
                Dim TempBCanCastle As New CanCastle
                Dim TempBInCheck As New InCheck
                Dim TempEnPassant As String
                Dim TempZobristValue As UInt64

                For n = 0 To PieceMoves.GetUpperBound(0) 'for each move...
                    If BInCheck.IsInCheck AndAlso Board(Val(PieceMoves(n, 0)(0)), Val(PieceMoves(n, 0)(1))) <> "k" Then
                        TempBInCheck.CopyFrom(BInCheck)
                        If DoesMoveResolveCheck(Board, False, PieceMoves(n, 0)(0), PieceMoves(n, 0)(1), PieceMoves(n, 1)(0), PieceMoves(n, 1)(1), TempBInCheck, EnPassant) Then
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
                        TempZobristValue = ZobristValue
                        MakeMove(TempBoard, PieceMoves(n, 0)(0), PieceMoves(n, 0)(1), PieceMoves(n, 1)(0), PieceMoves(n, 1)(1), TempBCanCastle, TempBKPos, TempMaterialCount, TempEnPassant, TempZobristValue)
                        If TempMaterialCount(0) + TempMaterialCount(1) = 0 Then
                            TotalPositionsSearched += 1
                            HighestQuiescenceDepth = Math.Min(HighestQuiescenceDepth, depth - 1)
                            CurrentMove = 0
                        ElseIf Not SearchSettings.UseQuiescence AndAlso depth = 1 Then
                            TotalPositionsSearched += 1
                            HighestQuiescenceDepth = Math.Min(HighestQuiescenceDepth, depth - 1)
                            CurrentMove = Evaluate(TempBoard, TempMaterialCount, WKPos, TempBKPos)
                        Else
                            CurrentMove = MiniMax(TempBoard, depth - 1, True, WCanCastle, TempBCanCastle, WKPos, TempBKPos, TempEnPassant, TempMaterialCount, TempZobristValue, Alpha, Beta)
                        End If

                        If ABORT Then
                            If depth > 0 Then TranspositionTable(EntryInTT).RemoveAt(IndexInTT)
                            Return 0
                        End If

                        If CurrentMove < BestMove Then
                            BestMove = CurrentMove
                            If depth > 0 Then
                                TempTTEntry.BestMoveOld = PieceMoves(n, 0)
                                TempTTEntry.BestMoveNew = PieceMoves(n, 1)
                                If Not SearchSettings.StableSearch Then TempTTEntry.Flag = 0
                            End If

                            Beta = Math.Min(Beta, BestMove) 'Beta = best move found for white.
                            If Beta <= Alpha Then
                                If Board(Val(PieceMoves(n, 1)(0)), Val(PieceMoves(n, 1)(1))) = " " AndAlso (KillerMoves(MasterDepth - depth, 0) <> PieceMoves(n, 0) & PieceMoves(n, 1)) Then
                                    KillerMoves(MasterDepth - depth, 1) = KillerMoves(MasterDepth - depth, 0)
                                    KillerMoves(MasterDepth - depth, 0) = PieceMoves(n, 0) & PieceMoves(n, 1)
                                End If

                                If depth > 0 Then
                                    TempTTEntry.Score = BestMove
                                    TempTTEntry.Flag = 2
                                    TranspositionTable(EntryInTT)(IndexInTT) = TempTTEntry
                                End If

                                Return BestMove
                            End If
                        End If
                    End If
                Next
            End If
        End If

        If Math.Abs(BestMove) = 32767 Then
            'No legal move found for the player.
            If WInCheck.IsInCheck Then
                'Checkmate for white: return -(30000 + depth) so that the quickest path to checkmate is chosen by the AI.
                BestMove = -(30000 - (MasterDepth - depth))
                CheckmatesFound += 1
            ElseIf BInCheck.IsInCheck Then
                'Checkmate for black: return (30000 + depth) so that the quickest path to checkmate is chosen by the AI.
                BestMove = (30000 - (MasterDepth - depth))
                CheckmatesFound += 1
            Else 'Stalemate: return 0
                BestMove = 0
            End If
            TempTTEntry.Flag = 5 'Represents an end-state in the Transposition Table.
        End If

        'Search completed without Alpha-beta cutoff - store position in Transposition Table.
        If depth > 0 Then
            TempTTEntry.Score = BestMove
            TranspositionTable(EntryInTT)(IndexInTT) = TempTTEntry
        End If

        'Return the best move's score found this iteration.
        Return BestMove
    End Function

    'This algorithm is used to condense a board position into an evaluation score, used to determine best moves.
    'We take into account the difference in material between the two sides, along with a heuristic to help
    'the AI find checkmated in simple endgame positions.
    Private Function Evaluate(ByVal Board(,) As Char, ByVal MaterialCount() As Int16, ByVal WKPos As String, ByVal BKPos As String) As Int16
        'Finds difference in material between both sides.
        Dim Score As Int16 = MaterialCount(0) - MaterialCount(1)

        If SearchSettings.UsePieceHeatMaps Then
            'If the opponent has a lot of material, then score positions where the player's king is safe as being more advantageous.
            If MaterialCount(1) >= 1500 Then Score += PieceHeatMap.HeatSqaures(9, Val(WKPos(1)), Val(WKPos(0)))
            If MaterialCount(0) >= 1500 Then Score -= PieceHeatMap.HeatSqaures(9, 7 - Val(BKPos(1)), Val(BKPos(0)))

            'For each piece on the board, calculate how advantageously it is placed (using the PieceHeatSquares). Add this to the score.
            For y = 0 To 7
                For x = 0 To 7
                    If Not (Board(x, y) = " " OrElse UCase(Board(x, y)) = "K") Then
                        If Char.IsUpper(Board(x, y)) Then
                            Score += PieceHeatMap.HeatSqaures(Asc(Board(x, y)) Mod 11, y, x)
                        Else
                            Score -= PieceHeatMap.HeatSqaures((Asc(Board(x, y)) + 1) Mod 11, 7 - y, x)
                        End If
                    End If
                Next
            Next
        End If

        'If the opponent has little material left, we try to find positions where the opponent king is close to
        'the edge / corner of the board, and where the kings are closer together. This can help find a checkmate.
        If MaterialCount(0) <= 1200 OrElse MaterialCount(1) <= 1200 Then
            'Finds distances between kings.
            Dim KingDistance As Byte = Math.Max(Math.Abs(Val(WKPos(0)) - Val(BKPos(0))), Math.Abs(Val(WKPos(1)) - Val(BKPos(1))))
            Dim KingCentreDistance As Byte
            If MaterialCount(1) <= 1200 Then
                'Finds distance from opponent's king to the centre of the board.
                KingCentreDistance = Math.Max(Val(BKPos(0)) - 4, 3 - Val(BKPos(0))) + Math.Max(Val(BKPos(1)) - 4, 3 - Val(BKPos(1)))
                'Heuristic becomes more prevelant as the opponent has fewer and fewer pieces (exponential curve).
                Score += Math.Round((KingCentreDistance * 1.5 + (7 - KingDistance)) * (1.25 ^ (12 - MaterialCount(1) / 100)))
            End If
            If MaterialCount(0) <= 1200 Then 'Similar code for the white pieces.
                KingCentreDistance = Math.Max(Val(WKPos(0)) - 4, 3 - Val(WKPos(0))) + Math.Max(Val(WKPos(1)) - 4, 3 - Val(WKPos(1)))
                Score -= Math.Round((KingCentreDistance * 1.5 + (7 - KingDistance)) * (1.25 ^ (12 - MaterialCount(0) / 100)))
                'TODO: make calculation more efficient.
            End If
        End If

        Return Score
    End Function

End Class
