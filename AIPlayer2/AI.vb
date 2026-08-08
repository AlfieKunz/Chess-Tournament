'This class contains my Chess AI, which is built using the MiniMax algorithm with Alpha-Beta Pruning.
'This class is modular from my Chess class - being constructed only from the FEN position, and only returning a Move (see the structure below).
'Some other interacting is done, however, such as allowing the AI to be remotely aborted.
Imports System.IO
Imports System.Runtime
Imports System.Text

Partial Public Class AI 'i shall thy the Alfie Alphafish (bit optimistic, I know).
    Inherits CoreMethods

    Private HasBeenInstantiated As Boolean 'The AI will not perform methods if it has not been fully instantiated with a FEN.
    'Below are the details that the AI requires for a search. Please see their counterparts in the Chess form for their info.
    Private PrimaryBoard(7, 7), PrimaryTFTable(7, 7) As Char, MiniMaxTFTable(7, 7) As Char
    Private PrimaryMeCanCastle, PrimaryEnemyCanCastle As New CanCastle

    Private PrimaryMeInCheck As Byte 'Checking data is represented as a set of bits, in the format:
    'CDXXXYYY
    'C = Check Flag. D = Double Check Flag. X = Checking Piece X coors. Y = Checking Piece Y coors.
    Private PrimaryMeKPos, PrimaryEnemyKPos As Byte
    Private PrimaryEnPassant As Byte
    'The above three attributes are represented as a set of bits, where the three LSBs refer to the Y coordinate
    'of the king / square, and the next three bits refer to the X coordinate of the king / square.

    Private PrimaryMaterialCount(1) As UInt16
    Private PlayerTurn As Boolean
    Private BaseZobristValue As UInt64 'Represents the Zorbist Hash Key of the position to be searched on.

    Private BasePieceMoves() As UInt16 'Represents the current legal moves in the position (used so that the moves don't have to
    'be recomputed every time we call the AI).
    'Moves are stored as a 16-bit number to save space, with the following properties:
    'CFFFXXXYYYxxxyyy
    'C = Capture Flag.
    '
    'F = Other Flags: (Mask = 28672)
    '000 = No Flag     001 = Queen Promotion (Mask = 4096)     010 = Pawn Double Push (Mask = 8192)     011 = En-Passant Capture (Mask = 12288)
    '100 = Castle Flag (Mask = 16384)     101 = KS Castle Flag (Mask = 20480)     110 = QS Castle Flag (Mask = 24576)     111 = Knight Promotion Flag (Mask = 28672).
    '
    'X = Start X Coordinate (0-7)  -  Mask = 3584, Shift of 9.
    'Y = Start Y Coordinate (0-7)  -  Mask = 448, Shift of 6.
    'x = End X Coordinate (0-7)  -  Mask = 56, Shift of 3.
    'y = End Y Coordinate (0-7)  -  Mask = 7.



    Private SearchSettings As New AISearchSettings 'Settings of the current search.
    Private TotalPositionsSearched, TranspositionsFound, WinsFound As UInt32 'Numbers showing the stats of the current search.
    Private LifetimePositions, LifetimeTranspositions, LifetimeCheckmates As UInt64 'Numbers showing the lifetime stats of the AI (persists
    'across multiple boot-ups).
    Private HighestQuiescenceDepth, HighestSuccessfulQuiescenceDepth As SByte 'Shows the maximum reached depth of a search that has not been ABORTed.
    Private NodeCount, EndPositionCount, PositionCollisions As UInt64 'Variables containing the stats of a Node Search.
    'NodeCount = Total number of positions searched. EndPositionCount = Total Number of Leaf Nodes searched. PositionCollisions = Number of Positions evaluated multiple times.

    Private ABORT As Boolean 'Controlled by the thread handlers - an AI is aborted if another AI has
    'found a mating pattern, or if the AI has ran out of time. If ABORT is set to True, then the AI finishes its search ASAP.
    Private MasterDepth As SByte 'The Surface Depth of the search, as set by the user.


    Private KillerMoves(255, 1) As UInt16 'Array containing Killer Moves: non-capture moves which caused an alpha-beta cut off.
    'If we detect killer moves in sibling positions (ie: positions of the same depth), we search the Killer Move(s) first.

    'Arrays that the CreateMoves function uses (delared before to save processing time in the search process).
    Private AmazingCaptureMoves(39) As UInt16 'Min size = 19
    Private CaptureMoves(39) As UInt16 'Min size = 19
    Private OtherCaptureMoves(39) As UInt16 'Min size = 19
    Private PawnPromotionMoves(15) As UInt16 'Min size = 7
    Private GoodMoves(99) As UInt16 'Min size = 49
    Private OtherMoves(99) As UInt16 'Min size = 99
    Private BadMoves(74) As UInt16 'Min size = 49
    Private TerribleMoves(74) As UInt16 'Min size = 49


    'Structures for the TranspositionTable - a large table containing the basic details of a position - stored via their Zobrist Hash.
    'Stored as a hashed array so that the overall size of the Table can be reduced (would need to be 2^64 elements large otherwise).
    'This structure uses the 'Greatest Depth' replacement scheme, where each entry is timestamped for 3 moves.
    'Table will contain 2^n entries, where n is the value set in TranspositionTableSize (in the brackets).
    Private TranspositionTable(2 ^ (64 - GlobalConstants.TranspositionTableSize) - 1) As TTEntry
    Private TTIsEmpty As Boolean


    Private Structure TTEntry
        'Dim isPopulated As Boolean
        Dim Key As UInt64 'Zobrist Key of position - used to pinpoint the correct board.
        Dim TimeToLive As Byte
        Dim Depth As SByte
        Dim Flag As Byte 'Represents additional information about the move:
        '0 = Score is exact (no ambiguity), 1 = Lower Bound (score could be higher), 2 = Upper Bound (score could be lower),
        '4 = Position is currently being searched on (for 3RF), 5 = End State (ie: checkmate, stalemate).
        Dim Score As Int16 'Stores the evaluation of the position
        Dim BestMove As UInt16 'Stores the best move found from this position when previously computed.
    End Structure



    'Constructor method.
    Public Sub New()
        Me.New(GlobalConstants.InitialFENPosition)
    End Sub
    Public Sub New(ByVal FEN As String)
        PieceHeatMap = GeneratePieceHeatSquares()
        PopulateEndgameEvalLookupTable()
        'Configures the AI using a given FEN.
        Reconfigure(FEN, True)
        'Loads the Lifetime stats file, and stores the results into their appropriate variables.
        Try
            Using SR As New StreamReader(AppDomain.CurrentDomain.BaseDirectory & "Assets\User\AIStats.txt", Encoding.UTF8, True)
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

    'Subroutine which converts a user's input FEN into all the details needed to conduct a MiniMax search.
    Public Sub Reconfigure(ByVal FEN As String, ByVal ResetTT As Boolean)
        Dim BasePseudoLegalMoves() As UInt16
        'Converts the user's FEN into a board position, then resets checking rules.
        HighestSuccessfulQuiescenceDepth = 1
        PrimaryBoard = FENConverter(FEN, PrimaryMeCanCastle, PrimaryEnemyCanCastle, PrimaryMeKPos, PrimaryEnemyKPos, PrimaryEnPassant, PlayerTurn)
        PrimaryMeInCheck = 0
        If PlayerTurn Then
            'Creates the TFTable for the white pieces, then creates king symbol.
            FixTFTables(PrimaryBoard, True, PrimaryTFTable, PrimaryMeKPos, PrimaryMeInCheck, PrimaryEnPassant)
            BasePseudoLegalMoves = CreateMoves(PrimaryBoard, True, PrimaryTFTable, PrimaryEnemyKPos, PrimaryMeInCheck, PrimaryMeCanCastle, PrimaryEnPassant, False, 0, 0)
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
            Dim TempKPos As Byte = PrimaryMeKPos
            PrimaryMeKPos = PrimaryEnemyKPos
            PrimaryEnemyKPos = TempKPos
            'Creates the TFTable for the black pieces, then creates king symbol.
            FixTFTables(PrimaryBoard, False, PrimaryTFTable, PrimaryMeKPos, PrimaryMeInCheck, PrimaryEnPassant)
            BasePseudoLegalMoves = CreateMoves(PrimaryBoard, False, PrimaryTFTable, PrimaryEnemyKPos, PrimaryMeInCheck, PrimaryMeCanCastle, PrimaryEnPassant, False, 0, 0)
            BaseZobristValue = ZobristHashPosition(PrimaryBoard, False, PrimaryEnemyCanCastle, PrimaryMeCanCastle, PrimaryEnPassant)
        End If


        'Filters the moves in the base position, so that only legal moves exist (rather than pseudo-legal moves).
        If BasePseudoLegalMoves Is Nothing Then
            'No pseudo-legal moves detected - hence no legal moves in the position.
            BasePieceMoves = Nothing
        ElseIf PrimaryMeInCheck >= 128 Then
            Dim TempMeInCheck As Byte
            Dim NoOfLegalMoves As Byte = BasePseudoLegalMoves.Length 'Assume all moves are legal, unless proven otherwise.
            For n As Byte = 0 To BasePseudoLegalMoves.Length - 1 'For each move.
                'If the user is in check, eliminate the moves that do not escape check.
                If (BasePseudoLegalMoves(n) And 4032) >> 6 = PrimaryMeKPos Then
                    'King moves are Not considered, as the player's TFTable will ensure that all king moves are legal.
                    TempMeInCheck = 0
                Else
                    'Runs move through the DoesMoveResolveCheck algorithm.
                    TempMeInCheck = PrimaryMeInCheck
                    If DoesMoveResolveCheck(PrimaryBoard, PlayerTurn, BasePseudoLegalMoves(n), TempMeInCheck) Then
                        'Move has resolved check.
                        TempMeInCheck = 0
                    End If
                End If
                If TempMeInCheck > 0 Then
                    'Player is still in check, and hence the move is not legal - remove it from the array.
                    BasePseudoLegalMoves(n) = 0
                    NoOfLegalMoves -= 1
                End If
            Next
            If NoOfLegalMoves = 0 Then
                'No legal moves detected.
                BasePieceMoves = Nothing
            Else
                'Copy all the legal moves to BasePieceMoves.
                ReDim BasePieceMoves(NoOfLegalMoves - 1)
                Dim Index As Byte
                For n As Byte = 0 To BasePseudoLegalMoves.Length - 1
                    If BasePseudoLegalMoves(n) <> 0 Then
                        'Copies legal move.
                        BasePieceMoves(Index) = BasePseudoLegalMoves(n)
                        Index += 1
                    End If
                Next

            End If
        Else
            'The player is not in check, and hence all the Pseudo-Legal moves are legal.
            'Copy the BasePseudoLegalMoves array to the BasePieceMoves array.
            ReDim BasePieceMoves(BasePseudoLegalMoves.Length - 1)
            Array.Copy(BasePseudoLegalMoves, BasePieceMoves, BasePseudoLegalMoves.Length)
        End If

        'For Each Move In BasePieceMoves
        '    OutputBitMoveToConsole(Move)
        'Next

        'Finds the material count of the board.
        PrimaryMaterialCount = CountMaterial(PrimaryBoard)
        'Resets KillerMoves.
        For n As Int16 = 0 To 255
            KillerMoves(n, 0) = 0
            KillerMoves(n, 1) = 0
        Next
        If ResetTT Then ResetTranspositionTable() Else DecrementTTTimeToLive()
        HasBeenInstantiated = True
    End Sub
    Public Function GetVersion() As String
        Return GlobalConstants.ProgramVersion
    End Function



    'Subroutines which handle the Transposition Table. This involves resetting the table for a new position,
    'clearing its memory for a garbage collection, and incrementing / decrementing the TimeToLive attribute
    'on each of the nodes in the Transposition Table.
    Public Sub ResetTranspositionTable()
        For n As UInt32 = 0 To TranspositionTable.Length - 1
            TranspositionTable(n) = New TTEntry
        Next
        TTIsEmpty = True
    End Sub
    Public Sub DisposeTranspositionTable()
        TranspositionTable = Nothing
        TTIsEmpty = True
    End Sub

    'Subroutines which increment / decrement the TimeToLive attribute in the TranspositionTable Entries.
    'After every move on the board, each TTEntry has its 'TimeToLife' attribute decremented by 1. If they
    'reach 0, then that entry can be overriden, no matter the overriding node's depth.
    Public Sub IncrementTTTimeToLive()
        If Not TTIsEmpty Then
            For n As UInt32 = 0 To TranspositionTable.Length - 1
                If TranspositionTable(n).Flag <> 4 AndAlso TranspositionTable(n).TimeToLive < 255 Then
                    TranspositionTable(n).TimeToLive += 1
                End If
            Next
        End If
    End Sub
    Public Sub DecrementTTTimeToLive()
        If Not TTIsEmpty Then
            For n As UInt32 = 0 To TranspositionTable.Length - 1
                If TranspositionTable(n).Flag <> 4 AndAlso TranspositionTable(n).TimeToLive > 0 Then
                    TranspositionTable(n).TimeToLive -= 1
                End If
            Next
        End If
    End Sub



    'Subroutines that adds / removes the Board History to / from the Transposition Table, so that MiniMax can flag these positions
    'as being 3-fold repetition (if they are encountered in a search).
    Public Sub AddBoardHistory(ByVal BoardHistory() As UInt64)
        If TranspositionTable IsNot Nothing Then
            'Calibrates this new entry in the Transpositon Table.
            Dim TempTTEntry As TTEntry
            TempTTEntry.TimeToLive = 255
            TempTTEntry.Depth = 100 'Ensures that the depth will always be greater than the current search, meaning that the TTEntry
            'will never be ignored by a MiniMax branch.
            TempTTEntry.Flag = 4 'Represents a repeated position.

            'Adds each position to the Transposition Table.
            For n As UInt16 = 0 To BoardHistory.Length - 1
                'Hashes Zobrist key to find the location of the Entry in the Table.
                TempTTEntry.Key = BoardHistory(n)
                TranspositionTable(BoardHistory(n) >> GlobalConstants.TranspositionTableSize) = TempTTEntry
            Next
        End If
    End Sub
    Public Sub RemoveBoardHistory(ByVal BoardHistory() As UInt64)
        If Not TTIsEmpty Then
            Dim EntryInTT As UInt32
            'Finds, then removes each position to the Transposition Table.
            For n As UInt16 = 0 To BoardHistory.Length - 1
                'Hashes Zobrist key to find the location of the Entry in the Table.
                EntryInTT = BoardHistory(n) >> GlobalConstants.TranspositionTableSize
                If TranspositionTable(EntryInTT).Key = BoardHistory(n) Then
                    TranspositionTable(EntryInTT) = New TTEntry
                End If
            Next
        End If
    End Sub



    'Subroutine which copies a parameter's settings to the AI's SearchSettings.
    Public Sub ConfigureSettings(ByVal UserSearchSettings As AISearchSettings, ByVal ResetTTIfSettingsChanged As Boolean)
        If SearchSettings.CopyFrom(UserSearchSettings) AndAlso ResetTTIfSettingsChanged AndAlso Not TTIsEmpty Then
            Console.ForegroundColor = ConsoleColor.Red
            Console.WriteLine("Core AI Settings Changed. Resetting Transposition Table...")
            Console.ResetColor()
            ResetTranspositionTable()
        End If
    End Sub

    'Function which retrieves all the legal moves in the base position, formatted as strings (so that the Chess system can
    'convert them to Moves more easily).
    Public Function GetLegalMoves() As String(,)
        'Stored as a set of strings in the format 0="xy", 1="XY", xy = start coors, XY = end coors.
        Dim FormattedMoves(BasePieceMoves.Length - 1, 1) As String
        For n As Byte = 0 To BasePieceMoves.Length - 1
            FormattedMoves(n, 0) = ((BasePieceMoves(n) And 3584) >> 9).ToString() & ((BasePieceMoves(n) And 448) >> 6).ToString()
            FormattedMoves(n, 1) = ((BasePieceMoves(n) And 56) >> 3).ToString() & (BasePieceMoves(n) And 7).ToString()
        Next
        Return FormattedMoves
    End Function



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
        Dim CurrentMove As New Move
        Dim BestBitMove As UInt16
        If SearchSettings.ReturnBestMove Then BestMove.Score = -32767 Else BestMove.Score = 32767 '+ or - infinity.

        If HasBeenInstantiated AndAlso Depth > 0 AndAlso BasePieceMoves IsNot Nothing Then
            'Creates alpha (white's best move) and beta (black's best move).
            Dim CurrentScore As Int16
            Dim Alpha As Int16 = -32767
            Dim Beta As Int16 = 32767
            'Resets debug variables.
            ABORT = False
            TTIsEmpty = False
            TotalPositionsSearched = 0
            TranspositionsFound = 0
            WinsFound = 0
            HighestQuiescenceDepth = 1


            'Creates temp variables.
            MasterDepth = Depth
            Dim TempBoard(7, 7) As Char
            Dim TempMaterialCount(1) As UInt16
            Dim TempMeKPos As Byte
            Dim TempMeCanCastle As New CanCastle
            Dim TempZobristValue As UInt64
            Dim TempEnPassant As Byte
            If SearchSettings.OutputToConsole Then
                Console.ForegroundColor = ConsoleColor.DarkCyan
                If Not SearchSettings.OutputMoveDebugInfo Then Console.Write("Searching at a Depth of " & MasterDepth & "...") : Console.SetCursorPosition(0, Console.CursorTop)
            End If

            'If the AI needs to return the worst move in the position, it searches the moves from worst to best (in order to improve
            'alpha-beta cutoff likelihood).
            Dim StartValue, EndValue As Int16
            Dim StepValue As SByte
            If SearchSettings.ReturnBestMove Then
                StartValue = 0
                EndValue = BasePieceMoves.Length - 1
                StepValue = 1
            Else
                StartValue = BasePieceMoves.Length - 1
                EndValue = 0
                StepValue = -1
            End If

            For n As Int16 = StartValue To EndValue Step StepValue 'for each move...

                If SearchSettings.OutputToConsole AndAlso SearchSettings.OutputMoveDebugInfo Then
                    'Outputs the move that is currently being searched on, to the console.
                    AddBitMoveToMove(CurrentMove, BasePieceMoves(n))
                    Dim StringToOutput As String = "Searching at a Depth of " & MasterDepth & " - Processing Move: " & MoveConverter(PrimaryBoard, CurrentMove, PrimaryEnPassant) & " ("
                    If SearchSettings.ReturnBestMove Then
                        StringToOutput &= n + 1
                    Else
                        StringToOutput &= BasePieceMoves.Length - n
                    End If
                    StringToOutput &= "/" & BasePieceMoves.GetUpperBound(0) + 1 & ")"
                    Console.Write(StringToOutput.PadRight(64))
                    'Moves the Console Cursor Position 1 up, so that the newly-written string can be replaced by the next move.
                    Console.SetCursorPosition(0, Console.CursorTop)
                End If

                'Copies board info to temp variables.
                Array.Copy(PrimaryBoard, TempBoard, 64)
                Array.Copy(PrimaryMaterialCount, TempMaterialCount, 2)
                TempMeKPos = PrimaryMeKPos
                TempMeCanCastle.CopyFrom(PrimaryMeCanCastle)
                TempEnPassant = PrimaryEnPassant
                TempZobristValue = BaseZobristValue
                'Makes move on temp board, then calls MiniMax for this new position.
                MakeMove(TempBoard, BasePieceMoves(n), TempMeCanCastle, TempMeKPos, TempMaterialCount, TempEnPassant, TempZobristValue)

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
                'Console.WriteLine("Move: " & MoveConverter(PrimaryBoard, CurrentMove, PrimaryEnPassant) & " Has " & TotalPositionsSearched & " Branching Nodes.") : TotalPositionsSearched = 0

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
                            AddBitMoveToMove(BestMove, BasePieceMoves(n))
                            BestBitMove = BasePieceMoves(n)
                            BestMove.BitMove = BestBitMove
                        End If
                    Else
                        Beta = Math.Min(Beta, CurrentScore)
                        If CurrentScore < BestMove.Score Then
                            'Move has been beaten (worse) - replace it.
                            BestMove.Score = CurrentScore
                            AddBitMoveToMove(BestMove, BasePieceMoves(n))
                            BestBitMove = BasePieceMoves(n)
                            BestMove.BitMove = BestBitMove
                        End If
                    End If
                End If
            Next


            'Formats the score inside the correct range, then flips score if it is black to move.
            BestMove.Score /= 100

            'LINE REMOVED FOR VERSION COMPARER
            'If Not PlayerTurn Then BestMove.Score = -BestMove.Score

            HighestSuccessfulQuiescenceDepth = Math.Abs(HighestQuiescenceDepth) + MasterDepth
            'Erases the information from the MoveDebug Information.
            If SearchSettings.OutputToConsole AndAlso SearchSettings.OutputMoveDebugInfo Then Console.Write(New String(" ", 64)) : Console.SetCursorPosition(0, Console.CursorTop)

            If Math.Abs(BestMove.Score) = 327.67 Then
                'Search either had 0 legal moves, or has been terminated so early that no move could be successfully searched.
                BestMove.Code = "a" 'Note for aborted search.
                If SearchSettings.OutputToConsole Then
                    Console.ForegroundColor = ConsoleColor.DarkGreen
                    Console.WriteLine(("Depth Of " & Depth & " Incomplete.").PadRight(30))
                    Console.ResetColor()
                End If
                If Not ABORT Then Return BestMove
            ElseIf SearchSettings.OutputToConsole Then
                If Math.Abs(BestMove.Score) > 295 Then
                    'Calibrates checkmate score.
                    If BestMove.Score > 0 Then
                        BestMove.Score -= 0.01
                    Else
                        BestMove.Score += 0.01
                    End If
                End If

                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.Write("Depth Of " & Depth)
                If SearchSettings.UseQuiescence Then Console.Write("-" & GetHighestQuiescenceDepth()) 'Retrieves the maximum reached depth via Quiescence (if enabled).
                If ABORT Then Console.Write(" Incomplete. Predicted ") Else Console.Write(" Completed. ")
                OutputMoveInfo(BestMove) 'Outputs the diagnostics for the current search.

                If SearchSettings.OutputPath Then Console.WriteLine("Path = " & GenerateBestMoveLine(BestBitMove, Math.Abs(BestMove.Score) >= 295)) 'Generates the path of 'best moves' leading from this position.
            End If

            If SearchSettings.OutputToConsole Then Console.WriteLine("Positions Searched: " & TotalPositionsSearched.ToString("N0") & vbCrLf & "Transposition Hits: " & TranspositionsFound.ToString("N0") & vbCrLf & "Win Sequence Count: " & WinsFound.ToString("N0") & vbCr)

            If SearchSettings.UpdateLifetimeStats Then
                'Increments lifetime stats.
                LifetimePositions += TotalPositionsSearched
                LifetimeTranspositions += TranspositionsFound
                If Math.Abs(BestMove.Score) = 299.99 Then LifetimeCheckmates += 1
            End If
        Else 'AI not correctly instantiated.
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when Attempting Search - FEN Position not set correctly / Depth set too low.")
            Console.ResetColor()
            BestMove.Code = "a"
        End If
        Return BestMove
    End Function

    'Search function with added 'PreviousBestMove' attribute - ammends 'BasePieceMoves' so that PreviousBestMove will be searched first.
    Public Function Search(ByVal Depth As Integer, ByVal PreviousBestMove As Move) As Move
        Dim TempMove As UInt16 = Val(PreviousBestMove.OldMoveX) << 9 Or Val(PreviousBestMove.OldMoveY) << 6 Or Val(PreviousBestMove.NewMoveX) << 3 Or Val(PreviousBestMove.NewMoveY)
        Dim LocationInMoves As Byte
        If (BasePieceMoves(0) And 4095) <> TempMove Then
            'Finds the location of PreviousBestMove in BasePieceMoves.
            Dim MoveFound As Boolean
            For n As Byte = 1 To BasePieceMoves.Length - 1
                If (BasePieceMoves(n) And 4095) = TempMove Then
                    If PreviousBestMove.Code = "Q" Then
                        If (BasePieceMoves(n) And 28672) = 4096 Then MoveFound = True
                    ElseIf PreviousBestMove.Code = "N" Then
                        If (BasePieceMoves(n) And 28672) = 28672 Then MoveFound = True
                    Else
                        MoveFound = True
                    End If
                End If

                If MoveFound Then
                    LocationInMoves = n
                    Exit For
                End If
            Next
            If MoveFound Then
                'Shifts all BasePieceMoves variables one index down, then adds PreviousBestMove to the start of the array.
                Dim BestMove As UInt16 = BasePieceMoves(LocationInMoves)
                If SearchSettings.ReturnBestMove Then
                    Array.Copy(BasePieceMoves, 0, BasePieceMoves, 1, LocationInMoves)
                    BasePieceMoves(0) = BestMove
                Else
                    'Puts the best (worst) move to be the last move, as moves will be searched in reverse.
                    Array.Copy(BasePieceMoves, LocationInMoves + 1, BasePieceMoves, LocationInMoves, BasePieceMoves.Length - LocationInMoves - 1)
                    BasePieceMoves(BasePieceMoves.Length - 1) = BestMove
                End If
            Else
                Console.ForegroundColor = ConsoleColor.Red
                Console.WriteLine("Unable to find Input Move in the current position - performing search as normal...")
                Console.ResetColor()
            End If
        End If
        'Performs the search as normal.
        Return Search(Depth)
    End Function


    'Function that performs a shallow (depth of 2, no Quiescence) search on the position, that is assumed to take
    'up almost no time. Used to test if a position is valid, to test AI features (+ debugging), and to perform
    'a simple search on the position, in case the main AI search cannot be completed in time.
    Public Function PerformTestSearch() As Move
        Dim TempQuiescenceOption As Boolean = SearchSettings.UseQuiescence
        SearchSettings.UseQuiescence = False
        Dim TestSearchMove As New Move
        TestSearchMove = Search(2)
        SearchSettings.UseQuiescence = TempQuiescenceOption
        Return TestSearchMove
    End Function




    Public Sub EnableMoveDebugInfo()
        SearchSettings.OutputMoveDebugInfo = True
    End Sub

    Public Function GetHighestQuiescenceDepth() As Byte
        Return HighestSuccessfulQuiescenceDepth
    End Function

    'Subroutine that returns the diagnostics of the current search. If required, we can set this to return its results instead of outputting (namely the PGN equivalent).
    Public Function OutputMoveInfo(ByVal BestMove As Move, Optional ByVal OnlyReturnPGN As Boolean = False) As String
        If HasBeenInstantiated Then
            Dim PGNMove As String = MoveConverter(PrimaryBoard, BestMove, PlayerTurn, PrimaryMeKPos, PrimaryEnPassant, PrimaryTFTable)
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


    'Function that generates the line containing the best sequence of moves, as found by the AI,
    'generated by following the TranspositionTable 'best move' trail.
    Private Function GenerateBestMoveLine(ByVal BestMove As UInt16, ByVal IsCheckmate As Boolean) As String
        Dim TempMove As New Move
        AddBitMoveToMove(TempMove, BestMove)
        Dim BestLine As String = MoveConverter(PrimaryBoard, TempMove, PrimaryEnPassant)

        'Copies primary board attributes to their temporary counterparts.
        Dim isWhite As Boolean = PlayerTurn
        Dim TempBoard(7, 7) As Char
        Array.Copy(PrimaryBoard, TempBoard, 64)
        Dim TempWCanCastle, TempBCanCastle As New CanCastle
        Dim TempWKPos, TempBKPos As Byte
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
        Dim TempMaterialCount(1) As UInt16
        TempMaterialCount(0) = UInt16.MaxValue \ 2
        TempMaterialCount(1) = UInt16.MaxValue \ 2
        Dim TempEnPassant As Byte = PrimaryEnPassant
        Dim TempZobristValue As UInt64 = BaseZobristValue


        Dim TempTTEntry As TTEntry
        Dim EntryInTT As UInt32
        Dim MaxIterations As Byte = 20

        'Caps the maximum amount of moves being displaced to prevent the system from getting stuck in a loop.
        For i As Byte = 1 To MaxIterations
            'Makes the AI's calculated Best Move (from this position) on the temporary board.
            If isWhite Then
                MakeMove(TempBoard, BestMove, TempWCanCastle, TempWKPos, TempMaterialCount, TempEnPassant, TempZobristValue)
            Else
                MakeMove(TempBoard, BestMove, TempBCanCastle, TempBKPos, TempMaterialCount, TempEnPassant, TempZobristValue)
            End If

            'Hashes the current position, then finds the TranspositionTable entry containing that move.
            EntryInTT = TempZobristValue >> GlobalConstants.TranspositionTableSize
            If TranspositionTable(EntryInTT).Key = TempZobristValue Then
                TempTTEntry = TranspositionTable(EntryInTT) 'Node is a match - assign to TempTTEntry.
            Else
                'No position was found from the previous move - must be end of sequence.
                Exit For
            End If

            If TempTTEntry.BestMove <> 0 Then
                'As this node in the TranspositionTable is involved in the set of best moves (as predicted by the AI), we
                'keep it alive by resetting its TimeToLive value. This ensures that the AI does not 'forget' its most vital
                'nodes, due to them expiring.
                TranspositionTable(EntryInTT).TimeToLive = Math.Max(TranspositionTable(EntryInTT).TimeToLive, 2)
                'We have stored a move in this position - retrieve this move, then add the PGN version of it to BestLine.
                BestMove = TempTTEntry.BestMove
                AddBitMoveToMove(TempMove, BestMove)
                BestLine &= "," & MoveConverter(TempBoard, TempMove, TempEnPassant)
                isWhite = Not isWhite
            Else
                'This position is empty - exit the loop.
                Exit For
            End If

            If i = MaxIterations Then
                BestLine &= ".." 'Move limit reached.
                If IsCheckmate Then BestLine &= "."
            End If
        Next

        If IsCheckmate Then BestLine &= "#"
        Return BestLine & "."
    End Function

    'Function that accepts a move, and returns the FEN of the position that would be after making that move on the board.
    Public Function ReturnFENAfterMove(ByVal TempMove As Move) As String
        If HasBeenInstantiated Then
            'Creates temporary variables of each of the main board controls, so that we can make this temporary move.
            Dim TempBoard(7, 7) As Char
            Dim TempMeCanCastle As New CanCastle
            Dim TempEnemyCanCastle As New CanCastle
            Dim TempEnPassant As Int16 = PrimaryEnPassant
            'Copies primary assets to temporary assets, then makes the move on the temporary board.
            Array.Copy(PrimaryBoard, TempBoard, 64)
            TempMeCanCastle.CopyFrom(PrimaryMeCanCastle)
            TempEnemyCanCastle.CopyFrom(PrimaryEnemyCanCastle)
            Try
                MakeMove(TempBoard, TempMove.BitMove, TempMeCanCastle, 0S, {10000, 10000}, TempEnPassant, 0UL)
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



    Public Function GetBoard() As Char(,)
        Return PrimaryBoard
    End Function
    Public Function GetZobristValue() As UInt64
        Return BaseZobristValue
    End Function
    Public Function GetMaterialCount() As Integer()
        Return New Integer() {PrimaryMaterialCount(0), PrimaryMaterialCount(1)}
    End Function



    'Subroutine which outputs the lifetime stats of the AI to the AIStats file.
    Public Sub OutputStatsToFile()
        Using SR As New StreamWriter(AppDomain.CurrentDomain.BaseDirectory & "Assets\User\AIStats.txt")
            SR.WriteLine(LifetimePositions)
            SR.WriteLine(LifetimeTranspositions)
            SR.WriteLine(LifetimeCheckmates)
        End Using
    End Sub

    'Function which calculates how full the TranspositionTable is.
    Public Function GetPercentageTranspositionTableFilled() As Decimal
        Dim TotalHits As UInt32
        If Not TTIsEmpty Then
            For n As UInt32 = 0 To TranspositionTable.Length - 1
                If TranspositionTable(n).Key > 0 Then TotalHits += 1
            Next
        End If
        Return Math.Round(TotalHits / TranspositionTable.Length, 3)
    End Function





    'Function that returns all the legal moves of a given piece on the board.
    'Used for when the user is attempting to move a piece on the GUI.
    Public Function ReturnPiecesLegalMoves(ByVal CoorX As String, ByVal CoorY As String) As String()
        Dim LegalMoves(26) As String
        If HasBeenInstantiated AndAlso BasePieceMoves IsNot Nothing Then
            Dim NoOfLegalMoves As Byte
            'Loops through all of the legal moves in the position, and makes a note of all of them that involve
            'the piece that is wanting to move.
            For n As Byte = 0 To BasePieceMoves.Length - 1
                If Val(CoorX) = (BasePieceMoves(n) And 3584) >> 9 AndAlso Val(CoorY) = (BasePieceMoves(n) And 448) >> 6 Then
                    'Move found - add it to LegalMoves.
                    LegalMoves(NoOfLegalMoves) = ((BasePieceMoves(n) And 56) >> 3).ToString() & (BasePieceMoves(n) And 7).ToString()
                    NoOfLegalMoves += 1
                End If
            Next
            'Resizes the array to the correct size.
            If NoOfLegalMoves = 0 Then Return Nothing Else Array.Resize(LegalMoves, NoOfLegalMoves)
        Else 'AI not correctly instantiated.
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when Attempting Search - FEN Position not set / no Legal Moves in Position.")
            Console.ResetColor()
        End If
        Return LegalMoves
    End Function

    'Function that scans the position for End States - positions where the game must terminate.
    Public Function CheckForEndState() As Move
        Dim CurrentMove As New Move
        If HasBeenInstantiated Then
            If BasePieceMoves Is Nothing Then
                'No legal moves in the position - hence the player is either in checkmate, or is in a stalemate.
                If PrimaryMeInCheck >= 128 Then
                    CurrentMove.Code = "c" 'Checkmate flag.
                Else
                    CurrentMove.Code = "s" 'Stalemate flag.
                End If
            Else
                If BasePieceMoves.GetUpperBound(0) = 0 Then
                    'Only one legal move in the position - make a note of this move.
                    AddBitMoveToMove(CurrentMove, BasePieceMoves(0))
                    CurrentMove.Code = "o" 'One move flag.
                Else
                    'Multiple moves detected - flag accordingly.
                    CurrentMove.Code = "f"
                End If
            End If
        Else 'AI not correctly instantiated.
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when Attempting Search - FEN Position not set / no Legal Moves in Position.")
            Console.ResetColor()
            CurrentMove.Code = "a"
        End If
        Return CurrentMove
    End Function




    'Method which uses the NegaMax algorithm to calculate how many nodes / positions stem from the current position.
    'Used for testing the AI system.
    Public Sub PerformNodeTestOnPosition(ByVal Depth As SByte)
        Console.WriteLine()
        If HasBeenInstantiated AndAlso Depth > 0 Then
            ABORT = False
            If Depth > 1 AndAlso BasePieceMoves IsNot Nothing Then
                Dim NodeTestStopwatch As New Stopwatch
                ResetTranspositionTable()

                'Resets the test statistics.
                If SearchSettings.NodeSearchCalculateRepetitions Then TTIsEmpty = False
                NodeCount = 0
                EndPositionCount = 0
                PositionCollisions = 0

                Console.ForegroundColor = ConsoleColor.DarkCyan
                Console.Write("Performing Node Test at a Depth of " & Depth & "...")
                Console.SetCursorPosition(0, Console.CursorTop)

                GCSettings.LatencyMode = GCLatencyMode.SustainedLowLatency 'Relaxes garbage collection during the NegaMax search.
                NodeTestStopwatch.Start()
                NodeTest(PrimaryBoard, Depth, PlayerTurn, PrimaryMeCanCastle, PrimaryEnemyCanCastle, PrimaryMeKPos, PrimaryEnemyKPos, PrimaryEnPassant, BaseZobristValue)
                NodeTestStopwatch.Stop()
                GCSettings.LatencyMode = GCLatencyMode.Interactive

                If Not ABORT Then
                    'Outputs the statistics of the node test.
                    Console.ForegroundColor = ConsoleColor.DarkGreen
                    Console.WriteLine("Node Test at a Depth of " & Depth & " Completed.     ")
                    Console.ResetColor()

                    Dim NodesPerSecond As Decimal = (NodeCount + EndPositionCount) / NodeTestStopwatch.Elapsed.TotalMilliseconds
                    Dim NodesPerSecondString = If(NodesPerSecond >= 1000, NodesPerSecond.ToString("N0"), Math.Round(NodesPerSecond, 1))
                    Console.WriteLine((NodeCount + EndPositionCount).ToString("N0") & " Nodes Searched in " & NodeTestStopwatch.Elapsed.TotalMilliseconds.ToString("N0") & "ms (" & NodesPerSecondString & "k Nodes/s).")

                    Console.Write("Total Position Count = " & EndPositionCount.ToString("N0"))
                    If SearchSettings.NodeSearchCalculateRepetitions Then Console.WriteLine(" (" & (EndPositionCount - PositionCollisions).ToString("N0") & " Unique Positions).") Else Console.WriteLine(".")
                End If
            Else
                'Depth is 1, or there are no legal moves in the position.
                'Therefore, we can just use the total legal move count in the current position to determine the node count.
                Dim TotalMoveCount As Byte
                If BasePieceMoves IsNot Nothing Then TotalMoveCount = BasePieceMoves.Length Else ABORT = True
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("Node Test at a Depth of " & Depth & " Completed. Total Node Count = " & TotalMoveCount & ".")
            End If
        Else
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when Attempting Node Test - FEN Position not set / depth set too low.")
            Console.ResetColor()
            ABORT = True 'Stops future depths from being performed.
        End If
    End Sub

    'Subroutine which uses the NegaMax algorithm to calculate the total nodes in a given board position.
    Private Sub NodeTest(ByVal Board(,) As Char, ByVal depth As SByte, ByVal isWhite As Boolean, ByVal MeCanCastle As CanCastle, ByVal EnemyCanCastle As CanCastle, ByVal MeKPos As Byte, ByVal EnemyKPos As Byte, ByVal EnPassant As Byte, ByVal ZobristValue As UInt64)
        If ABORT Then Exit Sub
        NodeCount += 1
        Dim MeInCheck As Byte
        Dim LegalMoveInPosition As Boolean
        Dim PieceMoves() As UInt16
        Dim TempTTEntry As New TTEntry

        'Creates temporary vehicles.
        Dim TempBoard(7, 7), TFTable(7, 7) As Char
        Dim TempMeKPos As Byte
        Dim TempMeCanCastle As New CanCastle
        Dim TempMeInCheck As Byte
        Dim TempEnPassant As Byte
        Dim TempZobristValue As UInt64

        'Creates the TFTable for the player, in preparation for move generation.
        FixTFTables(Board, isWhite, TFTable, MeKPos, MeInCheck, EnPassant)
        For y As Byte = 0 To 7
            For x As Byte = 0 To 7
                'Creates the pseudo-legal moves for each piece on the board.
                If isWhite Then
                    If Char.IsUpper(Board(x, y)) Then PieceMoves = WhitePieceLegalMoves(Board, x, y, TFTable, MeInCheck, MeCanCastle, EnPassant) Else PieceMoves = Nothing
                Else
                    If Char.IsLower(Board(x, y)) Then PieceMoves = BlackPieceLegalMoves(Board, x, y, TFTable, MeInCheck, MeCanCastle, EnPassant) Else PieceMoves = Nothing
                End If

                If PieceMoves IsNot Nothing Then
                    'At least one pseudo-legal move exists.
                    For n As Byte = 0 To PieceMoves.Length - 1
                        'Adds capture flag to the move, if needed.
                        If Board((PieceMoves(n) And 56) >> 3, PieceMoves(n) And 7) <> " " Then PieceMoves(n) = PieceMoves(n) Or 32768

                        If MeInCheck >= 128 AndAlso (PieceMoves(n) And 4032) >> 6 <> MeKPos Then
                            'Runs move through the DoesMoveResolveCheck algorithm.
                            TempMeInCheck = MeInCheck
                            If DoesMoveResolveCheck(Board, isWhite, PieceMoves(n), TempMeInCheck) Then
                                'Move has resolved check.
                                TempMeInCheck = 0
                            End If
                        Else
                            TempMeInCheck = 0
                        End If

                        If TempMeInCheck = 0 Then 'Therefore, is a legal move.
                            LegalMoveInPosition = True
                            'Copies the board position to its temporary counterparts.
                            Array.Copy(Board, TempBoard, 64)
                            TempEnPassant = EnPassant
                            TempZobristValue = ZobristValue
                            TempMeKPos = MeKPos
                            TempMeCanCastle.CopyFrom(MeCanCastle)

                            'Makes the current move onto the temporary board.
                            MakeMove(TempBoard, PieceMoves(n), TempMeCanCastle, TempMeKPos, {10000, 10000}, TempEnPassant, TempZobristValue)

                            If depth = 1 Then
                                EndPositionCount += 1
                                If SearchSettings.NodeSearchCalculateRepetitions Then
                                    'Leaf node reached - check if end position has already been encountered, using the Zobrist Hash of the position.
                                    Dim EntryInTT As UInt32 = TempZobristValue >> GlobalConstants.TranspositionTableSize
                                    If TranspositionTable(EntryInTT).Key = TempZobristValue Then
                                        PositionCollisions += 1
                                    Else
                                        TranspositionTable(EntryInTT).Key = TempZobristValue
                                    End If

                                End If
                            Else
                                'Recursively calls the Node Count on this new position.
                                NodeTest(TempBoard, depth - 1, Not isWhite, EnemyCanCastle, TempMeCanCastle, EnemyKPos, TempMeKPos, TempEnPassant, TempZobristValue)
                                'If depth = Test Then OutputBitMoveToConsole(PieceMoves(n)) : Console.WriteLine(" " & EndPositionCount - TempValue) : TempValue = EndPositionCount
                            End If
                        End If
                    Next
                End If
            Next
        Next
    End Sub






    'Function which creates and orders all the pseudo-legal moves a player can make, given certain criteria.
    Public Function CreateMoves(ByRef Board(,) As Char, ByVal isWhite As Boolean, ByRef TrueFalseTable(,) As Char, ByVal KPos As Byte, ByVal InCheck As Byte, ByVal CanCastle As CanCastle, ByVal EnPassant As Byte, ByVal OnlyCaptures As Boolean, ByVal KillerDepth As SByte, ByVal TTMove As UInt16) As UInt16()
        Dim ArrLens(7) As Byte 'Represents the number of moves in each move category array.

        'Creates variables for finding the Transposition Table's Best Move from the current position.
        Dim UseTTMove As Boolean = (TTMove <> 0)
        'Dim PieceMayBeTTMove As Boolean
        Dim TTMoveFoundInPosition As Byte

        'Variables that handle the KillerMoves array - first determines if the correct KillerMove index is empty.
        Dim PieceKillerOneFull As Boolean = KillerMoves(KillerDepth, 0) <> 0
        Dim PieceKillerTwoFull As Boolean = KillerMoves(KillerDepth, 1) <> 0
        Dim KillerMoveOneCount, KillerMoveTwoCount As Byte


        Dim PieceValueDif As Int16 'Difference in weight between the capturing piece, and the piece being captured.
        Dim XPosNew, YPosNew As Byte

        If isWhite Then
            For y As SByte = 0 To 7
                For x As SByte = 0 To 7
                    If Char.IsUpper(Board(x, y)) Then
                        'Generates the moves that the piece can make.
                        Dim PieceMoves() As UInt16 = WhitePieceLegalMoves(Board, x, y, TrueFalseTable, InCheck, CanCastle, EnPassant)

                        If PieceMoves IsNot Nothing Then 'If there are any moves...
                            For n As Byte = 0 To PieceMoves.Length - 1 'for each move...

                                'Checks if the move is the stored move in the Transposition Table.
                                If UseTTMove AndAlso PieceMoves(n) = (TTMove And 32767) Then
                                    'Move found - stop any other moves from being checked against TTMove.
                                    TTMoveFoundInPosition = 1
                                    UseTTMove = False
                                    'PieceMayBeTTMove = False
                                Else
                                    XPosNew = (PieceMoves(n) And 56) >> 3
                                    YPosNew = PieceMoves(n) And 7
                                    If Board(XPosNew, YPosNew) <> " " Then '= capture move.
                                        PieceMoves(n) = PieceMoves(n) Or 32768 'Adds capture flag.
                                        'Gets the difference in weight between the capturing piece, and the captured piece.
                                        PieceValueDif = ReturnPieceValue(UCase(Board(XPosNew, YPosNew))) - ReturnPieceValue(Board(x, y))
                                        If PieceValueDif >= 0 Then 'Capturing piece weighs less than captured piece.
                                            'Ammend move list.
                                            AmazingCaptureMoves(ArrLens(0)) = PieceMoves(n)
                                            ArrLens(0) += 1
                                        ElseIf PieceValueDif = 0 Then 'Capturing piece weighs the same as captured piece.
                                            'Ammend move list.
                                            CaptureMoves(ArrLens(1)) = PieceMoves(n)
                                            ArrLens(1) += 1
                                        Else 'Capturing piece weighs more than captured piece.
                                            'Ammend move list.
                                            OtherCaptureMoves(ArrLens(2)) = PieceMoves(n)
                                            ArrLens(2) += 1
                                        End If
                                    ElseIf Not OnlyCaptures Then 'Non-Capture moves are not considered when Quiescence mode has been activated.
                                        'Determines if the move is a direct match with the required index in KillerMoves(). If there is one,
                                        'the move is added to the TempKillerMoves
                                        If PieceKillerOneFull AndAlso PieceMoves(n) = KillerMoves(KillerDepth, 0) Then
                                            KillerMoveOneCount = 1
                                            'Prevents more moves from being added to this index.
                                            PieceKillerOneFull = False
                                        ElseIf PieceKillerTwoFull AndAlso PieceMoves(n) = KillerMoves(KillerDepth, 1) Then
                                            KillerMoveTwoCount = 1
                                            PieceKillerTwoFull = False
                                        Else
                                            If Board(x, y) = "P" AndAlso YPosNew <= 1 Then 'User is promoting a pawn (or is very close to).
                                                'Ammend move list.
                                                PawnPromotionMoves(ArrLens(3)) = PieceMoves(n)
                                                ArrLens(3) += 1
                                            ElseIf YPosNew >= 2 AndAlso (Board(Math.Max(XPosNew - 1, 0), YPosNew - 1) = "p" OrElse Board(Math.Min(XPosNew + 1, 7), YPosNew - 1) = "p") Then
                                                'New square is controlled by an enemy pawn - ammend move list.
                                                TerribleMoves(ArrLens(7)) = PieceMoves(n)
                                                ArrLens(7) += 1
                                            ElseIf Math.Max(Math.Abs(((KPos And 56) >> 3) - XPosNew), Math.Abs((KPos And 7) - YPosNew)) <= 3 Then
                                                'Piece moves to a location close to the enemy king - leading to a possible check / attack.
                                                GoodMoves(ArrLens(4)) = PieceMoves(n)
                                                ArrLens(4) += 1
                                            ElseIf TrueFalseTable(XPosNew, YPosNew) = "F" Then
                                                'Piece is positioned on a "False" on the TFTable, meaning the square is controlled by an enemy piece.
                                                'Ammend move list.
                                                BadMoves(ArrLens(6)) = PieceMoves(n)
                                                ArrLens(6) += 1
                                            Else 'Is a regular move. Ammend move list.
                                                OtherMoves(ArrLens(5)) = PieceMoves(n)
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
            For y As SByte = 7 To 0 Step -1
                For x As SByte = 0 To 7
                    If Char.IsLower(Board(x, y)) Then
                        Dim PieceMoves() As UInt16 = BlackPieceLegalMoves(Board, x, y, TrueFalseTable, InCheck, CanCastle, EnPassant)

                        If PieceMoves IsNot Nothing Then
                            For n As Byte = 0 To PieceMoves.Length - 1

                                If UseTTMove AndAlso PieceMoves(n) = (TTMove And 32767) Then
                                    TTMoveFoundInPosition = 1
                                    UseTTMove = False
                                Else
                                    XPosNew = (PieceMoves(n) And 56) >> 3
                                    YPosNew = PieceMoves(n) And 7
                                    If Board(XPosNew, YPosNew) <> " " Then
                                        PieceMoves(n) = PieceMoves(n) Or 32768
                                        PieceValueDif = ReturnPieceValue(Board(XPosNew, YPosNew)) - ReturnPieceValue(UCase(Board(x, y)))
                                        If PieceValueDif >= 0 Then
                                            AmazingCaptureMoves(ArrLens(0)) = PieceMoves(n)
                                            ArrLens(0) += 1
                                        ElseIf PieceValueDif = 0 Then
                                            CaptureMoves(ArrLens(1)) = PieceMoves(n)
                                            ArrLens(1) += 1
                                        Else
                                            OtherCaptureMoves(ArrLens(2)) = PieceMoves(n)
                                            ArrLens(2) += 1
                                        End If
                                    ElseIf Not OnlyCaptures Then
                                        If PieceKillerOneFull AndAlso PieceMoves(n) = KillerMoves(KillerDepth, 0) Then
                                            KillerMoveOneCount = 1
                                            PieceKillerOneFull = False
                                        ElseIf PieceKillerTwoFull AndAlso PieceMoves(n) = KillerMoves(KillerDepth, 1) Then
                                            KillerMoveTwoCount = 1
                                            PieceKillerTwoFull = False
                                        Else
                                            If Board(x, y) = "p" AndAlso YPosNew >= 6 Then
                                                PawnPromotionMoves(ArrLens(3)) = PieceMoves(n)
                                                ArrLens(3) += 1
                                            ElseIf YPosNew <= 5 AndAlso (Board(Math.Max(XPosNew - 1, 0), YPosNew + 1) = "P" OrElse Board(Math.Min(XPosNew + 1, 7), YPosNew + 1) = "P") Then
                                                TerribleMoves(ArrLens(7)) = PieceMoves(n)
                                                ArrLens(7) += 1
                                            ElseIf Math.Max(Math.Abs(((KPos And 56) >> 3) - XPosNew), Math.Abs((KPos And 7) - YPosNew)) <= 3 Then
                                                GoodMoves(ArrLens(4)) = PieceMoves(n)
                                                ArrLens(4) += 1
                                            ElseIf TrueFalseTable(XPosNew, YPosNew) = "F" Then
                                                BadMoves(ArrLens(6)) = PieceMoves(n)
                                                ArrLens(6) += 1
                                            Else
                                                OtherMoves(ArrLens(5)) = PieceMoves(n)
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
        Dim TotalMoveCount As Byte = TTMoveFoundInPosition + ArrLens(0) + ArrLens(1) + ArrLens(2)
        If Not OnlyCaptures Then TotalMoveCount += KillerMoveOneCount + KillerMoveTwoCount + ArrLens(3) + ArrLens(4) + ArrLens(5) + ArrLens(6) + ArrLens(7)
        If TotalMoveCount = 0 Then Return Nothing 'No moves found in the position.
        'At the end of the function, we merge all the category arrays into one - producing a huge, tiered,
        'list of a player's total pseudo-legal moves. If the user is in Quiescence mode, all non-capture moves
        'are discounted, and are not merged.
        Dim AllMoves(TotalMoveCount - 1) As UInt16

        If TTMoveFoundInPosition = 1 Then
            'Store TTMove at the start, meaning that it will be searched first.
            AllMoves(0) = TTMove
            Array.Copy(AmazingCaptureMoves, 0, AllMoves, 1, ArrLens(0))
            ArrLens(0) += 1 'Makes sure that all further moves will be stored below AmazingCaptureMoves, as index 0 is left for TT moves.
        Else
            Array.Copy(AmazingCaptureMoves, 0, AllMoves, 0, ArrLens(0))
        End If

        'Copies all other move arrays to AllMoves(,)
        Array.Copy(CaptureMoves, 0, AllMoves, ArrLens(0), ArrLens(1))
        Array.Copy(OtherCaptureMoves, 0, AllMoves, ArrLens(0) + ArrLens(1), ArrLens(2))

        If Not OnlyCaptures Then 'Copies all the non-capture moves to the main array.
            ArrLens(0) += ArrLens(1) + ArrLens(2)

            'Copies KillerMoves to AllMoves, if any have been found.
            If KillerMoveOneCount = 1 Then
                AllMoves(ArrLens(0)) = KillerMoves(KillerDepth, 0)
                ArrLens(0) += 1
            End If
            If KillerMoveTwoCount = 1 Then
                AllMoves(ArrLens(0)) = KillerMoves(KillerDepth, 1)
                ArrLens(0) += 1
            End If


            If ArrLens(3) > 0 Then Array.Copy(PawnPromotionMoves, 0, AllMoves, ArrLens(0), ArrLens(3))
            Array.Copy(GoodMoves, 0, AllMoves, ArrLens(0) + ArrLens(3), ArrLens(4))
            Array.Copy(OtherMoves, 0, AllMoves, ArrLens(0) + ArrLens(3) + ArrLens(4), ArrLens(5))
            ArrLens(0) += ArrLens(3) + ArrLens(4) + ArrLens(5)
            Array.Copy(BadMoves, 0, AllMoves, ArrLens(0), ArrLens(6))
            Array.Copy(TerribleMoves, 0, AllMoves, ArrLens(0) + ArrLens(6), ArrLens(7))
        End If

        Return AllMoves
    End Function


    'Function which receives a game position and a possible move. The function makes this move on the board (using
    'shortcuts that can only be made on a virtual board), and then generates the possible moves of the attacking
    'piece(s). If the king is no longer being threatened, then the check as been resolved.
    Private Function DoesMoveResolveCheck(ByRef Board(,) As Char, ByVal PlayerTurn As Boolean, ByVal Move As UInt16, ByRef InCheck As Byte) As Boolean
        'If the player is in a double check, then only king moves could be legal. Therefore, as this part is done in the
        'TFTable generation, we skip this here.
        If (InCheck And 64) = 0 Then
            Dim NewPosX As SByte = (Move And 56) >> 3
            Dim NewPosY As SByte = Move And 7

            If (((NewPosX << 3) Or NewPosY) = (InCheck And 63)) OrElse ((Move And 28672) = 12288) Then
                'Move captures the attacking piece (or en-passant is performed, henceforth blocking the check).
                'Therefore we assume the move is legal.
                Return True
            ElseIf Move < 32768 Then
                'Move is not a capture move.
                InCheck = InCheck And 127
                If PlayerTurn Then Board(NewPosX, NewPosY) = "O" Else Board(NewPosX, NewPosY) = "o" 'Move made on temporary board.
                'Calculate the legal moves of the attacking piece. If the king is still in check, then WInCheck.IsInCheck
                'will flag from False to True - therefore it is illegal.
                If PlayerTurn Then
                    BlackPieceLegalMoves(Board, (InCheck And 56) >> 3, InCheck And 7, TrueTable, 0, InCheck, 0)
                Else
                    WhitePieceLegalMoves(Board, (InCheck And 56) >> 3, InCheck And 7, TrueTable, 0, InCheck, 0)
                End If
                Board(NewPosX, NewPosY) = " " 'Move un-made on temporary board.
                If InCheck < 128 Then Return True
            End If
            'Otherwise, the move is a capture move, but not capturing the attacking piece. Therefore, it has to be illegal.
        End If
        Return False
    End Function


    'Subroutine that makes a move on the board, given coordinates. Includes castling (& rights), pawn promotion, and manipuation of ZobristValue.
    Private Sub MakeMove(ByRef Board(,) As Char, ByVal Move As UInt16, ByRef CanCastle As CanCastle, ByRef KPos As Byte, ByRef MaterialCount() As UInt16, ByRef EnPassant As Byte, ByRef ZobristValue As UInt64)
        Dim OldCoorX As SByte = (Move And 3584) >> 9
        Dim OldCoorY As SByte = (Move And 448) >> 6
        Dim NewCoorX As SByte = (Move And 56) >> 3
        Dim NewCoorY As SByte = Move And 7

        Dim TempPiece As Char = Board(OldCoorX, OldCoorY)
        Dim HasEnPassanted As Boolean
        'If TempMove > 32768 Then MakeMove = Board((TempMove And 56) >> 3, TempMove And 7)
        If Char.IsUpper(TempPiece) Then
            'Removes the piece from the board's Zobrist Value.
            ZobristValue = ZobristValue Xor ZobristHashTable(Asc(TempPiece) Mod 11, 0, OldCoorX, OldCoorY)
            If TempPiece = "P" Then
                'Code for Promoting Pawns and En Passant. Also increments the material count.
                If (Move And 28672) = 8192 AndAlso (Board(Math.Max(NewCoorX - 1, 0), 4) = "p" OrElse Board(Math.Min(NewCoorX + 1, 7), 4) = "p") Then
                    'EnPassant creation.
                    If EnPassant <> 0 Then ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, (EnPassant And 56) >> 3, 2)
                    EnPassant = (NewCoorX << 3) Or 5
                    ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, NewCoorX, 5) 'Ammended for en passant creation.
                    HasEnPassanted = True
                ElseIf (Move And 28672) = 12288 Then
                    Board(NewCoorX, 3) = " "
                    ZobristValue = ZobristValue Xor ZobristHashTable(3, 1, NewCoorX, 3) 'Ammended for a capture of a pawn.
                    MaterialCount(1) -= ReturnPieceValue("P")
                ElseIf (Move And 28672) = 4096 Then 'Queen Promotion.
                    TempPiece = "Q"
                    MaterialCount(0) += ReturnPieceValue("Q") - ReturnPieceValue("P") '+ 9 for a new queen, - 1 for losing the pawn in the process.
                ElseIf (Move And 28672) = 28672 Then 'Knight Promotion.
                    TempPiece = "N"
                    MaterialCount(0) += ReturnPieceValue("N") - ReturnPieceValue("P") '+ 3 for a new knight, - 1 for losing the pawn in the process.
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
                KPos = (Move And 63)
                'Code for Castling.
                If CanCastle.KS Then
                    If Move = 23031 Then
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
                    If Move = 27095 Then
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
                If (Move And 28672) = 8192 AndAlso (Board(Math.Max(NewCoorX - 1, 0), 3) = "P" OrElse Board(Math.Min(NewCoorX + 1, 7), 3) = "P") Then
                    If EnPassant <> 0 Then ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, (EnPassant And 56) >> 3, 5)
                    EnPassant = (NewCoorX << 3) Or 2
                    ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, NewCoorX, 2)
                    HasEnPassanted = True
                ElseIf (Move And 28672) = 12288 Then
                    Board(NewCoorX, 4) = " "
                    ZobristValue = ZobristValue Xor ZobristHashTable(3, 0, NewCoorX, 4)
                    MaterialCount(0) -= ReturnPieceValue("P")
                ElseIf (Move And 28672) = 4096 Then
                    TempPiece = "q"
                    MaterialCount(1) += ReturnPieceValue("Q") - ReturnPieceValue("P")
                ElseIf (Move And 28672) = 28672 Then
                    TempPiece = "n"
                    MaterialCount(1) += ReturnPieceValue("N") - ReturnPieceValue("P")
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
                KPos = (Move And 63)
                If CanCastle.KS Then
                    If Move = 22576 Then
                        Board(5, 0) = "r"
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 1, 5, 0)
                        Board(7, 0) = " "
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 1, 7, 0)
                    End If
                    CanCastle.KS = False
                    ZobristValue = ZobristValue Xor HashConstants(3)
                End If
                If CanCastle.QS Then
                    If Move = 26640 Then
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

        If Not (EnPassant = 0 OrElse HasEnPassanted) Then
            'Removal of EnPassant.
            ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, (EnPassant And 56) >> 3, EnPassant And 7)
            EnPassant = 0
        End If
        'At the end of the subroutine, the Piece is placed at the new coordinates, and the old position is cleared.
        'If the new position contains a piece, then the material count is updated for only that piece.
        If Move > 32768 Then
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
    Private Function MiniMax(ByRef Board(,) As Char, ByVal depth As SByte, ByVal isWhite As Boolean, ByVal WCanCastle As CanCastle, ByVal BCanCastle As CanCastle, ByVal WKPos As Byte, ByVal BKPos As Byte, ByVal EnPassant As Byte, ByVal MaterialCount() As UInt16, ByVal ZobristValue As UInt64, ByVal Alpha As Int16, ByVal Beta As Int16) As Int16
        If ABORT Then Return 0
        TotalPositionsSearched += 1
        HighestQuiescenceDepth = Math.Min(HighestQuiescenceDepth, depth)
        'Creates moves & checking rules.
        Dim CurrentMove, BestMove As Int16
        Dim PlayerInCheck As Byte

        Dim TempTTEntry As TTEntry
        'Hashes Zobrist key to find the location of the Entry in the Table.
        Dim EntryInTT As UInt32 = ZobristValue >> GlobalConstants.TranspositionTableSize
        Dim OldFlag As Byte
        Dim ReplaceTTNode As Boolean

        'Searches for position in TranspositionTable.
        If TranspositionTable(EntryInTT).Key = ZobristValue Then TempTTEntry = TranspositionTable(EntryInTT)

        If TempTTEntry.Key > 0 Then
            'Code for exiting the MiniMax branch early if it can successfully retrieve a correct score from that entry in TT.
            'Note that the depth of the stored position must be greater than that of the current position, as otherwise the
            'evalutation of the stored entry will be less accurate than performing the search.
            If TempTTEntry.Flag = 5 Then 'Position has already been calculated as an end-state - return this position.
                If (PlayerTurn AndAlso TempTTEntry.Score >= 29500) OrElse (Not PlayerTurn AndAlso TempTTEntry.Score <= -29500) Then WinsFound += 1
                Return TempTTEntry.Score
            End If
            If TempTTEntry.Depth >= depth Then
                Select Case TempTTEntry.Flag
                    Case 4
                        TranspositionsFound += 1
                        Return 0
                    Case 0
                        TranspositionsFound += 1
                        Return TempTTEntry.Score
                    Case 1
                        If TempTTEntry.Score >= Beta Then TranspositionsFound += 1 : Return TempTTEntry.Score
                    Case 2
                        If TempTTEntry.Score <= Alpha Then TranspositionsFound += 1 : Return TempTTEntry.Score
                End Select
            End If

            If depth > 0 Then
                OldFlag = TempTTEntry.Flag
                TempTTEntry.Flag = 4 'flags that the position is currently being searched on - if child nodes also detect this position,
                'then we return a draw (three-fold repetition).
                TempTTEntry.Depth = depth
                TranspositionTable(EntryInTT) = TempTTEntry 'Replaces entry.
                ReplaceTTNode = True
            Else
                'Make sure recommended move isn't passed into the move generation code (as move may not be a capture move).
                TempTTEntry.BestMove = 0
            End If

        ElseIf depth > 0 AndAlso (TranspositionTable(EntryInTT).Depth < depth OrElse TranspositionTable(EntryInTT).TimeToLive = 0) Then
            OldFlag = 4
            'Creates a new TTEntry for the current position, as it could not be found in the Transposition Table.
            TempTTEntry.TimeToLive = 3
            TempTTEntry.Key = ZobristValue
            TempTTEntry.Flag = 4
            TempTTEntry.Depth = depth
            TranspositionTable(EntryInTT) = TempTTEntry
            ReplaceTTNode = True
        End If


        If isWhite Then
            'Creates and forms the TFTable for the player to move.
            FixTFTables(Board, True, MiniMaxTFTable, WKPos, PlayerInCheck, EnPassant)
            If Not (depth > 0 OrElse PlayerInCheck >= 128) Then 'Quiescence mode activated.
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
            Dim PieceMoves() As UInt16 = CreateMoves(Board, True, MiniMaxTFTable, BKPos, PlayerInCheck, WCanCastle, EnPassant, Not (depth > 0 OrElse PlayerInCheck >= 128), MasterDepth - depth, TempTTEntry.BestMove) 'If Quiescence mode is activated then use capture moves only.
            If PieceMoves IsNot Nothing Then 'If any move exists...
                'Creates temp variables.
                Dim TempBoard(7, 7) As Char
                Dim TempMaterialCount(1) As UInt16
                Dim TempWKPos As Byte
                Dim TempWCanCastle As New CanCastle
                Dim TempPlayerInCheck As Byte
                Dim TempEnPassant As Byte
                Dim TempZobristValue As UInt64

                For n As Byte = 0 To PieceMoves.Length - 1 'for each move...
                    'If depth = MasterDepth - 1 AndAlso n <> 7 Then Continue For
                    'If the user is in check, eliminate the moves that do not escape check. King moves are not considered
                    'as the player's TFTable will ensure that all king moves are legal.
                    If PlayerInCheck >= 128 AndAlso (PieceMoves(n) And 4032) >> 6 <> WKPos Then
                        'Runs move through the DoesMoveResolveCheck algorithm.
                        TempPlayerInCheck = PlayerInCheck
                        If DoesMoveResolveCheck(Board, True, PieceMoves(n), TempPlayerInCheck) Then
                            'Move has resolved check.
                            TempPlayerInCheck = 0
                        End If
                    Else
                        TempPlayerInCheck = 0
                    End If
                    If TempPlayerInCheck = 0 Then 'Therefore, is a legal move.
                        'Copies board info to temp variables.
                        Array.Copy(Board, TempBoard, 64)
                        TempMaterialCount(0) = MaterialCount(0)
                        TempMaterialCount(1) = MaterialCount(1)
                        TempWKPos = WKPos
                        TempWCanCastle.CopyFrom(WCanCastle)
                        TempEnPassant = EnPassant
                        TempZobristValue = ZobristValue
                        'Makes move on temp board, then calls MiniMax for this new position.
                        MakeMove(TempBoard, PieceMoves(n), TempWCanCastle, TempWKPos, TempMaterialCount, TempEnPassant, TempZobristValue)
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
                            'Resets the current position's entry in the Transposition Table (to prevent leftover "4"
                            'entries from affecting future searches), then exits search immediately.
                            If ReplaceTTNode Then
                                'Entry in the Transposition Table is a newly-added one - remove it.
                                If OldFlag = 4 Then TranspositionTable(EntryInTT) = New TTEntry Else TranspositionTable(EntryInTT).Flag = OldFlag
                            End If
                            Return 0
                        End If

                        If CurrentMove > BestMove Then
                            BestMove = CurrentMove 'Best Move has been beaten - replace it.

                            If ReplaceTTNode Then
                                'Updates the best move in the Transposition Table entry to match this new, best move.
                                TempTTEntry.BestMove = PieceMoves(n)
                                'We now have proof that the move is not an upper bound, as the move was able to produce
                                'a valid evaluation - replace it with 'exact'.
                                'Alfie Note 3/8/23: Fun Fact! The below IF statement (checking against Alpha / Beta) seems to 
                                'eliminate (as far as I can tell) all of the instability I've been having in my Transposition Table.
                                'I've been bug hunting for this for like 6 FREAKING MONTHS AND ALL IT TOOK WAS ONE STUPID IF CHECK!?!?!?
                                'I would be dancing around my room with excitement, but I'm mostly just angry that it took me this
                                'long to figure this out lmaoo. Still pretty happy & relieved, though :DD.
                                If Not SearchSettings.StableSearch AndAlso BestMove > Alpha Then TempTTEntry.Flag = 0
                            End If

                            Alpha = Math.Max(Alpha, BestMove) 'Alpha = best move found for white.
                            If Beta <= Alpha Then 'Move was too strong for player; opponent will not choose this branch.
                                If PieceMoves(n) < 32768 AndAlso KillerMoves(MasterDepth - depth, 0) <> PieceMoves(n) Then
                                    'The pruned move is not a capture move - add move to KillerMoves(), in the hope that the move
                                    'is also possible in sibling positions. If this move is detected, it is searched earlier.
                                    KillerMoves(MasterDepth - depth, 1) = KillerMoves(MasterDepth - depth, 0)
                                    KillerMoves(MasterDepth - depth, 0) = PieceMoves(n)
                                End If

                                If Math.Abs(BestMove) >= 29500 Then
                                    'This sub-position leads to an inevitable checkmate. However, as this position is 1 move further
                                    'away to that checkmate, subtlety diminish the value of this position, so that the fastest
                                    'possible checkmating path will be chosen by the AI.
                                    If BestMove > 0 Then
                                        BestMove -= 1
                                    Else
                                        BestMove += 1
                                    End If
                                End If
                                If ReplaceTTNode Then
                                    'Store position & its detail in the Transposition Table.
                                    TempTTEntry.Score = BestMove
                                    TempTTEntry.Flag = 1
                                    'Caused an Alpha-beta cutoff, so the move may be even better than its score suggests - mark as lower bound.
                                    TranspositionTable(EntryInTT) = TempTTEntry 'Replaces entry.
                                End If

                                Return BestMove 'Alpha-Beta Pruning - return best move.
                            End If
                        End If
                    End If
                Next
            End If

        Else 'Near-identical code for the black side.

            FixTFTables(Board, False, MiniMaxTFTable, BKPos, PlayerInCheck, EnPassant)
            If Not (depth > 0 OrElse PlayerInCheck >= 128) Then
                BestMove = Evaluate(Board, MaterialCount, WKPos, BKPos)
                Beta = Math.Min(Beta, BestMove)
                If Beta <= Alpha Then
                    Return BestMove
                End If
            Else
                BestMove = 32767
            End If

            TempTTEntry.Flag = 1
            Dim PieceMoves() As UInt16 = CreateMoves(Board, False, MiniMaxTFTable, WKPos, PlayerInCheck, BCanCastle, EnPassant, Not (depth > 0 OrElse PlayerInCheck >= 128), MasterDepth - depth, TempTTEntry.BestMove)
            If PieceMoves IsNot Nothing Then
                Dim TempBoard(7, 7) As Char
                Dim TempMaterialCount(1) As UInt16
                Dim TempBKPos As Byte
                Dim TempBCanCastle As New CanCastle
                Dim TempPlayerInCheck As Byte
                Dim TempEnPassant As Byte
                Dim TempZobristValue As UInt64
                For n As Byte = 0 To PieceMoves.Length - 1 'for each move...
                    If PlayerInCheck >= 128 AndAlso (PieceMoves(n) And 4032) >> 6 <> BKPos Then
                        TempPlayerInCheck = PlayerInCheck
                        If DoesMoveResolveCheck(Board, False, PieceMoves(n), TempPlayerInCheck) Then
                            TempPlayerInCheck = 0
                        End If
                    Else
                        TempPlayerInCheck = 0
                    End If
                    If TempPlayerInCheck = 0 Then
                        Array.Copy(Board, TempBoard, 64)
                        TempMaterialCount(0) = MaterialCount(0)
                        TempMaterialCount(1) = MaterialCount(1)
                        TempBKPos = BKPos
                        TempBCanCastle.CopyFrom(BCanCastle)
                        TempEnPassant = EnPassant
                        TempZobristValue = ZobristValue
                        MakeMove(TempBoard, PieceMoves(n), TempBCanCastle, TempBKPos, TempMaterialCount, TempEnPassant, TempZobristValue)
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
                            If ReplaceTTNode Then
                                If OldFlag = 4 Then TranspositionTable(EntryInTT) = New TTEntry Else TranspositionTable(EntryInTT).Flag = OldFlag
                            End If
                            Return 0
                        End If

                        If CurrentMove < BestMove Then
                            BestMove = CurrentMove
                            If ReplaceTTNode Then
                                TempTTEntry.BestMove = PieceMoves(n)
                                If Not SearchSettings.StableSearch AndAlso BestMove < Beta Then TempTTEntry.Flag = 0
                            End If

                            Beta = Math.Min(Beta, BestMove) 'Beta = best move found for white.
                            If Beta <= Alpha Then
                                If PieceMoves(n) < 32768 AndAlso KillerMoves(MasterDepth - depth, 0) <> PieceMoves(n) Then
                                    KillerMoves(MasterDepth - depth, 1) = KillerMoves(MasterDepth - depth, 0)
                                    KillerMoves(MasterDepth - depth, 0) = PieceMoves(n)
                                End If

                                If Math.Abs(BestMove) >= 29500 Then
                                    If BestMove > 0 Then
                                        BestMove -= 1
                                    Else
                                        BestMove += 1
                                    End If
                                End If
                                If ReplaceTTNode Then
                                    TempTTEntry.Score = BestMove
                                    TempTTEntry.Flag = 2
                                    TranspositionTable(EntryInTT) = TempTTEntry
                                End If

                                Return BestMove
                            End If
                        End If
                    End If
                Next
            End If
        End If

        If Math.Abs(BestMove) >= 29500 Then
            If Math.Abs(BestMove) = 32767 Then
                'No legal move found for the player.
                If PlayerInCheck >= 128 Then
                    'Checkmate!
                    If isWhite Then
                        BestMove = -30000
                        If Not PlayerTurn Then WinsFound += 1
                    Else
                        BestMove = 30000
                        If PlayerTurn Then WinsFound += 1
                    End If
                Else 'Stalemate: return 0.
                    BestMove = 0
                End If
            Else
                'Position leads to checkmate. Diminish score to reflect longer path to checkmate.
                If BestMove > 0 Then
                    BestMove -= 1
                Else
                    BestMove += 1
                End If
            End If
            TempTTEntry.Flag = 5 'Represents an end-state in the Transposition Table.
        End If

        'Search completed without Alpha-beta cutoff - store position in Transposition Table.
        If ReplaceTTNode Then
            TempTTEntry.Score = BestMove
            TranspositionTable(EntryInTT) = TempTTEntry
        End If

        'Return the best move's score found this iteration.
        Return BestMove
    End Function

    'This algorithm is used to condense a board position into an evaluation score, used to determine best moves.
    'We take into account the difference in material between the two sides, along with a heuristic to help
    'the AI find checkmated in simple endgame positions.
    Private Function Evaluate(ByRef Board(,) As Char, ByVal MaterialCount() As UInt16, ByVal WKPos As Byte, ByVal BKPos As Byte) As Int16
        'Finds difference in material between both sides.
        Dim Score As Int32 = MaterialCount(0)
        Score -= MaterialCount(1)

        If SearchSettings.UsePieceHeatMaps Then
            'If the opponent has a lot of material, then score positions where the player's king is safe as being more advantageous.
            If MaterialCount(1) >= 1500 Then Score += PieceHeatMap(9, WKPos And 7, (WKPos And 56) >> 3)
            If MaterialCount(0) >= 1500 Then Score -= PieceHeatMap(9, 7 - (BKPos And 7), (BKPos And 56) >> 3)

            'For each piece on the board, calculate how advantageously it is placed (using the PieceHeatSquares). Add this to the score.
            For y As Byte = 0 To 7
                For x As Byte = 0 To 7
                    If Not (Board(x, y) = " " OrElse UCase(Board(x, y)) = "K") Then
                        If Char.IsUpper(Board(x, y)) Then
                            Score += PieceHeatMap(Asc(Board(x, y)) Mod 11, y, x)
                        Else
                            Score -= PieceHeatMap((Asc(Board(x, y)) + 1) Mod 11, 7 - y, x)
                        End If
                    End If
                Next
            Next
        End If


        'If a player has little material left, we calculate the value of this endgame position for the opposing player, using the
        'Lookup Table. This value takes into consideration the distances between the kings, and how close the player's king is to
        'the edge of the board (and therefore how easy / difficult it will be to checkmate the player).
        If MaterialCount(0) <= 1200 Then
            'White has little material left. Penelise white's king from being at the edges of the board, and when white's king
            'is close to black's king.
            Score -= EndgameEvalLookupTable(WKPos, BKPos, MaterialCount(0) / 100)
        End If
        If MaterialCount(1) <= 1200 Then
            'Black has little material left.
            Score += EndgameEvalLookupTable(BKPos, WKPos, MaterialCount(1) / 100)
        End If


        Return Score
    End Function

End Class
