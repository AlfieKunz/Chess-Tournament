'This class contains my Chess AI, which is built using the NegaMax algorithm with Alpha-Beta Pruning.
'This class is modular from my Chess class - being constructed only from the FEN position, and only returning a Move (see the structure below).
'Some other interacting is done, however, such as allowing the AI to be remotely aborted.
Imports System.ComponentModel
Imports System.IO
Imports System.Runtime
Imports System.Text

Partial Public Class AI 'i shall thy the Alfie Alphafish (bit optimistic, I know).
    Inherits CoreMethods

    Private HasBeenInstantiated As Boolean 'The AI will not perform methods if it has not been fully instantiated with a FEN.
    'Below are the details that the AI requires for a search. Please see their counterparts in the Chess form for their info.
    Private PrimaryBoard(7, 7), PrimaryTFTable(7, 7) As Char, NegaMaxTFTable(7, 7) As Char
    Private TFTableStorageArray(127) As TFTableStorage 'Holds the TFTables for each depth of the search in the NegaMax algorithm.
    'The reason we do this is because of Null Moves: as this is called between TFTableFixer & CreateMoves, the branches from the Null Nodes
    'mess with the TFTable. As a result, we store the TFTable for each depth, and call it when needed.
    'Alfie Note 24.12.24: this seems very unnecessary... why can't we just pass a reference to NegaMaxTFTable?? Hopefully wanting to fix this soon...
    Private PrimaryMeCanCastle, PrimaryEnemyCanCastle As New CanCastle

    Private PrimaryMeInCheck As UInt16 'Checking data is represented as a set of bits, in the format:
    'CDXXXYYY
    'C = Check Flag. D = Double Check Flag. X = Checking Piece X coors. Y = Checking Piece Y coors.
    Private PrimaryMeKPos, PrimaryEnemyKPos As Int16
    Private PrimaryEnPassant As Int16
    'The above three attributes are represented as a set of bits, where the three LSBs refer to the Y coordinate
    'of the king / square, and the next three bits refer to the X coordinate of the king / square.

    Private PrimaryMaterialCount(1) As Integer
    Private PlayerTurn As Boolean
    Private PrimaryZobristValue As UInt64 'Represents the Zorbist Hash Key of the position to be searched on.
    Private PrimaryHalfMoveSize As UInt16 'Represents the Half-Move count of the base position

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
    Private TotalPositionsSearched, TranspositionsFound, WinsFound As UInt64 'Numbers showing the stats of the current search.
    Private LifetimePositions, LifetimeTranspositions, LifetimeCheckmates As UInt64 'Numbers showing the lifetime stats of the AI (persists
    'across multiple boot-ups).
    Private DetailedMoveOutput As Boolean = True
    Private HighestQuiescenceDepth As Integer 'Shows the maximum reached depth of a search that has not been ABORTed.
    Private NoRepeatedSearches As Integer
    Private NodeCount, EndPositionCount, PositionCollisions As UInt64 'Variables containing the stats of a Node Search.
    'NodeCount = Total number of positions searched. EndPositionCount = Total Number of Leaf Nodes searched. PositionCollisions = Number of Positions evaluated multiple times.

    Private ABORT, TERMINATED As Boolean 'Controlled by the thread handlers - an AI is aborted if another AI has
    'found a mating pattern, or if the AI has ran out of time. If ABORT is set to True, then the AI finishes its search ASAP.
    Private MasterDepth, DepthFromRoot As Integer 'The Surface Depth of the search, as set by the user, and the current depth (from the base position) at a current point in the search.
    Private Const InfScore As Int16 = Int16.MaxValue


    Private KillerMoves(127, 1) As UInt16 'Array containing Killer Moves: non-capture moves which caused an alpha-beta cut off.
    'If we detect killer moves in sibling positions (ie: positions of the same depth), we search the Killer Move(s) first.

    'Arrays that the CreateMoves function uses (delared before to save processing time in the search process).
    Private AmazingCaptureMoves(49) As UInt16 'Min size = 19
    Private CaptureMoves(49) As UInt16 'Min size = 19
    Private OtherCaptureMoves(49) As UInt16 'Min size = 19
    Private PawnPromotionMoves(15) As UInt16 'Min size = 7
    Private GoodMoves(99) As UInt16 'Min size = 49
    Private OtherMoves(99) As UInt16 'Min size = 99
    Private BadMoves(74) As UInt16 'Min size = 49
    Private TerribleMoves(74) As UInt16 'Min size = 49


    'Structures for the TranspositionTable - a large table containing the basic details of a position - stored via their Zobrist Hash.
    'Stored as a hashed array so that the overall size of the Table can be reduced (would need to be 2^64 elements large otherwise).
    'This structure uses the 'Greatest Depth' replacement scheme, where each entry is timestamped for 3 moves.
    'Table will contain 2^n entries, where n is the value set in TranspositionTableSize (in the brackets).
    Private TranspositionTable(1 << (64 - GlobalConstants.TranspositionTableSize) - 1) As TTEntry
    Private TTIsEmpty As Boolean
    Private TTGeneration As Int16 'Represents the current move count of the position, so that we can index when TTEntries are made.


    Private Structure TTEntry
        'Dim isPopulated As Boolean
        Dim Key As UInt64 'Zobrist Key of position - used to pinpoint the correct board.
        Dim Generation As Int16 'Represents the move at which the entry was created. If the current move is much higher than this number, we call this entry 'dead'.
        Dim Depth As SByte
        Dim Flag As Byte 'Represents additional information about the move:
        '0 = Score is exact (no ambiguity), 1 = Lower Bound (score could be higher), 2 = Upper Bound (score could be lower),
        '4 = Position is currently being searched on (for 3RF), 5 = End State (ie: checkmate, stalemate).
        Dim Score As Int16 'Stores the evaluation of the position
        Dim BestMove As UInt16 'Stores the best move found from this position when previously computed.
    End Structure



    'Constructor methods.
    Public Sub New()
        Me.New(GlobalConstants.StartingFENPosition)
    End Sub
    Public Sub New(ByVal FEN As String)
        PieceHeatMap = GeneratePieceHeatSquares()
        PopulateEndgameEvalLookupTable()
        'Configures the AI using a given FEN.
        Reconfigure(FEN, True)
        'Loads the Lifetime stats file, and stores the results into their appropriate variables.
        Try
            Using SR As New StreamReader(GlobalConstants.StartupPath & "\Assets\User\AIStats.txt", Encoding.UTF8, True)
                LifetimePositions = ULong.Parse(SR.ReadLine())
                LifetimeTranspositions = ULong.Parse(SR.ReadLine())
                LifetimeCheckmates = ULong.Parse(SR.ReadLine())
            End Using
        Catch ex As Exception
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Unable to retrieve AI lifetime stats. Resetting...")
            Console.ForegroundColor = ConsoleColor.White
        End Try
    End Sub

    'Subroutine which converts a user's input FEN into all the details needed to conduct a NegaMax search.
    Public Sub Reconfigure(ByVal FEN As String, Optional ByVal ResetTT As Boolean = False)
        Dim BasePseudoLegalMoves() As UInt16
        'Converts the user's FEN into a board position, then resets checking rules.
        Try
            PrimaryBoard = FENConverter(FEN, PrimaryMeCanCastle, PrimaryEnemyCanCastle, PrimaryMeKPos, PrimaryEnemyKPos, PrimaryEnPassant, PlayerTurn)
        Catch ex As Exception
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Unable to Calibrate AI from given FEN. Please try again...")
            Console.ForegroundColor = ConsoleColor.White
            HasBeenInstantiated = False
            Exit Sub
        End Try
        PrimaryMeInCheck = 0
        If PlayerTurn Then
            'Creates the TFTable for the white pieces, then creates king symbol.
            FixTFTable(PrimaryBoard, True, PrimaryTFTable, PrimaryMeKPos, PrimaryMeInCheck, PrimaryEnPassant)
            BasePseudoLegalMoves = CreateMoves(PrimaryBoard, True, PrimaryTFTable, PrimaryEnemyKPos, PrimaryMeInCheck, PrimaryMeCanCastle, PrimaryEnPassant, False, 0, 0)
            'Creates the Zobrist Value for the position.
            PrimaryZobristValue = ZobristHashPosition(PrimaryBoard, True, PrimaryMeCanCastle, PrimaryEnemyCanCastle, PrimaryEnPassant)
        Else
            'Swaps the Primary & Enemy Castling privileges, along with the Primary & Enemy King privileges.
            'This is because, for the AI, all variables are in context of which player the AI is controlling,
            'so 'me' and 'enemy' can refer to different colours depending on the board position.
            Dim TempCanCastle As New CanCastle
            TempCanCastle.CopyFrom(PrimaryMeCanCastle)
            PrimaryMeCanCastle.CopyFrom(PrimaryEnemyCanCastle)
            PrimaryEnemyCanCastle.CopyFrom(TempCanCastle)
            Dim TempKPos As Int16 = PrimaryMeKPos
            PrimaryMeKPos = PrimaryEnemyKPos
            PrimaryEnemyKPos = TempKPos
            'Creates the TFTable for the black pieces, then creates king symbol.
            FixTFTable(PrimaryBoard, False, PrimaryTFTable, PrimaryMeKPos, PrimaryMeInCheck, PrimaryEnPassant)
            BasePseudoLegalMoves = CreateMoves(PrimaryBoard, False, PrimaryTFTable, PrimaryEnemyKPos, PrimaryMeInCheck, PrimaryMeCanCastle, PrimaryEnPassant, False, 0, 0)
            PrimaryZobristValue = ZobristHashPosition(PrimaryBoard, False, PrimaryEnemyCanCastle, PrimaryMeCanCastle, PrimaryEnPassant)
        End If


        'Filters the moves in the base position, so that only legal moves exist (rather than pseudo-legal moves).
        If BasePseudoLegalMoves Is Nothing Then
            'No pseudo-legal moves detected - hence no legal moves in the position.
            BasePieceMoves = Nothing
        ElseIf PrimaryMeInCheck >= 128 Then
            Dim TempMeInCheck As UInt16
            Dim NoOfLegalMoves As Integer = BasePseudoLegalMoves.Length 'Assume all moves are legal, unless proven otherwise.
            For n = 0 To BasePseudoLegalMoves.Length - 1 'For each move.
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
                Dim Index As Integer
                For n = 0 To BasePseudoLegalMoves.Length - 1
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
        SortMovesThourough(BasePieceMoves, PrimaryBoard, PlayerTurn, PrimaryMeKPos, PrimaryEnemyKPos, PrimaryMeCanCastle, PrimaryEnPassant, PrimaryZobristValue)

        'If BasePieceMoves IsNot Nothing Then
        '    For Each Move In BasePieceMoves
        '        OutputBitMoveToConsole(Move)
        '    Next
        'End If

        'Finds the material count of the board.
        PrimaryMaterialCount = CountMaterial(PrimaryBoard)
        'Resets KillerMoves.
        For n As Int16 = 0 To 127
            KillerMoves(n, 0) = 0
            KillerMoves(n, 1) = 0
            TFTableStorageArray(n) = New TFTableStorage
        Next
        If ResetTT Then ResetTranspositionTable() Else IncreaseTTGeneration()
        PrimaryHalfMoveSize = 0

        HasBeenInstantiated = True
    End Sub

    Public Function GetVersion() As String
        Return GlobalConstants.ProgramVersion
    End Function




    'Subroutines which handle the Transposition Table. This involves resetting the table for a new position,
    'clearing its memory for a garbage collection, and incrementing / decrementing the TimeToLive attribute
    'on each of the nodes in the Transposition Table.
    Public Sub ResetTranspositionTable()
        For n = 0 To TranspositionTable.Length - 1
            TranspositionTable(n) = New TTEntry
        Next
        TTIsEmpty = True
        TTGeneration = 0
    End Sub
    Public Sub DisposeTranspositionTable()
        TranspositionTable = Nothing
        TTIsEmpty = True
        TTGeneration = 0
    End Sub
    Public Sub IncreaseTTGeneration()
        If TTGeneration >= Int16.MaxValue Then
            'Move Limit has been matched - must reset TT :(
            If Not TTIsEmpty Then
                For n = 0 To TranspositionTable.Length - 1
                    If TranspositionTable(n).Flag <> 4 And TranspositionTable(n).Generation <> UInt16.MaxValue Then
                        TranspositionTable(n).Generation = 0
                    End If
                Next
            End If
            TTGeneration = 0
        Else
            TTGeneration += 1S
        End If
    End Sub



    'Subroutines that adds / removes the Board History to / from the Transposition Table, so that NegaMax can flag these positions
    'as being 3-fold repetition (if they are encountered in a search).
    Public Sub AddBoardHistory(ByVal BoardHistory() As UInt64, Optional ByVal HalfMoveSize As UInt16 = 0)
        If TranspositionTable IsNot Nothing Then
            'Calibrates this new entry in the Transpositon Table.
            Dim TempTTEntry As TTEntry
            TempTTEntry.Generation = Byte.MaxValue 'These values should never be cleared from the TranspositionTable.
            TempTTEntry.Depth = CSByte(100) 'Ensures that the depth will always be greater than the current search, meaning that the TTEntry
            'will never be ignored by a NegaMax branch.
            TempTTEntry.Flag = 4 'Represents a repeated position.

            'Adds each position to the Transposition Table.
            For n = 0 To BoardHistory.Length - 1
                'Hashes Zobrist key to find the location of the Entry in the Table.
                TempTTEntry.Key = BoardHistory(n)
                TranspositionTable(CInt(BoardHistory(n) >> GlobalConstants.TranspositionTableSize)) = TempTTEntry
            Next
            PrimaryHalfMoveSize = HalfMoveSize
        End If
    End Sub
    Public Sub RemoveBoardHistory(ByVal BoardHistory() As UInt64)
        If Not TTIsEmpty Then
            Dim EntryInTT As Int32
            'Finds, then removes each position to the Transposition Table.
            For n = 0 To BoardHistory.Length - 1
                'Hashes Zobrist key to find the location of the Entry in the Table.
                EntryInTT = CInt(BoardHistory(n) >> GlobalConstants.TranspositionTableSize)
                If TranspositionTable(EntryInTT).Key = BoardHistory(n) Then
                    TranspositionTable(EntryInTT) = New TTEntry
                End If
            Next
            PrimaryHalfMoveSize = 0
        End If
    End Sub



    'Subroutine which copies a parameter's settings to the AI's SearchSettings.
    Public Sub ConfigureSettings(ByVal UserSearchSettings As AISearchSettings, ByVal ResetTTIfSettingsChanged As Boolean)
        If SearchSettings.CopyFrom(UserSearchSettings) AndAlso ResetTTIfSettingsChanged AndAlso Not TTIsEmpty Then
            Console.ForegroundColor = ConsoleColor.Red
            Console.WriteLine("Core AI Settings Changed. Resetting Transposition Table...")
            Console.ForegroundColor = ConsoleColor.White
            ResetTranspositionTable()
        End If
    End Sub

    'Function which retrieves all the legal moves in the base position, formatted as strings (so that the Chess system can
    'convert them to Moves more easily).
    Public Function GetLegalMoves() As String(,)
        If HasBeenInstantiated Then
            'Stored as a set of strings in the format 0="xy", 1="XY", xy = start coors, XY = end coors.
            Dim FormattedMoves(BasePieceMoves.Length - 1, 1) As String
            For n = 0 To BasePieceMoves.Length - 1
                FormattedMoves(n, 0) = ((BasePieceMoves(n) And 3584) >> 9).ToString() & ((BasePieceMoves(n) And 448) >> 6).ToString()
                FormattedMoves(n, 1) = ((BasePieceMoves(n) And 56) >> 3).ToString() & (BasePieceMoves(n) And 7).ToString()
            Next
            Return FormattedMoves
        End If
        Return Nothing
    End Function
    Public Function GetNoOfLegalMoves() As Integer
        If HasBeenInstantiated Then Return BasePieceMoves.Length Else Return Nothing
    End Function


    Public Function GetBoard() As Char(,)
        Return PrimaryBoard
    End Function
    Public Function GetZobristValue() As UInt64
        Return PrimaryZobristValue
    End Function
    Public Function GetMaterialCount() As Integer()
        Return PrimaryMaterialCount
    End Function



    'Setter & Getter Functions for the ABORT attribute.
    Public Sub ABORTSearch()
        ABORT = True
    End Sub
    Public Function GetABORTState() As Boolean
        Return ABORT
    End Function
    Public Sub TERMINATESearch()
        ABORT = True
        TERMINATED = True
    End Sub

    'Setter Functions for the DetailedMoveOutput attribute.
    Public Sub SetDetailedMoveOutput(ByVal Value As Boolean)
        DetailedMoveOutput = Value
    End Sub




    'Algorithm that finds the AI's 'Best Move' for a given position, using the NegaMax algorithm.
    Public Function Search(ByVal Depth As Integer, Optional ByVal PreviousBestScore As Int16 = -InfScore) As Move
        Dim BestMove As New Move
        Dim CurrentMove As New Move
        Dim BestBitMove As UInt16
        If SearchSettings.ReturnBestMove Then BestMove.Score = -InfScore Else BestMove.Score = InfScore '+ or - infinity.

        If HasBeenInstantiated AndAlso Depth > 0 AndAlso BasePieceMoves IsNot Nothing Then
            'Creates alpha (white's best move) and beta (black's best move).
            Dim CurrentScore As Int16
            Dim Alpha, Beta As Int16
            'Resets debug variables.
            ABORT = False
            TERMINATED = False
            TTIsEmpty = False
            TotalPositionsSearched = 0
            TranspositionsFound = 0
            WinsFound = 0
            HighestQuiescenceDepth = 1
            NoRepeatedSearches = 0

            'Creates temp variables.
            MasterDepth = Depth
            Dim TempBoard(7, 7) As Char
            Dim TempMaterialCount(1) As Integer
            Dim TempMeKPos As Int16
            Dim TempMeCanCastle As New CanCastle
            Dim TempZobristValue As UInt64
            Dim TempHalfMoveSize As UInt64
            Dim TempEnPassant As Int16

            'If the AI needs to return the worst move in the position, it searches the moves from worst to best (in order to improve
            'alpha-beta cutoff likelihood).
            Dim StartValue, EndValue As Integer
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

            'Aspiration-Windows: if the previous search (via iterative deepening) produced a move with a valid score, we use this as an _estimate_ for this search,
            'and set the Alpha-Beta Bounds to be around this previous evaluation. If we were wrong, and the new score is outside this window (ie: upon searching
            'deeper, the position is significantly better / worse than the previous score suggested) then we must repeat the whole search again, this time with
            'an infinite window. To make things easier, we set this 'cut-off' move to be the next move to search, as it is likely very good :D.
            Dim AspirationWindow() As Int16
            While True
                If SearchSettings.OutputToConsole Then
                    Console.ForegroundColor = ConsoleColor.DarkCyan
                    If Not SearchSettings.OutputMoveDebugInfo Then Console.Write("Searching at a Depth of " & MasterDepth & "...") : Console.SetCursorPosition(0, Console.CursorTop)
                End If

                If PreviousBestScore = -InfScore OrElse SearchSettings.AspirationWindowWidth <= 0 Then
                    AspirationWindow = {-InfScore, InfScore}
                Else
                    'Aspiration Windows - set Alpha & Beta via the previous search score.
                    AspirationWindow = {PreviousBestScore - SearchSettings.AspirationWindowWidth, PreviousBestScore + SearchSettings.AspirationWindowWidth}
                End If
                Alpha = AspirationWindow(0)
                Beta = AspirationWindow(1)

                For n = StartValue To EndValue Step StepValue 'for each move...

                    If SearchSettings.OutputToConsole AndAlso SearchSettings.OutputMoveDebugInfo Then
                        'Outputs the move that is currently being searched on, to the console.
                        ConvertBitMoveToMove(CurrentMove, BasePieceMoves(n))
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
                    TempZobristValue = PrimaryZobristValue
                    TempHalfMoveSize = PrimaryHalfMoveSize
                    'Makes move on temp board, then calls NegaMax for this new position.
                    MakeMove(TempBoard, BasePieceMoves(n), TempMeCanCastle, TempMeKPos, TempMaterialCount, TempEnPassant, TempZobristValue, TempHalfMoveSize)
                    DepthFromRoot = 1

                    If TempMaterialCount(0) + TempMaterialCount(1) = 0 Then
                        'Enforce draw by repetition.
                        TotalPositionsSearched += 1UL
                        CurrentScore = 0
                    ElseIf Depth = 1 AndAlso Not SearchSettings.UseQuiescence Then
                        'We have reached a leaf position - return the evaluation for this position.
                        TotalPositionsSearched += 1UL
                        CurrentScore = Evaluate(TempBoard, PlayerTurn, TempMaterialCount, PrimaryMeKPos, PrimaryEnemyKPos)
                    Else

                        'Late Move Reducitons & Internal Iterative Reductions - search everything but the first n moves at a reduced depth. If no hash move could be found, then the position
                        'is deemed 'more quiet', and so more moves are searched at a reduced depth.
                        Dim NeedFullSearch As Boolean = True
                        If Not SearchSettings.StableSearch AndAlso Depth >= 4 AndAlso If(SearchSettings.ReturnBestMove, n, BasePieceMoves.Length - n) >= SearchSettings.MoveReductionThreshold + 1 Then
                            CurrentScore = -NegaMax(TempBoard, Depth - 2, 0, Not PlayerTurn, PrimaryEnemyCanCastle, TempMeCanCastle, PrimaryEnemyKPos, TempMeKPos, TempEnPassant, TempMaterialCount, TempZobristValue, TempHalfMoveSize, -If(SearchSettings.ReturnBestMove, Alpha, Beta) - 1S, -If(SearchSettings.ReturnBestMove, Alpha, Beta), True)
                            If CurrentScore > Alpha Then
                                NoRepeatedSearches += 1
                            Else
                                NeedFullSearch = False
                            End If
                        End If
                        If NeedFullSearch Then
                            'Alpha & Beta are replaced with -Beta & -Alpha so that all moves' scores are given in context
                            'of the player to move. This is so that the largest number will always be the best score.
                            CurrentScore = -NegaMax(TempBoard, Depth - 1, 0, Not PlayerTurn, PrimaryEnemyCanCastle, TempMeCanCastle, PrimaryEnemyKPos, TempMeKPos, TempEnPassant, TempMaterialCount, TempZobristValue, TempHalfMoveSize, -Beta, -Alpha, True)
                        End If
                    End If
                    'Console.WriteLine("Move: " & MoveConverter(PrimaryBoard, CurrentMove, PrimaryEnPassant) & " Has " & TotalPositionsSearched & " Branching Nodes.") : TotalPositionsSearched = 0

                    'Code for selecting the best move in the position.
                    If TERMINATED Then
                        'Search ends abruptly, without returning a full move.
                        If SearchSettings.UpdateLifetimeStats Then
                            'Increments lifetime stats.
                            LifetimePositions += TotalPositionsSearched
                            LifetimeTranspositions += TranspositionsFound
                        End If
                        BestMove.Code = "t"c
                        Return BestMove
                    ElseIf ABORT Then
                        'Latest, unfinished move will not be considered.
                        Exit For
                    Else
                        If SearchSettings.ReturnBestMove Then
                            If CurrentScore > BestMove.Score Then
                                'Move has been beaten (better) - replace it.
                                Alpha = CurrentScore
                                BestMove.Score = CurrentScore
                                ConvertBitMoveToMove(BestMove, BasePieceMoves(n))
                                BestBitMove = BasePieceMoves(n)
                            End If
                        Else
                            If CurrentScore < BestMove.Score Then
                                'Move has been beaten (worse) - replace it.
                                Beta = CurrentScore
                                BestMove.Score = CurrentScore
                                ConvertBitMoveToMove(BestMove, BasePieceMoves(n))
                                BestBitMove = BasePieceMoves(n)
                            End If
                        End If
                        If Beta <= Alpha Then Exit For 'The Alpha-Beta Window has been exceeded, and so the search is not valid. We must repeat it again at a larger window.
                    End If
                Next

                'Note that if PreviousBestScore = -InfScore then the Aspiration-Window can never be exceeded.
                If (AspirationWindow(0) < BestMove.Score AndAlso BestMove.Score < AspirationWindow(1)) OrElse ABORT Then
                    'Search produced a score within the Aspiration Window (ie: no Alpha-Beta cutoffs have occured), and so we assume the score is exact.
                    Exit While
                Else
                    'We must reproduce the search again, with the full Alpha-Beta window.
                    If SearchSettings.OutputToConsole Then
                        Console.ForegroundColor = ConsoleColor.Magenta
                        Console.WriteLine($"Search at Depth {Depth} Broke the Aspiration-Window. Re-searching with a Larger Window...")
                        'Console.WriteLine("Move that Caused Cut-Off: " & MoveConverter(PrimaryBoard, BestMove, PlayerTurn, PrimaryMeKPos, PrimaryEnPassant, PrimaryTFTable))
                        'Console.WriteLine($"Alpha-Beta: ({Alpha},{Beta}). Window: ({AspirationWindow(0)},{AspirationWindow(1)})." & vbCrLf & $"Score = {BestMove.Score}, Previous Score = {PreviousBestScore}")
                    End If

                    'If a move Caused an Alpha-Beta Cutoff, stores it as the first move to search (as it is clearly promising...)
                    If BestMove.Score >= AspirationWindow(1) Then
                        Dim LocationInMoves As Integer = Array.IndexOf(BasePieceMoves, BestBitMove)
                        If LocationInMoves > -1 Then
                            If SearchSettings.ReturnBestMove Then
                                Array.Copy(BasePieceMoves, 0, BasePieceMoves, 1, LocationInMoves)
                                BasePieceMoves(0) = BestBitMove
                            Else
                                'Puts the best (worst) move to be the last move, as moves will be searched in reverse.
                                Array.Copy(BasePieceMoves, LocationInMoves + 1, BasePieceMoves, LocationInMoves, BasePieceMoves.Length - LocationInMoves - 1)
                                BasePieceMoves(BasePieceMoves.Length - 1) = BestBitMove
                            End If
                        End If
                    End If

                    PreviousBestScore = -InfScore
                    BestMove.Score = InfScore * If(SearchSettings.ReturnBestMove, -1, 1)
                End If
            End While


            'Formats the score inside the correct range, then flips score if it is black to move.
            BestMove.Score /= 100
            'Erases the information from the MoveDebug Information.
            If SearchSettings.OutputToConsole AndAlso SearchSettings.OutputMoveDebugInfo Then Console.Write(New String(" "c, 64)) : Console.SetCursorPosition(0, Console.CursorTop)

            If Math.Abs(BestMove.Score) = 327.67 Then
                'Search either had 0 legal moves, or has been terminated so early that no move could be successfully searched.
                BestMove.Code = "a"c 'Note for aborted search.
                If SearchSettings.OutputToConsole Then
                    Console.ForegroundColor = ConsoleColor.DarkGreen
                    Console.WriteLine(("Depth Of " & Depth & " Incomplete.").PadRight(30))
                    Console.ForegroundColor = ConsoleColor.White
                End If
                If Not ABORT Then Return BestMove
            ElseIf SearchSettings.OutputToConsole Then

                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.Write("Depth Of " & Depth)
                If SearchSettings.UseQuiescence Then Console.Write("-" & GetHighestQuiescenceDepth()) 'Retrieves the maximum reached depth via Quiescence (if enabled).
                If ABORT Then Console.Write(" Incomplete. Predicted ") Else Console.Write(" Completed. ")
                OutputMoveInfo(BestMove) 'Outputs the diagnostics for the current search.
                If SearchSettings.OutputPath AndAlso SearchSettings.UseTranspositionTable AndAlso DetailedMoveOutput Then Console.WriteLine("Path = " & GenerateBestMoveLine(BestBitMove, Math.Abs(BestMove.Score) >= 295)) 'Generates the path of 'best moves' leading from this position.
            End If

            If SearchSettings.OutputToConsole AndAlso DetailedMoveOutput Then
                Console.WriteLine("Positions Searched: " & TotalPositionsSearched.ToString("N0"))
                If SearchSettings.UseTranspositionTable Then Console.WriteLine("Transposition Hits: " & TranspositionsFound.ToString("N0"))
                If Not SearchSettings.StableSearch Then Console.WriteLine("Late Fail-High Pos: " & NoRepeatedSearches.ToString("N0"))
                Console.WriteLine("Win Sequence Count: " & WinsFound.ToString("N0") & vbCr)
            End If

            If SearchSettings.UpdateLifetimeStats Then
                'Increments lifetime stats.
                LifetimePositions += TotalPositionsSearched
                LifetimeTranspositions += TranspositionsFound
                If Math.Abs(BestMove.Score) = 299.99 Then LifetimeCheckmates += 1UL
            End If
        Else 'AI not correctly instantiated.
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when Attempting Search - FEN Position not set correctly / Depth set too low.")
            Console.ForegroundColor = ConsoleColor.White
            BestMove.Code = "a"c
        End If
        Return BestMove
    End Function

    'Search function with added 'PreviousBestMove' attribute - ammends 'BasePieceMoves' so that PreviousBestMove will be searched first.
    Public Function Search(ByVal Depth As Integer, ByVal PreviousBestMove As Move) As Move
        Dim TempMove As UInt16 = PreviousBestMove.BitMove
        Dim LocationInMoves As Integer
        If BasePieceMoves(0) <> TempMove Then
            'Finds the location of PreviousBestMove in BasePieceMoves.
            For n = 1 To BasePieceMoves.Length - 1
                If BasePieceMoves(n) = TempMove Then
                    LocationInMoves = n
                    Exit For
                End If
            Next
            If LocationInMoves > 0 Then
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
                Console.ForegroundColor = ConsoleColor.White
            End If
        End If
        'Performs the search as normal.
        If SearchSettings.UseQuiescence AndAlso Not SearchSettings.StableSearch AndAlso Depth >= 4 AndAlso Math.Abs(PreviousBestMove.Score) <= 300 Then
            Return Search(Depth, CShort(PreviousBestMove.Score * 100)) 'For aspiration Windows.
        Else
            Return Search(Depth)
        End If
    End Function


    'Function that performs a shallow (depth of 2, no Quiescence) search on the position, that is assumed to take
    'up almost no time. Used to test if a position is valid, to test AI features (+ debugging), and to perform
    'a simple search on the position, in case the main AI search cannot be completed in time.
    Public Function PerformTestSearch() As Move
        Dim TempQuiescenceOption As Boolean = SearchSettings.UseQuiescence
        SearchSettings.UseQuiescence = False
        Dim TestSearchMove As Move = Search(2)
        SearchSettings.UseQuiescence = TempQuiescenceOption
        Return TestSearchMove
    End Function




    Public Sub EnableMoveDebugInfo()
        SearchSettings.OutputMoveDebugInfo = True
    End Sub

    Public Function GetHighestQuiescenceDepth() As Integer
        Return HighestQuiescenceDepth
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
        ConvertBitMoveToMove(TempMove, BestMove)
        Dim BestLine As String = MoveConverter(PrimaryBoard, TempMove, PrimaryEnPassant)

        'Copies primary board attributes to their temporary counterparts.
        Dim isWhite As Boolean = PlayerTurn
        Dim TempBoard(7, 7) As Char
        Array.Copy(PrimaryBoard, TempBoard, 64)
        Dim TempWCanCastle, TempBCanCastle As New CanCastle
        Dim TempWKPos, TempBKPos As Int16
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
        Dim TempMaterialCount(1) As Integer
        TempMaterialCount(0) = Int32.MaxValue \ 2
        TempMaterialCount(1) = Int32.MaxValue \ 2
        Dim TempEnPassant As Int16 = PrimaryEnPassant
        Dim TempZobristValue As UInt64 = PrimaryZobristValue


        Dim TempTTEntry As TTEntry
        Dim EntryInTT As Int32
        Dim MaxIterations As Byte = 20

        'Caps the maximum amount of moves being displaced to prevent the system from getting stuck in a loop.
        For i As Byte = 1 To MaxIterations
            'Makes the AI's calculated Best Move (from this position) on the temporary board.
            If isWhite Then
                MakeMove(TempBoard, BestMove, TempWCanCastle, TempWKPos, TempMaterialCount, TempEnPassant, TempZobristValue, 0US)
            Else
                MakeMove(TempBoard, BestMove, TempBCanCastle, TempBKPos, TempMaterialCount, TempEnPassant, TempZobristValue, 0US)
            End If

            'Hashes the current position, then finds the TranspositionTable entry containing that move.
            EntryInTT = CInt(TempZobristValue >> GlobalConstants.TranspositionTableSize)
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
                TranspositionTable(EntryInTT).Generation = 3S + TTGeneration
                'We have stored a move in this position - retrieve this move, then add the PGN version of it to BestLine.
                BestMove = TempTTEntry.BestMove
                ConvertBitMoveToMove(TempMove, BestMove)
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
                MakeMove(TempBoard, TempMove.BitMove, TempMeCanCastle, 0S, {10000, 10000}, TempEnPassant, 0UL, 0US)
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



    'Subroutine which outputs the lifetime stats of the AI to the AIStats file.
    Public Sub OutputStatsToFile()
        Using SR As New StreamWriter(GlobalConstants.StartupPath & "\Assets\User\AIStats.txt")
            SR.WriteLine(LifetimePositions)
            SR.WriteLine(LifetimeTranspositions)
            SR.WriteLine(LifetimeCheckmates)
        End Using
    End Sub

    'Function which calculates how full the TranspositionTable is.
    Public Function GetPercentageTranspositionTableFilled() As Double
        Dim TotalHits As Integer
        If Not TTIsEmpty Then
            For n = 0 To TranspositionTable.Length - 1
                If TranspositionTable(n).Key > 0 Then TotalHits += 1
            Next
        End If
        Return Math.Round(TotalHits / TranspositionTable.Length, 3)
    End Function





    'Function that returns all the legal moves of a given piece on the board.
    'Used for when the user is attempting to move a piece on the GUI.
    Public Function ReturnPiecesLegalMoves(ByVal CoorX As String, ByVal CoorY As String) As String()
        Dim LegalMoves As New List(Of String)
        If HasBeenInstantiated AndAlso BasePieceMoves IsNot Nothing Then
            'Loops through all of the legal moves in the position, and makes a note of all of them that involve
            'the piece that is wanting to move.
            Dim PieceMove As String
            For n = 0 To BasePieceMoves.Length - 1
                If Val(CoorX) = (BasePieceMoves(n) And 3584) >> 9 AndAlso Val(CoorY) = (BasePieceMoves(n) And 448) >> 6 Then
                    'Move found - add it to LegalMoves.
                    PieceMove = ((BasePieceMoves(n) And 56) >> 3).ToString() & (BasePieceMoves(n) And 7).ToString()
                    If Not LegalMoves.Contains(PieceMove) Then LegalMoves.Add(PieceMove) 'Removes any duplicates that arrise from actions such as pawn promotions.
                End If
            Next
        Else 'AI not correctly instantiated.
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when Attempting Search - FEN Position not set / no Legal Moves in Position.")
            Console.ForegroundColor = ConsoleColor.White
        End If
        If LegalMoves.Count = 0 Then Return Nothing Else Return LegalMoves.ToArray()
    End Function

    'Function that scans the position for End States - positions where the game must terminate.
    Public Function CheckForEndState() As Move
        Dim CurrentMove As New Move
        If HasBeenInstantiated Then
            If BasePieceMoves Is Nothing Then
                'No legal moves in the position - hence the player is either in checkmate, or is in a stalemate.
                If PrimaryMeInCheck >= 128 Then
                    CurrentMove.Code = "c"c 'Checkmate flag.
                Else
                    CurrentMove.Code = "s"c 'Stalemate flag.
                End If
            Else
                If BasePieceMoves.GetUpperBound(0) = 0 Then
                    'Only one legal move in the position - make a note of this move.
                    ConvertBitMoveToMove(CurrentMove, BasePieceMoves(0))
                    CurrentMove.Code = "o"c 'One move flag.
                Else
                    'Multiple moves detected - flag accordingly.
                    CurrentMove.Code = "f"c
                End If
            End If
        Else 'AI not correctly instantiated.
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when Attempting Search - FEN Position not set / no Legal Moves in Position.")
            Console.ForegroundColor = ConsoleColor.White
            CurrentMove.Code = "a"c
        End If
        Return CurrentMove
    End Function




    'Method which uses the NegaMax algorithm to calculate how many nodes / positions stem from the current position.
    'Used for testing the AI system.
    Public Sub PerformNodeTestOnPosition(ByVal Depth As Integer)
        Console.WriteLine()
        If HasBeenInstantiated AndAlso Depth > 0 Then
            ABORT = False
            TERMINATED = False
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
                NodeTest(PrimaryBoard, Depth, PlayerTurn, PrimaryMeCanCastle, PrimaryEnemyCanCastle, PrimaryMeKPos, PrimaryEnemyKPos, PrimaryEnPassant, PrimaryZobristValue)
                NodeTestStopwatch.Stop()
                GCSettings.LatencyMode = GCLatencyMode.Interactive

                If Not ABORT Then
                    'Outputs the statistics of the node test.
                    Console.ForegroundColor = ConsoleColor.DarkGreen
                    Console.WriteLine("Node Test at a Depth of " & Depth & " Completed.     ")
                    Console.ForegroundColor = ConsoleColor.White

                    Dim NodesPerSecond As Double = (NodeCount + EndPositionCount) / NodeTestStopwatch.Elapsed.TotalMilliseconds
                    Dim NodesPerSecondString As String = If(NodesPerSecond >= 1000.0, NodesPerSecond.ToString("N0"), CStr(Math.Round(NodesPerSecond, 1)))
                    Console.WriteLine((NodeCount + EndPositionCount).ToString("N0") & " Nodes Searched in " & NodeTestStopwatch.Elapsed.TotalMilliseconds.ToString("N0") & "ms (" & NodesPerSecondString & "k Nodes/s).")

                    Console.Write("Total Position Count = " & EndPositionCount.ToString("N0"))
                    If SearchSettings.NodeSearchCalculateRepetitions Then Console.WriteLine(" (" & (EndPositionCount - PositionCollisions).ToString("N0") & " Unique Positions).") Else Console.WriteLine(".")
                End If
            Else
                'Depth is 1, or there are no legal moves in the position.
                'Therefore, we can just use the total legal move count in the current position to determine the node count.
                Dim TotalMoveCount As Integer
                If BasePieceMoves IsNot Nothing Then TotalMoveCount = BasePieceMoves.Length Else ABORT = True
                Console.ForegroundColor = ConsoleColor.DarkGreen
                Console.WriteLine("Node Test at a Depth of " & Depth & " Completed. Total Node Count = " & TotalMoveCount & ".")
            End If
        Else
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when Attempting Node Test - FEN Position not set / depth set too low.")
            Console.ForegroundColor = ConsoleColor.White
            ABORT = True 'Stops future depths from being performed.
        End If
    End Sub

    'Subroutine which uses the NegaMax algorithm to calculate the total nodes in a given board position.
    Private Sub NodeTest(ByVal Board(,) As Char, ByVal depth As Integer, ByVal isWhite As Boolean, ByVal MeCanCastle As CanCastle, ByVal EnemyCanCastle As CanCastle, ByVal MeKPos As Int16, ByVal EnemyKPos As Int16, ByVal EnPassant As Int16, ByVal ZobristValue As UInt64)
        If ABORT Then Exit Sub
        NodeCount += 1UL
        Dim MeInCheck As UInt16

        'Creates temporary vehicles.
        Dim TempBoard(7, 7) As Char
        Dim TempMeKPos As Int16
        Dim TempMeCanCastle As New CanCastle
        Dim TempMeInCheck As UInt16
        Dim TempEnPassant As Int16
        Dim TempZobristValue As UInt64

        Dim PieceMoves() As UInt16 = NodeTestCreateMoves(Board, isWhite, MeKPos, MeInCheck, MeCanCastle, EnPassant)
        If PieceMoves IsNot Nothing Then
            'At least one pseudo-legal move exists.
            For n = 0 To PieceMoves.Length - 1
                'Adds capture flag to the move, if needed.
                If Board((PieceMoves(n) And 56) >> 3, PieceMoves(n) And 7) <> " " Then PieceMoves(n) = (PieceMoves(n) Or 32768US)

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
                    'Copies the board position to its temporary counterparts.
                    Array.Copy(Board, TempBoard, 64)
                    TempEnPassant = EnPassant
                    TempZobristValue = ZobristValue
                    TempMeKPos = MeKPos
                    TempMeCanCastle.CopyFrom(MeCanCastle)

                    'Makes the current move onto the temporary board.
                    MakeMove(TempBoard, PieceMoves(n), TempMeCanCastle, TempMeKPos, {10000, 10000}, TempEnPassant, TempZobristValue, 0S)

                    If depth = 1 Then
                        EndPositionCount += 1UL
                        If SearchSettings.NodeSearchCalculateRepetitions Then
                            'Leaf node reached - check if end position has already been encountered, using the Zobrist Hash of the position.
                            Dim EntryInTT As Integer = CInt(TempZobristValue >> GlobalConstants.TranspositionTableSize)
                            If TranspositionTable(EntryInTT).Key = TempZobristValue Then
                                PositionCollisions += 1UL
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
                'If depth = 2 Then
                '    OutputBitMoveToConsole(PieceMoves(n))
                '    Console.WriteLine(EndPositionCount)
                '    EndPositionCount = 0
                'End If
            Next
        End If
    End Sub
    Private Function NodeTestCreateMoves(ByRef Board(,) As Char, ByVal isWhite As Boolean, ByRef KPos As Int16, ByRef InCheck As UInt16, ByRef CanCastle As CanCastle, ByRef EnPassant As Int16) As UInt16()
        Dim TotalMoves(GlobalConstants.MaxTurnLegalMoves) As UInt16
        Dim MoveCount As UInt16
        Dim PieceMoves() As UInt16
        FixTFTable(Board, isWhite, NegaMaxTFTable, KPos, InCheck, EnPassant) 'Creates the TFTable for the player, in preparation for move generation.

        If isWhite Then
            For y = 0US To 7US
                For x = 0US To 7US
                    If Char.IsUpper(Board(x, y)) Then
                        'Creates the pseudo-legal moves for each piece on the board.
                        PieceMoves = WhitePieceLegalMoves(Board, x, y, NegaMaxTFTable, InCheck, CanCastle, EnPassant)
                        Array.Copy(PieceMoves, 1, TotalMoves, MoveCount, PieceMoves(0)) 'Adds this to the big array of moves.
                        MoveCount += PieceMoves(0)
                    End If
                Next
            Next
        Else 'Same code, for the black pieces.
            For y = 0US To 7US
                For x = 0US To 7US
                    If Char.IsLower(Board(x, y)) Then
                        PieceMoves = BlackPieceLegalMoves(Board, x, y, NegaMaxTFTable, InCheck, CanCastle, EnPassant)
                        Array.Copy(PieceMoves, 1, TotalMoves, MoveCount, PieceMoves(0))
                        MoveCount += PieceMoves(0)
                    End If
                Next
            Next
        End If
        If MoveCount > 0 Then Array.Resize(TotalMoves, MoveCount) : Return TotalMoves 'Shaves off the unused moves, to save memory.
        Return Nothing
    End Function






    'Function which creates and orders all the pseudo-legal moves a player can make, given certain criteria.
    Public Function CreateMoves(ByRef Board(,) As Char, ByVal isWhite As Boolean, ByRef TrueFalseTable(,) As Char, ByVal KPos As Int16, ByVal InCheck As UInt16, ByVal CanCastle As CanCastle, ByVal EnPassant As Int16, ByVal OnlyCaptures As Boolean, ByVal KillerDepth As Integer, ByVal TTMove As UInt16) As UInt16()
        Dim ArrLens(7) As Integer 'Represents the number of moves in each move category array.

        'Creates variables for finding the Transposition Table's Best Move from the current position.
        Dim UseTTMove As Boolean = (TTMove <> 0)
        'Dim PieceMayBeTTMove As Boolean
        Dim TTMoveFoundInPosition As Byte

        'Variables that handle the KillerMoves array - first determines if the correct KillerMove index is empty.
        Dim PieceKillerOneFull As Boolean = KillerMoves(KillerDepth, 0) <> 0
        Dim PieceKillerTwoFull As Boolean = KillerMoves(KillerDepth, 1) <> 0
        Dim KillerMoveOneCount, KillerMoveTwoCount As Byte


        Dim PieceValueDif As Integer 'Difference in weight between the capturing piece, and the piece being captured.
        Dim XPosNew, YPosNew As Integer
        Dim PieceMoves() As UInt16

        If isWhite Then
            For y = 0US To 7US
                For x = 0US To 7US
                    If Char.IsUpper(Board(x, y)) Then
                        'Generates the moves that the piece can make.
                        PieceMoves = WhitePieceLegalMoves(Board, x, y, TrueFalseTable, InCheck, CanCastle, EnPassant)

                        For n = 1 To PieceMoves(0) 'for each move...

                            'Checks if the move is the stored move in the Transposition Table.
                            If UseTTMove AndAlso PieceMoves(n) = (TTMove And 32767US) Then
                                'Move found - stop any other moves from being checked against TTMove.
                                TTMoveFoundInPosition = 1
                                UseTTMove = False
                                'PieceMayBeTTMove = False
                            Else
                                XPosNew = (PieceMoves(n) And 56) >> 3
                                YPosNew = PieceMoves(n) And 7
                                If Board(XPosNew, YPosNew) <> " " Then '= capture move.
                                    PieceMoves(n) = PieceMoves(n) Or 32768US 'Adds capture flag.
                                    'Gets the difference in weight between the capturing piece, and the captured piece.
                                    PieceValueDif = ReturnPieceValue(Board(XPosNew, YPosNew)) - ReturnPieceValue(Board(x, y))
                                    If PieceValueDif > 0 Then 'Capturing piece weighs less than captured piece.
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
                                ElseIf (PieceMoves(n) And 28672US) = 12288US Then 'Is an En Passant Capture - add to Capture Moves.
                                    '(aghhh I don't like how we need to check EVERY single move to see whether it's EnPassant) :((
                                    CaptureMoves(ArrLens(1)) = PieceMoves(n)
                                    ArrLens(1) += 1
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
                                        ElseIf Not (YPosNew < 2 OrElse (XPosNew < 7 AndAlso Board(7, YPosNew - 1) <> "p"c) OrElse (XPosNew > 0 AndAlso Board(0, YPosNew - 1) <> "p"c)) Then
                                            'New square is controlled by an enemy pawn - ammend move list.
                                            TerribleMoves(ArrLens(7)) = PieceMoves(n)
                                            ArrLens(7) += 1
                                        ElseIf Math.Max(Math.Abs(((KPos And 56) >> 3) - XPosNew), Math.Abs((KPos And 7) - YPosNew)) <= 3 Then
                                            'Piece moves to a location close to the enemy king - leading to a possible check / attack.
                                            GoodMoves(ArrLens(4)) = PieceMoves(n)
                                            ArrLens(4) += 1
                                        ElseIf TrueFalseTable(XPosNew, YPosNew) = "F"c Then
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
                Next
            Next
        Else 'Identical code for the black pieces.
            For y = 0US To 7US
                For x = 0US To 7US
                    If Char.IsLower(Board(x, y)) Then
                        PieceMoves = BlackPieceLegalMoves(Board, x, y, TrueFalseTable, InCheck, CanCastle, EnPassant)

                        For n = 1 To PieceMoves(0)

                            If UseTTMove AndAlso PieceMoves(n) = (TTMove And 32767US) Then
                                TTMoveFoundInPosition = 1
                                UseTTMove = False
                            Else
                                XPosNew = (PieceMoves(n) And 56) >> 3
                                YPosNew = PieceMoves(n) And 7
                                If Board(XPosNew, YPosNew) <> " " Then
                                    PieceMoves(n) = PieceMoves(n) Or 32768US
                                    PieceValueDif = ReturnPieceValue(Board(XPosNew, YPosNew)) - ReturnPieceValue(Board(x, y))
                                    If PieceValueDif > 0 Then
                                        AmazingCaptureMoves(ArrLens(0)) = PieceMoves(n)
                                        ArrLens(0) += 1
                                    ElseIf PieceValueDif = 0 Then
                                        CaptureMoves(ArrLens(1)) = PieceMoves(n)
                                        ArrLens(1) += 1
                                    Else
                                        OtherCaptureMoves(ArrLens(2)) = PieceMoves(n)
                                        ArrLens(2) += 1
                                    End If
                                ElseIf (PieceMoves(n) And 28672US) = 12288US Then
                                    CaptureMoves(ArrLens(1)) = PieceMoves(n)
                                    ArrLens(1) += 1
                                ElseIf Not OnlyCaptures Then
                                    If PieceKillerOneFull AndAlso PieceMoves(n) = KillerMoves(KillerDepth, 0) Then
                                        KillerMoveOneCount = 1
                                        PieceKillerOneFull = False
                                    ElseIf PieceKillerTwoFull AndAlso PieceMoves(n) = KillerMoves(KillerDepth, 1) Then
                                        KillerMoveTwoCount = 1
                                        PieceKillerTwoFull = False
                                    Else
                                        If Board(x, y) = "p"c AndAlso YPosNew >= 6 Then
                                            PawnPromotionMoves(ArrLens(3)) = PieceMoves(n)
                                            ArrLens(3) += 1
                                        ElseIf Not (YPosNew > 5 OrElse (XPosNew < 7 AndAlso Board(7, YPosNew + 1) <> "P"c) OrElse (XPosNew > 0 AndAlso Board(0, YPosNew + 1) <> "P"c)) Then
                                            TerribleMoves(ArrLens(7)) = PieceMoves(n)
                                            ArrLens(7) += 1
                                        ElseIf Math.Max(Math.Abs(((KPos And 56) >> 3) - XPosNew), Math.Abs((KPos And 7) - YPosNew)) <= 3 Then
                                            GoodMoves(ArrLens(4)) = PieceMoves(n)
                                            ArrLens(4) += 1
                                        ElseIf TrueFalseTable(XPosNew, YPosNew) = "F"c Then
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
                Next
            Next
        End If

        'Creates a new array that represents the total move count.
        Dim TotalMoveCount As Integer = TTMoveFoundInPosition + ArrLens(0) + ArrLens(1) + ArrLens(2)
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


            If ArrLens(3) > 0 Then Array.Copy(PawnPromotionMoves, 0, AllMoves, ArrLens(0), ArrLens(3)) : ArrLens(0) += ArrLens(3)
            Array.Copy(GoodMoves, 0, AllMoves, ArrLens(0), ArrLens(4))
            Array.Copy(OtherMoves, 0, AllMoves, ArrLens(0) + ArrLens(4), ArrLens(5))
            ArrLens(0) += ArrLens(4) + ArrLens(5)
            Array.Copy(BadMoves, 0, AllMoves, ArrLens(0), ArrLens(6))
            Array.Copy(TerribleMoves, 0, AllMoves, ArrLens(0) + ArrLens(6), ArrLens(7))
        End If

        Return AllMoves
    End Function


    'Algorithm that orderes the moves (as created by CreateMoves) in a more distinguished fashion. Note that this is only valid for the Base Position.
    Public Sub SortMovesThourough(ByRef Moves() As UInt16, ByRef Board(,) As Char, ByVal isWhite As Boolean, ByVal MeKPos As Int16, ByVal EnemyKPos As Int16, ByVal CanCastle As CanCastle, ByVal EnPassant As Int16, ByVal ZobristValue As UInt64)
        If Moves Is Nothing Then Exit Sub
        Dim MoveScores(Moves.Length - 1) As Double
        Dim XOld, YOld, XNew, YNew As Integer
        Dim IsCaptureMove, IsControlledByEnemyPawn As Boolean

        Dim OldEval, NewEval As Int16
        If SearchSettings.UsePieceHeatMaps Then OldEval = Evaluate(Board, isWhite, {10000, 10000}, MeKPos, EnemyKPos)

        'Generates the full TFTable, for all the opposing pieces.
        Dim MeInCheck As UInt16
        Array.Copy(MasterTrueTable, NegaMaxTFTable, 64)
        For y = 0US To 7US
            For x = 0US To 7US
                If Not isWhite AndAlso Char.IsUpper(Board(x, y)) Then
                    WhitePieceLegalMoves(Board, x, y, NegaMaxTFTable, CUShort(MeKPos), MeInCheck, CUShort(EnPassant))
                ElseIf isWhite AndAlso Char.IsLower(Board(x, y)) Then
                    BlackPieceLegalMoves(Board, x, y, NegaMaxTFTable, CUShort(MeKPos), MeInCheck, CUShort(EnPassant))
                End If
            Next
        Next

        For n = 0 To Moves.Length - 1
            'Converts the move into a more friendly format.
            XOld = (Moves(n) And 3584US) >> 9
            YOld = (Moves(n) And 448US) >> 6
            XNew = (Moves(n) And 56) >> 3
            YNew = Moves(n) And 7
            MoveScores(n) = 10000

            If (Moves(n) And 32768) = 32768 OrElse (Moves(n) And 28672US) = 12288US Then '= capture move.
                IsCaptureMove = True
                'Gets the difference in weight between the capturing piece, and the captured piece (MVV-LVA).
                Dim PieceValueDiff As Double = If((Moves(n) And 32768) = 32768, 1.1 * ReturnPieceValue(UCase(Board(XNew, YNew))), GlobalConstants.PieceWeight.Pawn)
                PieceValueDiff -= ReturnPieceValue(Board(XOld, YOld))
                MoveScores(n) += 30000 + PieceValueDiff 'Huge bonus for taking pieces, and especially relatively heavy pieces.
            Else
                IsCaptureMove = False
                If UCase(Board(XOld, YOld)) = "P"c Then
                    If YNew = 0 OrElse YNew = 7 Then 'User has promoted a pawn - big bonus :D.
                        MoveScores(n) += 1.5 * If((Moves(n) And 28672US) = 4096US, GlobalConstants.PieceWeight.Queen, GlobalConstants.PieceWeight.Knight)
                    ElseIf YNew <= 1 OrElse YNew >= 6 Then 'User is very close to promoting a pawn.
                        MoveScores(n) += 100
                    End If
                End If
                If Math.Max(Math.Abs(((EnemyKPos And 56) >> 3) - XNew), Math.Abs((EnemyKPos And 7) - YNew)) <= 3 Then
                    'Piece moves to a location close to the enemy king - leading to a possible check / attack.
                    MoveScores(n) += 50
                End If
                'Checks if the new square is controlled by an enemy pawn.
                If isWhite Then
                    IsControlledByEnemyPawn = Not (YNew < 2 OrElse (XNew < 7 AndAlso Board(7, YNew - 1) <> "p"c) OrElse (XNew > 0 AndAlso Board(0, YNew - 1) <> "p"c))
                Else
                    IsControlledByEnemyPawn = Not (YNew > 5 OrElse (XNew < 7 AndAlso Board(7, YNew + 1) <> "P"c) OrElse (XNew > 0 AndAlso Board(0, YNew + 1) <> "P"c))
                End If
                If IsControlledByEnemyPawn Then MoveScores(n) -= (50 + 0.5 * ReturnPieceValue(Board(XOld, YOld)))
            End If


            If NegaMaxTFTable(XNew, YNew) = "F" Then
                If IsCaptureMove Then
                    'The captured piece can be recaptured on the next move. Add a small penalty.
                    MoveScores(n) -= 100
                Else
                    'The piece is moving to a position that is attacked by an enemy piece. Add a small penalty.
                    MoveScores(n) -= 50
                End If
            End If

            'Calculates if the move puts the opposing player into check.
            'Creates temporary variables.
            Dim TempBoard(7, 7), TempTFTable(7, 7) As Char
            Array.Copy(Board, TempBoard, 64)
            Dim TempMeKPos As Int16 = MeKPos
            Dim TempMeCanCastle As New CanCastle
            TempMeCanCastle.CopyFrom(CanCastle)
            Dim TempInCheck As UInt16
            Dim TempEnPassant As Int16 = EnPassant
            Dim TempZobristValue As UInt64 = ZobristValue

            MakeMove(TempBoard, Moves(n), TempMeCanCastle, TempMeKPos, {10000, 10000}, TempEnPassant, TempZobristValue, 0US)

            FixTFTable(TempBoard, Not isWhite, TempTFTable, EnemyKPos, TempInCheck, TempEnPassant)
            If TempInCheck >= 128 Then
                'The move has put the enemy king in check - give a big bonus.
                MoveScores(n) += 20000
            End If

            'Evaluates how this move improves the player's position, using the PieceHeatMaps.
            If SearchSettings.UsePieceHeatMaps Then
                NewEval = Evaluate(TempBoard, Not isWhite, {10000, 10000}, TempMeKPos, EnemyKPos)
                MoveScores(n) -= (NewEval + OldEval) * 0.5
            End If

            'If we are able to find the new position in the Transposition Table, the position is likely 'interesting' - add a small bonus.
            Dim EntryInTT As Integer = CInt(TempZobristValue >> GlobalConstants.TranspositionTableSize)
            If TranspositionTable(EntryInTT).Key = TempZobristValue Then MoveScores(n) += 25
        Next

        'Orderes the moves using the bubble sort.
        Dim IsSorted As Boolean
        Dim swapI As UInt16
        Dim swapD As Double
        For n = 0 To Moves.Length - 2
            IsSorted = True
            For m = 0 To Moves.Length - n - 2
                If MoveScores(m) < MoveScores(m + 1) Then
                    'Swap the moves, and their scores.
                    swapI = Moves(m + 1)
                    Moves(m + 1) = Moves(m)
                    Moves(m) = swapI

                    swapD = MoveScores(m + 1)
                    MoveScores(m + 1) = MoveScores(m)
                    MoveScores(m) = swapD

                    IsSorted = False
                End If
            Next
            If IsSorted Then Exit For
        Next
    End Sub



    'Function which receives a game position and a possible move. The function makes this move on the board (using
    'shortcuts that can only be made on a virtual board), and then generates the possible moves of the attacking
    'piece(s). If the king is no longer being threatened, then the check as been resolved.
    Private Function DoesMoveResolveCheck(ByRef Board(,) As Char, ByVal PlayerTurn As Boolean, ByVal Move As UInt16, ByRef InCheck As UInt16) As Boolean
        'If the player is in a double check, then only king moves could be legal. Therefore, as this part is done in the
        'TFTable generation, we skip this here.
        If (InCheck And 64) = 0 Then
            Dim NewPosX As UInt16 = (Move And 56US) >> 3
            Dim NewPosY As UInt16 = Move And 7US

            If ((NewPosX << 3) Or NewPosY) = (InCheck And 63) Then
                'Move captures the attacking piece. Therefore we assume the move is legal.
                Return True
            ElseIf (Move And 28672US) = 12288US AndAlso (NewPosX << 3) = (InCheck And 56) Then
                'An en-passant capture is made, that captures the checking piece - move is legal.
                Return True
            ElseIf Move < 32768 Then
                'Move is not a capture move.
                InCheck = InCheck And 127US
                If PlayerTurn Then Board(NewPosX, NewPosY) = "O"c Else Board(NewPosX, NewPosY) = "o"c 'Move made on temporary board.
                'Calculate the legal moves of the attacking piece. If the king is still in check, then WInCheck.IsInCheck
                'will flag from False to True - therefore it is illegal.
                If PlayerTurn Then
                    BlackPieceLegalMoves(Board, (InCheck And 56US) >> 3, InCheck And 7US, TrueTable, CByte(0), InCheck, CByte(0))
                Else
                    WhitePieceLegalMoves(Board, (InCheck And 56US) >> 3, InCheck And 7US, TrueTable, CByte(0), InCheck, CByte(0))
                End If
                Board(NewPosX, NewPosY) = " "c 'Move un-made on temporary board.
                If InCheck < 128 Then Return True
            End If
            'Otherwise, the move is a capture move, but not capturing the attacking piece. Therefore, it has to be illegal.
        End If
        Return False
    End Function



    'Subroutine that makes a move on the board, given coordinates. Includes castling (& rights), pawn promotion, and manipuation of ZobristValue.
    Private Sub MakeMove(ByRef Board(,) As Char, ByVal Move As UInt16, ByRef CanCastle As CanCastle, ByRef KPos As Int16, ByRef MaterialCount() As Integer, ByRef EnPassant As Int16, ByRef ZobristValue As UInt64, ByRef HalfMoveSize As UInt16)
        Dim OldCoorX As UInt16 = (Move And 3584US) >> 9
        Dim OldCoorY As UInt16 = (Move And 448US) >> 6
        Dim NewCoorX As UInt16 = (Move And 56US) >> 3
        Dim NewCoorY As UInt16 = Move And 7US

        Dim TempPiece As Char = Board(OldCoorX, OldCoorY)
        Dim HasEnPassanted As Boolean
        HalfMoveSize += 1 'We assume that the move is not a pawn move or a capture, and increment the Half-Move count. If we are wrong, we just reset to 0 :).
        'If TempMove > 32768 Then MakeMove = Board((TempMove And 56) >> 3, TempMove And 7)
        If Char.IsUpper(TempPiece) Then
            'Removes the piece from the board's Zobrist Value.
            ZobristValue = ZobristValue Xor ZobristHashTable(Asc(TempPiece) Mod 11, 0, OldCoorX, OldCoorY)
            If TempPiece = "P"c Then
                'Code for Promoting Pawns and En Passant. Also increments the material count.
                If (Move And 28672US) > 0US Then
                    If (Move And 28672) = 8192 AndAlso (Board(Math.Max(NewCoorX - 1, 0), 4) = "p" OrElse Board(Math.Min(NewCoorX + 1, 7), 4) = "p") Then
                        'EnPassant creation.
                        If EnPassant <> 0 Then ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, (EnPassant And 56) >> 3, 2)
                        EnPassant = CShort((NewCoorX << 3US) Or 5US)
                        ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, NewCoorX, 5) 'Ammended for en passant creation.
                        HasEnPassanted = True
                    ElseIf (Move And 28672) = 12288 Then
                        Board(NewCoorX, 3) = " "c
                        ZobristValue = ZobristValue Xor ZobristHashTable(3, 1, NewCoorX, 3) 'Ammended for a capture of a pawn.
                        MaterialCount(1) -= GlobalConstants.PieceWeight.Pawn
                    ElseIf (Move And 28672) = 4096 Then 'Queen Promotion.
                        TempPiece = "Q"c
                        MaterialCount(0) += GlobalConstants.PieceWeight.Queen - GlobalConstants.PieceWeight.Pawn '+ 9 for a new queen, - 1 for losing the pawn in the process.
                    ElseIf (Move And 28672) = 28672 Then 'Knight Promotion.
                        TempPiece = "N"c
                        MaterialCount(0) += GlobalConstants.PieceWeight.Knight - GlobalConstants.PieceWeight.Pawn '+ 3 for a new knight, - 1 for losing the pawn in the process.
                    End If
                End If
                HalfMoveSize = 0 'A pawn has moved - reset the Half-Move.
                'If piece is a Rook, part of Castling is disabled (depending on which Rook has moved).
            ElseIf TempPiece = "R"c AndAlso CanCastle.CanICastle Then
                If CanCastle.KS AndAlso OldCoorX = 7 AndAlso OldCoorY = 7 Then
                    'Rook has been moved - the player can no longer castle that side of the board.
                    CanCastle.KS = False
                    ZobristValue = ZobristValue Xor HashConstants(1) 'Ammends the Zobrist Value for that player no longer being able to castle.
                ElseIf CanCastle.QS AndAlso OldCoorX = 0 AndAlso OldCoorY = 7 Then
                    CanCastle.QS = False
                    ZobristValue = ZobristValue Xor HashConstants(2)
                End If
            ElseIf TempPiece = "K"c Then
                KPos = CShort(Move And 63US)
                'Code for Castling.
                If CanCastle.KS Then
                    If Move = 23031 Then
                        'Moves elements about on the board, and the Zobrist value.
                        Board(5, 7) = "R"c
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 0, 5, 7)
                        Board(7, 7) = " "c
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 0, 7, 7)
                    End If
                    'Player can no longer castle.
                    CanCastle.KS = False
                    ZobristValue = ZobristValue Xor HashConstants(1)
                End If
                If CanCastle.QS Then
                    If Move = 27095 Then
                        Board(0, 7) = " "c
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 0, 0, 7)
                        Board(3, 7) = "R"c
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
            If TempPiece = "p"c Then
                If (Move And 28672US) > 0US Then
                    If (Move And 28672) = 8192 AndAlso (Board(Math.Max(NewCoorX - 1, 0), 3) = "P" OrElse Board(Math.Min(NewCoorX + 1, 7), 3) = "P") Then
                        If EnPassant <> 0 Then ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, (EnPassant And 56) >> 3, 5)
                        EnPassant = CShort((NewCoorX << 3US) Or 2US)
                        ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, NewCoorX, 2)
                        HasEnPassanted = True
                    ElseIf (Move And 28672) = 12288 Then
                        Board(NewCoorX, 4) = " "c
                        ZobristValue = ZobristValue Xor ZobristHashTable(3, 0, NewCoorX, 4)
                        MaterialCount(0) -= GlobalConstants.PieceWeight.Pawn
                    ElseIf (Move And 28672) = 4096 Then
                        TempPiece = "q"c
                        MaterialCount(1) += GlobalConstants.PieceWeight.Queen - GlobalConstants.PieceWeight.Pawn
                    ElseIf (Move And 28672) = 28672 Then
                        TempPiece = "n"c
                        MaterialCount(1) += GlobalConstants.PieceWeight.Knight - GlobalConstants.PieceWeight.Pawn
                    End If
                End If
                HalfMoveSize = 0
            ElseIf TempPiece = "r"c AndAlso CanCastle.CanICastle Then
                If CanCastle.KS AndAlso OldCoorX = 7 AndAlso OldCoorY = 0 Then
                    CanCastle.KS = False
                    ZobristValue = ZobristValue Xor HashConstants(3)
                ElseIf CanCastle.QS AndAlso OldCoorX = 0 AndAlso OldCoorY = 0 Then
                    CanCastle.QS = False
                    ZobristValue = ZobristValue Xor HashConstants(4)
                End If
            ElseIf TempPiece = "k"c Then
                KPos = CShort(Move And 63US)
                If CanCastle.KS Then
                    If Move = 22576 Then
                        Board(5, 0) = "r"c
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 1, 5, 0)
                        Board(7, 0) = " "c
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 1, 7, 0)
                    End If
                    CanCastle.KS = False
                    ZobristValue = ZobristValue Xor HashConstants(3)
                End If
                If CanCastle.QS Then
                    If Move = 26640 Then
                        Board(0, 0) = " "c
                        ZobristValue = ZobristValue Xor ZobristHashTable(5, 1, 0, 0)
                        Board(3, 0) = "r"c
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
                MaterialCount(1) -= ReturnPieceValue(CapturedPiece)
                ZobristValue = ZobristValue Xor ZobristHashTable((Asc(CapturedPiece) + 1) Mod 11, 1, NewCoorX, NewCoorY)
            End If
            HalfMoveSize = 0 'Capture Move - reset Half-Move count.
        End If
        Board(NewCoorX, NewCoorY) = TempPiece
        Board(OldCoorX, OldCoorY) = " "c
        'Changes the player to move on the Zobrist Key.
        ZobristValue = ZobristValue Xor HashConstants(0)
    End Sub

    'Subroutine that makes, or un-makes, a Null Move on the board, for use by Null-Move Pruning (effectively changing the Zobrist Hash Key, for use by the Transposition Table).
    Private Sub ActNullMove(ByRef Board(,) As Char, ByVal EnPassant As Int16, ByRef ZobristValue As UInt64)
        'Removes EnPassant Privileges from the hash value.
        If EnPassant <> 0 Then ZobristValue = ZobristValue Xor ZobristHashTable(2, 0, (EnPassant And 56) >> 3, EnPassant And 7)
        'Changes the player to move on the Zobrist Key.
        ZobristValue = ZobristValue Xor HashConstants(0)
    End Sub





    'This function contains my NegaMax algorithm (ie: the main search algorithm). Current Optimisations & Improvements:
    '• Alpha-Beta Pruning.
    '• Quiescence.
    '• Transposition Table Pruning.
    '• Repetition Detection.
    '• Transposition Table Move Lookup (+ Prioritising).
    '• Search Extensions (in check).
    '• Null Move Pruning.
    '• Delta Pruning.
    '• Late Move Reductions.
    '• Internal Iterative Reductions.
    '• Killer Moves.
    Private Function NegaMax(ByRef Board(,) As Char, ByVal depth As Integer, ByVal NumDepthExt As Integer, ByVal isWhite As Boolean, ByVal MeCanCastle As CanCastle, ByVal EnemyCanCastle As CanCastle, ByVal MeKPos As Int16, ByVal EnemyKPos As Int16, ByVal EnPassant As Int16, ByVal MaterialCount() As Integer, ByVal ZobristValue As UInt64, ByVal HalfMoveSize As UInt16, ByVal Alpha As Int16, ByVal Beta As Int16, ByVal CanTakeNullMove As Boolean) As Int16
        If ABORT Then Return 0

        'Checks for darws via the 50-move rule. Note that I'm a little worried about this... Surely this will corrupt the Transposition Table entry of
        'this node's immediate parent, in case we reach here from a different branch? Apparently all the top engines don't care about this :O.
        If HalfMoveSize >= 100 AndAlso depth > 0 Then
            'We have hit the 50-move rule - this can only be overruled if we are in checkmate, so assuming that is not the case, we can safely return 0.
            Dim FiftyMoveCheck As UInt16
            FixTFTable(Board, isWhite, TFTableStorageArray(DepthFromRoot).Table, MeKPos, FiftyMoveCheck, EnPassant)
            If FiftyMoveCheck >= 128 Then
                Dim FiftyMoveMoves() As UInt16 = CreateMoves(Board, isWhite, TFTableStorageArray(DepthFromRoot).Table, EnemyKPos, FiftyMoveCheck, MeCanCastle, EnPassant, False, DepthFromRoot, 0)
                If FiftyMoveMoves IsNot Nothing Then
                    Dim TempFiftyMoveCheck As UInt16
                    For n = 0 To FiftyMoveMoves.Length - 1
                        If (FiftyMoveMoves(n) And 4032) >> 6 = MeKPos Then
                            'There is a king move that can bring us out of check.
                            Return 0
                        Else
                            'Runs move through the DoesMoveResolveCheck algorithm.
                            TempFiftyMoveCheck = FiftyMoveCheck
                            'Move has resolved check - we cannot be in checkmate :D.
                            If DoesMoveResolveCheck(Board, isWhite, FiftyMoveMoves(n), TempFiftyMoveCheck) Then Return 0
                        End If
                    Next
                End If
            Else
                'We have confirmed that we are not in check, and so can classify this position as a draw.
                Return 0
            End If
            'We must be in checkmate! Return the checkmate score.
            Return -30000S + CShort(DepthFromRoot)
        End If


        Dim TempTTEntry As TTEntry
        'Hashes Zobrist key to find the location of the Entry in the Table.
        Dim EntryInTT As Integer = CInt(ZobristValue >> GlobalConstants.TranspositionTableSize)
        Dim OldFlag As Byte
        Dim OldDepth As SByte
        Dim ReplaceTTNode As Boolean

        'Searches for position in TranspositionTable.
        If TranspositionTable(EntryInTT).Key = ZobristValue Then TempTTEntry = TranspositionTable(EntryInTT)

        If TempTTEntry.Key > 0 Then

            'Code for exiting the NegaMax branch early if it can successfully retrieve a correct score from that entry in TT.
            'As the TT nodes represents the ply to checkmate from *that* position, we must scale this to our current position by adding DepthFromRoot.
            Dim CorrectedTTScore As Int16
            If Math.Abs(TempTTEntry.Score) >= 29500 Then
                'Is a mate score - calculates the checkmate score from this node.
                CorrectedTTScore = TempTTEntry.Score + CShort(DepthFromRoot) * If(TempTTEntry.Score > 0, -1S, 1S)
            Else
                CorrectedTTScore = TempTTEntry.Score
            End If

            If TempTTEntry.Flag = 5 Then 'Position has already been calculated as an end-state - return this position.
                If (PlayerTurn AndAlso CorrectedTTScore >= 29500) OrElse (Not PlayerTurn AndAlso CorrectedTTScore <= -29500) Then WinsFound += 1UL
                TranspositionsFound += 1UL
                Return CorrectedTTScore
            End If
            'Note that the depth of the stored position must be greater than that of the current position, as otherwise the
            'evalutation of the stored entry will be less accurate than performing the search.
            If TempTTEntry.Depth >= depth Then
                Select Case TempTTEntry.Flag
                    Case 2 'Upper Bound.
                        If CorrectedTTScore <= Alpha Then TranspositionsFound += 1UL : Return CorrectedTTScore
                    Case 1 'Lower Bound.
                        If CorrectedTTScore >= Beta Then TranspositionsFound += 1UL : Return CorrectedTTScore
                    Case 4
                        TranspositionsFound += 1UL
                        Return 0
                    Case 0
                        TranspositionsFound += 1UL
                        Return CorrectedTTScore
                End Select
            End If

            If depth > 0 AndAlso SearchSettings.UseTranspositionTable Then
                OldFlag = TempTTEntry.Flag
                TempTTEntry.Flag = 4 'flags that the position is currently being searched on - if child nodes also detect this position,
                'then we return a draw (three-fold repetition).
                OldDepth = TempTTEntry.Depth
                TempTTEntry.Depth = CSByte(depth)
                TranspositionTable(EntryInTT) = TempTTEntry 'Replaces entry. Note that this is using an always-replace scheme, that I've heard goes
                'against the logic of iterative deepening. Might be worth trialing out a depth-preferred replacement scheme..?
                ReplaceTTNode = True
            Else
                'Make sure recommended move isn't passed into the move generation code (as move may not be a capture move).
                TempTTEntry.BestMove = 0
            End If

        ElseIf depth > 0 AndAlso (TranspositionTable(EntryInTT).Depth < depth OrElse TTGeneration - TranspositionTable(EntryInTT).Generation >= SearchSettings.TimeToLive) AndAlso SearchSettings.UseTranspositionTable Then
            OldFlag = 4
            'Creates a new TTEntry for the current position, as it could not be found in the Transposition Table.
            TempTTEntry.Generation = CShort(TTGeneration)
            TempTTEntry.Key = ZobristValue
            TempTTEntry.Flag = 4
            TempTTEntry.Depth = CSByte(depth)
            TranspositionTable(EntryInTT) = TempTTEntry
            ReplaceTTNode = True
        End If


        Dim CurrentMove, BestMove, StandPat As Int16
        Dim PlayerInCheck As UInt16
        Dim DepthExt, NoLegalMoves As Integer
        Dim NeedFullSearch As Boolean
        'Creates and forms the TFTable for the player to move. This subroutine will also flag for Minor & Major piece in the position.
        Dim NoPieceInPos As Boolean = True
        FixTFTable(Board, isWhite, TFTableStorageArray(DepthFromRoot).Table, MeKPos, PlayerInCheck, EnPassant, NoPieceInPos)

        If Not (depth > 0 OrElse PlayerInCheck >= 128) Then 'Quiescence mode activated.
            'Evaluation of board is the current move to beat.
            StandPat = Evaluate(Board, isWhite, MaterialCount, MeKPos, EnemyKPos)
            Alpha = Math.Max(Alpha, StandPat)
            If Beta <= Alpha Then Return StandPat 'Alpha-Beta Pruning.
            BestMove = StandPat
        Else
            'Null Move Pruning - if we are neiter in check, nor in the late endgame (to avoid zugzwang), we force the current player to pass their turn to the
            'opponent, and search this new positon at a reduced depth. If this new position is *not* good enough to cause a Alpha-Beta cutoff (note that we
            'only pass in Beta) then we are clearly _much_ better than the opponent. Treat this as an Alpha-Beta cutoff.
            If depth >= SearchSettings.NullMoveRValue + 1 AndAlso PlayerInCheck < 128 AndAlso CanTakeNullMove AndAlso Not NoPieceInPos Then
                'If we are not in a good position ourselves, then skipping a turn will almost certainly make things worse. Therefore only perform a null
                'move if our static evaluation is at least good enough.
                StandPat = Evaluate(Board, isWhite, MaterialCount, MeKPos, EnemyKPos)
                If StandPat >= Beta Then
                    ActNullMove(Board, EnPassant, ZobristValue)
                    DepthFromRoot += 1
                    'Turn CanTakeNullMove off for the next move, to prevent infinite null moves.
                    BestMove = -NegaMax(Board, depth - SearchSettings.NullMoveRValue - 1, NumDepthExt, Not isWhite, EnemyCanCastle, MeCanCastle, EnemyKPos, MeKPos, 0S, MaterialCount, ZobristValue, HalfMoveSize, -Beta, -Beta + 1S, False)
                    DepthFromRoot -= 1
                    'Undos the null move, which is just equivalent to taking another null move (via the properties of xor in Zobrist Hashing).
                    ActNullMove(Board, EnPassant, ZobristValue)

                    If ABORT Then
                        'Resets the current position's entry in the Transposition Table (to prevent leftover "4"
                        'entries from affecting future searches), then exits search immediately.
                        If ReplaceTTNode Then
                            If OldFlag = 4 Then TranspositionTable(EntryInTT) = New TTEntry Else TranspositionTable(EntryInTT).Flag = OldFlag : TranspositionTable(EntryInTT).Depth = OldDepth
                        End If
                        Return 0
                    End If
                    If Beta <= BestMove Then
                        If ReplaceTTNode Then
                            'Store position & its detail in the Transposition Table.
                            TempTTEntry.Score = BestMove
                            'Corrects score for checkmating patterns.
                            If Math.Abs(BestMove) >= 29500 Then TempTTEntry.Score += CShort(If(BestMove > 0, DepthFromRoot, -DepthFromRoot))
                            TempTTEntry.Flag = 1 'Caused an Alpha-beta cutoff, so the move may be even better than its score suggests - mark as lower bound.
                            TranspositionTable(EntryInTT) = TempTTEntry 'Replaces entry.
                        End If
                        Return BestMove 'Alpha-Beta Pruning.
                    End If
                End If
            End If

            BestMove = -InfScore
            'Search Extensions - if we are put into check, we might want to explore deeper, to see if it leads anywhere...
            'TODO: Extend search for pawns pushing to the 7th rank, or if there is only 1 move available?
            If Not SearchSettings.StableSearch AndAlso PlayerInCheck >= 128 AndAlso NumDepthExt < SearchSettings.MaxDepthExt Then DepthExt = 1
        End If

        'Assumes Flag to be an Upper bound, unless proven otherwise.
        TempTTEntry.Flag = 2
        'Creates the pseudo-legal moves for the chosen player. If Quiescence mode is activated then use capture moves only.
        Dim PieceMoves() As UInt16 = CreateMoves(Board, isWhite, TFTableStorageArray(DepthFromRoot).Table, EnemyKPos, PlayerInCheck, MeCanCastle, EnPassant, Not (depth > 0 OrElse PlayerInCheck >= 128), DepthFromRoot, TempTTEntry.BestMove)

        If PieceMoves IsNot Nothing Then 'If any move exists...
            'Creates temp variables.
            Dim TempBoard(7, 7) As Char
            Dim TempMaterialCount(1) As Integer
            Dim TempMeKPos As Int16
            Dim TempMeCanCastle As New CanCastle
            Dim TempPlayerInCheck As UInt16
            Dim TempEnPassant As Int16
            Dim TempZobristValue As UInt64
            Dim TempHalfMoveSize As UInt16

            For n = 0 To PieceMoves.Length - 1 'for each move...
                'If the user is in check, eliminate the moves that do not escape check. King moves are not considered
                'as the player's TFTable will ensure that all king moves are legal.
                If PlayerInCheck >= 128 AndAlso (PieceMoves(n) And 4032) >> 6 <> MeKPos Then
                    'Runs move through the DoesMoveResolveCheck algorithm.
                    TempPlayerInCheck = PlayerInCheck
                    If DoesMoveResolveCheck(Board, isWhite, PieceMoves(n), TempPlayerInCheck) Then
                        'Move has resolved check.
                        TempPlayerInCheck = 0
                    End If
                Else
                    TempPlayerInCheck = 0
                End If
                If TempPlayerInCheck = 0 Then 'Therefore, is a legal move.
                    NoLegalMoves += 1

                    'Delta-Pruning in the Quiescence Search (capture moves).
                    If Not (depth > 0 OrElse PlayerInCheck >= 128) AndAlso Math.Min(MaterialCount(0), MaterialCount(1)) >= 600 Then
                        'We are not in the *late* endgame phase - intiate Delta-Pruning, as otherwise we might ignore ways to trade into a drawn endgame (eg: KN vs K).
                        Dim CapturedPieceValue As Integer
                        'Calculate the value of the piece we are capturing. Note that en-passant captures don't flag as 'capture moves', so we must manually add the value of the pawn.
                        CapturedPieceValue = If(PieceMoves(n) > 32768, ReturnPieceValue(Board((PieceMoves(n) And 56US) >> 3, PieceMoves(n) And 7US)), GlobalConstants.PieceWeight.Pawn)
                        If (PieceMoves(n) And 28672US) > 0US Then
                            'Check for flags of pawn promotions. If this is the case, the promotion to a queen / knight _may_ be enough to overtake alpha.
                            If (PieceMoves(n) And 28672US) = 4096US Then 'Queen Promotion.
                                CapturedPieceValue += GlobalConstants.PieceWeight.Queen
                            ElseIf (PieceMoves(n) And 28672US) = 28672US Then 'Knight Promotion.
                                CapturedPieceValue += GlobalConstants.PieceWeight.Knight
                            End If
                        End If
                        'If the below IF passes, we assume it is impossible to exceed Alpha from the given position - reject this move.
                        If StandPat < Alpha - CapturedPieceValue - 200 Then Continue For
                    End If

                    'Copies board info to temp variables.
                    Array.Copy(Board, TempBoard, 64)
                    TempMaterialCount(0) = MaterialCount(0)
                    TempMaterialCount(1) = MaterialCount(1)
                    TempMeKPos = MeKPos
                    TempMeCanCastle.CopyFrom(MeCanCastle)
                    TempEnPassant = EnPassant
                    TempZobristValue = ZobristValue
                    TempHalfMoveSize = HalfMoveSize
                    'Makes move on temp board, then calls NegaMax for this new position.
                    MakeMove(TempBoard, PieceMoves(n), TempMeCanCastle, TempMeKPos, TempMaterialCount, TempEnPassant, TempZobristValue, TempHalfMoveSize)
                    DepthFromRoot += 1
                    TotalPositionsSearched += 1UL

                    If TempMaterialCount(0) + TempMaterialCount(1) = 0 Then
                        'Enforce draw by repetition.
                        HighestQuiescenceDepth = Math.Max(HighestQuiescenceDepth, DepthFromRoot)
                        CurrentMove = 0
                    ElseIf Not SearchSettings.UseQuiescence AndAlso depth = 1 Then
                        'We have reached a leaf position - return the evaluation for this position.
                        CurrentMove = Evaluate(TempBoard, isWhite, TempMaterialCount, TempMeKPos, EnemyKPos) 'Evaluate position for opponent.
                    Else 'No leaf node or drawn position (or are using Quiescence) - put position through NegaMax recursively.
                        HighestQuiescenceDepth = Math.Max(HighestQuiescenceDepth, DepthFromRoot)
                        'Late Move Reducitons & Internal Iterative Reductions - search everything but the first n moves at a reduced depth. If no hash move could be found, then the position
                        'is deemed 'more quiet', and so more moves are searched at a reduced depth.
                        'We disable this feature if there are no search extensions, as these are put into place when a position is deemed 'crutial' enough for a full search.
                        NeedFullSearch = True
                        If Not SearchSettings.StableSearch AndAlso depth >= 3 AndAlso DepthExt = 0 AndAlso NoLegalMoves + If(TempTTEntry.BestMove = 0, 1, 0) >= SearchSettings.MoveReductionThreshold Then
                            'We use a tightened Alpha-Beta window here, so that if any fail-high nodes then are detected and sent back up the tree instantly.
                            CurrentMove = -NegaMax(TempBoard, depth - 2, NumDepthExt, Not isWhite, EnemyCanCastle, TempMeCanCastle, EnemyKPos, TempMeKPos, TempEnPassant, TempMaterialCount, TempZobristValue, TempHalfMoveSize, -Alpha - 1S, -Alpha, True)
                            If CurrentMove > Alpha Then
                                NoRepeatedSearches += 1
                            Else
                                NeedFullSearch = False
                            End If
                        End If
                        If NeedFullSearch Then CurrentMove = -NegaMax(TempBoard, depth + DepthExt - 1, NumDepthExt + DepthExt, Not isWhite, EnemyCanCastle, TempMeCanCastle, EnemyKPos, TempMeKPos, TempEnPassant, TempMaterialCount, TempZobristValue, TempHalfMoveSize, -Beta, -Alpha, True)
                    End If

                    DepthFromRoot -= 1

                    'For debugging...
                    'If TERMINATED Then
                    '    OutputBoardToConsole(Board)
                    '    OutputBitMoveToConsole(PieceMoves(n))
                    '    Return 0
                    'End If

                    If ABORT Then
                        'Resets the current position's entry in the Transposition Table (to prevent leftover "4"
                        'entries from affecting future searches), then exits search immediately.
                        If ReplaceTTNode Then
                            If OldFlag = 4 Then TranspositionTable(EntryInTT) = New TTEntry Else TranspositionTable(EntryInTT).Flag = OldFlag : TranspositionTable(EntryInTT).Depth = OldDepth
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
                            If BestMove > Alpha Then TempTTEntry.Flag = 0
                        End If

                        Alpha = Math.Max(Alpha, BestMove) 'Alpha = best move found for player.
                        If Beta <= Alpha Then 'Move was too strong for player; opponent will not choose this branch.
                            If depth > 0 AndAlso PieceMoves(n) < 32768US AndAlso KillerMoves(DepthFromRoot, 0) <> PieceMoves(n) Then
                                'The pruned move is not a capture move - add move to KillerMoves(), in the hope that the move
                                'is also possible in sibling positions. If this move is detected, it is searched earlier.
                                KillerMoves(DepthFromRoot, 1) = KillerMoves(DepthFromRoot, 0)
                                KillerMoves(DepthFromRoot, 0) = PieceMoves(n)
                            End If
                            If ReplaceTTNode Then
                                'Store position & its detail in the Transposition Table.
                                TempTTEntry.Score = BestMove
                                'Corrects score for checkmating patterns.
                                If Math.Abs(BestMove) >= 29500 Then TempTTEntry.Score += CShort(If(BestMove > 0, DepthFromRoot, -DepthFromRoot))
                                TempTTEntry.Flag = 1 'Caused an Alpha-beta cutoff, so the move may be even better than its score suggests - mark as lower bound.
                                TranspositionTable(EntryInTT) = TempTTEntry 'Replaces entry.
                            End If

                            Return BestMove 'Alpha-Beta Pruning - return best move.
                        End If
                    End If
                End If
            Next
        End If

        If Math.Abs(BestMove) >= 29500 Then
            If NoLegalMoves = 0 Then
                'No legal move found for the player.
                If PlayerInCheck >= 128 Then
                    'Checkmate!
                    BestMove = -30000S + CShort(DepthFromRoot)
                    If ReplaceTTNode Then TempTTEntry.Score = -30000
                    If PlayerTurn <> isWhite Then WinsFound += 1UL
                Else 'Stalemate: return 0.
                    BestMove = 0
                    If ReplaceTTNode Then TempTTEntry.Score = 0
                End If
                TempTTEntry.Flag = 5 'Represents an end-state in the Transposition Table.
            ElseIf ReplaceTTNode Then
                'Position leads to checkmate. Store this in the Transposition Table, with score referring to how many moves the mate is from *this* position.
                'that way, when we encounter this position again, we can add DepthFromRoot to get the correct checkmate score.
                TempTTEntry.Score = BestMove + CShort(If(BestMove > 0, DepthFromRoot, -DepthFromRoot))
            End If
        ElseIf ReplaceTTNode Then
            TempTTEntry.Score = BestMove 'Add ReplaceTTNode checks?
        End If

        'Search completed without Alpha-beta cutoff - store position in Transposition Table.
        If ReplaceTTNode Then TranspositionTable(EntryInTT) = TempTTEntry

        'Return the best move's score found this iteration.
        Return BestMove
    End Function



    'This algorithm is used to condense a board position into an evaluation score, used to determine best moves.
    'We take into account the difference in material between the two sides, along with a heuristic to help
    'the AI find checkmated in simple endgame positions.
    Private Function Evaluate(ByRef Board(,) As Char, ByVal isWhite As Boolean, ByVal MaterialCount() As Integer, ByVal MeKPos As Int16, ByVal EnemyKPos As Int16) As Int16
        'Overload for the Evaluation function, for use in NegaMax.
        If isWhite Then
            Return Evaluate(Board, MaterialCount, MeKPos, EnemyKPos)
        Else
            Return -Evaluate(Board, MaterialCount, EnemyKPos, MeKPos) '- as '-1' is good for black, but bad for white.
        End If
    End Function
    Private Function Evaluate(ByRef Board(,) As Char, ByVal MaterialCount() As Integer, ByVal WKPos As Int16, ByVal BKPos As Int16) As Int16
        'Finds difference in material between both sides.
        Dim Score As Int32 = MaterialCount(0)
        Score -= MaterialCount(1)

        If SearchSettings.UsePieceHeatMaps Then
            'If the opponent has a lot of material, then score positions where the player's king is safe as being more advantageous.
            If MaterialCount(1) >= 1500 Then Score += PieceHeatMap(9, WKPos And 7, (WKPos And 56) >> 3)
            If MaterialCount(0) >= 1500 Then Score -= PieceHeatMap(9, 7 - (BKPos And 7), (BKPos And 56) >> 3)

            'For each piece on the board, calculate how advantageously it is placed (using the PieceHeatSquares). Add this to the score.
            For y = 0 To 7
                For x = 0 To 7
                    If Not (Board(x, y) = " "c OrElse UCase(Board(x, y)) = "K"c) Then
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
            Score -= EndgameEvalLookupTable(WKPos, BKPos, MaterialCount(0) \ 100)
        End If
        If MaterialCount(1) <= 1200 Then
            'Black has little material left.
            Score += EndgameEvalLookupTable(BKPos, WKPos, MaterialCount(1) \ 100)
        End If

        Return CShort(Score)
    End Function

    'Catch ex As Exception
    '    Console.WriteLine("oops")
    '    OutputBoardToConsole(Board)
    '    TERMINATED = True
    'End Try

End Class
