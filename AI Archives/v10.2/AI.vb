Option Strict On

'This class contains my Chess AI, which is built using the NegaMax algorithm with Alpha-Beta Pruning.
'This class is modular from my Chess class - being constructed only from the FEN position, and only returning a Move (see the structure below).
'Some other interacting is done, however, such as allowing the AI to be remotely aborted.
Imports System.ComponentModel
Imports System.Diagnostics.Eventing.Reader
Imports System.DirectoryServices.ActiveDirectory
Imports System.Formats.Asn1.AsnWriter
Imports System.Globalization
Imports System.IO
Imports System.Numerics
Imports System.Reflection
Imports System.Reflection.Metadata.Ecma335
Imports System.Runtime
Imports System.Runtime.CompilerServices
Imports System.Runtime.Intrinsics
Imports System.Text
Imports System.Text.RegularExpressions
Imports System.Windows.Forms.AxHost
Imports System.Windows.Forms.VisualStyles
Imports System.Xml
Imports Chess_AI.GlobalConstants

Partial Public Class AI 'i shall thy the Alfie Alphafish (bit optimistic, I know).
    Inherits CoreMethods

    Private HasBeenInstantiated As Boolean 'The AI will not perform methods if it has not been fully instantiated with a FEN.
    'Below are the details that the AI requires for a search. Please see their counterparts in the Chess form for their info.

    Private PrimaryState As BoardState
    Private PrimarySearchVars As NegaMaxSearchTools
    Private PrimaryMeKPos, PrimaryEnemyKPos As UInt16
    Private NegaMaxBoardStates(127) As BoardState

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
    'XY = Start X & Y Coordinate (0-63)  -  Mask = 4032, Shift of 6.
    'xy = End X & Y Coordinate (0-63)  -  Mask = 63, Shift of 0.
    Private MoveBuffer(128 * GlobalConstants.MaxTurnLegalMoves - 1) As UInt16
    Public NumCapturesThreatsInBasePos As Integer
    Private PlayerTurn As Boolean


    Private PrimaryBoard(7, 7), LegacyTFTable(7, 7) As Char


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
    Private InAspirationBreak As Boolean 'Denotes whether the AI's search has just broken an Aspiration Window, and is currently in the process of re-searching at a full window.
    Private MasterDepth, DepthFromRoot As Integer 'The Surface Depth of the search, as set by the user, and the current depth (from the base position) at a current point in the search.
    Private Const InfScore As Int16 = Int16.MaxValue


    Private KillerMoves(255) As UInt16 'Array containing Killer Moves: non-capture moves which caused an alpha-beta cut off.
    'If we detect killer moves in sibling positions (ie: positions of the same depth), we search the Killer Move(s) first.

    'Arrays that the CreateMoves function uses (delared before to save processing time in the search process).
    Private TempCaptureMoves(99) As UInt16 'Min size = 19
    Private TempCaptureMoveScores(99) As UInt16
    Private PawnPromotionMoves(15) As UInt16 'Min size = 7
    Private GoodMoves(99) As UInt16 'Min size = 49
    Private OtherMoves(99) As UInt16 'Min size = 99
    Private BadMoves(74) As UInt16 'Min size = 49
    Private TerribleMoves(74) As UInt16 'Min size = 49


    'Structures for the TranspositionTable - a large table containing the basic details of a position - stored via their Zobrist Hash.
    'Stored as a hashed array so that the overall size of the Table can be reduced (would need to be 2^64 elements large otherwise).
    'This structure uses the 'Greatest Depth' replacement scheme, where each entry is timestamped for 3 moves.
    'Table will contain 2^n entries, where n is the value set in TranspositionTableSize (in the brackets).
    Private TranspositionTable((1 << (64 - GlobalConstants.TranspositionTableSize)) - 1) As TTEntry
    Private TTIsEmpty As Boolean
    Private TTGeneration As Byte 'Represents the current move count of the position, so that we can index when TTEntries are made.


    Private Structure TTEntry
        'Dim isPopulated As Boolean
        Dim Key As UInt64 'Zobrist Key of position - used to pinpoint the correct board.
        Dim Generation As Byte 'Represents the move at which the entry was created. If the current move is much higher than this number, we call this entry 'dead'.
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
        PrecomputeAllPieceMaps()
        PopulatePieceHeatSquares()
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
            OutputStatsToFile()
            Console.ForegroundColor = ConsoleColor.White
        End Try
    End Sub

    'Subroutine which converts a user's input FEN into all the details needed to conduct a NegaMax search.
    Public Sub Reconfigure(ByVal FEN As String, Optional ByVal ResetTT As Boolean = False)
        Dim NoLegalMoves As Integer
        'Converts the user's FEN into a board position, then resets checking rules.
        Try
            PrimaryBoard = FENConverter(FEN, PrimaryState.WhiteCanCastle, PrimaryState.BlackCanCastle, PrimaryMeKPos, PrimaryEnemyKPos, PrimaryState.EnPassant, PlayerTurn)
            ConvertBoardtoBitboards(PrimaryBoard, PrimaryState, True)
        Catch ex As Exception
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Unable to Calibrate AI from given FEN. Please try again...")
            Console.ForegroundColor = ConsoleColor.White
            HasBeenInstantiated = False
            Exit Sub
        End Try
        'Finds the material count of the board.
        Dim MaterialCount = CountMaterial(PrimaryBoard) : PrimaryState.MaterialCountWhite = MaterialCount(0) : PrimaryState.MaterialCountBlack = MaterialCount(1)
        'Calculates the PHM values for the base position.
        If SearchSettings.UsePieceHeatMaps Then Dim PHMValues = GetPHMEval(PrimaryState, PrimaryMeKPos, PrimaryEnemyKPos, CalculateEndgameValues:=False) : PrimaryState.PHMValueWhite = PHMValues.White : PrimaryState.PHMValueBlack = PHMValues.Black

        If Not PlayerTurn Then
            'Swaps the Primary & Enemy King privileges. This is because, for the AI, all variables are in context of which player the AI
            'is controlling, so 'me' and 'enemy' can refer to different colours depending on the board position.
            Dim TempKPos As UInt16 = PrimaryMeKPos
            PrimaryMeKPos = PrimaryEnemyKPos
            PrimaryEnemyKPos = TempKPos
        End If
        'Creates the TFTable for the white pieces (legacy algorithms still use it a lot), then creates the legal moves.
        PrimarySearchVars = CalibrateForMoveGeneration(PrimaryState, PrimaryMeKPos, PrimaryEnemyKPos, PlayerTurn)
        'Creates the legacy TFTable.
        For y As Int16 = 0 To 7
            For x As Int16 = 0 To 7
                Dim Square As Int16 = Flatten2DBoardIndex(x, y)
                Dim SquareMap As UInt64 = 1UL << Square
                If (PrimarySearchVars.PinInfoDiag And SquareMap) <> 0UL Then
                    'Calculates which way the piece is pinned (1 or 3), using the friendly king position.
                    Dim dx As Integer = (PrimaryMeKPos Mod 8) - (Square Mod 8)
                    Dim dy As Integer = (PrimaryMeKPos \ 8) - (Square \ 8)
                    LegacyTFTable(x, y) = If(Math.Sign(dx) = Math.Sign(dy), "3"c, "1"c)
                ElseIf (PrimarySearchVars.PinInfoStraight And SquareMap) <> 0UL Then
                    'Calculates which way the piece is pinned (0 or 2), using the friendly king position.
                    LegacyTFTable(x, y) = If((PrimaryMeKPos \ 8) = (Square \ 8), "0"c, "2"c)
                ElseIf (PrimarySearchVars.TFTable And SquareMap) <> 0UL Then
                    LegacyTFTable(x, y) = "T"c
                Else
                    LegacyTFTable(x, y) = "F"c
                End If
            Next
        Next
        NoLegalMoves = CreateMoves(PrimaryState, 0, PlayerTurn, PrimarySearchVars, PrimaryMeKPos, PrimaryEnemyKPos, True, 0, 0US)
        'Creates the Zobrist Value for the position.
        PrimaryState.ZobristValue = ZobristHashPosition(PrimaryBoard, PlayerTurn, PrimaryState.WhiteCanCastle, PrimaryState.BlackCanCastle, PrimaryState.EnPassant)

        If NoLegalMoves > 0 Then
            'Copy all the legal moves to BasePieceMoves.
            ReDim BasePieceMoves(NoLegalMoves - 1)
            Array.Copy(MoveBuffer, BasePieceMoves, NoLegalMoves)
        Else
            BasePieceMoves = Nothing
        End If
        SortMovesThorough(BasePieceMoves, PrimaryState, PlayerTurn, PrimaryMeKPos, PrimaryEnemyKPos, PrimarySearchVars.TFTable)

        'If BasePieceMoves IsNot Nothing Then
        '    For Each Move In BasePieceMoves
        '        OutputBitMoveToConsole(Move)
        '    Next
        'End If

        'Resets KillerMoves.
        For n As Int16 = 0 To 255
            KillerMoves(n) = 0
        Next
        If ResetTT Then ResetTranspositionTable() Else IncreaseTTGeneration()
        PrimaryState.HalfMoveSize = 0

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
        If TTGeneration >= Byte.MaxValue Then
            'Move Limit has been matched - must reset TT :(
            Console.ForegroundColor = ConsoleColor.Red
            Console.WriteLine("Transposition Table Generation Limit Reached: Resetting...")
            Console.ResetColor()
            If Not TTIsEmpty Then
                For n = 0 To TranspositionTable.Length - 1
                    If TranspositionTable(n).Flag <> 4 And TranspositionTable(n).Generation <> Byte.MaxValue Then
                        TranspositionTable(n).Generation = 0
                    End If
                Next
            End If
            TTGeneration = 0
        Else
            TTGeneration = CByte(TTGeneration + 1)
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
            PrimaryState.HalfMoveSize = HalfMoveSize
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
            PrimaryState.HalfMoveSize = 0
        End If
    End Sub



    'Subroutine which copies a parameter's settings to the AI's SearchSettings.
    Public Sub ConfigureSettings(ByVal UserSearchSettings As AISearchSettings, ByVal ResetTTIfSettingsChanged As Boolean)
        If SearchSettings.CopyFrom(UserSearchSettings) AndAlso ResetTTIfSettingsChanged Then
            Console.ForegroundColor = ConsoleColor.Red
            Console.WriteLine("Core AI Settings Changed. Resetting Transposition Table...")
            Console.ForegroundColor = ConsoleColor.White
            ResetTranspositionTable()
            'In these cases, changing the AI settings means that the move ordering system as determined via SortMovesThourough is not applicable anymore. Reset.
            SortMovesThorough(BasePieceMoves, PrimaryState, PlayerTurn, PrimaryMeKPos, PrimaryEnemyKPos, PrimarySearchVars.TFTable)
            'Calculates the PHM values for the base position.
            If SearchSettings.UsePieceHeatMaps Then Dim PHMValues = GetPHMEval(PrimaryState, If(PlayerTurn, PrimaryMeKPos, PrimaryEnemyKPos), If(PlayerTurn, PrimaryEnemyKPos, PrimaryMeKPos), CalculateEndgameValues:=False) : PrimaryState.MaterialCountWhite = PHMValues.White : PrimaryState.MaterialCountBlack = PHMValues.Black
        End If
    End Sub

    'Function which retrieves all the legal moves in the base position, formatted as strings (so that the Chess system can
    'convert them to Moves more easily).
    Public Function GetLegalMoves() As String(,)
        If HasBeenInstantiated Then
            'Stored as a set of strings in the format 0="xy", 1="XY", xy = start coors, XY = end coors.
            Dim FormattedMoves(BasePieceMoves.Length - 1, 1) As String
            For n = 0 To BasePieceMoves.Length - 1
                Dim OldSquare As Integer = (BasePieceMoves(n) And 4032US) >> 6
                Dim NewSquare As Integer = BasePieceMoves(n) And 63US
                FormattedMoves(n, 0) = (OldSquare Mod 8).ToString() & (OldSquare \ 8).ToString()
                FormattedMoves(n, 1) = (NewSquare Mod 8).ToString() & (NewSquare \ 8).ToString()
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
        Return PrimaryState.ZobristValue
    End Function
    Public Function GetMaterialCount() As Integer()
        Return New Integer() {PrimaryState.MaterialCountWhite, PrimaryState.MaterialCountBlack}
    End Function

    Public Function GetLegacyTFTable() As Char(,)
        Return LegacyTFTable
    End Function
    Public Function IsInCheck() As Boolean
        Return PrimarySearchVars.CheckInfo <> 0US
    End Function
    Public Function GetCheckSquare() As Integer
        Return If(IsInCheck(), PrimarySearchVars.CheckInfo And 63US, -1)
    End Function

    Public Function IsInactiveKingInCheck() As Boolean
        If PlayerTurn Then
            If (PawnBlackAttackMap(PrimaryEnemyKPos) And PrimaryState.BitboardPawnWhite) <> 0UL Then Return True
            If (KnightMoveMap(PrimaryEnemyKPos) And PrimaryState.BitboardKnightWhite) <> 0UL Then Return True
            If (BishopMagicLookup(PrimaryEnemyKPos, PrimarySearchVars.OccupancyMask) And (PrimaryState.BitboardBishopWhite Or PrimaryState.BitboardQueenWhite)) <> 0UL Then Return True
            If (RookMagicLookup(PrimaryEnemyKPos, PrimarySearchVars.OccupancyMask) And (PrimaryState.BitboardRookWhite Or PrimaryState.BitboardQueenWhite)) <> 0UL Then Return True
        Else
            If (PawnWhiteAttackMap(PrimaryEnemyKPos) And PrimaryState.BitboardPawnBlack) <> 0UL Then Return True
            If (KnightMoveMap(PrimaryEnemyKPos) And PrimaryState.BitboardKnightBlack) <> 0UL Then Return True
            If (BishopMagicLookup(PrimaryEnemyKPos, PrimarySearchVars.OccupancyMask) And (PrimaryState.BitboardBishopBlack Or PrimaryState.BitboardQueenBlack)) <> 0UL Then Return True
            If (RookMagicLookup(PrimaryEnemyKPos, PrimarySearchVars.OccupancyMask) And (PrimaryState.BitboardRookBlack Or PrimaryState.BitboardQueenBlack)) <> 0UL Then Return True
        End If
        Return False
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

    Public Function CheckIfInAspirationBreak() As Boolean
        Return InAspirationBreak
    End Function

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
            InAspirationBreak = False
            TTIsEmpty = False
            TotalPositionsSearched = 0
            TranspositionsFound = 0
            WinsFound = 0
            HighestQuiescenceDepth = 1
            NoRepeatedSearches = 0

            'Creates temp variables.
            MasterDepth = Depth
            Dim TempMeKPos As UInt16

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
            Dim DynamicAWWidth As Int16 = SearchSettings.AspirationWidth
            Dim AspirationWindowCode As String = ""
            Dim AWFailCount(1) As Integer
            If PreviousBestScore = -InfScore OrElse DynamicAWWidth <= 0 OrElse Depth < 4 Then
                AspirationWindow = {-InfScore, InfScore}
            Else
                'Aspiration Windows - set Alpha & Beta via the previous search score. Note we only do this when the depth is high enough, so that we can get
                'a good, stable bound on the score.
                If Depth < 7 Then DynamicAWWidth += CShort(DynamicAWWidth * 0.5)
                DynamicAWWidth += CShort(Math.Min(Math.Abs(PreviousBestScore), 1250S) / 25) 'Change the Aspiraton Window Width Depending on the position: a higher score is more likely to be volatile, so increase the width accordingly.
                AspirationWindow = {PreviousBestScore - DynamicAWWidth, PreviousBestScore + DynamicAWWidth}
            End If

            While True
                Alpha = AspirationWindow(0)
                Beta = AspirationWindow(1)
                If SearchSettings.OutputToConsole Then
                    Console.ForegroundColor = ConsoleColor.DarkCyan
                    If Not SearchSettings.OutputMoveDebugInfo Then Console.Write("Searching at a Depth of " & MasterDepth & "...") : Console.SetCursorPosition(0, Console.CursorTop)
                End If

                For n = StartValue To EndValue Step StepValue 'for each move...

                    If SearchSettings.OutputToConsole AndAlso SearchSettings.OutputMoveDebugInfo Then
                        'Outputs the move that is currently being searched on, to the console.
                        ConvertBitMoveToMove(CurrentMove, BasePieceMoves(n))
                        Dim StringToOutput As String = "Searching at a Depth of " & MasterDepth & " - Processing Move: " & GetPGNFromMove(CurrentMove) & " ("
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
                    DepthFromRoot = 1
                    NegaMaxBoardStates(DepthFromRoot) = PrimaryState
                    TempMeKPos = PrimaryMeKPos
                    'Makes move on temp board, then calls NegaMax for this new position.
                    MakeMove(BasePieceMoves(n), NegaMaxBoardStates(DepthFromRoot), PlayerTurn, TempMeKPos)

                    If PrimaryState.MaterialCountWhite + PrimaryState.MaterialCountBlack = 0 Then
                        'Enforce draw by repetition.
                        TotalPositionsSearched += 1UL
                        CurrentScore = 0
                    ElseIf Depth = 1 AndAlso Not SearchSettings.UseQuiescence Then
                        'We have reached a leaf position - return the evaluation for this position.
                        TotalPositionsSearched += 1UL
                        CurrentScore = Evaluate(NegaMaxBoardStates(DepthFromRoot), PlayerTurn, TempMeKPos, PrimaryEnemyKPos)
                    Else
                        CurrentScore = -NegaMax(NegaMaxBoardStates(DepthFromRoot), Depth - 1, 0, Not PlayerTurn, PrimaryEnemyKPos, TempMeKPos, -Beta, -Alpha, True)
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
                        Exit While
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
                If BestMove.Score <= AspirationWindow(0) Then
                    'We failed low - widen the lower bound.
                    If Depth > 15 AndAlso AWFailCount(0) = 0 Then
                        'If the aspiration window breaks for a deep search such as this, it would be very expensive to re-search for a full window.
                        'Thus, run again on a larger window, to encapsulate minor to moderate positional changes.
                        AspirationWindow(0) -= 2S * DynamicAWWidth
                        AWFailCount(0) = 1
                    Else
                        AspirationWindow(0) = -InfScore
                    End If
                    AspirationWindowCode = "Low"
                ElseIf BestMove.Score >= AspirationWindow(1) Then
                    'We failed high - widen the upper bound.
                    If Depth > 15 AndAlso AWFailCount(1) = 0 Then
                        AspirationWindow(1) += 2S * DynamicAWWidth
                        AWFailCount(1) = 1
                    Else
                        AspirationWindow(1) = InfScore
                    End If
                    AspirationWindowCode = "High"

                    'If a move Caused an Alpha-Beta Cutoff, stores it as the first move to search (as it is clearly promising...)
                    Dim LocationInMoves As Integer = Array.IndexOf(BasePieceMoves, BestBitMove)
                    If LocationInMoves > 0 Then
                        If SearchSettings.ReturnBestMove Then
                            Array.Copy(BasePieceMoves, 0, BasePieceMoves, 1, LocationInMoves)
                            BasePieceMoves(0) = BestBitMove
                        Else
                            'Puts the best (worst) move to be the last move, as moves will be searched in reverse.
                            Array.Copy(BasePieceMoves, LocationInMoves + 1, BasePieceMoves, LocationInMoves, BasePieceMoves.Length - LocationInMoves - 1)
                            BasePieceMoves(BasePieceMoves.Length - 1) = BestBitMove
                        End If
                    End If

                Else
                    'Search produced a score within the Aspiration Window (ie: no Alpha-Beta cutoffs have occured), and so we assume the score is exact.
                    Exit While
                End If
                InAspirationBreak = True
                BestMove.Score = InfScore * If(SearchSettings.ReturnBestMove, -1, 1)

                If SearchSettings.OutputToConsole Then
                    Console.ForegroundColor = ConsoleColor.Magenta
                    Console.WriteLine($"Search at Depth {Depth} Broke the Aspiration-Window (Fail-{AspirationWindowCode}). Re-searching with a Larger Window...")
                    'Console.WriteLine("Move that Caused Cut-Off: " & MoveConverter(PrimaryBoard, BestMove, PlayerTurn, PrimaryMeKPos, PrimaryEnPassant, PrimaryTFTable))
                    'Console.WriteLine($"Alpha-Beta: ({Alpha},{Beta}). Window: ({AspirationWindow(0)},{AspirationWindow(1)})." & vbCrLf & $"Score = {BestMove.Score}, Previous Score = {PreviousBestScore}")
                End If
            End While
            InAspirationBreak = False


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
        If SearchSettings.UseQuiescence AndAlso Depth >= 4 AndAlso Math.Abs(PreviousBestMove.Score) <= 300 Then
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
            'TODO: redesign function to make better.
            Dim PGNMove As String = GetPGNFromMove(BestMove)
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
        Dim BestLine As String = GetPGNFromMove(TempMove)

        'Copies primary board attributes to their temporary counterparts.
        Dim isWhite As Boolean = PlayerTurn
        Dim TempState As BoardState = PrimaryState
        Dim TempWKPos, TempBKPos As UInt16
        If PlayerTurn Then
            TempWKPos = PrimaryMeKPos
            TempBKPos = PrimaryEnemyKPos
        Else
            TempWKPos = PrimaryEnemyKPos
            TempBKPos = PrimaryMeKPos
        End If
        Dim TempMaterialCount(1) As Integer
        TempMaterialCount(0) = Int32.MaxValue \ 2
        TempMaterialCount(1) = Int32.MaxValue \ 2


        Dim TempTTEntry As TTEntry
        Dim EntryInTT As Int32
        Dim MaxIterations As Byte = 20

        'Caps the maximum amount of moves being displaced to prevent the system from getting stuck in a loop.
        For i As Byte = 1 To MaxIterations
            'Makes the AI's calculated Best Move (from this position) on the temporary board.
            If isWhite Then
                MakeMove(BestMove, TempState, True, TempWKPos)
            Else
                MakeMove(BestMove, TempState, False, TempBKPos)
            End If


            'Hashes the current position, then finds the TranspositionTable entry containing that move.
            EntryInTT = CInt(TempState.ZobristValue >> GlobalConstants.TranspositionTableSize)
            If TranspositionTable(EntryInTT).Key = TempState.ZobristValue Then
                TempTTEntry = TranspositionTable(EntryInTT) 'Node is a match - assign to TempTTEntry.
            Else
                'No position was found from the previous move - must be end of sequence.
                Exit For
            End If

            If TempTTEntry.BestMove <> 0US Then
                'As this node in the TranspositionTable is involved in the set of best moves (as predicted by the AI), we
                'keep it alive by resetting its TimeToLive value. This ensures that the AI does not 'forget' its most vital
                'nodes, due to them expiring.
                TranspositionTable(EntryInTT).Generation = CByte(Math.Min(3 + TTGeneration, Byte.MaxValue))
                'We have stored a move in this position - retrieve this move, then add the PGN version of it to BestLine.
                BestMove = TempTTEntry.BestMove
                ConvertBitMoveToMove(TempMove, BestMove)
                isWhite = Not isWhite
                Dim WhitePieceMask As UInt64 = TempState.BitboardPawnWhite Or TempState.BitboardKnightWhite Or TempState.BitboardBishopWhite Or TempState.BitboardRookWhite Or TempState.BitboardQueenWhite
                Dim BlackPieceMask As UInt64 = TempState.BitboardPawnBlack Or TempState.BitboardKnightBlack Or TempState.BitboardBishopBlack Or TempState.BitboardRookBlack Or TempState.BitboardQueenBlack
                BestLine &= "," & MoveConverter(TempState, TempMove, isWhite, If(isWhite, TempWKPos, TempBKPos), WhitePieceMask Or BlackPieceMask, If(isWhite, BlackPieceMask, WhitePieceMask))
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
            Dim TempState As BoardState = PrimaryState
            'Copies primary assets to temporary assets, then makes the move on the temporary board.
            Try
                MakeMove(TempMove.BitMove, TempState, PlayerTurn, 0S)
                'Returns this new FEN. TODO: REMOVE CALL
                Return ConvertToFEN(ConvertBitboardstoBoard(TempState), TempState.WhiteCanCastle, TempState.BlackCanCastle, TempState.EnPassant, Not PlayerTurn)
            Catch ex As Exception
                'The move is not valid on this position - there must be some error.
                Console.ForegroundColor = ConsoleColor.DarkRed
                Console.WriteLine("Error: Move is invalid in this Position.")
                Console.ForegroundColor = ConsoleColor.White
            End Try
        End If
        Return Nothing
    End Function

    'Function that returns the move that the AI has cached in its TranspositionTable for this position (a good predictor).
    Public Function CheckIfMoveIsTTPrediction(ByVal TempMove As Move) As Boolean
        'Locates the Transposition Table entry corresponding to the base position.
        Dim BaseEntryInTT As Integer = If(SearchSettings.UseTranspositionTable AndAlso TranspositionTable(CInt(PrimaryState.ZobristValue >> GlobalConstants.TranspositionTableSize)).Key = PrimaryState.ZobristValue, CInt(PrimaryState.ZobristValue >> GlobalConstants.TranspositionTableSize), 0)
        If BaseEntryInTT = 0 Then Return False 'Could not find the move.
        Dim TTMove As Move = ConvertBitMoveToMove(TranspositionTable(BaseEntryInTT).BestMove)
        Return TempMove.CompareAgainstOtherMove(TTMove)
    End Function



    Public Function GetPGNFromMove(ByVal TempMove As Move) As String
        Return MoveConverter(PrimaryState, TempMove, PlayerTurn, PrimaryMeKPos, PrimarySearchVars.OccupancyMask, PrimarySearchVars.EnemyPieceMask)
    End Function
    Public Function GetMoveFromPGN(ByVal PGNMove As String) As Move
        Return ConvertToMove(PGNMove, PrimaryState, PlayerTurn, PrimaryMeKPos, PrimarySearchVars.OccupancyMask)
    End Function



    'Subroutine which outputs the lifetime stats of the AI to the AIStats file.
    Public Sub OutputStatsToFile()
        'Creates the folder structure if needed.
        Directory.CreateDirectory(Path.Combine(GlobalConstants.StartupPath, "Assets", "User"))
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
                Dim OldSquare As Integer = (BasePieceMoves(n) And 4032US) >> 6
                If Val(CoorX) = (OldSquare Mod 8) AndAlso Val(CoorY) = (OldSquare \ 8) Then
                    'Move found - add it to LegalMoves.
                    PieceMove = ((BasePieceMoves(n) And 63) Mod 8).ToString() & ((BasePieceMoves(n) And 63) \ 8).ToString()
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
                If PrimarySearchVars.CheckInfo >= 128 Then
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
                If SearchSettings.NodeSearchUseHashing Then TTIsEmpty = False
                NodeCount = 0
                EndPositionCount = 0
                PositionCollisions = 0

                Console.ForegroundColor = ConsoleColor.DarkCyan
                Console.Write("Performing Node Test at a Depth of " & Depth & "...")
                Console.SetCursorPosition(0, Console.CursorTop)

                GCSettings.LatencyMode = GCLatencyMode.SustainedLowLatency 'Relaxes garbage collection during the NegaMax search.
                NodeTestStopwatch.Start()
                NodeTest(PrimaryState, Depth, PlayerTurn, PrimaryMeKPos, PrimaryEnemyKPos)
                NodeTestStopwatch.Stop()
                GCSettings.LatencyMode = GCLatencyMode.Interactive

                If Not ABORT Then
                    'Outputs the statistics of the node test.
                    Console.ForegroundColor = ConsoleColor.DarkGreen
                    Console.WriteLine("Node Test at a Depth of " & Depth & " Completed.     ")
                    Console.ForegroundColor = ConsoleColor.White

                    Dim NodesPerSecond As Double = (NodeCount + EndPositionCount) / (1000 * NodeTestStopwatch.Elapsed.TotalMilliseconds)
                    Console.WriteLine((NodeCount + EndPositionCount).ToString("N0") & " Nodes Searched in " & NodeTestStopwatch.Elapsed.TotalMilliseconds.ToString("N0") & "ms (" & NodesPerSecond.ToString("N2") & "M Nodes/s).")

                    Console.Write("Total Position Count = " & EndPositionCount.ToString("N0"))
                    If SearchSettings.NodeSearchUseHashing Then Console.WriteLine(" (~" & (EndPositionCount - PositionCollisions).ToString("N0") & " Unique Positions).") Else Console.WriteLine(".")
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
    Private Sub NodeTest(ByRef State As BoardState, ByVal depth As Integer, ByVal isWhite As Boolean, ByVal MeKPos As UInt16, ByVal EnemyKPos As UInt16)
        If ABORT Then Exit Sub
        NodeCount += 1UL
        'Calibrates core objects, in preparation for CreateMoves.
        Dim SearchVars As NegaMaxSearchTools = CalibrateForMoveGeneration(State, MeKPos, EnemyKPos, isWhite)

        Dim MoveBufferStrafe As Integer = GlobalConstants.MaxTurnLegalMoves * depth
        Dim NoLegalMoves As Integer = NodeTestCreateMoves(State, MoveBufferStrafe, isWhite, MeKPos, SearchVars)
        If NoLegalMoves > 0 Then
            If depth = 1 AndAlso Not SearchSettings.NodeSearchUseHashing Then
                EndPositionCount += CULng(NoLegalMoves)
            Else
                'At least one pseudo-legal move exists.
                For n = MoveBufferStrafe To MoveBufferStrafe + NoLegalMoves - 1
                    'Copies the board position to its temporary counterparts.
                    NegaMaxBoardStates(depth) = State
                    Dim TempMeKPos As UInt16 = MeKPos

                    'Makes the current move onto the temporary board.
                    NodeTestMakeMove(MoveBuffer(n), NegaMaxBoardStates(depth), isWhite, TempMeKPos)

                    If depth = 1 Then
                        EndPositionCount += 1UL
                        'Leaf node reached - check if end position has already been encountered, using the Zobrist Hash of the position.
                        Dim EntryInTT As Integer = CInt(NegaMaxBoardStates(depth).ZobristValue >> GlobalConstants.TranspositionTableSize)
                        If TranspositionTable(EntryInTT).Key = NegaMaxBoardStates(depth).ZobristValue Then
                            PositionCollisions += 1UL
                        Else
                            TranspositionTable(EntryInTT).Key = NegaMaxBoardStates(depth).ZobristValue
                        End If
                    Else
                        'Recursively calls the Node Count on this new position.
                        NodeTest(NegaMaxBoardStates(depth), depth - 1, Not isWhite, EnemyKPos, TempMeKPos)
                        'If depth = Test Then OutputBitMoveToConsole(PieceMoves(n)) : Console.WriteLine(" " & EndPositionCount - TempValue) : TempValue = EndPositionCount
                    End If
                    'If depth = 2 Then
                    '    OutputBitMoveToConsole(MoveBuffer(n))
                    '    Console.WriteLine(EndPositionCount)
                    '    EndPositionCount = 0
                    'End If
                Next
            End If
        End If
    End Sub
    'Calcualates pseudo-legal moves (apart from double check avoidance) - let NodeTest handle this.
    Private Function NodeTestCreateMoves(ByRef State As BoardState, ByVal MoveBufferStrafe As Integer, ByVal isWhite As Boolean, ByRef KPos As UInt16, ByRef SearchVars As NegaMaxSearchTools) As Integer
        Dim TempPieceMap As UInt64
        Dim NoLegalMoves As Integer
        Dim LegalMoveArray() As UInt16
        Dim NotInCheck As Boolean = SearchVars.CheckInfo = 0US
        Dim AllMovesValid As Boolean = NotInCheck AndAlso State.EnPassant = 0US
        Dim PieceLegalMoves As Integer

        If isWhite Then
            'Double checks cannot be resolved by anything other than king moves (handled above)
            If (SearchVars.CheckInfo And 64US) = 0US Then
                TempPieceMap = State.BitboardPawnWhite
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = WhitePawnLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, KPos, State.EnPassant, True)
                    PieceLegalMoves = LegalMoveArray(0)
                    If PieceLegalMoves > 0 Then
                        If AllMovesValid Then 'Directly copies all moves to the buffer.
                            LegalMoveArray.AsSpan(1, PieceLegalMoves).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe + NoLegalMoves))
                            NoLegalMoves += PieceLegalMoves
                        Else 'We need to check exclicity which moves are valid, and which are not.
                            For n = 1 To PieceLegalMoves
                                If ValidateMove(LegalMoveArray(n), State, True, GlobalConstants.PieceIndex.Pawn, KPos, SearchVars.CheckInfo, SearchVars.OccupancyMask) Then
                                    MoveBuffer(MoveBufferStrafe + NoLegalMoves) = LegalMoveArray(n)
                                    NoLegalMoves += 1
                                End If
                            Next
                        End If
                    End If
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While

                TempPieceMap = State.BitboardKnightWhite
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = KnightLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight Or SearchVars.PinInfoDiag)
                    PieceLegalMoves = LegalMoveArray(0)
                    If PieceLegalMoves > 0 Then
                        If NotInCheck Then 'Note that we don't need to check for EnPassant pin breaks if the piece is not a pawn.
                            LegalMoveArray.AsSpan(1, PieceLegalMoves).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe + NoLegalMoves))
                            NoLegalMoves += PieceLegalMoves
                        Else
                            For n = 1 To PieceLegalMoves
                                If ValidateMove(LegalMoveArray(n), State, True, GlobalConstants.PieceIndex.Knight, KPos, SearchVars.CheckInfo, SearchVars.OccupancyMask) Then
                                    MoveBuffer(MoveBufferStrafe + NoLegalMoves) = LegalMoveArray(n)
                                    NoLegalMoves += 1
                                End If
                            Next
                        End If
                    End If
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While

                TempPieceMap = State.BitboardBishopWhite
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = BishopLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, KPos)
                    PieceLegalMoves = LegalMoveArray(0)
                    If PieceLegalMoves > 0 Then
                        If NotInCheck Then
                            LegalMoveArray.AsSpan(1, PieceLegalMoves).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe + NoLegalMoves))
                            NoLegalMoves += PieceLegalMoves
                        Else
                            For n = 1 To PieceLegalMoves
                                If ValidateMove(LegalMoveArray(n), State, True, GlobalConstants.PieceIndex.Bishop, KPos, SearchVars.CheckInfo, SearchVars.OccupancyMask) Then
                                    MoveBuffer(MoveBufferStrafe + NoLegalMoves) = LegalMoveArray(n)
                                    NoLegalMoves += 1
                                End If
                            Next
                        End If
                    End If
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While

                TempPieceMap = State.BitboardRookWhite
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = RookLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, KPos)
                    PieceLegalMoves = LegalMoveArray(0)
                    If PieceLegalMoves > 0 Then
                        If NotInCheck Then
                            LegalMoveArray.AsSpan(1, PieceLegalMoves).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe + NoLegalMoves))
                            NoLegalMoves += PieceLegalMoves
                        Else
                            For n = 1 To PieceLegalMoves
                                If ValidateMove(LegalMoveArray(n), State, True, GlobalConstants.PieceIndex.Rook, KPos, SearchVars.CheckInfo, SearchVars.OccupancyMask) Then
                                    MoveBuffer(MoveBufferStrafe + NoLegalMoves) = LegalMoveArray(n)
                                    NoLegalMoves += 1
                                End If
                            Next
                        End If
                    End If
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While

                TempPieceMap = State.BitboardQueenWhite
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = QueenLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, KPos)
                    PieceLegalMoves = LegalMoveArray(0)
                    If PieceLegalMoves > 0 Then
                        If NotInCheck Then
                            LegalMoveArray.AsSpan(1, PieceLegalMoves).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe + NoLegalMoves))
                            NoLegalMoves += PieceLegalMoves
                        Else
                            For n = 1 To PieceLegalMoves
                                If ValidateMove(LegalMoveArray(n), State, True, GlobalConstants.PieceIndex.Queen, KPos, SearchVars.CheckInfo, SearchVars.OccupancyMask) Then
                                    MoveBuffer(MoveBufferStrafe + NoLegalMoves) = LegalMoveArray(n)
                                    NoLegalMoves += 1
                                End If
                            Next
                        End If
                    End If
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While
            End If

            LegalMoveArray = KingLegalMoves(KPos, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.TFTable, State.WhiteCanCastle, SearchVars.CheckInfo)
            'King legal moves are always legal (baked into TFTable).
            PieceLegalMoves = LegalMoveArray(0)
            If PieceLegalMoves > 0 Then
                LegalMoveArray.AsSpan(1, PieceLegalMoves).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe + NoLegalMoves))
                NoLegalMoves += PieceLegalMoves
            End If

        Else
            If (SearchVars.CheckInfo And 64US) = 0US Then
                TempPieceMap = State.BitboardPawnBlack
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = BlackPawnLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, KPos, State.EnPassant)
                    PieceLegalMoves = LegalMoveArray(0)
                    If PieceLegalMoves > 0 Then
                        If AllMovesValid Then
                            LegalMoveArray.AsSpan(1, PieceLegalMoves).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe + NoLegalMoves))
                            NoLegalMoves += PieceLegalMoves
                        Else
                            For n = 1 To PieceLegalMoves
                                If ValidateMove(LegalMoveArray(n), State, False, GlobalConstants.PieceIndex.Pawn, KPos, SearchVars.CheckInfo, SearchVars.OccupancyMask) Then
                                    MoveBuffer(MoveBufferStrafe + NoLegalMoves) = LegalMoveArray(n)
                                    NoLegalMoves += 1
                                End If
                            Next
                        End If
                    End If
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While
                TempPieceMap = State.BitboardKnightBlack
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = KnightLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight Or SearchVars.PinInfoDiag)
                    PieceLegalMoves = LegalMoveArray(0)
                    If PieceLegalMoves > 0 Then
                        If NotInCheck Then
                            LegalMoveArray.AsSpan(1, PieceLegalMoves).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe + NoLegalMoves))
                            NoLegalMoves += PieceLegalMoves
                        Else
                            For n = 1 To PieceLegalMoves
                                If ValidateMove(LegalMoveArray(n), State, False, GlobalConstants.PieceIndex.Knight, KPos, SearchVars.CheckInfo, SearchVars.OccupancyMask) Then
                                    MoveBuffer(MoveBufferStrafe + NoLegalMoves) = LegalMoveArray(n)
                                    NoLegalMoves += 1
                                End If
                            Next
                        End If
                    End If
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While
                TempPieceMap = State.BitboardBishopBlack
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = BishopLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, KPos)
                    PieceLegalMoves = LegalMoveArray(0)
                    If PieceLegalMoves > 0 Then
                        If NotInCheck Then
                            LegalMoveArray.AsSpan(1, PieceLegalMoves).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe + NoLegalMoves))
                            NoLegalMoves += PieceLegalMoves
                        Else
                            For n = 1 To PieceLegalMoves
                                If ValidateMove(LegalMoveArray(n), State, False, GlobalConstants.PieceIndex.Bishop, KPos, SearchVars.CheckInfo, SearchVars.OccupancyMask) Then
                                    MoveBuffer(MoveBufferStrafe + NoLegalMoves) = LegalMoveArray(n)
                                    NoLegalMoves += 1
                                End If
                            Next
                        End If
                    End If
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While
                TempPieceMap = State.BitboardRookBlack
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = RookLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, KPos)
                    PieceLegalMoves = LegalMoveArray(0)
                    If PieceLegalMoves > 0 Then
                        If NotInCheck Then
                            LegalMoveArray.AsSpan(1, PieceLegalMoves).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe + NoLegalMoves))
                            NoLegalMoves += PieceLegalMoves
                        Else
                            For n = 1 To PieceLegalMoves
                                If ValidateMove(LegalMoveArray(n), State, False, GlobalConstants.PieceIndex.Rook, KPos, SearchVars.CheckInfo, SearchVars.OccupancyMask) Then
                                    MoveBuffer(MoveBufferStrafe + NoLegalMoves) = LegalMoveArray(n)
                                    NoLegalMoves += 1
                                End If
                            Next
                        End If
                    End If
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While
                TempPieceMap = State.BitboardQueenBlack
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = QueenLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, KPos)
                    PieceLegalMoves = LegalMoveArray(0)
                    If PieceLegalMoves > 0 Then
                        If NotInCheck Then
                            LegalMoveArray.AsSpan(1, PieceLegalMoves).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe + NoLegalMoves))
                            NoLegalMoves += PieceLegalMoves
                        Else
                            For n = 1 To PieceLegalMoves
                                If ValidateMove(LegalMoveArray(n), State, False, GlobalConstants.PieceIndex.Queen, KPos, SearchVars.CheckInfo, SearchVars.OccupancyMask) Then
                                    MoveBuffer(MoveBufferStrafe + NoLegalMoves) = LegalMoveArray(n)
                                    NoLegalMoves += 1
                                End If
                            Next
                        End If
                    End If
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While
            End If
            LegalMoveArray = KingLegalMoves(KPos, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.TFTable, State.BlackCanCastle, SearchVars.CheckInfo)
            PieceLegalMoves = LegalMoveArray(0)
            If PieceLegalMoves > 0 Then
                LegalMoveArray.AsSpan(1, PieceLegalMoves).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe + NoLegalMoves))
                NoLegalMoves += PieceLegalMoves
            End If
        End If
        Return NoLegalMoves
    End Function
    'Subroutine that makes a move on the board, given coordinates. Includes castling (& rights), pawn promotion, and manipuation of ZobristValue.
    Private Sub NodeTestMakeMove(ByVal Move As UInt16, ByRef State As BoardState, ByVal isWhite As Boolean, ByRef KPos As UInt16)
        Dim OldSquare As UInt16 = (Move And 4032US) >> 6
        Dim NewSquare As UInt16 = Move And 63US
        Dim OldPieceMap As UInt64 = 1UL << OldSquare
        Dim NewPieceMap As UInt64 = 1UL << NewSquare
        Dim DontResetEnPassant As Boolean
        If isWhite Then
            If (OldPieceMap And State.BitboardPawnWhite) <> 0UL Then

                'Code for Promoting Pawns and En Passant. Also increments the material count.
                If (Move And 28672US) > 0US Then
                    If (Move And 28672US) = 4096US Then 'Queen Promotion.
                        State.BitboardQueenWhite = State.BitboardQueenWhite Xor NewPieceMap
                    ElseIf (Move And 28672US) = 28672US Then 'Knight Promotion.
                        State.BitboardKnightWhite = State.BitboardKnightWhite Xor NewPieceMap
                    Else
                        If (Move And 28672US) = 8192US AndAlso ((((NewPieceMap And &HFEFEFEFEFEFEFEFEUL) >> 1) Or ((NewPieceMap And &H7F7F7F7F7F7F7F7FUL) << 1)) And State.BitboardPawnBlack) <> 0UL Then
                            'EnPassant creation, if we are neighbouring an enemy pawn. First removes old data.
                            If State.EnPassant <> 0US Then State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(State.EnPassant Mod 8)
                            State.EnPassant = NewSquare + 8US
                            DontResetEnPassant = True
                        ElseIf (Move And 28672US) = 12288US Then 'En Passant capture - remove enemy pawn.
                            State.BitboardPawnBlack = State.BitboardPawnBlack Xor (NewPieceMap << 8)
                        End If
                        'Updates bitboards for normal moves.
                        State.BitboardPawnWhite = State.BitboardPawnWhite Xor NewPieceMap
                    End If
                Else
                    'Updates bitboards for normal moves.
                    State.BitboardPawnWhite = State.BitboardPawnWhite Xor NewPieceMap
                End If
                State.BitboardPawnWhite = State.BitboardPawnWhite Xor OldPieceMap

            Else
                If (OldPieceMap And State.BitboardKnightWhite) <> 0UL Then
                    State.BitboardKnightWhite = State.BitboardKnightWhite Xor OldPieceMap Xor NewPieceMap
                ElseIf (OldPieceMap And State.BitboardBishopWhite) <> 0UL Then
                    State.BitboardBishopWhite = State.BitboardBishopWhite Xor OldPieceMap Xor NewPieceMap
                ElseIf (OldPieceMap And State.BitboardRookWhite) <> 0UL Then
                    State.BitboardRookWhite = State.BitboardRookWhite Xor OldPieceMap Xor NewPieceMap
                    'If piece is a Rook, part of Castling is disabled (depending on which Rook has moved).
                    If State.WhiteCanCastle.KS AndAlso OldSquare = 63US Then
                        'Rook has been moved - the player can no longer castle that side of the board.
                        State.WhiteCanCastle.KS = False
                    ElseIf State.WhiteCanCastle.QS AndAlso OldSquare = 56US Then
                        State.WhiteCanCastle.QS = False
                    End If
                ElseIf (OldPieceMap And State.BitboardQueenWhite) <> 0UL Then
                    State.BitboardQueenWhite = State.BitboardQueenWhite Xor OldPieceMap Xor NewPieceMap
                Else 'The piece must be the king!
                    KPos = NewSquare
                    'Code for Castling.
                    If State.WhiteCanCastle.KS Then
                        If Move = 24382US Then
                            'Moves elements about on the board, and the Zobrist value. Uses the bitboard mask for the old and new squares for
                            'the rook moving to produce this.
                            State.BitboardRookWhite = State.BitboardRookWhite Xor &HA000000000000000UL '&H00000000000000A0UL for black.
                        End If
                        'Player can no longer castle.
                        State.WhiteCanCastle.KS = False
                    End If
                    If State.WhiteCanCastle.QS Then
                        If Move = 28474US Then
                            State.BitboardRookWhite = State.BitboardRookWhite Xor &H900000000000000UL '&0000000000000009UL for black.
                        End If
                        State.WhiteCanCastle.QS = False
                    End If
                End If


            End If
            If Move > 32768US Then
                If (NewPieceMap And State.BitboardPawnBlack) <> 0UL Then
                    State.BitboardPawnBlack = State.BitboardPawnBlack Xor NewPieceMap
                ElseIf (NewPieceMap And State.BitboardKnightBlack) <> 0UL Then
                    State.BitboardKnightBlack = State.BitboardKnightBlack Xor NewPieceMap
                ElseIf (NewPieceMap And State.BitboardBishopBlack) <> 0UL Then
                    State.BitboardBishopBlack = State.BitboardBishopBlack Xor NewPieceMap
                ElseIf (NewPieceMap And State.BitboardRookBlack) <> 0UL Then
                    State.BitboardRookBlack = State.BitboardRookBlack Xor NewPieceMap
                    If State.BlackCanCastle.CanICastle() Then
                        If State.BlackCanCastle.KS AndAlso NewSquare = 7US Then
                            State.BlackCanCastle.KS = False
                        ElseIf State.BlackCanCastle.QS AndAlso NewSquare = 0US Then
                            State.BlackCanCastle.QS = False
                        End If
                    End If
                Else 'The piece must be the queen: king captures are impossible (hopefully lol).
                    State.BitboardQueenBlack = State.BitboardQueenBlack Xor NewPieceMap
                End If
            End If

        Else 'Near-identical code for the black pieces.
            If (OldPieceMap And State.BitboardPawnBlack) <> 0UL Then
                If (Move And 28672US) > 0US Then
                    If (Move And 28672US) = 4096US Then ' Queen Promotion
                        State.BitboardQueenBlack = State.BitboardQueenBlack Xor NewPieceMap
                    ElseIf (Move And 28672US) = 28672US Then ' Knight Promotion
                        State.BitboardKnightBlack = State.BitboardKnightBlack Xor NewPieceMap
                    Else
                        If (Move And 28672US) = 8192US AndAlso ((((NewPieceMap And &HFEFEFEFEFEFEFEFEUL) >> 1) Or ((NewPieceMap And &H7F7F7F7F7F7F7F7FUL) << 1)) And State.BitboardPawnWhite) <> 0UL Then
                            If State.EnPassant <> 0US Then State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(State.EnPassant Mod 8)
                            State.EnPassant = NewSquare - 8US
                            DontResetEnPassant = True
                        ElseIf (Move And 28672US) = 12288US Then
                            State.BitboardPawnWhite = State.BitboardPawnWhite Xor (NewPieceMap >> 8)
                        End If
                        State.BitboardPawnBlack = State.BitboardPawnBlack Xor NewPieceMap
                    End If
                Else
                    State.BitboardPawnBlack = State.BitboardPawnBlack Xor NewPieceMap
                End If
                State.BitboardPawnBlack = State.BitboardPawnBlack Xor OldPieceMap
            Else
                If (OldPieceMap And State.BitboardKnightBlack) <> 0UL Then
                    State.BitboardKnightBlack = State.BitboardKnightBlack Xor OldPieceMap Xor NewPieceMap
                ElseIf (OldPieceMap And State.BitboardBishopBlack) <> 0UL Then
                    State.BitboardBishopBlack = State.BitboardBishopBlack Xor OldPieceMap Xor NewPieceMap
                ElseIf (OldPieceMap And State.BitboardRookBlack) <> 0UL Then
                    State.BitboardRookBlack = State.BitboardRookBlack Xor OldPieceMap Xor NewPieceMap
                    If State.BlackCanCastle.KS AndAlso OldSquare = 7US Then
                        State.BlackCanCastle.KS = False
                    ElseIf State.BlackCanCastle.QS AndAlso OldSquare = 0US Then
                        State.BlackCanCastle.QS = False
                    End If
                ElseIf (OldPieceMap And State.BitboardQueenBlack) <> 0UL Then
                    State.BitboardQueenBlack = State.BitboardQueenBlack Xor OldPieceMap Xor NewPieceMap
                Else
                    KPos = NewSquare
                    If State.BlackCanCastle.KS Then
                        If Move = 20742US Then
                            State.BitboardRookBlack = State.BitboardRookBlack Xor &HA0UL
                        End If
                        State.BlackCanCastle.KS = False
                    End If
                    If State.BlackCanCastle.QS Then
                        If Move = 24834US Then
                            State.BitboardRookBlack = State.BitboardRookBlack Xor &H9UL
                        End If
                        State.BlackCanCastle.QS = False
                    End If
                End If
            End If

            If Move > 32768US Then
                If (NewPieceMap And State.BitboardPawnWhite) <> 0UL Then
                    State.BitboardPawnWhite = State.BitboardPawnWhite Xor NewPieceMap
                ElseIf (NewPieceMap And State.BitboardKnightWhite) <> 0UL Then
                    State.BitboardKnightWhite = State.BitboardKnightWhite Xor NewPieceMap
                ElseIf (NewPieceMap And State.BitboardBishopWhite) <> 0UL Then
                    State.BitboardBishopWhite = State.BitboardBishopWhite Xor NewPieceMap
                ElseIf (NewPieceMap And State.BitboardRookWhite) <> 0UL Then
                    State.BitboardRookWhite = State.BitboardRookWhite Xor NewPieceMap
                    If State.WhiteCanCastle.CanICastle() Then
                        If State.WhiteCanCastle.KS AndAlso NewSquare = 63US Then
                            State.WhiteCanCastle.KS = False
                        ElseIf State.WhiteCanCastle.QS AndAlso NewSquare = 56US Then
                            State.WhiteCanCastle.QS = False
                        End If
                    End If
                Else
                    State.BitboardQueenWhite = State.BitboardQueenWhite Xor NewPieceMap
                End If
            End If
        End If

        'Removes EnPassant information, if it was present.
        If Not (State.EnPassant = 0US OrElse DontResetEnPassant) Then
            'Removal of EnPassant.
            State.EnPassant = 0US
        End If
    End Sub






    'Function which creates and orders all the legal moves a player can make, given certain criteria. Returns the total number of moves.
    Public Function CreateMoves(ByRef State As BoardState, ByVal MoveBufferStrafe As Integer, ByVal isWhite As Boolean, ByRef SearchVars As NegaMaxSearchTools, ByVal MeKPos As UInt16, ByVal EnemyKPos As UInt16, ByVal IncludeNonCaptures As Boolean, ByVal KillerDepth As Integer, Optional ByVal TTMove As UInt16 = 0US) As Integer
        Dim TempPieceMap As UInt64
        Dim CaptureCount As Integer
        Dim LegalMoveArray() As UInt16

        Dim KillerOneMove As UInt16 = KillerMoves(2 * KillerDepth)
        Dim KillerTwoMove As UInt16 = KillerMoves(2 * KillerDepth + 1)
        Dim TTMoveFlag As Integer = If(TTMove = 0US, -1, 0)
        Dim KillerMoveOneFlag As Integer = If(KillerOneMove = 0US, -1, 0)
        Dim KillerMoveTwoFlag As Integer = If(KillerTwoMove = 0US, -1, 0)

        Dim InCheck As Boolean = SearchVars.CheckInfo <> 0US
        Dim NeedValidateMoves As Boolean = InCheck OrElse State.EnPassant <> 0US

        'Reset move buffers.
        PawnPromotionMoves(0) = 0
        TerribleMoves(0) = 0
        GoodMoves(0) = 0
        BadMoves(0) = 0
        OtherMoves(0) = 0

        If isWhite Then

            'Double checks cannot be resolved by anything other than king moves (handled above)
            If (SearchVars.CheckInfo And 64US) = 0US Then
                TempPieceMap = State.BitboardPawnWhite
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = WhitePawnLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, MeKPos, State.EnPassant, IncludeNonCaptures)
                    If LegalMoveArray IsNot Nothing AndAlso LegalMoveArray(0) <> 0US Then ValidateAndPopulateMoveBuffer(LegalMoveArray, State, GlobalConstants.PieceIndex.Pawn, SearchVars, NeedValidateMoves, MeKPos, EnemyKPos, True, MoveBufferStrafe, CaptureCount, TTMove, TTMoveFlag, KillerOneMove, KillerMoveOneFlag, KillerTwoMove, KillerMoveTwoFlag)
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While

                TempPieceMap = State.BitboardKnightWhite
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = KnightLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight Or SearchVars.PinInfoDiag, IncludeNonCaptures)
                    If LegalMoveArray IsNot Nothing AndAlso LegalMoveArray(0) <> 0US Then ValidateAndPopulateMoveBuffer(LegalMoveArray, State, GlobalConstants.PieceIndex.Knight, SearchVars, InCheck, MeKPos, EnemyKPos, True, MoveBufferStrafe, CaptureCount, TTMove, TTMoveFlag, KillerOneMove, KillerMoveOneFlag, KillerTwoMove, KillerMoveTwoFlag)
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While

                TempPieceMap = State.BitboardBishopWhite
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = BishopLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, MeKPos, IncludeNonCaptures)
                    If LegalMoveArray IsNot Nothing AndAlso LegalMoveArray(0) <> 0US Then ValidateAndPopulateMoveBuffer(LegalMoveArray, State, GlobalConstants.PieceIndex.Bishop, SearchVars, InCheck, MeKPos, EnemyKPos, True, MoveBufferStrafe, CaptureCount, TTMove, TTMoveFlag, KillerOneMove, KillerMoveOneFlag, KillerTwoMove, KillerMoveTwoFlag)
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While

                TempPieceMap = State.BitboardRookWhite
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = RookLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, MeKPos, IncludeNonCaptures)
                    If LegalMoveArray IsNot Nothing AndAlso LegalMoveArray(0) <> 0US Then ValidateAndPopulateMoveBuffer(LegalMoveArray, State, GlobalConstants.PieceIndex.Rook, SearchVars, InCheck, MeKPos, EnemyKPos, True, MoveBufferStrafe, CaptureCount, TTMove, TTMoveFlag, KillerOneMove, KillerMoveOneFlag, KillerTwoMove, KillerMoveTwoFlag)
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While

                TempPieceMap = State.BitboardQueenWhite
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = QueenLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, MeKPos, IncludeNonCaptures)
                    If LegalMoveArray IsNot Nothing AndAlso LegalMoveArray(0) <> 0US Then ValidateAndPopulateMoveBuffer(LegalMoveArray, State, GlobalConstants.PieceIndex.Queen, SearchVars, InCheck, MeKPos, EnemyKPos, True, MoveBufferStrafe, CaptureCount, TTMove, TTMoveFlag, KillerOneMove, KillerMoveOneFlag, KillerTwoMove, KillerMoveTwoFlag)
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While
            End If

            LegalMoveArray = KingLegalMoves(MeKPos, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.TFTable, State.WhiteCanCastle, SearchVars.CheckInfo, IncludeNonCaptures)
            'Never need to validate king moves - pseudolegal = legal here.
            If LegalMoveArray IsNot Nothing AndAlso LegalMoveArray(0) <> 0US Then ValidateAndPopulateMoveBuffer(LegalMoveArray, State, GlobalConstants.PieceIndex.King, SearchVars, False, MeKPos, EnemyKPos, True, MoveBufferStrafe, CaptureCount, TTMove, TTMoveFlag, KillerOneMove, KillerMoveOneFlag, KillerTwoMove, KillerMoveTwoFlag)
        Else
            If (SearchVars.CheckInfo And 64US) = 0US Then
                TempPieceMap = State.BitboardPawnBlack
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = BlackPawnLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, MeKPos, State.EnPassant, IncludeNonCaptures)
                    If LegalMoveArray IsNot Nothing AndAlso LegalMoveArray(0) <> 0US Then ValidateAndPopulateMoveBuffer(LegalMoveArray, State, GlobalConstants.PieceIndex.Pawn, SearchVars, NeedValidateMoves, MeKPos, EnemyKPos, False, MoveBufferStrafe, CaptureCount, TTMove, TTMoveFlag, KillerOneMove, KillerMoveOneFlag, KillerTwoMove, KillerMoveTwoFlag)
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While
                TempPieceMap = State.BitboardKnightBlack
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = KnightLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight Or SearchVars.PinInfoDiag, IncludeNonCaptures)
                    If LegalMoveArray IsNot Nothing AndAlso LegalMoveArray(0) <> 0US Then ValidateAndPopulateMoveBuffer(LegalMoveArray, State, GlobalConstants.PieceIndex.Knight, SearchVars, InCheck, MeKPos, EnemyKPos, False, MoveBufferStrafe, CaptureCount, TTMove, TTMoveFlag, KillerOneMove, KillerMoveOneFlag, KillerTwoMove, KillerMoveTwoFlag)
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While
                TempPieceMap = State.BitboardBishopBlack
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = BishopLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, MeKPos, IncludeNonCaptures)
                    If LegalMoveArray IsNot Nothing AndAlso LegalMoveArray(0) <> 0US Then ValidateAndPopulateMoveBuffer(LegalMoveArray, State, GlobalConstants.PieceIndex.Bishop, SearchVars, InCheck, MeKPos, EnemyKPos, False, MoveBufferStrafe, CaptureCount, TTMove, TTMoveFlag, KillerOneMove, KillerMoveOneFlag, KillerTwoMove, KillerMoveTwoFlag)
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While
                TempPieceMap = State.BitboardRookBlack
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = RookLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, MeKPos, IncludeNonCaptures)
                    If LegalMoveArray IsNot Nothing AndAlso LegalMoveArray(0) <> 0US Then ValidateAndPopulateMoveBuffer(LegalMoveArray, State, GlobalConstants.PieceIndex.Rook, SearchVars, InCheck, MeKPos, EnemyKPos, False, MoveBufferStrafe, CaptureCount, TTMove, TTMoveFlag, KillerOneMove, KillerMoveOneFlag, KillerTwoMove, KillerMoveTwoFlag)
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While
                TempPieceMap = State.BitboardQueenBlack
                While TempPieceMap <> 0UL
                    Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                    LegalMoveArray = QueenLegalMoves(Square, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.PinInfoStraight, SearchVars.PinInfoDiag, MeKPos, IncludeNonCaptures)
                    If LegalMoveArray IsNot Nothing AndAlso LegalMoveArray(0) <> 0US Then ValidateAndPopulateMoveBuffer(LegalMoveArray, State, GlobalConstants.PieceIndex.Queen, SearchVars, InCheck, MeKPos, EnemyKPos, False, MoveBufferStrafe, CaptureCount, TTMove, TTMoveFlag, KillerOneMove, KillerMoveOneFlag, KillerTwoMove, KillerMoveTwoFlag)
                    TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
                End While
            End If
            LegalMoveArray = KingLegalMoves(MeKPos, SearchVars.EnemyPieceMask, SearchVars.OccupancyMask, SearchVars.TFTable, State.BlackCanCastle, SearchVars.CheckInfo, IncludeNonCaptures)
            If LegalMoveArray IsNot Nothing AndAlso LegalMoveArray(0) <> 0US Then ValidateAndPopulateMoveBuffer(LegalMoveArray, State, GlobalConstants.PieceIndex.King, SearchVars, False, MeKPos, EnemyKPos, False, MoveBufferStrafe, CaptureCount, TTMove, TTMoveFlag, KillerOneMove, KillerMoveOneFlag, KillerTwoMove, KillerMoveTwoFlag)
        End If

        Dim TotalMoveCount As Integer = If(TTMoveFlag > 0, 1, 0) + CaptureCount
        If IncludeNonCaptures Then TotalMoveCount += If(KillerMoveOneFlag > 0, 1, 0) + If(KillerMoveTwoFlag > 0, 1, 0) + PawnPromotionMoves(0) + GoodMoves(0) + OtherMoves(0) + BadMoves(0) + TerribleMoves(0)
        If TotalMoveCount = 0US Then Return 0US 'No moves found in the position.

        'At the end of the function, we merge all the category arrays into one - producing a huge, tiered,
        'list of a player's total pseudo-legal moves. If the user is in Quiescence mode, all non-capture moves
        'are discounted, and are not merged.

        'Sorts all the capture moves via MVA-LVA, using the scores saved in TempCaptureMoves, via the insersion sort (there aren't many, so it should be okay) :).
        If CaptureCount > 1 Then
            For i As Integer = 1 To CaptureCount - 1
                Dim keyMove As UInt16 = TempCaptureMoves(i)
                Dim keyScore As UInt16 = TempCaptureMoveScores(i)
                Dim j As Integer = i - 1
                While j >= 0 AndAlso TempCaptureMoveScores(j) < keyScore
                    TempCaptureMoves(j + 1) = TempCaptureMoves(j)
                    TempCaptureMoveScores(j + 1) = TempCaptureMoveScores(j)
                    j -= 1
                End While
                TempCaptureMoves(j + 1) = keyMove
                TempCaptureMoveScores(j + 1) = keyScore
            Next
        End If

        'Store TTMove at the start, meaning that it will be searched first.
        If TTMoveFlag = 1 Then MoveBuffer(MoveBufferStrafe) = TTMove : MoveBufferStrafe += 1
        If CaptureCount > 0 Then
            TempCaptureMoves.AsSpan(0, CaptureCount).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe))
            MoveBufferStrafe += CaptureCount 'Makes sure that all further moves will be stored below CaptureMoves.
        End If

        If IncludeNonCaptures Then 'Copies all the non-capture moves to the main array.
            'Copies KillerMoves to AllMoves, if any have been found.
            If KillerMoveOneFlag = 1 Then
                MoveBuffer(MoveBufferStrafe) = KillerOneMove
                MoveBufferStrafe += 1
            End If
            If KillerMoveTwoFlag = 1 Then
                MoveBuffer(MoveBufferStrafe) = KillerTwoMove
                MoveBufferStrafe += 1
            End If

            Dim TempMoveCount As UInt16 = PawnPromotionMoves(0)
            'As very few moves will likely end up in this array, for loops are faster (no overhead).
            For n = 1 To TempMoveCount
                MoveBuffer(MoveBufferStrafe + n - 1) = PawnPromotionMoves(n)
            Next
            MoveBufferStrafe += TempMoveCount

            TempMoveCount = GoodMoves(0)
            If TempMoveCount > 0US Then
                GoodMoves.AsSpan(1, TempMoveCount).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe))
                MoveBufferStrafe += TempMoveCount
            End If

            TempMoveCount = OtherMoves(0)
            If TempMoveCount > 0US Then
                OtherMoves.AsSpan(1, TempMoveCount).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe))
                MoveBufferStrafe += TempMoveCount
            End If

            TempMoveCount = BadMoves(0)
            If TempMoveCount > 0US Then
                BadMoves.AsSpan(1, TempMoveCount).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe))
                MoveBufferStrafe += TempMoveCount
            End If

            TempMoveCount = TerribleMoves(0)
            If TempMoveCount > 0US Then TerribleMoves.AsSpan(1, TempMoveCount).CopyTo(MoveBuffer.AsSpan(MoveBufferStrafe))
        End If

        Return TotalMoveCount 'We already have the strafe into this array - this allows us to calculate all the moves.
    End Function

    Public Sub ValidateAndPopulateMoveBuffer(ByVal LegalMoveArray() As UInt16, ByRef State As BoardState, ByVal PieceIndex As Integer, ByRef SearchInfo As NegaMaxSearchTools, ByVal NeedValidateMoves As Boolean, ByVal MeKPos As UInt16, ByVal EnemyKPos As UInt16, ByVal isWhite As Boolean, ByVal Strafe As Integer, ByRef CaptureCount As Integer, ByVal TTMove As UInt16, ByRef TTMoveFlag As Integer, ByVal KillerOneMove As UInt16, ByRef KillerOneFlag As Integer, ByVal KillerTwoMove As UInt16, ByRef KillerTwoFlag As Integer)
        For n = 1 To LegalMoveArray(0)
            Dim Move As UInt16 = LegalMoveArray(n)
            'Locates TTMoves immediately.
            If TTMoveFlag = 0 AndAlso Move = TTMove Then
                TTMoveFlag = 1
                Continue For
            End If
            If NeedValidateMoves AndAlso Not ValidateMove(Move, State, isWhite, PieceIndex, MeKPos, SearchInfo.CheckInfo, SearchInfo.OccupancyMask) Then Continue For

            'The move is legal: add it to the main move buffer (if it is a capture move - sorted by MVVLVA first) or a segmented set of move classes.
            Dim PieceMap As UInt64 = 1UL << (Move And 63US)
            If (Move >= 32768US) Then  '= capture move.
                'Finds the defending piece.
                Dim PieceValueDiff As UInt16
                If isWhite Then
                    If (PieceMap And State.BitboardPawnBlack) <> 0UL Then
                        PieceValueDiff = MVVLVAValues(6 * GlobalConstants.PieceIndex.Pawn + PieceIndex)
                    ElseIf (PieceMap And State.BitboardKnightBlack) <> 0UL Then
                        PieceValueDiff = MVVLVAValues(6 * GlobalConstants.PieceIndex.Knight + PieceIndex)
                    ElseIf (PieceMap And State.BitboardBishopBlack) <> 0UL Then
                        PieceValueDiff = MVVLVAValues(6 * GlobalConstants.PieceIndex.Bishop + PieceIndex)
                    ElseIf (PieceMap And State.BitboardRookBlack) <> 0UL Then
                        PieceValueDiff = MVVLVAValues(6 * GlobalConstants.PieceIndex.Rook + PieceIndex)
                    Else 'The piece must be the queen: king captures are impossible (hopefully lol).
                        PieceValueDiff = MVVLVAValues(6 * GlobalConstants.PieceIndex.Queen + PieceIndex)
                    End If
                Else
                    If (PieceMap And State.BitboardPawnWhite) <> 0UL Then
                        PieceValueDiff = MVVLVAValues(6 * GlobalConstants.PieceIndex.Pawn + PieceIndex)
                    ElseIf (PieceMap And State.BitboardKnightWhite) <> 0UL Then
                        PieceValueDiff = MVVLVAValues(6 * GlobalConstants.PieceIndex.Knight + PieceIndex)
                    ElseIf (PieceMap And State.BitboardBishopWhite) <> 0UL Then
                        PieceValueDiff = MVVLVAValues(6 * GlobalConstants.PieceIndex.Bishop + PieceIndex)
                    ElseIf (PieceMap And State.BitboardRookWhite) <> 0UL Then
                        PieceValueDiff = MVVLVAValues(6 * GlobalConstants.PieceIndex.Rook + PieceIndex)
                    Else
                        PieceValueDiff = MVVLVAValues(6 * GlobalConstants.PieceIndex.Queen + PieceIndex)
                    End If
                End If

                TempCaptureMoves(CaptureCount) = Move
                TempCaptureMoveScores(CaptureCount) = PieceValueDiff
                CaptureCount += 1US
            ElseIf (Move And 28672US) = 12288US Then 'Is an En Passant Capture - add to Capture Moves.
                '(aghhh I don't like how we need to check EVERY single move to see whether it's EnPassant) :((
                TempCaptureMoves(CaptureCount) = Move
                TempCaptureMoveScores(CaptureCount) = MVVLVAValues(7 * GlobalConstants.PieceIndex.Pawn)
                CaptureCount += 1US
            Else 'In Quiescence, non capture moves are already removed by PieceLegalMoves.
                'Determines if the move is a direct match with the required index in KillerMoves(). If there is one,
                'the move is added to the TempKillerMoves
                If KillerOneFlag = 0 AndAlso Move = KillerOneMove Then
                    KillerOneFlag = 1
                ElseIf KillerTwoFlag = 0 AndAlso Move = KillerTwoMove Then
                    KillerTwoFlag = 1
                Else '"Bucket" based move ordering system - get rid of bad moves first!!
                    Dim TargetSquare As UInt16 = Move And 63US
                    If PieceIndex = GlobalConstants.PieceIndex.Pawn AndAlso (TargetSquare < 16US OrElse TargetSquare > 47US) Then 'User is promoting a pawn (or is very close to).
                        PawnPromotionMoves(0) += 1US
                        PawnPromotionMoves(PawnPromotionMoves(0)) = Move
                    ElseIf If(isWhite, ((((PieceMap And &HFEFEFEFEFEFEFEFEUL) >> 9) Or ((PieceMap And &H7F7F7F7F7F7F7F7FUL) >> 7)) And State.BitboardPawnBlack),
                       (((PieceMap And &HFEFEFEFEFEFEFEFEUL) << 7) Or ((PieceMap And &H7F7F7F7F7F7F7F7FUL) << 9)) And State.BitboardPawnWhite) <> 0UL Then
                        'New square is controlled by an enemy pawn - ammend move list.
                        TerribleMoves(0) += 1US
                        TerribleMoves(TerribleMoves(0)) = Move
                    ElseIf (SearchInfo.TFTable And PieceMap) = 0UL Then
                        'Piece is positioned on a "False" on the TFTable, meaning the square is controlled by an enemy piece.
                        BadMoves(0) += 1US
                        BadMoves(BadMoves(0)) = Move
                    ElseIf (KingDangerMapKnight(EnemyKPos) And PieceMap) <> 0UL Then
                        'Piece moves to a location close to the enemy king - leading to a possible check / attack.
                        GoodMoves(0) += 1US
                        GoodMoves(GoodMoves(0)) = Move
                    Else 'Is a regular move. Ammend move list.
                        OtherMoves(0) += 1US
                        OtherMoves(OtherMoves(0)) = Move
                    End If
                End If
            End If
        Next
    End Sub
    'Successor of DoesMoveResolveCheck. Function which receives a game position and a possible move. The function makes this move on the
    'board (using shortcuts that can only be made on a virtual board), and then generates the possible moves of the attacking
    'piece(s). If the king is no longer being threatened, then the check as been resolved.
    Public Function ValidateMove(ByVal Move As UInt16, ByRef State As BoardState, ByVal isWhite As Boolean, ByVal PieceIndex As Integer, ByVal KPos As UInt16, ByVal CheckInfo As UInt16, ByVal OccupancyMap As UInt64) As Boolean
        If CheckInfo <> 0US Then
            'Assume all moves are legal, unless proven otherwise.
            'King moves are Not considered, as the player's TFTable will ensure that all king moves are legal.
            If PieceIndex <> GlobalConstants.PieceIndex.King Then
                'Runs through bitboard DoesMoveResolveCheck algorithm.
                If Move >= 32768US Then
                    'Captures of the checking piece (or en-passant captures) is considered a resolution. Otherwise, not valid.
                    Return (Move And 63US) = (CheckInfo And 63US)
                ElseIf (Move And 28672US) = 12288US Then
                    Return (Move And 7US) = (CheckInfo And 7US)
                Else
                    'Move must block a sliding piece's ray for it to be valid.
                    Return (RayMap(64 * (CheckInfo And 63US) + KPos) And 1UL << (Move And 63US)) <> 0UL
                End If
            End If
        ElseIf (Move And 28672US) = 12288US Then
            'Removes violations of EnPassant Pins by simulating the move and checking if this opened up a rook's ray.
            Dim NewSquare As Integer = Move And 63US
            Dim LostPawnSquare As Integer = NewSquare + If(isWhite, 8, -8)
            Dim OccupancyAfterEnPassant As UInt64 = OccupancyMap Xor ((1UL << LostPawnSquare) Or (1UL << ((Move And 4032US) >> 6))) Xor (1UL << NewSquare)
            Dim PossibleEnPassantPinners As UInt64 = If(isWhite, State.BitboardRookBlack Or State.BitboardQueenBlack, State.BitboardRookWhite Or State.BitboardQueenWhite)
            If (RookMagicLookup(KPos, OccupancyAfterEnPassant) And PossibleEnPassantPinners) <> 0UL Then Return False
        End If
        Return True
    End Function



    'Algorithm that orderes the moves (as created by CreateMoves) in a more distinguished fashion. Note that this is only valid for the Base Position.
    Public Sub SortMovesThorough(ByRef Moves() As UInt16, ByRef State As BoardState, ByVal isWhite As Boolean, ByVal MeKPos As UInt16, ByVal EnemyKPos As UInt16, ByVal TFTable As UInt64)
        If Moves Is Nothing Then Exit Sub
        NumCapturesThreatsInBasePos = 0
        Dim MoveScores(Moves.Length - 1) As Double
        Dim IsCaptureMove As Boolean

        Dim OldEval, NewEval As Int16
        If SearchSettings.UsePieceHeatMaps Then OldEval = Evaluate(State, isWhite, MeKPos, EnemyKPos)

        Dim IndexInTT As Integer = CInt(State.ZobristValue >> GlobalConstants.TranspositionTableSize)
        Dim BaseEntryInTT As Integer = If(SearchSettings.UseTranspositionTable AndAlso TranspositionTable(IndexInTT).Key = State.ZobristValue, IndexInTT, -1)
        For n = 0 To Moves.Length - 1
            MoveScores(n) = 10000
            Dim MoveIsCapture As Boolean = (Moves(n) >= 32768US) OrElse (Moves(n) And 28672US) = 12288US

            'If the Transposition Table has an entry for the current position, and we are looking at the best move found, give this a bloody massive bonus.
            If BaseEntryInTT >= 0 AndAlso TranspositionTable(BaseEntryInTT).BestMove = Moves(n) Then
                MoveScores(n) = 100000
                If MoveIsCapture Then NumCapturesThreatsInBasePos += 1
            Else
                Dim OldSquare As UInt16 = (Moves(n) And 4032US) >> 6
                Dim NewSquare As UInt16 = Moves(n) And 63US
                Dim PieceIndex As Integer = GetPieceIndexFromSquare(OldSquare, State, isWhite)
                Dim PieceWeight As Integer = PieceValue(PieceIndex)
                IsCaptureMove = False
                If MoveIsCapture Then '= capture move.
                    NumCapturesThreatsInBasePos += 1
                    IsCaptureMove = True
                    'Gets the difference in weight between the capturing piece, and the captured piece (MVV-LVA).
                    Dim PieceValueDiff As Double = If((Moves(n) And 32768) = 32768, 6 * PieceValue(GetPieceIndexFromSquare(NewSquare, State, Not isWhite)) - PieceWeight, 5 * GlobalConstants.PieceWeight.Pawn)
                    MoveScores(n) += 5000 + 5 * PieceValueDiff 'Huge bonus for taking pieces, and especially relatively heavy pieces.
                End If

                If PieceIndex = GlobalConstants.PieceIndex.Pawn Then
                    If NewSquare < 8US OrElse NewSquare > 55US Then 'User has promoted a pawn - big bonus :D.
                        MoveScores(n) += 10000 + 10 * If((Moves(n) And 28672US) = 4096US, GlobalConstants.PieceWeight.Queen, GlobalConstants.PieceWeight.Knight)
                    ElseIf NewSquare < 16US OrElse NewSquare > 47US Then 'User is very close to promoting a pawn.
                        MoveScores(n) += 250
                    End If
                End If
                Dim PieceMap As UInt64 = 1UL << OldSquare
                If (KingDangerMapKnight(EnemyKPos) And PieceMap) <> 0UL Then
                    'Piece moves to a location close to the enemy king - leading to a possible check / attack.
                    MoveScores(n) += 50
                End If

                'Checks if the old square is controlled by an enemy pawn. If so, we should encourage moving it.
                Dim SquareIsControlledByPawn As Boolean
                If isWhite Then
                    SquareIsControlledByPawn = ((((PieceMap And &HFEFEFEFEFEFEFEFEUL) >> 9) Or ((PieceMap And &H7F7F7F7F7F7F7F7FUL) >> 7)) And State.BitboardPawnBlack) <> 0UL
                Else
                    SquareIsControlledByPawn = ((((PieceMap And &HFEFEFEFEFEFEFEFEUL) << 7) Or ((PieceMap And &H7F7F7F7F7F7F7F7FUL) << 9)) And State.BitboardPawnWhite) <> 0UL
                End If
                If SquareIsControlledByPawn Then
                    MoveScores(n) += PieceWeight * PieceWeight \ 500
                ElseIf (PieceMap And TFTable) = 0UL Then
                    MoveScores(n) += PieceWeight * PieceWeight \ 2500
                End If

                'Checks if the new square is controlled by an enemy pawn.
                PieceMap = 1UL << NewSquare
                If isWhite Then
                    SquareIsControlledByPawn = ((((PieceMap And &HFEFEFEFEFEFEFEFEUL) >> 9) Or ((PieceMap And &H7F7F7F7F7F7F7F7FUL) >> 7)) And State.BitboardPawnBlack) <> 0UL
                Else
                    SquareIsControlledByPawn = ((((PieceMap And &HFEFEFEFEFEFEFEFEUL) << 7) Or ((PieceMap And &H7F7F7F7F7F7F7F7FUL) << 9)) And State.BitboardPawnWhite) <> 0UL
                End If
                If SquareIsControlledByPawn Then
                    MoveScores(n) -= (PieceWeight * PieceWeight \ 250) * If(IsCaptureMove, 1, 2)
                ElseIf (PieceMap And TFTable) = 0UL Then
                    'The captured piece can be recaptured on the next move, or the piece is moving to a position that is attacked by an enemy piece.
                    MoveScores(n) -= (PieceWeight * PieceWeight \ 1000) * If(IsCaptureMove, 0.5, 1) 'Add a penalty.
                End If

                'Castling is generally pretty cool :D.
                If (Moves(n) And 16384US) <> 0US Then MoveScores(n) += 100

                'Calculates if the move puts the opposing player into check.
                'Creates temporary variables.
                Dim TempState As BoardState = State
                Dim TempMeKPos As UInt16 = MeKPos
                MakeMove(Moves(n), TempState, isWhite, TempMeKPos)
                Dim TempSearchVars As NegaMaxSearchTools = CalibrateForMoveGeneration(TempState, TempMeKPos, EnemyKPos, Not isWhite)
                If TempSearchVars.CheckInfo <> 0US Then
                    'The move has put the enemy king in check - give a big bonus.
                    MoveScores(n) += 2500
                    NumCapturesThreatsInBasePos += 1
                End If

                'Evaluates how this move improves the player's position, using the PieceHeatMaps.
                If SearchSettings.UsePieceHeatMaps Then
                    NewEval = Evaluate(TempState, isWhite, TempMeKPos, EnemyKPos)
                    MoveScores(n) += (NewEval - OldEval) * 4
                End If
            End If
        Next

        'Orderes the moves using the bubble sort.
        Dim IsSorted As Boolean
        Dim ISwap As UInt16
        Dim DSwap As Double
        For n = 0 To Moves.Length - 2
            IsSorted = True
            For m = 0 To Moves.Length - n - 2
                If MoveScores(m) < MoveScores(m + 1) Then
                    'Swap the moves, and their scores.
                    ISwap = Moves(m + 1)
                    Moves(m + 1) = Moves(m)
                    Moves(m) = ISwap

                    DSwap = MoveScores(m + 1)
                    MoveScores(m + 1) = MoveScores(m)
                    MoveScores(m) = DSwap

                    IsSorted = False
                End If
            Next
            If IsSorted Then Exit For
        Next
    End Sub



    'Subroutine that makes a move on the board, given coordinates. Includes castling (& rights), pawn promotion, and manipuation of ZobristValue.
    Private Sub MakeMove(ByVal Move As UInt16, ByRef State As BoardState, ByVal isWhite As Boolean, ByRef KPos As UInt16)
        Dim OldSquare As UInt16 = (Move And 4032US) >> 6
        Dim NewSquare As UInt16 = Move And 63US
        Dim OldPieceMap As UInt64 = 1UL << OldSquare
        Dim NewPieceMap As UInt64 = 1UL << NewSquare
        Dim DontResetEnPassant As Boolean
        State.HalfMoveSize += 1US 'We assume that the move is not a pawn move or a capture, and increment the Half-Move count. If we are wrong, we just reset to 0 :).
        If isWhite Then
            If (OldPieceMap And State.BitboardPawnWhite) <> 0UL Then

                'Code for Promoting Pawns and En Passant. Also increments the material count.
                If (Move And 28672US) > 0US Then
                    If (Move And 28672US) = 4096US Then 'Queen Promotion.
                        State.BitboardQueenWhite = State.BitboardQueenWhite Xor NewPieceMap
                        State.MaterialCountWhite += GlobalConstants.PieceWeight.Queen - GlobalConstants.PieceWeight.Pawn '+ 9 for a new queen, - 1 for losing the pawn in the process.
                        State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Queen, 0, NewSquare)
                        State.PHMValueWhite += GetPHMValue(GlobalConstants.PieceIndex.Queen, 0, NewSquare, 16)
                    ElseIf (Move And 28672US) = 28672US Then 'Knight Promotion.
                        State.BitboardKnightWhite = State.BitboardKnightWhite Xor NewPieceMap
                        State.MaterialCountWhite += GlobalConstants.PieceWeight.Knight - GlobalConstants.PieceWeight.Pawn '+ 3 for a new knight, - 1 for losing the pawn in the process.
                        State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Knight, 0, NewSquare)
                        State.PHMValueWhite += GetPHMValue(GlobalConstants.PieceIndex.Knight, 0, NewSquare, 16)
                    Else
                        If (Move And 28672US) = 8192US AndAlso ((((NewPieceMap And &HFEFEFEFEFEFEFEFEUL) >> 1) Or ((NewPieceMap And &H7F7F7F7F7F7F7F7FUL) << 1)) And State.BitboardPawnBlack) <> 0UL Then
                            'EnPassant creation, if we are neighbouring an enemy pawn. First removes old data.
                            If State.EnPassant <> 0US Then State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(State.EnPassant Mod 8)
                            State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(NewSquare Mod 8) 'Ammended for en passant creation.
                            State.EnPassant = NewSquare + 8US
                            DontResetEnPassant = True
                        ElseIf (Move And 28672US) = 12288US Then 'En Passant capture - remove enemy pawn.
                            State.BitboardPawnBlack = State.BitboardPawnBlack Xor (NewPieceMap << 8)
                            State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Pawn, 1, NewSquare + 8US) 'Ammended for a capture of a pawn.
                            State.MaterialCountBlack -= GlobalConstants.PieceWeight.Pawn
                            State.PHMValueBlack -= GetPHMValue(GlobalConstants.PieceIndex.Pawn, 1, NewSquare + 8US, 16)
                        End If
                        'Updates bitboards for normal moves.
                        State.BitboardPawnWhite = State.BitboardPawnWhite Xor NewPieceMap
                        State.PHMValueWhite += GetPHMValue(GlobalConstants.PieceIndex.Pawn, 0, NewSquare, 16)
                        State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Pawn, 0, NewSquare)
                    End If
                Else
                    'Updates bitboards for normal moves.
                    State.BitboardPawnWhite = State.BitboardPawnWhite Xor NewPieceMap
                    State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Pawn, 0, NewSquare)
                    State.PHMValueWhite += GetPHMValue(GlobalConstants.PieceIndex.Pawn, 0, NewSquare, 16)
                End If
                State.BitboardPawnWhite = State.BitboardPawnWhite Xor OldPieceMap
                'Removes the piece from the board's Zobrist Value, and modifies PHM values.
                State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Pawn, 0, OldSquare)
                State.PHMValueWhite -= GetPHMValue(GlobalConstants.PieceIndex.Pawn, 0, OldSquare, 16)
                State.HalfMoveSize = 0 'A pawn has moved - reset the Half-Move.

            Else
                Dim PieceIndex As Integer
                If (OldPieceMap And State.BitboardKnightWhite) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Knight
                    State.BitboardKnightWhite = State.BitboardKnightWhite Xor OldPieceMap Xor NewPieceMap
                ElseIf (OldPieceMap And State.BitboardBishopWhite) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Bishop
                    State.BitboardBishopWhite = State.BitboardBishopWhite Xor OldPieceMap Xor NewPieceMap
                ElseIf (OldPieceMap And State.BitboardRookWhite) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Rook
                    State.BitboardRookWhite = State.BitboardRookWhite Xor OldPieceMap Xor NewPieceMap
                    'If piece is a Rook, part of Castling is disabled (depending on which Rook has moved).
                    If State.WhiteCanCastle.KS AndAlso OldSquare = 63US Then
                        'Rook has been moved - the player can no longer castle that side of the board.
                        State.WhiteCanCastle.KS = False
                        State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(9) 'Ammends the Zobrist Value for that player no longer being able to castle.
                    ElseIf State.WhiteCanCastle.QS AndAlso OldSquare = 56US Then
                        State.WhiteCanCastle.QS = False
                        State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(10)
                    End If
                ElseIf (OldPieceMap And State.BitboardQueenWhite) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Queen
                    State.BitboardQueenWhite = State.BitboardQueenWhite Xor OldPieceMap Xor NewPieceMap
                Else 'The piece must be the king!
                    PieceIndex = GlobalConstants.PieceIndex.King
                    KPos = NewSquare
                    'Code for Castling.
                    If State.WhiteCanCastle.KS Then
                        If Move = 24382US Then
                            'Moves elements about on the board, and the Zobrist value. Uses the bitboard mask for the old and new squares for
                            'the rook moving to produce this.
                            State.BitboardRookWhite = State.BitboardRookWhite Xor &HA000000000000000UL '&H00000000000000A0UL for black.
                            State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Rook, 0, 61) Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Rook, 0, 63)
                            State.PHMValueWhite += GetPHMValue(GlobalConstants.PieceIndex.Rook, 0, 61, 16) - GetPHMValue(GlobalConstants.PieceIndex.Rook, 0, 63, 16)
                        End If
                        'Player can no longer castle.
                        State.WhiteCanCastle.KS = False
                        State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(9)
                    End If
                    If State.WhiteCanCastle.QS Then
                        If Move = 28474US Then
                            State.BitboardRookWhite = State.BitboardRookWhite Xor &H900000000000000UL '&0000000000000009UL for black.
                            State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Rook, 0, 56) Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Rook, 0, 59)
                            State.PHMValueWhite += GetPHMValue(GlobalConstants.PieceIndex.Rook, 0, 59, 16) - GetPHMValue(GlobalConstants.PieceIndex.Rook, 0, 56, 16)
                        End If
                        State.WhiteCanCastle.QS = False
                        State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(10)
                    End If
                End If

                'No pawn promotion shenanigans, or en-passant - captured pieces are at the desination square, and no pieces are changing into others.
                'Removes the piece from the board's Zobrist Value, and calculates PHM values.
                State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(PieceIndex, 0, OldSquare) Xor GetZobristHashTableValue(PieceIndex, 0, NewSquare)
                State.PHMValueWhite += GetPHMValue(PieceIndex, 0, NewSquare, 16) - GetPHMValue(PieceIndex, 0, OldSquare, 16)
            End If

            'At the end of the subroutine, the Piece is placed at the new coordinates, and the old position is cleared.
            'If the new position contains a piece, then the material count is updated for only that piece.
            If Move > 32768US Then
                Dim PieceIndex As Integer
                If (NewPieceMap And State.BitboardPawnBlack) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Pawn
                    State.BitboardPawnBlack = State.BitboardPawnBlack Xor NewPieceMap
                ElseIf (NewPieceMap And State.BitboardKnightBlack) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Knight
                    State.BitboardKnightBlack = State.BitboardKnightBlack Xor NewPieceMap
                ElseIf (NewPieceMap And State.BitboardBishopBlack) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Bishop
                    State.BitboardBishopBlack = State.BitboardBishopBlack Xor NewPieceMap
                ElseIf (NewPieceMap And State.BitboardRookBlack) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Rook
                    State.BitboardRookBlack = State.BitboardRookBlack Xor NewPieceMap
                    If State.BlackCanCastle.CanICastle() Then
                        If State.BlackCanCastle.KS AndAlso NewSquare = 7US Then
                            State.BlackCanCastle.KS = False
                            State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(11)
                        ElseIf State.BlackCanCastle.QS AndAlso NewSquare = 0US Then
                            State.BlackCanCastle.QS = False
                            State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(12)
                        End If
                    End If
                Else 'The piece must be the queen: king captures are impossible (hopefully lol).
                    PieceIndex = GlobalConstants.PieceIndex.Queen
                    State.BitboardQueenBlack = State.BitboardQueenBlack Xor NewPieceMap
                End If
                State.MaterialCountBlack -= PieceValue(PieceIndex)
                State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(PieceIndex, 1, NewSquare)
                State.PHMValueBlack -= GetPHMValue(PieceIndex, 1, NewSquare, 16)
                State.HalfMoveSize = 0 'Capture Move - reset Half-Move count.
            End If

        Else 'Near-identical code for the black pieces.
            If (OldPieceMap And State.BitboardPawnBlack) <> 0UL Then
                If (Move And 28672US) > 0US Then
                    If (Move And 28672US) = 4096US Then ' Queen Promotion
                        State.BitboardQueenBlack = State.BitboardQueenBlack Xor NewPieceMap
                        State.MaterialCountBlack += GlobalConstants.PieceWeight.Queen - GlobalConstants.PieceWeight.Pawn
                        State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Queen, 1, NewSquare)
                        State.PHMValueBlack += GetPHMValue(GlobalConstants.PieceIndex.Queen, 1, NewSquare, 16)
                    ElseIf (Move And 28672US) = 28672US Then ' Knight Promotion
                        State.BitboardKnightBlack = State.BitboardKnightBlack Xor NewPieceMap
                        State.MaterialCountBlack += GlobalConstants.PieceWeight.Knight - GlobalConstants.PieceWeight.Pawn
                        State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Knight, 1, NewSquare)
                        State.PHMValueBlack += GetPHMValue(GlobalConstants.PieceIndex.Knight, 1, NewSquare, 16)
                    Else
                        If (Move And 28672US) = 8192US AndAlso ((((NewPieceMap And &HFEFEFEFEFEFEFEFEUL) >> 1) Or ((NewPieceMap And &H7F7F7F7F7F7F7F7FUL) << 1)) And State.BitboardPawnWhite) <> 0UL Then
                            If State.EnPassant <> 0US Then State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(State.EnPassant Mod 8)
                            State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(NewSquare Mod 8)
                            State.EnPassant = NewSquare - 8US
                            DontResetEnPassant = True
                        ElseIf (Move And 28672US) = 12288US Then
                            State.BitboardPawnWhite = State.BitboardPawnWhite Xor (NewPieceMap >> 8)
                            State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Pawn, 0, NewSquare - 8US)
                            State.MaterialCountWhite -= GlobalConstants.PieceWeight.Pawn
                            State.PHMValueWhite -= GetPHMValue(GlobalConstants.PieceIndex.Pawn, 0, NewSquare - 8US, 16)
                        End If
                        State.BitboardPawnBlack = State.BitboardPawnBlack Xor NewPieceMap
                        State.PHMValueBlack += GetPHMValue(GlobalConstants.PieceIndex.Pawn, 1, NewSquare, 16)
                        State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Pawn, 1, NewSquare)
                    End If
                Else
                    State.BitboardPawnBlack = State.BitboardPawnBlack Xor NewPieceMap
                    State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Pawn, 1, NewSquare)
                    State.PHMValueBlack += GetPHMValue(GlobalConstants.PieceIndex.Pawn, 1, NewSquare, 16)
                End If
                State.BitboardPawnBlack = State.BitboardPawnBlack Xor OldPieceMap
                State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Pawn, 1, OldSquare)
                State.PHMValueBlack -= GetPHMValue(GlobalConstants.PieceIndex.Pawn, 1, OldSquare, 16)
                State.HalfMoveSize = 0
            Else
                Dim PieceIndex As Integer
                If (OldPieceMap And State.BitboardKnightBlack) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Knight
                    State.BitboardKnightBlack = State.BitboardKnightBlack Xor OldPieceMap Xor NewPieceMap
                ElseIf (OldPieceMap And State.BitboardBishopBlack) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Bishop
                    State.BitboardBishopBlack = State.BitboardBishopBlack Xor OldPieceMap Xor NewPieceMap
                ElseIf (OldPieceMap And State.BitboardRookBlack) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Rook
                    State.BitboardRookBlack = State.BitboardRookBlack Xor OldPieceMap Xor NewPieceMap
                    If State.BlackCanCastle.KS AndAlso OldSquare = 7US Then
                        State.BlackCanCastle.KS = False
                        State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(11)
                    ElseIf State.BlackCanCastle.QS AndAlso OldSquare = 0US Then
                        State.BlackCanCastle.QS = False
                        State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(12)
                    End If
                ElseIf (OldPieceMap And State.BitboardQueenBlack) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Queen
                    State.BitboardQueenBlack = State.BitboardQueenBlack Xor OldPieceMap Xor NewPieceMap
                Else
                    PieceIndex = GlobalConstants.PieceIndex.King
                    KPos = NewSquare
                    If State.BlackCanCastle.KS Then
                        If Move = 20742US Then
                            State.BitboardRookBlack = State.BitboardRookBlack Xor &HA0UL
                            State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Rook, 1, 5) Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Rook, 1, 7)
                            State.PHMValueBlack += GetPHMValue(GlobalConstants.PieceIndex.Rook, 1, 5, 16) - GetPHMValue(GlobalConstants.PieceIndex.Rook, 1, 7, 16)
                        End If
                        State.BlackCanCastle.KS = False
                        State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(11)
                    End If
                    If State.BlackCanCastle.QS Then
                        If Move = 24834US Then
                            State.BitboardRookBlack = State.BitboardRookBlack Xor &H9UL
                            State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Rook, 1, 0) Xor GetZobristHashTableValue(GlobalConstants.PieceIndex.Rook, 1, 3)
                            State.PHMValueBlack += GetPHMValue(GlobalConstants.PieceIndex.Rook, 1, 3, 16) - GetPHMValue(GlobalConstants.PieceIndex.Rook, 1, 0, 16)
                        End If
                        State.BlackCanCastle.QS = False
                        State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(12)
                    End If
                End If

                State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(PieceIndex, 1, OldSquare) Xor GetZobristHashTableValue(PieceIndex, 1, NewSquare)
                State.PHMValueBlack += GetPHMValue(PieceIndex, 1, NewSquare, 16) - GetPHMValue(PieceIndex, 1, OldSquare, 16)
            End If

            If Move > 32768US Then
                Dim PieceIndex As Integer
                If (NewPieceMap And State.BitboardPawnWhite) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Pawn
                    State.BitboardPawnWhite = State.BitboardPawnWhite Xor NewPieceMap
                ElseIf (NewPieceMap And State.BitboardKnightWhite) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Knight
                    State.BitboardKnightWhite = State.BitboardKnightWhite Xor NewPieceMap
                ElseIf (NewPieceMap And State.BitboardBishopWhite) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Bishop
                    State.BitboardBishopWhite = State.BitboardBishopWhite Xor NewPieceMap
                ElseIf (NewPieceMap And State.BitboardRookWhite) <> 0UL Then
                    PieceIndex = GlobalConstants.PieceIndex.Rook
                    State.BitboardRookWhite = State.BitboardRookWhite Xor NewPieceMap
                    If State.WhiteCanCastle.CanICastle() Then
                        If State.WhiteCanCastle.KS AndAlso NewSquare = 63US Then
                            State.WhiteCanCastle.KS = False
                            State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(9)
                        ElseIf State.WhiteCanCastle.QS AndAlso NewSquare = 56US Then
                            State.WhiteCanCastle.QS = False
                            State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(10)
                        End If
                    End If
                Else
                    PieceIndex = GlobalConstants.PieceIndex.Queen
                    State.BitboardQueenWhite = State.BitboardQueenWhite Xor NewPieceMap
                End If
                State.MaterialCountWhite -= PieceValue(PieceIndex)
                State.ZobristValue = State.ZobristValue Xor GetZobristHashTableValue(PieceIndex, 0, NewSquare)
                State.PHMValueWhite -= GetPHMValue(PieceIndex, 0, NewSquare, 16)
                State.HalfMoveSize = 0
            End If
        End If

        'Removes EnPassant information, if it was present.
        If Not (State.EnPassant = 0US OrElse DontResetEnPassant) Then
            'Removal of EnPassant.
            State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(State.EnPassant Mod 8)
            State.EnPassant = 0US
        End If
        'Changes the player to move on the Zobrist Key.
        State.ZobristValue = State.ZobristValue Xor ZobristHashConstants(8)
    End Sub

    'Subroutine that makes, or un-makes, a Null Move on the board, for use by Null-Move Pruning (effectively changing the Zobrist Hash Key, for use by the Transposition Table).
    Private Sub ActNullMove(ByVal EnPassant As UInt16, ByRef ZobristValue As UInt64)
        'Removes EnPassant Privileges from the hash value.
        If EnPassant <> 0US Then ZobristValue = ZobristValue Xor ZobristHashConstants(EnPassant Mod 8US)
        'Changes the player to move on the Zobrist Key.
        ZobristValue = ZobristValue Xor ZobristHashConstants(8)
    End Sub





    'This function contains my NegaMax algorithm (ie: the main search algorithm). Current Optimisations & Improvements:
    '• Alpha-Beta Pruning.
    '• Quiescence.
    '• Transposition Table Pruning.
    '• Repetition Detection.
    '• Transposition Table Move Lookup (+ Prioritising).
    '• Search Extensions (in check).
    '• PVS Search.
    '• Null Move Pruning.
    '• Delta Pruning.
    '• Late Move Reductions.
    '• Internal Iterative Reductions.
    '• Killer Moves.
    Private Function NegaMax(ByRef State As BoardState, ByVal depth As Integer, ByVal NumDepthExt As Integer, ByVal isWhite As Boolean, ByVal MeKPos As UInt16, ByVal EnemyKPos As UInt16, ByVal Alpha As Int16, ByVal Beta As Int16, ByVal CanTakeNullMove As Boolean) As Int16
        If ABORT Then Return 0
        Dim SearchVars As NegaMaxSearchTools

        'Checks for darws via the 50-move rule. Note that I'm a little worried about this... Surely this will corrupt the Transposition Table entry of
        'this node's immediate parent, in case we reach here from a different branch? Apparently all the top engines don't care about this :O.
        If State.HalfMoveSize >= 100 AndAlso depth > 0 Then
            'We have hit the 50-move rule - this can only be overruled if we are in checkmate, so assuming that is not the case, we can safely return 0.
            SearchVars = CalibrateForMoveGeneration(State, MeKPos, EnemyKPos, isWhite)
            If SearchVars.CheckInfo <> 0US Then
                Dim FiftyMoveCount As Integer = CreateMoves(State, GlobalConstants.MaxTurnLegalMoves * DepthFromRoot, isWhite, SearchVars, MeKPos, EnemyKPos, True, DepthFromRoot, 0US)
                If FiftyMoveCount > 0 Then Return 0 'There is at least one valid move in the position - not in checkmate :D.
            Else
                'We have confirmed that we are not in check, and so can classify this position as a draw.
                Return 0
            End If
            'We must be in checkmate! Return the checkmate score.
            Return -30000S + CShort(DepthFromRoot)
        End If


        Dim TempTTEntry As TTEntry
        Dim BackupTTEntry As TTEntry
        'Hashes Zobrist key to find the location of the Entry in the Table.
        Dim EntryInTT As Integer = CInt(State.ZobristValue >> GlobalConstants.TranspositionTableSize)
        Dim ReplaceTTNode As Boolean

        'Searches for position in TranspositionTable.
        If TranspositionTable(EntryInTT).Key = State.ZobristValue Then TempTTEntry = TranspositionTable(EntryInTT)

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
                'Makes a backup of the entry.
                BackupTTEntry = TempTTEntry
                TempTTEntry.Flag = 4 'flags that the position is currently being searched on - if child nodes also detect this position,
                'then we return a draw (three-fold repetition).
                TempTTEntry.Depth = CSByte(depth)
                TranspositionTable(EntryInTT) = TempTTEntry
                ReplaceTTNode = True
            Else
                'Make sure recommended move isn't passed into the move generation code (as move may not be a capture move).
                TempTTEntry.BestMove = 0US
            End If

        ElseIf depth > 0 AndAlso (TranspositionTable(EntryInTT).Depth < depth OrElse TTGeneration - TranspositionTable(EntryInTT).Generation >= SearchSettings.TimeToLive) AndAlso SearchSettings.UseTranspositionTable Then
            'Creates a new TTEntry for the current position, as it could not be found in the Transposition Table.
            TempTTEntry.Generation = CByte(TTGeneration)
            TempTTEntry.Key = State.ZobristValue
            TempTTEntry.Flag = 4
            TempTTEntry.Depth = CSByte(depth)
            TranspositionTable(EntryInTT) = TempTTEntry
            ReplaceTTNode = True
        End If


        Dim CurrentMove, BestMove, StandPat As Int16
        Dim DepthExt, NoLegalMoves As Integer
        Dim NeedFullSearch As Boolean
        'Creates and forms the TFTable for the player to move. This subroutine will also flag for Minor & Major piece in the position.
        Dim PieceInPos As Boolean
        SearchVars = CalibrateForMoveGeneration(State, MeKPos, EnemyKPos, isWhite, PieceInPos)

        If Not (depth > 0 OrElse SearchVars.CheckInfo <> 0US) Then 'Quiescence mode activated.
            'Evaluation of board is the current move to beat.
            StandPat = Evaluate(State, isWhite, MeKPos, EnemyKPos)
            Alpha = Math.Max(Alpha, StandPat)
            If Beta <= Alpha Then Return StandPat 'Alpha-Beta Pruning.
            BestMove = StandPat
        Else
            'Null Move Pruning - if we are neither in check, nor in the late endgame (to avoid zugzwang), we force the current player to pass their turn to the
            'opponent, and search this new positon at a reduced depth. If this new position is *not* good enough to cause a Alpha-Beta cutoff (note that we
            'only pass in Beta) then we are clearly _much_ better than the opponent. Treat this as an Alpha-Beta cutoff.
            Dim NMPRValue As Integer = SearchSettings.NullMoveRValue + If(depth > 6, 1, 0) 'Don't eliminate too many nodes for a shallow search.
            If depth >= NMPRValue AndAlso CanTakeNullMove AndAlso SearchVars.CheckInfo = 0US AndAlso PieceInPos Then
                'If we are doing very well indeed, taking a Null Move is meaningless (already likely to fail-high).
                If Evaluate(State, isWhite, MeKPos, EnemyKPos) < Beta + GlobalConstants.PieceWeight.Pawn Then
                    Dim OldEnPassant As UInt16 = State.EnPassant
                    ActNullMove(State.EnPassant, State.ZobristValue)
                    State.EnPassant = 0US
                    DepthFromRoot += 1
                    'Turn CanTakeNullMove off for the next move, to prevent infinite null moves.
                    BestMove = -NegaMax(State, depth - NMPRValue, NumDepthExt, Not isWhite, EnemyKPos, MeKPos, -Beta, -Beta + 1S, False)
                    DepthFromRoot -= 1
                    'Undos the null move, which is just equivalent to taking another null move (via the properties of xor in Zobrist Hashing).
                    State.EnPassant = OldEnPassant
                    ActNullMove(State.EnPassant, State.ZobristValue)

                    If ABORT Then
                        'Resets the current position's entry in the Transposition Table (to prevent leftover "4"
                        'entries from affecting future searches), then exits search immediately. Note that if no node could be found, BackupTTEntry is just
                        'a blank entry.
                        If ReplaceTTNode Then TranspositionTable(EntryInTT) = BackupTTEntry
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
            If Not SearchSettings.StableSearch AndAlso SearchVars.CheckInfo <> 0US AndAlso NumDepthExt < SearchSettings.MaxDepthExt Then DepthExt = 1
        End If

        'Assumes Flag to be an Upper bound, unless proven otherwise.
        TempTTEntry.Flag = 2
        'Creates the legal moves for the chosen player. If Quiescence mode is activated then use capture moves only.
        Dim MoveBufferStrafe As Integer = GlobalConstants.MaxTurnLegalMoves * DepthFromRoot
        NoLegalMoves = CreateMoves(State, MoveBufferStrafe, isWhite, SearchVars, MeKPos, EnemyKPos, (depth > 0 OrElse SearchVars.CheckInfo <> 0US), DepthFromRoot, TempTTEntry.BestMove)

        If NoLegalMoves > 0 Then 'If any move exists...
            'Creates temp variables.
            Dim TempMeKPos As UInt16

            For n = MoveBufferStrafe To MoveBufferStrafe + NoLegalMoves - 1 'for each move...
                Dim Move As UInt16 = MoveBuffer(n)
                'Delta-Pruning in the Quiescence Search (capture moves).
                If Not (depth > 0 OrElse SearchVars.CheckInfo <> 0US) AndAlso (State.MaterialCountWhite >= 600 AndAlso State.MaterialCountBlack >= 600) Then
                    'We are not in the *late* endgame phase - intiate Delta-Pruning, as otherwise we might ignore ways to trade into a drawn endgame (eg: KN vs K).
                    Dim CapturedPieceValue As Integer
                    'Calculate the value of the piece we are capturing. Note that en-passant captures don't flag as 'capture moves', so we must manually add the value of the pawn.
                    CapturedPieceValue = If(Move >= 32768US, PieceValue(GetPieceIndexFromSquare(Move And 63US, State, Not isWhite)), GlobalConstants.PieceWeight.Pawn)
                    If (Move And 28672US) <> 0US Then
                        'Check for flags of pawn promotions. If this is the case, the promotion to a queen / knight _may_ be enough to overtake alpha.
                        If (Move And 28672US) = 4096US Then 'Queen Promotion.
                            CapturedPieceValue += GlobalConstants.PieceWeight.Queen
                        ElseIf (Move And 28672US) = 28672US Then 'Knight Promotion.
                            CapturedPieceValue += GlobalConstants.PieceWeight.Knight
                        End If
                    End If
                    'If the below IF passes, we assume it is impossible to exceed Alpha from the given position - reject this move.
                    If StandPat < Alpha - CapturedPieceValue - 200 Then Continue For
                End If

                'Copies board info to temp variables.
                DepthFromRoot += 1
                NegaMaxBoardStates(DepthFromRoot) = State
                TempMeKPos = MeKPos
                'Makes move on temp board, then calls NegaMax for this new position.
                MakeMove(Move, NegaMaxBoardStates(DepthFromRoot), isWhite, TempMeKPos)
                TotalPositionsSearched += 1UL

                If State.MaterialCountWhite = 0 AndAlso State.MaterialCountBlack = 0 Then
                    'Enforce draw by repetition.
                    HighestQuiescenceDepth = Math.Max(HighestQuiescenceDepth, DepthFromRoot)
                    CurrentMove = 0
                ElseIf Not SearchSettings.UseQuiescence AndAlso depth = 1 Then
                    'We have reached a leaf position - return the evaluation for this position.
                    CurrentMove = Evaluate(NegaMaxBoardStates(DepthFromRoot), isWhite, TempMeKPos, EnemyKPos) 'Evaluate position for opponent.
                Else 'No leaf node or drawn position (or are using Quiescence) - put position through NegaMax recursively.
                    HighestQuiescenceDepth = Math.Max(HighestQuiescenceDepth, DepthFromRoot)

                    If n = MoveBufferStrafe Then
                        'PVS Search: this is the first move - search it with a full window.
                        CurrentMove = -NegaMax(NegaMaxBoardStates(DepthFromRoot), depth + DepthExt - 1, NumDepthExt + DepthExt, Not isWhite, EnemyKPos, TempMeKPos, -Beta, -Alpha, True)
                    Else
                        'Late Move Reducitons & Internal Iterative Reductions - search everything but the first n moves at a reduced depth. If no hash move could be found, then the position
                        'is deemed 'more quiet', and so more moves are searched at a reduced depth.
                        'We disable this feature if there are no search extensions, as these are put into place when a position is deemed 'crutial' enough for a full search.
                        NeedFullSearch = True
                        If Not SearchSettings.StableSearch AndAlso depth >= 3 AndAlso DepthExt = 0 AndAlso (n - MoveBufferStrafe + If(TempTTEntry.BestMove = 0, 1, 2)) >= SearchSettings.ReductionThreshold Then
                            'We use a tightened Alpha-Beta window here, so that if any fail-high nodes then are detected and sent back up the tree instantly.
                            CurrentMove = -NegaMax(NegaMaxBoardStates(DepthFromRoot), depth - 2, NumDepthExt, Not isWhite, EnemyKPos, TempMeKPos, -Alpha - 1S, -Alpha, True)
                            If CurrentMove > Alpha Then
                                NoRepeatedSearches += 1
                            Else
                                NeedFullSearch = False
                            End If
                        End If
                        If NeedFullSearch Then
                            'We are in a non-PV node - search with a null window.
                            CurrentMove = -NegaMax(NegaMaxBoardStates(DepthFromRoot), depth + DepthExt - 1, NumDepthExt + DepthExt, Not isWhite, EnemyKPos, TempMeKPos, If(SearchSettings.UsePVS, -Alpha - 1S, -Beta), -Alpha, True)
                            If SearchSettings.UsePVS AndAlso CurrentMove > Alpha AndAlso CurrentMove < Beta Then
                                'The move was potentially better than the PV move, or caused a beta cutoff - make a full search.
                                CurrentMove = -NegaMax(NegaMaxBoardStates(DepthFromRoot), depth + DepthExt - 1, NumDepthExt + DepthExt, Not isWhite, EnemyKPos, TempMeKPos, -Beta, -Alpha, True)
                            End If
                        End If
                    End If
                End If
                DepthFromRoot -= 1

                ''For debugging...
                'If TERMINATED Then
                '    Console.WriteLine("~~~ Start of Depth " & depth & " ~~~")
                '    OutputBoardToConsole(Board)
                '    Console.CursorTop -= 1
                '    OutputBitMoveToConsole(PieceMoves(n), "Move Made in Pos: ")
                '    Console.WriteLine("In Null Move: " & Not CanTakeNullMove)
                '    Console.WriteLine("~~~~ End of Depth " & depth & " ~~~~" & vbCrLf)
                '    Return 0
                'End If

                If ABORT Then
                    'Resets the current position's entry in the Transposition Table (to prevent leftover "4"
                    'entries from affecting future searches), then exits search immediately.
                    If ReplaceTTNode Then TranspositionTable(EntryInTT) = BackupTTEntry
                    Return 0
                End If

                If CurrentMove > BestMove Then
                    BestMove = CurrentMove 'Best Move has been beaten - replace it.

                    If ReplaceTTNode Then
                        'Updates the best move in the Transposition Table entry to match this new, best move.
                        TempTTEntry.BestMove = Move
                        'We now have proof that the move is not an upper bound, as the move was able to produce
                        'a valid evaluation - replace it with 'exact'.
                        'Alfie Note 3/8/23: Fun Fact! The below IF statement (checking against Alpha) seems to 
                        'eliminate (as far as I can tell) all of the instability I've been having in my Transposition Table.
                        'I've been bug hunting for this for like 6 FREAKING MONTHS AND ALL IT TOOK WAS ONE STUPID IF CHECK!?!?!?
                        'I would be dancing around my room with excitement, but I'm mostly just angry that it took me this
                        'long to figure this out lmaoo. Still pretty happy & relieved, though :DD.
                        If BestMove > Alpha Then TempTTEntry.Flag = 0
                    End If

                    Alpha = Math.Max(Alpha, BestMove) 'Alpha = best move found for player.
                    If Beta <= Alpha Then 'Move was too strong for player; opponent will not choose this branch.
                        If depth > 0 AndAlso Move < 32768US AndAlso KillerMoves(2 * DepthFromRoot) <> Move Then
                            'The pruned move is not a capture move - add move to KillerMoves(), in the hope that the move
                            'is also possible in sibling positions. If this move is detected, it is searched earlier.
                            Dim KillerIndex As Integer = 2 * DepthFromRoot
                            KillerMoves(KillerIndex + 1) = KillerMoves(KillerIndex)
                            KillerMoves(KillerIndex) = Move
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
            Next
        End If

        If Math.Abs(BestMove) >= 29500 Then
            If NoLegalMoves = 0 Then
                'No legal move found for the player.
                If SearchVars.CheckInfo <> 0US Then
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
            TempTTEntry.Score = BestMove
        End If

        'Search completed without Alpha-beta cutoff - store position in Transposition Table.
        If ReplaceTTNode Then TranspositionTable(EntryInTT) = TempTTEntry

        'Return the best move's score found this iteration.
        Return BestMove
    End Function



    'This algorithm is used to condense a board position into an evaluation score, used to determine best moves.
    'We take into account the difference in material between the two sides, along with a heuristic to help
    'the AI find checkmated in simple endgame positions.
    Private Function Evaluate(ByRef State As BoardState, ByVal isWhite As Boolean, ByVal MeKPos As UInt16, ByVal EnemyKPos As UInt16) As Int16
        'Overload for the Evaluation function, for use in NegaMax.
        If isWhite Then
            Return Evaluate(State, MeKPos, EnemyKPos)
        Else
            Return -Evaluate(State, EnemyKPos, MeKPos) '- as '-1' is good for black, but bad for white.
        End If
    End Function
    Private Function Evaluate(ByRef State As BoardState, ByVal WKPos As UInt16, ByVal BKPos As UInt16) As Int16
        'Finds difference in material between both sides.
        Dim Score As Int32 = State.MaterialCountWhite - State.MaterialCountBlack

        'Calculates the score from the Piece Heat Maps, using the incremental score calculated in the tree.
        'Note that if the material count for either player drops below 16, the PHM tables merge towards an endgame state.
        'Thus, if this state occurs, we need to recalculate the values from scratch and then use these values (note that we still calculate
        'PHM values in case of pawn promotion, raising material count above 16 again).
        If SearchSettings.UsePieceHeatMaps Then
            'Calculates which player, if any, needs a PHM recalculation.
            Dim PHMCalculateCode As Integer = 3
            If State.MaterialCountWhite < 1600 Then PHMCalculateCode = 2
            If State.MaterialCountBlack < 1600 Then PHMCalculateCode = If(PHMCalculateCode = 3, 1, 0)
            If PHMCalculateCode < 3 Then 'Player needs a recalculation.
                Dim PHMBuffer = GetPHMEval(State, WKPos, BKPos, PHMCalculateCode)
                Score += If(State.MaterialCountBlack < 1600, PHMBuffer.White, State.PHMValueWhite) - If(State.MaterialCountWhite < 1600, PHMBuffer.Black, State.PHMValueBlack)
            Else 'Use previously calculated score.
                Score += State.PHMValueWhite - State.PHMValueBlack
            End If
        End If

        'We check for past pawns, isolated pawns, and doubled pawns, using the pawn bit masks.
        'For each of these, we apply bonuses & penalties based on how far the pawn is away from promoting.
        If SearchSettings.EvaluatePawnStructure Then
            'Note that the Pawn Masks are constructed by shifting 1UL based on the pawn's position, s.t the h1 square is the first bit, the a1 square
            'is the 8th bit, and the a8 square is the last (64th) bit.
            Dim PawnPosition, PawnRank, PawnFile As Integer
            Dim ForwardMask, FileMaskCentre, FileMaskLeft, FileMaskRight As ULong
            Dim TempWhitePawnMask As ULong = State.BitboardPawnWhite
            Dim TempBlackPawnMask As ULong = State.BitboardPawnBlack
            While TempWhitePawnMask <> 0UL
                PawnPosition = BitOperations.TrailingZeroCount(TempWhitePawnMask) 'Returns the bit position of the next particle, s.t a value of 0 refers to
                'the a8 square, 8 refers to a7, 63 refers to h1.
                'Calcualates all squares in front of the pawn.
                PawnRank = 8 - (PawnPosition \ 8)
                PawnFile = PawnPosition Mod 8
                ForwardMask = ULong.MaxValue >> (8 * PawnRank)
                'Calculates all the squares on the same rank as the pawn in question, and the rank to the left & right.
                FileMaskCentre = &H101010101010101UL << PawnFile '&H0101010101010101UL (hex) represents all the 1s on the a file.
                FileMaskLeft = If(PawnFile = 0, 0UL, &H101010101010101UL << (PawnFile - 1))
                FileMaskRight = If(PawnFile = 7, 0UL, &H101010101010101UL << (PawnFile + 1))

                'There are no enemy pawns in the way of the pawn in question - it is a past pawn. Add a bonus.
                If ((ForwardMask And (FileMaskLeft Or FileMaskCentre Or FileMaskRight)) And State.BitboardPawnBlack) = 0UL Then Score += EvalPastPawnBonus(PawnRank)
                'There are no friendly pawns that can support the pawn in question - it is an isolated pawn. Add a penalty.
                If ((FileMaskLeft Or FileMaskRight) And State.BitboardPawnWhite) = 0UL Then Score -= EvalIsolatedPawnPenalty(PawnRank)
                'There is a friendly pawn in front of the pawn in question - it is a doubled pawn. Add a penalty.
                If (ForwardMask And FileMaskCentre And State.BitboardPawnWhite) <> 0UL Then Score -= EvalDoubledPawnPenalty

                'Removes the 1 in the Pawn Mask referring to the pawn in question, and moves on until we've tackled all the pawns.
                TempWhitePawnMask = (TempWhitePawnMask And (TempWhitePawnMask - 1UL))
            End While
            While TempBlackPawnMask <> 0UL
                PawnPosition = BitOperations.TrailingZeroCount(TempBlackPawnMask)
                PawnRank = 1 + (PawnPosition \ 8)
                ForwardMask = ULong.MaxValue << (8 * PawnRank)
                PawnFile = PawnPosition Mod 8
                FileMaskCentre = &H101010101010101UL << PawnFile
                FileMaskLeft = If(PawnFile = 0, 0UL, &H101010101010101UL << (PawnFile - 1))
                FileMaskRight = If(PawnFile = 7, 0UL, &H101010101010101UL << (PawnFile + 1))
                If ((ForwardMask And (FileMaskLeft Or FileMaskCentre Or FileMaskRight)) And State.BitboardPawnWhite) = 0UL Then Score -= EvalPastPawnBonus(PawnRank)
                If ((FileMaskLeft Or FileMaskRight) And State.BitboardPawnBlack) = 0UL Then Score += EvalIsolatedPawnPenalty(PawnRank)
                If (ForwardMask And FileMaskCentre And State.BitboardPawnBlack) <> 0UL Then Score += EvalDoubledPawnPenalty
                TempBlackPawnMask = (TempBlackPawnMask And (TempBlackPawnMask - 1UL))
            End While
        End If


        'If a player has little material left, we calculate the value of this endgame position for the opposing player, using the
        'Lookup Table. This value takes into consideration the distances between the kings, and how close the player's king is to
        'the edge of the board (and therefore how easy / difficult it will be to checkmate the player).
        If State.MaterialCountWhite <= 1400 Then
            'White has little material left. Penelise white's king from being at the edges of the board, and when white's king
            'is close to black's king.
            Score -= GetEEVTValue(WKPos, BKPos, State.MaterialCountWhite \ 100)
        End If
        If State.MaterialCountBlack <= 1400 Then
            'Black has little material left.
            Score += GetEEVTValue(BKPos, WKPos, State.MaterialCountBlack \ 100)
        End If

        Return CShort(Score)
    End Function

    'Function that calculates the Piece Heat Map Values for the board, then assigns it to an array for both white & black's score.
    Private Function GetPHMEval(ByRef State As BoardState, ByVal WKPos As UInt16, ByVal BKPos As UInt16, Optional ByVal CalculateCode As Integer = 0, Optional ByVal CalculateEndgameValues As Boolean = True) As (White As Integer, Black As Integer)
        'Note that CalculateCode is used to compute the PHM Values for: both sides (0), white only (1) and black only (2).
        Dim WhitePHMEval, BlackPHMEval As Integer

        'For each piece on the board, calculate how advantageously it is placed (using the PieceHeatSquares). Add this to the score.
        Dim TempPieceMap As UInt64
        If CalculateCode <> 2 Then
            Dim ScaledMaterialCountB As Integer = If(CalculateEndgameValues AndAlso State.MaterialCountBlack < 1600, State.MaterialCountBlack \ 100, 16)
            TempPieceMap = State.BitboardPawnWhite
            While TempPieceMap <> 0UL
                Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                WhitePHMEval += GetPHMValue(GlobalConstants.PieceIndex.Pawn, 0, Square, ScaledMaterialCountB)
                TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
            End While
            TempPieceMap = State.BitboardKnightWhite
            While TempPieceMap <> 0UL
                Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                WhitePHMEval += GetPHMValue(GlobalConstants.PieceIndex.Knight, 0, Square, ScaledMaterialCountB)
                TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
            End While
            TempPieceMap = State.BitboardBishopWhite
            While TempPieceMap <> 0UL
                Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                WhitePHMEval += GetPHMValue(GlobalConstants.PieceIndex.Bishop, 0, Square, ScaledMaterialCountB)
                TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
            End While
            TempPieceMap = State.BitboardRookWhite
            While TempPieceMap <> 0UL
                Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                WhitePHMEval += GetPHMValue(GlobalConstants.PieceIndex.Rook, 0, Square, ScaledMaterialCountB)
                TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
            End While
            TempPieceMap = State.BitboardQueenWhite
            While TempPieceMap <> 0UL
                Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                WhitePHMEval += GetPHMValue(GlobalConstants.PieceIndex.Queen, 0, Square, ScaledMaterialCountB)
                TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
            End While
            WhitePHMEval += GetPHMValue(GlobalConstants.PieceIndex.King, 0, WKPos, ScaledMaterialCountB)
        End If
        If CalculateCode <> 1 Then
            Dim ScaledMaterialCountW As Integer = If(CalculateEndgameValues AndAlso State.MaterialCountWhite < 1600, State.MaterialCountWhite \ 100, 16)
            TempPieceMap = State.BitboardPawnBlack
            While TempPieceMap <> 0UL
                Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                BlackPHMEval += GetPHMValue(GlobalConstants.PieceIndex.Pawn, 1, Square, ScaledMaterialCountW)
                TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
            End While
            TempPieceMap = State.BitboardKnightBlack
            While TempPieceMap <> 0UL
                Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                BlackPHMEval += GetPHMValue(GlobalConstants.PieceIndex.Knight, 1, Square, ScaledMaterialCountW)
                TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
            End While
            TempPieceMap = State.BitboardBishopBlack
            While TempPieceMap <> 0UL
                Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                BlackPHMEval += GetPHMValue(GlobalConstants.PieceIndex.Bishop, 1, Square, ScaledMaterialCountW)
                TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
            End While
            TempPieceMap = State.BitboardRookBlack
            While TempPieceMap <> 0UL
                Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                BlackPHMEval += GetPHMValue(GlobalConstants.PieceIndex.Rook, 1, Square, ScaledMaterialCountW)
                TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
            End While
            TempPieceMap = State.BitboardQueenBlack
            While TempPieceMap <> 0UL
                Dim Square As UInt16 = CUShort(BitOperations.TrailingZeroCount(TempPieceMap))
                BlackPHMEval += GetPHMValue(GlobalConstants.PieceIndex.Queen, 1, Square, ScaledMaterialCountW)
                TempPieceMap = TempPieceMap And (TempPieceMap - 1UL)
            End While
            BlackPHMEval += GetPHMValue(GlobalConstants.PieceIndex.King, 1, BKPos, ScaledMaterialCountW)
        End If

        Return (WhitePHMEval, BlackPHMEval)
    End Function

    'Catch ex As Exception
    '    Console.WriteLine("oops")
    '    OutputBoardToConsole(Board)
    '    TERMINATED = True
    'End Try

End Class
