Imports System
Imports System.ComponentModel
Imports System.Drawing
Imports System.IO
Imports System.Net.Mime.MediaTypeNames
Imports System.Reflection.Metadata.Ecma335
Imports System.Runtime.CompilerServices
Imports System.Text
Imports System.Threading

Imports AIPlayer1
Imports AIPlayer2

Module Program
    Private Player1Codename As String
    Private Player2Codename As String

    Private Player1 As AIPlayer1.AI
    Private Player2 As AIPlayer2.AI

    Private AI1Settings As New AIPlayer1.AISearchSettings()
    Private AI2Settings As New AIPlayer2.AISearchSettings()

    Private CurrentPlayerOne, Player1White As Boolean
    Private StartingDepth As Integer
    Private CurrentAIDepth As Integer
    Private PreviousDepthBestMove1 As AIPlayer1.Move
    Private PreviousDepthBestMove2 As AIPlayer2.Move
    Private GameStopwatch As New Stopwatch
    Private AIStopwatch As New Stopwatch
    Private HasCompletedMove As Boolean

    Private ResultMap() As Integer

    Private BoardHistory1 As New AIPlayer1.GameHistory
    Private BoardHistory2 As New AIPlayer2.GameHistory
    'Private BoardPositionCache As List(Of Char(,))
    Private GameInvalid As Boolean = False
    Private TimeExceeded As Boolean
    Private TimePerMove As Decimal


    'NOTE: IF YOU CHANGE THE AI FILES, MAKE SURE TO CLEAN THE SOLUTION AFTERWARDS!!!!!!
    Private Sub AdjustIndividualAISettings()
        Player1Codename = Player1.GetVersion()
        'AI1Settings.UseTranspositionTable = True
        'AI1Settings.StableSearch = True
        'AI1Settings.AspirationWindowWidth = 0
        'AI1Settings.UseBitMasks = False
        'AI1Settings.UsePVS = False
        'AI1Settings.AspirationWindowWidth = 35

        Player2Codename = Player2.GetVersion()
        'AI2Settings.UseTranspositionTable = False
        'AI2Settings.StableSearch = True
        'AI2Settings.MoveReductionThreshold = 1000
        'AI2Settings.AspirationWindowWidth = 0
        'AI2Settings.UseBitMasks = False
        'AI2Settings.UsePVS = False
        'AI2Settings.AspirationWindowWidth = 35
    End Sub


    Sub Main()
        Console.OutputEncoding = Encoding.UTF8
        Console.ForegroundColor = ConsoleColor.White
        Console.WriteLine("Welcome To this Tournament, In which we will pit together two versions Of Alfie Kunz's Chess Game & AI!")
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.Write("Please input how many games you want to play: ")
        Console.ForegroundColor = ConsoleColor.White
        Dim NoMatches As Integer = CInt(Console.ReadLine())
        Console.ForegroundColor = ConsoleColor.Cyan
        Console.Write("And how long will each player search per move (in milli-seconds)? ")
        Console.ForegroundColor = ConsoleColor.White
        TimePerMove = CDec(Console.ReadLine()) / 1000
        Console.ForegroundColor = ConsoleColor.Blue
        Console.WriteLine("Accepted. Setting up...")

        Player1 = New AIPlayer1.AI()
        Player2 = New AIPlayer2.AI()
        AdjustIndividualAISettings()
        AI1Settings.OutputToConsole = False
        AI2Settings.OutputToConsole = False
        Player1.ConfigureSettings(AI1Settings, False)
        Player2.ConfigureSettings(AI2Settings, False)

        Console.ForegroundColor = ConsoleColor.Green
        Console.Write("Successfully Loaded AIs: ")
        Console.ForegroundColor = ConsoleColor.White
        Console.Write(Player1Codename)
        Console.ForegroundColor = ConsoleColor.Green
        Console.Write(" vs ")
        Console.ForegroundColor = ConsoleColor.White
        Console.Write(Player2Codename)
        Console.ForegroundColor = ConsoleColor.Green
        Console.WriteLine(".")

        Dim Player1Legacy As Boolean = CDec(Player1.GetVersion().Substring(1)) < 7.0
        Dim Player2Legacy As Boolean = CDec(Player2.GetVersion().Substring(1)) < 7.0
        If Player1Legacy OrElse Player2Legacy Then
            Console.ForegroundColor = ConsoleColor.Magenta
            If Player1Legacy AndAlso Player2Legacy Then
                Console.WriteLine("Neither AI can track Three-Fold Repetition: Loading Zobrist Values and Detecting Locally...")
                TrackRepetitions = True
                InitZobristValues()
                Player1UseLegacyMode = True
                Player2UseLegacyMode = True
                For n = 0 To 3
                    Player1LegacyAI(n) = New AIPlayer1.AI
                    Player2LegacyAI(n) = New AIPlayer2.AI
                Next
                Console.Write("Note: Both AI Models use")
            Else
                Console.Write("Note: AI ")
                Console.ForegroundColor = ConsoleColor.White
                If Player1Legacy Then
                    Player1UseLegacyMode = True
                    For n = 0 To 3
                        Player1LegacyAI(n) = New AIPlayer1.AI
                    Next
                    Console.Write(Player1Codename)
                Else
                    Player2UseLegacyMode = True
                    For n = 0 To 3
                        Player2LegacyAI(n) = New AIPlayer2.AI
                    Next
                    Console.Write(Player2Codename)
                End If
                Console.ForegroundColor = ConsoleColor.Magenta
                Console.Write(" uses")
            End If
            Console.WriteLine(" a Legacy 'Multiple-Depth Multithreading' Design: Performance may Differ Slightly...")
        End If

        'Console.WriteLine()
        'OutputWinRatios(250, 145, 35)
        'Console.ForegroundColor = ConsoleColor.White
        'Console.WriteLine("Thank you so much for using this program - have a wonderful day! :D" & vbCrLf)
        'Console.ReadLine()

        'Loads positions.
        Console.ForegroundColor = ConsoleColor.Blue
        Console.WriteLine("Loading Training Positions...")
        Dim StartPositions((NoMatches + 1) \ 2 - 1) As String
        Using SR As New StreamReader(AppDomain.CurrentDomain.BaseDirectory & "\Assets\OpeningPositions.txt")
            Dim line As String
            Dim Counter As Integer
            While Counter < StartPositions.Count
                line = SR.ReadLine()
                If line Is Nothing Then
                    Console.ForegroundColor = ConsoleColor.Red
                    Console.WriteLine("End of Stream Reached - Terminating List...")
                    Array.Resize(StartPositions, Counter)
                    NoMatches = Counter * 2
                    Exit While
                End If
                StartPositions(Counter) = line.Trim()
                Counter += 1
            End While
        End Using
        ReDim ResultMap(NoMatches)

        Console.ForegroundColor = ConsoleColor.Green
        Console.WriteLine("Setup Complete. Ready to Play!" & vbCrLf & vbCrLf)

        Player1White = True
        Dim WinCount1, WinCount2 As Integer
        Dim NoGamesInvalid As Integer = 0
        Dim TournamentTime As TimeSpan
        For n As Integer = 1 To NoMatches
            Console.ForegroundColor = ConsoleColor.Blue
            Console.Write("Commensing Game " & n & " of " & NoMatches & ": ")
            Console.ForegroundColor = If(Player1White, ConsoleColor.White, ConsoleColor.DarkGray)
            Console.Write(Player1Codename)
            Console.ForegroundColor = ConsoleColor.Blue
            Console.Write(" vs ")
            Console.ForegroundColor = If(Player1White, ConsoleColor.DarkGray, ConsoleColor.White)
            Console.Write(Player2Codename)
            Console.ForegroundColor = ConsoleColor.Blue
            Console.WriteLine("..." & vbCrLf)


            GameStopwatch.Restart()
            CurrentPlayerOne = Player1White
            Dim GameResult As String = PlayGame(StartPositions((n - 1) Mod CInt(Math.Ceiling(NoMatches / 2))))
            GameStopwatch.Stop()
            TournamentTime += GameStopwatch.Elapsed

            If GameInvalid Then
                NoGamesInvalid += 1
                GameInvalid = False
            Else
                Console.ForegroundColor = ConsoleColor.Blue
                Console.WriteLine(vbCrLf & "Game Completed in " & Math.Round(GameStopwatch.Elapsed.TotalSeconds) & " Seconds. Result:")
                If GameResult = "Win" Then
                    Console.ForegroundColor = ConsoleColor.Green
                    Console.WriteLine("Win for " & Player1Codename & ".")
                    Console.ForegroundColor = ConsoleColor.Red
                    Console.WriteLine("Loss for " & Player2Codename & ".")
                    WinCount1 += 1
                    ResultMap((n - 1) Mod CInt(Math.Ceiling(NoMatches / 2))) += 1
                ElseIf GameResult = "Loss" Then
                    Console.ForegroundColor = ConsoleColor.Red
                    Console.WriteLine("Loss for " & Player1Codename & ".")
                    Console.ForegroundColor = ConsoleColor.Green
                    Console.WriteLine("Win for " & Player2Codename & ".")
                    WinCount2 += 1
                    ResultMap((n - 1) Mod CInt(Math.Ceiling(NoMatches / 2))) -= 1
                ElseIf GameResult = "Draw" Then
                    Console.ForegroundColor = ConsoleColor.Gray
                    Console.WriteLine("Draw.")
                Else
                    Console.ForegroundColor = ConsoleColor.DarkRed
                    Console.Write("ERROR IN COMPLETION OF GAME. Aborting...")
                    NoMatches = n - 1
                    Exit For
                End If
            End If
            Console.WriteLine(vbCrLf)

            If Player1White AndAlso n >= NoMatches / 2 AndAlso NoMatches <> 1 Then
                Console.ForegroundColor = ConsoleColor.Magenta
                Console.WriteLine("Half the number of games have been played - Here are the scores so far!")
                OutputWinRatios(n, WinCount1, WinCount2, NoGamesInvalid, False)
                Console.ForegroundColor = ConsoleColor.DarkYellow
                Console.WriteLine($"Current Simulation Time: {TournamentTime.Hours} hrs, {TournamentTime.Minutes} mins, {TournamentTime.Seconds} secs.")
                Console.ForegroundColor = ConsoleColor.Magenta
                Console.WriteLine("Repeating all positions with colours swapped..." & vbCrLf & vbCrLf)
                Player1White = False
            ElseIf Console.KeyAvailable AndAlso n < NoMatches Then
                'Pauses Simulation on space.
                If Console.ReadKey(True).Key = ConsoleKey.Spacebar Then
                    Console.ForegroundColor = ConsoleColor.White
                    Console.WriteLine("Simulation Paused.")
                    Console.ForegroundColor = ConsoleColor.DarkYellow
                    Console.WriteLine($"Current Time       : {TournamentTime.Hours} hrs, {TournamentTime.Minutes} mins, {TournamentTime.Seconds} secs.")
                    Dim EstimatedTime As TimeSpan = (TournamentTime / n) * (NoMatches - n)
                    Console.WriteLine($"Estimated Time Left: {EstimatedTime.Hours} hrs, {EstimatedTime.Minutes} mins, {EstimatedTime.Seconds} secs.")
                    OutputWinRatios(n, WinCount1, WinCount2, NoGamesInvalid, False)
                    Console.ForegroundColor = ConsoleColor.White
                    Console.WriteLine("Press SPACE to Resume the Tournament...")
                    Thread.Sleep(1500)
                    While Console.KeyAvailable
                        Console.ReadKey() 'Clears all buffered key presses.
                    End While
                    'Resumes simulation next space press.
                    Do
                        Thread.Sleep(200)
                    Loop Until Console.KeyAvailable AndAlso Console.ReadKey(True).Key = ConsoleKey.Spacebar
                    Console.WriteLine(vbCrLf)
                End If
            End If
        Next



        OutputWinRatios(NoMatches, WinCount1, WinCount2, NoGamesInvalid, True)
        Console.ForegroundColor = ConsoleColor.DarkYellow
        Console.WriteLine($"Total Simulation Time: {TournamentTime.Hours} hrs, {TournamentTime.Minutes} mins, {TournamentTime.Seconds} secs.")
        Console.ForegroundColor = ConsoleColor.White
        Console.WriteLine("Thank you so much for using this program - have a wonderful day! :D" & vbCrLf)

        Console.ReadLine()
    End Sub

    Private Sub OutputWinRatios(ByVal NoMatches As Integer, ByVal WinCount1 As Integer, ByVal WinCount2 As Integer, Optional ByVal NoGamesInvalid As Integer = 0, Optional ByVal TournamentEnded As Boolean = True)
        NoMatches -= NoGamesInvalid
        If TournamentEnded Then
            Console.ForegroundColor = ConsoleColor.Blue
            Console.WriteLine("Tournament Complete! " & NoMatches & " Games Successfully Played. The Results are in:")
        End If
        Dim NoDraws As Integer = NoMatches - (WinCount1 + WinCount2)
        Dim Player1WinPercentage As Double = WinCount1 * 100 / NoMatches
        Dim Player2WinPercentage As Double = WinCount2 * 100 / NoMatches
        Dim DrawPercentage As Double = NoDraws * 100 / NoMatches
        Dim LargestName As Integer = Math.Max(Player1Codename.Length, Player2Codename.Length)

        Console.ForegroundColor = ConsoleColor.White
        Console.Write(Player1Codename.PadRight(LargestName) & " : ")
        Console.ForegroundColor = ConsoleColor.Green
        Console.Write(WinCount1 & " Wins (" & Math.Round(Player1WinPercentage) & "%), ")
        Console.ForegroundColor = ConsoleColor.Red
        Console.Write(WinCount2 & " Losses (" & Math.Round(Player2WinPercentage) & "%), ")
        Console.ForegroundColor = ConsoleColor.Gray
        Console.Write(NoDraws & " Draws (" & Math.Round(DrawPercentage) & "%)")
        If TournamentEnded Then
            Console.Write(", ")
            Console.ForegroundColor = ConsoleColor.DarkGreen
            Dim OutclassCount As Integer
            For Each m In ResultMap
                If m = 2 Then OutclassCount += 1
            Next
            Console.Write(OutclassCount & " Outclasses (" & Math.Round(OutclassCount * 100 / NoMatches) & "%).")
        Else
            Console.Write(".")
        End If

        'Checks how long that line was, to find out how big of a progress bar we can make.
        Dim ProgressPips As Integer = If(Console.CursorLeft >= 84, 100, 50)
        Console.ForegroundColor = ConsoleColor.White
        Console.Write(vbCrLf & "[")
        For i = 1 To ProgressPips
            'assume 100
            If i <= Player1WinPercentage * If(ProgressPips = 100, 1, 0.5) Then
                Console.ForegroundColor = ConsoleColor.Green
            ElseIf i >= (100 - Player2WinPercentage) * If(ProgressPips = 100, 1, 0.5) Then
                Console.ForegroundColor = ConsoleColor.Red
            Else
                Console.ForegroundColor = ConsoleColor.Gray
            End If
            Console.Write("-")
        Next
        Console.ForegroundColor = ConsoleColor.White
        Console.WriteLine("]")

        Console.ForegroundColor = ConsoleColor.White
        Console.Write(Player2Codename.PadRight(LargestName) & " : ")
        Console.ForegroundColor = ConsoleColor.Green
        Console.Write(WinCount2 & " Wins (" & Math.Round(Player2WinPercentage) & "%), ")
        Console.ForegroundColor = ConsoleColor.Red
        Console.Write(WinCount1 & " Losses (" & Math.Round(Player1WinPercentage) & "%), ")
        Console.ForegroundColor = ConsoleColor.Gray
        Console.Write(NoDraws & " Draws (" & Math.Round(DrawPercentage) & "%)")
        If TournamentEnded Then
            Console.Write(", ")
            Console.ForegroundColor = ConsoleColor.DarkGreen
            Dim OutclassCount As Integer
            For Each m In ResultMap
                If m = -2 Then OutclassCount += 1
            Next
            Console.WriteLine(OutclassCount & " Outclasses (" & Math.Round(OutclassCount * 100 / NoMatches) & "%).")
        Else
            Console.WriteLine(".")
        End If

        Console.ForegroundColor = ConsoleColor.White
        Console.Write("[")
        For i = 1 To ProgressPips
            'assume 100
            If i <= Player2WinPercentage * If(ProgressPips = 100, 1, 0.5) Then
                Console.ForegroundColor = ConsoleColor.Green
            ElseIf i >= (100 - Player1WinPercentage) * If(ProgressPips = 100, 1, 0.5) Then
                Console.ForegroundColor = ConsoleColor.Red
            Else
                Console.ForegroundColor = ConsoleColor.Gray
            End If
            Console.Write("-")
        Next
        Console.ForegroundColor = ConsoleColor.White
        Console.WriteLine("]")

        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.Write("This makes ")
        If WinCount1 > WinCount2 Then
            Console.Write(Player1Codename)
        ElseIf WinCount1 < WinCount2 Then
            Console.Write(Player2Codename)
        Else
            Console.Write("Nobody :O")
        End If
        Console.WriteLine(" The Winner" & If(TournamentEnded, "!", " So Far!"))
        If NoGamesInvalid > 0 Then
            Console.ForegroundColor = ConsoleColor.Red
            Console.WriteLine(NoGamesInvalid & " Game(s) contained an error, and so were not included in the total.")
        End If
    End Sub

    Private Function PlayGame(ByVal InitialPosition As String) As String
        'InitialPosition = "3r3k/8/7p/51p1/4P1P1/7P/2q5/5K2 w - - 0 1"
        'BoardPositionCache = New List(Of Char(,))
        Player1.Reconfigure(InitialPosition, True)
        Player2.Reconfigure(InitialPosition, True)
        If TrackRepetitions Then
            Clear()
            HalfSize = 0
        Else
            BoardHistory1.Clear(Player1.GetZobristValue())
            BoardHistory2.Clear(Player2.GetZobristValue())
        End If
        HalfSize = 0
        'Note that the initial position is not saved to BoardHistory. Ahhhhhh I'm sure this will be fine lol.

        CurrentAIDepth = 0
        PreviousDepthBestMove1.Score = 0
        PreviousDepthBestMove2.Score = 0

        Dim ScreenLineBuffer As Integer
        Dim MovesPGN As New List(Of String)
        Dim CurrentPosition As String = InitialPosition
        Console.ForegroundColor = ConsoleColor.DarkYellow
        Console.WriteLine("Starting Position: " & InitialPosition & ". Moves:")

        While True
            Dim CurrentPlayer As String = If(CurrentPlayerOne, Player1Codename, Player2Codename)
            HasCompletedMove = False
            TimeExceeded = False

            'Updates the player's Transposition Table.
            If CurrentPlayerOne Then
                Player1.AddBoardHistory(BoardHistory1.GetZobristArray())
            Else
                Player2.AddBoardHistory(BoardHistory2.GetZobristArray())
            End If

            'Outputs the game's current board position, along with the current moves, to the screen.
            ScreenLineBuffer = 13
            Dim PGNBuffer As String = ""
            Dim Board(,) As Char = If(CurrentPlayerOne, Player1.GetBoard(), Player2.GetBoard())
            Dim ScoreToWrite As Double
            'BoardPositionCache.Add(Board)
            For y As Byte = 0 To 9
                Select Case y
                    Case < 8
                        'Outputs board row.
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
                                    Console.ForegroundColor = ConsoleColor.Gray
                                End If
                                Console.Write(Board(x, y))
                            End If
                        Next
                        Console.Write("  ")
                    Case 8
                        'Outputs the depth of the most recent search.
                        Console.ForegroundColor = ConsoleColor.Cyan
                        Console.Write(("Dept: " & Math.Min(CurrentAIDepth, 99)).PadRight(10))
                    Case 9
                        'Outputs Current Evaluation of the position.
                        Console.ForegroundColor = ConsoleColor.Cyan
                        ScoreToWrite = If(CurrentPlayerOne, PreviousDepthBestMove2.Score * If(Player1White, -1, 1), PreviousDepthBestMove1.Score * If(Player1White, 1, -1))
                        Console.Write("Ev: ")
                        Console.ForegroundColor = If(Player1White Xor CurrentPlayerOne, ConsoleColor.White, ConsoleColor.DarkGray)
                        Console.Write(FormatScore(ScoreToWrite).PadRight(6))
                End Select

                'Outputs PGN info.
                Console.ForegroundColor = ConsoleColor.DarkYellow
                PGNBuffer = ""
                For i = (10 * y) + 1 To Math.Min(10 * y + 10, MovesPGN.Count)
                    If i Mod 2 = 1 Then PGNBuffer &= (i \ 2) + 1 & ". "
                    PGNBuffer &= MovesPGN(i - 1) & " "
                Next
                Console.WriteLine(PGNBuffer.PadRight(CurrentPosition.Length + 14))
            Next

            'Outputs remaining PGN moves below the board position, if there are any left.
            If MovesPGN.Count > 100 Then
                PGNBuffer = ""
                ScreenLineBuffer += 1
                For i = 101 To MovesPGN.Count
                    If i Mod 2 = 1 Then PGNBuffer &= (i \ 2) + 1 & ". "
                    PGNBuffer &= MovesPGN(i - 1) & " "
                    Console.WriteLine(PGNBuffer.PadRight(CurrentPosition.Length + 24))
                    If i <> MovesPGN.Count Then
                        If ((i - 100) Mod 12 = 0) Then
                            ScreenLineBuffer += 1
                            PGNBuffer = ""
                        Else
                            Console.CursorTop -= 1
                        End If
                    End If
                Next
            End If

            Console.ForegroundColor = ConsoleColor.Magenta
            Console.WriteLine("Current Position: " & CurrentPosition & New String(" "c, 25))
            Console.WriteLine(New String(" "c, 45 + Math.Max(Player1Codename.Length, Player2Codename.Length)))
            Console.ForegroundColor = ConsoleColor.Blue
            Console.Write("Waiting on Move from ")
            If CurrentPlayerOne Then
                Console.ForegroundColor = ConsoleColor.White
                Console.Write(CurrentPlayer)
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.Write(CurrentPlayer)
            End If
            Console.ForegroundColor = ConsoleColor.Blue
            Console.Write("..." & New String(" "c, 20 + Math.Max(Player1Codename.Length, Player2Codename.Length)))
            Console.CursorLeft = 0

            Dim UseMultithreading = If(CurrentPlayerOne, Player1UseLegacyMode, Player2UseLegacyMode)
            Dim Tasks As New List(Of Task) With {
                        .Capacity = If(UseMultithreading, 6, 1)
                    }
            If UseMultithreading Then
                Tasks.Add(New Task(AddressOf InitialiseThread1))
                Tasks.Add(New Task(AddressOf InitialiseThread2))
                Tasks.Add(New Task(AddressOf InitialiseThread3))
                Tasks.Add(New Task(AddressOf InitialiseThread4))
                Tasks.Add(New Task(AddressOf InitialiseThread5))
                CalculateAbsoluteDepth(ScoreToWrite)
                LegacyAIMovedToHigherDepth(0) = False
                LegacyAIMovedToHigherDepth(1) = False
            Else
                Tasks.Add(New Task(AddressOf VariableAISearchHandler))
                StartingDepth = 2
            End If
            AIStopwatch.Restart()
            Tasks.All(Function(t As Task)
                          t.Start()
                          Return True
                      End Function)

            While Tasks.Any(Function(t As Task) Not t.IsCompleted)
                Thread.Sleep(5) 'Allows more processing time to be spent on the AIs.

                If AIStopwatch.ElapsedMilliseconds / 1000 > TimePerMove Then 'Time's up!
                    If HasCompletedMove Then
                        TimeExceeded = True
                        If CurrentPlayerOne Then
                            Player1.AbortSearch()
                            If Player1UseLegacyMode Then
                                For n = 0 To 3
                                    Player1LegacyAI(n).AbortSearch()
                                Next
                            End If
                        Else
                            Player2.ABORTSearch()
                            If Player2UseLegacyMode Then
                                For n = 0 To 3
                                    Player2LegacyAI(n).ABORTSearch()
                                Next
                            End If
                        End If
                    ElseIf Not TimeExceeded Then
                        Console.ForegroundColor = ConsoleColor.Red
                        Console.WriteLine(vbCrLf & "Warning: Player " & CurrentPlayer & " was unable to make their move on time - allocating more time...")
                        Console.ForegroundColor = ConsoleColor.Blue
                        Console.SetCursorPosition(Console.CursorLeft, Console.CursorTop - 2)
                        TimeExceeded = True

                        If CurrentPlayerOne AndAlso Player1UseLegacyMode Then
                            If LegacyAIMovedToHigherDepth(0) Then Player1LegacyAI(0).AbortSearch()
                            Player1LegacyAI(1).AbortSearch()
                            Player1.AbortSearch()
                            Player1LegacyAI(2).AbortSearch()
                            Player1LegacyAI(3).AbortSearch()
                        ElseIf Not CurrentPlayerOne AndAlso Player2UseLegacyMode Then
                            If LegacyAIMovedToHigherDepth(0) Then Player2LegacyAI(0).ABORTSearch()
                            Player2LegacyAI(1).ABORTSearch()
                            Player2.ABORTSearch()
                            Player2LegacyAI(2).ABORTSearch()
                            Player2LegacyAI(3).ABORTSearch()
                        End If
                    ElseIf If(CurrentPlayerOne, Player1UseLegacyMode, Player2UseLegacyMode) AndAlso AIStopwatch.ElapsedMilliseconds / 1500 > TimePerMove AndAlso StartingDepth > 4 Then
                        'The AI has spent WAY too much time searching: cut everything off, and force it to make a depth-2 search (if if isn't already working on at least a depth <3 search)
                        Console.ForegroundColor = ConsoleColor.Red
                        Console.WriteLine(vbCrLf & vbCrLf & "Warning: Player " & CurrentPlayer & " was unable to make their move on time (again) - forcing a 2-ply search...")
                        Console.ForegroundColor = ConsoleColor.Blue
                        Console.SetCursorPosition(Console.CursorLeft, Console.CursorTop - 3)
                        If CurrentPlayerOne Then
                            Player1LegacyAI(0).AbortSearch()
                            Player1LegacyAI(1).AbortSearch()
                            Player1.AbortSearch()
                            Player1LegacyAI(2).AbortSearch()
                            Player1LegacyAI(3).AbortSearch()
                        Else
                            Player2LegacyAI(0).ABORTSearch()
                            Player2LegacyAI(1).ABORTSearch()
                            Player2.ABORTSearch()
                            Player2LegacyAI(2).ABORTSearch()
                            Player2LegacyAI(3).ABORTSearch()
                        End If
                    End If
                End If
            End While
            AIStopwatch.Stop()

            If UseMultithreading Then
                If CurrentPlayerOne Then
                    If LegacyAIMovedToHigherDepth(1) AndAlso Player1LegacyAIBestMoves(6).Code <> "a" Then
                        PreviousDepthBestMove1 = Player1LegacyAIBestMoves(6)
                        CurrentAIDepth = StartingDepth + 4
                    ElseIf LegacyAIMovedToHigherDepth(0) AndAlso Player1LegacyAIBestMoves(5).Code <> "a" Then
                        PreviousDepthBestMove1 = Player1LegacyAIBestMoves(5)
                        CurrentAIDepth = StartingDepth + 3
                    ElseIf Player1LegacyAIBestMoves(4).Code <> "a" Then
                        PreviousDepthBestMove1 = Player1LegacyAIBestMoves(4)
                        CurrentAIDepth = StartingDepth + 2
                    ElseIf Player1LegacyAIBestMoves(3).Code <> "a" Then
                        PreviousDepthBestMove1 = Player1LegacyAIBestMoves(3)
                        CurrentAIDepth = StartingDepth + 1
                    ElseIf Player1LegacyAIBestMoves(2).Code <> "a" Then
                        PreviousDepthBestMove1 = Player1LegacyAIBestMoves(2)
                        CurrentAIDepth = StartingDepth
                    ElseIf Player1LegacyAIBestMoves(1).Code <> "a" Then
                        PreviousDepthBestMove1 = Player1LegacyAIBestMoves(1)
                        CurrentAIDepth = StartingDepth - 1
                    ElseIf Player1LegacyAIBestMoves(0).Code <> "a" Then
                        PreviousDepthBestMove1 = Player1LegacyAIBestMoves(0)
                        CurrentAIDepth = StartingDepth - 2
                    Else
                        'No AI have completed their search - start a new search at a depth of 2.
                        'Sets the result of this new to be the best move.
                        PreviousDepthBestMove1 = Player1.Search(2)
                        CurrentAIDepth = 2
                    End If
                Else
                    If LegacyAIMovedToHigherDepth(1) AndAlso Player2LegacyAIBestMoves(6).Code <> "a" Then
                        PreviousDepthBestMove2 = Player2LegacyAIBestMoves(6)
                        CurrentAIDepth = StartingDepth + 4
                    ElseIf LegacyAIMovedToHigherDepth(0) AndAlso Player2LegacyAIBestMoves(5).Code <> "a" Then
                        PreviousDepthBestMove2 = Player2LegacyAIBestMoves(5)
                        CurrentAIDepth = StartingDepth + 3
                    ElseIf Player2LegacyAIBestMoves(4).Code <> "a" Then
                        PreviousDepthBestMove2 = Player2LegacyAIBestMoves(4)
                        CurrentAIDepth = StartingDepth + 2
                    ElseIf Player2LegacyAIBestMoves(3).Code <> "a" Then
                        PreviousDepthBestMove2 = Player2LegacyAIBestMoves(3)
                        CurrentAIDepth = StartingDepth + 1
                    ElseIf Player2LegacyAIBestMoves(2).Code <> "a" Then
                        PreviousDepthBestMove2 = Player2LegacyAIBestMoves(2)
                        CurrentAIDepth = StartingDepth
                    ElseIf Player2LegacyAIBestMoves(1).Code <> "a" Then
                        PreviousDepthBestMove2 = Player2LegacyAIBestMoves(1)
                        CurrentAIDepth = StartingDepth - 1
                    ElseIf Player2LegacyAIBestMoves(0).Code <> "a" Then
                        PreviousDepthBestMove2 = Player2LegacyAIBestMoves(0)
                        CurrentAIDepth = StartingDepth - 2
                    Else
                        PreviousDepthBestMove2 = Player2.Search(2)
                        CurrentAIDepth = 2
                    End If
                End If

            End If
            If GameInvalid Then Return "-"

            Dim CurrentAIMove As String = If(CurrentPlayerOne, Player1.OutputMoveInfo(PreviousDepthBestMove1, True), Player2.OutputMoveInfo(PreviousDepthBestMove2, True))
            Console.Write("Move Received From ")
            If CurrentPlayerOne Then
                Console.ForegroundColor = ConsoleColor.White
                Console.Write(CurrentPlayer)
            Else
                Console.ForegroundColor = ConsoleColor.DarkGray
                Console.Write(CurrentPlayer)
            End If
            Console.ForegroundColor = ConsoleColor.Blue
            Console.WriteLine(" : " & CurrentAIMove & ". Playing Move..." & New String(" "c, 5))
            MovesPGN.Add(CurrentAIMove)

            If CurrentPlayerOne Then
                CurrentPosition = Player1.ReturnFENAfterMove(PreviousDepthBestMove1)
            Else
                CurrentPosition = Player2.ReturnFENAfterMove(PreviousDepthBestMove2)
            End If
            Player1.Reconfigure(CurrentPosition, False)
            Player2.Reconfigure(CurrentPosition, False)
            CurrentPlayerOne = Not CurrentPlayerOne

            Dim GameEnded As Char = EnforceEndStates(True, CurrentPosition)
            If GameEnded = "c" Then
                'A player has won the game.
                Return If(CurrentPlayerOne, "Loss", "Win")
            ElseIf GameEnded = "s" OrElse GameEnded = "d" Then
                'The game has ended in a draw.
                Return "Draw"
            End If
            PushPGN(CurrentAIMove)

            'Screen Buffer.
            Console.CursorTop = Math.Max(Console.CursorTop - ScreenLineBuffer, 0)

        End While
        Return ""
    End Function

    Public Function FormatScore(Score As Double) As String
        If Score = 0 Then
            FormatScore = "0.00"
        ElseIf Math.Abs(Score) >= 100 Then
            Dim MateDistance As Integer = CInt(Math.Round((299.99 + If(CurrentPlayerOne, 0.01, 0) - Math.Abs(Score)) * 100))
            FormatScore = If(Score > 0, "+", "-") & "M" & MateDistance.ToString()
        Else
            FormatScore = If(Score > 0, "+", "-") & Math.Abs(Score).ToString("0.0")
            If FormatScore.Length > 4 Then FormatScore = Math.Round(Score, 0, MidpointRounding.AwayFromZero).ToString("+#;-#")
        End If
    End Function



    'Subroutines controlling the AI thread. Variable Search = the AI starts at a depth of AIHandles.StartingDepth, and increments by one after each search (iterative deepening).
    'Fixed Search = the AI only performs one search, but at the specified depth that the user provided.
    Private Sub VariableAISearchHandler()
        Dim AICurrentMove1 As AIPlayer1.Move
        Dim AICurrentMove2 As AIPlayer2.Move
        CurrentAIDepth = StartingDepth
        Dim PreviousCode As Char
        Dim PreviousScore As Double
        While CurrentAIDepth < 100
            'Performs a new search using iterative deepening. For all searches apart from the first, feed the previous search's best
            'move into the new search. This is so that this previous best move can be searched first, resulting in more AlphaBeta prunes.
            Try
                If CurrentAIDepth = StartingDepth Then
                    If CurrentPlayerOne Then
                        AICurrentMove1 = Player1.Search(CurrentAIDepth)
                    Else
                        AICurrentMove2 = Player2.Search(CurrentAIDepth)
                    End If
                Else
                    If CurrentPlayerOne Then
                        AICurrentMove1 = Player1.Search(CurrentAIDepth, PreviousDepthBestMove1)
                    Else
                        AICurrentMove2 = Player2.Search(CurrentAIDepth, PreviousDepthBestMove2)
                    End If
                End If
            Catch ex As Exception
                Console.ForegroundColor = ConsoleColor.DarkRed
                Console.WriteLine("An error occured in the game: " & ex.ToString() & ". Neglecting game..." & vbCr)
                GameInvalid = True
                Exit While
            End Try


            HasCompletedMove = True

            If CurrentPlayerOne Then
                PreviousCode = AICurrentMove1.Code
                PreviousScore = AICurrentMove1.Score
            Else
                PreviousCode = AICurrentMove2.Code
                PreviousScore = AICurrentMove2.Score
            End If

            If PreviousCode = "a" Then
                CurrentAIDepth -= 1 'The search could not be completed - the previous depth is used instead.
                Exit While
            Else
                If CurrentPlayerOne Then
                    PreviousDepthBestMove1 = AICurrentMove1
                Else
                    PreviousDepthBestMove2 = AICurrentMove2
                End If
                If (CurrentPlayerOne AndAlso Player1.GetABORTState()) OrElse (Not CurrentPlayerOne AndAlso Player2.GetABORTState()) Then
                    'The time to search has expired - exit search process.
                    Exit While
                ElseIf Math.Abs(PreviousScore) >= 295 AndAlso CurrentAIDepth >= CInt(100 * (299.99 - Math.Abs(PreviousScore))) Then
                    'A checkmating pattern has been found, and we can assume that it is the fastest pattern (as, by enforcing the above Depth requirement,
                    'we know that the effects of search extensions has not produced a slower mate).
                    Exit While
                End If
                CurrentAIDepth += 1
            End If

        End While
    End Sub



    'Checks for Checkmate & Stalemate are calculated by running the opponent AI on a special mode, where the
    'AI runs to a depth of 1 but makes no moves on the board. This determines if the opponent actually has any
    'legal moves (or if there is only 1 forced move), and if they don't, then then the game will end. This
    'algorithm also checks for draws by insufficient material, three-fold repetition, or 50-move rule. If the
    'procedure does detect any of these end states, then it stops the game and/or notifies the user (depending on gamemode).
    Private Function EnforceEndStates(ByVal NormalMove As Boolean, ByVal NewPosition As String) As Char
        'Adds the new board position's hash value to the Game History.
        Dim LocalZobristValue As ULong
        If TrackRepetitions Then
            Dim Board(,) As Char = If(CurrentPlayerOne, Player1.GetBoard(), Player2.GetBoard())
            Dim WCKS, WCQS, BCKS, BCQS As Boolean
            Dim EnPassant As String = "-"
            Dim FEN As String = NewPosition
            FEN = Right(FEN, Len(FEN) - FEN.IndexOf(" ") - 3)
            For m = 0 To Len(FEN) - 1
                If FEN(m) = "K" AndAlso (Board(4, 7) = "K" AndAlso Board(7, 7) = "R") Then
                    WCKS = True
                ElseIf FEN(m) = "Q" AndAlso (Board(4, 7) = "K" AndAlso Board(0, 7) = "R") Then
                    WCQS = True
                ElseIf FEN(m) = "k" AndAlso (Board(4, 0) = "k" AndAlso Board(7, 0) = "r") Then
                    BCKS = True
                ElseIf FEN(m) = "q" AndAlso (Board(4, 0) = "k" AndAlso Board(0, 0) = "r") Then
                    BCQS = True
                ElseIf FEN(m) >= "a" AndAlso FEN(m) <= "h" Then
                    EnPassant = Asc(FEN.Substring(m, 2)(0)) - 97 & 8 - Val(FEN.Substring(m, 2)(1))
                End If
            Next
            LocalZobristValue = ZobristHashPosition(Board, Not (Player1White Xor CurrentPlayerOne), WCKS, WCQS, BCKS, BCQS, EnPassant)
            PushZobrist(LocalZobristValue)
        Else
            BoardHistory1.PushZobrist(Player1.GetZobristValue(), NormalMove)
            BoardHistory2.PushZobrist(Player2.GetZobristValue(), NormalMove)
        End If
        'NormalMove specifies if the move is a move that the user or the AI has made,
        'or whether it is a move formed by, for example, undoing the board position. Depending on which it is, GameHistory will store
        'the previous state to its buffer (allowing the state to be undone, if needed).

        Dim TempMoveCode As Char = If(CurrentPlayerOne, Player1.CheckForEndState().Code, Player2.CheckForEndState().Code) 'Checks for endstates in the current position.
        Select Case TempMoveCode
            Case "c"c 'Position is checkmate.
                Console.ForegroundColor = ConsoleColor.Red
                Console.WriteLine(vbCrLf & "The Game has Ended. Cause = Checkmate.")
                Return "c"c
            Case "s"c 'Position is stalemate.
                Console.ForegroundColor = ConsoleColor.Red
                Console.WriteLine(vbCrLf & "The Game has Ended. Cause = Stalemate.")
                Return "s"c
        End Select

        'Draw by insufficient material is then checked for. In principle, if it is physically impossible for one
        'player to checkmate the other (such as king vs king, or king vs king + a knight / bishop), then the
        'position is delared 'dead' and the game ends in a draw.
        Dim AIBishopWeight As Integer = If(CurrentPlayerOne, CInt(AIPlayer1.GlobalConstants.PieceWeight.Bishop), CInt(AIPlayer2.GlobalConstants.PieceWeight.Bishop))
        Dim MasterBoard(,) As Char = If(CurrentPlayerOne, Player1.GetBoard(), Player2.GetBoard())
        Dim TempMaterialCount As Integer() = If(CurrentPlayerOne, Player1.GetMaterialCount(), Player2.GetMaterialCount())
        If TempMaterialCount(0) + TempMaterialCount(1) = 0 Then 'only kings remain.
            Console.ForegroundColor = ConsoleColor.Red
            Console.WriteLine(vbCrLf & "The Game has Ended. Cause = Draw by Insufficient Material (K v K).")
            Return "d"c
        ElseIf TempMaterialCount(0) + TempMaterialCount(1) = AIBishopWeight Then 'could be king vs king + knight / bishop.
            For y = 0 To 7
                For x = 0 To 7
                    'scans for knights / bishops.
                    If UCase(MasterBoard(x, y)) = "B" OrElse UCase(MasterBoard(x, y)) = "N" Then
                        'Game ends in a draw.
                        Console.ForegroundColor = ConsoleColor.Red
                        Console.WriteLine(vbCrLf & "The Game has Ended. Cause = Draw by Insufficient Material (K v K+B/N).")
                        Return "d"c
                    End If
                Next
            Next
        ElseIf TempMaterialCount(0) = AIBishopWeight AndAlso TempMaterialCount(1) = AIBishopWeight Then 'only possibility is king + bishop vs king + bishop (of same type).
            Dim NoOfBishopsFound As Integer
            Dim BishopType As Boolean 'True = Light, False = Dark
            For y = 0 To 7
                For x = 0 To 7
                    If UCase(MasterBoard(x, y)) = "B" Then
                        Select Case NoOfBishopsFound
                            Case 0
                                'Calculates if the bishop is on a light square or a dark square.
                                If (x Mod 2 = 0 AndAlso y Mod 2 = 0) OrElse (x Mod 2 = 1 AndAlso y Mod 2 = 1) Then
                                    BishopType = True
                                Else
                                    BishopType = False
                                End If
                            Case 1
                                'If this bishop is on the same colour complex (light / dark) as the previous bishop,
                                'then we end the game.
                                If (x Mod 2 = 0 AndAlso y Mod 2 = 0 AndAlso BishopType) OrElse (x Mod 2 = 1 AndAlso y Mod 2 = 1 AndAlso BishopType) OrElse (x Mod 2 = 1 AndAlso y Mod 2 = 0 AndAlso Not BishopType) OrElse (x Mod 2 = 0 AndAlso y Mod 2 = 1 AndAlso Not BishopType) Then
                                    Console.ForegroundColor = ConsoleColor.Red
                                    Console.WriteLine(vbCrLf & "The Game has now Ended. Cause = Draw by Insufficient Material (K v K+B+B).")
                                    Return "d"c
                                End If
                        End Select
                        NoOfBishopsFound += 1
                    ElseIf Not (UCase(MasterBoard(x, y)) = "K" OrElse UCase(MasterBoard(x, y)) = " ") Then
                        'Another piece found - exit search.
                        NoOfBishopsFound = -1
                        Exit For
                    End If
                Next
                If NoOfBishopsFound = -1 Then Exit For
            Next
        End If

        'Checks for three-fold repetition and 50-move rule occurence, using BoardHistory arrays.
        Dim ZobristOccurences As Int16
        If TrackRepetitions Then
            ZobristOccurences = CheckNoOfZobristOccurances(LocalZobristValue)
        Else
            ZobristOccurences = If(CurrentPlayerOne, BoardHistory1.CheckNoOfZobristOccurances(Player1.GetZobristValue()), BoardHistory2.CheckNoOfZobristOccurances(Player2.GetZobristValue()))
        End If
        If ZobristOccurences = 3 OrElse HalfSize >= 100 Then
            'Position has ocurred three times - end game.
            Console.ForegroundColor = ConsoleColor.Red
            Console.Write(vbCrLf & "The Game has Ended. Cause = Draw by ")
            If HalfSize >= 100 Then
                Console.WriteLine("50 Move Rule.")
            Else
                Console.WriteLine("Three-fold Repetition.")
            End If
            Return "d"c
        End If

        Return TempMoveCode
    End Function





    Private TrackRepetitions As Boolean = False
    Private ReadOnly ZobristHashTable(9, 1, 7, 7) As UInt64
    Private ReadOnly HashConstants(4) As UInt64
    Private ReadOnly ZobristMain(2047) As UInt64
    Private MainSize As UInt16
    Private HalfSize As Integer
    Public Sub InitZobristValues()
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
    Public Function ZobristHashPosition(ByVal Board(,) As Char, ByVal isWhite As Boolean, ByVal WCKS As Boolean, ByVal WCQS As Boolean, ByVal BCKS As Boolean, ByVal BCQS As Boolean, ByVal EnPassant As String) As UInt64
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
        If WCKS Then ZobristHashPosition = ZobristHashPosition Xor HashConstants(1)
        If WCQS Then ZobristHashPosition = ZobristHashPosition Xor HashConstants(2)
        If BCKS Then ZobristHashPosition = ZobristHashPosition Xor HashConstants(3)
        If BCQS Then ZobristHashPosition = ZobristHashPosition Xor HashConstants(4)
        If EnPassant <> "-" Then ZobristHashPosition = ZobristHashPosition Xor ZobristHashTable(2, 0, Val(EnPassant(0)), Val(EnPassant(1)))
    End Function
    Public Sub Clear(Optional ByVal ZobristToResetTo As UInt64 = 0)
        MainSize = 0
        If ZobristToResetTo <> 0 Then PushZobrist(ZobristToResetTo, False)
    End Sub

    'Subroutine that adds a new Zobrist Hash to the GameHistory array.
    Public Sub PushZobrist(ByVal Value As UInt64, Optional ByVal MoveArray As Boolean = False)
        If MainSize < 2047 Then
            ZobristMain(MainSize) = Value
            MainSize += 1US
        Else
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when adding Position Data to GameHistory: Array Full.")
            Console.ForegroundColor = ConsoleColor.White
        End If
    End Sub

    'Function that checks how many times the input Zobrist Key is present in the GameHistory array (used to enforce three-fold repetition).
    Public Function CheckNoOfZobristOccurances(ByVal Value As UInt64) As Int16
        CheckNoOfZobristOccurances = 0
        For n = 0 To MainSize - 1
            If ZobristMain(n) = Value Then
                CheckNoOfZobristOccurances += 1S
                If CheckNoOfZobristOccurances = 3S Then Exit For 'There will never be >3 occurances, as if this were true then the game would end.
            End If
        Next
    End Function
    Public Sub PushPGN(ByVal Value As String, Optional ByVal MoveArray As Boolean = False)
        'If the move was not a pawn move or a capture, we increment the half-move counter. Otherwise, we reset it.
        If (Value(0) >= "a" AndAlso Value(0) <= "h") OrElse Value.IndexOf("x"c) >= 0 Then
            HalfSize = 0
        Else
            HalfSize += 1
        End If
    End Sub



    Private Player1UseLegacyMode As Boolean
    Private Player2UseLegacyMode As Boolean
    Private Player1LegacyAI(3) As AIPlayer1.AI
    Private Player2LegacyAI(3) As AIPlayer2.AI
    Private Player1LegacyAIBestMoves(6) As AIPlayer1.Move
    Private Player2LegacyAIBestMoves(6) As AIPlayer2.Move
    Private LegacyAIMovedToHigherDepth(1) As Boolean

    Private Sub CalculateAbsoluteDepth(ByVal CurrentEvaluation As Double)
        'Counts the material on the board.
        Dim TotalMaterial As Integer() = If(CurrentPlayerOne, Array.ConvertAll(Player1.CountMaterial(Player1.GetBoard()), Function(s) CInt(s)), Array.ConvertAll(Player2.CountMaterial(Player2.GetBoard()), Function(s) CInt(s)))
        Array.ConvertAll(Player1.CountMaterial(Player1.GetBoard()), Function(s) CInt(s))
        'TotalMaterial(0) = White's total material, TotalMaterial(1) = Black's total material.
        'If the user is using Quiescence, then we knock off 0.5 from the depth (as Quiescence is slightly slower).
        Dim DepthAlgorithm As Integer = CInt(-2 * Math.Log(TotalMaterial(0) + TotalMaterial(1) + 1, 4) + 10 + (-0.5))

        'If the previous AI search resulted in a forced Checkmate being found, the depth is limited to only the
        'depth that is required to achieve that Checkmate. This saves on a lot of unnecessary processing, as
        'forced Checkmates are unavoidable. However, the evaluation system was changed post v6.2, so this only works
        '_both_ AI use the legacy multithreading.
        If Math.Abs(Val(CurrentEvaluation)) > 900 AndAlso Player1UseLegacyMode AndAlso Player2UseLegacyMode Then
            StartingDepth = CurrentAIDepth - CInt(Math.Abs(Val(CurrentEvaluation)) - 999)
        Else
            StartingDepth = DepthAlgorithm
        End If
        StartingDepth = Math.Max(StartingDepth, 4)
    End Sub


    Private Sub InitialiseThread1()
        Dim PreviousScore As Decimal
        Dim PreviousCode As String
        Try
            If CurrentPlayerOne Then
                Player1LegacyAIBestMoves(0) = Player1LegacyAI(0).Search(StartingDepth - 2)
            Else
                Player2LegacyAIBestMoves(0) = Player2LegacyAI(0).Search(StartingDepth - 2)
            End If
        Catch ex As Exception
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("An error occured in the game: " & ex.ToString() & ". Neglecting game..." & vbCr)
            GameInvalid = True
            Exit Sub
        End Try

        HasCompletedMove = True
        If CurrentPlayerOne Then
            PreviousScore = CDec(Player1LegacyAIBestMoves(0).Score)
        Else
            PreviousScore = CDec(Player2LegacyAIBestMoves(0).Score)
        End If

        If Not TimeExceeded Then
            If Math.Abs(PreviousScore) > 900 Then
                'Checkmating sequence found - terminate all other AIs.
                If CurrentPlayerOne Then
                    Player1LegacyAI(1).AbortSearch()
                    Player1.AbortSearch()
                    Player1LegacyAI(2).AbortSearch()
                    Player1LegacyAI(3).AbortSearch()
                Else
                    Player2LegacyAI(1).ABORTSearch()
                    Player2.ABORTSearch()
                    Player2LegacyAI(2).ABORTSearch()
                    Player2LegacyAI(3).ABORTSearch()
                End If
            ElseIf AIStopwatch.ElapsedMilliseconds / 500 < TimePerMove Then
                'Starts a new search at a higher depth (if there is >50% left on the timer).
                LegacyAIMovedToHigherDepth(0) = True
                Try
                    If CurrentPlayerOne Then
                        Player1LegacyAIBestMoves(5) = Player1LegacyAI(0).Search(StartingDepth + 3)
                    Else
                        Player2LegacyAIBestMoves(5) = Player2LegacyAI(0).Search(StartingDepth + 3)
                    End If
                Catch ex As Exception
                    Console.ForegroundColor = ConsoleColor.DarkRed
                    Console.WriteLine("An error occured in the game: " & ex.ToString() & ". Neglecting game..." & vbCr)
                    GameInvalid = True
                    Exit Sub
                End Try
                If Not TimeExceeded Then
                    If CurrentPlayerOne Then
                        PreviousScore = CDec(Player1LegacyAIBestMoves(5).Score)
                        PreviousCode = Player1LegacyAIBestMoves(5).Code
                    Else
                        PreviousScore = CDec(Player2LegacyAIBestMoves(5).Score)
                        PreviousCode = Player2LegacyAIBestMoves(5).Code
                    End If
                    If PreviousCode <> "a" AndAlso Math.Abs(PreviousScore) > 900 Then
                        'Checkmating sequence found - terminate all other AIs.
                        If CurrentPlayerOne Then
                            Player1LegacyAI(1).AbortSearch()
                            Player1.AbortSearch()
                            Player1LegacyAI(2).AbortSearch()
                            Player1LegacyAI(3).AbortSearch()
                        Else
                            Player2LegacyAI(1).ABORTSearch()
                            Player2.ABORTSearch()
                            Player2LegacyAI(2).ABORTSearch()
                            Player2LegacyAI(3).ABORTSearch()
                        End If
                    End If
                End If
            End If
        End If
    End Sub
    Private Sub InitialiseThread2()
        Dim PreviousScore As Decimal
        Dim PreviousCode As String
        Try
            If CurrentPlayerOne Then
                Player1LegacyAIBestMoves(1) = Player1LegacyAI(1).Search(StartingDepth - 1)
            Else
                Player2LegacyAIBestMoves(1) = Player2LegacyAI(1).Search(StartingDepth - 1)
            End If
        Catch ex As Exception
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("An error occured in the game: " & ex.ToString() & ". Neglecting game..." & vbCr)
            GameInvalid = True
            Exit Sub
        End Try

        If CurrentPlayerOne Then
            PreviousScore = CDec(Player1LegacyAIBestMoves(1).Score)
            PreviousCode = Player1LegacyAIBestMoves(1).Code
        Else
            PreviousScore = CDec(Player2LegacyAIBestMoves(1).Score)
            PreviousCode = Player2LegacyAIBestMoves(1).Code
        End If
        If PreviousCode <> "a" Then
            HasCompletedMove = True
            If Math.Abs(PreviousScore) > 900 Then
                'Checkmating sequence found - terminate all other AIs.
                If CurrentPlayerOne Then
                    Player1LegacyAI(0).AbortSearch()
                    Player1.AbortSearch()
                    Player1LegacyAI(2).AbortSearch()
                    Player1LegacyAI(3).AbortSearch()
                Else
                    Player2LegacyAI(0).ABORTSearch()
                    Player2.ABORTSearch()
                    Player2LegacyAI(2).ABORTSearch()
                    Player2LegacyAI(3).ABORTSearch()
                End If
            ElseIf AIStopwatch.ElapsedMilliseconds / 500 < TimePerMove Then
                'Starts a new search at a higher depth (if there is >50% left on the timer).
                LegacyAIMovedToHigherDepth(1) = True
                Try
                    If CurrentPlayerOne Then
                        Player1LegacyAIBestMoves(6) = Player1LegacyAI(1).Search(StartingDepth + 4)
                    Else
                        Player2LegacyAIBestMoves(6) = Player2LegacyAI(1).Search(StartingDepth + 4)
                    End If
                Catch ex As Exception
                    Console.ForegroundColor = ConsoleColor.DarkRed
                    Console.WriteLine("An error occured in the game: " & ex.ToString() & ". Neglecting game..." & vbCr)
                    GameInvalid = True
                    Exit Sub
                End Try
                If Not TimeExceeded Then
                    If CurrentPlayerOne Then
                        PreviousScore = CDec(Player1LegacyAIBestMoves(6).Score)
                        PreviousCode = Player1LegacyAIBestMoves(6).Code
                    Else
                        PreviousScore = CDec(Player2LegacyAIBestMoves(6).Score)
                        PreviousCode = Player1LegacyAIBestMoves(6).Code
                    End If
                    If PreviousCode <> "a" AndAlso Math.Abs(PreviousScore) > 900 Then
                        'Checkmating sequence found - terminate all other AIs.
                        If CurrentPlayerOne Then
                            Player1LegacyAI(0).AbortSearch()
                            Player1.AbortSearch()
                            Player1LegacyAI(2).AbortSearch()
                            Player1LegacyAI(3).AbortSearch()
                        Else
                            Player2LegacyAI(0).ABORTSearch()
                            Player2.ABORTSearch()
                            Player2LegacyAI(2).ABORTSearch()
                            Player2LegacyAI(3).ABORTSearch()
                        End If
                    End If
                End If
            End If
        End If
    End Sub
    Private Sub InitialiseThread3()
        Dim PreviousScore As Decimal
        Dim PreviousCode As String
        Try
            If CurrentPlayerOne Then
                Player1LegacyAIBestMoves(2) = Player1.Search(StartingDepth)
            Else
                Player2LegacyAIBestMoves(2) = Player2.Search(StartingDepth)
            End If
        Catch ex As Exception
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("An error occured in the game: " & ex.ToString() & ". Neglecting game..." & vbCr)
            GameInvalid = True
            Exit Sub
        End Try

        If CurrentPlayerOne Then
            PreviousScore = CDec(Player1LegacyAIBestMoves(2).Score)
            PreviousCode = Player1LegacyAIBestMoves(2).Code
        Else
            PreviousScore = CDec(Player2LegacyAIBestMoves(2).Score)
            PreviousCode = Player2LegacyAIBestMoves(2).Code
        End If
        If PreviousCode <> "a" Then
            HasCompletedMove = True
            If Math.Abs(PreviousScore) > 900 Then
                'Checkmating sequence found - terminate all other AIs.
                If CurrentPlayerOne Then
                    Player1LegacyAI(0).AbortSearch()
                    Player1LegacyAI(1).AbortSearch()
                    Player1LegacyAI(2).AbortSearch()
                    Player1LegacyAI(3).AbortSearch()
                Else
                    Player2LegacyAI(0).ABORTSearch()
                    Player2LegacyAI(1).ABORTSearch()
                    Player2LegacyAI(2).ABORTSearch()
                    Player2LegacyAI(3).ABORTSearch()
                End If
            End If
        End If
    End Sub
    Private Sub InitialiseThread4()
        Dim PreviousScore As Decimal
        Dim PreviousCode As String
        Try
            If CurrentPlayerOne Then
                Player1LegacyAIBestMoves(3) = Player1LegacyAI(2).Search(StartingDepth + 1)
            Else
                Player2LegacyAIBestMoves(3) = Player2LegacyAI(2).Search(StartingDepth + 1)
            End If
        Catch ex As Exception
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("An error occured in the game: " & ex.ToString() & ". Neglecting game..." & vbCr)
            GameInvalid = True
            Exit Sub
        End Try

        If CurrentPlayerOne Then
            PreviousScore = CDec(Player1LegacyAIBestMoves(3).Score)
            PreviousCode = Player1LegacyAIBestMoves(3).Code
        Else
            PreviousScore = CDec(Player2LegacyAIBestMoves(3).Score)
            PreviousCode = Player2LegacyAIBestMoves(3).Code
        End If
        If PreviousCode <> "a" Then
            HasCompletedMove = True
            If Math.Abs(PreviousScore) > 900 Then
                'Checkmating sequence found - terminate all other AIs.
                If CurrentPlayerOne Then
                    Player1LegacyAI(0).AbortSearch()
                    Player1LegacyAI(1).AbortSearch()
                    Player1.AbortSearch()
                    Player1LegacyAI(3).AbortSearch()
                Else
                    Player2LegacyAI(0).ABORTSearch()
                    Player2LegacyAI(1).ABORTSearch()
                    Player2.ABORTSearch()
                    Player2LegacyAI(3).ABORTSearch()
                End If
            End If
        End If
    End Sub
    Private Sub InitialiseThread5()
        Dim PreviousScore As Decimal
        Dim PreviousCode As String
        Try
            If CurrentPlayerOne Then
                Player1LegacyAIBestMoves(4) = Player1LegacyAI(3).Search(StartingDepth + 2)
            Else
                Player2LegacyAIBestMoves(4) = Player2LegacyAI(3).Search(StartingDepth + 2)
            End If
        Catch ex As Exception
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("An error occured in the game: " & ex.ToString() & ". Neglecting game..." & vbCr)
            GameInvalid = True
            Exit Sub
        End Try

        If CurrentPlayerOne Then
            PreviousScore = CDec(Player1LegacyAIBestMoves(4).Score)
            PreviousCode = Player1LegacyAIBestMoves(4).Code
        Else
            PreviousScore = CDec(Player2LegacyAIBestMoves(4).Score)
            PreviousCode = Player2LegacyAIBestMoves(4).Code
        End If
        If PreviousCode <> "a" Then
            HasCompletedMove = True
            If Math.Abs(PreviousScore) > 900 Then
                'Checkmating sequence found - terminate all other AIs.
                If CurrentPlayerOne Then
                    Player1LegacyAI(0).AbortSearch()
                    Player1LegacyAI(1).AbortSearch()
                    Player1.AbortSearch()
                    Player1LegacyAI(2).AbortSearch()
                Else
                    Player2LegacyAI(0).ABORTSearch()
                    Player2LegacyAI(1).ABORTSearch()
                    Player2.ABORTSearch()
                    Player2LegacyAI(2).ABORTSearch()
                End If
            End If
        End If
    End Sub


End Module
