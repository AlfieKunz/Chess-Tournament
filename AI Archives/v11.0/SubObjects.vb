Option Strict On

Imports System.Runtime.CompilerServices
Imports System.Windows.Forms.VisualStyles.VisualStyleElement.ProgressBar
Imports Microsoft.VisualBasic.ApplicationServices


'Class holding all the constants that my program needs - can be accessed by all classes.
Public Class GlobalConstants
    Public Const StartingFENPosition As String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    Public Const ProgramName As String = "Chess Game & Artificial Intelligence" 'also known as 'chessbot 9000' - thanks stroganoff <3
    Public Const ProgramVersion As String = "v11.0"
    Public Shared ReadOnly StartupPath As String = (AppDomain.CurrentDomain.BaseDirectory).TrimEnd("\"c)

    Public Const TranspositionTableSize As Integer = 26 'Constant referring to how large the TranspositionTable object is.
    'Used to determine how much to scate the ZobristValue by.

    'Structure holding the relative weights of all the pieces on the board.
    Public Structure PieceWeight
        Public Const Pawn As Integer = 100
        Public Const Knight As Integer = 300
        Public Const Bishop As Integer = 300
        Public Const Rook As Integer = 500
        Public Const Queen As Integer = 900
        Public Const King As Integer = 1000 'No meaning to this value, other than to de-prioritise king captures.
    End Structure

    'Structure holding the unique identifier for each piece type. Used for indexing into PieceValue, MVVLVAValues, ZobristHashTable.
    Public Structure PieceIndex
        Public Const Pawn As Integer = 0
        Public Const Knight As Integer = 1
        Public Const Bishop As Integer = 2
        Public Const Rook As Integer = 3
        Public Const Queen As Integer = 4
        Public Const King As Integer = 5
    End Structure

    Public Const MaxPieceLegalMoves As Integer = ((27)) - 1 'The maximum number of legal moves that can be theoretically made by a piece.
    Public Const MaxTurnLegalMoves As Integer = ((218))     'The max number of legal moves that can be made by on a given player's turn.
    Public Const MaxPositionsPerGame As UInt16 = 2048 'Holds the value of the maximum number of positions that can be stored in GameHistory.

    Public Const DefaultGeneralOptions As String = "TTFTTFFF" '8-character string that represents the configuration of the program.
    'Index: 0 = Sound, 1 = Opening Animation, 2 = Small Opening Book, 3 = Board Highlights, 4 = Piece Highlights, 5 = Touch Move Rule, 6 = Invisible Pieces, 7 = Hammad Mode (bad AI).
    Public Const DefaultAnimationSpeed As Byte = 2 'Represents the speed of the piece-moving animation: 0 = Off, 1 = VFast, 2 = Fast, 3 = Medium, 4 = Slow.
    Public Const MemoryThreshold As UInt64 = 256 * (1024 * 1024) 'Max amount of memory (in Bytes) that can be allocated before
    'the AI's Transposition Table is reset.

    Public Const TrainingMovesPerPosition As Byte = 3 'A constant referring to the number of moves the user needs to make before
    'a new random position is chosen.

End Class




Public Structure CanCastle
    Public KS As Boolean
    Public QS As Boolean

    Public Sub CanCastle() 'Sets Castling Privileges.
        KS = True
        QS = True
    End Sub
    Public Sub CannotCastle() 'Resets Castling Privileges.
        KS = False
        QS = False
    End Sub
    Public Sub CopyFrom(ByVal Copier As CanCastle) 'Copies all the data from the parameter class.
        KS = Copier.KS
        QS = Copier.QS
    End Sub
    Public Function CanICastle() As Boolean 'Returns True if any castling privileges exist.
        Return KS OrElse QS
    End Function
End Structure




'Information about an AI's move.
Public Structure Move
    Public Score As Double 'Move Evaluation.
    Public OldMoveX As String
    Public OldMoveY As String
    Public NewMoveX As String
    Public NewMoveY As String
    Public Code As Char 'Contains information about an AI's search:
    'f = no end-state detected, a = search manually aborted, c = checkmate found, s = stalemate, o = one move found (forced)
    Public BitMove As UInt16

    Public Sub SetEmptyMove()
        Score = 0.0
        OldMoveX = ""
        OldMoveY = ""
        NewMoveX = ""
        NewMoveY = ""
        Code = "t"c 'Code for a terminated search move.
    End Sub

    Public Sub Flip() 'Orients the move w.r.t the black side, for 'Flip Mode'
        If OldMoveX <> "-1" Then OldMoveX = CStr(7 - CInt(OldMoveX))
        If OldMoveY <> "-1" Then OldMoveY = CStr(7 - CInt(OldMoveY))
        If NewMoveX <> "-1" Then NewMoveX = CStr(7 - CInt(NewMoveX))
        If NewMoveY <> "-1" Then NewMoveY = CStr(7 - CInt(NewMoveY))
    End Sub

    Public Sub Invert() 'Inverts the move: ie b1c3 -> c3b1.
        Dim BufferX As String = OldMoveX
        Dim BufferY As String = OldMoveY
        OldMoveX = NewMoveX
        OldMoveY = NewMoveY
        NewMoveX = BufferX
        NewMoveY = BufferY
    End Sub

    Public Sub OutputToConsole() 'Outputs the move to the console, using pretty colours :D.
        Dim OldCoors As String = If(Val(OldMoveX) >= 0 AndAlso Val(OldMoveY) >= 0, Chr(Integer.Parse(OldMoveX) + 97) & 8 - Integer.Parse(OldMoveY), "XX")
        Dim NewCoors As String = If(Val(NewMoveX) >= 0 AndAlso Val(NewMoveY) >= 0, Chr(Integer.Parse(NewMoveX) + 97) & 8 - Integer.Parse(NewMoveY), "XX")
        Console.ForegroundColor = ConsoleColor.White
        Console.Write("Move: ")
        Console.ForegroundColor = ConsoleColor.Red
        Console.Write(OldMoveX)
        Console.ForegroundColor = ConsoleColor.DarkYellow
        Console.Write(OldMoveY)
        Console.ForegroundColor = ConsoleColor.White
        Console.Write(" -> ")
        Console.ForegroundColor = ConsoleColor.Green
        Console.Write(NewMoveX)
        Console.ForegroundColor = ConsoleColor.Blue
        Console.Write(NewMoveY)
        Console.ForegroundColor = ConsoleColor.Gray
        Console.Write(" (" + OldCoors & NewCoors & "). ")
        'Outputs the Move's Code.
        Console.ForegroundColor = ConsoleColor.Magenta
        Dim OutputCode As String = If(Code = Nothing, "<EMP>", Code)
        Console.WriteLine("Code: " + OutputCode + ".")
        Console.ForegroundColor = ConsoleColor.White
    End Sub

    'Function that checks the move against a user input move, to see if the structures are identical.
    Public Function CompareAgainstOtherMove(ByVal ComparisonMove As Move, Optional ByVal CheckFlag As Boolean = False) As Boolean
        If OldMoveX = ComparisonMove.OldMoveX AndAlso OldMoveY = ComparisonMove.OldMoveY AndAlso NewMoveX = ComparisonMove.NewMoveX AndAlso NewMoveY = ComparisonMove.NewMoveY Then
            Return Not CheckFlag OrElse Code = ComparisonMove.Code
        End If
        Return False
    End Function
End Structure




'Structure that holds all the settings the AI will use in its search.
Public Class AISearchSettings
    'Denotes all the variables that should not be displayed in the 'Modify AI Settings' panel.
    Public NonDisplayable() As String = {"NonDisplayable", "ReturnBestMove"}

    Public UseQuiescence As Boolean
    Public UsePieceHeatMaps As Boolean
    Public UseTranspositionTable As Boolean
    Public OutputToConsole As Boolean
    Public OutputMoveDebugInfo As Boolean
    Public OutputPath As Boolean
    Public ReturnBestMove As Boolean
    Public BlunderTemperature As UInt16
    Public UpdateLifetimeStats As Boolean
    Public NodeSearchUseHashing As Boolean
    Public TimeToLive As Integer
    Public NullMoveRValue As Integer
    Public UseIterativeDeepening As Boolean
    Public StableSearch As Boolean
    Public MaxDepthExt As Integer
    Public ReductionThreshold As Integer
    Public AspirationWidth As Int16
    Public EvaluatePawnStructure As Boolean
    Public UsePVS As Boolean
    Public ReadOnly Property InfoDescriptions As New Dictionary(Of String, Object) From {
        {"UseQuiescence", "Will the AI use the Quiescence algorithm?"},
        {"UsePieceHeatMaps", "Will the AI use PieceHeatMaps in its search?"},
        {"UseTranspositionTable", "Will the AI use the Transposition Table in its search?"},
        {"OutputToConsole", "Will the AI output its chosen move, evaluation & search time?"},
        {"OutputMoveDebugInfo", "Will the AI output the details of the current move it is searching on?" & vbCrLf & "(Note: this setting can reduce AI performance slightly, as it results in many, rapid, writes to the console.)"},
        {"OutputPath", "Will the AI output its path of 'best moves' from the current position?"},
        {"ReturnBestMove", "If set to False, the AI will return the worst move in the position," & vbCrLf & "rather than the best move (also called Hammad Mode) :D."},
        {"BlunderTemperature", "Introduces probability that a better move (with score ds) won't be accepted in the main search." & vbCrLf & "Controls the 'Temperature' T, where P = 1 - e^[s/T]. The higher T, the more likely blunders are." & vbCrLf & "Enabling this also disables Aspiration Windows, only accepts moves from non-aborted searches, and forces Return Best Move."},
        {"UpdateLifetimeStats", "Will the AI add its current search stats to its lifetime stats?"},
        {"NodeSearchUseHashing", "Controls whether or not the AI will include the TranspositionTable during a Node Search." & vbCrLf & "Whilst this does make the search drastically slower, this allows the AI to detect duplicate positions," & vbCrLf & "so it can record how many unique nodes are encountered in a search (estimation)."},
        {"TimeToLive", "Represents how many moves need to be made before a Transposition Table entry" & vbCrLf & "is deemed 'dead', after which we are allowed to update that entry."},
        {"NullMoveRValue", "Represents how shallow (specifically, how much we reduce the depth)" & vbCrLf & "we search when calculating null moves."},
        {"UseIterativeDeepening", "Will the AI use Iterative Deepening in its search?"},
        {"StableSearch", "Denotes (the lack of) the ability for the AI to change the depth of nodes," & vbCrLf & "depending on certain criteria (eg: search extensions in checks, late move reductions, etc)."},
        {"MaxDepthExt", "Each time the AI is put into check, it increases its search depth by 1." & vbCrLf & "This value limits the number of these 'extensions' in a given path."},
        {"ReductionThreshold", "Denotes how many legal moves will be searched at the full depth" & vbCrLf & "(with the remaining, 'late' moves being searched at a reduced depth to save time)."},
        {"AspirationWidth", "Denotes the (half) width of the Aspiration Window, for use in iterative deepening." & vbCrLf & "Measured in centipawns (100 = pawn weight)."},
        {"EvaluatePawnStructure", "Denotes whether the AI is able to use bit-masks," & vbCrLf & "for use in past pawn & isolated pawn detection."},
        {"UsePVS", "Can the AI use Principle Variation Search, for the root node?"}
    }

    'Constants that determine the 'off' values for each field
    Public ReadOnly Property DisabledValues As New Dictionary(Of String, Object) From {
        {"BlunderTemperature", 0US},
        {"TimeToLive", 0},
        {"NullMoveRValue", Integer.MaxValue - 1},
        {"MaxDepthExt", 0},
        {"ReductionThreshold", Integer.MaxValue},
        {"AspirationWidth", 0S}
    }


    'Public StableSearch As Boolean 'If set to True, NegaMax disables 'exact' moves (TTEntry.Flag = 0, where the evaluation of the
    ''position is asummed correct) from being stored, which reduces the natural instability of the Transposition Table (at the cost of a lot of speed).
    ''Example position of where disabling this feature helps is 8/4k3/8/3K1P2/8/8/8/8 b - - 0 1.
    ''For more information, see https://web.archive.org/web/20071031100051/http://www.brucemo.com/compchess/programming/hashing.htm#instability.
    ''I think I have fixed this issue now :DD, but for the time being, I'll leave it as a toggle - just in case :).
    ''Perhaps my Transposition Table could be more stable without disabling these 'exact' moves, but I can't for the life of me figure out a way of doing that.

    Public Sub New()
        SetDefaultSettings()
    End Sub

    Public ReadOnly Property DefaultValues As New Dictionary(Of String, Object) From {
        {"BlunderTemperature", 0US},
        {"TimeToLive", 4},
        {"NullMoveRValue", 3},
        {"MaxDepthExt", 8},
        {"ReductionThreshold", 4},
        {"AspirationWidth", CShort(40)}
    }
    Public Sub SetDefaultSettings()
        UseQuiescence = True
        UsePieceHeatMaps = True
        UseTranspositionTable = True
        OutputToConsole = True
        OutputMoveDebugInfo = False
        OutputPath = True
        ReturnBestMove = True
        BlunderTemperature = 0US
        UpdateLifetimeStats = True
        NodeSearchUseHashing = False
        TimeToLive = CSByte(DefaultValues("TimeToLive"))
        NullMoveRValue = CInt(DefaultValues("NullMoveRValue"))
        UseIterativeDeepening = True
        StableSearch = False
        MaxDepthExt = CInt(DefaultValues("MaxDepthExt"))
        ReductionThreshold = CInt(DefaultValues("ReductionThreshold"))
        AspirationWidth = CShort(DefaultValues("AspirationWidth"))
        EvaluatePawnStructure = True
        UsePVS = True
    End Sub

    'Function that copies a user's Search Settings to this class. If the core AI settings have changed (ie: Quiescence, PieceHeatMaps, StableSearch),
    'then the function outputs True.
    Public Function CopyFrom(ByRef Copier As AISearchSettings) As Boolean
        Dim CoreAISettingsChanged As Boolean
        If UseQuiescence <> Copier.UseQuiescence Then CoreAISettingsChanged = True : UseQuiescence = Copier.UseQuiescence
        If UsePieceHeatMaps <> Copier.UsePieceHeatMaps Then CoreAISettingsChanged = True : UsePieceHeatMaps = Copier.UsePieceHeatMaps
        If UseTranspositionTable <> Copier.UseTranspositionTable Then CoreAISettingsChanged = True : UseTranspositionTable = Copier.UseTranspositionTable
        OutputToConsole = Copier.OutputToConsole
        OutputPath = Copier.OutputPath
        OutputMoveDebugInfo = Copier.OutputMoveDebugInfo
        If ReturnBestMove <> Copier.ReturnBestMove Then CoreAISettingsChanged = True : ReturnBestMove = Copier.ReturnBestMove
        If BlunderTemperature <> Copier.BlunderTemperature Then CoreAISettingsChanged = True : BlunderTemperature = Copier.BlunderTemperature
        UpdateLifetimeStats = Copier.UpdateLifetimeStats
        NodeSearchUseHashing = Copier.NodeSearchUseHashing
        TimeToLive = Copier.TimeToLive
        NullMoveRValue = Copier.NullMoveRValue
        UseIterativeDeepening = Copier.UseIterativeDeepening
        If StableSearch <> Copier.StableSearch Then CoreAISettingsChanged = True : StableSearch = Copier.StableSearch
        MaxDepthExt = Copier.MaxDepthExt
        ReductionThreshold = Copier.ReductionThreshold
        If AspirationWidth <> Copier.AspirationWidth Then CoreAISettingsChanged = True : AspirationWidth = Copier.AspirationWidth
        If EvaluatePawnStructure <> Copier.EvaluatePawnStructure Then CoreAISettingsChanged = True : EvaluatePawnStructure = Copier.EvaluatePawnStructure
        If UsePVS <> Copier.UsePVS Then CoreAISettingsChanged = True : UsePVS = Copier.UsePVS
        Return CoreAISettingsChanged
    End Function

End Class



'Class holding the TFTable of each depth of the search.
Public Structure BoardState

    'All Uint64 Bitboards. We don't store king bitboards here - this is done via KPos information.
    Public BitboardPawnWhite As UInt64
    Public BitboardPawnBlack As UInt64
    Public BitboardKnightWhite As UInt64
    Public BitboardKnightBlack As UInt64
    Public BitboardBishopWhite As UInt64
    Public BitboardBishopBlack As UInt64
    Public BitboardRookWhite As UInt64
    Public BitboardRookBlack As UInt64
    Public BitboardQueenWhite As UInt64
    Public BitboardQueenBlack As UInt64

    Public ZobristValue As UInt64

    Public MaterialCountWhite As Integer
    Public MaterialCountBlack As Integer
    Public PHMValueWhite As Integer 'Represents the base Piece Heat Map values for each player, for the base position, using the 100% middlegame values.
    Public PHMValueBlack As Integer

    Public EnPassant As UInt16
    Public WhiteCanCastle As CanCastle
    Public BlackCanCastle As CanCastle
    Public HalfMoveSize As UInt16


    'Public Sub CopyFrom(ByRef PreviousState As BoardState)

    'End Sub
    'Public Sub Reset()
    '    ClearBitboards()
    '    ZobristValue = 0UL
    '    TFTable = 0UL
    '    EnPassant = 0S
    'End Sub
    Public Sub ClearBitboards()
        BitboardPawnWhite = 0UL
        BitboardPawnBlack = 0UL
        BitboardKnightWhite = 0UL
        BitboardKnightBlack = 0UL
        BitboardBishopWhite = 0UL
        BitboardBishopBlack = 0UL
        BitboardRookWhite = 0UL
        BitboardRookBlack = 0UL
        BitboardQueenWhite = 0UL
        BitboardQueenBlack = 0UL
    End Sub
End Structure

Public Structure NegaMaxSearchTools
    Dim TFTable As UInt64 'An attacking map of all pieces that could influence the king's motion (where the king is removed)
    'Check detection is handled via the generation of TFTable (non-sliding pieces), and placing a queen at the king's location and casting rays via occupancy masks (sliding pieces).
    'Resolving via captures & king movement handled via TFTable and KPos InCheck information, resolving via blocks handled by running checking piece bitboard for updated occupancy mask.
    'An attacking map of all pieces that could influence the king's motion (where the king is removed)
    Dim PinInfoStraight, PinInfoDiag As UInt64
    Dim OccupancyMask, EnemyPieceMask As UInt64
    Dim CheckInfo As UInt16 'Checking data is represented as a set of bits, in the format:
    '00000000CDXXXYYY
    'C = Check (Flag = 128). D = Double Check (Flag = 64). XY = Checking Piece Coordinates (Flag = 63)
End Structure