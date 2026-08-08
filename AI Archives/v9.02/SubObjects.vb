Imports System.Runtime.CompilerServices
Imports System.Windows.Forms.VisualStyles.VisualStyleElement.ProgressBar
Imports Microsoft.VisualBasic.ApplicationServices


'Class holding all the constants that my program needs - can be accessed by all classes.
Public Class GlobalConstants
    Public Shared StartingFENPosition As String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    Public Shared ProgramName As String = "Chess Game & Artificial Intelligence" 'also known as 'chessbot 9000' - thanks stroganoff <3
    Public Shared ProgramVersion As String = "v9.02"
    Public Shared StartupPath As String = (AppDomain.CurrentDomain.BaseDirectory).TrimEnd("\"c)

    Public Shared TranspositionTableSize As Byte = 64 - ((23)) 'Constant referring to how large the TranspositionTable object is.
    'Used to determine how much to scate the ZobristValue by.

    'Structure holding the relative weights of all the pieces on the board.
    Public Structure PieceWeight
        Public Shared Pawn As Integer = 100
        Public Shared Knight As Integer = 300
        Public Shared Bishop As Integer = 300
        Public Shared Rook As Integer = 500
        Public Shared Queen As Integer = 900
        Public Shared King As Integer = 300 'No meaning to this value, other than to de-prioritise king captures somewhat.
    End Structure

    Public Shared MaxPieceLegalMoves As Byte = ((27)) - 1 'The maximum number of legal moves that can be theoretically made by a piece.
    Public Shared MaxTurnLegalMoves As Byte = ((218)) - 1 'The max number of legal moves that can be made by on a given player's turn.
    Public Shared MaxPositionsPerGame As UInt16 = 2048 'Holds the value of the maximum number of positions that can be stored in GameHistory.

    Public Shared DefaultGeneralOptions As String = "TTFTTFFF" '8-character string that represents the configuration of the program.
    'Index: 0 = Sound, 1 = Opening Animation, 2 = Small Opening Book, 3 = Board Highlights, 4 = Piece Highlights, 5 = Touch Move Rule, 6 = Invisible Pieces, 7 = Hammad Mode (bad AI).
    Public Shared DefaultAnimationSpeed As Byte = 2 'Represents the speed of the piece-moving animation: 0 = Off, 1 = VFast, 2 = Fast, 3 = Medium, 4 = Slow.
    Public Shared MemoryThreshold As UInt64 = 256 * (1024 * 1024) 'Max amount of memory (in Bytes) that can be allocated before
    'the AI's Transposition Table is reset.

    Public Shared TrainingMovesPerPosition As Byte = 3 'A constant referring to the number of moves the user needs to make before
    'a new random position is chosen.

End Class




Public Class CanCastle
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
End Class




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
End Structure




'Structure that holds all the settings the AI will use in its search.
Public Class AISearchSettings
    Public UseQuiescence As Boolean 'Will the AI use the Quiescence algorithm?
    Public UsePieceHeatMaps As Boolean 'Will the AI use PieceHeatMaps in its search?
    Public UseTranspositionTable As Boolean 'Will the AI use the Transposition Table in its search?
    Public OutputToConsole As Boolean 'Will the AI output its chosen move, evaluation & search time?
    Public OutputMoveDebugInfo As Boolean 'Will the AI output the details of the current move it is searching on?
    'Note: this setting can reduce AI performance slightly, as it results in many, rapid, writes to the console.
    Public OutputPath As Boolean 'Will the AI output its path of 'best moves' from the current position?
    Public ReturnBestMove As Boolean 'If set to False, the AI will return the worst move in the position, rather than the best move.
    '(also called Hammad Mode) :D.
    Public UpdateLifetimeStats As Boolean 'Will the AI add its current search stats to its lifetime stats?
    Public NodeSearchCalculateRepetitions As Boolean 'Controls whether or not the AI will include the TranspositionTable
    'during a Node Search. Whilst this does make the search ~10% slower, this allows the AI to detect duplicate positions,
    'so it can record how many unique nodes are encountered in a search? (estimation)
    Public TimeToLive As SByte 'Represents how many moves need to be made before a Transposition Table entry is deemed 'dead', after which
    'we are allowed to update that entry.
    Public NullMoveRValue As Integer 'Represents how shallow (specifically, how much we reduce the depth) we search when calculating null moves.
    Public StableSearch As Boolean 'Denotes (the lack of) the ability for the AI to change the depth of nodes, depending on certain criteria (eg: search extensions
    'in checks, late move reductions, etc).
    Public MaxDepthExt As Integer 'Each time the AI is put into check, it increases its search depth by 1. This value limits the number of these 'extensions'
    'in a given path.
    Public MoveReductionThreshold As Integer 'Denotes how many legal moves will be searched at the full depth (with the remaining, 'late' moves being
    'searched at a reduced depth to save time).
    Public AspirationWindowWidth As Int16 'Denotes the (half) width of the Aspiration Window, for use in iterative deepening.


    'Public StableSearch As Boolean 'If set to True, NegaMax disables 'exact' moves (TTEntry.Flag = 0, where the evaluation of the
    ''position is asummed correct) from being stored, which reduces the natural instability of the Transposition Table (at the cost of a lot of speed).
    ''Example position of where disabling this feature helps is 8/4k3/8/3K1P2/8/8/8/8 b - - 0 1.
    ''For more information, see https://web.archive.org/web/20071031100051/http://www.brucemo.com/compchess/programming/hashing.htm#instability.
    ''I think I have fixed this issue now :DD, but for the time being, I'll leave it as a toggle - just in case :).
    ''Perhaps my Transposition Table could be more stable without disabling these 'exact' moves, but I can't for the life of me figure out a way of doing that.

    Public Sub New()
        SetDefaultSettings()
    End Sub

    Public Sub SetDefaultSettings()
        UseQuiescence = True
        UsePieceHeatMaps = True
        UseTranspositionTable = True
        OutputToConsole = True
        OutputMoveDebugInfo = False
        OutputPath = True
        ReturnBestMove = True
        UpdateLifetimeStats = True
        NodeSearchCalculateRepetitions = False
        TimeToLive = 3
        NullMoveRValue = 3
        StableSearch = False
        MaxDepthExt = 8
        MoveReductionThreshold = If(UsePieceHeatMaps, 4, 7)
        AspirationWindowWidth = 50 'Where 100 is the Weight of a Pawn, for reference.
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
        UpdateLifetimeStats = Copier.UpdateLifetimeStats
        NodeSearchCalculateRepetitions = Copier.NodeSearchCalculateRepetitions
        TimeToLive = Copier.TimeToLive
        NullMoveRValue = Copier.NullMoveRValue
        If StableSearch <> Copier.StableSearch Then CoreAISettingsChanged = True : StableSearch = Copier.StableSearch
        MaxDepthExt = Copier.MaxDepthExt
        MoveReductionThreshold = Copier.MoveReductionThreshold
        AspirationWindowWidth = Copier.AspirationWindowWidth
        Return CoreAISettingsChanged
    End Function

End Class



'Class holding the TFTable of each depth of the search.
Public Class TFTableStorage
    Public Table(7, 7) As Char
    'Public Sub SetTable(ByVal TableToCopy(,) As Char)
    '    Array.Copy(TableToCopy, Table, 64)
    'End Sub
    'Public Sub CopyTableTo(ByVal TableToCopy(,) As Char)
    '    Array.Copy(Table, TableToCopy, 64) 'Alfie Note 24.12.24 - this seems very unnecessary... why can't we just pass a reference to NegaMaxTFTable??
    'End Sub
    'Public Function GetTable() As Char(,)
    '    Return Table
    'End Function
End Class
