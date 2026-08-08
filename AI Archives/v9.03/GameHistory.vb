'Stack that holds the Zobrist Keys & PGNs of all the positions that have previously appeared in the game. Used to enforce
'three-fold repetition (both for the Chess form & for the AI), for FEN information, and to capture the PGN of the game.
Public Class GameHistory

    'Stacks for the Zobrist values. 'Buffer' holds the GameHistory of the previous position (used in case the user undos their move).
    Private ReadOnly ZobristMain(GlobalConstants.MaxPositionsPerGame - 1) As UInt64
    Private ReadOnly ZobristBuffer(GlobalConstants.MaxPositionsPerGame - 1) As UInt64

    'Stacks for the PGN values.
    Private ReadOnly PGNMain(GlobalConstants.MaxPositionsPerGame - 1) As String
    Private ReadOnly PGNBuffer(GlobalConstants.MaxPositionsPerGame - 1) As String

    Private MainSize, BufferSize As UInt16 '= Index of last item + 1
    Private MainHalfSize, BufferHalfSize As UInt16 'Referring to the number of moves made since the last pawn push or capture.

    Public Sub Clear(Optional ByVal ZobristToResetTo As uint64 = 0)
        Move()
        MainSize = 0
        MainHalfSize = 0
        If ZobristToResetTo <> 0 Then PushZobrist(ZobristToResetTo, False)
    End Sub


    'Subroutine that swaps the GameHistory and Buffer arrays, along with their sizes.
    Public Sub Swap()
        Dim TempZobrist(1023) As UInt64
        Dim TempPGN(1023) As String
        Dim TempSize, TempHalfSize As UInt16
        Array.Copy(ZobristMain, TempZobrist, MainSize)
        Array.Copy(PGNMain, TempPGN, MainSize)
        TempSize = MainSize
        TempHalfSize = MainHalfSize
        Array.Copy(ZobristBuffer, ZobristMain, BufferSize)
        Array.Copy(PGNBuffer, PGNMain, BufferSize)
        MainSize = BufferSize
        MainHalfSize = BufferHalfSize
        Array.Copy(TempZobrist, ZobristBuffer, TempSize)
        Array.Copy(TempPGN, PGNBuffer, TempSize)
        BufferSize = TempSize
        BufferHalfSize = TempHalfSize
        'Removes the last FEN position of the (new) GameHistory array, as it will be later added by the EnforceEndStates() subroutine.
        PopZobrist()
    End Sub

    'Subroutine that copies the GameHistory array to the Buffer array, along with its size.
    Private Sub Move()
        Array.Copy(ZobristMain, ZobristBuffer, MainSize)
        Array.Copy(PGNMain, PGNBuffer, MainSize)
        BufferSize = MainSize
        BufferHalfSize = MainHalfSize
    End Sub

    'Subroutine that adds a new Zobrist Hash to the GameHistory array.
    Public Sub PushZobrist(ByVal Value As UInt64, Optional ByVal MoveArray As Boolean = False)
        'MoveArray is used to signify if a new move has been played on the board (if it hasn't [ie: the user has undone the position]
        'then we don't overwrite Buffer, as the board hsan't actually changed).
        If MainSize < GlobalConstants.MaxPositionsPerGame - 1 Then
            If MoveArray Then Move()
            ZobristMain(MainSize) = Value
            MainSize += 1US
        Else
            Console.ForegroundColor = ConsoleColor.DarkRed
            Console.WriteLine("Error when adding Position Data to GameHistory: Array Full.")
            Console.ForegroundColor = ConsoleColor.White
        End If
    End Sub

    'Subroutine that adds a new PGN Value to the GameHistory array.
    Public Sub PushPGN(ByVal Value As String, Optional ByVal MoveArray As Boolean = False)
        If MoveArray Then Move()
        PGNMain(MainSize) = Value
        'If the move was not a pawn move or a capture, we increment the half-move counter. Otherwise, we reset it.
        If (Value(0) >= "a" AndAlso Value(0) <= "h") OrElse Value.IndexOf("x"c) >= 0 Then
            MainHalfSize = 0
        Else
            MainHalfSize += 1
        End If
    End Sub

    'Function that removes the lastly-added position (FIFO structure) from the GameHistory array.
    Public Function PopZobrist() As UInt64
        If MainSize > 0 Then
            MainSize -= 1US
            Return ZobristMain(MainSize)
        Else 'The array is empty.
            Return 0
        End If
    End Function

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

    'Functions that return the main size of the array (representing the number of full moves) and the number of half moves (for 50-move rule).
    Public Function GetSize() As UInt16
        Return MainSize
    End Function

    Public Function GetHalfSize() As UInt16
        Return MainHalfSize
    End Function
    Public Sub SetHalfSize(ByVal HalfSize As UInt16)
        MainHalfSize = HalfSize
    End Sub

    'Function that returns the GameHistory array, but without any empty space (so that processing can be done quicker).
    Public Function GetZobristArray() As UInt64()
        Dim TempArray(MainSize - 1) As UInt64
        Array.Copy(ZobristMain, TempArray, MainSize)
        Return TempArray
    End Function

    'Function which gets the set of PGNs held in the Main GameHistory array, and formats it into a string.
    Public Function GetFormattedPGNString(ByVal GameEnded As Boolean) As String
        GetFormattedPGNString = ""
        If MainSize > 0 Then
            'Adds each PGN to the string.
            For n = 0 To MainSize - 1
                'PGNs are formatted as such: "1. WMove BMove 2. WMove BMove 3. ..."
                '... so add the move number for every other move.
                If n Mod 2 = 1 Then GetFormattedPGNString &= (n \ 2 + 1) & ". "
                GetFormattedPGNString &= PGNMain(n) & " "
            Next
            GetFormattedPGNString = GetFormattedPGNString.TrimEnd(" "c)
            'If the game has been terminated, we add the # symbol at the end to represent this.
            If GameEnded Then GetFormattedPGNString &= "#"
        End If
    End Function

End Class
