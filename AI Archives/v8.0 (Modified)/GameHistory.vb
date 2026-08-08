'Stack that holds the Zobrist Keys of all the positions that have previously appeared in the game. Used to enforce
'three-fold repetition (both for the Chess form & for the AI), and for FEN information.
Public Class GameHistory
    Private GameHistoryArray(1023) As UInt64
    Private Buffer(1023) As UInt64 'Holds the GameHistory of the previous position (used in case the user undos their move).
    Private MainSize, BufferSize As UInt16 '= Index of last item + 1

    Public Sub Clear(Optional ByVal param = False)
        Move()
        MainSize = 0
    End Sub

    'Subroutine that swaps the GameHistory and Buffer arrays, along with their sizes.
    Public Sub Swap()
        Dim TempSet(1023) As UInt64
        Dim TempSize As Int16
        Array.Copy(GameHistoryArray, TempSet, MainSize)
        TempSize = MainSize
        Array.Copy(Buffer, GameHistoryArray, BufferSize)
        MainSize = BufferSize
        Array.Copy(TempSet, Buffer, TempSize)
        BufferSize = TempSize
        'Removes the last position of the (new) GameHistory array, as it will be later added by the EnforceEndStates subroutine.
        Pop()
    End Sub

    'Subroutine that copies the GameHistory array to the Buffer array, along with its size.
    Private Sub Move()
        Array.Copy(GameHistoryArray, Buffer, MainSize)
        BufferSize = MainSize
    End Sub

    'Subroutine that adds a new Zobrist Hash to the GameHistory array.
    Public Sub PushZobrist(ByVal Value As UInt64, ByVal MoveArray As Boolean)
        'MoveArray is used to signify if a new move has been played on the board (if it hasn't [ie: the user has undone the position]
        'then we don't overwrite Buffer, as the board hsan't actually changed).
        If MoveArray Then Move()
        GameHistoryArray(MainSize) = Value
        MainSize += 1
    End Sub

    'Function that removes the lastly-added position (FIFO structure) from the GameHistory array.
    Public Function Pop() As UInt64
        If MainSize > 0 Then
            MainSize -= 1
            Return GameHistoryArray(MainSize)
        Else 'The array is empty.
            Return 0
        End If
    End Function

    'Function that checks how many times the input Zobrist Key is present in the GameHistory array (used to enforce three-fold repetition).
    Public Function CheckNoOfZobristOccurances(ByVal Value As UInt64) As Byte
        CheckNoOfZobristOccurances = 0
        For n = 0 To MainSize - 1
            If GameHistoryArray(n) = Value Then
                CheckNoOfZobristOccurances += 1
                If CheckNoOfZobristOccurances = 3 Then Exit For 'There will never be >3 occurances, as if this were true then the game would end.
            End If
        Next
    End Function

    Public Function GetSize() As UInt16
        Return MainSize
    End Function

    'Function that returns the GameHistory array, but without any empty space (so that processing can be done quicker).
    Public Function GetZobristArray() As UInt64()
        Dim TempArray(MainSize - 1) As UInt64
        Array.Copy(GameHistoryArray, TempArray, MainSize)
        Return TempArray
    End Function

End Class
