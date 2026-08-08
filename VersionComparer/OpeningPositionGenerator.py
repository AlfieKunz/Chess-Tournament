import chess
import chess.pgn
import chess.engine
import time
import random

StockfishPath = "stockfish-windows-x86-64-avx2/stockfish/stockfish-windows-x86-64-avx2.exe"
StockfishDepth = 15
PGNPath = "lichess_elite_2021-11/lichess_elite_2021-11.pgn"
OutputPath = "OpeningPositions.txt"

TotalPositionCount = 250
StartingFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

MinMoveCount = 8           # The minimum full move number for a random position
MaxMoveCount = 30          # The maximum full move number for a random position
MinMovesAfterPosition = 20 # Game must continue for at least this many moves AFTER the position
MinPieceCount = 15         # Position must have at least this many pieces on the board
EvalCutoffPoint = 10       # Centipawn threshold for equality (20 cp = 0.2 pawns)

MinGameSkipValue = 10      # Minimum number of games to skip to get a new random game
MaxGameSkipValue = 100     # Maximum number of games to skip




Stockfish = chess.engine.SimpleEngine.popen_uci(StockfishPath)

Positions = set()
NoGames = 0
st = time.time()

with open(PGNPath) as PGN:
    while len(Positions) < TotalPositionCount - 1:
        TempGame = chess.pgn.read_game(PGN)
        if TempGame is None:
            print("Reached end of PGN file. Resetting...")
            PGN.seek(0)
            continue

        NoGames += 1
        Moves = list(TempGame.mainline_moves())
        
        # Picks a random move.
        SampleMoves = [p for p in range((MinMoveCount * 2) - 1, (MaxMoveCount * 2), 2) if p < len(Moves)]
        if not SampleMoves:
            continue
        
        # Biases for early moves (allows for a good game).
        Move = min(random.choice(SampleMoves), random.choice(SampleMoves))
        if (len(Moves) - (Move + 1)) < MinMovesAfterPosition:
            continue
        
        Board = TempGame.board()
        for i in range(Move + 1):
            Board.push(Moves[i])
        if Board.turn != chess.WHITE:
            continue
        if len(Board.piece_map()) < MinPieceCount:
            continue
        if Board.fen() in Positions:
            continue
        
        Eval = Stockfish.analyse(Board, chess.engine.Limit(depth=StockfishDepth))["score"].white().score(mate_score=10000)
        if abs(Eval) <= EvalCutoffPoint:
            Positions.add(Board.fen())
            print(f"Found {len(Positions) + 1}/{TotalPositionCount} | Move {Board.fullmove_number} | Eval: {Eval/100:.2f} | {Board.fen()}")
            
        for _ in range(random.randint(MinGameSkipValue, MaxGameSkipValue)):
            if chess.pgn.skip_game(PGN) is None:
                break


ft = time.time()
print(f"Done! :D Found {len(Positions)} random positions in {ft - st:.2f} seconds.")

with open(OutputPath, "w") as f:
    f.write(f"{StartingFEN}\n")
    for fen in sorted(list(Positions)):
        f.write(f"{fen}\n")
        
print(f"Saved positions.")
Stockfish.quit()