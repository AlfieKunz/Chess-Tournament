# Chess AI Sui-Style Tournament & Benchmarking System.

![VB.NET](https://img.shields.io/badge/VB.NET-%23512BD4.svg?style=for-the-badge&logo=dotnet&logoColor=white)
![WinForms](https://img.shields.io/badge/Console_Application-%230078D4.svg?style=for-the-badge&logo=windows&logoColor=white)
![Project](https://img.shields.io/badge/Project-Chess-%23000000.svg?style=for-the-badge&logo=lichess&logoColor=white)
![Pipeline](https://img.shields.io/badge/Pipeline-Elo%20Benchmarking%20(Swiss)-%23E10098.svg?style=for-the-badge)

---

A small program that allows multiple versions of my [**Chess AI**](https://github.com/AlfieKunz/Chess-Game-AI) to compete in a tournement system, across a customisable variety of starting positions and thinking time. Supports a wide variety of AI models across various legacy designs and formats in an intuitive 'plug and play' methodology, with live game showcases and statistics via a colourful, information-rich terminal log. To help with the smooth running of games, 'arbiter' controls can be managed via the program, allowing for termination of games from broken or illegal moves, and across the full spread of chess win/draw conditions.

This work is self-motivated and self-funded, and forms a small part of my 'commercial grade' [**Chess Game & Artificial Intelligence**](https://github.com/AlfieKunz/Chess-Game-AI): new features, flavours of AI, and possible bugs are always tested and compared using this system, and helps me build the strongest AI possible! :) Data from this program can also be used to calculate a relative ELO score for each AI version.

This project is written primarily in VB.NET as a Visual Studio console application.  
Also attached is the python code for generating robust, equal, and 'fun' starting positions, using Stockfish.

<p align="center">
  <img width="50%" alt="ChessTournament" src="./readme_img/ChessTournament.png" />
</p>

---

## Features and Highlights

**Tournament & Match Engine**  
✅ Fully configurable head-to-head tournaments — set the number of games and per-move thinking time at launch.  
✅ Automatic colour-swap fixtures: once half the match-up is complete, every starting position is replayed with colours reversed to cancel out first-move bias.  
✅ Live pause/resume — hit Space mid-tournament to freeze play, view a running scoreboard and an ETA, then resume with another keystroke.  
✅ Self-healing invalid-game handling: any game that throws an exception (illegal move, engine fault, etc.) is excluded from the results and logged separately rather than halting the tournament.  
✅ Auto-adapts the total match count if the opening-position library runs out early, instead of crashing.  
✅ Rolling and final scoreboards, including elapsed and extrapolated remaining tournament time.

**Multi-Version AI Compatibility ("Plug & Play")**  
✅ Loads two independently-versioned AI builds side-by-side via separate namespace imports — swap either engine without touching the tournament logic.  
✅ Automatic legacy-version detection, parsed straight from each engine's semantic version string.  
✅ Legacy 'Multiple-Depth Multithreading' emulation — for pre-v7.0 engines lacking native iterative deepening, five parallel searches are launched at staggered depths (extending further if time allows) to reconstruct modern iterative-deepening performance externally.  
✅ Native iterative-deepening handler for modern engines, seeding each depth's search with the previous depth's best move to sharpen alpha-beta pruning.  
✅ Forced-mate short-circuiting — the moment any thread reports a forced checkmate, every sibling search thread is aborted instantly.  
✅ Material-adaptive search depth, scaling logarithmically with the pieces remaining so both AIs dig deeper automatically as the game heads into the endgame.  
✅ Mate-distance-aware depth capping, so a confirmed forced mate is searched only as deep as needed to deliver it.  
✅ Per-engine tunable search parameters (transposition tables, aspiration window width, PVS, bitmasked move generation, move-reduction thresholds) for controlled configuration testing.

**Time Control**  
✅ Shared, configurable per-move thinking time for both AIs.  
✅ Three-tier time-overrun handling: a clean abort once a move is ready, an escalated warning with bonus time if a player stalls, and a forced 2-ply 'emergency' search if a legacy engine blows its budget twice in a row.

**Arbiter & Rules Enforcement**  
✅ Full endgame detection — checkmate, stalemate, threefold repetition, and the 50-move rule.  
✅ Draw-by-insufficient-material detection, covering K v K, K v K+minor piece, and K+B v K+B on matching bishop colour complexes.  
✅ From-scratch Zobrist hashing engine (random 64-bit key generation, full position/castling-rights/en-passant hashing) as a fallback repetition detector for engines that can't track it natively.  
✅ Independent game-history tracking for each engine.

**Live Terminal Visualisation**  
✅ Colour-coded Unicode board rendering, distinguishing piece colour and light/dark squares at a glance.  
✅ Real-time search depth and evaluation readout alongside the board.  
✅ Live-scrolling PGN transcript printed beside the board, with overflow handling for very long games.  
✅ Non-scrolling, self-refreshing display — the board, evaluation, and transcript redraw in place each move instead of filling the console with scrollback.  
✅ Custom score formatter, converting raw evaluations into standard notation (e.g. +2.3, or +M5 for mate-in-5).  
✅ Colour-coded win/loss/draw scoreboard with a proportional ASCII progress bar that resizes to fit the console width.  
✅ 'Outclass' tracking — flags starting positions where one AI won as both White and Black, a stronger dominance signal than a raw win tally.

**Opening Position Generator (Python + Stockfish)**  
✅ Standalone tool for building a curated bank of fair, balanced starting positions to feed the tournament.  
✅ Sources genuine positions from human master games via the Lichess Elite PGN database, rather than synthetic setups.  
✅ Early-game-biased random move sampling within a configurable move-number window.  
✅ Stockfish equality filter — only positions within a configurable centipawn threshold of dead level are accepted, keeping every starting point genuinely fair.  
✅ Minimum piece-count floor, filtering out over-simplified endgame positions.  
✅ Minimum remaining-game-length filter, ensuring every sampled position still has a real contest ahead of it.  
✅ Enforces White-to-move and de-duplicates identical FENs via a hash set.  
✅ Randomised game-skipping between samples to maximise positional diversity.  
✅ Automatic PGN wraparound if the source database is exhausted before the target count is reached.  
✅ Live progress logging and total-runtime benchmarking.  
✅ Configurable target position count and Stockfish analysis depth.  
✅ Clean output file: standard starting position followed by a sorted, de-duplicated FEN list.

✅ Graceful adaptation of old Chess AI features, such as old approaches to multithreading (per-depth thread allocation, vs newer iterative deepening), Option Strict, etc.  

---

## Project Showcase

> **Project Results:**  A vast number of tournaments have been run across many versions of my Chess AI, from v5.2 to the latest version (including many debug versions and pre-releases). You can see the <a href="https://docs.google.com/document/d/1l2azNiomihHmkGiXxQ9ZlNTO_YLcn6JF" target="_blank" rel="noopener noreferrer">**results of those tournaments here**</a>.

The best way to interact with this program for full control is directly through the source code - see the instructions below.

> **Program Controls:**
>1) First, drag and drop the relevant AI files into either the "AIPlayer1" or "AIPlayer2" folder, from either a model of choice from the "AI Archives" folder, or from any [Chess AI GitHub release](https://github.com/AlfieKunz/Chess-Game-AI/releases) succeeding v9.0. In the latter case, specifically copy the files {"AI.vb", "AILookupTables.vb", "CoreMethods.vb", "GameHistory.vb", "PieceLegalMoveGenerators.vb", "SubObjects.vb"}.
>2) Open the project solution in your IDE of choice, and click "Clean Solution" before building (to reset the AIPlayer?.vbproj files).
>3) To change any AI settings for a fully customisable tournament, navigate to the AdjustIndividualAISettings subroutine in "Program.vb" and adjust the AI1Settings or AI2Settings classes directly. For more information of what settings can be adjusted, and what each of them do, see the "AISearchSettings" class in the "SubObjects.vb" file of each AI player.
>4) Run the project, and input the number of games and maximum AI thinking time per move to run the tournament!
>5) At any time, press SPACE to pause the simulation (at the conclusion of the current game) to output the current tournament statistics, and estimated time of completion. Then, press SPACE again to instantly resume the tournament.

---

## Installation, and Folder Structure

### Required Software: Visual Studio (.NET 8.0).

To install, simply clone this repository using the following terminal prompts.
```bash
git clone https://github.com/AlfieKunz/Chess-Tournament
cd Chess-Tournament
```
Then, simply open the "VersionComparer.sln" file in Visual Studio.

Feel free to also fork this repository, open an issue, or submit pull requests. All contributions welcome! :)  
To better navigate this project, please see below for the related folder structure.

```
Chess-Tournament
├─ AI Archives                    // Full list of relevant AI files for a wide variety of legacy AI models, from v5.2 (A-Level NEA release) to v10.0. Details of each version in the "Tournament Results" document as mentioned above
├─ AIPlayer1                      // Folder containing all relevant AI files for "Player 1" in the tournament. Do not touch any non-AI related files
├─ AIPlayer2                      // Ditto for "Player 2"
├─ VersionComparer                //
│  ├─ OpeningPositionGenerator.py // Code for generating the fair, balanced starting positions that are used in the chess tournament (full set for Player 1 starting as white, full set for black)
│  └─ Program.vb                  // Main program code, for loading & interacting with AI models, and preparing, running, and analysing the tourament's games
└─ VersionComparer.sln            // Main VS code solution
```

---

## References & Inspiration

This work is self-motivated and self-funded. If you use this code or data in your work, please cite the associated preprint:

**Text Citation:**
> Kunz, A. (2025). *Chess AI Sui-Style Tournament & Benchmarking System*. Available at https://github.com/AlfieKunz/Chess-Tournament.

**BibTeX:**
```bibtex
@software{Kunz2025ChessTournament,
  title = {Chess AI Sui-Style Tournament & Benchmarking System},
  author = {Kunz, Alfie},
  year = {2025},
  url = {https://github.com/AlfieKunz/Chess-Tournament}
}
```