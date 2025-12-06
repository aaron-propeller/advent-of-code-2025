# Advent of Code 2025

![AI Disclaimer](https://img.shields.io/badge/Project%20Setup-AI-blue?style=flat-square&logo=robot) ![Human Solutions](https://img.shields.io/badge/Daily%20Puzzles-Human-green?style=flat-square&logo=brain)

> **AI Usage Disclaimer**: AI was used to set up the project infrastructure, tooling, and development environment. AI was **not** used in solving the daily Advent of Code puzzles - all puzzle solutions are written by humans.

Solutions for the [Advent of Code 2025](https://adventofcode.com/2025) puzzles, implemented in Haskell.

## Project Structure

```
├── inputs/                    # All input/test data files
│   ├── day01/
│   │   ├── input.txt         # Real puzzle input (git-crypt encrypted)
│   │   ├── sample.txt        # Sample/test input
│   │   └── expected.txt      # Expected results for validation
│   └── day02-day12/          # Days 2-12 follow same structure
│
└── src/                       # All source code
    ├── AoCDays/              # Daily puzzle solutions
    │   ├── Day01.hs          # Day 1 solution (partA & partB)
    │   ├── Day02.hs          # Day 2 solution (partA & partB)
    │   └── ... Day12.hs      # Days 3-12 solutions
    │
    ├── AoCUtils/             # Challenge-specific utilities
    │   ├── AoCGrid.hs        # Grid operations, coordinates, directions
    │   ├── AoCList.hs        # List manipulation (chunks, windows, etc.)
    │   ├── AoCParsing.hs     # Text parsing helpers
    │   └── AoCRange.hs       # Range operations and parsing
    │
    ├── AoCRunner/            # Application infrastructure
    │   ├── AoCCLI.hs         # Command line interface
    │   ├── AoCDisplay.hs     # Enhanced result display
    │   ├── AoCExecution.hs   # Timing and execution engine
    │   ├── AoCFiles.hs       # File I/O operations
    │   └── AoCRegistry.hs    # Day runner registry
    │
    └── Main.hs               # Main entry point (12 lines!)
```

Each day includes:

- **Solution module** (`AoCDays/DayXX.hs`) with `partA` and `partB` functions
- **Input files** in `inputs/dayXX/`:
  - `input.txt` - Real puzzle input (encrypted for privacy)
  - `sample.txt` - Test data from puzzle description
  - `expected.txt` - Expected sample results for validation

## Requirements

- GHC 9.4.7+
- Cabal 3.12+
- ghcid (for auto-reload development)
- git-crypt (for input encryption)

## Building

```bash
cabal build
```

## Running Solutions

### Enhanced Mode (Recommended)

Run with just a day number to test both sample and real inputs with validation:

```bash
# Using cabal directly
cabal exec aoc <day>

# Examples:
cabal exec aoc 5     # Runs both sample and input for Day 5
cabal exec aoc 01    # Runs both sample and input for Day 1
```

Enhanced output format with validation:

```
=== Day 05 Results ===

📋 SAMPLE:
   Part A: 3 ✅ (Expected: 3) [0.059ms] 🚀
   Part B: 14 ✅ (Expected: 14) [0.016ms] 🚀

🎯 INPUT:
   Part A: 782 [3.749ms] 🚀
   Part B: 353863745078671 [0.500ms] 🚀

Parse time: 0.398500 ms
Part A time: 1.904000 ms (avg)
Part B time: 0.258000 ms (avg)
Total time: 5.121000 ms
```

**Performance indicators:**

- 🚀 Very fast (<10ms)
- ⚡ Fast (<100ms)
- 🐌 Slow (>1s)

## Features

### Sample Validation

- Compare sample results against expected values in `expected.txt`
- Visual indicators (✅/❌) show validation status
- Automatic mismatch detection with warnings

### Performance Monitoring

- Detailed timing for parsing and both parts
- Average timing across sample and input runs
- Performance indicators for quick assessment

### Modular Architecture

- **AoCUtils**: Reusable challenge utilities (parsing, grids, ranges)
- **AoCRunner**: Application infrastructure (CLI, timing, display)
- **AoCDays**: Individual day solutions

## Input Files & Security

### File Types

All files are encrypted with git-crypt.

Located in `inputs/dayXX/`:

- **sample.txt**: Test inputs from puzzle descriptions (committed unencrypted)
- **input.txt**: Real puzzle inputs (encrypted with git-crypt for privacy)
- **expected.txt**: Expected sample results for automated validation

### git-crypt Setup

The project uses git-crypt to encrypt `input.txt` files while keeping sample data and source code public. This respects Advent of Code's request not to share puzzle inputs publicly.

```bash
# Initialize git-crypt (already done)
git-crypt init

# Check encryption status
git-crypt status

# Unlock repository (requires key)
git-crypt unlock
```

## Day Format

Days are numbered **01-12** (zero-padded). The program accepts flexible input:

- `1`, `01` → normalized to `01`
- `10` → stays as `10`
- `12` → maximum day

Each day module exports `partA` and `partB` functions that both run automatically.
