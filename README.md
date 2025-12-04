# Advent of Code 2025

![AI Disclaimer](https://img.shields.io/badge/Project%20Setup-AI-blue?style=flat-square&logo=robot) ![Human Solutions](https://img.shields.io/badge/Daily%20Puzzles-Human-green?style=flat-square&logo=brain)

> **AI Usage Disclaimer**: AI was used to set up the project infrastructure, tooling, and development environment. AI was **not** used in solving the daily Advent of Code puzzles - all puzzle solutions are written by humans.

Solutions for the [Advent of Code 2025](https://adventofcode.com/2025) puzzles, implemented in Haskell.

## Project Structure

```
src/
├── AoCUtils.hs         # Common utilities and helper functions
├── Main.hs             # Main program entry point
├── Day01/
│   ├── Day01.hs        # Day 1 solution (both parts A & B)
│   ├── input.txt       # Real puzzle input (git-crypt encrypted)
│   └── sample.txt      # Sample/test input (git-crypt encrypted)
├── Day02/
│   ├── Day02.hs        # Day 2 solution (both parts A & B)
│   ├── input.txt       # Real puzzle input (git-crypt encrypted)
│   └── sample.txt      # Sample/test input (git-crypt encrypted)
└── Day03-Day12/        # Days 3-12 follow same structure
```

Each day is self-contained with:

- **Haskell module** with `partA` and `partB` functions
- **input.txt** - Real puzzle input (encrypted for privacy)
- **sample.txt** - Test data from puzzle description (safe to share)

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

### Automatic Input Selection (Recommended)

Run with just a day number to test both sample and real inputs:

```bash
# Using the aoc script
./aoc run <day>

# Using cabal directly
cabal run aoc -- <day>

# Examples:
./aoc run 04        # Runs Day04/sample.txt then Day04/input.txt
cabal run aoc -- 01 # Runs Day01/sample.txt then Day01/input.txt
```

Output format:

```
=== Running Day 04 with sample.txt ===
Parse time: 0.123 ms
Part A: 42
Part A time: 0.456 ms
Part B: 84
Part B time: 0.789 ms
Total time: 1.368 ms

=== Running Day 04 with input.txt ===
Parse time: 1.234 ms
Part A: 12345
Part A time: 5.678 ms
Part B: 67890
Part B time: 9.012 ms
Total time: 15.924 ms
```

### Custom Input File

Specify a custom input file:

```bash
./aoc run <day> <input-file>
cabal run aoc -- <day> <input-file>

# Examples:
./aoc run 01 Day01/sample.txt
./aoc run 04 custom-input.txt
cabal run aoc -- 02 my-test.txt
```

## Available Scripts

The `./aoc` script provides convenient shortcuts:

```bash
# Build and run once with both sample and input
./aoc run <day>

# Build and run with specific file
./aoc run <day> <file>

# Auto-reload development with ghcid (rebuilds on file changes)
./aoc day <day>
./aoc day <day> <file>

# Install binary to ./bin/
./aoc install
```

### Examples

```bash
# Run day 4 with both sample and input files
./aoc run 04

# Run day 1 with specific file
./aoc run 01 Day01/sample.txt

# Development mode with auto-reload (both files)
./aoc day 04

# Development mode with specific file
./aoc day 04 Day04/sample.txt

# Install for system-wide use
./aoc install
```

## Input Files & Security

### File Types

- **sample.txt**: Test inputs from puzzle descriptions (committed unencrypted)
- **input.txt**: Real puzzle inputs (encrypted with git-crypt for privacy)

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
