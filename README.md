# Advent of Code 2025

![AI Disclaimer](https://img.shields.io/badge/AI-Project%20Setup%20Only-blue?style=flat-square&logo=robot) ![Human Solutions](https://img.shields.io/badge/Daily%20Puzzles-Human%20Solved-green?style=flat-square&logo=brain)

> **AI Usage Disclaimer**: AI was used to set up the project infrastructure, tooling, and development environment. AI was **not** used in solving the daily Advent of Code puzzles - all puzzle solutions are written by humans.

This project contains solutions for Advent of Code 2025 puzzles implemented in Haskell using Cabal.

## Requirements

- GHC 9.4.7+
- Cabal 3.12+
- ghcid (for auto-reload development)

## Building

```sh
cabal build
```

## Running Solutions

### Using the Runner Script (Recommended)

The `./aoc` script provides several convenient commands:

```sh
# Build and run once
./aoc run <day> <input-file>

# Auto-reload with ghcid (rebuilds on file changes)
./aoc day <day> <input-file>

# Install to local bin directory
./aoc install
```

### Day Format

Days must be in **XXy** format where:

- **XX** = day number with two digits (01-12)
- **y** = part letter (a or b)

### Examples

```sh
# Run day 1, part A with sample data
./aoc run 01a src/01a/sample.txt

# Auto-reload day 2, part B (rebuilds when you save files)
./aoc day 02b src/02b/input.txt

# Install to local bin directory
./aoc install
```

### Direct Cabal Usage

You can also run directly with cabal:

```sh
cabal run aoc -- 01a src/01a/input.txt
```
