---
title: Sudoku Matroids
author: Yu Cong
lang: en
date: 2025-12-16
showtoc: true
---

This is a note on Emil Jeřábek's answer in the mathoverflow link.

Refs:

- <https://matroidunion.org/?p=1070>
- <https://mathoverflow.net/questions/129143>

We need some sudoku notations.

```
       [3x3 sudoku]
      +---+---+---+
  +---+-B | B | B |  ← band 1 (each 3 rows)
  ↓   +---+---+---+
Block | B | B | B |  ← band 2
  ↑   +---+---+---+
  +---+-B | B | B |  ← band 3
      +---+---+---+
        ↑   ↑   ↑
          stacks (each 3 columns)
```

Note that a $n\times n$ sudoku contains $n$ bands, $n$ stacks and $n$ blocks. So there are $n^4$ grids.

# the hidden matroid

# sudoku matroid on graphs