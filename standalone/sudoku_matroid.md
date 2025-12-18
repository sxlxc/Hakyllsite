---
title: Sudoku Matroids
author: Yu Cong
tags: matroid
lang: en
date: 2025-12-16
showtoc: true
---

In a 3×3 sudoku there are 9×9 cells and 3×9 constraints (9 rows, 9 columns and 9 blocks).
Given a filled sudoku, how many constraints are necessary to verify that the solution is correct?

<!--This is a note on Emil Jeřábek's answer in the mathoverflow link.-->

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

Note that a $n\times n$ sudoku contains $n$ bands, $n$ stacks and $n$ blocks. So there are $n^4$ cells and each cell can be filled with some number in $[n^2]$.

# the hidden matroid

<!-- I don't understand the original proof... try to prove it myself -->
We will see that there is a matroid with constraints as its groundset.
Let $S$ be the set of constraints. Take a subset $T\subseteq S$ of constraints.
We say that a constraint $s\in S$ is in the span of $T$ if every filled sudoku grid (with fixed dimension) satisfies $T$ also satisfies $s$. Then $\span(T)$ is the maximal set of satisfied constraints if we check $T$.

::: Theorem
Let $S$ be a finite set. A function $\span:2^S\to 2^S$ is the span function of a matroid iff

1. $T\subseteq \span(T)$ for all $T\subseteq S$;
2. if $T,U\subseteq S$ and $U\subseteq \span(T)$, then $\span(U)\subseteq \span(T)$;
3. if $T\subset S, t\in S\setminus \span(T)$, and $s\in \span(T+t)\setminus \span(T)$, then $t\in \span(T+s)$.
:::

It follows immediately from the definition of our span function that (1.) and (2.) are satisfied.
We need to prove (3.)

# sudoku matroid on graphs