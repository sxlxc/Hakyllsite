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

We will see that there is a matroid with constraints as its groundset.[^1]

Let $S$ be the set of constraints. Take a subset $T\subseteq S$ of constraints.
We say that $T$ **spans** a constraint $s\in S$ if every filled sudoku grid (with fixed dimension) satisfies $T$ also satisfies $s$. Then $\span(T)$ is the maximal set of satisfied constraints if we check $T$.

::: {.Theorem #spanmatroid}
Let $S$ be a finite set. A function $\span:2^S\to 2^S$ is the span function of a matroid iff

1. $T\subseteq \span(T)$ for all $T\subseteq S$;
2. if $T,U\subseteq S$ and $U\subseteq \span(T)$, then $\span(U)\subseteq \span(T)$;
3. if $T\subset S, t\in S\setminus \span(T)$, and $s\in \span(T+t)\setminus \span(T)$, then $t\in \span(T+s)$.
:::

It follows immediately from the definition of our span function that (1.) and (2.) are satisfied.
We need to prove (3.)

Let's forget about matroids for now and try to understand why some constraints are not necessary.
A vague idea is that the sudoku grid structure comes with some information about the occurrence of the correctly filled numbers in some cells.

The definition of "satisfy" does not reveal information of the sudoku structure. To solve this issue, consider a linear mapping $\phi_G:S\to \Z^{n^2}$ for every filled sudoku grid $G$. For a cell $x\in S$, we define $\phi_G(x)$ to be an $n^2$ dimensional vector, which indicates the number of occurrence of each number in $[n^2]$. Then $x$ is satisfied by sudoku grid $G$ iff $\phi_G(x)=\mathbf 1$.

Note that our linear mapping $\phi$ works on linear combinations of $S$, so we slightly change its type to $\phi_G:V\to \Z^{n^2}$ where $V$ is the linear space with basis $S$.
Consider vectors $\sum_{j\in [n]} r_{ij}$ (sum of rows in a band $i$) and $\sum_{j\in [n]} b_{ij}$ (sum of blocks in the same band). They cover exactly the same set of cells. So for any grid $G$, $\phi_G(\sum_{j\in [n]} r_{ij} - \sum_{j\in [n]} b_{ij})=0$. The same happens for columns and blocks in the same stack. We denote by $V_0$ the subspace of $V$ in which $\phi_G$ is $0$ for all grid $G$.

::: {.Lemma #lemspan}
Let $T\subset S$ be a set of constraints and let $s\notin T$ be a constraint.
Then $T$ spans $s$ if and only if $s\in \span(T\cup V_0)$.[^2]
:::

[^2]: Note that $\span(T\cup V_0)$ is the subspace of $V$ spanned by vectors in $T\cup V_0$.

::: Proof
Note that $T$ spans $s$ means that for any sudoku grid $G$ such that $\phi_G(t)=\mathbf 1$ for all $t\in T$, we have $\phi_G(s)=\mathbf 1$.

**the if part** Since $s\in \span(T\cup V_0)$, we can write $s$ as $\sum_i \alpha_i t_i+y$ where $t_i\in T$ and $y\in V_0$.
It follows from the linearity of $\phi$ that $\phi_G(s)=\sum_i \alpha_i \mathbf{1}+0$ holds for every grid satisfying $T$. Then we pick a correctly filled grid $G^*$.
One can see that $\mathbf{1}=\phi_G(s)= (\sum_i \alpha_i) \mathbf{1}$ which implies $\sum_i \alpha_i=1$.
Then we have $\phi_G(s)=1$.

**the only if part** Suppose by contradiction that $s\notin \span(T\cup V_0)$ and for every $G$ satisfying $T$ we have $\phi_G(s)=\mathbf 1$. We can write $s=\sum_i \alpha_i t_i + \sum_j \beta_j s_j+y$ where $s_j$'s are in the basis $S$ but not in the span.
Again there is a $G^*$ that satisfies every constraint and we have $\sum_i \alpha_i+\sum_j \beta_j=1$.
Now consider any grid $G$ that satisfies $T$.
We have $\phi_G(s)=\sum_i \alpha_i \mathbf{1} + \sum_j \beta_j s_j = \mathbf{1}$.
One can see that the equation holds if and only if every $s_j$ is $\mathbf 1$, which contradicts the fact that $s\notin \span(T+V_0)$.
:::

::: Remark
[@lemspan] shows that the span function in $V$ is the same as the span function we will use in [@spanmatroid] and that $\span(T)$ is the span function of a linear matroid.
:::

# sudoku matroid on graphs

[^1]: The idea is from the joint work of authors in the mathoverflow post. I think this is a simpler and more intuitive proof.