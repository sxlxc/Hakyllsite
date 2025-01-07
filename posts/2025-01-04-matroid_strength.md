---
title: On matroid base packing and covering
tags:  matroid, optimization, combinatorics
author: Yu Cong
lang: en
draft: true
---

\DeclareMathOperator{\B}{\mathcal B}
\DeclareMathOperator{\I}{\mathcal I}
\DeclareMathOperator{\polylog}{polylog}

Recently(actually, not that recent), I have been interested in the fractional version of matroid base packing and covering problems.

As far as I know, there are few text books in combinatorial optimization cover topics in matroid base packing, while matroid base covering([matroid partition problem](https://en.wikipedia.org/wiki/Matroid_partitioning)) is everywhere.
Packing and covering of trees in graphs is discussed in chapter 51 of *Combinatorial optimization Polyhedra and Efficiency* by Alexander Schrijver.

# Base packing & base covering

::: Problem
Given a matroid $M=(E,\I)$ and its bases $\B$, find

1. the minimum number of bases whose union is $E$(base covering); or 
2. the maximum number of pairwise disjoint bases(base packing).
:::

These problems can be formulated with the following integer programs,
base packing:
\begin{align*}
\max \; &\sum_{B\in\B} x_B\\
s.t. \quad \sum_{B:e\in B} x_B &\leq 1 \quad \forall e\in E\\
x_B&\in \set{0,1}
\end{align*}

base covering:
\begin{align*}
\min \; &\sum_{B\in\B} x_B\\
s.t. \quad \sum_{B:e\in B} x_B &\geq 1 \quad \forall e\in E\\
x_B&\in \set{0,1}
\end{align*}

In general integer programming are hard. Here the base packing and covering problems have
exponential number of variables. If nothing is known for these two problems, people 
natually study their linear relaxation.

> Just a note. It is widely known that any linear program with a rank-$m$ constraint 
> matrix has a support with size no larger than $m$. For similar problem on integer programming,
> one might think that there is also a small support based on the knowledge that the optimal 
> solution for the integer program is simply a integer point inside the feasible region.
> However, the size of support for integer programs is not that small. Currently the best known 
> upperbound is roughly $m\cdot \polylog(\|{A}\|_1)$, see [this paper](https://drops.dagstuhl.de/storage/00lipics/lipics-vol283-isaac2023/LIPIcs.ISAAC.2023.13/LIPIcs.ISAAC.2023.13.pdf).

Actually these two problems are not hard on general matroids. They can both be solved in polynomial number of 
independence oracle calls. <span style="color:red">is it?...How to do packing?</span>

## Connections

# Matroid strength and density

:::{.Theorem}
A matroid is uniformly dense if and only if $\sigma(M)=\alpha(M)$
:::

:::{.Proof}
hmm...
:::