---
title: LP with box constraints
tags: optimization
lang: en
author: Yu Cong
draft: true
date: 2025-03-13
---

In [@chalermsook_approximating_2022] the following edge-connectivity related problem is studied.

::: Problem
Given a undirected weighted graph $G=(V,E)$, a weight function $w:E\to \Z_+$ and an integer $k$, find a $k$-edge-connected spanning subgraph of $G$ with the minimum weight. 
:::

Consider the following LP relaxation,

\[\begin{align*}
    \min& \quad \sum_{e\in E} w(e)x_e\\
    s.t.& \quad \sum_{e\in \delta(S)}x_e\geq k  &&\forall S\subset V\\
        & \quad 0\le x_e \le 1  &&\forall e\in E
\end{align*}\]

Note that $x_e\le 1$ is necessary since each edge can only be chosen once. In the paper the authors mentioned that this kind of constraints are called box constraints and they usually make LP difficult. There is also a box-free version of LP relaxation,

\[
\begin{align*}
    \min& \quad \sum_{e\in E} w(e)x_e\\
    s.t.& \quad \sum_{e\in C\setminus S}x_e\geq k-|S|  &&\forall \text{ cut } C\\
    & &&\forall S\in \set{F: |F|\le k-1 \wedge  F\subset C}\\
        & \quad \phantom{\sum_{e\in C\setminus S}}  x_e\ge 0  &&\forall e\in E
\end{align*}
\]
which is box-free but includes exponentially many extra constraints. I will call the first LP boxLP and the second one boxlessLP. Any feasible solution to the boxLP is also feasible in the boxlessLP. For any $x_e>1$ in a feasible solution of boxlessLP, we consider those cuts containing $e$. For such a cut $C$, $\sum_{f\in C\setminus e} x_f\geq k-1$ and thus $\sum_{e\in C} x_e>1$. Since this holds for any cut $C$ containing $e$, we can certainly decrease $x_e$ for a smaller objective. Hence, in the optimal solution to boxlessLP, every $x_e$ is less than or equal to 1. In fact, one can see from the proof that enumerating all singletons $F=\set{f}$ is sufficient.

TODO:

1. why do box constraints make LP hard?
2. ~~prove there formulations are equivalent~~
3. is this method widely used?