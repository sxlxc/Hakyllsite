---
title: Matroid circuit packing and covering
tags:  matroid, optimization, combinatorics
author: Yu Cong, with the help of Kangyi Tian
lang: en
draft: true
showtoc: true
date: 2025-06-15
---

In the [previous post](/posts/basepacking1.html), we mainly focus on the algorithmic part of integral and fractional base packing and base covering. In this post we consider packing and covering of matroid circuits.

# Packing/Covering Defect

Seymour [@seymour_packing_1980] proved the following theorem.

::: Theorem
Let $M=(E,I)$ be a matroid without coloop. Then one has
\[\theta(M)-\kappa(M)\leq r^*(M)-\nu(M),\]
where $\theta(M)$ is the minimum number of circuits whose union is $E$,
$\kappa(M)$ is the number of connected components in $M$,
$r^*$ is the corank and
$\nu(M)$ is the max number of disjoint circuits.
:::

The left hand side $\theta(M)-\kappa(M)$ is called the *circuit covering defect* and the right hand side $r^*(M)-\nu(M)$ is called the *circuit packing defect*. I guess the name "covering defect" comes from the fact that $\theta(M)-\kappa(M)$ is the gap between the circuit covering number and a lowerbound $\kappa(M)$. $\kappa(M)\leq \theta(M)$ since there is no circuit containing two elements in different components. The packing defect is the set-point dual of the covering version. To see the duality, one can write $\kappa(M)$ as the max size of $X\subset E$ such that $|C\cap X|\leq 1$ for all circuit $C$ and write $r^*(M)$ as the minimum size of $X\subset E$ such that $|X\cap C|\geq 1$ for all circuit $C$.

# Complexity

Computing the corank $r^*$ and the component number $\kappa(M)$ is easy. What about $\theta(M)$ and $\nu(M)$?

The problem of determining if a sparse split graph (a special case of chordal graphs) can have its edges partitioned into edge-disjoint triangles is NP-complete [@feder_packing_2012]. So finding $\theta(M)$ and $\nu(M)$ is NP-hard even for some special graphic matroids.

# Cycle Double Cover

[Cycle double cover conjecture](https://en.wikipedia.org/wiki/Cycle_double_cover) is a famous unsolved problem posed by W. T. Tutte, Itai and Rodeh, George Szekeres and Paul Seymour.
The cycle double cover conjecture asks whether every bridgeless undirected graph has a collection of cycles such that each edge of the graph is contained in exactly two of the cycles.

<!-- read [@Zhang_CDC_2016] and [@ding_packing_2009] -->
[@Zhang_CDC_2016] is a nice survey. However, there is little discussion about (even simplier version of) circuit double cover on some special case of matroids. 
For example, [this question](https://math.stackexchange.com/questions/1835067/multi-cover-a-matroid-with-circuits) on math.sx is a relaxation of faithful CDC on matroids.

::: {.Problem title="Not so faithful circuit cover"}
Given a matroid $M=(E,\mathcal I)$ and a non-negative integral weight function $w:E\to \Z_{\geq 0}$, decide if there is a multiset of circuits of $M$ such that each element in $E$ is covered by at least 1 and at most $w(e)$ circuits in the multiset.
:::

[@ding_packing_2009] studied a related (and seemingly simplier) optimization variant. How many circuits can we pack with element capacity $k w(e)$?

\begin{equation*}
\begin{aligned}
\nu_{k,w}=\max&   &   \sum_C x_C&    &   &\\
s.t.&   &   \sum_{C:e\in C} x_C &\leq k w(e)    &   &\forall e\in E\\
    &   &                   x_C &\in \Z_{\geq 0}
\end{aligned}
\end{equation*}

\begin{equation*}
\begin{aligned}
\tau_{k,w}=\min&   &   \sum_e w(e)&y_e    &   &\\
s.t.&   &   \sum_{e\in C} y_e &\geq k    &   &\forall \text{ circuit $C$}\\
    &   &                   y_e &\in \Z_{\geq 0}
\end{aligned}
\end{equation*}

Clearly the linear relaxation of $\nu_{k,w}$ and of $\tau_{k,w}$ are LP dual of each other and have the same optimum. For what class of matroids do we have equality for the integral version, $\nu_{k,w}=\tau_{k,w}$ ?