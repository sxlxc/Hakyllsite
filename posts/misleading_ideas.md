---
title: "\"Gotchas\" in Combinatorial Optimization"
tags: optimization, alg
author: Yu Cong
lang: en
# draft: true
date: 2025-04-30
showtoc: true
---

\DeclareMathOperator{\polylog}{polylog}

It would be fun to have a list of misleading ideas and traps in CO. I will update this post for new problems.

# Polytope for s-t cut

\begin{equation}
\begin{aligned}
\sum_{e\in p} x_e &\geq 1   &   &\forall \text{ $s$-$t$ path $p$}\\
              x_e &\geq 0   &   &\forall e\in E
\end{aligned}
\end{equation}
Is this polytope integral?

Initially I thought, we have max flow min cut thm and flow polytope is integral and this path formulation of s-t cut should be the dual of flow problem and thus it is integral. However, hitting spanning tree (dual to tree packing) formulation has a integrality gap of 2. Make sense.

However, LP(1) is **not** [the dual of Max-flow](https://en.wikipedia.org/wiki/Max-flow_min-cut_theorem#Linear_program_formulation).

# Size of support

Given a linear program with a rank $r$ constraint matrix, what is the size of support of its optimal solution?

This is mentioned in [a previous post](/posts/basepacking.html). The description there is not precise.
Consider the following linear program,

\begin{equation*}
\begin{aligned}
\min&   &   c^Tx&   \\
s.t.&   &   Ax&\leq b\\
    &   &   x&\geq 0
\end{aligned}
\end{equation*}

Let $r$ be the rank of $A$. We may assume $b\geq0$. For any $c$ that this LP has a bounded solution, there must exist an optimal solution $x^*$ with support at most $r$.
There are at most $r$ tight constraints in $Ax\leq b$ and hence the optimal solution $x^*$ lies in the intersection of $\R^n_+$ and a rank $\geq n-r$ affine subspace, which is a convex polyhedron.
If the LP has a bounded solution, then the optimal $x^*$ must be some vertex of the polyhedron. To make a point in our affine subspace a vertex in the polyhedron, we need at least $n-r$ hyperplanes $x_i=0$, each of the hyperplanes gives us a zero coordinate.
Thus for any bounded solution, the support is at most $r$.

What about integer programs? One may think that the support should also be at most $r$, since the 0s in the optimal solution of its linear relaxation are integers. But rounding the fractional coordinates may break the feasibility. The currently best upperbound is roughly $m (3\|A\|_1+\sqrt{\log( \|A\|_1 )})$ [@Berndt_Brinkop_Jansen_Mnich_Stamm_2023].
