---
title: "\"Gotchas\" in Combinatorial Optimization"
tags: optimization, alg
author: 赵亚杰, 丛宇
lang: en
draft: true
date: 2025-04-30
---

It would be fun to have a list of misleading ideas and traps in CO.

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