---
title: Notes on Minimizing Sum of Piecewise Linear Convex Functions
tags: alg, optimization
author: Yu Cong
---


This is my note on low-dimension linear programming & color refinement algorithms. 

# the problem & failed attempts

In September I'm working on the following small problem,

:::{.Problem title="Minimizing the Sum of Piecewise Linear Convex Functions" #probpwl}
Given $n$ piecewise linear convex functions $f_1,...,f_n:\R \to \R$ of total $m$ breakpoints, and $n$ linear functions $a_i\cdot x-b_i:\R^d\to \R$, find $\min_x \sum_i f_i(a_i\cdot x-b_i)$.
:::

which is highly related to algorithms for linear programming in low dimensions.

This can be solve in $O(2^{2^d}(m+n))$ through Megiddo's algorithm for multidimensional search problem. (see [my slides](/pdfs/LowdimLP-Megiddo.pdf))

I want to show that for general piecewise linear convex functions in $\R^d$, [my problem](#probpwl) can be formulated as a [LP-type](https://en.wikipedia.org/wiki/LP-type_problem) problem with low combinatorial dimension.

A failed attempt is trying to write $F=\sum_i f_i$. However, there may be too many breakpoints on $F$. (see [a previous post](/posts/2024-09-16-piecewise-linear.html)).

Another possible way is using some dimension reduction techniques [@grohe_dimension_2014]. 

# color refinement for matrices

Given a initial coloring of vertices in a directed graph $G=(V,A)$, we want to compute the *coarsest regular congruent* coloring.

Colorings can be considered as equivalence relations on the vertices. An equivalence relation $R$ on $V$ is *congruent* if for all $u,v,w\in V$, [$(u,v)\in R$ and $(v,w)\in A$] implies that [$\exists v'\in V$ such that $(v,v')\in A$ and $(v',w)\in R$]. Note that this coincides with the general [definition of congruence relation](https://en.wikipedia.org/wiki/Congruence_relation) in algebraic structures.(We can copy each vertex #outdegree times to make $A$ an unary operation.) 

A coloring is *regular* if for any two vertices $u,v$, the number of successors in each color are the same. Consider two colorings $C_1,C_2$. $C_1$ is a refinement of $C_2$(or $C_2$ is coarser than $C_1$) if for any two vertices having the same color in $C_1$, they have the same color in $C_2$.

**Basic Lemma 1** in [@cardon_partitioning_1982] shows that the problem above is equivalent to the description in [this wiki page](https://en.wikipedia.org/wiki/Colour_refinement_algorithm).

The first quasilinear time algorithm for the color refinement problem on graphs is given in [@cardon_partitioning_1982], and later in [@paige_three_1987]. It is shown in [@berkholz_tightbound_2017] that $O((m+n)\log n)$ is the best possible running time. Also see [this](https://www.lics.rwth-aachen.de/global/show_document.asp?id=aaaaaaaaabbtcqu) for a nice survey on applications.

## connections

Now we add edge weights on the directed graph. Suppose all arcs have weight 1.
One can see that a congruent and regular coloring requires that two vertices have the same color iff for each color the total weight of arcs going to vertices in that color are the same. Slightly generalize this configuraton, we can consider arbitary arc weights.

------------

Here are some materials for Low dimension LP:

- chapter 20 of [*Geometric Approximation Algorithms*](https://sarielhp.org/book/) by Sariel Har-Peled.  <https://sarielhp.org/book/chapters/lp.pdf>
- the fastest deterministic algorithm for this problem [@chan_improved_nodate]
- my [slides](/pdfs/LowdimLP-Seidel.pdf) on Seidel's $O(d!n)$ algorithm 