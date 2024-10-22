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

# color refinement on matrices

> I do not have high expectation on this method, since I need to show that color refinement indeed reduce the dimension from $n+d$ to $d$ and that the reduction can be done in linear time.

Given a initial coloring of vertices in a directed graph $G=(V,A)$, we want to compute the *coarsest regular congruent* coloring.

Colorings can be considered as equivalence relations on the vertices. An equivalence relation $R$ on $V$ is *congruent* if for all $u,v,w\in V$, [$(u,v)\in R$ and $(v,w)\in A$] implies that [$\exists v'\in V$ such that $(v,v')\in A$ and $(v',w)\in R$]. Note that this coincides with the general [definition of congruence relation](https://en.wikipedia.org/wiki/Congruence_relation) in algebraic structures.(We can copy each vertex #outdegree times to make $A$ an unary operation.) 

A coloring is *regular* if for any two vertices $u,v$, the number of successors in each color are the same. Consider two colorings $C_1,C_2$. $C_1$ is a refinement of $C_2$(or $C_2$ is coarser than $C_1$) if for any two vertices having the same color in $C_1$, they have the same color in $C_2$.

**Basic Lemma 1** in [@cardon_partitioning_1982] shows that the problem above is equivalent to the description in [this wiki page](https://en.wikipedia.org/wiki/Colour_refinement_algorithm).

The first quasilinear time algorithm for the color refinement problem on graphs is given in [@cardon_partitioning_1982], and later in [@paige_three_1987]. It is shown in [@berkholz_tightbound_2017] that $O((m+n)\log n)$ is the best possible running time. It is also shown in [@kiefer_et_alicalp20] that for any number of vertices there exists a graph which requires at least $n-2$ iterations to reach a stable coloring(note that the upperbound is $n-1$). See [this](https://www.lics.rwth-aachen.de/global/show_document.asp?id=aaaaaaaaabbtcqu) for a nice survey on applications.

## connections

### color refinement on graphs $\to$ on matrices
Now we add edge weights on the directed graph. Suppose all arcs have weight 1.
One can see that a congruent and regular coloring requires that two vertices have the same color iff for each color the total weight of arcs going to vertices in that color are the same. Slightly generalize this configuraton, we can consider arbitrary arc weights.

Now color refinement on matrices are almost the same as doing color refinement on the incidence matrix of a weighted digraph. However, not every matrix is square. For any matrix $A\in \R^{v\times w}$, we consider it as a bitartite graph $G=(V\sqcup W,A)$, where $|V|=v$ and $|W|=w$. Then $A_{ij}=w(i, j)$ if $(i, j)$ is an arc in $G$ and 0 otherwise.

### color refinement on matrices $\to$ dimension reduction of LPs

This part is not intuitive and complicated. I think the ESA 14 paper [@grohe_dimension_2014] is very concise (compared to the arxiv version), however still takes four and a half pages to explain this part. So I will just briefly explain the idea.

This connection is based on an important theorem, stating that the color refinement of a matrix $A$ has strong relation with the fractional automorphism of $A$. To do the reduction, we first compute a color refinement of $A$. Based on the partitions of columns and rows of $A$ in the color refinement(partition matrices), we can conpute the **factor matrix** of $A$, denoted by $[A]$, which is small than $A$. Then finally, the authors proved a reduction lemma, which shows that the optimal solution to factor matrix of the entire LP(I don't know the exact name, just the matrix one uses in the simplex method) is a linear mapping of the optimal solution of the original LP.

## is it useful?

The reduction looks clever and has a wide application. However, as far as I know, it does nothing on my problem. I don't think the matrix in my problem is special enough to allow color refinement algorithms run in linear time on it. Also color refinement does not necessarily partition all $f_i$ columns in one part. `¯\_(⊙︿⊙)_/¯`

------------

Here are some materials for Low dimension LP:

- chapter 20 of [*Geometric Approximation Algorithms*](https://sarielhp.org/book/) by Sariel Har-Peled.  <https://sarielhp.org/book/chapters/lp.pdf>
- the fastest deterministic algorithm for this problem [@chan_improved_nodate]
- my [slides](/pdfs/LowdimLP-Seidel.pdf) on Seidel's $O(d!n)$ algorithm 