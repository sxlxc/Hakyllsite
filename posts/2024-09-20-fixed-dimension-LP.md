---
title: Minimizing Sum of PWL Convex Functions
tags: alg, optimization
author: Yu Cong
lang: en
showtoc: true
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

## reflection
Multidimensional search is harder than LP-type problems, this question is no exception. Now there are three kinds of problems I am interested in.

1. Minimax parametric optimization
2. Multidimensional search problems
3. LP-type problems

What's the connections among them?...

:::{.Definition title="Minimax parametric optimization problem" #minmax}
Given a combinatorial maximization problem with a parameter. Find the parameter value minimizing the weight of a solution to the combinatorial maximization problem.
:::

:::{.Definition title="Multidimensional search problem" #multisearch}
Given a set of hyperplanes $\mathcal H$ in $\R^d$, and an oracle which answers the relative position of one hyperplane and a unknown fixed point $x^*\in \R^d$. Compute the relative position of every hyperplane in $\mathcal H$ and $x^*$ with as small number of oracle calls as possible.
:::

:::{.Definition title="LP-type problem" #lptype}
Given a set $S$ and a function $f$ from $S$ to a totally ordered set. $f$ has to satisfy two properties,

1. monotonicity: $\forall A\subseteq B\subseteq S, f(A)\leq f(B)\leq f(S)$,
2. locality: $\forall A\subset B\subset S$, consider any element $x\in S$, if $f(A)=f(B)=f(A+x)$, then $f(A)=f(B+x)$.
:::

Now there are some important concrete problem in the intersections.

### Euclidean one-centre problem

:::{.Problem title="Euclidean one-centre problem"}
Given $n$ points $V=\{v_1,\dots, v_n\}$ in $\R^d$, with weights $w_1,\dots,w_n$, find a point in $\R^d$ which has the minimal of the maximum weighted distance to all points in $V$, that is, compute $\min_x \max_{i} w_i^2(v_i-x)^2$.
:::

Dyer showed that this problem can be considered as a multidimensional search problem (in $\R^{d+1}$) in [@dyer_multidimensional_1986]. The conversion is not easy and not intuitive. Das et al. claimed that this problem is a LP-type problem with some additional constraints in [@das_linear_2020](I didn't read this carefully, it seems that they made the claim in the introduction but have never proven it). It is still unknown whether it is possible to formulate the weighted Euclidean one-centre problem in $\R^d$ as a LP-type problem with combinatorial dimension $O(d)$ (which is quite surprising...)

### Minimizing the sum of some pwl convex functions(my problem)

see [above](#probpwl) for the definition.

Clearly this is a minimax parametric optimization problem. Zemel also showed this is a multidimensional search problem with dimension $d$. We want to know that if this is a LP-type problem with combinatorial dimension $O(d)$...


It seems that recognizing LP-type is hard. A [lecture](https://ics.uci.edu/~eppstein/164/lecture10.pdf) on LP-type problems by David Eppstein.


------------

Here are some materials for Low dimension LP:

- chapter 20 of [*Geometric Approximation Algorithms*](https://sarielhp.org/book/) by Sariel Har-Peled.  <https://sarielhp.org/book/chapters/lp.pdf>
- the fastest deterministic algorithm for this problem [@chan_improved_nodate]
- my [slides](/pdfs/LowdimLP-Seidel.pdf) on Seidel's $O(d!n)$ algorithm 