---
title: Notes on Fixed Dimension LPs
tags: alg, optimization
author: Yu Cong
---


This is my note on low-dimension linear programming. In September I'm working on the following small problem,

:::{.Problem title="Minimizing the Sum of Piecewise Linear Convex Functions" #probpwl}
Given $n$ piecewise linear convex functions $f_1,...,f_n:\R \to \R$ of total $m$ breakpoints, and $n$ linear functions $a_i\cdot x-b_i:\R^d\to \R$, find $\min_x \sum_i f_i(a_i\cdot x-b_i)$.
:::

which is highly related to algorithms for linear programming in low dimensions.

This can be solve in $O(2^{2^d}(m+n))$ through Megiddo's algorithm for multidimensional search problem. (see [my slides](/pdfs/LowdimLP-Megiddo.pdf))

I want to show that for general piecewise linear convex functions in $\R^d$, [my problem](#probpwl) can be formulated as a [LP-type](https://en.wikipedia.org/wiki/LP-type_problem) problem with low combinatorial dimension.

A failed attempt is trying to write $F=\sum_i f_i$. However, there may be too many breakpoints on $F$. (see [a previous post](/posts/2024-09-16-piecewise-linear.html)).

Another possible way is using some dimension reduction techniques [@grohe_dimension_2014]. I haven't read this yet...

some materials for Low dimension LP:

- chapter 20 of [*Geometric Approximation Algorithms*](https://sarielhp.org/book/) by Sariel Har-Peled.  <https://sarielhp.org/book/chapters/lp.pdf>
- the fastest deterministic algorithm for this problem [@chan_improved_nodate]
- my [slides](/pdfs/LowdimLP-Seidel.pdf) on Seidel's $O(d!n)$ algorithm 