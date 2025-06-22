---
title: Max number of points in "general" position
tags: combinatorics, CG
author: Yu Cong
lang: en
draft: true
showtoc: true
date: 2025-06-18
---

# General Position

An set of 2D points is said to be in general position if any 3 points are not collinear. Given a 2D area $D$, the max number of points one can place in $D$ in general position is infinity. However, what if one relaxes the collinear constraints to "seemingly collinear"? 

More precisely, 3 points $(x_i,y_i)$ are said to be "seemingly collinear" if there is a linear function $f(x)$ fitting these points with error $\sum_{i\in [3]}(y_i-f(x_i))^2 \leq c$ for some global constant $c$. A set of 2D points is "seemingly in general position" if it does not contain any "seemingly collinear" triples.

::: Problem
Given a 2D area $D$, find the max number of points one can place in $D$ such that the points are "seemingly in general position".
:::

## Related Problems/Works

- A recent paper [@balogh_2025] studied the "typical size" of the largest point set in general position in which each points is taken from $\F_q^3$ iid with probability $p$. "Typical size" means we want upper and lower bounds with high probability. Piror to this work, [@Roche-Newton_Warren_2022] gave an upperbound for the case of $\F^2_q$.
A set of points $S\subset \F_q^d$ is in general position if there are no $d+1$ points in $S$ contained in a $(d-1)$-dimensional affine subspace. 
[@balogh_2025] also mentioned some non-random bounds. The max size of a point set in general position in $\F^d_q$ is at least $q$ and at most $(1+o(1))q$.
- [@froese_finding_2017] studied the problem of finding the largest subset of points in general position in a point set $P\subset \mathbb{Q}^2$. They showed that this problem is NP-hard, APX-hard and no $2^{o(n)}n^{O(1)}$-time alg under ETH.
- [This blog post](https://adamsheffer.wordpress.com/2018/05/31/points-in-general-position/) discusses constructions of general-position point sets $\subset \Z^2$ with some extremal structures.
- [@Suk_Zeng_2023] improved the upperbound of the max number $a(d,k,n)$ of points selected from $[n]^d$ such that no $k+2$ members lie on a $k$-flat.