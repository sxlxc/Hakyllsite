---
title: Points in "general" position
tags: combinatorics, CG
author: Yu Cong
lang: en
draft: true
showtoc: true
date: 2025-06-18
---

# General Position

An set of 2D points is said to be in general position if any 3 points are not collinear. Given a set of points $P$ in the plane, find the largest subset $S\subset P$ in general position.

## Related Problems/Works

- A recent paper [@balogh_2025] studied the "typical size" of the largest point set in general position in which each points is taken from $\F_q^3$ iid with probability $p$. "Typical size" means we want upper and lower bounds with high probability. Piror to this work, [@Roche-Newton_Warren_2022] gave an upperbound for the case of $\F^2_q$.
A set of points $S\subset \F_q^d$ is in general position if there are no $d+1$ points in $S$ contained in a $(d-1)$-dimensional affine subspace. 
[@balogh_2025] also mentioned some non-random bounds. The max size of a point set in general position in $\F^d_q$ is at least $q$ and at most $(1+o(1))q$.
- [@froese_finding_2017] studied the problem of finding the largest subset of points in general position in a point set $P\subset \mathbb{Q}^2$. They showed that this problem is NP-hard, APX-hard and no $2^{o(n)}n^{O(1)}$-time alg under ETH. The reduction is from graph independent set.
- [This blog post](https://adamsheffer.wordpress.com/2018/05/31/points-in-general-position/) discusses constructions of general-position point sets $\subset \Z^2$ with some extremal structures.
- [@Suk_Zeng_2023] improved the upperbound of the max number $a(d,k,n)$ of points selected from $[n]^d$ such that no $k+2$ members lie on a $k$-flat.

# Integer Program and Duality

Let $P$ be a set of $n$ points in the plane.
We can define $\binom{n}{2}$ lines and write a point-line incidence matrix $A_{\binom{n}{2}\times n}$ which indicates if point $p$ is on the line defined by $\{p_1,p_2\}$.
Now we can write an IP for finding the largest subset of $P$ in general position.

\[
\begin{aligned}
\max&   &   \sum_p &x_p  &   &\\
s.t.&   &   \sum_p &A_{i,p}x_p \leq 2 & &\forall i\in \left[\binom{n}{2}\right]\\
    &   &   &x_p\in \set{0,1}
\end{aligned}
\]

Consider its LP dual,

\[
\begin{aligned}
\min&   &   \sum_i &2y_i    &   &\\
s.t.&   &   \sum_i &A_{i,p}y_i\geq 1    &   &\forall p\in P\\
    &   &       &y_i\in \set{0,1}
\end{aligned}
\]

One can see that the dual is finding the smallest number of lines to cover points in $P$. Denote by $\sigma$ the max number of general position points in $P$ and let $\alpha$ be the minimum line covering number. (Borrow these symbols from matroid strength and density.) There are some results in [@froese_finding_2017].

::: Observation
$\sqrt{\alpha}< \sigma \leq 2\alpha$.
:::

::: Proof
The second inequality directly follows from LP duality. 
For the first inequality, note that the largest set of points in general position defines $\binom{\sigma}{2}$ lines and any other point must lie on one of these lines. Thus these $\binom{\sigma}{2}$ lines cover every points in $P$. We have $\alpha \leq \binom{\sigma}{2} < \sigma^2$.
:::

## Approximation

Now I am interested in the gaps of above IPs and approximations algorithms for $\sigma$. 
For the $n\times n$ grid case there is a $3/4$ approximation [@HALL1975336].
It seems that in general cases the currently best known polynomial time approximation ratio is $\sqrt{\sigma}$ ? (see Cheng Cao's [master thesis](https://core.ac.uk/reader/147229038) and chatper 9.5 of David Eppstein's [book](https://ics.uci.edu/~eppstein/forbidden/)) One can easily prove that the simple greedy method gives a $\sqrt{\sigma}$ approximation.

# Open Problems

1. [[No Three in Line]{.sc} problem](https://en.wikipedia.org/wiki/No-three-in-line_problem): find the max number of points that can be placed in the $n\times n$ grid such that there is no collinear triple. An easy upperbound is $2n$. The currently best lowerbound is $\frac{3}{2}n-o(n)$ [@HALL1975336]. The main interest here is to get better bounds.
2. ...