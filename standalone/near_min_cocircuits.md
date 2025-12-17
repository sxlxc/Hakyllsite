---
title: Number of minimum circuits and cocircuits
tags: matroid
author: Yu Cong
lang: en
draft: true
date: 2025-11-10
showtoc: true
---

# \# α-min-cuts in graph

Random contraction works. Consider graphic matroids. One can show that $α\lambda\leq αc \frac{|E|}{r}$. Then while contracting edges the probability of preserving certain min-cut is at least $\frac{r-αc}{r}\cdot \frac{r-αc-1}{r-1}\dots \frac{1}{αc+1}\geq \frac{1}{r^{αc}}$. For graphic matroids we have $c=2$ and $r<n$. So the number of min-cuts is $O(n^{2α})$.

# \# α-minimum circuits in graph

Random contraction does not work. One needs to show $g\leq c\frac{|E|}{r}$ for girth $g$.
However, this is not true since there exists a cubic graph with $g\geq \frac{4}{3} \log n$.

Note that we assume graphs to be simple since otherwise the girth would be 1 or 2 and the number of minimum circuits can be upperbounded by $m$ or $m^2$.

Subramanian first proved an upperbound of $2m^2/g$ on the number of minimum circuits [@Subramanian_1995]. The proof is quite interesting. 
Consider a robot moving along a simple cycle of length $\ell$. 
There are $2\ell$ ways for it to traverse the cycle (consider the starting point and the direction).

At time 0 we assign 2 robots at the middle of each edge facing opposite directions. Robots start to move along edges at unit speed. When a robot reaches a vertex, it splits into several copies that then leave the vertex in different directions. Distince robots transverse distinct trajectories and the trajectory of each robot must be a simple cycle of length at most $\alpha g$. A robot fails to meet these contraints destroys itself immediately. Robots stop moving when completing a simple cycle. One can see that at time $\alpha g$ every robot stops moving. Let $g^-$ be a quantity slightly smaller than $g$. The claim is that for any $j\in \Z_{\geq 0}$, at time $jg^-/2$, there will be at most $(2m)^{j+1}$ robots on the graph.

$j=0$ holds trivially. Suppose that the claim is true for $\leq j-1$ and consider time interval $\left[ (j-1)g^-/2,jg^-/2\right]$. Note that the trajectories of one robot and its copies must be a tree since otherwise there is a cycle of length $g^-$ in the graph. Then one can see that on each edge there cannot be two copies facing the same direction. Thus each robots splits into $2m$ copies in this time interval and the claim follows.

A recent paper [@Aissi_Baiou_Barahona_2025] slightly improves the bound. For cuts integer edge weights can be converted to parallel edges. However, for cycles this is not the case. On unweighted graphs the number of minimum circuits is called the kissing number. The authors mention that there are existing works on kissing numbers, roughly ${\rm Kiss}(G)\leq 2m(m-n+1)/g$.
In [@Aissi_Baiou_Barahona_2025] the improved bound is $n^4/4$ when the weighted girth is even and $n^3/2$ when the weighted girth is odd.
These upperbounds are asymptotically tight. Consider $K_{n,n}$ with unit cost, the number of $C_4$ in it is $\binom{n}{2}^2$. For odd girth consider $K_n$, the number of $C_3$ is $\binom{n}{3}$.

# \# α-minimum circuits in regular matroids

[@Gurjar_Vishnoi_2021] showed that the number of $\alpha$-minimum circuits in a regular matroid is $m^{O(\alpha^2)}$.

- can we improve the bound in [@Gurjar_Vishnoi_2021] to $O(m^{c\alpha})$?
- is it possible to find a unified way to upperbound the number of minimum circuit in graphic and cographic matroids?

A recent paper[^1] shows that there is no polynomial algorithm approximating the girth or the cogirth of binary matroids to any constant factor.


[^1]: [PCP-free APX-Hardness of Nearest Codeword and Minimum Distance](https://arxiv.org/abs/2503.11131), merged into [another paper](https://arxiv.org/abs/2410.02636) in FOCS'25