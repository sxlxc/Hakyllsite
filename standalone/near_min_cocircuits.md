---
title: Number of minimum circuits and cocircuits
tags: matroid
author: Yu Cong
lang: en
draft: true
date: 2025-11-10
showtoc: true
---

hmm... Indians like this kind of problem...

# \# α-min-cuts in graph

Random contraction works. Consider graphic matroids. One can show that $α\lambda\leq αc \frac{|E|}{r}$. Then while contracting edges the probability of preserving certain min-cut is at least $\frac{r-αc}{r}\cdot \frac{r-αc-1}{r-1}\dots \frac{1}{αc+1}\geq \frac{1}{r^{αc}}$. For graphic matroids we have $c=2$ and $r<n$. So the number of min-cuts is $O(n^{2α})$.

# \# α-minimum circuits in graph

Random contraction does not work. One needs to show $g\leq c\frac{|E|}{r}$ for girth $g$.
However, this is not true since there exists a cubic graph with $g\geq \frac{4}{3} \log n$.

Subramanian first proved an upperbound of $2m^2/g$ on the number of minimum circuits [@Subramanian_1995]. The proof is quite interesting. 
Consider a robot moving along a simple cycle of length $\ell$. 
There are $2\ell$ ways for it to traverse the cycle (consider the starting point and the direction).

At time 0 we assign 2 robots at the middle of each edge facing opposite directions. Robots start to move along edges at unit speed. When a robot reaches a vertex, it splits into several copies that then leave the vertex in different directions. Distince robots transverse distinct trajectories and the trajectory of each robot must be a simple cycle of length at most $\alpha g$. A robot fails to meet these contraints destroys itself immediately. Robots stop moving when completing a simple cycle. One can see that at time $\alpha g$ every robot stops moving. Let $g^-$ be a quantity slightly smaller than $g$. The claim is that for any $j\in \Z_{\geq 0}$, at time $jg^-/2$, there will be at most $(2m)^{j+1}$ robots on the graph.

$j=0$ holds trivially. Suppose that the claim is true for $\leq j-1$ and consider time interval $\left[ (j-1)g^-/2,jg^-/2\right]$. Note that the trajectories of one robot and its copies must be a tree since otherwise there is a cycle of length $g^-$ in the graph. Then one can see that on each edge there cannot be two copies facing the same direction. Thus each robots splits into $2m$ copies in this time interval and the claim follows.

[@Aissi_Baiou_Barahona_2025] slightly improves the bound.

# \# α-minimum circuits in regular matroids

[@Gurjar_Vishnoi_2021] showed that the number of $\alpha$-minimum circuits in a regular matroid is $m^{O(\alpha^2)}$.