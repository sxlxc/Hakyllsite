---
title: Number of minimum circuits and cocircuits
tags: matroid
author: Yu Cong
lang: en
draft: true
date: 2025-11-10
showtoc: true
---

Refs:

(@regular) [On the Number of Circuits in Regular Matroids](https://arxiv.org/pdf/1807.05164)
(@cographic) [A polynomial bound on the number of light cycles in an undirected graph](https://www.sciencedirect.com/science/article/pii/002001909400202A)

# number of min-cuts in graph

Random contraction works. Consider graphic matroids. One can show that $\lambda\leq c \frac{|E|}{r}$. Then while contracting edges the probability of preserving certain min-cut is at least $\frac{r-c}{r}\cdot \frac{r-c-1}{r-1}\dots \frac{1}{c+1}\geq \frac{1}{r^c}$. For graphic matroids we have $c=2$. So the number of min-cuts is $O(n^2)$.

# number of minimum circuits in graph

Random contraction does not work. One needs to show $g\leq c\frac{|E|}{r}$ for girth $g$.
However, this is not true since there exists a cubic graph with $g\geq \frac{4}{3} \log n$.

The number of minimum circuits is $O(m^2)$. There is a proof in Refs(@cographic).

