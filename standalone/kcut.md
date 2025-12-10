---
title: k-cut
author: Yu Cong
lang: en
date: 2025-12-10
showtoc: true
---

:::{.Problem title="k-cut"}
Given a connected graph $G=(V,E)$ and edge capacity $c:E\to \R_{\geq 0}$, find the minimum-capacity edge set $X\subset E$ such that $(V,E\setminus X)$ has at least $k$ components.
:::

# Exact Algorithms

# Approximations

## greedy

Saran and Vazirani showed a $(2-\frac{2}{k})$-approximate $k$-cut can be obtained by $O(k)$ minimum cut computations [@saran_cuts_1995].