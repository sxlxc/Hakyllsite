---
title: Breaking vertex connectivity
tags: alg, combinatorics, optmization, 
lang: en
author: Yu Cong
draft: true
---

As a natural generalization of min-cut, the following problem seems interesting to me,

:::{.Problem #prob1}
Given a graph $G=(V,E)$ which is $k$-connected. What is the minimum set of edges whose removal breaks $k$-vertex connectivity?
:::

# Checking $k$-vertex connectivity

Checking if a given graph is $k$-vertex connected [can be done in polynomial time](https://en.wikipedia.org/wiki/K-vertex-connected_graph#Computational_complexity).
By [Menger's theorem](https://en.wikipedia.org/wiki/Menger%27s_theorem) we need to check if for every vertex pair $(s,t)$ there are at least $k$ vertex disjoint paths (excluding $s$ and $t$) connecting $s$ and $t$. The number of vertex disjoint paths between $s$ and $t$ can be easily computed through max flow. Duplicate every vertex except $s$ and $t$ and connect an edge with capacity 1 between every pair of new vertices. The capacity is 1 for all edges. Since the constraint matrix of flow problems is TU, maximizing the flow gives the number of internally disjoint paths between $s$ and $t$.

> Instead of computing the flow for every pair, if we want one flow that gets the demand for every vertex pair $(i<j)$, the problem becomes much harder. This is [Multi-commodity flow problem](https://en.wikipedia.org/wiki/Multi-commodity_flow_problem).

Currently the fastest algorithm for computing vertex connectivity is [@HENZINGER2000222].
There is a nice table for a summary of connectivity related algorithms.

![image courtesy of Abdol–Hossein Esfahanian. [link](http://www.cse.msu.edu/~esfahani/book_chapter/Graph_connectivity_chapter.pdf)](/images/vertex_connectivity_cut/table.png)

[@HENZINGER2000222] appears in the last line. The conference version was published on FOCS96.

# Minimum cut for edge connectivity


# Connectivity interdiction

Connectivity interdiction is first studied by Zenklusen [@zenklusen_connectivity_2014]. 

::: Problem
Given a graph $G=(V,E)$ and costs $c:E\to \Z_+$ and weights $w:E\to \Z_+$ and a budget $B\in \Z_+$, find the edge set $R$ such that $c(R)\leq B$ and that minimizes the $w$-weighted min cut in $(V,E\setminus R)$.
:::