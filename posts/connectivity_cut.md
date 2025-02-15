---
title: Min-cut for certain level of connectivity
tags: alg, combinatorics, optmization, 
lang: en
author: Yu Cong
draft: true
date: 2025-02-13
---

As a natural generalization of min-cut, the following problem seems interesting to me,

:::{.Problem #prob1}
Given a graph $G=(V,E)$ and an integer $k$. What is the minimum edge set whose removal breaks $k$-vertex connectivity?
:::

# Checking $k$-vertex connectivity

Checking if a given graph is $k$-vertex connected [can be done in polynomial time](https://en.wikipedia.org/wiki/K-vertex-connected_graph#Computational_complexity).
By [Menger's theorem](https://en.wikipedia.org/wiki/Menger%27s_theorem) we need to check if for every vertex pair $(s,t)$ there are at least $k$ vertex disjoint paths (excluding $s$ and $t$) connecting $s$ and $t$. The number of vertex disjoint paths between $s$ and $t$ can be easily computed through max flow. 
<!-- Duplicate every vertex except $s$ and $t$ and connect an directed edge with capacity 1 between every pair of new vertices. The capacity is 1 for all edges.  -->
We replace every internal vertex $v$ with two copies $v_{in}$ and $v_{out}$ and add directed edges from $N(v)$ to $v_{in}$ and from $v_{out}$ to $N(v)$ with capacity 1. The integral max flow is the number of internally disjoint paths between $s$ and $t$. 
Since the constraint matrix of flow problems is TU, maximizing the flow gives us the vertex connectivity.

> Instead of computing the flow for every pair, if we want one flow that gets the demand for every vertex pair $(i<j)$, the problem becomes much harder. This is [Multi-commodity flow problem](https://en.wikipedia.org/wiki/Multi-commodity_flow_problem).

Currently the fastest algorithm for computing vertex connectivity is [@HENZINGER2000222].
There is a nice table for a summary of connectivity related algorithms.

![image courtesy of Abdol–Hossein Esfahanian. [link](http://www.cse.msu.edu/~esfahani/book_chapter/Graph_connectivity_chapter.pdf)](/images/vertex_connectivity_cut/table.png)

[@HENZINGER2000222] appears in the last line. The conference version was published on FOCS96.

## Minimum cut for edge connectivity

Finding the minimum edge set whose removal breaks the $k$-edge connectivity is easy. It is known that the global min-cut is the edge connectivity number. Thus we can simply compute the min-cut and remove any number of the edges as needed.

## Minimum cut for vertex connectivity

With the knowledge of how to compute vertex connectivity, we try to compute the minimum cut for $k$-vertex connectivity in a similar way. First we can find the vertex pair $(s,t)$ with the smallest number of internally disjoint paths. Note that we are dealing with the modified graph when computing the vertex connectivity number with flow. Hence the min-cut may contain edges that are not in the original graph, i.e., the edges connecting $v_{in}$ and $v_{out}$. For example, consider a graph where every edge has multiplicities 2. The min-cut reported by the flow algorithm should only contain edges between $v_{in}$ and $v_{out}$.

# Connectivity interdiction

Connectivity interdiction is first studied by Zenklusen [@zenklusen_connectivity_2014].

::: Problem
Given a graph $G=(V,E)$ and costs $c:E\to \Z_+$ and weights $w:E\to \Z_+$ and a budget $B\in \Z_+$, find the edge set $R$ such that $c(R)\leq B$ and that minimizes the $w$-weighted min cut in $(V,E\setminus R)$.
:::


A recent paper [@vygen_fptas_2024] gives an FPTAS for the problem. Here I try to develop the intuition since I have never seen an algorithm based on re-weighting edges this complicated and ingenious.

First one can see that the optimal solution can always be a subset of edges in a cut of $G$. This is because if the optimal solution $R^*$ contains any edge not in the cut, we can safely delete it from $R^*$. Thus the optimal solution is indeed a pair $(C^*,R^*\subset C^*)$. The authors call this problem the *$b$-free min-cut problem* ($b$ is the budget and we are allowed to pick edges for free in the "mincut" with total weight at most $b$).

So the goal is to find a FPTAS for the $b$-free mincut problem. The problem is hard since it contains knapsack as a special case. (Consider a graph with many parallel edges and only 2 vertices.) However, it is known that there is a FPTAS for knapsack. If we know part of the optimal solution, i.e., $C^*$, we can use the FPTAS for knapsack to find the optimal $R^*$.

