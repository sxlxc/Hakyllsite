---
title: Matroid girth
tags:  matroid, optimization, combinatorics
author: Yu Cong
lang: en
draft: true
showtoc: true
date: 2025-08-12
---

\DeclareMathOperator{\girth}{girth}

Let $M=(E,\mathcal I)$ be a matroid with non-negative weights $w:E\to \R_{\geq 0}$. 
The girth of $M$ is 
\[
    \min \set{\sum_{e\in C} w(e): \text{$C$ is a circuit of $M$}}.
\]

Cogirth of $M$ is the girth of the dual matroid of $M$.

Computing girth is NP-hard for binary matroids but can be done in polynomial time for graphs.
[Wikipedia](https://en.wikipedia.org/wiki/Matroid_girth#Computational_complexity) lists some negative complexity results, which mainly concern more general matroid classes than binary matroids. 
So here are some positive results filling the gap between graphic matroids and binary matroids[^1].

[^1]: Results can be found in <https://matroidunion.org/?p=1106>

# Regular matroid

::: {.Theorem title="Seymour decomposition [@seymour_decomposition_1980]" #regulardecomp}
Every regular matroid may be constructed by combining graphic matroids, cographic matroids, and a certain ten-element regular matroid that is neither graphic nor cographic, using 3 binary operations:

- 1-sum is direct sum of two matroids
- 2-sum is patching two matroid on 1 common element
- 3-sum is patching two matroids on 3 common elements
forming a 3-circuit in each matroid.
:::

This decomposition can be found in polynomial time[^2].

[^2]: One can decide if a matroid $M$ can be decomposed into $M_1$ and $M_2$ using 1/2/3-sum in polynomial time. See <https://www.emis.de/monographs/md/index.html>.

[@regulardecomp] leads to a natural algorithm for computing the girth in regular matroids. The decomposition of regular matroids gives us a binary tree, where each node is a regular matroid and each leaf is either (co)graphic or a special 10 element regular matroid.
Every non-leaf node in the decomposition tree represents a regular matroid $M$ which is 1/2/3-sum of its two direct decendents $M_1$ and $M_2$. Let $A \oplus_i B$ be the $i$-sum of $A$ and $B$ for $i\in [3]$. Now there are only 3 cases:

1. $M=M_1\oplus_1 M_2$. Direct sum does not create new circuit. $\girth(M)=\min \set{\girth(M_1),\girth(M_2)}$.
2. $M=M_1\oplus_2 M_2$. Let $e$ be the common element of $E(M_1)$ and $E(M_2)$. In this case there may be new circuits. However, any circuit of $M$ which is not a circuit of $M_1$ and $M_2$ must be contained in $C_1\cup C_2\setminus \set{e}$, where $C_i$ is a circuit in $M_i$ containing $e$. Thus to find the minimum weighted new circuit we can compute the minimum circuit in $M_1$ containing $e$ (say $C_1^*$) and replace the weight $w(e)$ in $M_2$ with $w(C_1^*)-w(e)$ and then find the minimum weight circuit in $M_2$ containing $e$. $\girth(M)=\min \set{\girth(M_1),\girth(M_2),\min\set{w(C): \text{$C$ is new circuit}}}$. 

    We need to prove that all these operations can be done in polynomial time. 

    For the 2/3-sum case we need to find in at least one of the summands the minimum circuit that contains a common element $e$. However, finding such a minimum circuit is regular matroids is not known to be polynomially solvable.
    To understand what's happening here we need to ~~look into Seymour's proof~~. 
    
    > *I finally realized that one doesn't need to understand Seymour's 55-page paper to see why the desired operations can be done in polynomial time... Readers who are not interested in the proof should skip this blockquote.*
    >
    >   The proof of [@regulardecomp] has 3 parts:
    >   1. There is a special 10-element regular matroid $R_{10}$ such that any regular matroid can be obtained by 1/2-sums from regular matroids without $R_{10}$ minor and copies of $R_{10}$. 
    >
    >       (Now we can assume that we are working with regular matroids which have no $R_{10}$ minor and are not separable via 1/2-sum.)
    >   2. There is another 12-element regular matroid $R_{12}$ such that any regular matroid can be obtained by 1/2/3-sums from matroids without $R_{12}$ minor. 
    > 
    >       (Now we are working with regular matroids that are not separable via 1/2/3-sum and have no $R_{10}$ or $R_{12}$ minors.)
    >   3. Every 3-connected regular matroid which is neither graphic nor cographic has an $R_{10}$ or $R_{12}$ minor.
    >
    >      Let $M$ be a matroid. $M$ is 3-connected iff $M$ is not expressible as a 1- or 2-sum. (cf. [@seymour_decomposition_1980] 2.10(b))
    >
    >      It follows that the remaining regular matroids are graphic or cographic.

    Instead of considering our binary decomposition tree, we now construct a new graph $G$ where each vertex represents a graphic matroid, cographic matroid or $R_{10}$ and there is an edge between two vertices if the corresponding matroids are patched using 1/2/3-sum. We claim that there is no cycle in the graph. The graph is connected. Assume that there is a cycle and let $M_1,M_2$ be two matroids whose corresponding vertices are in the cycle. Consider the LCA $M$ of $M_1$ and $M_2$ in the binary tree. $M$ represents a connected subgraph $H\subset G$ that contains $M_1$ and $M_2$ but not the entire cycle since otherwise there will be 2 1/2/3-sum operation between two regular matroids. However, $M_1$ and $M_2$ are still connected in $G-E[H]$ since $M_1$ and $M_2$ are in the same cycle, which contradicts the uniqueness of the LCA.

    Thus $G$ is a tree and we can always assume that one of the matroids in the summands is graphic matroid, cographic matroid or $R_{10}$. Finding the minimum circuit containing a fixed element can be done in those matroids in polynomial time and there exists a algorithm that computes the tree in cubic time [@truemper_decomposition_1990].

3. $M=M_1\oplus_3 M_2$. Similar to the 2-sum case. There are only 3 common elements. We can enumerate all circuits of $M_1$ which contain one of the common elements.

However, deciding whether a regular matroid has a circuit of length at most k containing two fixed elements [is FPT](https://mathoverflow.net/questions/434026/algorithm-for-finding-a-minimum-weight-circuit-in-a-weighted-binary-matroid#comment1118055_434045).

# Perturbed graphic matroids

Jim Geelen and Rohan Kapadia [@geelen_computing_2018] showed that the (co)girth can be computed in polynomial time for a subclass of binary matroids called perturbed graphic matroids.

The most important problem in this field is the following.

::: {.Conjecture title="Geelen, Gerards, and Whittle [@Geelen_Gerards_Whittle_2015]" #conjgirth}
For any proper minor-closed class $\mathcal M$ of binary matroids, there is a polynomial-time algorithm for computing the girth of matroids in $\mathcal M$.
:::

Similar to Seymour's decomposition for regular matroids, every proper minor-closed class of binary matroid admits a "decomposition" into graphic matroids and some binary matroids.

::: {.Theorem title="[@Geelen_Gerards_Whittle_2015]" #binarydecomp}
For each proper minor-closed class $\mathcal M$ of binary matroids, there exist integers $k,t\geq 0$ such that for each vertically $k$-connected matroid $M\in \mathcal M$, there exist matrices $A,P\in \mathrm{GF}(2)^{r\times n}$ such that $A$ is the incidence matrix of a graph, $r(P)\leq t$ and either $M=M(A+P)$ or $M^*=M(A+P)$.
:::

Using [@binarydecomp], [@conjgirth] is true if one can prove the followings:

1. there is a polynomial-time alg that finds the girth of $M(A+P)$;
2. One can reduce the problem of computing the girth of members of $\mathcal M$ to that of computing the girth of vertically $k$-connected members of $\mathcal M$.