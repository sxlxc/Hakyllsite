---
title: Matroid circuit packing and covering
tags:  matroid, optimization, combinatorics
author: Yu Cong, with the help of Kangyi Tian
lang: en
draft: true
showtoc: true
date: 2025-06-15
---

In the [previous post](/posts/basepacking1.html), we mainly focus on the algorithmic part of integral and fractional base packing and base covering. In this post we consider packing and covering of matroid circuits.

# Packing/Covering Defect

Seymour [@seymour_packing_1980] proved the following theorem.

::: Theorem
Let $M=(E,I)$ be a matroid without coloop. Then one has
\[\theta(M)-\kappa(M)\leq r^*(M)-\nu(M),\]
where $\theta(M)$ is the minimum number of circuits whose union is $E$,
$\kappa(M)$ is the number of connected components in $M$,
$r^*$ is the corank and
$\nu(M)$ is the max number of disjoint circuits.
:::

The left hand side $\theta(M)-\kappa(M)$ is called the *circuit covering defect* and the right hand side $r^*(M)-\nu(M)$ is called the *circuit packing defect*. I guess the name "covering defect" comes from the fact that $\theta(M)-\kappa(M)$ is the gap between the circuit covering number and a lowerbound $\kappa(M)$. $\kappa(M)\leq \theta(M)$ since there is no circuit containing two elements in different components. The packing defect is the set-point dual of the covering version. To see the duality, one can write $\kappa(M)$ as the max size of $X\subset E$ such that $|C\cap X|\leq 1$ for all circuit $C$ and write $r^*(M)$ as the minimum size of $X\subset E$ such that $|X\cap C|\geq 1$ for all circuit $C$.

# Complexity

Computing the corank $r^*$ and the component number $\kappa(M)$ is easy. What about $\theta(M)$ and $\nu(M)$?

The problem of determining if a sparse split graph (a special case of chordal graphs) can have its edges partitioned into edge-disjoint triangles is NP-complete [@feder_packing_2012]. So finding $\theta(M)$ and $\nu(M)$ is NP-hard even for some special graphic matroids.

# Cycle Double Cover

[Cycle double cover conjecture](https://en.wikipedia.org/wiki/Cycle_double_cover) is a famous unsolved problem posed by W. T. Tutte, Itai and Rodeh, George Szekeres and Paul Seymour.
The cycle double cover conjecture asks whether every bridgeless undirected graph has a collection of cycles such that each edge of the graph is contained in exactly two of the cycles.

<!-- read [@Zhang_CDC_2016] and [@ding_packing_2009] -->
[@Zhang_CDC_2016] is a nice survey. However, there is little discussion about (even simplier version of) circuit double cover on some special case of matroids. 
For example, [this question](https://math.stackexchange.com/questions/1835067/multi-cover-a-matroid-with-circuits) on math.sx is a relaxation of faithful CDC on matroids.

::: {.Problem title="Not so faithful circuit cover"}
Given a matroid $M=(E,\mathcal I)$ and a non-negative integral weight function $w:E\to \Z_{\geq 0}$, decide if there is a multiset of circuits of $M$ such that each element in $E$ is covered by at least 1 and at most $w(e)$ circuits in the multiset.
:::

[@ding_packing_2009] studied a related (and seemingly simplier) optimization variant. How many circuits can we pack with element capacity $k w(e)$?

\begin{equation*}
\begin{aligned}
\nu_{k,w}=\max&   &   \sum_C x_C&    &   &\\
s.t.&   &   \sum_{C:e\in C} x_C &\leq k w(e)    &   &\forall e\in E\\
    &   &                   x_C &\in \Z_{\geq 0}
\end{aligned}
\end{equation*}

\begin{equation*}
\begin{aligned}
\tau_{k,w}=\min&   &   \sum_e w(e)&y_e    &   &\\
s.t.&   &   \sum_{e\in C} y_e &\geq k    &   &\forall \text{ circuit $C$}\\
    &   &                   y_e &\in \Z_{\geq 0}
\end{aligned}
\end{equation*}

Clearly the linear relaxation of $\nu_{k,w}$ and of $\tau_{k,w}$ are LP dual of each other and have the same optimum. For what class of matroids do we have equality $\nu_{k,w}=\tau_{k,w}$ for any weight function $w$?

When $k=1$ this is relatively simple. First we can assume that $M$ contains no coloop since coloops won't appear in any circuit. Suppose that there are two circuits $C_1,C_2$ whose intersection is non-empty. Let $a,b,c$ be the smallest weight of elements in $C_1,C_2, C_1\cap C_2$ respectively. It follows by definition that $a\leq c$ and $b\leq c$. The max number of circuits we can pack in the matroid $M|_{C_1\cup C_2}$ is $\min(a+b,c)$. Now we further assume that $a,b\leq c\leq a+b$. The minimum weight of elements hitting every circuit is not necessarily $c$, since by the circuit axiom there must another circuit $C'\in C_1\cup C_2-e$ for any $e\in C_1\cap C_2$ which won't be hit if we are selecting element in $C_1\cap C_2$. Thus for the case of $k=1$, any matroid satisfying $\nu_{1,w}=\tau_{1,w}$ has no intersecting circuits.

The characterization of matroids satisfying $\nu_{2,w}=\tau_{2,w}$ is the following theorem [@ding_packing_2009].

::: Theorem
A matroid satisfies $\nu_{2,w}=\tau_{2,w}$ iff none of its minor is isomorphic to $U_{2,4},F_7,F_7^*,M(K_{3,3}),M(K_5^-)$ or $M(K)$, where $K_5^-$ is $K_5$ deleting an edge and $K$ is a special 4-node graph.
<figure>
<img src="../images/circuitpacking/K.png" style="width: 250px;" />
Graph K
</figure>
:::

Their proof is also based on LP. In fact they prove the following theorem.

::: Theorem
Let $M$ be a matroid. The following statements are equivalent:

1. $M$ does not contain $U_{2,4},F_7,F_7^*,M(K_{3,3}),M(K_5^-)$ or $M(K)$ as a minor;
2. the linear system $\{ \sum_{e\in C} y_e \geq 2 \;\forall C, y_e\geq 0\}$ is TDI;
3. the polytope $\{y: \sum_{e\in C} y_e \geq 2 \;\forall C, y_e\geq 0\}$ is integral.
:::

$2\to 3$ is easy. To show $3\to 1$ they prove that if $M$ satisfies 3 then so do its minors and none of the matroids in 1 satisfies 3. The hard part is proving $1\to 2$, for which they use the following two lemmas.

::: Lemma
If $M$ satisfies 1, then $M=M^*(G)$ for some graph $G$ that contains neither the planar dual of $K$ nor of $K_5^-$ as a minor.
:::

::: Proof
A matroid $M$ is regular iff it has no minor isomorphic to $U_{2,4},F_7,F_7^*$. Then $M$ must be regular since it satisfies 1. The lemma then follows from the "excluded minor characterization of graphic matroids in regular matroids".
A regular matroid is cographic iff it has no minor isomorphic to $M(K_5)$ and $M(K_{3,3})$. (see Corollary 10.4.3 in Oxley's Matroid Theory book 2nd edition)
:::

::: Lemma
If a graph $G$ contains neither the planar dual of $K$ nor of $K_5^-$ as a minor, then $M^*(G)$ satisfies 2.
:::

This is the hardest part and it takes a lot of work to prove it.

They first prove a complete characterization of grpahs that contain neither the planar dual of $K$ nor that of $K_5^-$ as a minor using $0,1,2$-sum and then prove that summing operations preserves the TDI property.

The $0,1,2$-sum theorem looks like this.
Let $K^*$ and $P$ be the planar dual of graph $K$ and $K_5^-$ respectively. 

::: {.Theorem title="informal, thm 3.1 in [@ding_packing_2009]"}
A simple graph $G$ has no minors $P$ and $K^*$ iff $G$ can be obtained by repeatedly taking $0,1,2$-sums starting from some small graphs and from some cyclically 3-connected graphs with no minors $P$ and $K^*$.
:::

It remains to show that all the summand graphs in the above theorem have the TDI property. This is also a challenging task. Small graphs can be verified with the help of computers. The difficult case is the cyclically 3-connected graphs with no minors $P$ and $K^*$.