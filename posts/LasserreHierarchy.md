---
title: Understanding Lasserre Hierarchy
tags: optimization
author: Yu Cong
lang: en
draft: true
date: 2025-05-24
---

\DeclareMathOperator*{\pr}{Pr}
\DeclareMathOperator*{\las}{LAS}

Useful links:

1. <https://sites.math.washington.edu/~rothvoss/lecturenotes/lasserresurvey.pdf>
2. Laurent's survey [@laurent_comparison_2003]

I guess there should be a probabilistic way to understand most of the lemmas and the intuition...

# What is Lasserre Hierarchy?

We want to solve a 0-1 integer program. Since this task is NP-hard in general, we usually consider its linear relaxation. Different LP formulations have different integrality gaps. For example, consider the following linear relaxation of the  max matching IP in non-bipartite graph.

\begin{equation*}
\begin{aligned}
\sum_{e\in \delta(v)} x(e)&\leq 1   &   &\forall v\in V\\
                    x(e)&\in [0,1]  &   &\forall e\in E
\end{aligned}
\end{equation*}
:(, this polytope is not integral. Edmonds proved that the following formulation is integral.

\begin{equation*}
\begin{aligned}
\sum_{e\in \delta(v)} x(e)&\leq 1   &   &\forall v\in V\\
                    x(e)&\in [0,1]  &   &\forall e\in E\\
\sum_{e\in E[U]} x(e) &\leq (|U|-1)/2 &  &\forall U\subset V, |U| \text{ odd}
\end{aligned}
\end{equation*}

Schrijver [@schrijver_polyhedral_1986] showed that those odd constraints can be obtained by adding cutting planes to the previous polytope. Fortunately for matching polytope we have a polynomial time separation oracle. However, for harder problems adding cutting planes may make the program NP-hard to solve. Lasserre hierarchy is a method to strengthen the polytope while providing provable good properties and keeping the program polynomial time solvable (if applied constant number of times).

There is a good interpretation of the linear relaxation of 0-1 integer programs. Let $K=\set{x\in \R^n| Ax\geq b}\subset [0,1]^n$ be the feasible solution of the linear relaxation. The goal of solving the integer program is to describe $K\cap \set{0,1}^n$. For any $x\in K$, $x_i$ can be seen as the probability of $x_i=1$. The integral solutions $K\cap \set{0,1}^n$ can be understood as a discrete distribution that only takes non-zero values on feasible integral points. However, each $x\in K$ only describes some marginal probabilities.
Thus we consider adding more random variables. Let $y\in \R^{2^{n}}$ be a random vector such that $y_I=\pr(\land_{i\in I}(x_i=1))$. Note that we define $y_\emptyset=1$. To make $y$ a feasible probability and to make $(y_{\set{1}},...,y_{\set{n}})^T\in K$ we have to add more constraints.

<!-- why psd? -->

::: {.Definition title="$k$-th level of Lasserre hierarchy"}
The $n$-th level of Lasserre hierarchy of a convex polytope $K=\set{x\in \R^n| Ax\geq b}\subset [0,1]^n$ is the set of vectors $y\in \R^{2^n}$ that satisfy the followings.

1. $M_t(y):=(y_{I\cup J})_{I,J\subseteq [k]}\succeq 0$
2. $M_t^\ell(y):=\left( \sum_{i=1}^n A_{\ell i}y_{I\cup J\cup \set{i}}-b_\ell y_{I\cup J} \right)_{I,J\subseteq [k]}\succeq 0$
:::