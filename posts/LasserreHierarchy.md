---
title: Understanding Lasserre Hierarchy
tags: optimization
author: Yu Cong and Hongjie Qing
lang: en
draft: true
date: 2025-05-24
---

\DeclareMathOperator*{\pr}{Pr}
\DeclareMathOperator*{\las}{LAS}
\DeclareMathOperator*{\conv}{conv}

Useful links:

1. <https://sites.math.washington.edu/~rothvoss/lecturenotes/lasserresurvey.pdf>
2. Laurent's survey [@laurent_comparison_2003]

I guess there should be a probabilistic way to understand most of the lemmas and the intuition...

# $K\subset [0,1]^n \to K\cap \set{0,1}^n$

We want to solve a 0-1 integer program. Since this task is NP-hard in general, we usually consider its linear relaxation. Different LP formulations have different integrality gaps. For example, consider the following linear relaxation of the  max matching IP in non-bipartite graph.

\begin{equation*}
\begin{aligned}
\sum_{e\in \delta(v)} x(e)&\leq 1   &   &\forall v\in V\\
                    x(e)&\in [0,1]  &   &\forall e\in E
\end{aligned}
\end{equation*}
`:(`, this polytope is not integral. Edmonds proved that the following formulation is integral.

\begin{equation*}
\begin{aligned}
\sum_{e\in \delta(v)} x(e)&\leq 1   &   &\forall v\in V\\
                    x(e)&\in [0,1]  &   &\forall e\in E\\
\sum_{e\in E[U]} x(e) &\leq (|U|-1)/2 &  &\forall U\subset V, |U| \text{ odd}
\end{aligned}
\end{equation*}

Schrijver [@schrijver_polyhedral_1986] showed that those odd constraints can be obtained by adding cutting planes to the previous polytope. Fortunately for matching polytope we have a polynomial time separation oracle. However, for harder problems adding cutting planes may make the program NP-hard to solve. Lasserre hierarchy is a method to strengthen the polytope to approaching the integer hull while providing provable good properties and keeping the program polynomial time solvable (if applied constant number of times).

# Probability Perspective

There is a good interpretation of the linear relaxation of 0-1 integer programs. Let $K=\set{x\in \R^n| Ax\geq b}\subset [0,1]^n$ be the polytope of the linear relaxation. The goal of solving the integer program is to describe all possible discrete distribution over $K\cap \set{0,1}^n$. Note that for a fixed distribution the expected position $(\sum_p \pr[X_p(1)=1] x_p(1),...,\sum_p \pr[X_p(n)=1] x_p(n))^T$ is in $\conv(K\cup \set{0,1}^n)$ and iterating over all possible distribution gives us the integer-hull. Hence we can find the integral optimal solution if having access to all distribution over integer points.

For any $x\in K$, $x_i$ can be seen as the probability of $x_i=1$. 
We only care about the discrete distribution on feasible integral points. 
However, each $x\in K$ only describes some marginal probabilities and this this marginal probability may not be even feasible. Consider the following 2D example. Any point in $\text{green area}\setminus \text{orange area}$ is not a marginal distribution of any possible joint distribution over $(0,0),(1,0)$ and $(1,1)$. The idea is to iteratively prune this area.

<figure>
<img src="../images/lasserre/feasiblepoints.png" alt="2D example" style="width: 300px;" />
</figure>

Now we need to think about how to represent all possible joint distribution. One natural way is to use a vector $y\in \R^{2^n}$ for the distribution law of every possible integer point in $\set{0,1}^n$.
However, this method does not work well with our existing marginal probabilities. 
Let $y\in \R^{2^{n}}$ be a random vector such that $y_I=\pr[\land_{i\in I}(x_i=1)]$ and $y_\emptyset=1$. Computing all feasible $y$ is the same as finding all possible bivariate discrete distribution on the integer points.
To make $y$ a feasible probability from some joint distribution and to make $(y_{\set{1}},...,y_{\set{n}})^T\in K$ we have to add more constraints.

<!-- why psd? -->
## Feasible Probability

We work with the 2D example first. Let $x=(x_1,x_2)^T\in K$ be a marginal distribution. One can see that $y=(1,x_1,x_2,\pr[X_1=X_2=1])^T$ and the last number is not arbitrary. In fact, $\pr[X_1=X_2=1]$ must in range $[\max(0, x_1+x_2-1),\min(x_1,x_2)]$.

To make sure $y$ is indeed a probability the moment matrix is considered. The moment matrix $M(y)$ is of size $2^n \times 2^n$ and $M(y)[I,J]$ is defined as the expectation $E[\prod_{i\in I\cup J}x_i]=y_{I\cup J}$ (the only non-zero term is $1\cdot \pr[\land_{i\in I\cup j}x_i=1]=y_{I\cup J}$). The expectation is taken over the distribution defined by $y$.

::: Lemma
For any probability distribution $y$, the moment matrix is psd.
:::

::: Proof
We need to verify $z^T M(y) z\geq 0$ for any $z$.
\begin{equation*}
\begin{aligned}
z^T M(y) z    &= \sum_I \sum_J z_I y_{I\cup J} z_J\\
                    &= \sum_I \sum_J z_I E[\prod_{i\in I\cup J} x_i] z_J\\
                    &= E\left[\left( \sum_I (z_I \prod_{i\in I} x_i)\right)^2 \right]
\end{aligned}
\end{equation*}
:::

Note that in the proof something like sum of squares appears. Lasserre hierarchy has deep connections with [SOS optimization](https://en.wikipedia.org/wiki/Sum-of-squares_optimization) and is also known as sum-of-squares hierarchy.

::: Lemma
If $M(y)$ is psd then $y$ is a probability distribution.
:::

This can be seen from properties of Lasserre hierarchy. We defer the proof.

<!-- slacks? -->
## Projection in $K$

Let the projection of $y$ be $(y_{\set{1}},\dots,y_{\set{n}})^T$. For any $y$ the projection should always lie in $K$.
One may want to define moment matrices for constraints $Ax\geq b$. This is called the moment matrix of slacks. For simplicity we only consider one linear constraint $a^Tx-b\geq 0$. The moment matrix for this constraint is $M_t(y)=\left( \sum_{i=1}^n a_i y_{I\cup J\cup \set{i}}-b y_{I\cup J} \right)_{I,J\subseteq [n]}$. Then we can do similar arguments.

\begin{equation*}
\begin{aligned}
z^T M(y) z    &= \sum_I \sum_J z_I z_J (\sum_{i=1}^n a_i y_{I\cup J\cup \set{i}}-b y_{I\cup J})\\
                    &= \sum_I \sum_J z_I z_J (\sum_i a_i E[\prod_{k\in I\cup J\cup\set{i}} x_k]) - b \sum_I \sum_J z_I z_J E[\prod_{k\in I\cup J}x_k]\\
                    &= \sum_I \sum_J z_I z_J E\left[ (\sum_i a_i x_i -b) \prod_{k\in I\cup J}x_k \right]\\
\end{aligned}
\end{equation*}

Note that we can combine the expactations since they are taken over the same probability distribution. Now assume that we have $a^Tx-b\geq 0$...

::: {.Definition title="$k$-th level of Lasserre hierarchy"}
The $k$-th level of Lasserre hierarchy of a convex polytope $K=\set{x\in \R^n| Ax\geq b}\subset [0,1]^n$ is the set of vectors $y\in \R^{2^n}$ that make the following matrices psd.

1. moment matrix $M_t(y):=(y_{I\cup J})_{I,J\subseteq [k]}\succeq 0$
2. moment matrix of slacks $M_t^\ell(y):=\left( \sum_{i=1}^n A_{\ell i}y_{I\cup J\cup \set{i}}-b_\ell y_{I\cup J} \right)_{I,J\subseteq [k]}\succeq 0$
:::