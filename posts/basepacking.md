---
title: Matroid base packing and covering
tags:  matroid, optimization, combinatorics
author: Yu Cong
lang: en
# draft: true
date: 2025-01-04
showtoc: true
---

\DeclareMathOperator{\B}{\mathcal B}
\DeclareMathOperator{\I}{\mathcal I}
\DeclareMathOperator{\polylog}{polylog}
\DeclareMathOperator{\OPT}{OPT}

There are few text books in combinatorial optimization discussing topics in matroid base packing, while matroid base covering([matroid partition problem](https://en.wikipedia.org/wiki/Matroid_partitioning)) is everywhere.
Packing and covering of trees in graphs is discussed in chapter 51 of [@Schrijver2004].

# Base packing & base covering

::: Problem
Given a matroid $M=(E,\I)$ and its bases $\B$, find

1. (minimum base covering) the min number of bases whose union is $E$, and
2. (maximum base packing) the max number of pairwise disjoint bases.
:::

These problems can be formulated with the following integer programs,
base packing:
\begin{align*}
\max&   &   \sum_{B\in\B} x_B&            &   &\\
s.t.&   &   \sum_{B:e\in B} x_B &\leq 1   &   &\forall e\in E\\
    &   &              x_B&\in \set{0,1}  &   &\forall \text{ base $B$}
\end{align*}

base covering:
\begin{align*}
\min&   & \sum_{B\in\B} x_B&            &   &\\
s.t.&   & \sum_{B:e\in B} x_B &\geq 1   &   &\forall e\in E\\
    &   &            x_B&\in \set{0,1}  &   &\forall \text{ base $B$}
\end{align*}

Integer programs are hard and these IPs have exponential number of variables. We consider the linear relaxations.

> For any LP with bounded solutions, there must exist an optimal solution with support at most 
> For similar problem on integer programming,
> one might think that there is also a small support based on the knowledge that the optimal 
> solution for the integer program is simply a integer point inside the feasible region.
> However, the size of support for integer programs is not that small. Currently the best known 
> upperbound is roughly $m\cdot \polylog(\|{A}\|_1)$, see [this paper](https://drops.dagstuhl.de/storage/00lipics/lipics-vol283-isaac2023/LIPIcs.ISAAC.2023.13/LIPIcs.ISAAC.2023.13.pdf).

Actually these two problems are not hard on general matroids. 
Both of them can be solved in polynomial number of independence oracle calls.

- matroid base covering = matroid partitioning ≈ matroid union. Let $M=(E,\I)$ be the matroid. The minimum number of bases that cover the groundset is $\arg\min\limits_k r_{k}(E)=|E|$, where $r_{k}(\cdot)$ is the rank function of $M^k$.
- matroid base packing ≈ matroid union. Maximum integral base packing number is $\arg\max\limits_k r_{k}(E)=kr(M)$.

Thus the integral version of these two problem is polynomial solvable (in terms of the number of oracle calls) since matroid union is tractable. We will discuss computing the fractional version later.

> The base covering number may be much larger than the base packing number, since $E-B_k$ may not be independent for $M$. ($B_k$ is the union of bases in the optimal packing)

# Matroid strength and density

We will talk about matroid strength and density and their relation with base packing and covering in this section. 
I think none of the results is new. You can find some of them in [@catlin_fractional_1992] and [@fan_extensions_2019].

The fractional base covering number for graphic matroids are called fractional arboricity. It is known that the fractional arboricity $\alpha(G)$ equals to $\max\limits_{\emptyset \subsetneq X\subset E}\frac{|X|}{r(X)}$. Define the density for a matroid $M$ as $\alpha(M)=\max\limits_{\emptyset \subsetneq X\subset E}\frac{|X|}{r(X)}$. The name "density" comes from [@catlin_fractional_1992]. I use symbol $\alpha$ since density is a generalization of arboricity.

For the packing part consider the fractional version of Nash-Williams theorem,

::: Theorem
The fractional spanning tree packing number of a connected graph $G=(V,E)$ equals to $\max \frac{|E[\mathcal P]|}{|\mathcal P|-1}$, where the maximum is taken among all partitions $\mathcal P$ of $V$.
:::

The fraction in above theorem can be rewrite as $\frac{|E-F|}{r(E)-r(F)}$, which only uses elements in the groundset and the rank function and thus can be generalized to non-graphic matroids. The maximum of this fraction, $\sigma(M)=\max_{F\subset E}\frac{|E-F|}{r(E)-r(F)}$ is called matroid strength.(The name also comes from [@catlin_fractional_1992].)

For the connections between density and strength, we have the following inequality,

\[
\alpha(M)=\max \frac{|X|}{r(X)} \geq \frac{|E|}{r(E)} \geq \min \frac{|E-F|}{r(E)-r(F)} =\sigma(M).
\]

:::{.Theorem}
Maximum fractional base packing number is $\sigma(M)$.
:::

:::{.Proof}
The proof is similar to the graph strength proof for tree packing in [@Schrijver2004].
Let $B(M)$ be the base polytope of $M$ and $\Pi$ be the powerset of $E$.
Consider the following linear programs,
\begin{align*}
LP1=\min&   &    lx&  \\
    s.t.&   &    x&\in B(M)
\end{align*}

\begin{align*}
LP2=\max&   & \sum_{F\subsetneq E} y_{E\setminus F}(r(E)-r(F))&  \\
    s.t.&   & \sum_{F\subsetneq E} y_{E\setminus F} \chi^{E\setminus F} & \leq l\\
        &   &    y & \in \R^\Pi_+
\end{align*}

and the dual of $LP2$,
\begin{align*}
    LP3=\min&   &    lx&                                    &   &\\
        s.t.&   &    x^T\chi^{E\setminus F} &\geq r(E)-r(F) &   &\forall F\subsetneq E\\
            &   &           x&\in \R^E_+                    &   &
\end{align*}  

We first prove that the polyhedron in $LP3$, $Q=\{ x | x\geq 0,x^T\chi^{E\setminus F} \geq r(E)-r(F) \quad \forall F\subsetneq E\}$ is the base polytope of $M$. One can see that $B(M)\subseteq Q$. Now suppose $Q$ is larger than $B(M)$, there must exists $x\in Q$ such that $x(U)>r(U)$ for some $U\subseteq E$. Thus $\OPT(LP3)>\OPT(LP1)$. However, for the optimal solution $x$ to $LP1$ and any feasible solution $y$ to $LP2$ we have
\[
  \OPT(LP1)\geq \sum_{F\subsetneq E} y_{E\setminus F}\chi^{E\setminus F}\cdot x = \sum_{F\subsetneq E} y_{E\setminus F} \left(\sum_{e\in E}x_e-\sum_{e\in F}x_e\right)\geq \sum_{F\subsetneq E} y_{E\setminus F} \left(r(E)-r(F)\right)=\OPT(LP3)
\]
Hence $Q=B(M)$.

Recall that $\sigma(M)=\min_{F\subsetneq E}\frac{|E\setminus F|}{r(E)-r(F)}$. 
Note that $\sigma(M)\geq 1$. 
$\sigma(M)$ can be interpreted as the largest $\lambda>1$ such that $|E\setminus F| \geq \lambda(r(E)-r(F))$ holds for all $F\subsetneq E$.
Hence $\sigma(M)=\max \{\lambda | \mathbf 1\in \lambda B(M)\}$ since $Q=B(M)$. 
For fixed $\lambda$, $\mathbf 1 \in \lambda B(M)$ if and only if there exists $\lambda_b\geq 0$ for all bases of $M$ such that $\sum_b \lambda_b=\lambda$ and $\sum_b \lambda_b \chi^b\leq 1$. Hence this shows $\sigma(M)$ is exactly the base packing LP $\max\{\sum_b{\lambda_b}| \sum_{b}\lambda_b\chi^b\leq 1,\lambda_b\geq 0\;\forall b\in B\}$.
:::

> Note that this proof is a straightforward generalization of the tree packing theorem in [@Schrijver2004], which is similar to the blocking polyhedra method described in [@schrijver_polyhedral_1986].

In [@Galtier_2018b] there is a constructive proof that recovers the optimal $F\subset E$ in $\sigma(M)$ from any optimal solution of hitting set LP(dual to base packing).

Define two sets $P_{+}^{a,b}, P^{a,b}\in \R^{|E|}$.
\begin{equation*}
\begin{aligned}
P_+^{a,b}   &=\set{y\in \R^{|E|}: a y(e)\geq b \;\forall e\in E; y(B)\geq 1 \; \forall \text{ base B}}\\
P^{a,b}     &=\set{y\in P_+^{a,b}: \forall e\in E, \exists B^e\in \mathcal B \; s.t. \; e\in B^e \land y(B^e)=\min_{B\in \mathcal B} y(B)}
\end{aligned}
\end{equation*}

$P^{a,b}$ is contained in $P_{+}^{a,b}$ and every element is in a minimum base with respect to weights $y:E\to \R$.

::: Proposition
Let $a\neq 0$, $b$ such that $rb\leq a$ and $y\in P_+^{a,b}$. There exists $y'\in P^{a,b}$ s.t. $y(e)\geq y'(e)$ for all $e$.
:::

::: Proof
The proof is contrustive. Let $B=\set{e_1,\ldots, e_r}$ be a minimum weight base with $y$. 
Assume that $y(e_1)\leq \ldots \leq y(e_r)$.
For each element $e\notin B$, let $C_e$ be the fundamental circuit in $B+e$.
Then we define $y'$ as follows.
\begin{equation*}
y'(e)=
\begin{cases}
y(e)    & e\in B\\
\min\limits_{e\in C_e-e} y(e) &e\notin B
\end{cases}
\end{equation*}

One can easily verify that $y'(e)\leq y(e)$ for all $e$ and $B$ is still the minimum weight base under weights $y'$. Now it remains to show that $y'\in P^{a,b}$. 

1. Every element is in a minimum base. For $e\in B$ this is automatically satisfied. We consider $e\notin B$. Let $f\in C_e$ be the element in the fundamental circuit of $B+e$ with smallest weight $y(e)$. $B^e=B-e+f$ is a base and we have $y'(B^e)=y'(B)$.
2. For all base $B'$, $y'(B')\geq 1$ holds since $y'(B')\geq y'(B) = y(B)\geq 1$.
:::

Note that $P_+^{1,0}$ is the polytope for base hitting set. The importance of $P^{a,b}$ is that for any $y\in P^{a,b}$ and set $P(\theta,y)=\set{e:y(e)\leq \theta}$, the size of the intersection of $P(\theta,y)$ and any minimum weight base $B$ is exactly the rank of $P(\theta,y)$. The proof is by contradiction.



-------------

::: Theorem
Minimum fractional base covering is $\alpha(M)$.
:::

The proof is similar to and easier than the previous one. The corresponding polyhedron in $LP3$ becomes $\{x|\chi^{F}\cdot x\leq r(F)\; \forall F\subset E\}$ which is exactly the independence polytope.

Note that these two theorems can be generalized to weighted packing and covering of matroid bases.

## Integral gap

It is known that the integral base packing number is $\floor{\sigma(M)}$ and the integral base covering number is $\ceil{\alpha(M)}$. Thus the integral gap for both base packing and covering are quite small.

In [@fan_extensions_2019] there are stronger theorems describing the relations between integral packing/covering number and $\sigma$ or $\alpha$. 

::: Theorem
Let $\varepsilon\in [0,1)$ be the fractional part of $\sigma(M)$ or $\alpha(M)$, then there exists a packing(covering) of size $\floor{\sigma(M)}$($\ceil{\alpha(M)}$), one of the independnet sets in the packing(covering) is of size at most $\varepsilon\cdot r(M)$.
:::

## Duality

Applying the rank function of matroid dual derives the following(theorem 1 in [@catlin_fractional_1992]),

::: Theorem
For matroid $M$ without any loop or coloop,
\[\sigma(M^*)=\frac{\alpha(M)}{\alpha(M)-1}\]
:::

Another relation worth noting is hitting set and set covering. The hitting set problem for matroid bases is known as computing the cogirth of the matroid. However, base covering is not a dual problem for cogirth. Sets in the corresponding hitting set problem of set covering is $S_e=\set{B|e\in B}$ for all $e\in E$.

# Computing the strength and density

For graphic matroids, the strength and fractional arboricity are known to be computable in strongly polynomial time. See chapter 51.4 in [@Schrijver2004] and [this notes](https://courses.grainger.illinois.edu/cs598csc/fa2024/Notes/lec-tree-packing.pdf).

The idea is to consider the dual problem which has only $|E|$ variables. If there is a separation oracle for testing whether a dual solution $x$ is feasible, then ellipsoid method can be used for a polynomial time algorithm.

For spanning tree packing the dual is graph min-cut problem, which is easy for graphic matroids but NP-Hard for general matroids (to find the cogirth). The separation oracle = find minimum weight base.
Chekuri and Quanrud [@chekuri_near_linear_2017] discovered near-linear time approximation scheme for some implicit fraction packing problems. For fractional base packing, their algorithm outputs a $(1-\epsilon)$-approximation with $\tilde O(n/\epsilon^2)$ independence oracle calls. For the capacitated version, the number of oracle calls becomes $\tilde O(rn/\epsilon^2)$.

> Almost at the same time, [Jérôme Galtier](https://dblp.org/pid/54/3460.html) published similar results for graphs [@Galtier_2018a] and for matroids [@Galtier_2018b].

For spanning tree covering the dual is finding a maximum edge set whose intersection with each spanning tree is at most 1. This problem can be thought as a set cover, in which the sets are $\set{T|e\in T}$ for each edge $e$. The separation oracle solves the following problem: given edge weight $x:E\to [0,1]$, is there a spanning tree with weight greater than 1? We can simply find a matroid base with the largest weight. Thus for general matroid we can find the fractional arboricity through ellipsoid method.