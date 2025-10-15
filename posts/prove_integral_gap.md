---
title: Proving integrality gap for LPs
tags:  optimization, LP
author: Yu Cong
lang: en
# draft: true
date: 2025-04-01
showtoc: true
---

\DeclareMathOperator*{\opt}{OPT}
\DeclareMathOperator*{\deg}{deg}

Proving integral gap of linear programs are generally hard. It would be great if one can classify LPs with a constant gap. It is known that deciding whether a polyhedron is integral is co-NP-complete [@ding_complexity_2008]. 
I am interested in techniques for proving constant upperbound for integral gaps of linear programs.

Here are some methods with examples that I read in books and papers.

# Counting

Just do the counting.

## tree packing theorem

::: Example
Consider the following integer program on graph $G=(V,E)$,
\begin{align*}
\lambda=\min&   &  \sum_{e\in E} x_e&       &   & \\
s.t.&           &  \sum_{e\in T} x_e&\ge 1  &   &\forall T\in \mathcal T \\
 &              &  x_e&\in\Z_{\ge 0}        &   &\forall e\in E
\end{align*}

where $\mathcal T$ is the set of spanning tree in $G$.
Let $\tau$ be the optimum of the linear relaxation.
:::

::: Theorem
$\lambda \le 2 \tau$.
:::

Note that the optimum solution to $\lambda$ is the minimum cut in $G$.
It is known that $\tau$ is the maximum tree packing in $G$ and $\tau=\min\limits_{F\subset E}\frac{|E-F|}{r(E)-r(F)}$, where $r$ is the rank of the graphic matroid on $G$ [@Schrijver2004].
Then the proof is a simple counting argument.

::: Proof
If $G$ is not connected, Let $G_1,...,G_k$ be the set of components in $G$. One can easily see that the gap of $G$ is at most the largest gap of the components. Thus considering connected graphs is sufficient.

We fix $F^*\in \arg\min \frac{|E-F^*|}{r(E)-r(F^*)}$. $r(E)-r(F^*)$ must be positive and $E-F^*$ is a cut in $G$. Suppose $E-F^*$ is any cut in $G$. Let $S_1,...,S_h$ be components in $G\setminus (E\setminus F^*)$. For any $S_i$, the set of edges with exactly one endpoint in $S_i$ (denoted by $e[S_i]$) must contain a cut of $G$ since the $G$ is connected. One can see that $2|E-F^*|=\sum_i |e[S_i]|\ge \lambda (r(E)-r(F))$ since the number of component is $r(E)-r(F^*)$.
:::

## $k$-cut

::: Example
\begin{align*}
\lambda_k=\min&   &  \sum_{e\in E} c(&e)x_e       &   & \\
s.t.&             &  \sum_{e\in T} x_e&\ge k  &   &\forall T\in \mathcal T \\
 &                &  0\le x_e&\le 1        &   &\forall e\in E
\end{align*}
:::

::: Theorem
The integral gap of $\lambda_k$ is at most $2(1-1/n)$.
:::

The proof is in section 5 of [@chekuri_lp_2020]. Here is a sketch.

For $k$-cut we cannot use the simple counting argument since the dual LP is not a tree packing. (the LP dual needs extra variables $z_e$ for constraints $x_e\le 1$.) However, it is still easy to find an upperbound for the integral optimum. If we sort vertices in increasing order of their degree, that is, $\deg(v_1)\le \dots \le \deg(v_n)$, then $\sum_{i=1}^{k-1} \deg(v_i)$ is an upperbound for integral $k$-cut. Then it is easy to prove that if the optimal solution $x^*$ to $\lambda_k$ is fully fractional ($x_e^*\in (0,1)$ for all $e\in E$), then the gap is $2(1-1/n)$. The proof is to use complemantary slackness conditions, i.e., $z_e=0,\sum_{e\in T}y_T=c(e)\;\forall e\in E$. The following observations reduce general $x^*$ to fully fractional case:

1. Given an optimal solution $x^*$, let $X$ be the set of edges $e$ such that $x_e^*=0$. The optimum to $\lambda_k$ on $G/X$ is the same as on $G$.
2. For an optimal solution $x^*$, let $F$ be the set of edges $e$ such that $x_e^*=1$. Let $x^*|_{E-F}$ be the restriction of $x^*$ to $E-F$. $x^*|_{E-F}$ is a fully fractional optimum solution to $\lambda_k$. (Some discussions are needed for the number of components in $G\setminus F$. The reduction can be done using the fact that if $1\leq \frac{\lambda}{\sigma}\le c$ then $1\le \frac{\lambda+k}{\sigma+k}\le c$.)

# Rounding

A constant factor approximation algorithm based on LP may imply a constant upperbound of the corresponding LP.

Examples:

1. vertex cover and set cover [uiuc cs598csc](https://courses.grainger.illinois.edu/cs598csc/sp2011/Lectures/lecture_4.pdf)
2. facility location [Dartmouth](https://www.cs.dartmouth.edu/~deepc/LecNotes/Appx/5.%20Deterministic%20Rounding%20for%20Facility%20Location.pdf)
3. CKR relaxation of multiway cut [uiuc cs583](https://courses.grainger.illinois.edu/cs583/sp2018/Notes/multiwaycut-ckr.pdf)
4. uniform labeling [FOCS'99](https://www.cs.cornell.edu/home/kleinber/focs99-mrf.pdf) basically multiway cut with assignment cost.


# Intermediate problem

I read about this in [@chalermsook_approximating_2022]. Suppose that we want to prove constant gap for $LP1$. The idea is to find another LP (say $LP2$) which is integral or has constant gap and to prove that $\frac{\opt(IP1)}{\opt(IP2)}\le c_1$ and $\frac{\opt(LP2)}{\opt(LP1)}\le c_2$. Finally we will have something like this,

\begin{equation}
\opt(IP1)\le c_1\opt(IP2)= c_1 \opt(LP2)\le c_1 c_2 \opt(LP1)
\end{equation}

## minimum $k$-edge-connected spanning subgraph

We want to prove that the integral gap for the following LP is 2.

\begin{align*}
LP1=\min&   & \sum_{e\in E} w(e&)x_e    & &\\
s.t.&       & \sum_{e\in C} x_e&\ge k    & &\forall \text{cut $C$}\\
&           &  0\le x_e &\le 1    & &\forall e\in E
\end{align*}

(Finding the minimum $k$-edge-connected spanning subgraph of $G=(V,E)$)

Now we construct LP2. Consider the bidirection version of $G$, denoted by $D=(V,A)$ where $A=\{(u,v),(v,u) \quad \forall (u,v)\in E\}$. Pick a special vertex $r$.

\begin{align*}
LP2=\min&   & \sum_{e\in A} w(e)&y_e            & & \\
s.t.&       & \sum_{e\in \delta^+(S)} y_e&\ge k & &\forall S\subset V \land r\in S\\
 &          & 0\le y_e &\le 1                   &  &\forall e\in E
\end{align*}

(Finding min k-arborescence)

It is known that the polytope in LP2 is integral [@Schrijver2004]. Given any feasible solution of LP, for any edge $e=(u,v)\in E$ we set $y_{(u,v)}=y_{(v,u)}=x_e$. Thus the optimum of LP2 is no larger than $2\opt(LP)$ since $y$ is always feasible.

On the other hand, given a feasible integral solution $y$ of LP2, we set $x_e=1$ if any orientation of $e$ is in $y$. It is clear from the definition of LP2 that $x_e$ is a feasible integral solution of LP. Hence, applying eq(1) proves that the integral gap of LP is 2. (Note that in this example $c_1=1$ and $c_2=2$.)

# Notes

There are many discussions about the integrality gap on cstheory.

1. <https://cstheory.stackexchange.com/questions/30984/exactly-solvable-but-non-trivial-integrality-gap>
2. <https://cstheory.stackexchange.com/questions/4915/integrality-gap-and-approximation-ratio>
3. <https://cstheory.stackexchange.com/questions/392/the-importance-of-integrality-gap>
4. <https://cstheory.stackexchange.com/questions/55188/randomized-rounding-schemes-that-depend-on-the-weights-in-the-lp-objective>
5. <https://cstheory.stackexchange.com/questions/21060/optimization-problems-with-minimax-characterization-but-no-polynomial-time-algo>
6. <https://cstheory.stackexchange.com/questions/3871/minimum-maximal-solutions-of-lps>

It seems that the integrality gap has a deep connection with hardness of approximation. There are two kinds of problems that i find particularly interesting.

- The LP has a relatively large gap, but some algorithm based on that LP achieves a better approximation than the gap.(see [this FOCS'09 paper](http://www.cis.upenn.edu/~sanjeev/postscript/FOCS09_MaxMin.pdf))
- The integrality gap is small (a constant), but approximation algs based on the LP cannot do that good. (Zhiyi Huang gave a [talk at UESTC](https://tcsuestc.com/2025/06/13/optimal-4-approximation-for-the-correlated-pandoras-problem/) recently. <https://arxiv.org/abs/2509.17029> The correlated Pandora's problem has a natural LP formulation with gap $<4$, while [it is NP-hard](https://tetali.math.gatech.edu/PUBLIS/mssc_final.pdf) to aproximate it within a ratio of $4-\epsilon$.)