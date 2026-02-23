---
title: MWU and related stuff
author: Yu Cong
lang: en
date: 2025-12-14
showtoc: true
---

This is a note on multiplicative weight update (MWU) method.

I read about MWU methods on 2 problems, finding the $k$-edge-connected spanning subgraph with smallest weight ($k$-ECSS) [@chalermsook_approximating_2022] and the minimum $k$-cut [@Quanrud_2019]. It seems that there are some connections in these two problems since both paper discover $(2+\epsilon)$ approximation algorithms in nearly linear time with MWU.

# Positive covering LPs


# $k$-cuts

# $k$-ECSS

# Fractional packing

Plotkin, Shmoys and Tardos [@plotkin_fast_1991] found a Lagrangian relaxation method for approximately solving fractional packing (or covering) problems.
The original paper is old. I found [this note](https://www.columbia.edu/~nhw2114/files/Fast_Approximation_Algorithms_for_Fractional_Packing_Problems.pdf) summarizing its techniques.

Formally, we want to find $x\in P$ such that $Ax\leq b$ where $P\in\R^n$ is a polytope, $b\in \R^m_+$ and for any $x\in P$ we have $Ax\geq 0$. Moreover, we assume that for any $c\in \R^n_+$, optimizing $c\cdot x$ over $x\in P$ is easy.

One can think $x\in P$ as an indicator vector of a collection of subsets of $[m]$ and $Ax\leq b$ would be packing constraints.
Sometimes $Ax\leq b$ is not feasible for any $x\in P$. So one may want to relax the packing constraint by finding the minimum $\lambda>0$ such that $Ax\leq \lambda b$ is feasible.

\begin{equation}
\begin{aligned}
\lambda^*   &= \min_{x\in P,\lambda\geq 0} \set{\lambda | Ax\leq \lambda b}\\
            &\geq \min_{x\in P,\lambda\geq 0} \max_{y\geq 0} \lambda+y^T(Ax-\lambda b)\\
            &=\max_{y\geq 0} \min_{x\in P,\lambda \geq 0} y^TAx+\lambda(1-y^Tb)\\
            &\geq \max_{y\geq 0} \min_{x\in P} \set{y^T Ax | y^Tb\leq 1}\\
            &=\max_{y\geq 0}  \frac{\min_{x\in P} y^T Ax}{y^Tb}\\
\end{aligned}
\end{equation}

Note that the two "$\geq$" indicates applying Lagrangian dual twice.
Strong duality holds since the primal problem is feasible and bounded.
Hence, there exists dual variable $y^*$ such that $\min_{x\in P} {y^*}^T Ax/{y^*}^Tb=\lambda^*$.
Also, $\min_{x\in P} y^T Ax$ in the last line is an optimization over $P$ with a linear objective $y^T Ax$, which is easy to solve by assumption.

We say $x\in P$ is an $\e$-approximate solution if $Ax\leq (1+\e)\lambda^* b$.
Let $\lambda_x$ be $\min_{\lambda\geq 0} \set{\lambda | Ax\leq \lambda b}$ for fixed $x$.
It follows from the argument above that $x$ is $\e$-approximate iff there exists some $y$ that $\lambda_x\leq (1+\e)y^T Ax/y^Tb$.

Consider a primal dual approach. Let $x\in P$ be a primal solution and $y$ be a dual variable.
We have,
\[\min_{x'} y^TAx' \leq y^TAx \leq \lambda_x y^T b.\]

Intuitively, to make $x$ and $y$ close to the optimal solution, we want the gaps between 3 terms to be bounded by $\e$.
In the paper they use the following two conditions:

1. $\lambda_x y^T b - y^T Ax \leq \e \lambda_x y^T b$,
2. $y^T Ax - \min_{x'} y^TAx'\leq \e (y^T Ax+\lambda_x y^T b)$.

The RHS of these conditions look strange since we actually want to bound $\frac{\lambda_x y^Tb}{\min_{x'} y^TA}$.
One can verify that for $x,y$ satisfying these conditions and small enough $\e$, $x$ is a $6\e$-approximate solution.
