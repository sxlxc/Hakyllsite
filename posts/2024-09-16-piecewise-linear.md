---
title: Epigraphs of Piecewise Linear Convex Function, Infimal Convolution & Minkowski Sum
tags: alg, optimization
author: Yu Cong
---

This post is a note on epigraphs, infimal convolution, Minkowski sum & convex conjugate of piecewise linear convex functions in $\R^d$. I want to provide proofs for relations between these operations and counterexamples for wrong guesses.

Notions:

- infimal convolution: $\square$,
- Minkowski sum: $\oplus$,
- convex conjugate: $f^*$

<https://angms.science/doc/CVX/Epigraph.pdf>

<https://math.stackexchange.com/questions/1597809/inf-convolution-two-basic-questions>

### piecewise linear function $f:\R^d\to \R$

<https://juan-pablo-vielma.github.io/publications/Fitting-Piecewise-Linear.pdf>

Let $\mathcal P$ be a set of bounded convex polytopes in $\R^d$. A piecewise linear function $f$ can be defined as 

$$
f(x)=c_P^T x+d_P, \text{ for } x\in P,
$$
where $(c_P,d_P)\in \R^{d+1}$ is a vector associated with polytope $P$.

For *continuity*, we require

$$
c_P^T x+d_P=c_Q^T x+d_Q, \; \forall x\in P\cap Q, \forall P,Q\in \mathcal{P}
$$

For *convexity*, we further require that 

1. for any subset $\mathcal{P'}\subset \mathcal P$, $\cup_{P\in \mathcal P'}$ is convex;(for polytopes)
2. the restriction of $f$ to any two polytopes from P that share a facet is convex.(for $f$, however, requirement 1 is included here)

(this is from  <https://www.sciencedirect.com/science/article/pii/089571779090090A>)

...

<!-- ::: Problem
Find the square root of a integer n, without using the built in sqrt function. (The range of the result was not specified, I assume it's double)
:::

::: Proof
aaa
:::


aa `sdf` -->