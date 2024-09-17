---
title: Piecewise Linear Convex Functions
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

# piecewise linear function $f:\R^d\to \R$

The following definition comes from [@Toriello_Vielma_2012].

::: {.Definition title="piecewise linear function in $\R^d$"}
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
2. the restriction of $f$ to any two polytopes from P that share a facet is convex.[@Tarela_Alonso_Martínez_1990] (for $f$. In fact, 1 is already included in 2.)

:::

# $\square$

...