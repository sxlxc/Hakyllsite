---
title: Piecewise Linear Convex Functions
tags: alg, optimization
author: Yu Cong
lang: en
showtoc: true
---

\DeclareMathOperator{\epi}{epi}
\DeclareMathOperator{\dom}{dom}
\DeclareMathOperator{\conv}{conv}

This post is a note on epigraphs, infimal convolution, Minkowski sum & convex conjugate of piecewise linear convex functions in $\R^d$. I want to provide proofs for relations between these operations and counterexamples for wrong guesses.

Notions:

- infimal convolution: $\square$,
- Minkowski sum: $\oplus$,
- convex conjugate: $f^*$,
- epigraph: $\epi f$.

some notes:

- <https://angms.science/doc/CVX/Epigraph.pdf>
- <https://math.stackexchange.com/questions/1597809/inf-convolution-two-basic-questions>

# piecewise linear function $f:\R^d\to \R$

A intuitive but very complex definition is the following[@Toriello_Vielma_2012],

::: Definition
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

(Later I find a much simpler definition in [@boyd_convex_2004] example 3.5.)

:::{.Definition title="piecewise linear function in $\R^d$"}
\[
f(x)=\max \{a_1^Tx+b_1,\ldots,a_L^Tx+b_L\}
\]
:::

It is shown in exercise 3.29 that every piecewise linear convex function can be expressed in this form.

::: Proof
We have $f(\alpha x+(1-\alpha)y)\leq \alpha f(x)+(1-\alpha)f(y)$ for any $\alpha\in [0,1]$. $\alpha x+(1-\alpha)y$ can locate in the same $X_i$ as $y$ since we can arbitrarily choose $\alpha$. Thus $f(x)\geq \frac{f(\alpha x+(1-\alpha)y)-(1-\alpha)f(y)}{\alpha}$. Note that $f$ is linear in each $X_i$. Thus $f(x)\geq \frac{a_i^T(\alpha x+(1-\alpha)y)+b_i-(1-\alpha)(a_i^Ty+b_i)}{\alpha}=a_i^Tx+b_i$. Thus $f(x)=\max_i a_i^Tx+b_i$.
:::

Now given these definitions we are particularly interested in the minimum number of polytopes which define the piecewise liner convex function in high dimension.

# properties

pwl = piecewise linear. Let $L$ be the number of hyperplanes in the definition of $f$.

## convex conjugate

::: Observation
Let $f^*$ be the convex conjugate of a pwl convex function $f$. $f^*$ is also pwl convex if restricted to $\dom f^*$.
:::

Consider the convex conjugate from a geometric view. The epigraph of our pwl convex function $f$ is some convex polytope in $\R^d\times \overline{\R}$. The convex conjugate is $f^*(z)=\sup_x\{z^Tx-f(x)\}$. $z^Tx$ is a hyperplane with normal vector $z$ and passing through the origin. Now $\sup_x\{z^Tx-f(x)\}$ is the amount of space hyperplane $z^Tx$ has to shift along the $d+1$ dimension to make itself a supporting hyperplane of $\epi f$. Note that the tangent points are exactly vertices of $\epi f$.

::: Proof
<!-- By definition of pwl convex function in high dimension, we can see that ... No... I think this is dual polyhedron. but it is quite complex -->
It is safe to write $f^*(z)=\max_x\{z^Tx-f(x)\}$ since we only consider the extended domain. Thus we have $f^*(z)=\max_x\{z^Tx-\max_i\{a_i^Tx+b_i\}\}=\max_x\{\max_i\{z^Tx-a_i^Tx+b_i\}\}$. Let $n$ be the number of vertices on $\conv (\epi f)$. One can see that $f^*(z)$ is the maximum of $O(nL)$ affine functions.
:::

I believe there will be only $O(n)$ hyperplanes on $f^*$ instead of $O(nL)$... However, we know that in general $f^*$ is the maximum of at least $O(n)$ functions since every vertex corresponds to a hyperplane in $\epi f^*$.

## pwl convex function in $\R$ $\circ$ a linear mapping

::: Problem
Let $f:\R^d\to\R$ be a pwl convex function.
Does there always exist a pwl convex $g:\R\to \R$ and a linear mapping $a^Tx-b:\R^d\to \R$ such that $f(x)=g(a^Tx-b)$. 
:::

As you expected, the answer is no. Let $f:\R^2\to \R$ be the maximum of a set of 2D planes. Consider a series of points $\set{p_1,p_2,...,p_k}$ on the 2D plane. After applying the linear mapping to $P=\set{p_1,p_2,...,p_k}$, we will get a sequence of numbers(points in 1D) $P'=\set{p_1',p_2',...,p_k'}$. We assume that $P'$ is non-decreasing. Note that the value of $g$ on $P'$ is always unimodal since $g$ is convex. However, the value of $f$ on $P$ may not be unimodal. Thus the composition of a linear mapping and a pwl convex function in 1D is not equivalent to pwl convex functions in high dimensions.

# sum of pwl convex functions

I want to show that in general the number of hyperplanes in the sum of pwl convex functions can be large. 

It is known that for two pwl convex functions $f,g$, $f^*+g^*=(f\square g)^*$. It is also known that $\epi f \oplus \epi g=\epi f\square g$(with some requirements on $f$ and $g$). There is a theorem in [@mountford_minkowski_nodate] section 4.3 which shows that the number of faces of the Minkowski sum of two polytopes can be huge. The bound can be reached by sums of cyclic polytopes. 

We can define pwl convex functions based on cyclic polytopes and we know that the Minkowski sum will have lots of faces(of different dimensions). We also know that the number of faces in $f^*$ is at least $n$ where $n$ is the number of vertices(faces of 1D) in $\epi f$. Now if the infimal convolution of two pwl convex functions also has many faces, the number of faces in the sum of pwl convex functions will be large.

::: Problem
Let $f_1,f_2$ be two pwl convex functions in $\R^d$. Let $n_1,n_2$ be the number of hyperplanes in $f_1,f_2$ respectively. What is the minimum number of hyperplanes in $f_1 \square f_2$?
:::

