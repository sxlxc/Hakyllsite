---
title: Metric notes
tags: CG, optimization
author: Yu Cong
lang: en
draft: true
date: 2025-05-26
---

\DeclareMathOperator*{\polylog}{polylog}
\DeclareMathOperator*{\poly}{poly}

# Finite metric embeds into $\ell_1$

Bourgain's theorem states that there exists such an embedding with distortion $O(\log n)$. What if we only want to use tree metrics? It seems that the distortion becomes $O(\log n \log \log n)$. <https://chekuri.cs.illinois.edu/papers/packing.pdf>

# Shortest path representation

For any finite metric on $V$ there is a corresponding graph shortest path metric on $G=(V,E)$ with $c:E\to \R$. Given a finite metric on $V$, how to find $G$ with $|E|$ as small as possible? This looks similar to [a previous post in chinese](/posts/2023-01-26-minDAG.html). How to prove that computing the minimum number of edges is NP-hard?

# $(k,c)$-outlier embedding into $\ell_2$ [@chawla_composition_2023]

Given two metric space $(X,d_X)$ and $(Y,d_Y)$, $(X,d_X)$ is said to have a $(k,c)$-outlier embedding into $(Y,d_Y)$ if there is a set $K\subset X$ of size at most $k$ and a mapping $\alpha: X\setminus K \to Y$ with distortion $\leq c$. Deciding if $(X,d_X)$ has a $(k,c)$-outlier embedding into $(Y,d_Y)$ is NP-Complete for $(Y,d_Y)=(\R^n, \ell_p)$.
Authors of [@chawla_composition_2023] provide a polytime algorithm that constructs an $(O(k\polylog k), O(c))$-outlier embedding into $\ell_2$ (thm 2.9). They noticed the followings

1. Given a subset $S\subset X$ of size $|X|-k$ and a partial embedding $\alpha: S \to Y$ with distortion $c_S$, there is a P time alg that finds a weak $125 H_k c_S$-nested composition. (thm 2.6, note that the host space can be any Banach space, thus a weak $125 H_k c_S$-nested composition into $\ell_2$)
2. A weak $125 H_k c_S$-nested composition into $\ell_2$ implies an $(O(k\polylog k), O(c))$-outlier embedding into $\ell_2$ (thm 2.9, via Outlier SDP, lemma 3.1)

Weak $f(k,c)$-nested composition is somewhat stronger than $(k,c)$-outlier embedding since the former additionally requires an expansion bound on $X$.
In fact I guess that the definition of weak $f$-nested composition is extracted from the SDP formulation of min-outlier SDP.

\[
\begin{equation}
\begin{aligned}
\min&   &   \sum_x \delta_x&    &   &\\
s.t.&   &   (1-\delta_x - \delta_y) d^2(x,y)\leq \|v_x-v_y\|^2 &\leq (c^2+(\delta_x+\delta_y)f(k)) d^2(x,y) &   &\forall x,y\in X\\
    &   &   \delta_x\in [0,1], v_x&\in \R^p   &   &\forall x\in X
\end{aligned}
\end{equation}
\]

Note that the dimension $p$ of $v_x$ can be $\poly(n)$. Lemma 3.1 shows that this SDP is a relaxation of weak $\sqrt{f(k)}$-nested composition. This SDP is also a relaxation of $(k,c)$-outlier embedding (find smallest $k$ for fixed $c$). The proof of thm 2.9 (the 2. above) is rouding the solution of SDP. Thm 2.6 (1. above) is showing that this SDP with $f(k)=125c H_k$ admits a solution with objective at most $k$.

