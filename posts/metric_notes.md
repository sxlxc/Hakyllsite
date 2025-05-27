---
title: Metric notes
tags: CG, optimization
author: Yu Cong
lang: en
draft: true
date: 2025-05-26
---

\DeclareMathOperator*{\polylog}{polylog}

# Finite metric embeds into $\ell_1$

Bourgain's theorem states that there exists such an embedding with distortion $O(\log n)$. What if we only want to use tree metrics? It seems that the distortion becomes $O(\log n \log \log n)$. <https://chekuri.cs.illinois.edu/papers/packing.pdf>

# Shortest path representation

For any finite metric on $V$ there is a corresponding graph shortest path metric on $G=(V,E)$ with $c:E\to \R$. Given a finite metric on $V$, how to find $G$ with $|E|$ as small as possible? This looks similar to [a previous post in chinese](/posts/2023-01-26-minDAG.html). How to prove that computing the minimum number of edges is NP-hard?

# $(k,c)$-outlier embedding into $\ell_2$ [@chawla_composition_2023]

Given two metric space $(X,d_X)$ and $(Y,d_Y)$, $(X,d_X)$ is said to have a $(k,c)$-outlier embedding into $(Y,d_Y)$ if there is a set $K\subset X$ of size at most $k$ and a mapping $\alpha: X\setminus K \to Y$ with distortion $\leq c$. Deciding if $(X,d_X)$ has a $(k,c)$-outlier embedding into $(Y,d_Y)$ is NP-Complete for $(Y,d_Y)=(\R^n, \ell_p)$.
Authors of [@chawla_composition_2023] provide a polytime algorithm that constructs an $(O(k\polylog k), O(c))$-outlier embedding into $\ell_2$ (thm 2.9). They noticed the followings

1. Given a subset $S\subset X$ of size $|X|-k$ and a partial embedding $\alpha: S \to Y$ with distortion $c_S$, there is a P time alg that finds a weak $125 H_k c_S$-nested composition. (thm 2.6, note that the host space can be any Banach space, thus a weak $125 H_k c_S$-nested composition into $\ell_2$)
2. A weak $125 H_k c_S$-nested composition into $\ell_2$ implies an $(O(k\polylog k), O(c))$-outlier embedding into $\ell_2$ (thm 2.9, via Outlier SDP, lemma 3.1)

[2.] is easy. The hard part is [1.].