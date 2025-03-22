---
title: Detecting base orderable matroids is NP-hard
tags: matroids
lang: en
author: Yu Cong
# draft: true
date: 2025-03-22
---

> This is posted on <https://mathoverflow.net/questions/194006/>. But I guess no one cares about negetive results on the algorithmic part of base orderability. The OP left mathoverflow 7 years ago.

Detecting (strong) base orderability of a general matroid is not in P under the independence oracle model. The proof idea is to combine an excluded minor theorem for base-orderability on paving matroids and Seymour&Walton's theorem on the complexity of detecting matroid minors.

**Thm1** [Bonin and Savitsky, https://arxiv.org/pdf/1507.05521] *A paving matroid is base orderable iff it has no $M(K_4)$ minor.*

Let $\mathscr C\not=\emptyset$ be a collection of matroids. We say $\mathscr C$ is detectable if one can check for a matroid $M$ whether $M$ contains any $M'\in\mathscr C$ as a minor in polynomial number of oracle calls.

**Thm2** [Seymour and Walton] *If $\mathscr C$ is detectable, then $\mathscr C$ contains at least one uniform matroid.*

In order to show $M(K_4)$ is not detectable in paving matroids, we can prove that Thm2 holds on paving matroids. The proof in Seymour and Walton's paper constructs a special matroid $M(A,B,E)$ based on any matroid $M'\in \mathscr C$ (, which is non-uniform by assumption). They show that the constructed matroid requires an exponentional number of oracle calls to be distinguished from a uniform matroid. Their construction is as follows:

Let $E$ be the ground set and $r$ the rank function of $M'$. 
The groundset $S$ of $M(A,B,E)$ has size $2t+|E|$ where $t$ is any positive integer. Let $A,B,E$ be a partition of $S$ where $|A|=|B|=t$.  A subset $X\subseteq S$ is independent iff following conditions are both met,

1. $|X\cap A|+|X\cap C|\le t+r(X\cap C)$,
2. $|X|\leq t+r(M')$

In fact, $M(A,B,E)$ is paving if $M'$ is paving. Thus Seymour's proof applies to paving matroids and detecting $M(K_4)$ minor requires exponentional time even in paving matroids.

For strong base orderability, there is a infinite set of excluded minors[https://arxiv.org/pdf/1507.05521, section 9]. However, none of the excluded minors is unifor. I think Seymour and Walton's proof still applies.


<cite authors="Seymour, P. D.; Walton, P. N.">_Seymour, P. D.; Walton, P. N._, [**Detecting matroid minors**](https://doi.org/10.1112/jlms/s2-2.2.193), J. Lond. Math. Soc., II. Ser. 23, 193-203 (1981). [ZBL0487.05016](https://zbmath.org/?q=an:0487.05016).</cite>

<cite authors="Bonin, Joseph E.; Savitsky, Thomas J.">_Bonin, Joseph E.; Savitsky, Thomas J._, [**An infinite family of excluded minors for strong base-orderability**](https://doi.org/10.1016/j.laa.2015.09.055), Linear Algebra Appl. 488, 396-429 (2016). [ZBL1326.05025](https://zbmath.org/?q=an:1326.05025).</cite>