---
title: Notes on Matroid Representation
tags: matroid
author: Yu Cong
lang: en
---

While reading <https://arxiv.org/pdf/2407.09477> (for a reading group) I realized that I lack knowledge about matroid representation.

This is a very incomplete notes on matroid representation related problems.

List of materials I (briefly) read:

1. <https://math.mit.edu/~goemans/18438F09/lec8.pdf>
2. <https://iuuk.mff.cuni.cz/~pangrac/vyuka/matroids/matroid-ch2.pdf>
3. <https://fardila.com/Clase/Matroids/LectureNotes/lectures1-25.pdf>

### graphic matroids are regular

Consider the vertex-edge incidence matrix. Randomly orient the edges. $A_{v,e}=+1$ if $e$ enters $v$, $-1$ if $e$ leaves $v$, $0$ otherwise. Minimal linear dependent columns are exactly the cycles in the original graph. Take $+1$ to be the multiplicative identity and $-1$ its additive inverse over any field.

### if $M$ is linear, $M^*$ is linear

<https://fardila.com/Clase/Matroids/LectureNotes/lectures1-25.pdf> page 21

In the dual matroid $M^*$, the groundset is the same as $M$ and the sum of their ranks is $n$.

The idea is, consider a linear matroid as a $r$ dimensional subspace $V$ in $\R^n$. Let $B^*$ be a basis of $M(V^\bot)$ and $B$ be a basis of $M(V)$.
$B^*$ spans $V^\bot$ and the intersection of $V^\bot$ and the subspace spanned by vectors (that is $V$) in $E-B^*$ empty. The subspace spanned by $B^*$ is exactly the orthogonal complement of $V$ which is the subspace spanned by $B$.

An alternative proof is <https://math.mit.edu/~goemans/18438F09/lec8.pdf> Theorem 2.

This argument works over any field.

Thus both graphic matroids and cographic matroids are regular.

### matroids on a graph

Given a undirected graph $G=(V,E)$, we can define graphic matroid on it. However, there are other matroids related to $G$.

#### cycle space
