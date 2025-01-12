---
title: Notes on matroid representation
tags: matroid
author: Yu Cong
lang: en
# draft: true
---

> While reading <https://arxiv.org/pdf/2407.09477> (for a reading group), I realized that I lack knowledge about matroid representation.
> 
> This is a very incomplete notes on matroid representation related problems.

List of materials I briefly read:

1. <https://math.mit.edu/~goemans/18438F09/lec8.pdf>
2. <https://iuuk.mff.cuni.cz/~pangrac/vyuka/matroids/matroid-ch2.pdf>
3. <https://fardila.com/Clase/Matroids/LectureNotes/lectures1-25.pdf>

# Graphic matroids are regular

Consider the vertex-edge incidence matrix. Randomly orient the edges. $A_{v,e}=+1$ if $e$ enters $v$, $-1$ if $e$ leaves $v$, $0$ otherwise. Minimal linear dependent columns are exactly the cycles in the original graph. Take $+1$ to be the multiplicative identity and $-1$ its additive inverse over any field.

# If $M$ is linear, so is $M^*$

> <https://fardila.com/Clase/Matroids/LectureNotes/lectures1-25.pdf> page 21

In the dual matroid $M^*$, the groundset is the same as $M$ and the sum of their ranks is $n$.

The idea is, consider a linear matroid as a $r$ dimensional subspace $V$ in $\R^n$. Let $B^*$ be a basis of $M(V^\bot)$ ($V^\bot$ is the orthogonal complement of the column space of $V$) and $B$ be a basis of $M(V)$.
$B^*$ spans $V^\bot$ and the intersection of $V^\bot$ and the subspace spanned by vectors (that is $V$) in $E-B^*$ empty. The subspace spanned by $B^*$ is exactly the orthogonal complement of $V$ which is the subspace spanned by $B$.

An alternative proof is <https://math.mit.edu/~goemans/18438F09/lec8.pdf> Theorem 2.

This argument works over any field.
Thus both graphic matroids and cographic matroids are regular.

# Cographic matroids

Cycle rank is the minimum number of edges whose removal makes the graph cycle less.
For any spanning forest $F$, all edges outside $F$ must be removed since otherwise there will be a cycle.
The size of spanning forests are the same, i.e. $n-c$, where $n$ is the number of vertices and $c$ is the number of components. Thus cycle rank is the rank of the dual matroid of the graphic matroid on $G$. 

For graphic matroids on non-planar graphs, their dual may not be graphic. Thus in general we do not have a graph representation of cographic matroids. However, cographic matroids are still linear and cycle space gives its representation.

Edge space is a vector space over $\F_2$. Elements in the edge space of $G=(V,E)$ are subsets of $E$. Addition of two elements is taking their symmetric difference. Bases in the edge space are spanning forests and the rank of edge space is $n-c$.

Cycle space is naturally defined as the vector space spanned by all cycles (together with $\emptyset$). What is the rank of the cycle space? The rank is exactly $m-(n-c)$ since if we fix a spanning forest $F$ (of size $n-c$) any edge outside $F$ form a cycle and those cycles are linearly independent. The basis chosen in this way is called a *fundamental cycle basis*. Indeed, the cycle space can be spanned with all induced cycles.

Cut space contains all cuts of the graph(why is this a subspace?). One possible basis of the cut space is $\text{star}(v)$ for any $n-c$ vertices. Thus the rank of the cut space is $n-c$. The set of minimal cuts also span the cut space. One may observe the fact that the sum of ranks of cut space and cycle space is $m$. In fact, they are orthogonal complement of each other. For proofs see chapter 1.9 in [@diestel_graph_2017].

One important fact we are assuming is that cycle space and cut space are subspaces. This is trivial for graphic matroids since the symmetric difference of two cuts is still a cut and the symmetric difference of two cycle is still a cycle of union of disjoint cycles. Is this still true for non-graphic matroids?

Unfortuantely, for general matroids the set of circuit (or cocircuits) is not closed under taking symmetric difference. This can be seen from circuit axioms. We only have $C\subset C_1 \cup C_2\setminus e$ for any circuit $C_1, C_2$ and $e\in C_1\cap C_2$. For example, consider two circuits $\set{1,2,3}$ and $\set{2,3,4}$ in $U_{4,2}$, the symmetric difference, $\set{1,4}$, is independent.

A similar problem is discussed on [mathoverflow](https://mathoverflow.net/questions/241766/base-decomposition-of-matroids) concerning a special basis (like $\text{star}(v)$) in the "cocircuit space".
