---
author: Yu Cong
title:  Mutually orthogonal Latin squares and projective planes and hypergraphs and matching and base packing
tags: combinatorics
lang: en
date: 2024-03-01
---
finally a title with lots of 'and'

just wrote this for fun


\DeclareMathOperator{\R}{\mathbb{R}}
\DeclareMathOperator{\CH}{CH}
\DeclareMathOperator{\opt}{OPT}
\DeclareMathOperator{\mols}{MOLS}


### projective plane

some results from [https://www.homepages.ucl.ac.uk/~ucahbdo/FiniteProjectivePlanes.pdf](https://www.homepages.ucl.ac.uk/~ucahbdo/FiniteProjectivePlanes.pdf)

> Definition 2.1. A Projective plane P is an ordered pair of sets (p(P), l(P)), whose elements are
> called points and lines, respectively, and a relation between these sets, called incidence,
> satisfying the following axioms:
> 
> 1. Given any two distinct points, there is exactly one line incident with both of them.
> 2. Given any two distinct lines, there is exactly one point incident with both of them.
> 3. There are four points such that no line is incident with more than two of them.
> We say that a projective plane is finite if the number of points of the plane is finite.

points are vertices and lines are hypergraph edges. The axioms are saying

1. for any two vertives there is exactly one edge contain them.
2. for any two edges their intersection is one vertex.
3. you can find 4 vertices in the hypergraph that no edge contains more than 2 of them.


the smallest projective plane is the [Fano plane](https://en.wikipedia.org/wiki/Fano_plane)

<!-- ![a larger example from math SE](https://i.stack.imgur.com/uoNl7.jpg) -->
<!-- *a larger example from [math se](https://math.stackexchange.com/questions/2262517/projective-plane-of-uniformity-4)* -->

**Theorem 2.5.** for any point in the projective plane, the number of lines incident is the same

**proof**: take $a\in P$ and $l\in L$ such that $a\not\in l$, by axiom 1 one can see that for any point $p$ on $l$ there is a unique line incident with both $p$ and $a$, thus the number of lines incident with $a$ is the same as the number of points on $l$. then take another point $b\in P$ and consider $b$ and $l$ in the same way. $a,b$ always exist and guaranteed to be different by aixiom 3. Hence the number of lines incident with $a$ = # lines incident with $b$ = # points on $l$.
<div dir="rtl">&#8718;</div>


by duality similar theorem holds for lines.

suppose the number of lines incident with every point is $k+1$, then define the order of the projective plane to be $k$.

One can see that the number of lines and points on a projective plane are the same. Suppose the order of the projective plane is $k$ and there will be $k+1$ lines incident with each point and on each line there will be $k+1$ points. Thus the number of lines is $(k+1)n/(k+1)=n$, the same as points.


Theorem 2.5. means the corresponding hypergraph must be $k+1$-uniform and $k+1$-regular

For special(satisfying the above 3 conditions) $k+1$-uniform $k+1$-regular hypergraph we easily know the number of edges and vertices is $k(k+1)+1$, so order $n$ projective plane has exactly $n^2+n+1$ points and lines.

However not all order is possible. All known order of projective plane is prime power. The existence of finite projective planes of other orders is an open question. see [https://en.wikipedia.org/wiki/Projective_plane#Finite_projective_planes](https://en.wikipedia.org/wiki/Projective_plane#Finite_projective_planes)


### Mutually orthogonal Latin squares(MOLS)

[Latin square](https://en.wikipedia.org/wiki/Latin_square) is just sudoku without the property of no repeated values in any of the nine blocks. Latin squares can be of size $n\times n$ not just $9\times 9$.

[https://en.wikipedia.org/wiki/Mutually_orthogonal_Latin_squares](https://en.wikipedia.org/wiki/Mutually_orthogonal_Latin_squares)

There is a thick book about this topic. [*Latin Squares and their application*](http://ndl.ethernet.edu.et/bitstream/123456789/77164/1/2.pdf) see chapter 5.2 for details about MOLS and projective planes.

from now on we only consider Latin squares of the same size.

Given two $n\times n$ Latin squares $A$ and $B$, make a new $n\times n$ square $C$(called Graeco-Latin square or Euler square or pair of orthogonal Latin squares). $C[i,j]$ is an ordered pair of $A[i,j]$ and $B[i,j]$. If there are no two cells in $C$ contains the same ordered pair, then the two latin squares are called orthogonal.

A set of Latin squares are orthogonal if and only if they are pairwise orthogonal.

Denote a set of $n\times n$ mutually orthogonal Latin squares by $\mols(n)$. I am insterested in $|\mols(n)|$.

One can see that $|\mols(n)| \leq n-1$ since ... well i don't know how to prove this. I find a proof from [*Latin Squares and their application*](http://ndl.ethernet.edu.et/bitstream/123456789/77164/1/2.pdf).

![*Latin Squares and their application* page 161](/images/MOLS/theorem512.png)


#### How is MOLS related to projective planes?

The question, for which $n$ $\max |\mols(n)|=n-1$ is still open.

**Theorem 5.2.2** Every finite projective plane of order n defines at least one complete set of MOLS of order n; and conversely a complete set of MOLS of order n defines a finite projective plane

**proof**:

1. projective plane => MOLS. Consider an order $n$ projective plane and choose one line $l$. There will be $n+1$ lines on $l$, say they are $x,b_1,...,b_{n-1},y$. For any point there will be $n+1$ lines incident with it. Denote $n$ lines(other than $l$) incident with $b_i$ by $b_{i1},...,b_{in}$. Denote these lines incident with $x$(or $y$) by $x_1,...,x_n$. Note that for any two points on the projective plane there will be exactly one line incident with both of them. Consider one of the $n^2$ points $a$ other than $x,b_1,...,b_{n-1},y$. There will $n+1$ lines incident with $a$ and there are $n+1$ points on $l$. Thus $a$ can be identified with the ordered tuple $i,s_1,...,s_{n-1},j$ of lines incident with both some $p\in \{x,b_1,...,b_{n-1},y\}$ and $a$. We write $s_k$ in the $(i,j)$th place of the $k$th Latin square. One can easily verify that this construction guarantees a MOLS of size $n-1$.
2. MOLS => projective plane. Juse the inverse of the previous construction. Not hard to see the resulting structure is an order $n$ projective plane.

<div dir="rtl">&#8718;</div>

### truncated projective plane

Truncated projective plane is just a projective plane removing one point and all lines incident with that point.

The interesting property is that if considered as hypergraphs truncated projective plane of order $r$ is an $r+1$-partite hypergraph and each line intersects with every other lines.

**proof**: The $r+1$-partites are exactly those sets of vertices on the deleted edges. They are disjoint since otherwise there will be two edges contains two common vertices. One can see that if one edge contains two vertices in the same part, this edge should be exactly the edge contains all vertices in this part and thus it should have been deleted. So no edge contains more than 1 vertex in one part.

The min vertex cover is $r-1$ since all vertices in one partite are needed.

The fractional matching(hitting set) = fractional vertex cover(edge packing) = $r-1$ if we assign $\frac{1}{r-1}$ to every hyperedge. (note: they are dual)

So for truncated projective planes the packing-cogirth gap is really large.

### [Ryser's conjecture](https://en.wikipedia.org/wiki/Ryser%27s_conjecture)

For hitting sets and edge packing in hypergraphs, one can see that <br>
max edge packing($\tau$) <= max fractional edge packing($\tau*$) = min fractional hitting set($\lambda*$) <= min hitting set($\lambda$)

moreover for $r$-uniform hypergraphs, it is easy to see that $r\tau\geq \lambda$.

Ryser's conjecture says for $r$-uniform $r$-parite hypergraphs $(r-1)\tau \geq \lambda$. It has not been proven yet.

There is another Ryser-Brualdi-Stein conjecture about size of transversals in Latin squares. recent work on arxiv <https://arxiv.org/pdf/2310.19779.pdf>

Matroid bases can be considered as edges of uniform hypergraphs.
For which kind of matroid the corresponding hypergraph has a small upperbound for $\lambda/\tau$?
