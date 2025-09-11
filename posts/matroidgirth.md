---
title: Matroid girth
tags:  matroid, optimization, combinatorics
author: Yu Cong
lang: en
# draft: true
showtoc: true
date: 2025-08-12
---

\DeclareMathOperator{\girth}{girth}

Let $M=(E,\mathcal I)$ be a matroid with non-negative weights $w:E\to \R_{\geq 0}$. 
The girth of $M$ is 
\[
    \min \set{\sum_{e\in C} w(e): \text{$C$ is a circuit of $M$}}.
\]

Cogirth of $M$ is the girth of the dual matroid of $M$.

Computing girth is NP-hard for binary matroids but can be done in polynomial time for graphs.
[Wikipedia](https://en.wikipedia.org/wiki/Matroid_girth#Computational_complexity) lists some negative complexity results, which mainly concern more general matroid classes than binary matroids. 
So here are some positive results filling the gap between graphic matroids and binary matroids. (Results can be found in <https://matroidunion.org/?p=1106>.) Proofs that you don’t see here can easily be found in the references.


# Regular matroid

::: {.Theorem title="Seymour decomposition [@seymour_decomposition_1980]" #regulardecomp}
Every regular matroid may be constructed by combining graphic matroids, cographic matroids, and a certain ten-element regular matroid that is neither graphic nor cographic, using 3 binary operations:

- 1-sum is direct sum of two matroids
- 2-sum is patching two matroid on 1 common element
- 3-sum is patching two matroids on 3 common elements
forming a 3-circuit in each matroid.
:::

This decomposition can be found in polynomial time. One can decide if a matroid $M$ can be decomposed into $M_1$ and $M_2$ using 1/2/3-sum in polynomial time. See <https://www.emis.de/monographs/md/index.html>.

[@regulardecomp] leads to a natural algorithm for computing the girth in regular matroids. The decomposition of regular matroids gives us a binary tree, where each node is a regular matroid and each leaf is either (co)graphic or a special 10 element regular matroid.
Every non-leaf node in the decomposition tree represents a regular matroid $M$ which is 1/2/3-sum of its two direct decendents $M_1$ and $M_2$. Let $A \oplus_i B$ be the $i$-sum of $A$ and $B$ for $i\in [3]$. Now there are only 3 cases:

1. $M=M_1\oplus_1 M_2$. Direct sum does not create new circuit. $\girth(M)=\min \set{\girth(M_1),\girth(M_2)}$.
2. $M=M_1\oplus_2 M_2$. Let $e$ be the common element of $E(M_1)$ and $E(M_2)$. In this case there may be new circuits. However, any circuit of $M$ which is not a circuit of $M_1$ and $M_2$ must be contained in $C_1\cup C_2\setminus \set{e}$, where $C_i$ is a circuit in $M_i$ containing $e$. Thus to find the minimum weighted new circuit we can compute the minimum circuit in $M_1$ containing $e$ (say $C_1^*$) and replace the weight $w(e)$ in $M_2$ with $w(C_1^*)-w(e)$ and then find the minimum weight circuit in $M_2$ containing $e$. The girth of $M$ is the minimum among $\girth(M_1)$, $\girth(M_2)$ and $\min\set{w(C): \text{$C$ is new circuit}}$. 

    We need to prove that all these operations can be done in polynomial time. 

    For the 2/3-sum case we need to find in at least one of the summands the minimum circuit that contains a common element $e$. However, finding such a minimum circuit is regular matroids is not known to be polynomially solvable.
    To understand what's happening here we need to look into ~~Seymour's proof[^1]~~ the decomposition tree. 

    Instead of considering our binary decomposition tree, we now construct a new graph $G$ where each vertex represents a graphic matroid, cographic matroid or $R_{10}$ and there is an edge between two vertices if the corresponding matroids are patched using 1/2/3-sum. We claim that there is no cycle in the graph. The graph is connected. Assume that there is a cycle and let $M_1,M_2$ be two matroids whose corresponding vertices are in the cycle. Consider the LCA $M$ of $M_1$ and $M_2$ in the binary tree. $M$ represents a connected subgraph $H\subset G$ that contains $M_1$ and $M_2$ but not the entire cycle since otherwise there will be 2 1/2/3-sum operation between two regular matroids. However, $M_1$ and $M_2$ are still connected in $G-E[H]$ since $M_1$ and $M_2$ are in the same cycle, which contradicts the uniqueness of the LCA.

    Thus $G$ is a tree and we can always assume that one of the matroids in the summands is graphic matroid, cographic matroid or $R_{10}$. Finding the minimum circuit containing a fixed element can be done in those matroids in polynomial time and there exists a algorithm that computes the tree in cubic time [@truemper_decomposition_1990].

3. $M=M_1\oplus_3 M_2$. Similar to the 2-sum case. There are only 3 common elements. We can enumerate all circuits of $M_1$ which contain one of the common elements.

However, deciding whether a regular matroid has a circuit of length at most k containing two fixed elements [is FPT](https://mathoverflow.net/questions/434026/algorithm-for-finding-a-minimum-weight-circuit-in-a-weighted-binary-matroid#comment1118055_434045).

# Proper minor-closed class of binary matroids

The most important problem in this field is the following.

::: {.Conjecture title="Geelen, Gerards, and Whittle [@Geelen_Gerards_Whittle_2015]" #conjgirth}
For any proper minor-closed class $\mathcal M$ of binary matroids, there is a polynomial-time algorithm for computing the girth of matroids in $\mathcal M$.
:::

Similar to Seymour's decomposition for regular matroids, every proper minor-closed class of binary matroid admits a "decomposition" into graphic matroids and some binary matroids.

::: {.Theorem title="[@Geelen_Gerards_Whittle_2015]" #binarydecomp}
For each proper minor-closed class $\mathcal M$ of binary matroids, there exist integers $k,t\geq 0$ such that for each vertically $k$-connected matroid $M\in \mathcal M$, there exist matrices $A,P\in \F_2^{r\times n}$ such that $A$ is the incidence matrix of a graph, $r(P)\leq t$ and either $M=M(A+P)$ or $M^*=M(A+P)$.
:::

The matroids $M(A+P)$ in [@binarydecomp] are called perturbed graphic matroids. Note that we can consider $k$ and $t$ in [@binarydecomp] as constants since for each minor-closed class they are fixed.

Using [@binarydecomp], [@conjgirth] is true if one can prove the followings:

1. there is a polynomial-time alg that finds the girth of $M(A+P)$;
2. One can reduce the problem of computing the girth of members of $\mathcal M$ to that of computing the girth of vertically $k$-connected members of $\mathcal M$.

# Perturbed graphic matroids

Jim Geelen and Rohan Kapadia [@geelen_computing_2018] showed that the (co)girth can be computed in randomized polynomial time for a subclass of binary matroids called perturbed graphic matroids. They made a reduction from the (co)girth problem of perturbed graphic matroids to graph cuts and matchings using $(s,t)$-signed-grafts. IMO the reduction is quite tricky. Let $s$ and $t$ be two non-negative integers. An $(s,t)$-signed-graft is a tuple $(G,S,T,B,C,D)$ such that:

- $G$ is a graph,
- $S$ is an $s$-element set disjoint from $V(G)$,
- $T$ is a $t$-element set disjoint from $E(G)$,
- $B,C,D$ are 0-1 matrices.

The incidence matrix of an $(s,t)$-signed-graft $(G,S,T,B,C,D)$ is
\[
A = \begin{array}{ccc}
      & \begin{array}{cc} E(G) & T \end{array} \\
    \begin{array}{c} V(G) \\ S \end{array}
      & \left(
        \begin{array}{cc}
          A(G) & B \\
          C & D 
        \end{array}
      \right)
\end{array}
\]
where $A(G)$ is the incidence matrix of $G$. Denote the matroid $M(A)$ by $M(G,S,T,B,C,D)$.

::: {.Lemma title="[@geelen_computing_2018,Lemma 4.1]" #lemgraft}
Let $G$ be a graph and let $P\in \F_2^{V(G)\times E(G)}$ be a rank-$t$ matrix. Then there is a $(t,t)$-signed-graft $(G,S,T,B,C,D)$ such that 
\[M(A(G)+P)=M(G,S,T,B,C,D)/T.\]
:::
The proof is taking $B,C$ as a rank decomposition of $P$ and applying some row operations.

Recall that [@binarydecomp] says that each vertically $k$-connected matroid $M$ in a proper minor-closed class of binary matroids is *either* $M(A+P)$ or $M(A+P)^*$. One has to consider the girth and cogirth problem separately.

## Reductions

::: {.Lemma title="the cogirth part. [@geelen_computing_2018,Lemma 4.2]" #lemcogirth}
Let $(G,S,T,B,C,D)$ be an $(s,t)$-signed-graft and $S'$ be a one-element set disjoint from $V(G)$. The cogirth of $M(G,S,T,B,C,D)/T$ is the mimimum of the cogirths of matroids $M(G,S',T,B,yC,yD)/T$ taken over all $y\in \F_2^{S'\times S}$.
:::

::: Proof
To see this lemma, I suggest considering the flats instead of cocycles.

- Each flat in $M=M(G,S',T,B,yC,yD)$ is also a flat $M'=M(G,S,T,B,C,D)$. Let $F'$ be a flat of $M'$ and $F$ be the corresponding set in $M$. If there is an element $e$ of $M\setminus F$ such that $e$ is linearly representable by vectors in $F$. Then $e$ is also representable by vectors in $F'$ by linearality of the multiplication.
- For each hyperplane $H$ in $M$, there is a $y\in \F_2^{S'\times S}$ such that $F'$ is a flat of $M'$.
Note that this only works for cocircuits (hyperplanes) but not cocycles (flats). We can assume that the $A(G),B$ part is empty. Let the first $k$ columns be the hyperplane $H$. Then the matrix is 
\[
M=\begin{pmatrix}
H & U
\end{pmatrix}.
\]
We want to show that there is a $y\in \F_2^s$ such that $H^T y=\mathbf{0}$ and $U^T y=\mathbf{1}.$ Let $B$ be a base in this linear matroid. Apply row operations to make $B$ a standard basis (at most one "1" in each column). The intersection of $B$ and $H$ has exactly $r-1$ vectors. Now we construct the vector $y.$ If there is any vector in $B\cap H$ that has a "1" in the $k$-th coordinate, let $y[k]=0$; Otherwise, we set $y[k]=1.$ Note that $H^T y=\mathbf{0}$ and $U^T y=\mathbf{1}.$ Thus $H$ remains a hyperplane in $M'.$
:::

::: {.Lemma title="the girth part. [@geelen_computing_2018,Lemma 4.3]" #lemgirth}
Let $(G,S,T,B,C,D)$ be an $(s,t)$-signed-graft and $T'$ be a one-element set disjoint from $E(G).$ The girth of $M(G,S,T,B,C,D)/T$ is the mimimum of the girths of matroids $M(G,S',T,Bx,C,Dx)/T$ taken over all $x\in \F_2^{T\times T'}.$
:::

It follows from [@lemgraft] we need to consider the (co)girth of $M/T$ where $M$ is the matroid of an $(s,t)$-signed-graft.

## cogirth → even cuts


By [@lemcogirth], to compute the cogirth of an $(s,t)$-signed-graft we only need to consider binary matroid of the following kind:

\[
A=
\begin{array}{ccc}
      & \begin{array}{cc} E(G) & T \end{array} \\
    \begin{array}{r} V(G) \\ \set{v} \end{array}
      &
        \begin{pmatrix}
          A(G) & B \\
          \sigma & \alpha
        \end{pmatrix}
\end{array}
\]

We want to find the cogirth of $M(A)/T$.
One useful proposition is the following. (See [this post](/posts/misleading_ideas.html#cocircuit-space-of-binary-matroids) for a proof sketch.)

::: {.Proposition title="[@oxley_matroid_2011,Proposition 9.2.2]"}
Let $A$ be a binary representation of a rank-$r$ binary matroid
$M$. Then the cocircuit space of $M$ equals the row space of $A$. Moreover, this space
has dimension $r$ and is the orthogonal subspace of the circuit space of $M$.
:::

What we are finding is the minimum support of vectors in the row space of $A$ such that the support has empty intersection with $T$. Note that the support of rows in the graph incidence matrix $A(G)$ has interpretation. They are exactly $\delta(X)$ where $X$ is the set of vertices for the summand rows. Thus we divide the problem into 2 cases. Let $B[u]$ be a $t$-dimentional binary label on each vertex.

1. The row indexed by $\set{v}$ is not in the solution. Find the smallest non-empty cut $\delta(X)$ in $G$ such that $\sum_{u\in X}B[u]=\mathbf 0$.
2. The row indexed by $\set{v}$ is in the solution. Now $\sigma$ represents a subset of edges in $G$. We want to find a cut $\delta(X)$ such that $\sigma \Delta \delta(X)$ is minimized and non-empty and $\sum_{u\in X}B[u]=\alpha$.

These are called the $t$-dimensional even-cut problem.
Geelen and Kapadia discovered a random contraction algorithm which solves both of the problems in randomized polynomial time [@geelen_computing_2018].

If the graph is not connected, it is possible that $\delta(X)$ is empty even if $X$ is non-empty. Fortunately, Geelen and Kapadia have done the reduction to connected graphs.

Note that the size of cut $|\delta(X)|$ is a submodular function on $V(G)$ but $|\delta(X)\Delta \sigma|$ is not necessarily submodular. The first case is minimizing a symmetric submodular function under some congruency constraints.

::: {.Problem title="Generalised Congruency-Constrained Submodular Minimization (GCCSM)" #probGCCSM}
Let $f:2^N \to \Z$ submodular, $p$ prime, $k\in \Z_{\geq 0}$, $r_1,\dots,r_k\in \Z_p$, and $S_1,\dots,S_k\in N$. 
\begin{equation*}
\begin{aligned}
\min& &  f(S)&  & &\\
s.t.& & |S\cap S_i|&\equiv r_i  & &\forall i\in [k]\\
    & & S&\subset N
\end{aligned}
\end{equation*}
:::

Nägele, Sudakov and Zenklusen showed that [@probGCCSM] can be done in polynomial time if the field is small and the number of congruency constraints is constant [@Nägele_Sudakov_Zenklusen_2019].

::: {.Theorem title="[@Nägele_Sudakov_Zenklusen_2019]"}
[@probGCCSM] can be solved in time $|N|^{2kp+O(1)}$.
:::

I think currently (Sep 2025) finding a deterministic polynomial time algorithm for $t$-dim even cut is still open.

## girth → parity cycle + parity join

Similar to the cogirth part, by [@lemgirth] we consider the following matrix

\[
A=
\begin{array}{ccc}
      & \begin{array}{cc} E(G) & \set{f} \end{array} \\
    \begin{array}{r} V(G) \\ S \end{array}
      &
        \begin{pmatrix}
          A(G) & b \\
          C & d
        \end{pmatrix}
\end{array}
\]
and we want to compute the girth of $M(A)/\set{f}$. Each edge in $G$ has a label $c(e)\in\F_2^{s}$ (the submatrix $C$). Contracting an element in a matroid may change circuits. There are two cases:

1. The minimum circuit does not contain $\set{f}$. Then the girth of $M(A)/\set{f}$ is the same as $M(A)\setminus \set{f}$. In this case we need to find the minimum cycle in $G$ such that the sum of its edge labels is exactly $\mathbf{0}$. This is the parity cycle problem.
2. The minimum circuit contains $\set{f}$. Let the girth of $M(A)/\set{f}$ be $\lambda$. Then $M(A)$ has a minimum circuit that contains $f$ and has size $\lambda+1$. Let $T$ be the set of vertices whose characteristic vector is $b$. To find the minimum circuit of $M(A)$, we want to find the minimum edge set $F\subset E$ such that the sum of labels is $d$ and $T$ is exactly the set of vertices with odd degree in $G[F]$. This is called the parity $T$ join problem.

Recently, Schlotter and Sebő find FPT time algorithm for the odd $T$-join problem[@schlotter_odd_2025].




[^1]: *I realized that one doesn't need to understand Seymour's 55-page paper to see why the desired operations can be done in polynomial time...*<br>
The proof of [@regulardecomp] has 3 parts:\
\
- There is a special 10-element regular matroid $R_{10}$ such that any regular matroid can be obtained by 1/2-sums from regular matroids without $R_{10}$ minor and copies of $R_{10}$. 
    (Now we can assume that we are working with regular matroids which have no $R_{10}$ minor and are not separable via 1/2-sum.)\
\
- There is another 12-element regular matroid $R_{12}$ such that any regular matroid can be obtained by 1/2/3-sums from matroids without $R_{12}$ minor. 
    (Now we are working with regular matroids that are not separable via 1/2/3-sum and have no $R_{10}$ or $R_{12}$ minors.)\
\
- Every 3-connected regular matroid which is neither graphic nor cographic has an $R_{10}$ or $R_{12}$ minor.
    Let $M$ be a matroid. $M$ is 3-connected iff $M$ is not expressible as a 1- or 2-sum. (cf. [@seymour_decomposition_1980] 2.10(b))
    It follows that the remaining regular matroids are graphic or cographic.