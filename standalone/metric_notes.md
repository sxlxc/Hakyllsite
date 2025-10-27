---
title: Metric notes
subtitle: Outlier embedding
tags: metrics, optimization
author: Yu Cong
lang: en
# draft: true
date: 2025-05-26
showtoc: true
---

\DeclareMathOperator*{\polylog}{polylog}
\DeclareMathOperator*{\poly}{poly}

# Finite metric embeds into $\ell_1$

Bourgain's theorem states that there exists such an embedding with distortion $O(\log n)$. What if we only want to use tree metrics? It seems that the distortion becomes $O(\log n \log \log n)$. <https://chekuri.cs.illinois.edu/papers/packing.pdf>

# Largest finite metric $(X\subset \R^n,\ell_2^2)$

Arora, Rao and Vazirani made a famous work about approximating uniform sparsest cut to $O(\sqrt{\log n})$ via SDP [@arora_expander_2004]. Their SDP is finding a finite metric $(X,\ell_2^2)$. An interesting question is that how large can $X\subset \R^d$ be given that $(X,\ell_2^2)$ is a metric. Satisfying the triangle inequality is the same as requiring any three points in $X$ must form a non-obtuse triangle. One can see that the vertex set in the $n$ dimensional hypercube $Q_n$ is always a feasible solution. The hard part is the upperbound. I asked this problem on [math.sx](https://math.stackexchange.com/questions/5066765) and found a proof in *Proofs from THE BOOK* [chapter 17](https://link.springer.com/chapter/10.1007/978-3-662-57265-8_17).


# Shortest path representation

For any finite metric on $V$ there is a corresponding graph shortest path metric on $G=(V,E)$ with $c:E\to \R$. Given a finite metric on $V$, how to find $G$ with $|E|$ as small as possible? This looks similar to [a previous post in chinese](/posts/minDAG.html). How to prove that computing the minimum number of edges is NP-hard?

Consider a simple case that the metric space is 2D Euclidean space. One can see that the graph for the shortest path representation is a complete graph if and only if all the points in the 2D plane is in general position. However, deciding if a set of points in $\mathbb Q\times \mathbb Q$ is NP-complete. The reduction is from graph independent set. See [this post](/posts/points_in_general_position.html).

# Embedding tree metric into $(\R^{O(\log n)},\ell_\infty)$

There is an exercise in [lecture notes 1](https://home.ttic.edu/~harry/teaching/pdf/lecture1.pdf) of TTIC CMCS 39600. Show that any $n$ point tree metric $(X,d)$ can be embedded isometrically into $(\R^{O(\log n)},\ell_\infty)$.

Let's first try the Fréchet embedding $f_{Y}: X\to \R^{|Y|}$ for some $Y\subset X$.
Define $f_{Y}(x)=\bigoplus_{y\in Y} d(x,y)$, where $\oplus$ is the direct sum of vector coordinates. 
Then one has

\begin{equation*}
\begin{aligned}
\|f_Y(u)-f_y(v)\|_\infty    &= \max_{y\in Y} |d(y,u)-d(y,v)|\\
                            &\leq d(u,v).
\end{aligned}
\end{equation*}

The subset $Y$ needs to satisfy the followings,

1. $|Y|=O(\log n)$ (dimension bound)
2. for each pair of points $u,v\in X$, there exists an element $y\in Y$ such that $d(u,v)=|d(y,u)-d(y,v)|$. (isometric)

It turns out that a hard example is the star $K_{1,n-1}$ with unit edge length. Now suppose we select any subset $Y$ and get the embedding $f_Y$. There must be at least two degree one vertices (say $u,v$) not in $Y$ since $Y\ll n$. Then we have $|d(y,u)-d(y,v)|=0$ for any $y$. However, the tree metric shows $d(u,v)=2$. Thus our Fréchet embedding is not isometric...

Or is it? We can adjust constraints on the desired point set $Y$ a little bit. Consider the coordinate for $y\in Y$. Instead of setting the value of the $y$-corrdinate for each $u$ with $d(u,y)$, we can make it a function of $u,y$ and the centroid of the tree. The condition (2.) is basically asking for a vertex set $Y$ such that for any path $u\sim v$ in the tree there is a vertex $y\in Y$ such that there is a path $y\sim u \sim v$ or $u\sim v \sim y$. (We can see that $|Y|$ is in $O(n)$ via the star example.)
This condition can be changed to finding a path $u\sim y\sim v$. It is known that for any tree $T$ with $n$ vertices there is a vertex called centroid $c_T$ that decomposes the tree into 2 trees $L,R$ such that,

1. $L\cap R=\set{c_T}$,
2. $L\cup R=T$,
3. $|L|,|R|\leq \frac{2}{3}n$.

(using $L$ for the set of vertices in $L$...)

We maintain a vector for each vertex in the tree metric space.
Decompose the current tree $T$ into two parts $L$ and $R$ at the centroid $c_T$. Add a new dimension (the $c_T$-coordinate) for each vector: For $u\in L$ the new coordinate is $d(c_T,u)$; For $u\in R$ the new coordinate is $-d(c_T,u)$. One can prove by induction that the resulting vectors with $\ell_\infty$ norm is an isometrically embedding of the original metric space $(X,d)$. The dimension is the same as the depth of centroid decomposition, which is clearly $O(\log n)$.

This method is described in [@linial_geometry_1995, Theorem 5.3].


# $(k,c)$-outlier embedding into $\ell_2$ [@chawla_composition_2023]

Given two metric space $(X,d_X)$ and $(Y,d_Y)$, $(X,d_X)$ is said to have a $(k,c)$-outlier embedding into $(Y,d_Y)$ if there is a set $K\subset X$ of size at most $k$ and a mapping $\alpha: X\setminus K \to Y$ with distortion $\leq c$. Deciding if $(X,d_X)$ has a $(k,c)$-outlier embedding into $(Y,d_Y)$ is NP-Complete for $(Y,d_Y)=(\R^n, \ell_p)$.
Authors of [@chawla_composition_2023] provide a polytime algorithm that constructs an $(O(k\polylog k), O(c))$-outlier embedding into $\ell_2$ (thm 2.9). They noticed the followings

1. Given a subset $S\subset X$ of size $|X|-k$ and a partial embedding $\alpha: S \to Y$ with distortion $c_S$, there is a polynomial time algorithm that finds a weak $125 H_k c_S$-nested composition. (thm 2.6, note that the host space can be any Banach space, thus a weak $125 H_k c_S$-nested composition into $\ell_2$)
2. One can round a weak $125 H_k c_S$-nested composition for an $(O(k\polylog k), O(c))$-outlier embedding into $\ell_2$ (via rounding a SDP, lemma 3.1)

Weak $f(k,c)$-nested composition is somewhat stronger than $(k,c)$-outlier embedding since the former additionally requires an expansion bound on outlier points.
In fact I guess that the definition of weak $f$-nested composition is extracted from the SDP formulation of min-outlier.

\begin{equation}
\begin{aligned}
\min&   &   \sum_x \delta_x&    &   &\\
s.t.&   &   (1-\delta_x - \delta_y) d^2(x,y)\leq \|v_x-v_y\|^2 &\leq (c^2+(\delta_x+\delta_y)f(k)) d^2(x,y) &   &\forall x,y\in X\\
    &   &   \delta_x\in [0,1], v_x&\in \R^p   &   &\forall x\in X
\end{aligned}
\end{equation}

Note that this SDP is **not** a relaxation for $(k,c)$-outlier embedding (finding the smallest $k$ for fixed $c$) since the optimal solution may not satisfy the constraint $\|v_x-v_y\|^2\leq (c^2+f(k))d^2(x,y)$ for outlier $x$ and non-outlier $y$.
However, (1.) shows that if $(X,d)$ admits a $(k,c)$-outlier embedding, then there is a weak $125 H_k c_S$-nested composition, which implies this SDP has a feasible solution with $f(k)=125 H_k c$ and integral $\delta_x$.
Then one can use the rounding process in (2.) to get a $(O(\frac{\log^2 k}{\epsilon}k),(1+\epsilon)c)$-outlier embedding into $\ell_2$.