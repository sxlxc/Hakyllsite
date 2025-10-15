---
title: Ultrametric notes
tags: LP, metrics
author: Yu Cong
lang: en
draft: true
date: 2025-10-13
showtoc: true
---

\DeclareMathOperator*{\lca}{LCA}

<https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.APPROX/RANDOM.2023.2>

# ultrametrics

::: {.Definition title="ultrametrics"}
A metric space $(X,d)$ is a ultrametric if it satisfies the strong triangle inequality:
\[
    d(x,y)\leq \max\{d(x,z),d(z,y)\} \quad \forall x,y,z\in X
\]
:::

Ultrametric is a special case of tree metric. To see this, consider the equivalence relation $x \sim_{\leq r} y$ if $d(x,y)\leq r$.
For each fixed $r$ there will be many equivalent classes. We collect all equivalent class for all possible $r$. Let this collection of subsets be $L$.
Note that we dont really have to consider all values of $r$ but only smallest $r$ such that the set of equivalent classes changes.

::: {.Observation}
$L$ is laminar.
:::

Now we have a tree-like structure. The root of the tree is the unique equivalent class of $\sim_{\leq \max d(x,y)}$. The leaves will be equivalent classes of $\sim_{\leq 0}$, i.e., singleton of elements in $X$. Then the laminar strucuture of our collection $L$ naturally forms a tree.

Note that 

(@height) every leaf in this tree has the same height.
(@lca) the LCA of $x$ and $y$ represents a equivalent class of $\sim_{\leq d(x,y)}$.

We have to decide the edge weights. Let $e$ be an edge between nodes $u$ and $v$ which are some equivalent class of $\sim_{\leq a}$ and $\sim_{\leq b}$. Then the weight of this edge is $\frac{1}{2}(a-b)$ (assuming $a\geq b$).

Now as (@lca) shows, the distance of leaves in the tree is
\begin{equation*}
\begin{aligned}
    d_T(x,y)&=d_T(\lca(x,y),x) + d_T(\lca(x,y),y)\\
            &=\frac{1}{2}(d(x,y) -0)+\frac{1}{2}(d(x,y) -0)\\
            &=d(x,y)
\end{aligned}
\end{equation*}

Now i guess we have invented the so-called *Hierarchically Separated Trees* (HST). Any ultrametric can be converted into a HST in this way.

## exact $c$-HST

In the paper the target ultrametrics are limited to a special kind called exact $c$-HST. Let $c>1$ be a constant. For a tree node $u$ which represents an equivalence class of $\sim_{\leq x}$, we write $level(u)$ for $x$. The tree structure is the same as HST but the edge weight is $w((u,v))=\frac{1}{2}({level(u)}-{level(v)})$. Let $x,y$ be two leaves in a exact $c$-HST, then $d(x,y)={level(\lca(x,y))}$.

The following lemma shows that we only lose a constant factor in the distortion when restricting the target space to exact $c$-HSTs.

::: {.Lemma #exactHST}
Given a metric space and its embedding into a distribution over ultrametrics with distortion $\alpha$, there is an embedding into a distribution over exact $c$-HSTs with distortion $\alpha\cdot c$.
:::

**An intuitive but failed proof method**
We try to show that any ultrametric can be embedded into an exact $c$-HST with distortion $c$. Proving the distortion can be reduced to the following combinatorial problem.
Given a sequence of $n$ strictly increasing numbers $\{a_i\}_{i\in [n]}$, find another sequence $c^{x_1},\dots, c^{x_n}$ where $x_i\leq x_j$ for all $i<j$, such that $\max_i \frac{a_i}{c^{x_i}}\leq c \min_i \frac{a_i}{c^{x_i}}$.
Now one can see that this is not always true for any fixed $c$... so embedding into a distribution may be important.

# LP rounding

The authors then focus on embedding a given metric space $(N,d)$ into distribution ober exact 2-HSTs. By [@exactHST] they lose a 2-factor in the optimal distortion.
It's safe to assume that $\min_{x,y\in N}d(x,y)=1$ and $\max_{x,y\in N}d(x,y)=\Delta$. (Thus our embedding is always expanding.)

Assume that we embed $(N,d)$ into a single 2-HST. ($N$ is the set of leaves in the tree.)
Consider nodes with the same level in the 2-HST. Recall that each nodes represents a subset of $N$ and these sets form the equivalence class of $\sim_{\leq 2^{p}}$ where $p$ is the level. The height of the tree is $O(\log \Delta)$.
Now consider the distortion of this embedding. Let $q$ be the distortion and let $\chi_{x,y}\in\set{0,1}$ be an incidator variable such that $\chi_{x,y}=0$ iff $x\sim y$. (we omit the subscripts if there is no confusion.)

\[
    \sum_{r=0}^{\log \Delta} r \cdot \chi_{x,y} \leq q\cdot d(x,y) \quad \forall x,y\in X
\]

<!-- For a fixed $r$, the equivalence relation $\sim$ defines a graph $(N,E)$ where $(u,v)\in E$ iff $u\sim v$ (equivalently, $d(u,v)\leq c^r$). -->


If we embed into a distribution of 2-HSTs, then the indicator variable $\chi$ will become the probability that $x,y$ are not in the same eqivalence class.

This kind of LP-formulating technique goes back to the uniform labeling LP of Jon Kleinberg and Eva Tardos (see their [JACM'02 paper](https://www.cs.cornell.edu/home/kleinber/focs99-mrf.pdf)) which in turn is analogous to the multiway cut LP of Călinescu (see this [IPCO'17 paper](https://arxiv.org/pdf/1611.05530)).
In all these problems we want to 'label' vertices in a graph and only edges with different labels contribute to the solution.

A good property of embedding into 2-HSTs is we don't require the solution to integral. However, the main difficulty is that we need to solve $\log \Delta$ labeling problems and they are dependent. For example, if $x,y$ are not separated at level $r$, they shouldn't be separated at any higher level; In a distribution of 2-HSTs, this means that the probability of $x\sim y$ is non-decreasing in the level.

With all these observation in mind, the LP seems intuitive.

# TODO

- understand their LP.
    - connections to CKR relaxation
    - connections to LP hierarchies
    - why the optimal solution is not always a distribution of HSTs
- how to move from exact 2-HST to general HSTs?