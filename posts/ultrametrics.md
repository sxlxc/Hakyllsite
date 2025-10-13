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

In the paper the input ultrametric is limited to a special kind called exact $c$-HST. Let $c>1$ be a constant. The tree structure is the same as HST but the edge weight is $w((u,v))=\frac{1}{2}(c^{depth(u)}-c^{depth(v)})$. Let $x,y$ be two leaves in a exact $c$-HST, then $d(x,y)=c^{depth(\lca(x,y))}$.

The key to restricting input to an exact $c$-HST is the following lemma.

::: Lemma
Given a metric space and its embedding into a distribution over ultrametrics with distortion $\alpha$, there is an embedding into a distribution over exact $c$-HSTs with distortion $\alpha\cdot c$.
:::

**An intuitive but failed proof method**
We try to show that any ultrametric can be embedded into an exact $c$-HST with distortion $c$. Proving the distortion can be reduced to the following combinatorial problem.
Given a sequence of $n$ strictly increasing numbers $\{a_i\}_{i\in [n]}$, find another sequence $c^{x_1},\dots, c^{x_n}$ where $x_i\leq x_j$ for all $i<j$, such that $\max_i \frac{a_i}{c^{x_i}}\leq c \min_i \frac{a_i}{c^{x_i}}$.
Now one can see that this is not always true for any fixed $c$... so embedding into a distribution may be important.