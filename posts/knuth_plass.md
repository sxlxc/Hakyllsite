---
title: Linear time linebreaker?
tags: alg, typography
author: Yu Cong
lang: en
# draft: true
date: 2025-10-28
# showtoc: true
---

Recently, Typst has merged a [PR](https://github.com/typst/typst/pull/6161) which includes support for [character-level justification](https://typst.app/blog/2025/typst-0.14#character-level-justification).
This feature is an extension to the linebreak algorithm. It allows changes in the inter-character space in each word and this adjustment may affect line break decisions. For line breaking Typst uses [Knuth–Plass line-breaking algorithm](https://en.wikipedia.org/wiki/Knuth%E2%80%93Plass_line-breaking_algorithm)[^1]. 

[^1]: See this nice [blog post](https://laurmaedje.github.io/posts/layout-models/) for a comparison between the layout models of TeX and Typst.

Let's forget about typography and focus on the algorithmic part of line-breaking problems.

# line-breaking

::: {.Problem title="line breaking"}
Given a sequence $S$ and a cost function $c$ mapping from contiguous subsequences (substrings) of $S$ to $\R$, divide the sequence $S$ (the paragraph) into substrings $S_1,\dots,S_k$ (lines) such that $\sum_{i\in [k]} c(S_i)$ is minimized.
:::

We over-simplify things here. In real typography world the cost function not only depends on the line $S_i$ but also other things like the length of the line.

The input size $n$ is the number of elements in $S$ and for now the cost function $c$ is assumed to be given in an oracle.

This line breaking admits a $O(n^2)$ dynamic programming algorithm:

\[
    f(i)=\max_{0\leq j<i} f(j)+c(S[j..i])
\]

# SMAWK algorithm

[Wikipedia](https://en.wikipedia.org/wiki/Knuth-Plass_line-breaking_algorithm#Computational_complexity) says that this problem can be solve in linear time using SMAWK algorithm, which finds the minimum in each row of a $n\times m$ totally monotone matrix in $O(n+m)$ [^2].

[^2]: This is a footnote. See section 6.5 in [Jeff Erickson's lecture notes](https://courses.grainger.illinois.edu/cs473/sp2016/notes/06-sparsedynprog.pdf)).

The matrix here would be $M[i,j]=f(j)+c(S[j..i])$. The dynamic programming algorithm is in fact finding the minimum for each row. If we can show this matrix $M$ is totally monotone, then we can make optimal line breaking decisions in linear time. A matrix is totally monotone, if each row's minimum value occurs in a column which is equal to or greater than the column of the previous row's minimum. A special case of totally monotone is [Monge array](https://en.wikipedia.org/wiki/Monge_array) which requires $M[i,k]+M[j,\ell]\geq M[j,k]+M[i,\ell]$ for all $i<j$ and $k<\ell$. Now we focus on checking Monge property of $M$.

Doing some easy math, one can see that the Monge property depends on the following inequality on the cost function $c$:
\[
c(S[i..k])+c(S[j..\ell])\leq c(S[i..\ell])+c(S[j..k]))\quad \forall i<j<k<\ell
\]

This looks like some intersecting supermodular property on set functions, but our domain here is the collection of substrings.

As cited on wiki, [this paper](https://doi.org/10.1016/0196-6774(88)90032-6) stated that the problem of optimally breaking up text of a paragraph into lines can be done in linear time. The cost function is $c(X)=(len(X)-parwidth)^2$, where $len(X)$ is the sum of character width in $X$ and $parwidth$ is the width of the paragraph. This cost function does generate a Monge array, but this is not the cost in the [original paper](https://gwern.net/doc/design/typography/tex/1981-knuth.pdf) of Knuth and Plass.

They introduced several cost functions: "first-fit", "best-fit" and "demerits", with increasing complexity. For simplicity of analysis, we consider the "best-fit" case, where the cost function is the sum of badness and penalty. 

Penalty relates to the ending character of each line. For example, we want as few number of hyphens as possible, then the penalty for ending hyphen should be large. Badness meansures how much do we need to stretch/shrink the white spaces in a line to make it fit. More formally, badness is $c\left(\frac{L}{W}\right)^3$ where $L$ is the sum of default length of all characters in this line, $W$ is the total length of stretchable/shrinkable whitespaces and $c$ is some universal constant factor.

Note that penalties do not affect Monge property, since the number of occurence of ending characters ($S[k]$ and $S[\ell]$) in the same on LHS and RHS. However, for badness, one can verify that $M$ is not always a Monge array. I think total monotonicity is violated too but don't have a counterexample now...

# character-level operations

Assume the cost function is still a parabola on line width and we further add some character-level operations. 

1. We can break all ligatures in one line. The cost function becomes $c(X)=\min\{c_1(X),c_2(X)\}$.
2. Stretch/shrink font glyphs and adjust kerning. $c(X)=\min_{\theta}(\theta {\rm len}_1(X)+{\rm len}_2(X)-parlength)^2$, where ${\rm len}_{1,2}$ are the total length of whitespaces and other characters in $X$ and $\theta\in [lb,ub]$ is the stretching factor.

Unfortuantely, neither of the operations preserves Monge property and i believe they break total monotonicity as well.
