---
title: Matroid girth
tags:  matroid, optimization, combinatorics
author: Yu Cong
lang: en
draft: true
showtoc: true
date: 2025-08-12
---

Let $M=(E,\mathcal I)$ be a matroid with non-negative weights $w:E\to \R_{\geq 0}$. 
The girth of $M$ is 
\[
    \min \set{\sum_{e\in C} w(e): \text{$C$ is a circuit of $M$}}.
\]

Cogirth of $M$ is the girth of the dual matroid of $M$.

Computing girth is NP-hard for binary matroids but can be done in polynomial time for graphs.
[Wikipedia](https://en.wikipedia.org/wiki/Matroid_girth#Computational_complexity) lists some negative complexity results, which mainly concern more general matroid classes than binary matroids. 
So here are some positive results filling the gap between graphic matroids and binary matroids.

# Regular matroid

::: {.Theorem title="Seymour decomposition [@seymour_decomposition_1980]"}
every regular matroid may be constructed by combining graphic matroids, co-graphic matroids, and a certain ten-element matroid that is neither graphic nor co-graphic, using 3 binary operations:

- 1-sum is direct sum of two matroids
- 2-sum is patching two matroid on 1 common element
- 3-sum is patching two matroids on 3 common elements
forming a 3-circuit in each matroid.
:::



# Perturbed graphic matroids