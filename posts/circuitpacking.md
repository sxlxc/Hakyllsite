---
title: Matroid circuit packing and covering
tags:  matroid, optimization, combinatorics
author: Yu Cong, Kangyi Tian
lang: en
draft: true
showtoc: true
date: 2025-06-15
---

In the [previous post](/posts/basepacking1.html), we mainly focus on the algorithmic part of integral and fractional base packing and base covering. In this post we consider packing and covering of matroid circuits.

# Packing/Covering Defect

Seymour [@seymour_packing_1980] proved the following theorem.

::: Theorem
Let $M=(E,I)$ be a matroid without coloop. Then one has
\[\theta(M)-\kappa(M)\leq r^*(M)-\nu(M),\]
where $\theta(M)$ is the minimum number of circuits whose union is $E$,
$\kappa(M)$ is the number of connected components in $M$,
$r^*$ is the corank and
$\nu(M)$ is the max number of disjoint circuits.
:::

The left hand side $\theta(M)-\kappa(M)$ is called the *covering defect* and the right hand side $r^*(M)-\nu(M)$ is called the *packing defect*.

# Complexity

Computing the corank $r^*$ and the component number $\kappa(M)$ is easy. What about $\theta(M)$ and $\nu(M)$?

The problem of determining if a sparse split graph (a special case of chordal graphs) can have its edges partitioned into edge-disjoint triangles is NP-complete [@feder_packing_2012]. So finding $\theta(M)$ and $\nu(M)$ is NP-hard even for some special graphic matroids.