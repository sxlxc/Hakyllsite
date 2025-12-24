---
title: k-cut notes
author: Yu Cong
lang: en
date: 2025-12-24
---

# Near linear time 2-approximation in $\poly(m,n,k)$ regime.

SOTA is Kent Quanrud's near linear time $(2+\epsilon)$-approximation. Under small set expansion hypothesis, the best ratio one can expect is 2. So there's a tiny bit of space for improvement.

# sub $\tilde O(n^k)$ exact algorithm

only possible on simple unweighted graphs

[A recent paper](https://arxiv.org/abs/2512.12900v1) combines the principal sequence of partitions and Kawarabayashi-Thorup contractions to get a sub-$n^k$ deterministic algorithm.