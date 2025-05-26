---
title: Metric notes
tags: CG, optimization
author: Yu Cong
lang: en
draft: true
date: 2025-05-26
---

# Finite metric embeds into $\ell_1$

Bourgain's theorem states that there exists such an embedding with distortion $O(\log n)$. What if we only want to use tree metrics? It seems that the distortion becomes $O(\log n \log \log n)$. <https://chekuri.cs.illinois.edu/papers/packing.pdf>

# Shortest path representation

For any finite metric on $V$ there is a corresponding graph shortest path metric on $G=(V,E)$ with $c:E\to \R$. Given a finite metric on $V$, how to find $G$ with $|E|$ as small as possible?