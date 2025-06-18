---
title: Max number of points in "general" position
tags: combinatorics, CG
author: Yu Cong
lang: en
draft: true
showtoc: true
date: 2025-06-18
---

# General Position

An arrangement of 2D points is said to be in general position if any 3 points are not colinear. Given a 2D area $D$, the max number of points one can place in $D$ in general position is infinity. However, what if one relaxes the colinear constraints to "seemingly colinear"? 

More precisely, 3 points $(x_i,y_i)$ are said to be "seemingly colinear" if there is a linear function $f(x)$ fitting these points with error $\sum_{i\in [3]}(y_i-f(x_i))^2 \leq c$ for some global constant $c$. An arrangement of points is "seemingly in general position" if every 3 points are not "seemingly colinear".

::: Problem
Given a 2D area $D$, find the max number of points one can place in $D$ such that the points are "seemingly in general position".
:::