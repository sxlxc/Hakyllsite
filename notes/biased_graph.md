---
title: Biased graphs and related matroids
tags: matroid
author: Yu Cong
lang: en
draft: true
date: 2025-10-30
# showtoc: true
---

Irene Pivotto wrote a lot on matroid union on biased graphs and related matroids. These matroids seem closely related to graphic matroids.

Matroid union pages can also be found in the internet archive.

# Biased graphs and their matroids I

<https://matroidunion.org/?p=161> 

Introduce biased graphs with signed graphs and group labelled graphs as examples. Frame matroids generalize graphic and bicircular matroids (they are (1,1) and (1,0)-sparsity matroids). Lift matroids generalize graphic matroids and even cycle matroids. So frame matroids and lift matroids seems interesting for basepacking project, since they generalize many constant-gap matroid classes.

# Graphical representations of matroids

<https://matroidunion.org/?p=266>

This is a post by Jim Geelen. Zaslavsky's characterization of frame matroids and lift matroids are interesting. He also talked about combining the two classes using frameworks. A connected graph $G$ is a framework for a matroid $M$ if $E(G)=E(M)$, $r(M)\leq |V|$ and each elementry cut $\delta(u)$ is a cocircuit of $M$.

He conjectured that:

- Given a biased/lift matroid $M$ and a graph $G$, decide if there is a biased graph $(G,\mathcal C)$ such that $M$ is the biased/lift matroid of $(G,\mathcal C)$ is NP-hard. (Proven. see <https://matroidunion.org/?p=2218>)
- There is a polynomial-time algorithm that, given a matroid $M$ via its rank oracle, determines whether $M$ is a framework matroid.

# Biased graphs and their matroids II
 
<https://matroidunion.org/?p=279>

Frame and lift matroids are minor closed. However, it is mentioned in [this post](https://matroidunion.org/?p=2218) that the number of excluded minors are infinite for both frame and lift matroids.


# Biased graphs and their matroids III

<https://matroidunion.org/?p=357>

del Greco proved an analogue to Seymour’s theorem for frame matroids. However, the theorem does not lead to a polynomial-time algorithm for deciding frame matroids, which somewhat supports Jim Geelen's conjectures. It is even NP-hard to determine if a matroid is bicircular...

> Aside from graphic matroids, I don’t know of any other polynomial-time recognition algorithm for classes of frame matroids. The situation is just as bad for lift matroids. This lack of algorithms suggests some obvious open problems:
>
>
> **Problem 1:** find a polynomial-time algorithm that, given a binary matroid $M$, determines whether $M$ is an even cycle matroid.
>
>
>**Problem 2:** find a polynomial-time algorithm that, given a ternary matroid 𝑀, determines whether 𝑀 is a signed-graphic matroid.

10 years later, Bertrand Guenin and Cheolwon Heo showed that even-cycle and even-cut matroids can be recognized in polynomial time, see [this paper](https://link.springer.com/article/10.1007/s10107-023-01944-6).

# Other links
- <https://matroidunion.org/?p=2218> Recognition of frame, and of lift matroids, is intractable.
- Heo, Cheolwon's thesis [Representations of even-cycle and even-cut matroids](https://uwspace.uwaterloo.ca/items/b86f4198-90ba-4d72-bb51-ca51fb162962)
- Pivotto, Irene's thesis [Even Cycle and Even Cut Matroids](https://uwspace.uwaterloo.ca/items/29ba81f1-13ca-47e5-b504-e4d684d9d678)

