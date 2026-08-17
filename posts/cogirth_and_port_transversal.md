---
title: "Matroid cogirth and quotient path"
tags:  matroid, optimization
author: Yu Cong, translated by GLM 5.3
lang: zh
# draft: true
showtoc: true
date: 2026-08-15
---

一个拟阵的cogirth是它最小（带非负权重或者元素数量最少）的cocircuit的大小。找一个拟阵的cogirth因此是图上最小割问题的推广。
假设拟阵用一个rank oracle给出，即使在binary matroid上计算cogirth都是NP-hard。
所以如果能找到些计算cogirth简单的matroid class会有意思。

关于这个问题，几个月前我有个失败的想法，现在把他写下来。

# 想法

拟阵和图有很多联系。除了graphic matroid，图上的$T$-join和$T$-cut也可以被表示成拟阵上的东西。考虑图的incidence matrix，$T$-join 要选择一个偶数大小的顶点集合$T$， 所以给incidence matrix加上一列$t$,表示某行对应的顶点在不在$T$里面。接下来要考虑这新矩阵上的binary matroid $M$。
众所周知，linear matroid的cocycle就是它矩阵表示的row space里向量的support，那么图的minimum $T$-cut恰好就是新加入一列为1的、在row space里的行向量的最小support。
而$T$-join是恰好让子图里$T$中的顶点度数是奇数的边集合，考虑在incidence matrix把$T$-join对应的列向量选出加起来，再加上$t$就会是零向量了。因此$T$-join刚好就是拟阵$M$中包含$t$的那些circuit去掉$t$的部分。这个东西叫作matroid port。

接下来我们即将观察到一个重要结论！仍然先考虑图，取任何一个cut $\delta(S)$，$|S\cap T|$不是奇数就是偶数😂！因此图的min-cut大小是这两者中较小的：$|S\cap T|$是偶数情况下最小的cut $\delta(S)$（也就是$M/t$的cocircuit），$|S\cap T|$是奇数情况下的最小cut（也就是$T$-cut，同时也是$M$的最小$t$-port transversal）实际上这对任何拟阵都成立。

对拟阵 $N$，记
\[
  \lambda(N)=\min\{|D|:D\in \mathcal{C}^*(N)\}
\]
为它的cogirth，若这个最小化不可行则取值为 $+\infty$。如果 $M$ 是带有一个指定元素 $f$ 的拟阵，它的 $f$-port 是如下clutter
\[
  \mathcal{P}_f(M)=
  \{C\setminus\{f\}:C\in \mathcal{C}(M),\ f\in C\}.
\]
对一个clutter $\mathcal{P}$，令 $\tau(\mathcal{P})$ 为transversal（即与 $\mathcal{P}$ 中每个成员都相交的集合）的最小大小。

::: {.Lemma #lem:matroid-cut-join}
$M$ 是groundset为 $E\cup\{f\}$ 的拟阵，其中 $f$ 既不是loop也不是coloop。则
\[
  \lambda(M\setminus f) =
  \min\{\lambda(M/f),\tau(\mathcal{P}_f(M))\}.
\]
:::

::: {.Proof}
固定 $X\subseteq E$。$X$ 是 $\mathcal{P}_f(M)$ 的transversal，当且仅当不存在包含 $f$ 的circuit整个落在 $(E\setminus X)\cup\{f\}$ 里。也就是说，$X$ 与 $\mathcal{P}_f(M)$ 的每个成员都相交，当且仅当 $f\notin \operatorname{cl}_M(E\setminus X)$。

由标准的closure–cocircuit对偶，$f\notin \operatorname{cl}_M(S)$ 当且仅当存在 $M$ 的一个cocircuit $D$ 使得 $f\in D$ 且 $D\cap S=\emptyset$。取 $S=E\setminus X$，这就等价于存在cocircuit $D$ 满足 $f\in D$ 且 $D\setminus\{f\}\subseteq X$。于是
\[
  \tau(\mathcal{P}_f(M)) =
  \min\{|D|-1:D\in \mathcal{C}^*(M),\ f\in D\}.
\]

另一方面，$M/f$ 的cocircuit恰好是 $M$ 中不含 $f$ 的那些cocircuit，所以
\[
  \lambda(M/f)
  =
  \min\{|D|:D\in \mathcal{C}^*(M),\ f\notin D\}.
\]

最后，$M\setminus f$ 的cocircuit是形如 $D\setminus\{f\}$（$D\in \mathcal{C}^*(M)$）的集合中的非空极小者。因此 $M\setminus f$ 的最小cocircuit要么来自 $M$ 中不含 $f$ 的cocircuit，给出 $\lambda(M/f)$；要么来自含 $f$ 的cocircuit，给出 $\tau(\mathcal{P}_f(M))$。
:::

这就是图上 $T$-cut/$T$-join关系的拟阵形式。在binary graphic的特例中，用偶大小的terminal set $T$ 的incidence vector给图 $G$ 的cycle matroid添加一个元素 $f$。port $\mathcal{P}_f(M)$ 就是由极小 $T$-join组成的clutter，而穿过 $f$ 的cocircuit删去 $f$ 之后就是 $T$-cut。于是上面的恒等式说的正是：最小割是“最小偶割”和“最小 $T$-cut”中较小的那个，而后者等于 $T$-join的最小transversal大小。

# Matroid port transversal

由 [@lem:matroid-cut-join] 的证明，计算 $\tau(\mathcal{P}_f(M))$ 等价于找 $M$ 中包含 $f$ 的最小cocircuit：
\[
  \tau(\mathcal{P}_f(M)) =
  \min\{|D|-1:D\in \mathcal{C}^*(M),\ f\in D\}.
\]

一些tractable的情形：

- graphic matroid：最小 $s$-$t$ 割
- cographic matroid：经过某条固定边的最短圈
- graphic matroid（binary表示）加上任意的binary向量：图上的 $T$-cut / odd cut
- regular matroid：<https://www.cs.unb.ca/tech-reports/documents/TR01_144.pdf> 中的一个步骤；判定是否存在包含 $T$、大小 $\leq \ell$ 的cocircuit的FPT算法：<https://arxiv.org/pdf/1607.05516>
- low rank perturbation of graphic matroids
- 在固定的有限域上branchwidth有界的拟阵：动态规划 <https://www.sciencedirect.com/science/article/pii/S0095895605001073>

所以看起来（几乎）所有存在多项式时间cogirth算法的matroid class，也都存在多项式时间的port transversal算法，不过对一般的拟阵，似乎没有办法把port transversal归约到cogirth。

# 同一ground set上所有拟阵的有向图

[GGW](https://www.math.uwaterloo.ca/~jfgeelen/Publications/link.pdf) 在拟阵上定义了一个度量。
给定ground set同为 $E$ 的两个拟阵 $M_1,M_2$。如果存在ground set为 $E\cup \set{e}$ 的拟阵 $N$，使得 $M_1=N/ e$ 且 $M_2=N\setminus e$，就说 $M_2$ 可以由 $M_1$ 经过一次初等变换得到。
他们把 $M_1$ 和 $M_2$ 之间的距离定义为把一个变成另一个所需初等变换的最少次数。

[@lem:matroid-cut-join] 给出了初等变换在cogirth问题上的算法版本。如果 $N$ 上的 $e$-port transversal可以被高效计算，那么 $N\setminus e$ 的cogirth就可以归约到 $N/e$ 上。

现在考虑一个有向图，顶点集是固定ground set $E$ 上的所有拟阵。如果存在从 $Q$ 到 $L$ 的初等变换，就从 $Q$ 向 $L$ 引一条弧。每条弧 $a$ 都关联一个ground set为 $E\cup \set{e}$ 的中间拟阵 $M_a$。如果 $M_a$ 上的 $e$-port transversal可以被高效求解，就称弧 $a$ 是certified的。
一条简单路径如果每条弧都是certified的，就称它是certified的。
于是 [@lem:matroid-cut-join] 说明：若给定了从某个我们能计算cogirth的拟阵 $Q$ 到 $L$ 的多项式长度的certified path，就可以在多项式时间内计算拟阵 $L$ 的cogirth。

注意这个有向图是有限的，因为ground set是固定的。它还是弱连通的，并且是一个DAG：当port元素 $e$ 既不是loop也不是coloop时，沿一条弧走会让rank增加1；否则这条弧是自环。

容易看出，如果存在从 $Q$ 到 $L$ 的路径，那么 $Q$ 必是 $L$ 的quotient。

# quotient对之间的路径

Oxley定义了一类有趣的集合族，叫作modular cut。
modular cut在certified path和matroid port transversal之间建立了联系。
参考文献见GGW第6节。

设 $M$ 是ground set为 $E$ 的拟阵，$A,B\subset E$。
若rank function在它们身上是modular的，就称 $(A,B)$ 是一个modular pair：
\[ r(A)+r(B)=r(A\cup B)+r(A\cap B)\]

$E$ 的子集族 $\mathcal F$ 是一个modular cut，如果：

- 向上封闭。若 $X\subset Y\subset E$ 且 $X\in \mathcal F$，则 $Y\in \mathcal F$
- 若 $(X,Y)$ 是modular pair，则 $X\cap Y\in \mathcal F$
- 若 $Y\in \mathcal F$ 且 $X\subset Y$ 满足 $r(X)=r(Y)$，则 $X\in \mathcal F$。

注意我们这里modular cut的定义遵循GGW，与Oxley的定义不同。

::: {.Theorem #thm-modular-cut title="Oxley"}
设 $\mathcal F$ 是拟阵 $M$ 的一个modular cut，则存在ground set为 $E(M)\cup \set{e}$ 的拟阵 $N$，使得 $N\setminus e=M$，且对每个 $X\subset E(M)$，$r_N(X\cup \set{e})=r_M(X)$ 当且仅当 $X\in \mathcal F$。
:::

[@thm-modular-cut] 给出了modular cut和初等变换之间的联系。
给定 $L$ 的一个modular cut $\mathcal F$，可以构造中间拟阵 $N$ 和“弧尾”拟阵 $Q=N/e$，使得

- $Q \to L$ 是一次初等变换
- $r_Q(X)=r_L(X)-\bm{1}_{X\in \mathcal F}$
- $N$ 的 $e$-port恰好是 $\mathcal F$ 中inclusionwise极小的集合。

::: {.Corollary #cor-modular-trasversal}
$X$ 是 $N$ 的 $e$-port transversal当且仅当 $E-X\notin \mathcal F$。
:::

## Higgs lifts

设 $Q,L$ 是ground set同为 $E$ 的两个拟阵。如果 $Q$ 的每个flat都是 $L$ 的flat，就称 $Q$ 是 $L$ 的quotient。

令 $k=r(L)-r(Q)$，并对 $i\in [k]$ 定义
\[r_i(X)=\min \set{r_L(X),r_Q(X)+i}\]
每个 $r_i$ 都是 $E$ 上某个拟阵的rank function。

这个操作叫作 *Higgs lifts*。背景见 <https://www.sciencedirect.com/science/article/pii/S0196885819300752> 及其参考文献。
令 $M_i$ 为rank function是 $r_i$ 的拟阵，则 $L=M_k$，$Q=M_0$。
$M_i$ 的基是大小为 $r(Q)+i$、张成 $Q$ 且在 $L$ 中独立的集合。

::: {.Lemma #lem-higgspath}
设 $Q$ 是 $L$ 的quotient，$Q=M_0 \to \cdots \to M_k=L$ 是从 $Q$ 到 $L$ 的一列Higgs lifts，
则边 $M_{i-1}\to M_i$ 是由modular cut
\[\mathcal F_i=\set{X\subset E: r_L(X)-r_Q(X)\geq i}\]
诱导的初等变换。
:::

::: {.Proof title="sketch"}
前半部分是Oxley书中的Proposition 7.3.5，只是那里用的名字是 "elementary quotient" 而不是初等变换。

modular cut的部分本质上就是 <https://www.sciencedirect.com/science/article/pii/0012365X78901218> 的Theorem 3.4，不过他们对我们的modular cut用的名字是modular filter，而他们的modular cut指的是modular filter中的flat（这也是Oxley的定义）。
:::

::: {.Theorem #thm-higgs-transversal}
设 $Q$ 是 $L$ 的quotient。则
\[
    \lambda(L)=\min \set{\lambda(Q),h_k},
\]
其中 $h_k=\min \set{|T|:T\subseteq E,  [r_L-r_L(E-T)]-[r_Q-r_Q(E-T)]>0}$。
:::

::: Proof
由 [@lem:matroid-cut-join]，$L$ 的cogirth是 $Q$ 的cogirth与Higgs lifts上各中间拟阵的最小port transversal之中的最小值。

把 [@lem-higgspath] 和 [@cor-modular-trasversal] 结合起来可知，每个最小port transversal就是 $h_i=\min \set{|T|:T\subseteq E,  r_L(E-T)-r_Q(E-T)<i}$。

注意 $\set{h_i}_{i\in [k]}$ 是不增的，因此只需要计算 $h_k=\min \set{|T|:T\subseteq E,  [r_L-r_L(E-T)]-[r_Q-r_Q(E-T)]>0}$。
:::

然而，当 $Q$ 是 $L$ 的quotient时这并没有什么用。
事实上，当 $\lambda(L)\neq \lambda(Q)$ 时，$L$ 的最小cocircuit总是 $h_k$ 的可行解，计算 $h_k$ 和求 $\lambda(L)$ 一样难。

## Separation path

[@thm-modular-cut] 说明拟阵 $M$ 的每个modular cut都给出一个初等变换 $N/e\to M$。

GGW还给出了一种基于separation寻找modular cut的方法。

::: {.Lemma #lem-sep-modular}
设 $\lambda_M(X)=r_M(X)+r_M(E-X)-r_M(E)$ 是ground set为 $E$ 的拟阵 $M$ 的connectivity function，$(A,B)$ 是 $E$ 的一个二部划分。
则所有满足 $\lambda_{M/X}(A-X)=0$ 的集合 $X\subseteq E$ 构成的集合族是 $M$ 的一个modular cut。
:::

::: {.Lemma #lem-sep-path title="Lemma 5.4 in GGW"}
设 $L$ 是一个拟阵，$(A,B)$ 是 $E(L)$ 的一个划分。则存在一条初等变换的路径
\[
L= M_k \leftarrow \cdots \leftarrow M_0=Q,
\]
其中 $M_{i-1}$ 是由 $M_i$ 通过 [@lem-sep-modular] 和 [@thm-modular-cut] 得到的，并且 $\lambda_{M_i}(A)=i$。
:::

沿用与Higgs lifts相同的论证，我们有 $\lambda(L)=\min\set{\lambda(L\setminus A),\lambda(L\setminus B),\min_{i\in [k]} h_i}$，其中 $h_i$ 是满足 $\lambda_{M/(E-X)}(A-(E-X))>0$ 的 $X\subseteq E$ 的最小大小。
设 $X$ 是 $L$ 的一个cocircuit，可以看出 $X$ 是 $h_k$ 的可行解当且仅当 $X-A$ 和 $X-B$ 都非空。
于是 $\lambda(L)$ 的这个刻画简单地退化成一个显然的事实：$L$ 的最小cocircuit要么含于 $A$、要么含于 $B$、要么同时跨越 $A$ 和 $B$……

# 为什么这些路径对计算cogirth没帮助 「by chatgpt」

上面的两种路径构造对组织证明有帮助，但它们本身并没有创造出新的算法。原因在于：从quotient $Q$ 到 $L$ 的任何一条有向路径，都只是把一个属于该quotient对的不变量分解开而已。

设 $Q$ 是 $L$ 在公共ground set $E$ 上的quotient。定义 *$L$ 相对于 $Q$ 的relative cogirth* 为
\[
  \rho(Q,L)=
  \min\set{|T|:T\subseteq E,
  [r_L(E)-r_L(E-T)]-[r_Q(E)-r_Q(E-T)]>0}.
\]
于是 $T$ 是 $\rho(Q,L)$ 的可行解，当且仅当删除 $T$ 在 $L$ 中造成的秩损失严格大于在 $Q$ 中造成的秩损失。也就是说，$T$ 杀掉了某些在 $L$ 中出现而在 $Q$ 中不出现的秩增量。

::: {.Theorem #thm-relative-cogirth}
设 $Q$ 是 $L$ 的quotient。则
\[
  \lambda(L)=\min\set{\lambda(Q),\rho(Q,L)}.
\]
此外，若
\[
  Q=M_0\to M_1\to\cdots\to M_k=L
\]
是任意一条非平凡的初等变换路径，则
\[
  \rho(Q,L)=\min_{i\in[k]} \rho(M_{i-1},M_i).
\]
对单独一条弧 $M_{i-1}\to M_i$，数 $\rho(M_{i-1},M_i)$ 恰好是中间那个ground set多出一个元素的拟阵的最小 $e$-port transversal。
:::

::: Proof
一个集合 $T$ 会降低拟阵 $M$ 的秩，当且仅当 $T$ 包含 $M$ 的某个cocircuit。
因为 $Q$ 是 $L$ 的quotient，$Q$ 中的任何秩损失也是 $L$ 中的秩损失。
因此 $Q$ 的每个cocircuit都包含 $L$ 的某个cocircuit，于是 $\lambda(L)\leq \lambda(Q)$。另外，若 $T$ 是 $\rho(Q,L)$ 的可行解，则它降低了 $L$ 的秩，从而包含 $L$ 的某个cocircuit；因此 $\lambda(L)\leq |T|$。这就证明了
\[
  \lambda(L)\leq \min\set{\lambda(Q),\rho(Q,L)}.
\]

反过来，设 $D$ 是 $L$ 的最小cocircuit。若删除 $D$ 会降低 $Q$ 的秩，则 $D$ 包含 $Q$ 的某个cocircuit，于是 $\lambda(Q)\leq |D|=\lambda(L)$。若删除 $D$ 不会降低 $Q$ 的秩，则 $D$ 是 $\rho(Q,L)$ 的可行解，于是 $\rho(Q,L)\leq |D|=\lambda(L)$。因此
\[
  \min\set{\lambda(Q),\rho(Q,L)}\leq \lambda(L),
\]
这就证明了第一个结论。

对于路径的结论，令 $S=E-T$ 并令
\[
  \delta_i(S)=r_{M_i}(S)-r_{M_{i-1}}(S).
\]
由于每条非平凡的弧都让rank增加1，所以 $\delta_i(E)=1$ 且 $\delta_i(S)\in\set{0,1}$。同时
\[
  r_L(S)-r_Q(S)=\sum_{i=1}^k \delta_i(S).
\]
因此
\[
\begin{aligned}
&[r_L(E)-r_L(S)]-[r_Q(E)-r_Q(S)]>0 \\
&\qquad\Longleftrightarrow
\sum_{i=1}^k (1-\delta_i(S))>0 \\
&\qquad\Longleftrightarrow
\delta_i(S)=0 \text{ for some }i.
\end{aligned}
\]
对 $T=E-S$ 取最小值即得
\[
  \rho(Q,L)=\min_{i\in[k]}\rho(M_{i-1},M_i).
\]
对于单独一条弧，其modular cut $\mathcal F$ 满足
$r_{M_{i-1}}(S)=r_{M_i}(S)-\bm 1_{S\in\mathcal F}$，所以 $\delta_i(S)=0$ 当且仅当 $S\notin\mathcal F$。由 [@cor-modular-trasversal]，这恰好就是 $T=E-S$ 是 $e$-port transversal的条件。
:::

这个定理说明certified path并没有改变底下的优化问题：它只是把relative cogirth $\rho(Q,L)$ 写成沿路径各个port-transversal值的最小值。因此，只有当所选的每条弧的label都各自比原来的相对问题更容易时，路径才有用。

## Higgs lifts

对Higgs路径 $Q=M_0\to\cdots\to M_k=L$，第 $i$ 条弧由
\[
  \mathcal F_i=\set{X\subseteq E:r_L(X)-r_Q(X)\geq i}
\]
诱导，因此它的port-transversal值是
\[
  h_i=\min\set{|T|:T\subseteq E,
  r_L(E-T)-r_Q(E-T)<i}.
\]
序列 $h_i$ 是不增的，所以Higgs路径上最小的label是 $h_k$。但是
\[
\begin{aligned}
  h_k
  &=\min\set{|T|: r_L(E-T)-r_Q(E-T)<r_L(E)-r_Q(E)}\\
  &=\min\set{|T|:
  [r_L(E)-r_L(E-T)]-[r_Q(E)-r_Q(E-T)]>0}\\
  &=\rho(Q,L).
\end{aligned}
\]
于是对quotient对 $Q\leq L$ 来说，[@thm-higgs-transversal] 恰好就是 [@thm-relative-cogirth]。特别地，当 $\lambda(L)\neq \lambda(Q)$ 时，$L$ 的最小cocircuit是 $h_k$ 的可行解，所以计算最后一个Higgs label本身就已经是在计算 $\lambda(L)$ 中最难的那部分了。

## Separation path

现在取 [@lem-sep-path] 给出的分离路径。设 $(A,B)$ 是 $E(L)$ 的一个划分，令 $k=\lambda_L(A)$。端点 $Q=M_0$ 满足 $\lambda_Q(A)=0$，并且与 $L$ 有相同的两个side minor，即
\[
  Q/A=L/A,
  \qquad
  Q/B=L/B.
\]
也就是说，$Q$ 是把 $A$ 与 $B$ 之间的全部连通性杀掉之后从 $L$ 得到的拟阵。由于 $\lambda_Q(A)=0$，$Q$ 的每个cocircuit都落在某一侧。因此 [@thm-relative-cogirth] 中的 $\lambda(Q)$ 项只是答案中“含于 $A$ 或含于 $B$”的那部分。

相对的那一项则是跨越的部分。为了从代数上看清这一点，设 $T\subseteq E$，令 $S=E-T$。由于 $Q$ 关于 $(A,B)$ 是分离的，且它的side minor与 $L$ 的一致，故
\[
  r_Q(S)=r_{L/B}(S\cap A)+r_{L/A}(S\cap B).
\]
把两个收缩的秩展开，得
\[
\begin{aligned}
  r_Q(S)
  &= r_L(B\cup (S\cap A))-r_L(B)
   + r_L(A\cup (S\cap B))-r_L(A).
\end{aligned}
\]
再利用 $k=\lambda_L(A)=r_L(A)+r_L(B)-r_L(E)$，得到
\[
  r_L(S)-r_Q(S)=k-\lambda_{L/S}(A-S).
\]
于是
\[
\begin{aligned}
&[r_L(E)-r_L(S)]-[r_Q(E)-r_Q(S)] \\
&\qquad = k-[r_L(S)-r_Q(S)]\\
&\qquad = \lambda_{L/S}(A-S).
\end{aligned}
\]
把 $S=E-T$ 代入，就有
\[
  \rho(Q,L)=
  \min\set{|T|:T\subseteq E,
  \lambda_{L/(E-T)}(A\cap T)>0}.
\]
这恰好就是分离路径计算里最后那个 $h_k$。

若 $T$ 是 $L$ 的一个cocircuit，则 $L/(E-T)$ 是ground set为 $T$、rank为1的拟阵，且在这个收缩里 $T$ 中没有元素是loop。因此
\[
  \lambda_{L/(E-T)}(A\cap T)>0
\]
当且仅当 $A\cap T$ 和 $B\cap T$ 都非空。所以分离路径的公式说的只是：$L$ 的最小cocircuit必属于三种类型之一——含于 $A$、含于 $B$、或跨越分离 $(A,B)$。

因此分离路径并不是cogirth的一种新归约。它只是对“杀掉 $A$–$B$ 连通性所得到的特殊quotient $Q$”套用relative cogirth公式，其最后一个port-transversal项恰好就是最小跨越cocircuit那一项。
