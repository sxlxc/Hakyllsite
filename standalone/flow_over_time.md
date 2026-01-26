---
title: Flows over time
author: Yu Cong
lang: en
date: 2026-01-20
showtoc: true
---

This is a note on Martin Skutella's [slides](https://www3.math.tu-berlin.de/combi/MDS/summerschool11-material/flows_over_time_bereinigt.pdf) on flows over time, whose audiences are those who know flow but not flow over time.^[A more comprehensive and readable introduction on this topic is his [paper](https://www3.math.tu-berlin.de/Vorlesungen/SS08/NetOpt/flows_over_time.pdf).]
I find this problem a great example for modeling real-world applications and for doing reductions to solve (seemingly) new theory problem.

Consider a pipe system carrying materials from a source to a sink. A pipe system can be naturally modelled by a digraph.
The vertex includes the source $s$, the sink $t$ and some intermediate nodes. The arcs represent pipes.
Each arc $e$ has a capacity $c_e$, meaning the amount of material it can carry per second, and a flow velocity (or transit time), which represents the times it takes going through the pipe/arc.

Now a natural and pratical question is, given time $T$, what is the maximum quantity of materials the system can transport from $s$ to $t$ in time interval $[0,T)$?

# Max flow over time

::: {.Problem title="Max flow over time"}
Given a digraph $G=(V,E)$ with capacity $c:E\to \Z_+$ and transit time $\tau:E\to \Z_+$ on arcs, a source node $s\in V$ and a sink node $t\in V$, and a time horizon $T>0$,
find the maximum $s$-$t$-flow over time.
:::

We haven't define flow over time yet.
Given the time interval $[0,T)$, the flow over time is a set of functions $f_e:[0,T)\to \R_+$ with the following constraints,

1. $f_e(\theta)=0$ for $\theta\in [T-\tau_e,T)$. This is because we don't want any remaining flow in the network after time $T$. We just stop sending flow if it won't arrive before time $T$.
2. For any intermediate node $u$, we have 
    \[
    \underbrace{\sum_{e\in \delta^{in}(u)} \int_0^{\theta - \tau_e} f_e(t) d t}_{\text{flow-in}} =
    \underbrace{\sum_{e\in \delta^{out}(u)} \int_{0}^{\theta} f_e(t) d t}_{\text{flow-out}}
    \]
    for $\theta \in [0,T)$.

At the first glance of this problem, one may come up with the idea of spliting vertices for discrete time stamps and then connecting arcs between vertices with different time stamps to represents transit time.
This is called the *time expanded network*. Time expanded networks reduce flow over time to static max flow, but the running time is pseudopolynomial in the input size, since the number of time stamps is $T$. 
Though time expanded network does not lead to a polynomial time algorithm, the reduction to static flow shows that there is an integral optimal solution which is also acyclic in the time expanded network.

Is there a polynomial-time algorithm for max-flow over time?

Here's some intuitive thought.
Assume that the time horizon $T$ is infinite. Then the maximum flow over time is simply the static maximum flow and the transit time is useless in this case.
Now suppose that the time horizon $T$ is large enough (say... much larger than the longest simple path from $s$ to $t$), then the optimal solution should be routing the static max flow^[also it's not clear how to break ties.] until the flow cannot arrive before time $T$, then swapping to a flow with shorter transit time.

One way to formalize this intuitive idea is to look at the set of $s$-$t$-paths.
Let $P$ be an $s$-$t$-path. $P$ is feasible at time $\theta$ if $T-\theta \geq \tau(P)= \sum_{e\in P} \tau_e$.

**Robot formulation.** Given any flow over time, one can think of it as continuously sending from the source $s$ some nano robots, which are programmed to go along some feasible path.^[It means that the path is feasible at the time of sending the robot. We can assume that there are no circulations.]
The number of robots sending at time $\theta$ is the flow rate.
We say an $s$-$t$-path $P$ is in the support of a flow over time if at some time $\theta$ we send robots to go along $P$.
However, it is still tricky to satisfy the flow conservation.
Fortunately, we have the following lemma.

::: {.Lemma #pathdecomp}
There exists an optimal flow over time such that for every $s$-$t$-path $P$ in its support, flows are sent along $P$ for time $T-\tau(P)$.
:::

... The plan is to consider the connections between time expanded network and robot formulation.

It follows from definitions that the number of robots we sent at time $\theta$ is amount of outgoing flows from $s_\theta$ in the time expanded network.
Then hopefully one can come up with a proof the the above lemma while avoiding cut over time.
However, i find it hard to do so. Anyway, the statement is a corollary of the max-flow min-cut over time theorem.

If [@pathdecomp] can be proven without $s$-$t$-cut over time, then one can derive the same algorithm as Ford & Fulkerson[^FF].

# Earliest arrival flow

::: {.Definition title="Earliest arrival flow"}
A feasible $s$-$t$-flow over time with time $f$ horizon $T$ is called earliest arrival flow if it maximizes the cumulative flow $\sum\limits_{e\in \delta^{\text{in}}(t)} \int_0^{\theta-\tau_e} f_e(\psi) d\psi$ into $t$ for all $\theta\in [0,T]$.
:::

Apparently, the earliest arrival flow must be some max $s$-$t$-flow over time. However, it not obvious if such a flow exists in any networks.



[^FF]:
    This is roughly how Ford & Fulkerson's algorithm works:

    - restrict the solution to temporally repeated flow, which is sending flows along each path in the decomposition of some static acyclic $s$-$t$-flow until the arrival time exceeds $T$. This always leads to feasible flow over time.
    - use flow decomposition to show that the maximum temporaly repeated flow comes from any static flow in $\arg\max_{f} T|f|-\sum_{e\in E}\tau_e f_e$, which is a min-cost circulation problem and can be solved in strongly polynomial time. 
    (It seems that, at the time (1958) when Ford and Fulkerson invented this algorithm, a strongly polynomial-time algorithm for circulation had not yet been found. (See Schrijver's book and refs therein.) They formulated this problem with LP and used primal-dual to solve it.)
    - find a max-flow min-cut over time theorem, and show that the dual LP (which is integral) for min cut over time has the same optimum as maximum tempraly repeated flow. Also min-cut over time is always no smaller than flow over time. It follows that the maximum flow over time is indeed achieved by temporally repeated flows.