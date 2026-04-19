---
title: "你的算法能被有健忘症的计算模型正确执行吗🤔"
tags: optimization，alg
author: 丛宇
lang: zh
draft: true
date: 2026-04-18
showtoc: true
---

讨论complexity的时候总要有个计算模型。比如有人研究有封闭类时曲线（CTC）的计算模型[^ctc]。
这里我们要考虑个更弱的模型---你的计算模型有概率忘记自己接下来要执行哪一步，就像在程序运行过程中program counter register有概率被清零。（不过我们也不讨论architecture有关的东西）

# Related works

<https://chatgpt.com/share/69e2fed0-524c-8398-83f1-ca64e4172e6d>

# 模型

我们的计算模型叫作AMNESIA。

- 失忆：根据[wikipedia](https://zh.wikipedia.org/zh-cn/%E5%81%A5%E5%BF%98%E7%97%87)，我们假设AMNESIA会发生[解离性失忆症](https://zh.wikipedia.org/zh-cn/%E8%A7%A3%E7%A6%BB%E6%80%A7%E5%A4%B1%E5%BF%86%E7%97%87)。这意味着如果某条指令执行结束之后失忆，AMNESIA会丢失所有历史结果，但是仍然知道失忆前运行到哪条指令。

    > <https://zh.wikipedia.org/zh-cn/解离性失忆症> <br>
    > 常见之症状有以下几类：
    > - 无法认出自己的朋友或家人。
    > - **无法记住自己曾经做过的事。**
    > - 对原来熟悉的地方感到陌生。
    > - ...
- 概率：简单起见假设执行完每条指令后失忆概率都是$\e\in (0,1)$。 因此完成一个complexity是$T(n)$的算法的概率是$(1-\e)^{T(n)}$.
- 为了保持可玩性，需要提供一个叫作`[大记忆恢复术]`的oracle，调用之后可恢复全部记忆。
- AMNESIA并不知道算法的complexity。但我们只关心多项式时间算法。

看起来AMNESIA的定义十分合理，因为与实际中的健忘症类似。

# 问题

## 固定的 `[大记忆恢复术]` 代价

如果健忘症发作，可以去医院里治疗来恢复记忆^[这好像并不合理，医院貌似无法恢复记忆...去找Rick Sanchez然后恢复记忆可以看成要花费固定的时间]，也可以选择重新开始做忘掉的工作。去医院治疗需要花费（基本上）固定的时间，而重新工作花费的时间则和进度有关。
类似的，AMNESIA在失忆之后也应该可以选择去医院恢复记忆还是重新做工作。这可以通过在算法过程中每条指令之前都插入根据进度判断^[只能用对失忆的AMNESIA可见的参数来判断]是否调用`[大记忆恢复术]` oracle来实现^[假设这个判断+调用oracle过程期间不会失忆]。如果认为`[大记忆恢复术]`花费constant time，asymptotically 算法的时间复杂度还是一样的。

假设我们提前知道在某个input instance上我们设计的算法准确的步数^[in terms of AMNESIA执行完可能失忆的atomic steps]，那么我们可以提前计算调用oracle的最优策略来让算法结束的期望步数最小。
但是实际上作为算法设计者我们并不知道input instance是什么，所以无法准确获得执行步数。


[^ctc]: 可以在未来将一些bit发送到过去。讨论的CTC模型是Deutschian model，大概讲世界在每个时间点都是概率的，考虑祖父悖论，祖父在时间$t_0$被炸弹杀死会让世界确定的运行，而实际上祖父是否被炸弹杀死是随机事件。祖父在时间$t_0$以概率1被杀说明在时间旅行时刻$t_1$观测，祖父实际上当时没死；这构成markov chain的状态转移矩阵，所以世界会自动设定祖父在$t_0$时刻被炸死的概率是markov chain的平稳状态。可见[这篇文章](https://drops.dagstuhl.de/storage/00lipics/lipics-vol029-fsttcs2014/LIPIcs.FSTTCS.2014.469/LIPIcs.FSTTCS.2014.469.pdf)。不过我们不会讨论complexity，原因是我不怎么懂。
