---
title: Libmatroid
tags: matroid
lang: zh
author: Yu Cong
draft: true
date: 2025-02-22
---

想要写一个更快的 matroid 算法的库, 用来和 gurobi 一起跑点测试验证一些猜想.

TODO:

1. sage有完善的matroid库, 是用 cython 写的, 我用cpp重写一部分效率也未必会大幅提升, 要看看里面用了什么算法, 比如 matroid intersection 有些新算法可能并没实现
2. 好像有个软件叫 m2 里面也有类似的功能, 要去看看
3. 麻烦的 cryptomorphism, 要仔细想想如何写这个库
4. 用来测试猜想常用的东西是枚举base, 枚举flat之类的算法 (比如<https://arxiv.org/abs/1904.04129>和 all base belong to us)

几乎所有的 matroid 算法都是用independence oracle, 所以应该用groundset+independence oracle做基础的类型.