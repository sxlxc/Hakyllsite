---
title: 在 KaTeX 中使用 Fira Math
tags: katex
author: Yu Cong with help from many others
lang: zh
# draft: true
date: 2025-05-25
---

> 现在有一个半成品可用 <https://github.com/congyu711/KaTeX>

[Fira Math](https://github.com/firamath/firamath)这字体很不错, 想要在KaTeX中使用它.

有用的链接:

1. <https://yingtongli.me/blog/2022/09/24/katex-custom-fonts.html> -- alphanumeric text only
2. <https://github.com/KaTeX/KaTeX/discussions/3716>
3. <https://github.com/firamath/firamath.github.io/blob/master/bibliography.md>
4. <https://learn.microsoft.com/en-us/typography/opentype/spec/math>

# $\KaTeX$ 字体部分如何工作?

关于fonts和metrics有关的东西首先要看`dockers/fonts/`. 看起来 `fonts/`里面的字体是从docker中安装的texlive的computer modern fonts提取的. 然后`dockers/fonts/buildMetrics.sh` 调用 `src/metrics`里面的代码,直接从有关的tfm文件里读一些metric信息. 

(我猜)生成的ttf,woff2等字体是保存 unicode -> (字形+一些信息) 这个映射关系的文件. `src/metrics/mapping.pl` 就是从tex math到unicode的映射. 做完这个输出这样的东西

```json
{
    ...
    "AMS-Regular": {
        "65": {
            "char": 65,
            "yshift": 0,
            "xshift": 0,
            "font": "msbm10"
        },
        ...
```

之后输入到`src/metrics/extract_tfms.py` 得到

```json
{
    "AMS-Regular": {
        "65": {
            "depth": 0.0,
            "height": 0.68889,
            "italic": 0.0,
            "skew": 0.0,
            "width": 0.72222
        },
        ...
```
最后会再做点工作然后把json塞入`src/fontMetricsData.js`. 我猜如果成功的把Fira Math的metric弄到这个js文件里, 接下来改改css里的字体然后rebuild就行了.

采集metric和生成fonts的过程是独立的, 所以看起来大部分东西都是可以复用的.

# plan

1. 由于$\KaTeX$本身字体分成很多小文件, 首先我也把 Fira Math 分成小文件
2. 然后想办法搞到正确的 `src/metrics/mapping.pl` for Fira Math
3. 最后改改css或者先rebuild再改css之类的.

在与 Fira Math 的[作者](https://stone-zeng.site/)交流之后意识到:

- 在 1. 中把Fira Math 像computer modern一样分成小文件可能可以避免让我需要修改大量KaTeX代码. KaTeX这样做是因为cm在TeX中是[Type 1 font](https://en.wikipedia.org/wiki/PostScript_fonts#Type_1), 单个文件只能容纳256字符.
- Fira Math是[Open Type font](https://en.wikipedia.org/wiki/OpenType). glyph id 到对应字符的 unicode 码位的映射已经包含在了字体的cmap表中. KaTeX生成的html里是unicode字符, 所以我不需要为Fira Math修改`src/metrics/mapping.pl`, 但是仍然需要对应的`src/fontMetricsData.js`.

# Extract fonts

KaTeX把`\mathbb{A} \mathcal{B}` 等等符号全部映射到普通拉丁字母A,B上, 这就导致需要在映射的时候做点工作. 而且另一个问题是KaTeX_AMS-Regular等字体在使用 unicode private use area表示一些unicode中缺少的符号, 比如`src/fonts/makeFF`中有这样的代码

```perl
    0x0A => 0xE010,         # \nleqslant
    0x0B => 0xE00F,         # \ngeqslant
```

因为这些不是我会用的符号, 我决定先不管它.

KaTeX_Main-Regular, KaTeX_Math-Italic.ttf 这些字体表现比较正常, 基本都是Fira Math的子集, 用[这里](https://github.com/congyu711/KaTeX/blob/main/fonts/mimic.py)的代码转换.
对于KaTeX_AMS-Regular和KaTeX_Size{k} 这些字体就需要单独处理, 我决定下载fontforge手动处理. ~~Caligraphic 和 Fraktur 字体Fira Math中也没有覆盖~~, Fira Math 本来就是sans serif字体, 所以我觉得其他字体都可直接用KaTeX版本.

## Italic

~~看起来Fira Math字体并无斜体, 查了查貌似TeX是通过OpenType MATH table来实现斜体, 我需要想想如何搞到Fira Math的斜体版本...~~ 原来Fira Math是有斜体的, 用fontforge修改比手写unicode映射要简单点(目前只有常用符号做了修改, 很多东西不能正常工作) KaTeX的很多设计让人疑惑, 为什么不做像unicode-math一样的设计, 把所有东西映射到正确的unicode上呢? 又比如$\neq$, KaTeX的做法是把一个$=$和一个类似$\setminus$的东西组合起来, 而且在字体中这个像$\setminus$的字符被映射到了private use area.

[Fira Math specimen](https://firamath.github.io/specimen.html) 有点老旧, 实际上其中很多missing glyph现在已经补上了. Texlive里的firamath-regular.otf貌似也是旧版本, 比如缺少(0x2216 ∖)

> 事实上从 KaTeX 的commit history能看到字体主要是[ylemkimon](https://github.com/ylemkimon)的工作. 我发邮件问了问他这些问题, 没有得到回复. 不过我看到[这个知乎回答](https://www.zhihu.com/question/337382562/answer/766077220)之后觉得映射问题可能是为了兼容screen reader? 不过我不觉得这样做就能让screen reader正确读出公式. 
> 另外要谢谢曾祥东老师, 他是Fira Math的主要作者, 读[博客](https://stone-zeng.site/)和[介绍FiraMath的知乎回答](https://www.zhihu.com/question/46196562/answer/766203485)都让我学到很多东西.

如果只是更换字体的话, 很多东西看起来有点奇怪

[commit [866527](https://github.com/congyu711/Hakyllsite/commit/86652755a18568249d02aa3d28624ccec354ea84)  to commit ____] 这个blog在使用这个版本的firamath katex.

![修改字体、没有调整metric](/images/katexfont/withoutmetric.png)


$$
\sqrt{\frac{\int \sum_{aaaa}^{bbbb} \tilde{O}(\log n)\frac{adsf^k}{wer\R_n^k}}{sdf_k}}
$$

但是我觉得最常用的LP, SDP之类的规划问题显示起来效果还不错

\begin{equation}
\begin{aligned}
\min&   &   \sum_x \delta_x&    &   &\\
s.t.&   &   (1-\delta_x - \delta_y) d^2(x,y)\leq \|v_x-v_y\|^2 &\leq (c^2+(\delta_x+\delta_y)f(k)) d^2(x,y) &   &\forall x,y\in X\\
    &   &   \delta_x\in [0,1], v_x&\in \R^p   &   &\forall x\in X
\end{aligned}
\end{equation}

![XeLaTeX](/images/katexfont/sdptest.png)

KaTeX版本和XeLaTeX相比我觉得下标位置看起来还要更自然一点, 不过$f(k)$这里有巨大奇怪间隙. 行内公式还有些其他问题, 比如$\sum_i^j$, 我怀疑我复制错了size1的sum或者KaTeX又做了不符合标准的修改...

接下来要想办法提取metric.

# Metric