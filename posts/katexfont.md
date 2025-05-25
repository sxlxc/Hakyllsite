---
title: 在 KaTeX 中使用 Fira Math
tags: katex
author: Yu Cong with help from many others
lang: zh
draft: true
date: 2025-05-25
---

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