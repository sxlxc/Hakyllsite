---
title: 一些写 pandoc markdown 的工具
tags: zzz
lang: zh
# showtoc: true
date: 2026-05-28
---

让AI帮忙做数学研究，写notes还用 LaTeX 并不方便。 AI的输出就是markdown，如果自己的notes也写markdown会方便不少。

需要的东西很简单，写markdown，有数学公式支持和citation，最好有定理和证明环境。pandoc markdown就很合适，像这个网站就是用hakyll生成的，用fenced divs来写定理环境，用`chaoDoc`中的filter 处理，最后在浏览器里预览。

各种编辑器对pandoc markdown的支持并不好，于是用codex写了不少工具。

# Lua version

首先要搞定如何预览pandocmd，hakyll watch对预览一个note来说太过强大了，于是直接用pandoc cli+一堆lua filter来搞定。entr来看文件改动，保存后自动让pandoc转换成html，扔到在跑webserver的文件夹里，最后在浏览器里预览。

预览部分有很多方便的东西，比如能跳转回zed的行号，自动刷新浏览器，preview on hover等等。

![[/posts/LasserreHierarchy/](/posts/LasserreHierarchy/)](/images/pandocmd/preview.png)

<details>
<summary>数学公式仍然在用MathML</summary>
在网页里本来就不该追求多麽完美的公式排版。<br>
至少Firefox里MathML看起来还行，而且也只有MathML能用基于Lato的数学字体LeteSansMath。
</details>

这些工具被整合到一个fish脚本里，扔到path里之后更新这套系统只需要 `git pull`。

<https://github.com/sxlxc/pandocmd-lua>

# Language server

在pandocmd里写cross-ref的东西需要经常查看label具体叫啥，如果有texlab/typst一样的编辑器支持就会方便很多。在写编辑器插件之前就需要有个language server。

pandocmd的tree sitter有人写过，基于markdown tree sitter。 codex直接用rust写了个language server出来，我完全不知道是怎么写的，但是看起来工作正常。主要在提供cross ref的提示以及一个简易linter。

<https://github.com/sxlxc/pandocmd-languageserver>


# Zed extension

![提示这citation是什么](/images/pandocmd/hover.png)

![补全label](/images/pandocmd/completion.png)

没有发布到zed的仓库里，只能通过install dev extension的方式安装。

<https://github.com/sxlxc/zed-pandoc-markdown>
