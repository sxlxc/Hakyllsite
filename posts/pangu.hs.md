---
title: pangu.hs ?
tags: hakyll
lang: zh
draft: true
date: 2025-12-15
---

[pangu.js](https://github.com/vinta/pangu.js) 是给兼有中英文字符的文本中插入空格的工具。

我觉得这种工具很有用。这个网站曾经使用[一个简化版本](https://www.npmjs.com/package/pangu.simple)。
不过js版本的工具会导致整个网页加载很慢，如果能在编译的时候运行就会变得更好。

最简单的办法其实是在 hakyll 生成 html 之后运行一下 pangu。
有人会想把pangu也变成一个 pandoc filter。在 pandoc AST 上判断中英文字符连接情况有点复杂，不过没有你想的那么复杂。
pangu在每个段落上的工作是独立的，不会需要知道别的段落的信息，在 pandoc AST 当中，每个段落都是一个 `Block`，constructor是 `Para [Inline]`。而 [`Inline`](https://hackage.haskell.org/package/pandoc-types-1.23.1/docs/Text-Pandoc-Definition.html#t:Inline) 包含了所有需要考虑的其他元素。 所以只需要在一个 `Inline` list 当中检查每个 element 内部是否需要空格，以及 list 两个元素之间是否需要空格。我们并不关心遍历 AST 的时候上一个 leaf 是什么。