---
title: pangu.hs
tags: hakyll
lang: zh
showtoc: true
date: 2025-12-15
---

[pangu.js](https://github.com/vinta/pangu.js) 是给兼有中英文字符的文本中插入空格的工具。

我觉得这种工具很有用。这个网站曾经使用[一个简化版本](https://www.npmjs.com/package/pangu.simple)。
不过js版本的工具会导致整个网页加载很慢，如果能在编译的时候运行就会变得更好。

最简单的办法其实是在 hakyll 生成 html 之后运行一下 pangu。
有人会想把pangu也变成一个 pandoc filter。在 pandoc AST 上判断中英文字符连接情况有点复杂，不过没有你想的那么复杂。
pangu在每个段落上的工作是独立的，不会需要知道别的段落的信息，在 pandoc AST 当中，每个段落都是一个 `Block`，constructor是 `Para [Inline]`。而 [`Inline`](https://hackage.haskell.org/package/pandoc-types-1.23.1/docs/Text-Pandoc-Definition.html#t:Inline) 包含了所有需要考虑的其他元素。 所以只需要在一个 `Inline` list 当中检查每个 element 内部是否需要空格，以及 list 两个元素之间是否需要空格。我们并不关心遍历 AST 的时候上一个 leaf 是什么。

# Python → Haskell

在元旦假期终于开始写。计划是从python版本迁移过来，因为vinta实现pangu的那些语言里我只看得懂python。

我发现貌似作者自己也没总结出什么中英文排版加空格、换全角或中文标点的规则（也可能是因为ww和大陆习惯也不同），而且正则表达式里漏洞很多。

pangu.hs 需要根据一些规则来替换字符和删减空格，所以应该把这些规则写下来然后弄一个存放规则的list，程序根据这个list里的规则来调整文本，同时用户见到自己不想要的rule也可以直接comment它。

我没有在Haskell中使用正则表达式的经验，于是求助Gemini和ChatGPT。相比于regex他们更推荐用 [megaparsec](https://hackage.haskell.org/package/megaparsec)。
如果要 find and replace，就需要用[`streamEdit`](https://hackage.haskell.org/package/replace-megaparsec-1.5.0.1/docs/Replace-Megaparsec.html#v:streamEdit).
这函数会找所有不重叠的section，完成替换任务。

```haskell
type Parser = Parsec Void Text
type Rule = Parser Text
type RuleSet = [Rule]

applyUntilFixed :: Rule -> Text -> Text
applyUntilFixed rule =
  fix
    ( \loop current ->
        let next = streamEdit (try rule) id current
         in if next == current then next else loop next
    )

applyRulesRecursively :: RuleSet -> Text -> Text
applyRulesRecursively rules input = 
  foldl (flip applyUntilFixed) input rules

applyRules :: RuleSet -> Text -> Text
applyRules rules input = foldl (flip applyOnce) input rules
  where
    applyOnce rule = streamEdit (try rule) id
```
<details>
  <summary>然后即可这样选择要启用哪些`Rule`</summary>
```hs
recursiveRules :: RuleSet
recursiveRules =
  [ fullwidthCJKsymCJK,
    fullwidthCJKsym
  ]

onepassRules :: RuleSet
onepassRules =
  [ dotsCJK,
    fixCJKcolAN,
    cjkquote,
    quoteCJK,
    fixQuote,
    cjkpossessivequote,
    -- singlequoteCJK,
    fixPossessivequote,
    hashANSCJKhash,
    cjkhash,
    -- hashcjk,
    anscjk,
    cjkans,
    empty -- a dummy rule
  ]

pangu :: Text -> Text
pangu input = 
  applyRules onepassRules $ applyRulesRecursively recursiveRules input
```

</details>

完整代码在 <https://github.com/sxlxc/pangu.hs> 可以看到。
功能并不和pangu.py完全一致

# Pandoc filter

现在想要在hakyll里使用这个包。
这部分很简单，只要实现上文说的想法就好了。
pandoc-types提供了很多方便写filter的函数，我们实现好处理 `[Inline]` 的filter就好了。

```hs
panguFilter :: Pandoc -> Pandoc
panguFilter = walk transformBlocks
  where
    transformBlocks :: Block -> Block
    transformBlocks (Para inlines) = Para (panguInlines inlines)
    transformBlocks x = x
```

完整的实现可以看[这个commit](https://github.com/sxlxc/Hakyllsite/commit/aa54a6a6014269646b190b78c708dc358c3e4fb0#diff-41a377645bcbb4f6f5b24bf5a69734635f3d75e7d0c2d6a7507ed7a4573bc7f9)。

# CSS `text-autospace`

一月九号才在[知乎](https://zhuanlan.zhihu.com/p/1990751194956723768)看到CSS也支持类似的功能: [`text-autospace`](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Properties/text-autospace)

2025年九月份就已经在主流浏览器上可用。
