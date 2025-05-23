---
title: Pdf Readers for MacOS
tags: macos
author: Yu Cong
lang: zh
# draft: true
date: 2025-05-23
---

要研究东西总是需要读纸, 纸都是pdf, 所以好用的pdf reader很有用. 

我来给用过的pdf reader以及需要的功能进行对比.

1. [skim](https://skim-app.sourceforge.io/). skim还挺好用的, 缺点是快捷键太不灵活, 如果能自己设置jk翻页之类的就好了, [之前](/posts/2024-05-23-buildskim.html)修改了代码, 勉强可用, 但是后来花了很长时间问AI工具都没搞明白如何更新目录和页码.
2. [Preview](https://en.wikipedia.org/wiki/Preview_(macOS)) 是一坨屎. 除了好看没有优点了. 为什么文件修改之后要focus才能更新页面? 设置的视图在文件重新加载也不能恢复. 这就导致完全无法用它preview :( 用latex写的东西. 一样无灵活的快捷键. skim和Preview都用Apple pdfkit, 目前如果让app始终显示滚动条的话, pdfkit貌似不能正确处理pdf页面宽度. Preview在pdf里面的搜索功能据说会漏掉一些.
3. [PDF.js(in Firefox)](https://mozilla.github.io/pdf.js/) 很好, 某个版本是使用`j,k`下移页面的, 但是目前变成了翻页. 这种按键逻辑就很奇怪, 为什么已经有了`n,p`来翻页还要把`j,k`设置成这样的功能? 即使你想用`j,k`来翻页, 那么使用singe page mode也可以. 快捷键做的最好的编辑器就是[sumatra](https://www.sumatrapdfreader.org/free-pdf-reader)了,可惜只能在windows上用. 如果用PDF.js来和编辑器一起写纸的话, vscode的插件可以在tab里用PDF.js预览, 编译之后自动刷新, 速度有点慢.
4. zotero 内置的 reader. 优点是可以准确的显示link到的东西(skim有类似功能,但是不能准确显示到目标), 但是滚动起来好卡, 而且zotero的tab用起来很难受. 但是zotero有个很好的插件 <https://github.com/retorquere/zotero-open-pdf>, 让你来选择用什么reader打开.

对于写latex来说, 实际上不需要频繁看编译出来的内容. 我觉得[这种Emacs mode](https://tony-zorman.com/posts/pretty-latex.html)就很好, 但是我不想花时间学如何用Emacs

对于读纸, 想要vim一样的键盘快捷键是因为用KDE的时候长期使用Okular养成的习惯. 如果能修改skim, 加入一个修改快捷键的功能会很好, 但是对于不懂objective-c的人来说可能太难了. skim源码在SourceForge上, 要用svn, 也是另一个劝退的原因.

对于这个网站来说, 如果katex能支持把字体改成firamath, 可能所有的notes都可以写markdown然后用浏览器看了.