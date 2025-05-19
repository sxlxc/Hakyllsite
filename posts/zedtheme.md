---
title: Zed theme for VSCode
tags: zzz
author: Yu Cong
lang: en
# draft: true
date: 2025-05-19
---

[Zed](https://zed.dev/) is a great editor. 
I especially like the One Light theme in Zed.
But for some reasons I cannot fully switch to it.

Throw the following into `settings.json` to make the Light Modern theme looks like Zed's One Light.

```yaml
// colors from zed's one light theme
"editor.lineHeight": 1.6,
"workbench.colorTheme": "Default Light Modern",
"workbench.colorCustomizations": {
    "titleBar.activeBackground": "#dcdcdd",
    "titleBar.inactiveBackground": "#dcdcdd",
    "editor.lineHighlightBackground": "#efeff0",
    "editor.lineHighlightBorder": "#efeff0",
    "tab.activeBackground": "#fafafb",
    "tab.inactiveBackground": "#ebebec",
    "tab.border": "#c9c9ca",
    "tab.activeBorder": "#fafafb",
    "tab.activeBorderTop": "#fafafb",
    "tab.selectedBorderTop": "#fafafb",
    "editorGroupHeader.tabsBackground": "#ebebec",
    "editorGroupHeader.border": "#ebebec",
    "statusBar.background": "#dcdcdd",
    "activityBar.background": "#ebebec",
},
"editor.tokenColorCustomizations": {
    "textMateRules": [
        {
            "scope": [
                // in light_plus
                "constant.character",
                "constant.other.option",
                // in light_vs
                "meta.embedded",
                "source.groovy.embedded",
                "string meta.image.inline.markdown",
                "variable.legacy.builtin.python",
                "constant.language",
                "meta.preprocessor",
                "entity.name.function.preprocessor",
                "storage",
                "storage.type",
                "storage.modifier",
                "keyword.operator.noexcept",
                "string.comment.buffered.block.pug",
                "string.quoted.pug",
                "string.interpolated.pug",
                "string.unquoted.plain.in.yaml",
                "string.unquoted.plain.out.yaml",
                "string.unquoted.block.yaml",
                "string.quoted.single.yaml",
                "string.quoted.double.xml",
                "string.quoted.single.xml",
                "string.unquoted.cdata.xml",
                "string.quoted.double.html",
                "string.quoted.single.html",
                "string.unquoted.html",
                "string.quoted.single.handlebars",
                "string.quoted.double.handlebars",
                "punctuation.definition.template-expression.begin",
                "punctuation.definition.template-expression.end",
                "punctuation.section.embedded",
                "keyword",
                "keyword.control",
                "keyword.operator.new",
                "keyword.operator.expression",
                "keyword.operator.cast",
                "keyword.operator.sizeof",
                "keyword.operator.alignof",
                "keyword.operator.typeid",
                "keyword.operator.alignas",
                "keyword.operator.instanceof",
                "keyword.operator.logical.python",
                "keyword.operator.wordlike",
                "variable.language",
            ],
            "settings": {
                "foreground": "#4e77ea"
            }
        },
    ]
},
```

![example](/images/zedtheme/example.png)