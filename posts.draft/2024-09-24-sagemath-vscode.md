---
title: Add SageMath to pylance
tags: sage
author: Yu Cong
---

I have been wondering how to write `.sage` files in vscode with proper lint and highlighting since half a year ago.

There is a question asked in 2023 on stackoverflow. <https://stackoverflow.com/questions/76201511/add-sagemath-to-pylance>


![select interpreter](/images/sagepylance/select.png)

But what is the interpreter path for python used by sage?

It turns out that you can just type `sage --python`... and everything works

![`sage --python`](/images/sagepylance/sage--python.png)

![\w](/images/sagepylance/ex.png)

There are still cython issues...