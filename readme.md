# Building this site

1. deps: [`ghcup`](https://www.haskell.org/ghcup/), [`rustup`](https://rust-lang.github.io/rustup/)
2. build [`katex_rust`](https://github.com/chaoxu/katex_cli/tree/77ce27d8a4670c61d87f865bd54723613897c0a5). link the binary to `./katex_cli`.
3. build the Haskell part with cabal. link the binary to `./site`. (use `cabal list-bin exe:site` to find the binary)
4. `./site {watch/build/clean/...}` [a list of what `site` can do](https://hackage-content.haskell.org/package/hakyll-4.16.6.0/docs/Hakyll-Commands.html)