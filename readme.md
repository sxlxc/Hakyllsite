
The Haskell part is compiled with ghc-9.6.7 and cabal 3.12.1.0

pandoc filters:
 - theorem and biliography filters are copied from [chaodoc](https://github.com/chaoxu/chaoxu.github.io/blob/develop/ChaoDoc.hs)
 - [pangu.hs](https://github.com/sxlxc/pangu.hs), which add spaces between CJK and ascii chars, can be found in [this commit](https://github.com/sxlxc/Hakyllsite/commit/aa54a6a6014269646b190b78c708dc358c3e4fb0)

## Cloudflare Pages

Run `make pages-login` once to authorize the Cloudflare CLI in your browser.
Then run `make publish-pages` whenever you want to build and publish the blog
to https://sxlxc.pages.dev.
This requires Node.js/npm as well as the existing Haskell build tools.

The publish target uploads `_site` to the Pages production branch (`main`)
and copies the custom 404 page to the location Pages expects. Build failures
stop the upload. The original `make publish` target still publishes via rsync.

Override `PAGES_PROJECT` or `CLOUDFLARE_ACCOUNT_ID` when targeting another
Pages project or account. For unattended deployments, supply a
`CLOUDFLARE_API_TOKEN` with Pages Edit access to the target account through
the environment; do not store tokens in the Makefile.
