
COMMANDS := build watch rebuild clean
.PHONY: $(COMMANDS) publish publish-pages pages-login

PAGES_PROJECT ?= sxlxc
CLOUDFLARE_ACCOUNT_ID ?= 89c5708422788def3a1199eb6f8d6bcd
WRANGLER ?= npx --yes wrangler@4

# Set the default goal, so running 'make' without arguments will run 'make build'.
.DEFAULT_GOAL := build


publish:
	rsync -avz --delete -e ssh ./_site/ root@paqtt1611v:/var/www/talldoor

# Authenticate once, then run make publish-pages for each update.
pages-login:
	$(WRANGLER) login

publish-pages: build
	cp _site/404/index.html _site/404.html
	CLOUDFLARE_ACCOUNT_ID=$(CLOUDFLARE_ACCOUNT_ID) $(WRANGLER) pages deploy _site --project-name=$(PAGES_PROJECT) --branch=main
# --- 
$(COMMANDS): site
	@echo "Running command: ./site $@"
	@./site $@


# --- Rules ---
# using relative symlinks should be fine since everything only works at ./


site: src/site.hs src/ChaoDoc.hs
	cabal build
	ln -sf "$(shell cabal list-bin exe:site)" site

# move from katex to mathjax
# katex_cli:
# 	cd katex_rust_fork && cargo build --release
# 	ln -sf ./katex_rust_fork/target/release/katex_cli katex_cli
