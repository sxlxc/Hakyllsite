
COMMANDS := build watch rebuild clean
.PHONY: $(COMMANDS), publish

# Set the default goal, so running 'make' without arguments will run 'make build'.
.DEFAULT_GOAL := build


publish:
	rsync -avz --delete -e ssh ./_site/ root@paqtt1611v:/var/www/talldoor
# --- 
$(COMMANDS): site
	@echo "Running command: ./site $@"
	-@./site $@


# --- Rules ---
# using relative symlinks should be fine since everything only works at ./


site: src/site.hs src/ChaoDoc.hs
	cabal build
	ln -sf "$(shell cabal list-bin exe:site)" site

# move from katex to mathjax
# katex_cli:
# 	cd katex_rust_fork && cargo build --release
# 	ln -sf ./katex_rust_fork/target/release/katex_cli katex_cli