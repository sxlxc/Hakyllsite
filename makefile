
COMMANDS := build watch rebuild clean
.PHONY: $(COMMANDS)

# Set the default goal, so running 'make' without arguments will run 'make build'.
.DEFAULT_GOAL := build


# --- 
$(COMMANDS): site
	@echo "Running command: ./site $@"
	-@./site $@


# --- Rules ---
# using relative symlinks should be fine since everything only works at ./


site: katex_cli
	cabal build
	ln -sf "$(shell cabal list-bin exe:site)" site

katex_cli:
	cd katex_rust_fork && cargo build --release
	ln -sf ./katex_rust_fork/target/release/katex_cli katex_cli