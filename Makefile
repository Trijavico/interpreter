
wasm:
	@wasm-pack build && \
	rm pkg/*.ts pkg/README.md pkg/.gitignore pkg/*.json

.PHONY: wasm
