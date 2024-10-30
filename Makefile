wasm: clean
	@wasm-pack build && \
	rm pkg/*.ts pkg/README.md pkg/.gitignore pkg/*.json && \
	mv ./pkg ./web/

clean:
	rm -rf ./web/pkg/

.PHONY: wasm clean
