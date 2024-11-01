prod: wasm
	@cd ./web && pnpm install && pnpm build

wasm: clean
	@wasm-pack build --target web --release && \
	mv ./pkg ./web/

clean:
	rm -rf ./web/pkg/

.PHONY: wasm clean prod
