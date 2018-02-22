.PHONY: test

test:
	stack test --file-watch

exec-path: 
	@echo "$(shell stack path --dist-dir)/build/fireward/fireward"
	
tmp/try.rules: ./.stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/fireward/fireward
tmp/try.rules: $(file) 
	test -r "$(file)"
	stack exec fireward -- -i $(file) > tmp/try.rules
	cat tmp/try.rules	

try: tmp/try.rules
	@#
