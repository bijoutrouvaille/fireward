.PHONY: test

EXEC:=fireward

test:
	stack test --file-watch

exec-path: 
	@echo "$(shell stack path --dist-dir)/build/fireward/fireward"
	
tmp/try.rules: ./.stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/fireward/fireward
tmp/try.rules: $(file) 
	test -r "$(file)"
	stack exec fireward -- -i $(file) > tmp/try.rules
	cat tmp/try.rules	

try: $(file)
	stack build
	test -r "$(file)"
	stack exec fireward -- -i $(file) 


prefix?=/usr/local/bin
PREFIX:=$(prefix)


LOCAL_PATH:=$(shell stack path --local-bin)
install: 
	stack install && cp $(LOCAL_PATH)/$(EXEC) $(prefix)/

VERSION=$(shell stack exec fireward -- -V)

buildtest:
	stack build	
	# stack test

V=$(shell stack exec fireward -- -V)
tag:
	git tag -a "$(V)"

release:
	make buildtest
	make tag
	git push origin master --follow-tags

v?=$(V)
publish: 
	cd npm-bin &&\
	curl -L https://github.com/bijoutrouvaille/fireward/releases/download/$(v)/fireward-linux > fireward-linux && \
	curl -L https://github.com/bijoutrouvaille/fireward/releases/download/$(v)/fireward-osx > fireward-osx && \
	curl -L https://github.com/bijoutrouvaille/fireward/releases/download/$(v)/fireward.exe > fireward.exe && \
	npm version "$(v)" && \
	npm publish

