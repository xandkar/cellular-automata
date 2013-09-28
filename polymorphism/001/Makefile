.PHONY: build clean

EXE_TYPE := native

build:
	$(eval executable_name     := polymorphism)
	$(eval executable_name_ext := $(executable_name).$(EXE_TYPE))
	@ocamlbuild \
		-use-ocamlfind \
		-package core \
		-tag thread \
		-tag short-paths \
		-cflags "-w +A-44" \
		src/$(executable_name_ext)
	@mkdir -p bin/
	@cp _build/src/$(executable_name_ext) bin/$(executable_name)
	@rm $(executable_name_ext)

clean:
	@ocamlbuild -clean
	@rm -rf bin/
