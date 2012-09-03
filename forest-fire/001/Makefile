COMPILER="ocamlopt"
OBJ_EXT="cmx"


compile:
	@mkdir -p bin
	@ocamlfind $(COMPILER) -linkpkg -package unix \
			-o bin/forest_fire \
			   src/forest_fire.ml


clean:
	@rm -rf bin
	@find src \
		    -iname '*.o' \
		-or -iname '*.cmi' \
		-or -iname "*.$(OBJ_EXT)" \
		| xargs rm
