COMPILER="ocamlopt"
OBJ_EXT="cmx"


compile:
	@mkdir -p bin
	@ocamlfind $(COMPILER) -linkpkg -package unix \
	    -o bin/life \
	       src/life.ml


clean:
	@rm -rf bin
	@find ./src \
	        -name \*.o \
	    -or -name \*.cmi \
	    -or -name \*.$(OBJ_EXT) \
	    | xargs rm -f
