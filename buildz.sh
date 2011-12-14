ocamllex  lexer.mll &&
ocamlyacc -v parser.mly &&
ocamlfind ocamlopt -package batteries,batteries.syntax -syntax camlp4o -thread -linkpkg -c asttypes.mli asttypes.ml &&
ocamlfind ocamlopt -package batteries,batteries.syntax -syntax camlp4o -thread -linkpkg -c parser.mli parser.ml &&
ocamlfind ocamlopt -package batteries,batteries.syntax -syntax camlp4o -thread -linkpkg -c lexer.ml &&
ocamlfind ocamlopt -package batteries,batteries.syntax -syntax camlp4o -thread -linkpkg asttypes.cmx lexer.cmx parser.cmx ast.ml -o ast
