all: parsage rapport

parsage: lexer.mll parser.mly
	ocamlc -c x86_64.mli	
	ocamlc -c x86_64.ml
	ocamlc -c asyntax.ml
	ocamlc -c ass.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c calc.ml
	ocamlc -o aritha x86_64.cmo asyntax.cmo lexer.cmo parser.cmo ass.cmo calc.cmo

rapport: rapport.tex
	pdflatex -shell-escape Rapport.tex -o rapport.pdf

result:
	gcc -no-pie input.s -o input
	./input

clean:
	rm -rf main calc lexer.ml parser.ml parser.mli  *.cmi *.cmo *~ *.log *.pdf *.s aritha rapport.pdf input