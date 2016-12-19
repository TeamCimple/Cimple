TARFILES = Makefile scanner.mll parser.mly ast.mli cimple.ml

OBJS = parser.cmo scanner.cmo astutil.cmo semant.cmo ctree.cmo ccodegen.cmo cimple.cmo

all: clean cimple

cimple: $(OBJS)
	ocamlc -o cimple $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : test

test: cimple
	cd tests/parse_trees/pass && ./run_tests.sh
	cd tests/builds/pass && ./run_tests.sh
	cd tests/builds/fail && ./run_tests.sh
	echo "All tests passed"

.PHONY: ast

ast:
	cat $(SRC) | ./cimple -a

.PHONY: anonbuild

anonbuild:
	@(cat tests/anon/anonFunction.cpl | ./cimple -c > anonbuild.c && gcc -o anonbuild anonbuild.c)||:

.PHONY: anonclass

anonclass:
	@(cat tests/anon/anonClass.cpl | ./cimple -c > anonclass.c && gcc -o anonclass anonclass.c)||:

.PHONY: anonint

anonint:
	@(cat tests/anon/anonInterface.cpl | ./cimple -c > anonint.c && gcc -o anonint anonint.c)||:


.PHONY: anonparse

anonparse:
	cat tests/anon/anonFunction.cpl | ./cimple -a

.PHONY : clean
clean :
	rm -f calc parser.ml parser.mli scanner.ml *.cmo *.cmi

# Generated by ocamldep *.ml *.mli
calc.cmo: scanner.cmo parser.cmi ast.cmi 
calc.cmx: scanner.cmx parser.cmx ast.cmi
cimple.cmo: ccodegen.cmo ctree.cmo semant.cmo scanner.cmo parser.cmi ast.cmi
cimple.cmx: ccodegen.cmx ctree.cmx semant.cmx scanner.cmx parser.cmx ast.cmi
parser.cmo: ast.cmi parser.cmi 
parser.cmx: ast.cmi parser.cmi
scanner.cmo: parser.cmi  
scanner.cmx: parser.cmx
parser.cmi: ast.cmi
semant.cmi: ast.cmi astutil.cmi
semant.cmo: ast.cmi semant.cmi
semant.cmx: ast.cmi semant.cmi
astutil.cmi: ast.cmi astutil.ml
astutil.cmo: ast.cmi astutil.ml
ctree.cmi: ast.cmi semant.cmi 
ctree.cmo: ast.cmi semant.cmi ctree.ml
ctree.cmx: ast.cmi semant.cmi ctree.cmi
ccodegen.cmi: ast.cmi ctree.cmi
ccodegen.cmo: ast.cmi ctree.ml
ccodegen.cmx: ast.cmi ctree.cmi
