TARFILES = Makefile scanner.mll parser.mly ast.mli cimple.ml

OBJS = parser.cmo scanner.cmo cimple.cmo

cimple: $(OBJS)
	ocamlc -o cimple $(OBJS)

all: $(OBJS)


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
	cat tests/functionDeclaration.test | ./cimple > tests/functionDeclaration.out
	cat tests/functionDeclaration.2.test | ./cimple > tests/functionDeclaration.2.out
	cat tests/functionDeclaration.3.test | ./cimple > tests/functionDeclaration.3.out

.PHONY : clean
clean :
	rm -f calc parser.ml parser.mli scanner.ml *.cmo *.cmi

# Generated by ocamldep *.ml *.mli
calc.cmo: scanner.cmo parser.cmi ast.cmi 
calc.cmx: scanner.cmx parser.cmx ast.cmi
cimple.cmo: scanner.cmo parser.cmi ast.cmi
cimple.cmx: scanner.cmx parser.cmx ast.cmi
parser.cmo: ast.cmi parser.cmi 
parser.cmx: ast.cmi parser.cmi
scanner.cmo: parser.cmi  
scanner.cmx: parser.cmx
parser.cmi: ast.cmi
