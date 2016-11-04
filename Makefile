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
	echo '__________________1+(3-2);_________________________________' > testOutput.txt
	echo '1+(3-2);' | ./cimple >> testOutput.txt
	echo '__________________1+3-2;___________________________________' >> testOutput.txt
	echo '1+3-2;' | ./cimple >> testOutput.txt
	echo '__________________a=1;_____________________________________' >> testOutput.txt
	echo 'a=1;' | ./cimple >> testOutput.txt
	echo '__________________a+=2;____________________________________' >> testOutput.txt
	echo 'a+=1;' | ./cimple >> testOutput.txt

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
