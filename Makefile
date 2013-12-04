

PACKAGES=async fileutils core.syntax camlp4 bin_prot.syntax sexplib.syntax postgresql
# I don't understand warning 18
OPTIONS=-tag annot -no-sanitize -tag debug -use-ocamlfind -cflags -w,+a-4-9-18-41-30-42-44 -cflags -warn-error,+5+10+8+12+20+11 -cflag -bin-annot -j 8 -tag thread -syntax camlp4o
#OPTIONS += -cflags -warn-error,+a
DIRECTORIES=src/common src/monitor src/conductor tests
OCAMLBUILD=ocamlbuild \
		 $(addprefix -package ,$(PACKAGES)) \
		 $(OPTIONS)	\
		 $(addprefix -I ,$(DIRECTORIES)) \

.PHONY: tests monitor.native tests_table.native tests_table.byte

all: monitor.native .merlin tests_table.native

monitor.native tests_table.native:
	$(OCAMLBUILD) src/monitor/monitor.native tests/tests_table.native

monitor.byte:
	$(OCAMLBUILD) src/monitor/monitor.byte

tests_table.byte:
	$(OCAMLBUILD) tests/tests_table.byte


#Because ocamlbuild doesn't give to ocamldoc the .ml when a .mli is present
dep:
	cd _build; \
	ocamlfind ocamldoc -o dependencies.dot $$(find src -name "*.ml" -or -name "*.mli") \
	$(addprefix -package ,$(PACKAGES)) \
	$(addprefix -I ,$(DIRECTORIES)) \
	-dot -dot-reduce
	sed -i -e "s/  \(size\|ratio\|rotate\|fontsize\).*$$//" _build/dependencies.dot
	dot _build/dependencies.dot -T svg > dependencies.svg

clean:
	ocamlbuild -clean

.merlin: Makefile
	@rm -f .merlin.tmp
	@for PKG in $(PACKAGES); do echo PKG $$PKG >> .merlin.tmp; done
	@for SRC in $(DIRECTORIES); do echo S $$SRC >> .merlin.tmp; done
	@for SRC in $(DIRECTORIES); do echo B _build/$$SRC >> .merlin.tmp; done
	@mv .merlin.tmp .merlin
