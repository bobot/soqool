

PACKAGES=async fileutils core.syntax camlp4 bin_prot.syntax sexplib.syntax postgresql csv
# I don't understand warning 18
OPTIONS=-tag annot -no-sanitize -tag debug -use-ocamlfind -cflags -w,+a-4-9-18-41-30-42-44 -cflags -warn-error,+5+10+8+12+20+11 -tag strict_sequence -cflag -bin-annot -j 8 -tag thread -syntax camlp4o -no-links
#OPTIONS += -cflags -warn-error,+a
DIRECTORIES=src tests
OCAMLBUILD=ocamlbuild \
		 $(addprefix -package ,$(PACKAGES)) \
		 $(OPTIONS)	\
		 $(addprefix -I ,$(DIRECTORIES)) \

.PHONY: tests tests_table.native tests_table.byte tests_api tests

LIB=soqool.cma soqool.cmxa soqool.cmxs soqool.a	\
    soqool.cmi soqool_core.cmi soqool_poly.cmi	\
    soqool_ty.cmi soqool_sql.cmi

all: .merlin lib/soqool.timestamp tests_table.native

.PHONY: force

lib/soqool.timestamp: force
	@mkdir -p lib/soqool
	@echo build soqool lib
	@$(OCAMLBUILD) $(addprefix src/, $(LIB))
	@tar c -C _build/src $(LIB) | tar x -C lib/soqool
	@cp -a META lib/soqool
	@touch $@


tests_table.native: DIRECTORIES=tests

tests_table.native: lib/soqool.timestamp
	@echo build $@
	@OCAMLPATH=$(PWD)/lib $(OCAMLBUILD) -package soqool tests/tests_table.native

doc:
	$(OCAMLBUILD) soqool.docdir/index.html

tests: tests/lahman2012/ tests_table.native
	./tests_table.native

tests_api:
	$(OCAMLBUILD) tests/tests_table.cmo

tests_table.byte:
	$(OCAMLBUILD) tests/tests_table.byte

tests/lahman2012-csv.zip:
	wget "http://seanlahman.com/files/database/lahman2012-csv.zip" -O tests/lahman2012-csv.zip


tests/lahman2012/: tests/lahman2012-csv.zip
	mkdir -p $@
	unzip tests/lahman2012-csv.zip -d $@

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

.PHONY: install uninstall

install:
	ocamlfind remove soqool
	ocamlfind install soqool META $(LIB)

uninstall:
	ocamlfind remove soqool

.merlin: Makefile
	@rm -f .merlin.tmp
	@for PKG in $(PACKAGES); do echo PKG $$PKG >> .merlin.tmp; done
	@for SRC in $(DIRECTORIES); do echo S $$SRC >> .merlin.tmp; done
	@for SRC in $(DIRECTORIES); do echo B _build/$$SRC >> .merlin.tmp; done
	@mv .merlin.tmp .merlin
