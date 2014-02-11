

PACKAGES=async fileutils core.syntax camlp4 bin_prot.syntax sexplib.syntax postgresql csv
# I don't understand warning 18
OPTIONS=-tag annot -no-sanitize -tag debug -use-ocamlfind -cflags -w,+a-4-9-18-41-30-42-44 -cflags -warn-error,+5+10+8+12+20+11 -tag strict_sequence -cflag -bin-annot -j 8 -tag thread -syntax camlp4o
#OPTIONS += -cflags -warn-error,+a
DIRECTORIES=src tests
OCAMLBUILD=ocamlbuild \
		 $(addprefix -package ,$(PACKAGES)) \
		 $(OPTIONS)	\
		 $(addprefix -I ,$(DIRECTORIES)) \

.PHONY: tests monitor.native tests_table.native tests_table.byte tests_api tests

all: .merlin tests_table.native

tests_table.native:
	$(OCAMLBUILD) tests/tests_table.native

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

.merlin: Makefile
	@rm -f .merlin.tmp
	@for PKG in $(PACKAGES); do echo PKG $$PKG >> .merlin.tmp; done
	@for SRC in $(DIRECTORIES); do echo S $$SRC >> .merlin.tmp; done
	@for SRC in $(DIRECTORIES); do echo B _build/$$SRC >> .merlin.tmp; done
	@mv .merlin.tmp .merlin
