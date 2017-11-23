VERSION=`grep "^Version" DESCRIPTION | sed -e 's/[^\.0-9]//g'`
MAKEVARS="$(abspath ${CURDIR}/src/Makevars)"
DEBUG?=0
SOURCES=src/*.c
PKG_SOURCES=R/*.r R/*.R
INSTALL_SRC?=../streambugs_$(VERSION).tar.gz
INSTALL_DEST?=`Rscript -e ".libPaths()[1]" | cut -f 2 -d " "`
ifeq ($(DEBUG),0)
    # productive setup (with some extra CRAN curator's flags)
    PKG_CPPFLAGS=-mtune=native -Wall -pedantic
else
    # debug setup: stop at minor compilation errors, and add debug symbol
	# Note: "-O0 -g3" flags will have no effect as they are overriden with
	#       "-g -O2" flags by devtools induced compilation of the C sources
	PKG_CPPFLAGS=-mtune=native -Wall -pedantic -Werror -DDEBUG
endif

define compile
	Rscript -e "devtools::document()"
endef

define compile_cran
	echo "PKG_CPPFLAGS=$(PKG_CPPFLAGS)" >> $(MAKEVARS)
	-$(call compile)
	sed -i -e "$$ d" $(MAKEVARS)
	@$(RM) $(MAKEVARS)-e # dirty fix for incompatibility of sed's -i switch (Mac OS vs. Unix)
endef

define create_package
	-$(call compile_cran)
	Rscript -e "devtools::build()"
endef

all: package


version:
	@echo $(VERSION)

build: $(SOURCES)
	$(call compile_cran)


clean_src:
	$(RM) src/*.o src/*.so

clean_doc:
	$(RM) man/*.Rd

clean_check:
	$(RM) -r streambugs.Rcheck/ ..Rcheck/

clean_pdf:
	$(RM) -r .Rd2pdf* ..pdf

clean_pkg:
	$(RM) $(INSTALL_SRC)

clean: clean_src clean_doc clean_check clean_pkg clean_pdf


package: $(SOURCES) $(PKG_SOURCES)
	$(call create_package)

install: package
	R CMD install $(INSTALL_SRC) $(INSTALL_DEST)

install_cwd:
	R CMD install . $(INSTALL_DEST)

uninstall:
	-R CMD REMOVE streambugs

pdf:
	$(call compile)
	R CMD Rd2pdf .

test:
	Rscript -e "devtools::test()"

check: package
	R CMD check --as-cran ../streambugs_$(VERSION).tar.gz
