.PHONY: all clean test install document check

all: document install

clean:
	rm -rf docs/
	rm -rf inst/doc/
	rm -rf man/

test:
	Rscript -e "devtools::test()"

install:
	Rscript -e "devtools::install()"

document:
	Rscript -e "devtools::document()"

check:
	Rscript -e "devtools::check()"

build:
	Rscript -e "devtools::build()"

build_site:
	Rscript -e "pkgdown::build_site()" 