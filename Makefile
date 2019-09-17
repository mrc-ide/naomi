RSCRIPT = Rscript --no-init-file

test:
	${RSCRIPT} -e "library(methods); devtools::test()"

roxygen:
	@mkdir -p man
	${RSCRIPT} -e "library(methods); devtools::document()"

install:
	R CMD INSTALL .

build:
	R CMD build .

check:
	_R_CHECK_CRAN_INCOMING_=FALSE make check_all

check_all:
	${RSCRIPT} -e "rcmdcheck::rcmdcheck(args = c('--as-cran', '--no-manual'))"

pkgdown:
	Rscript -e "library(methods); pkgdown::build_site()"

website: pkgdown
	./scripts/update_web.sh

.PHONY: test roxygen install build check check_all pkgdown website
