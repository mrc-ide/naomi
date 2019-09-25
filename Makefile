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

website: vignettes_rmd pkgdown
	./scripts/update_web.sh

vignettes/%.Rmd: vignettes_src/%.R
	mkdir -p vignettes
	${RSCRIPT} -e 'knitr::spin("$<", knit=FALSE)'
	mv vignettes_src/$(@F) $@

vignettes_rmd: vignettes/model-workflow.Rmd

vignettes: vignettes/data-model.Rmd vignettes/model-workflow.Rmd
	${RSCRIPT} -e 'tools::buildVignettes(dir = ".")'
	mkdir -p inst/doc
	cp vignettes/*.html vignettes/*.Rmd inst/doc

.PHONY: test roxygen install build check check_all pkgdown website vignettes vignettes_rmd
