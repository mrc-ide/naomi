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

vignettes_rmd: vignettes/model-workflow.Rmd

vignettes/src/model-workflow.Rmd: vignettes/src/model-workflow.R
	${RSCRIPT} -e 'knitr::spin("$<", knit=FALSE)'

vignettes/model-workflow.Rmd: vignettes/src/model-workflow.Rmd
	cd vignettes/src && ${RSCRIPT} -e 'knitr::knit("model-workflow.Rmd")'
	mv vignettes/src/model-workflow.md $@
	mv vignettes/src/figure vignettes/

vignettes/data-model.Rmd: vignettes/src/data-model.Rmd
	cp $^ $@

vignettes_install: vignettes/model-workflow.Rmd vignettes/data-model.Rmd
	${RSCRIPT} -e 'tools::buildVignettes(dir = ".")'

vignettes:
	rm -f vignettes/model-workflow.Rmd vignettes/data-model.Rmd
	make vignettes_install

.PHONY: test roxygen install build check check_all pkgdown website vignettes vignettes_rmd vignettes_install
