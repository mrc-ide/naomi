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

vignettes_src/model-workflow.Rmd: vignettes_src/model-workflow.R
	${RSCRIPT} -e 'knitr::spin("$<", knit=FALSE)'

vignettes/model-workflow.Rmd: vignettes_src/model-workflow.Rmd
	rm -rf vignettes_src/cache
	cd vignettes_src && ${RSCRIPT} -e 'knitr::knit("model-workflow.Rmd")'
	mv vignettes_src/model-workflow.md $@
	mv vignettes_src/figure vignettes/

vignettes/data-model.Rmd: vignettes_src/data-model.Rmd
	cp $^ $@
	mkdir vignettes/figure
	./vignettes/script/create_data_model.R

vignettes/hintr-example.Rmd: vignettes_src/hintr-example.R
  ${RSCRIPT} -e 'knitr::spin("$<", knit=FALSE)'

vignettes_install: vignettes/model-workflow.Rmd vignettes/data-model.Rmd vignettes/hintr-example.Rmd
	${RSCRIPT} -e 'tools::buildVignettes(dir = ".")'

vignettes:
	rm -rf vignettes/figure
	rm -rf vignettes_src/outputs
	rm -f vignettes/data-model.Rmd
	rm -f vignettes/model-workflow.Rmd
	rm -f vignettes_src/model-workflow.Rmd
	make vignettes_install

.PHONY: test roxygen install build check check_all pkgdown website vignettes vignettes_rmd vignettes_install
