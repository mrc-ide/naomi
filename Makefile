pkgdown:
	Rscript -e "library(methods); pkgdown::build_site()"

website: pkgdown
	./scripts/update_web.sh
