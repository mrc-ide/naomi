## Naomi <img src='images/naomi_hex.png' align="right" height="139" />

[![Travis build status](https://travis-ci.org/mrc-ide/naomi.svg?branch=master)](https://travis-ci.org/mrc-ide/naomi)
[![Codecov test coverage](https://codecov.io/gh/mrc-ide/naomi/branch/master/graph/badge.svg)](https://codecov.io/gh/mrc-ide/naomi?branch=master)

Naomi model for subnational HIV estimates

### Development steps

* Make changes in a new branch
* Run tests from the command line using `make test` or via R using `devtools::test()` to ensure changes don't cause regressions to existing functionality
* Run build and check before pushing to remote using `make build` and `make check` from command line or `devtools::check()` from R
* When branch is ready for merging create a PR and add a reviewer
* Ensure that the version number has been updated according to [semantic versioning](https://semver.org/) and add a news item describing the change
* Reviewer should check code and ensure the build passes on [travis](https://travis-ci.org/mrc-ide/naomi) before merging

## Code coverage

To check code coverage from R.

```
cov <- covr::package_coverage()
covr::report(cov)
```


### Website

A [pkgdown website](https://mrc-ide.github.io/naomi) is available for the package. To update the website using most recent docs and vignettes type `make website` on the command line from the root directory. Note that to see the effects in place immediately you may need to refresh the browser cache by opening developer console and right click refresh button -> Empty Cache and Hard Reload.

### Generating test data

Run `./scripts/build_test_data` to run the model and output the fit data. This will speed up some tests and stop the model fitting and uncertainty calculations being done every time. This data is not on github as it is quite large ~ 220MB for Malawi
