# naomi 0.0.28

* Model for ANC prevalence and ART coverage at time 2.
* Drop ANC observations that have NA for any required inputs.

# naomi 0.0.27

* Explicit type conversion of integer and logical model options.

# naomi 0.0.25

* Carry forward / backward interpolated population if uploaded population does not span years required.

# naomi 0.0.25

* Remove spectrum calibration option validation -- NULLs not handled appropriately.

# naomi 0.0.24

* Hide placeholder <1 / 1-4 outputs which are not implemented yet.
* Handle case where ANC testing provided but no year selected (valid) or where a year is requested not found in data (error) (mrc-1176)

# naomi 0.0.23

* Implement pre-fitting calibration to Spectrum population size by sex and 5-year age group.
* Implement post-fitting calibration to Spectrum PLHIV and number on ART.
* Save Spectrum calibration in `fit/spectrum_calibration.csv` in the ouput zip download.

# naomi 0.0.22

* Remove ancrt_hiv_status field from ANC testing dataset.
* Add parent_area_id and spectrum_region_code to outputs package.

# naomi 0.0.21

* Revise ART attendance model to be single district level 'attractiveness' parameter.
* Add model input validation step to hintr_run_model().
* Implement random number seed argument to sample_tmb() to return exact same results.
* Silent fitting for hintr_run_model() unless option outer_verbose = TRUE or inner_verbose = TRUE.
* Specify maximum number of iterations for fit_tmb().
* Warning if fit_tmb() convergence error.
* hintr_run_model() throw error if convergence error, unless option$permissive = TRUE.

# naomi 0.0.20

* Add function to return 5 year age groups
* Support multiple PJNZ extracts in a zip folder.

# naomi 0.0.19

* Add age <1 / 1-4 age stratification to outputs.
* Add number receiving art in area outputs.
* Add ASFR inputs.
* Add inputs/options.csv in zipped output package.

# naomi 0.0.18

* Use odds ratios from Spectrum to model prevalence and ART coverage above age of data availability.
* Save metadata alongside model outputs, when run via the hint run entrypoint (mrc-760)

# naomi 0.0.17

* Use annual programme data input instead of quarterly.
* Add spectrum_region_code to area hierarchy.
* Save metadata alongside model outputs, when run via the hint run entrypoint (mrc-760)
* Add advanced model run options
* Add skeleton of model input validation

# naomi 0.0.16

* Move metadata into directory separate from extdata
* Add error_low and error_high columns to metadata

# naomi 0.0.15

* Allow passing no of samples in model run options

# naomi 0.0.14

* Update progress message

# naomi 0.0.13

* Set default args for hintr model run function

# naomi 0.0.12

* Wrap model run in single function for use in hintr API

# naomi 0.0.11

* Fix issues with naomi model run options schema templates

# naomi 0.0.10

* Use iso3 to locate country plotting metadata instead of country name

# naomi 0.0.9

* Plotting metadata now returns default if country is missing from config

# naomi 0.0.8

* Iterate model options endpoint to be able to exclude optional data set options

# naomi 0.0.7

* Add function to return model run options template to build front end declaratively

# naomi 0.0.6

* Iterate metadata to include model outputs
* Expose get_metadata function for hintr use

# naomi 0.0.5

* Add function to aggregate multiple Spectrum files.

# naomi 0.0.4

* Allow use of survey VLS data.

# naomi 0.0.3

* Ouput uncertainty ranges based on Monte Carlo simulation from joint covariance
  matrix of fixed and random effects.

# naomi 0.0.2

* Add function to calculate prevalence and ART coverage from ANC data

# naomi 0.0.1

* Add travis build
