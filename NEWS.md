# naomi 0.0.47

* Add read_***() functions with col_type parsers.

# naomi 0.0.46

* Add TMB-Stan model fitting, with and without Laplace approximation.
* Add INLA model fitting. 

This is for development and comparison purposes, not for production use.

# naomi 0.0.45

* Remove age_group_id, indicator_id, and quarter_id from model frames; use human readable age_group, indicator, and calendar_quarter everywhere.
* Output number of ART attendees between every district pair. Currently output at 
  estimation level only.
* spread_areas() allows an sf object as argument and returns boundaries for wide 
  format areas if provided.
* No sex differences in prevalence, ART coverage, or incidence for age below 15 years.
* Cap Spectrum ART coverage between 0.001 and 0.999 to avoid logit transformation NaN errors.

# naomi 0.0.44

* Change ART indicator labels to "ART Number (residents)" and "ART Number (attending)".

# naomi 0.0.43

* Check that esimates quarter (time 2) is greater than survey quarter (time 1).

# naomi 0.0.42

* Add more translations

# naomi 0.0.41

* Separate PLHIV projection and incidence calculation.

# naomi 0.0.40

* Cap ART coverage by age/sex from Spectrum between 0.1% and 99.9% to avoid logit difference evaluating to +/-Inf.

# naomi 0.0.39

* Set Mozambique colour scales

# naomi 0.0.38

* If there is no ART data at time 2, use logit change in ART coverage between time 1 and time 2 from Spectrum to approximate change in ART coverage. Do not estimate non-identifiable parameters.

# naomi 0.0.37

* Use Spectrum estimates at time 1 for transmission rate defaults (mrc-1236).

# naomi 0.0.36

* Reverse order of ANC and ART sections
* Move artattend_log_gamma_offset into the advanced section

# naomi 0.0.35

* Throw error if trying to fit model at country level (mrc-1233).

# naomi 0.0.34

* Change quarter labels from range to end quarter ("Jan - Mar 2019" -> "March 2019").

# naomi 0.0.33

* Implement ANC prevalence and ART coverage for all areas.
* Add ANC prevalence and ART coverage to indicators outputs.  Note: currently this si implemented only for age 15-49 at the level of model fitting. This should enable comparisons with input data. Later this can be extended for higher level aggregations.

# naomi 0.0.32

* Add receiving ART output indicator

# naomi 0.0.31

* Fix validation bug caused by missing programme options

# naomi 0.0.30

* Undo ANC prevalence and ART coverage at time 2 which errors for different areas at two time points. Fix coming later.

# naomi 0.0.29

* Patch model validation for options$include_art_t1 = NULL.

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
