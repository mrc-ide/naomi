# naomi 1.0.11

* Add a basic vignette showing example script for running `hintr_run_model()` 
  for reproducing web app workflow.
  
# naomi 1.0.10

* Edit `summary_report.Rmd` to remove `age_group_id`.

# naomi 1.0.9

* Add summary_report_path to model run for saving summary report

# naomi 1.0.8

* Avoid segfault in `sample_tmb()` when redoing the sampling with different TMB
  configuration (different computer/number of set threads) by recreating the
  pointers if they are missing using `obj$retape()`.

# naomi 1.0.7

* Return indicator formatting metadata in `get_metadata()` and `get_plotting_metadata()`

# naomi 1.0.6

* Import sparse matrix multiplication `%*%` from `Matrix` package.

# naomi 1.0.5

* Add `hintr_calibrate()` which takes output and calibration options and runs
  calibration and returns in hintr output format

# naomi 1.0.4
* Rename summary_path in model run args to coarse_output_path to be more
  representative of actual output (in preparation for adding summary report)
* Add `get_calibration_options()` to retrieve options for calibration

# naomi 1.0.2

* Patch to `sample_tmb()` for R 4.0 changes.

# naomi 1.0.0

* Move tmbstan and INLA models to naomi.extensions repo as they add heavyweight 
  dependencies which are not required for the main model to run

# naomi 0.0.72

* Added function `create_adj_matrix()` to create an adjacency matrix from a shape file.
* Added function `scale_gmrf_precision()` to scale precision matrix such that geometric
  mean of marginal variance is one. This mirrors the behaviour of `INLA::inla.scale.model()`.
* Remove dependency on INLA package. Now prompted to install if `fit_inla()` is 
  called and the package is not found.

# naomi 0.0.71

* Use `rdhs::download_boundaries()` in script `data-raw/survey.R`. (No change to model or datasets.)

# naomi 0.0.70

* Use summary output download function to download a Naomi output package with 
  coarse age groups only: 15-49, 15-64, 15+, 50+, 00+, 00-64, 00-14, 15-24, 
  25-34, 35-49, 50-64, 65+.

# naomi 0.0.69 

* Add functions `subset_output_package()` to subset and re-save a zipped Naomi output package.

# naomi 0.0.68

* Pass some metadata back from model run for use in output formatting

# naomi 0.0.67

* Allow passing input data to `naomi_run_model` either as list of file paths or list of lists each containing a path, hash and filename

# naomi 0.0.66

* Allow fitting to ART data to aggregated areas. There is a check that the same model area is not included in more than on area in the ART dataset.
* Use logit offsets for ages below which survey data are available. This is principally used for age 0-14 when survey data start at age 15.
* Add option `rho_paed_15to49f_ratio` to calculate paediatric prevalence as ratio of female 15-49 adult prevalence.
* Recode string is passed by JSON for ANC testing data year as NULL.


# naomi 0.0.65

* Fix error in default option for time-varying ART attendance model option in 0.0.64 release.

# naomi 0.0.64

* Area random effect for prevalence below age 15 interaction as ICAR model. Implemented as option `rho_paed_x_term` specified by argument `naomi_model_frame(..., rho_paed_x_term = TRUE, ...)`. Defaualt value is `FALSE`.
* Initialize hyperparameters at prior mode.
* Add calibration option for new infections to Spectrum new infections.
* Update default Spectrum calibration option to "national".
* Update default time-varying ART attendance "yes".

# naomi 0.0.63 

* Change age_group_label for ages 0-4 and 5-9 to 00-04 and 05-09 for Data Pack.

# naomi 0.0.62

* Better error messages with pjnz/geojson mismatch

# naomi 0.0.61

* Add country-specific colour scales for WCA countries.

# noami 0.0.59

* Accept semicolon delimited files

# noami 0.0.58

* Small fix to tests: model options.

# naomi 0.0.57

Formatting updates requested for Data Pack 2019:

* Change age_group_label for ages <1 and 1-4 to <01 and 01-04.
* Change dataelement UIDs for TX_CURR_SUBNAT and PLHIV.

# naomi 0.0.56

* Replace `survey_year` with `survey_mid_calendar_quarter` in the survey indicators dataset.
* Add column `restype` for urban/rural to survey indicators datasets and option to stratify survey indicators calculation by urban/rural.

# naomi 0.0.55

* Option for time-varying ART attendance odds ratio. The option `artattend_t2` can be set to TRUE if there is evidence that patterns of cross-district ART attendance changed for the period before the survey and the inter-survey period.

# naomi 0.0.54

* Add output stratification for age groups <1, 1-4.  This is simply disaggregated based on proportions from Spectrum with now uncertainty.
* Add function `export_datapack()` to export an output package to datapack CSV format.

# naomi 0.0.53

* Do not calculate reported output aggregations during model fitting. This should modestly reduce model objective function computation time.
* Output projection to a third calendar quarter. Projection outputs are computed after objective function calcuation. Predicted ANC attendance at time 3 is not computed.

# naomi 0.0.52

* Automatically remove rows with all NA values in `read_*()` functions.
* Remove extraneous argument area_merged from output.

# naomi 0.0.51

* Add optional approximate design effect to scale effective sample size for survey prevalence, art coverage, and proportion recent observations.

# naomi 0.0.50

* Add spatial interaction to ART coverage model for <15 / 15+ to allow different paediatric vs. adult ART coverage and different change in paediatric coverage. This reduces paediatric ART data distorting adult model results.
* ART number dataset accepts either year or calendar_quarter column.
* Linearly interpolate number on ART for model fitting. If desired quarter is before earliest data, the earliest ART number may be carried backward by up to four quarters. Number on ART are never carried forwards.

# naomi 0.0.49

* Properly age new infections between time 1 and time 2 based on ageing and incidecne trend from Spectrum.
* Add new paediatric HIV infections betwen time 1 and time 2 proportional to HIV survivors by age and prevalence among women age 15-49 in Spectrum.

# naomi 0.0.48

* Add default values for area level and calendar quarter to generate estiamtes for

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
