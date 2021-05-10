# naomi 2.3.14

* Add alternate example survey data using data from MDHS 2015-16 survey final report.

# naomi 2.3.13

* Option to output summary report in French.

# naomi 2.3.12
* Fix to summary report to display input ANC data in description.
* Ensure summary report pulls most recent calibration options.
* Fix `validate_calibration_options()` typo.
* Add `option$calibrate_method` back into the summary report.

# naomi 2.3.11

* Add PEPFAR PSNU ID mapping for MLI, SEN, LBR, GHA, SLE.

# naomi 2.3.10

* Add description text for ADR upload to model output metadata.

# naomi 2.3.9

* Add PEPFAR PSNU level for BFA to level 1.

# naomi 2.3.8

* Ensure that log interpolation is non-negative values only by inserting `pmax(..., 0)`
  in function `log_lin_approx()`. (NAOMI TROUBLESHOOTING #100)

# naomi 2.3.7

* When selecting ART data in `artnum_mf()`, if exact ART quarter is found in the 
  data, filter those data instead of interpolating. This is more efficient and 
  avoids interpolation error if only a single quarter of data are uploaded.

# naomi 2.3.6

* If ART data are not available at both times, do not estimate overall or district-level 
  change in ART coverage. Previously this was only restricted if there were no ART at 
  Time 2; not the case where ART at time 2 but not at time 1.
* It no sex-stratified ART data from any source, do not estimate difference between adult 
  female and male ART coverage.

# naomi 2.3.5

* Add PEPFAR Datim UID for Angola.

# naomi 2.3.4

* Hotfix: remove `option$calibrate_method` from summary report (naomi troubleshooting #76).

# naomi 2.3.3

* Update naomi summary report
* Switch to brio for file reading to avoid encoding issues on windows

# naomi 2.3.2

* Add PEPFAR Datim UID for Uganda Arua and Terego districts.

# naomi 2.3.1

* Set default for `calibrate_method` model option in user interface via JSON metadata.
* Fix swapped indicator_code and indicator_id for Data Pack metadata which were swapped.
* Refactor [`calibrate_outputs()`]; handle uncertainty ranges for proportion adjustments.

# naomi 2.3.0

* Implement 'logistic' scaling option for [`calibrate_outputs()`] such that estimates are
  adjusted on logistic scale by fine district/sex/age group to ensure proportions do 
  not go above 100%.
  - Note: implementation does not yet handle uncertainty ranges. Those might still go above
    100%.
* Fix to model calibration for number aware of status to align with proportion aware 
  of status and number unaware of status.


# naomi 2.2.5

* Add BWA, HTI, and COD to PEPFAR Data Pack PSNU list
* Fix Datim data element UID for indicator DIAGNOSED_SUBNAT.T_1.

# naomi 2.2.4

* hintr_calibrate will store calibration outputs to new files instead of overwriting existing

# naomi 2.2.3

* Add BEN to PEPFAR Data Pack PSNU list.

# naomi 2.2.2

* Fix summary report map colour scales 

# naomi 2.2.1

* Update French translation strings supplied by UNAIDS.

# naomi 2.2.0

* Implement survey data likelihood as flexible aggregation over areas / sex / age groups, 
  similar to likelihood for number on ART and ANC testing.
  
* Adjust flexibility of random effects specification based on granularity of available 
  survey data when using aggregate survey data.
  - If only age 15-49 data are available, do not estimate age effects; use Spectrum 
    odds ratio as offset pattern for all ages.
  - If only both sexes data are available, do not estimate district x sex interaction.
  
* Add _advanced_ model option `use_survey_aggregate` to select use of aggregate uploaded
  survey dataset.

* Reparameterise random effects to be scaled to the linear predictor, as implemented
  by INLA parameterisation. That is now: mu = beta0 + u_i; u_i ~ N(0, sigma) instead of 
  the previous parameterisation: mu = beta0 + u_i * sigma; u_i ~ N(0, 1). 
  
  This does not change the model at all, reduces the fitting time noticeably 
  because the starting values for the linear predictor for the 'inner optimisation' 
  step do not change resulting from steps in the hyperparameters. 
  
  Implementation for the BYM2 model follows the sparsity preserving conditional 
  parameterisation described by Riebler _et al._ in Section 3.4.

* Update PEPFAR Data Pack export:
  * Only return future projection outputs except for `art_current` for current estimates.
  * Manually code `art_current` at current estimates as indicator_code `TX_CURR_SUBNAT.R`.
  * Return number aware of status instead of proportion aware of status (DIAGNOSED_SUBNAT.T_1).
  
* Update CIV Datim UID mapping for new 113 health districts.

# naomi 2.1.14

* Add calibration options back in, reverts 2.1.8 changes

# naomi 2.1.13

* Report progress back from calibration

# naomi 2.1.12

* Update dependency first90 v1.4.2 which ensures backwards compatibility for 
  previous .shiny90 files.
  
# naomi 2.1.11

* Catch error if output package construction returns NA error when calculating 
  quantiles and return the REPORTed indicator that threw error (#180).
* Replace unaware_untreated_prop = 1.0 if ART coverage is 100% in a particular
  Spectrum age group (support ticket 11).

# naomi 2.1.10

* Fix new infections scaling. In plotting outputs number of new infections were being inadvertently scaled x100 (reported by Lesotho estimates team).

# naomi 2.1.9

* Increase eppasm dependency to 0.5.9.

# naomi 2.1.8

* Hotfix to move calibration options back to model run. Calibration is too slow to run synchronously and is meaning no other users can use the application whilst calibration is being run - we will disable async calibration in hintr.

# naomi 2.1.7

* In model options:
  * Select most recent household survey for prevalence based on survey_mid_calendar_quarter
  * Select most recent household survey for art_coverage if data exist
  * If ANC data exist in the most recent survey year [defaulted for prevalence], default select that year for Time 1 (both prevalence and ART coverage). If no data in that year, do not select default
  * If ANC data exist in the year 2020, preselect that for 'Time 2' in ANC clients, ANC prevalence, and ANC coverage.

# naomi 2.1.6

* Summary report logic error corrected
* Patch front end issue if anc_clients_year2 is selected and unselected, 
  resulting in `anc_clients_year2` passed to model.
* Patch issue arising if no ART data is uploaded and R gets confused 
  about an option not supplied due to autocompletion of list$name.

# naomi 2.1.5

* Summary report updated to handle model options from naomi web application
* Calibration options unscrambled in get_model_calibration_options() test

# naomi 2.1.4

* Add final number of iterations and duration to model fit progress message.

# naomi 2.1.3

### Awareness of HIV status

* If a .shiny90 file is included, the _first90_ package is used to calculate the 
  proportion of untreated PLHIV who are aware of their HIV status by sex and age
  group.
* To estimate the proportion aware of status at the district, this proportion 
  is applied to the estimated untreated population in each district. This is 
  added to the ART coverage to estimate the total proportion aware of status at
  district level by age and sex.

* New output indicators for awarness of HIV status:
  * `untreated_plhiv_num`: Number of untreated PLHIV (the 'treatment gap').
  * `aware_plhiv_prop`: Proportion of PLHIV aware of HIV positive status ('first 90' indicator).
  * `unaware_plhiv_num`: Number of PLHIV who are not aware of their HIV positive status.
  
* Add model option `output_aware_plhiv` in 'Advanced options'. If no `.shiny90` file is present 
  in the PJNZ, this must be set to `FALSE`. An error is raised by `validate_model_options()` 
  prompting this if no `.shiny90` file is found inside the PJNZ.


### PEPFAR Data Pack

* Update PEFPAR datapack export to conform to 2021 Data Pack specification.


### Additional changes

* Update demo datasets to distinguish clearly from Malawi. Rename files as `demo_` and
  affix `_demo` to the end of the area IDs.
* Add argument `quantile(..., names = FALSE)` in the funciton `add_stats()` for computation 
  of posterior quantiles. This gives a decent speedup in producing the outptus package.


# naomi 2.1.2

* Make running input validation during model run optional

# naomi 2.1.1

* Make 'Advanced Options' panel on model options page collapsible by default
* Add option for outputting awareness of HIV status in 'Advanced options'.
* Change "Population calibration options" to "Population calibration" for consistent with other block labels.

# naomi 2.1.0 

This release revises the modelling of ANC prevalence and ART coverage and produces
outputs for district-level ANC testing cascade.

### ANC testing cascade

* Add ANC bias parameters to age-specific prevalence and ART coverage regression equations instead of to aggregate ANC prevalence and ART coverage.

* ANC prevalence and ANC ART coverage outputs are produced by five-year age group, 15-24, 25-34, 35-49, and all ages.

* New output indicators for ANC testing cascade:
  * `anc_clients`: Number of ANC1 clients.
  * `anc_plhiv`: Number of HIV positive ANC attendees.
  * `anc_already_art`: Number of ANC clients already on ART prior to first ANC visit.
  * `anc_art_new`: Number of HIV positive ANC attendees initiating ART.
  * `anc_known_pos`: Number of HIV positive ANC attendees aware of HIV status prior to first ANC.
  * `anc_tested_pos`: Number of HIV positive ANC attendees tested HIV positive during ANC.
  * `anc_known_pos`: Number of HIV negative ANC attendees.
  
  Currently these indicators are calculated assuming that the number of 'known positive' ANC
  attendees are the same as the number already on ART.
  
* Added a likelihood for the number of ANC clients observed in current year as a 
  function of district population size, age-specific ferility rate (fixed inputs),
  and a district-level random effect to scale overall fertility rate.
  * The number of months reflected in ANC client reporting is used as an offset
    for the number of clients such that predicted number of clients are projected
	annual total.
  * Results explicitly represent __number of ANC clients__, calibrated based on the number 
    in the current year data. No distinction between number of births versus ANC clients
	are made. Results should not be used for estimating PMTCT coverage (for example).
  * Currently only a single time point is used for ANC clients estimates and projections.
    The future projection is closely linked to the accuracy of current year data; 
	uncertainty around the estimate and projection are not appropriately quantified.

* Age-specific fertility rate ratios (FRR) for HIV positive relative to HIV 
  negative pregnant women and women already on ART relative to untreated HIV 
  positive women are calculated from Spectrum model outputs (using EPP-ASM 
  simulation) and included in regression equations for ANC prevalence and ANC
  ART coverage, respectively.
  
* Aggregation of ANC testing data observations generalized using same approach as aggregation
  of number currently on ART observations. This means that:
  - ANC testing observations can be input for coarser areas, for example admin 1.
  - Age-stratified ANC testing data can be input, for example 5-year age groups.

  Age-stratified ANC testing do not yet inform national or district level estimates for 
  age-specific fertility or HIV/ART fertility rate ratios.
    
Additional changes:

* Use Kish effective sample size in likelihood for household survey data.

* Update summary report with additional styling, number formatting, and methods overview.

* Streamline function `extract_pjnz_naomi()` to reduce the number of calls to unzip and 
  read .DP file by getting all outputs from [`eppasm::read_hivproj_output()`] which was 
  utilised; remove dependency on `specio`.

# naomi 2.0.7

* Slight text edits to ANC model options block labels.
* Add `area_name` field to example datasets, conforming to ADR schemas.
* Remove survey data preparation functions. Added to `naomi.utils` package.

# naomi 2.0.6

* Updates to 'Model Options' page in Naomi application:
  * Update default 'current estimates' calendar quarter to December 2020.
  * Move 'short-term projections' calendar quarter to General options block.
  * Add drop down to select 'number of months' included in ANC testing data reporting.
  * Add select option to use Kish design effect in survey data likelihood.
  * Select most recent `survey_mid_calendar_quarter` as survey run option.
  
# naomi 2.0.5

* Report iteration number and elapsed time from model fit

# naomi 2.0.4

* Tidy model options layout and tooltip text

# naomi 2.0.3

* Remove utility functions for creating area datasets: `check_boundaries()`, `compare_boundaries()`, and `gather_areas()`.
* Add `area_name` field to survey indicators dataset prepared by `calc_survey_indicators()`.

# naomi 2.0.2

* Supply default `"none"` calibration options in `hintr_run_model()` if calibration options are not specified in the options list.

# naomi 2.0.1

* Remove calibration options from model run options

# naomi 2.0.0

Version 2.0 established for 2021 UNAIDS estimates. Changes are not guaranteed to be backward compatible with 2020 version of Naomi model.

* Update age group codes to `YXXX_XXX`.
* Chage `age_group_label` from 00-04 to 0-4 and 05-09 to 5-9.
* Remove `age_group_id`, `indicator_id`, and `quarter_id` column from output package.
* Rename columns in datasets for consistency with Fjelltop UNAIDS data inputs package:
  - ANC testing dataset: `ancrt_*` changed to `anc_*`.
  - ART programme data: `current_art` changed to `art_current` and `art_new` column added.
  - Survey HIV indicators: make several column names more human readable.
* Add Kish effective sample size approximation (`sum(weights) ^ 2 / sum(weights ^ 2)`) 
  to survey indicators dataset in field `n_eff_kish`.
* Harmonise indicator names in survey dataset with outputs (`prevalence`, `art_coverage`).
* Rename calculated ANC input indicators to `anc_prevalence` and `anc_art_coverage`.
* Harmonise ART number output indicators with ART input data indicators:
  - `art_num_attend` becomes `art_current`.
  - `art_num_residents` becomes `art_current_residents`.
  
Internal changes:

* Move several metadata tables to CSV tables saved in `inst/metadata/` rather than
  scripted functions.
  - `meta_age_group.csv` accessed by `get_age_groups()`.
  - `meta_indicator.csv` accessed by `get_meta_indicator()`.
  - Data Pack ID mapping tables: `datapack_indicator_mapping.csv`, 
    `datapack_agegroup_mapping.csv`, `datapack_sex_mapping.csv`.


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
