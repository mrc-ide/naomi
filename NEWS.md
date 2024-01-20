# naomi 2.10.0

* Implement survey data at T1 and T2.


# naomi 2.9.19

* Add optional `vmmc_path` to Spectrum download function for path to DMMPT2 output Excel file.
* Add DMMPT2 outputs to PEPFAR Datapack CSV download, if DMMPT2 output file exists.
* Silence `spdep v1.2-8` warnings from `mat2listw()` (issue #421).

# naomi 2.9.18

* Change PEPFAR Data Pack file name to `pepfar_datapack_indicators_2024.csv`.

# naomi 2.9.17

* Bump version of naomi.options v1.2.0 to include defaults for 2024 HIV estimates
  - Extends calendar quarter model options drop downs from `December 2025` to `December 2027`
  - Updates T2, T3, T4, and T5 defaults all 1-year ahead
  - Updates default 'current' ANC input year to 2023
  - Updates TZA defaults to 2022 PHIA survey

# naomi 2.9.16

* Change programme data aggregation scripts to aggregate and tag missing data. Changes in the outputs of the following functions:
   - `aggregate_art()` and `aggregate_anc()`:  Data aggregated retaining missing values at lowest admin level and summed totals at higher admin levels.
   - `prepare_input_time_series_art()` and `prepare_input_time_series_anc()`: New column containing a list of area_ids corresponding to missing districts included in aggreagated totals. 

# naomi 2.9.15

* Add placeholder function `hintr_prepare_agyw_download` for creating AGYW tool.

# naomi 2.9.14

* Fix scrambled translation keys in summary report.

# naomi 2.9.13

* Ensure duckdb connection is readonly.

# naomi 2.9.12

* Automatically set T4 and T5 to 24- and 36-months ahead of T3 if not specified in model options.

# naomi 2.9.11

* hintr data frame outputs can now be saved as a duckdb database.

# naomi 2.9.10

* Update PJNZ extraction for adult ART need Dec 31 for 2023 PJNZ files. Previously child ART was
  note recorded in the .DP file tag `<NeedARTDec31 MV>`, and so it was fine to extract the total
  value. Now child ART is recorded, and so need to sum the adult age groups only.

# naomi 2.9.9

* Add indicators `aware_plhiv_attend` and `unaware_plhiv_attend` for consistent facility attendance cascades.

# naomi 2.9.8

* Display PLHIV number indicators to nearest 1.
* Drop input time series indicator "ART proportion <15" nd "ART adult-to-child ratio" in favour of "ART child-to-adult ratio" indicator.

# naomi 2.9.7

* Change default PEPFAR PSNU level for Namibia to level 1 (Region / Health Province).

# naomi 2.9.6

* Fix for comparison report failing to render when ANC data included at T1 in a different year to model T1. This fix will prevent ANC comparison plots for T1 from rendering in this case. 

# naomi 2.9.5

* Fix translation of output descriptions which are used as the resource description for files uploaded to the ADR.

# naomi 2.9.4

* Implement Spectrum ART scaling adjustment in Naomi calibration.
* Add PEPFAR area ID mapping for Ethiopia.

# naomi 2.9.3

* Update PEPFAR Datim UID mapping for CMR.

# naomi 2.9.2

* INTERNAL ONLY: Update code to silence warning messages from dplyr v1.1.0. No changes to model or interface.

# naomi 2.9.1

* Update PEPFAR Datim UID mapping and PSNU level for 2023 Target Setting Tool.

# naomi 2.9.0

* Add projection to T4 and T5 for PEPFAR COP projections. Only outputs plhiv, plhiv_attend, and infections, and, for T4, incidence.

* Update indicators and age groups for PEPFAR Data Pack export to 2023 specification.

* Change PEPFAR Data Pack file name to `pepfar_datapack_indicators_2023.csv`.

* Add higher area levels to PEPFAR Datim ID mapping table to enable alternate PSNU level selection.

* Add advanced model option to select PEPFAR PSNU level to produce Data Pack output. Defaults set to current choices.

# naomi 2.8.17

* Patch NULL checking code in `summary_report.Rmd` and `comparison_report.Rmd`.

# naomi 2.8.16

* Don't include ART plots in comparison report download when model fit with an ART calendar quarter different to t1 (issue 41).

# naomi 2.8.15

* Don't include prevalence plots in comparison report download when model fit with survey aggregate data (issue 36).

# naomi 2.8.14

* Show comparison barchart ratios to 1 decimal point accuracy.

# naomi 2.8.13

* Generate comparison report where no survey ART coverage is available
* Consistent age/sex descriptions in comparison plot titles.

# naomi 2.8.12

* Handle case when multiple .shiny90 files found in .PJNZ file. Choose the file with the shortest file name. This will most likely be the file that Spectrum fitter has saved.

# naomi 2.8.11

* Do not attempt to extract .shiny90 data if option `output_aware_plhiv` = "No".

# naomi 2.8.10

* Don't raise warning for not selecting ART attending when ART data is excluded at T1 and T2

# naomi 2.8.9

* Fix comparison bar plot error bars and filters.

# naomi 2.8.8

* Update bug in `art_spectrum_warning()` and `anc_spectrum_warning()` where district totals in naomi were compared to national totals in spectrum.
* Changes warning to compare district totals as opposed to district age sex totals to avoid excluding comparisons when age/sex dissagreagtes are different in spectrum and naomi data.

# naomi 2.8.7

* Show ANC prevalence on ANC time series to 1 decimal place instead of 2. It will now be consistent with prevalence in other plots.

# naomi 2.8.6

* `hintr_run_model` data can include a `resource_url` to include a static link to the original resource.

# naomi 2.8.5

No user-visible changes, only internal.

* Update `extract_shiny90_age_sex()` to extract awareness of status estimates saved with Spectrum internal Shiny90 implementation (Spectrum v6.21).
* Added test file with internally created .shiny90 file: `extdata/demo_mwi2019_v6.21-shiny90.PJNZ`

# naomi 2.8.4

* Make comparison report handle cases where the user selects only 1 survey for prevalence.

# naomi 2.8.3

* Update model fit to fallback to `anchor_home_district = TRUE` if option is not set.

# naomi 2.8.2

* Add model option to specify a new structure for ART attending random effects. In this change, the random effect for the home district applies to residents in that district (and in the neighbouring districts). This means that, for example, if a district is "attractive" to residents of neighbouring districts, it will also be more attractive to residents in that district to stay there, rather than go to a neighbouring district. In the previous specification of the model, the ART attending district parameter only applied to residents of neighbouring districts.
* This is now the default model specification with `options$anchor_home_district = TRUE`
* The model can be reverted to the previous specification with ``options$anchor_home_district = FALSE`

# naomi 2.8.1

* Fix `aggregate_art()` to aggregate ART separately when provide at different admin levels. Previous behaviour was to aggregate from lowest level provided.
* Add tests and adjusts aggregation scripts to account for missing ART and ANC data inputs:
  - Data provided at multiple levels for the same years
  - Data provided at more than one level for different years
  - Data missing for some sub-national units at one time point
  
# naomi 2.8.0

* Save model and calibration output from `hintr_run_model` and `hintr_calibrate` as qs files for faster reading. See https://github.com/traversc/qs.


# naomi 2.7.19

* Vectorise `quarter_id_to_calendar_quarter` conversion functions.
* Load `get_metadata` into memory on first retrieval for quicker repeated access from other packages.


# naomi 2.7.18

Update for Spectrum v6.2 with calendar year projection period instead of mid-year 
projection.

* Requires `eppasm >= 0.7.0` and `first90 >= 1.6.1` which incorporate code updates 
  for calendar year projection.
* Add demo PJNZ files saved in Spectrum 6.2 beta 29.
* For Spectrum inputs parsed by function `extract_pjnz_naomi()`, add a column
  specifying to which calendar quarter the outputs correspond. This enables
  Naomi model compatibility with either mid-year projection (Spectrum version <=6.19)
  or calendar year projections (Spectrum version >=6.2).
  
* Update `read_dp_anc_testing()` for new ANC testing data input tag in .DP file `"<ANCTestingValues MV4>"`.

# naomi 2.7.17

* Error if trying to generate comparison report with old model output

# naomi 2.7.16

* Add function to generate html report comparing naomi model estimates with input data:
  - The comparison report is generated with the function `generate_comparison_report()` that requires a naomi output file saved as an .Rds or output zip file created by `hintr_run_model()`
  - The report calls exported plotting functions that will generate interactive plotly figures from the `inputs_outputs` dataset in the naomi outputs including `bar_plotly()`, `age_bar_plotly()` and `scatter_plotly`.

# naomi 2.7.15

* Add and known negative and births facility inicators to input time series

# naomi 2.7.14

* Fix comparison barchart ANC age matched indicators, these were pointing to the wrong indicator data.

# naomi 2.7.13

* Add input time series indicator showing ratio of ANC births to ANC clients

# naomi 2.7.12

* Fix name and tooltip of `art_adult_child_ratio` indicator. This is ratio of adults to children not paediatric sex ratio.

# naomi 2.7.11

* Remove `get_calibration_option_labels()`, `get_model_calibration_options()` and `get_model_options_template()`. Model options specification has been refactored into naomi.options package.

# naomi 2.7.10

* Subset model options in `naomi_fit` object do not contain advanced options. Updates `read_output_package()` function to read in full model options when available in the model output zip
* Adds README to output zip describing outputs zip files
* Add backwards compatibility for old versions of coarse output zip

# naomi 2.7.9

* Add country name and ISO3 code to `extract_pjnz_one()`.

# naomi 2.7.8

* Add indicator `plhiv_attending` reporting the estimated denominator for `art_current` based on assuming same ART attendance probabilities for untreated population as the treated population. This give estimates for the predicted number of PLHIV ‘in the ART catchment’ for a district and the number of untreated PLHIV in the catchment. This is an effort to address the recurring issue that the TX_CURR value that is provided to Data Pack does not correspond to the district PLHIV denominator, and to provide an output more directly useful for setting a district treatment targets.

# naomi 2.7.7

* Report log likelihood in model fit object.

# naomi 2.7.6

* `hintr_comparison_plot()` will error if comparison plot data does not exist.
  This will avoid a class of obscure errors being shown to end user.

# naomi 2.7.5

* Adjust incidence projection for changing incidence trend between T1 and T2.

  Previously, new infections by age were calculated as:
    `(1 - exp(-lambda_t1 * duration)) * hiv_negative_t1`.
  This implicitly assumed that incidence rate and HIV negative population size
  were constant over the period from T1 to T2. This was reasonably okay for
  short periods, but less acceptable when time from T1 to T2 becomes many years.
  In settings with declining infections, this approach will tend to over-estimate
  the number of PLHIV at T2.

  New approach calculates infections in one year by age and sex based on `lambda_t1`
  and then extrapolates to the number of infections between T1 and T2 based on
  the ratio of the number of infections occurring in Spectrum between T1 and T2
  to the number of infections in the year preceding T1 (Spectrum definition for
  infections). This calculation also accounts for the number progressing from the
  age group at infection to the age group at T2 based on Spectrum infections by
  cohort (same as previous implementation).

  The model accounts for impact of different ART coverage levels at T1 on
  district-level transmission rate and incidence, but does not account for
  differential change in ART coverage between T1 and T2. That is, a larger scale-up
  in ART in district A than district B between T1 and T2 would not result in a
  larger relative decline in incidence in A than B during this period.

  The model does not explicitly account for mortality among infections between
  T1 and T2, but this is implicitly handled how new infections are subtracted
  away from the cohort survival probabilities for PLHIV at baseline.

  Better handling of incidence changes over time and mortality probably
  need a more complete simulation model rather than the single time jump
  approximation.

* Related to the previous change:
  - The age-specific incidence rate for distributing   - The age-specific incidence
    rate for distributingnew infections by age (`spec_incid_t1`, etc. in the result
    of `naomi_model_frame()`) uses *current year* HIV negative population as
    denominator instead of *previous year* HIV negative population as denominator
    (used by Spectrum). This is for consistency with current time population
    denominator used in internal incidence simulation.
  - Infections are gradauted to quarters over _periods_ rather than by _cohort_
    such that all infections remain within the same age as they are reported
    in single-age Spectrum outputs. Graduation is done via monotonic (Hyman)
    smoothing on cumulative infections.

* Handle adjacency matrix construction for case with single area.

# naomi 2.7.4

* Re-fetch meta data when output zip is generated so that output zip contents in `indicators.csv` and `meta_*.csv` are in the language selected by the user at the time of generation.

# naomi 2.7.3

* Patch `aggregate_anc()` and `aggregate_art()`: select required columns when joining to prevent inadvertent column name clash due to extra columns (e.g. `area_level`).

# naomi 2.7.2

* Restructure functions that subset model input data based on logic in model options:
 - In `select_naomi_data()`: Provide an annotated version of all input data provided with data tagged as `raw_included`, `raw_excluded` and `interpolated_inlcuded`. Individual data frames for survey, ART and ANC inputs are added to `naomi-data` in a list named `full_data`.
  - In `output_package()`: Create a long dataframe containing matched data inputs and model outputs with `align_inputs_output()`. This adds a new object to the naomi output package that may used to compare model estimates and data inputs for T1 and T2.

# naomi 2.7.1

* If `births_facility` in ANC data is missing it will be replaced by `NA` when reading input so input time series aggregation passes

# naomi 2.7.0

Updates for 2023 UNAIDS estimates (Dec 2022 - Mar 2023).

* Add fields to ANC routine testing data specification and example data sets:
  * `anc_known_neg`: Number of women who were not tested for HIV at antenatal visit because they had a recent documented HIV negative test.
     - This is only recorded by some countries HMIS. The column may be missing or blank if this is not captured within national ANC testing guidelines and reporting.
     - If the column is missing or `NA` in data input, it will be replaced by values `0` in reading input.
     - Value `anc_known_neg` is added to denominator for calcuating `anc_prevalence`.
  * `births_facility`: The number of live births recorded at health facilities. This is added for triangulation with trend in number of ANC visits for data review purposes. Currently not used in modelling.

# naomi 2.6.28

* Patch for incidence input argument in EPP-ASM simulation for `eppasm_v0.6.2`.

# naomi 2.6.27

* Move ANC and ART spectrum data mismatch warnings to be shown on model calibrate and review result pages

# naomi 2.6.26

* `hintr_prepare_spectrum_download` now takes arg `notes` for arbitrary notes which will be added into the output zip

# naomi 2.6.25

* Fix anc aggregation
* Add tests for aggregation and time series inouts scripts to ensure unique strata

# naomi 2.6.24

* Add `hintr_prepare_comparison_report_download` function to generate comparison summary report

# naomi 2.6.23

* Add area hierarchy into time series plot data

# naomi 2.6.22

* Ignore non PJNZ files when listing contents of uploaded zip, to stop naomi trying to read from __MACOSX folder that macs add to zip files

# naomi 2.6.21

* Add warning for spectrum totals that do not match aggregated district ART + ANC data

# naomi 2.6.19

* Specify incidence per 1000 outputs to be displayed with one decimal place.

# naomi 2.6.18

* Bug fix: calibration options read into summary report from `outputs` object
* Add test to ensure calibration options are read into summary report correctly if present

# naomi 2.6.17

* Add PEPFAR Datim PSNU UID for Benin.
* Change default population calibration option selection to "Subnational". If a national file is loaded, it will simply calibrate the national file.

# naomi 2.6.16

* Bug fix: remove duplicate addition of CLHIV in T2 projection arising from paediatric incidence. This was introduced in v2.6.0 when paediatric new infections were added to the outputs.

# naomi 2.6.15

* Add PEPFAR Datim UID for Burundi.

# naomi 2.6.14

* Add option for district-sex-time interaction for ART coverage. This is implemented by argument `naomi_model_frame(..., alpha_xst_term = TRUE, ...)`.
  The default is currently `alpha_xst_term = FALSE` for backwards compatibility. If set to `TRUE`, the model also checks that sex-stratified ART data
  are included at both T1 and T2.

* Add term to PLHIV projection from T1 to T2 and T2 to T3 to account for district-level net migration. The term is the ratio of the change in the cohort population at district level divided by the national level cohort change. The national level cohort change from Spectrum accounts for mortality and international net migration.

  Applying the ratio is controlled by the argument `adjust_area_growth = TRUE` to `naomi_model_frame()`.

  __This is currently set as `FALSE` by default.__ Pending further testing in problematic cases.

  Limitations of this approach: (1) It does not explicitly account for migration of PLHIV between districts. Therefore, it may 'create' or 'dissolve' infections if net population growth is greater in high prevalence districts (and vice versa). This is consistent with Spectrum and EPP handling of migration, but implications could be larger for smaller subnational areas. (2) In some cases, these net migration ratios mask unrealistic demographic assumptions in subnational population data (rather than true net migration patterns).

_Internal changes_

* Refactor function `create_Lproj()` to avoid replicating same code for T1 -> T2 and T2 -> T3 projection.
* Remove conversion of `sf` to `sp` object for `spdep::poly2nb()`, which now supports `sf` objects from v1.0.
* Replace "ART" in French and Portuguese strings with "TARV".

# naomi 2.6.13

* The model uses offsets from Spectrum to determine HIV prevalence in age groups outside the age range for which survey HIV prevalence are available. Stratify determination of offset age ranges by sex. This addresses situation for DHS surveys where maximum female age is 49 but maximum male age is 54 or 59.

* Set default to model paediatric prevalence relative to adult female age 15-49 prevalence instead of using offsets relative to age 15-19 prevalence. This will (1) ensure same prevalence for male and female children, and (2) reduce distortions to age 15-19 prevalence arising from paediatric ART data.

* Expose advanced option to allow paediatric to adult prevalence ratio to vary by district (option `rho_paed_x_term`).

* Throw validation error if time-varying ART attendance is selected, but ART data are not included at both Time 1 and Time 2.

# naomi 2.6.12

* Do not save the PEPFAR Data Pack CSV for coarse age output package because the five-year age group results are not included.

# naomi 2.6.11

* Revert plot ordering change made in 2.6.9 in favour of changing the default selected to HIV prevalence from hintr.

# naomi 2.6.10

* MOZ `area_id` in `inst/datapack/datapack_psnu_area_id_map.csv` to new area hierarchy with separate Maputo provinces at admin-2.

# naomi 2.6.9

* Switch plot ordering to show HIV prevalence first.
* Patch logic error in calibration table: use calibrated population (done pre model fitting) as denominator for raw prevalence denominator (troublshooting #2022-73)

# naomi 2.6.8

* Require `eppasm` v0.5.12 to avoid divide-by-zero in model computations (troubleshooting #2022-51).

# naomi 2.6.7

* Increase threshold to trigger warning for HIV prevalence in naomi outputs.
* Remove non-outputted indicators from `meta_indicator` table in the output package.
* Add `anc_tested` and `anc_tested_pos` to anc time series plot.

# naomi 2.6.6

* Add new infections output to PEPFAR data pack export.

# naomi 2.6.5

* Add HIV incidence rate output to PEPFAR datapack export.
* Add check `Cal_Population` for population calibration in UNAIDS Navigator checklist file.

# naomi 2.6.4

* Bug fix: UNAIDS navigator checklist was not checking calibration stratification `= "sex_age_coarse"` for any indicators. Add this to check.
* Return input time series data in `area_sort_order` order.

# naomi 2.6.3

* Adds helper function `output_naomi_warning()` to report specific area/age/sex disaggregates that are triggering warnings in simulated outputs.

# naomi 2.6.2

* Makes file reading utilities more robust replacing `readr::col_double()` with `readr::col_number()`

# naomi 2.6.1

* Adds output age group 10-19 and 25-49 years.

* Update TZA `area_id` in `inst/datapack/datapack_psnu_area_id_map.csv` to new area hierarchy with Zone level added.

# naomi 2.6.0

* In construction of survivorship projection matrices, the method for interpolating annual population totals to quarter-year age group totals was revised to use Hyman monotonic interpolation of population counts for smoother population distributions.

* Paediatric new infections (MTCT) are distributed to districts based on the ratio of paediatric incidence rate to adult 15-49 female HIV prevalence. This implicitly assumes the same mother-to-child transmission rate for all districts; results should be interpreted accordingly.

* Spectrum inputs for number on ART at end of year (Dec 31) for age 0-14, male 15+, and female 15+ are read from the PJNZ and used for model calibration. As a result, if calibration to Spectrum outputs is applied, Q4 outputs for `art_current` will match exactly to Spectrum. ART coverage will not be exactly the same because Spectrum uses mid-year PLHIV for the denominator and Naomi uses end-year PLHIV for the denominator.

_Internal changes_

* Added R version of the Naomi model in function [`naomi_objective_function_r()`] for stepping through model line by line.
  - `REPORT()` values match C++ code exactly.
  - AR1 likelihood is not yet implemented so objective function value does not match the C++ version exactly.

# naomi 2.5.21

* Patch to `summary_report.Rmd` to avoid spherical geometry aggregation error (troubleshooting #2022-43).

# naomi 2.5.20

* Implement Navigator checks.

# naomi 2.5.19

* Use standard file reading functions in input time series (troubleshooting #2022-41).

# naomi 2.5.18

* Update EPP-ASM dependency to v0.5.11 with patch for reading version of .DP file (troubleshooting #2022-42).

# naomi 2.5.17

* Update first90 package dependency to v1.5.1 for 2022 estimates.
* Add UNAIDS Estimates Navigator checklist CSV to output package .zip.

# naomi 2.5.16

* Update colour scales to match summary report and user feedback

# naomi 2.5.15

* Suppress s2 geometry in adjacency matrix construction.

# naomi 2.5.14

* Add `naomi_warning` helper to raise warnings which can be displayed in front end
* Return warnings from model fit, model calibration and model option validation
* Remove `permissive` option from model options, warning now returned instead of error

# naomi 2.5.13

* Replace `get_plot_type_label_and_description` with `get_plot_type_column_metadata` and include formatting info in response

# naomi 2.5.12

* Small fixes to text in Naomi summary report

# naomi 2.5.11

* Update defaults for current estimates period to `CY2021Q4` (December 2021) and short-term projection quarter to `CY2022Q3` (September 2022).

# naomi 2.5.10

* Change viral load indicators to `vl_tested_12mos` and `vl_suppressed_12mos` to specify that indicators are reported for the previous 12 months even if reporting period is quarterly.

# naomi 2.5.9

* Fix to allow `prepare_input_time_series_art()` to accept data without art_new and vls indicators provided

# naomi 2.5.8

* Add _simulated_ viral load testing data to example datasets.


# naomi 2.5.7

* Throw an error from `calibrate_outputs()` if user tries to calibrate an ouptut package that has
  already been calibrated. This is determined by whether the `output$spectrum_calibration` table
  exists in the output object. _In future, it would be nicer to allow user to re-calibrate an output
  package. This will require saving additional information to un-calibrate and re-calibrate._

* Save calibrated count outputs at Spectrum region level in the `spectrum_calibration.csv` outputs.

* Add function `hintr_calibrate_plot` to return data for plotting calibrate barchart.

# naomi 2.5.5

* Add argument `na.rm=` to `output_package()` to allow calculation of quantiles if there are missing values in the simulation. Default is `na.rm = FALSE` and `na.rm = TRUE` is to be used for debugging purposes only. Cases where missing values occur will usually indicate very poor model fits and issues that need to be addressed.

# naomi 2.5.4

Updates to PEPFAR Data Pack outputs.

* Add age groups 50-54, 55-59, 60-64, 65+.
* Edit format of PEPFAR data pack CSV:
  - Add column `dataelement_uid`.
  - Add `=""<>""` around age group values.
* Change file name save to `pepfar_datapack_indicators_2022.csv`.
* Separate input data aggregation and plot preparation scripts
* Remove `time_step` and add `year`, `quarter` and `calendar_quarter` in input
time series function outputs

# naomi 2.5.3

* Include `parent_area_id` and `area_sort_order` in input time series function outputs

# naomi 2.5.2

* Add function `get_plot_type_label_and_description` to get label and description for each input time series plot types.

# naomi 2.5.1

* Add functions `prepare_input_time_series_art` and `prepare_input_time_series_anc` for returning ART and ANC data formatted for plotting input time series graphs.

# naomi 2.5.0

* Add functions `hintr_prepare_spectrum_download`, `hintr_prepare_coarse_age_group_download` and `hintr_prepare_summary_report_download` for generating downloads from `hintr_output` object
* Refactor `hintr_run_model` and `hintr_calibrate` to save out minimal data

# naomi 2.4.2

* Add Portuguese translations
* Move summary plot functions into naomi package

# naomi 2.4.1

* Manually skip 'PSNU not found warning' in `write_datapack_csv()` when running `hintr_run_model()` with zone-level demo data. This occurs because the demo data have ISO3 = `MWI` for which the PSNU level is 3, but demo zone-level model fits are only applied to levels 0:2. A better solution would be to change the ISO3 for the demo data to something artificial, but this requires relaxing some validation in the user interface first.

# naomi 2.4.0

* Add demo datasets for subnational Spectrum files for three Malawi regions (Northern, Central, Southern).
* Add zone-level demo datasets population, ART, and ANC testing datasets.

# naomi 2.3.15

* Updates to summary report

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

* Add PEPFAR Datim UID for Angola.<

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
