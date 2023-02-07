#' Graduate HIV population to quarter-year age using Hyman monotonic
#' spline interpolation. 80+ open-ended age group is extended to age 100.
#'
#' @noRd
#'
graduate_mono <- function(y, x, xout) {
  xout <- c(xout, 2*xout[length(xout)] - xout[length(xout)-1])
  diff(stats::splinefun(x, c(0, cumsum(y)), method = "hyman")(xout))
}

create_Lproj <- function(spec, mf_model,
                         quarter_id1, quarter_id2,
                         population_colname1, population_colname2,
                         adjust_area_growth = TRUE) {

  ## Remove paediatric infections; these are not handled through the incidence, so should
  ## not be subtracted when calculating PLHIV survivorship.
  spec$infections[spec$age < 10] <- 0.0

  spec_quarter <- spec %>%
    dplyr::mutate(quarter_id = convert_quarter_id(year, 2L),
                  year = NULL) %>%
    dplyr::filter(dplyr::between(quarter_id, quarter_id1 - 8, quarter_id2 + 8)) %>%
    dplyr::arrange(spectrum_region_code, quarter_id, sex, age) %>%
    dplyr::reframe(
      age_quarter = 0:(400-1),
      dplyr::across(c(totpop, hivpop, artpop, infections), graduate_mono, c(age, 100), age_quarter/4),
      .by = c(spectrum_region_code, quarter_id, sex)
    )

  ## Log-linear interpolate mid-year population to T1 and T2
  hivpop <- tidyr::expand(spec_quarter,
                          tidyr::nesting(spectrum_region_code, sex, age_quarter),
                          quarter_id = c(quarter_id1, quarter_id2)) %>%
    dplyr::full_join(spec_quarter, by = names(.)) %>%
    dplyr::group_by(spectrum_region_code, sex, age_quarter) %>%
    dplyr::mutate(
      hivpop = exp(zoo::na.approx(log(hivpop), quarter_id, na.rm = FALSE)),
      hivpop = tidyr::replace_na(hivpop, 0),
      totpop = exp(zoo::na.approx(log(totpop), quarter_id, na.rm = FALSE)),
      totpop = tidyr::replace_na(totpop, 0)
    ) %>%
    dplyr::filter(quarter_id %in% c(quarter_id1, quarter_id2)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cohort_quarter = quarter_id - age_quarter,
                  age_group = age_quarter_to_age_group(age_quarter),
                  age_group1 = age_quarter_to_age_group(quarter_id1 - cohort_quarter),
                  age_group2 = age_quarter_to_age_group(quarter_id2 - cohort_quarter))

  ## Graduate infections over period quarters within each age
  infections_cohort <- spec_quarter %>%
    dplyr::arrange(spectrum_region_code, sex, age_quarter, quarter_id) %>%
    dplyr::reframe(
      quarter_id_out = seq.int(quarter_id1 - 4, quarter_id2 + 3),
      infections = graduate_mono(infections, c(min(quarter_id)-4, quarter_id), quarter_id_out),
      .by = c(spectrum_region_code, sex, age_quarter)
    ) %>%
    dplyr::rename(quarter_id = quarter_id_out) %>%
    dplyr::mutate(cohort_quarter = quarter_id - age_quarter)


  ## Aggregate infections according to age group at T1, age group at T2 aand age at infection
  infections_cohort <- infections_cohort %>%
    dplyr::filter(quarter_id >= quarter_id1 - 4,
                  quarter_id < quarter_id2 + 4) %>%
    dplyr::mutate(age_group1 = age_quarter_to_age_group(quarter_id1 - cohort_quarter),
                  age_group2 = age_quarter_to_age_group(quarter_id2 - cohort_quarter),
                  age_group_infection = age_quarter_to_age_group(age_quarter)) %>%
    dplyr::count(spectrum_region_code, sex, quarter_id, age_group1, age_group2,
                 age_group_infection,
                 wt = infections, name = "infections")


  ## Expand area-level populations to long format
  totpop_area <- mf_model %>%
    dplyr::select(area_id, spectrum_region_code, sex, age_group,
                  dplyr::all_of(c(population_colname1, population_colname2))) %>%
    tidyr::pivot_longer(dplyr::all_of(c(population_colname1, population_colname2)),
                        values_to = "population") %>%
    dplyr::mutate(
      quarter_id = dplyr::case_when(name == population_colname1 ~ quarter_id1,
                                    name == population_colname2 ~ quarter_id2),
      name = NULL
    )


  ## Graduate 5-year age group population to quarter-year age group population
  ## use hyman smoothing on cumulative population.

  totpop_area <- totpop_area %>%
    dplyr::left_join(
      dplyr::select(get_age_groups(), age_group, age_group_start),
      by = "age_group"
    ) %>%
    dplyr::reframe(
      age_quarter = 0:(400-1),
      population = graduate_mono(population, c(age_group_start, 100), age_quarter/4),
      .by = c(area_id, spectrum_region_code, sex, quarter_id)
    ) %>%
    dplyr::left_join(
      ## Merge age groups
      dplyr::select(hivpop, spectrum_region_code, sex, quarter_id,
                    age_quarter, age_group, age_group1, age_group2),
      by = c("spectrum_region_code", "sex", "quarter_id", "age_quarter")
    )


  ## Construct Lproj for t1 to t2

  hivpop_t1 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id1) %>%
    dplyr::count(spectrum_region_code, sex, age_group1,
                 wt = hivpop, name = "hivpop1")

  hivpop_t2 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id2) %>%
    dplyr::count(spectrum_region_code, sex, age_group1, age_group2,
                 wt = hivpop, name = "hivpop2")

  infections_t1t2 <- infections_cohort %>%
    dplyr::filter(
      quarter_id >= quarter_id1,
      quarter_id < quarter_id2
    ) %>%
    dplyr::count(spectrum_region_code, sex, age_group1, age_group2,
                 wt = infections, name = "infections_t1t2")

  hivpop_t1t2 <- dplyr::right_join(hivpop_t1, hivpop_t2,
                                   by = c("spectrum_region_code", "sex", "age_group1"),
                                   multiple = "all") %>%
    dplyr::left_join(infections_t1t2,
                     by = c("spectrum_region_code", "sex", "age_group1", "age_group2")) %>%
    dplyr::mutate(L_hivpop = (hivpop2 - infections_t1t2) / hivpop1)


  ## # Calculate net_growth_ratio: ratio of district population change vs. Spectrum
  ##   region population change by cohort.

  totpop_area_t1 <- totpop_area %>%
    dplyr::filter(quarter_id == quarter_id1) %>%
    dplyr::count(area_id, spectrum_region_code, sex, age_group1,
                 wt = population, name = "population1")

  totpop_area_t2 <- totpop_area %>%
    dplyr::filter(quarter_id == quarter_id2) %>%
    dplyr::count(area_id, spectrum_region_code, sex, age_group1, age_group2,
                 wt = population, name = "population2")

  totpop_area_t1t2 <- totpop_area_t1 %>%
    dplyr::right_join(totpop_area_t2,
                      by = c("area_id", "spectrum_region_code", "sex", "age_group1"),
                      multiple = "all") %>%
    dplyr::mutate(
      totpop_ratio_area = population2 / population1,
      population1 = NULL,
      population2 = NULL
    )

  totpop_spec_t1 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id1) %>%
    dplyr::count(spectrum_region_code, sex, age_group1,
                 wt = totpop, name = "totpop1")

  totpop_spec_t2 <- hivpop %>%
    dplyr::filter(quarter_id == quarter_id2) %>%
    dplyr::count(spectrum_region_code, sex, age_group1, age_group2,
                 wt = totpop, name = "totpop2")

  totpop_spec_t1t2 <- totpop_spec_t1 %>%
    dplyr::right_join(totpop_spec_t2,
                      by = c("spectrum_region_code", "sex", "age_group1"),
                      multiple = "all") %>%
    dplyr::mutate(
      totpop_ratio_spec = totpop2 / totpop1,
      totpop2 = NULL,
      totpop1 = NULL
    )

  net_growth_ratio_t1t2 <- totpop_area_t1t2 %>%
    dplyr::right_join(totpop_spec_t1t2,
                      by = c("spectrum_region_code", "sex", "age_group1", "age_group2")) %>%
    dplyr::mutate(net_growth_ratio = totpop_ratio_area / totpop_ratio_spec) %>%
    dplyr::select(area_id, sex, age_group1, age_group2, totpop_ratio_area, totpop_ratio_spec, net_growth_ratio)

  net_growth_ratio_t1t2_aggr <- net_growth_ratio_t1t2 %>%
    dplyr::summarise(dplyr::across(c(totpop_ratio_area, totpop_ratio_spec), sum),
                     .by = c(area_id, sex, age_group1)) %>%
    dplyr::mutate(net_growth_ratio = totpop_ratio_area / totpop_ratio_spec)

  if (!adjust_area_growth) {
    net_growth_ratio_t1t2$net_growth_ratio <- 1.0
    net_growth_ratio_t1t2_aggr$net_growth_ratio <- 1.0
  }

  hivpopLproj <- hivpop_t1t2 %>%
    dplyr::inner_join(
      dplyr::select(mf_model, spectrum_region_code, sex, age_group1 = age_group, area_id, idx1 = idx),
      by = c("spectrum_region_code", "sex", "age_group1"),
      multiple = "all"
    ) %>%
    dplyr::inner_join(
      dplyr::select(mf_model, spectrum_region_code, sex, age_group2 = age_group, area_id, idx2 = idx),
      by = c("spectrum_region_code", "sex", "age_group2", "area_id")
    )

  hivpopLproj <- hivpopLproj %>%
    dplyr::left_join(
      net_growth_ratio_t1t2,
      by = c("area_id", "sex", "age_group1", "age_group2")
    ) %>%
    dplyr::mutate(
      L_hivpop = L_hivpop * net_growth_ratio
    )

  Lproj_hivpop <- Matrix::sparseMatrix(i = hivpopLproj$idx2,
                                       j = hivpopLproj$idx1,
                                       x = hivpopLproj$L_hivpop,
                                       dims = rep(nrow(mf_model), 2))

  Lproj_netgrow <- Matrix::sparseMatrix(i = hivpopLproj$idx2,
                                        j = hivpopLproj$idx1,
                                        x = hivpopLproj$net_growth_ratio,
                                        dims = rep(nrow(mf_model), 2))

  infections_age_t2 <- infections_cohort %>%
    dplyr::filter(
      quarter_id >= quarter_id1,
      quarter_id < quarter_id2
    ) %>%
    dplyr::count(spectrum_region_code, sex, age_group_infection, age_group2,
                 wt = infections, name = "infections_age_t2")

  ## This uses the number of infections in the previous year because incidence
  ## relates to Spectrum incidence calculation, which is infections during
  ## the previous year

  infections_age_1year <- infections_cohort %>%
  dplyr::filter(
    quarter_id >= quarter_id1 - 4,
    quarter_id < quarter_id1
  ) %>%
    dplyr::count(spectrum_region_code, sex, age_group_infection,
                 wt = infections, name = "infections_age")

  incidLproj <- infections_age_t2 %>%
    dplyr::left_join(infections_age_1year,
                     by = c("spectrum_region_code", "sex", "age_group_infection"),
                     multiple = "all") %>%
    dplyr::mutate(
      L_incid = dplyr::if_else(infections_age == 0, 0, infections_age_t2 / infections_age)
    ) %>%
    dplyr::inner_join(
      dplyr::select(mf_model, spectrum_region_code, sex, age_group_infection = age_group, area_id, idx1 = idx),
      by = c("spectrum_region_code", "sex", "age_group_infection")
    ) %>%
    dplyr::inner_join(
      dplyr::select(mf_model, spectrum_region_code, sex, age_group2 = age_group, area_id, idx2 = idx),
      by = c("spectrum_region_code", "sex", "age_group2", "area_id")
    )

  ## !! REMOVED FROM v2.6.0 release; target for v2.6.1
  ## incidLproj <- incidLproj %>%
  ##   dplyr::left_join(
  ##     net_growth_ratio_t1t2,
  ##     by = c("area_id", "sex", "age_group_infection" = "age_group1", "age_group2")
  ##   ) %>%
  ##   dplyr::mutate(
  ##     L_incid2 = L_incid * (net_growth_ratio ^ 0.5)
  ##   )

  Lproj_incid <- Matrix::sparseMatrix(i = incidLproj$idx2,
                                      j = incidLproj$idx1,
                                      x = incidLproj$L_incid,
                                      dims = rep(nrow(mf_model), 2))

  ## Paediatric entrants and survivors between time 1 and time 2

  paedLproj <- hivpop_t1t2 %>%
    dplyr::filter(is.na(age_group1)) %>%
    dplyr::select(spectrum_region_code, sex2 = sex, age_group2, hivpop2) %>%
    dplyr::left_join(
      hivpop_t1 %>%
      dplyr::left_join(get_age_groups(),
                       by = c("age_group1" = "age_group"),
                       multiple = "all") %>%
      dplyr::filter(
        sex == "female",
        age_group_start >= 15,
        (age_group_start + age_group_span) < 50
      ) %>%
      dplyr::select(spectrum_region_code, sex1 = sex, age_group1, hivpop1),
      by = "spectrum_region_code"
    ) %>%
    dplyr::group_by(spectrum_region_code, sex2, age_group2) %>%
    dplyr::mutate(L_paed = hivpop2 / sum(hivpop1)) %>%
    dplyr::ungroup()

  paedLproj <- paedLproj %>%
    dplyr::inner_join(
      dplyr::select(mf_model, spectrum_region_code, sex1 = sex,
                    age_group1 = age_group, area_id, idx1 = idx),
      by = c("spectrum_region_code", "sex1", "age_group1")
    ) %>%
    dplyr::inner_join(
      dplyr::select(mf_model, spectrum_region_code, sex2 = sex,
                    age_group2 = age_group, area_id, idx2 = idx),
      by = c("spectrum_region_code", "sex2", "age_group2", "area_id")
    )

  paedLproj <- paedLproj %>%
    dplyr::left_join(
      dplyr::filter(net_growth_ratio_t1t2_aggr, sex == "female"),
      by = c("area_id", "age_group1")
    ) %>%
    dplyr::mutate(
      L_paed = L_paed * (net_growth_ratio ^ 0.5)
    )

  Lproj_paed <- Matrix::sparseMatrix(i = paedLproj$idx2,
                                     j = paedLproj$idx1,
                                     x = paedLproj$L_paed,
                                     dims = rep(nrow(mf_model), 2))

  list(Lproj_hivpop = Lproj_hivpop,
       Lproj_incid = Lproj_incid,
       Lproj_paed = Lproj_paed,
       Lproj_netgrow= Lproj_netgrow)
}
