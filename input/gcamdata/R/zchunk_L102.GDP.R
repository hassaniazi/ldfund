# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' Join past GDP time series to future.
#'
#' When we have to join two GDP time series, we usually find that they don't
#' match up at year of overlap (the "base year").  What we do in these cases is
#' we compute, for the later time series, ratios of GDPs in the future years to
#' those in the base year.  We then multiply the future ratios by the past base
#' year value.  That future time series can then be grafted onto the past
#' without leaving a seam.
#'
#' In practice, the past is often a single time series, while the future is
#' often a collection of scenarios.  Therefore, we assume that the past time
#' series has no scenario column.  If the future does not have a scenario
#' column, it is given a dummy one, which is dropped before the new table is
#' returned.  Note that we look for lower-case 'scenario' for this.
#'
#' The base year is calculated automatically.  It is the maximum of the years
#' that overlap between the two data sets.
#'
#' We also have to know how to group the data for calculating the gdp ratios.
#' Normally this will be either by country ('iso') or by GCAM region
#' ('GCAM_region_ID').  The choice of which is passed in as the 'grouping'
#' argument.
#'
#' Finally, although we have discussed this function in terms of joining two GDP
#' time series, in the future time series we use only the ratios of GDP to base
#' year GDP.  Therefore, any time series with the correct ratios will work.  For
#' example, if we have a time series of growth rates, we can convert those to
#' ratios using \code{\link[base]{cumprod}} and pass those ratios as the future
#' time series.  For similar reasons, even if the two time series have different
#' units (e.g., different dollar-years or PPP vs. MER), they can still be
#' joined.  The units of the output time series will be the same as the units of
#' \code{past}.
#'
#' @param past Tibble with the past time series (year, gdp, and grouping).
#' @param future Tibble with the future data (year, gdp, scenario, and
#' grouping).
#' @param grouping Name of the grouping column (generally either 'iso' or
#' 'GCAM_region_ID', but could be anything
#' @return Time series with the past and future joined as described in details.
join.gdp.ts <- function(past, future, grouping) {

  year <- gdp <- base.gdp <- gdp.ratio <- . <- scenario <-
    NULL                            # silence notes on package check.

  if(! 'scenario' %in% names(future)) {
    ## This saves us having to make a bunch of exceptions below when we
    ## include 'scenario' among the columns to join by.
    future$scenario <- 'scen'
    drop.scenario <- TRUE
  }
  else {
    drop.scenario <- FALSE
  }

  ## Find the base year
  base.year <- max(intersect(past$year, future$year))
  assert_that(is.finite(base.year))

  ## Base year gdp from the future dataset
  baseyear.future.gdp <- filter(future, year == base.year) %>%
    rename(base.gdp = gdp) %>%
    select(-year)

  gdp.future.ratio <- filter(future, year > base.year) %>%
    left_join_error_no_match(baseyear.future.gdp, by = c('scenario', grouping)) %>%
    mutate(gdp.ratio = gdp / base.gdp) %>%
    select('scenario', grouping, 'year', 'gdp.ratio')

  ## add the scenario column to the past
  gdp.past <- tidyr::crossing(past, scenario = unique(gdp.future.ratio[['scenario']]))
  baseyear.past.gdp <- filter(gdp.past, year == base.year) %>%
    rename(base.gdp = gdp) %>%
    select(-year)

  rslt <- left_join(baseyear.past.gdp, gdp.future.ratio,
                    by = c('scenario', grouping)) %>%
    mutate(gdp = base.gdp * gdp.ratio) %>%
    select('scenario', grouping, 'year', 'gdp') %>%
    bind_rows(gdp.past, .)

  if(drop.scenario) {
    select(rslt, -scenario)
  }
  else {
    rslt
  }
}

#' module_socioeconomics_L102.GDP
#'
#' Prepare historical and future GDP time series.  On the historical side, this
#' amounts to aggregating country-level GDP to GCAM regions.  On the future side
#' we create a time series for each of a variety of future scenarios.  The
#' outputs include GDP, pcGDP, and the PPP-MER conversion factor, all tabulated
#' by GCAM region.
#'
#' The scenarios generated include the SSPs and the gSSPs (SSPs modified by
#' near-term IMF projections).  GDP outputs are in millions of 1990 USD, Market
#' Exchange Rate (measured in 2010) is used for foreign currency.  Per-capita
#' values are in thousands of 1990 USD.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L102.gdp_mil90usd_Scen_R_Y}, \code{L102.pcgdp_thous90USD_Scen_R_Y}, \code{L102.gdp_mil90usd_GCAM3_R_Y}, \code{L102.gdp_mil90usd_GCAM3_ctry_Y}, \code{L102.pcgdp_thous90USD_GCAM3_R_Y}, \code{L102.pcgdp_thous90USD_GCAM3_ctry_Y}, \code{L102.PPP_MER_R}. The corresponding file in the
#' original data system was \code{L102.GDP.R} (socioeconomics level1).
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter full_join if_else intersect group_by left_join mutate one_of select summarise transmute
#' @importFrom tidyr complete gather nesting replace_na pivot_longer
#' @author RPL March 2017
module_socioeconomics_L102.GDP <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "common/GCAM_region_names",
             FILE = "socioeconomics/SSP_database_v9",
             FILE = "socioeconomics/IMF_GDP_growth",
             FILE = "socioeconomics/GCAM3_GDP",
             FILE = "ldfund/ldPools",
             FILE = "ldfund/ldGDPchange",
             FILE = "ldfund/ldGDPchange_regmap",
             FILE = "ldfund/country_emissions",
             "L100.gdp_mil90usd_ctry_Yh",
             "L101.Pop_thous_GCAM3_R_Y",
             "L101.Pop_thous_GCAM3_ctry_Y",
             "L101.Pop_thous_R_Yh",
             "L101.Pop_thous_Scen_R_Yfut"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L102.gdp_mil90usd_Scen_R_Y",
             "L102.pcgdp_thous90USD_Scen_R_Y",
             "L102.PPP_MER_R",
             "L102.gdp_mil90usd_GCAM3_R_Y",
             "L102.gdp_mil90usd_GCAM3_ctry_Y",
             "L102.pcgdp_thous90USD_GCAM3_R_Y",
             "L102.pcgdp_thous90USD_GCAM3_ctry_Y"))
  } else if(command == driver.MAKE) {

    iso <- GCAM_region_ID <- value <- year <- gdp <- MODEL <- VARIABLE <-
      UNIT <- SCENARIO <- scenario <- gdp.rate <- gdp.ratio <- population <-
      pcgdp <- MER <- PPP <- region_GCAM3 <- agg_val <- share <-
      GCAM3_value <- base <- ratio <- value.x <- value.y <- NULL     # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    SSP_database_v9 <- get_data(all_data, "socioeconomics/SSP_database_v9")
    IMF_GDP_growth <- get_data(all_data, "socioeconomics/IMF_GDP_growth")
    GCAM3_GDP <- get_data(all_data, "socioeconomics/GCAM3_GDP") %>%
      gather_years %>%
      mutate(value = as.numeric(value))
    L100.gdp_mil90usd_ctry_Yh <- get_data(all_data, "L100.gdp_mil90usd_ctry_Yh")
    L101.Pop_thous_GCAM3_R_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_R_Y")
    L101.Pop_thous_GCAM3_ctry_Y <- get_data(all_data, "L101.Pop_thous_GCAM3_ctry_Y")
    L101.Pop_thous_R_Yh <- get_data(all_data, "L101.Pop_thous_R_Yh")
    L101.Pop_thous_Scen_R_Yfut <- get_data(all_data, "L101.Pop_thous_Scen_R_Yfut")

    # loss and damage fund related files
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    ldPools <- get_data(all_data, "ldfund/ldPools") %>% change_iso_code('rou', 'rom')
    ldGDPchange <- get_data(all_data, "ldfund/ldGDPchange") %>% change_iso_code('rou', 'rom')
    ldGDPchange_regmap <- get_data(all_data, "ldfund/ldGDPchange_regmap")
    country_emissions <- get_data(all_data, "ldfund/country_emissions") %>% change_iso_code('rou', 'rom')

    ## iso--region lookup without extraneous data.  We'll use this several times.
    iso_region32_lookup <- select(iso_GCAM_regID, iso, GCAM_region_ID)

    ## Add region IDs to historical GDP and aggregate by region. Hang onto the country-level data
    ## because we will use them again when we make the IMF adjustments
    gdp_mil90usd_ctry <-
      left_join_error_no_match(L100.gdp_mil90usd_ctry_Yh, iso_region32_lookup, by = 'iso') %>%
      rename(gdp = value)

    gdp_mil90usd_rgn <- gdp_mil90usd_ctry %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(gdp = sum(gdp)) %>%
      ungroup()
    ## gdp_mil90usd_ctry:  iso, GCAM_region_ID, year, gdp
    ## gdp_mil90usd_rgn:  GCAM_region_ID, year, gdp

    # ===================================================
    ## Loss and damage fund processing -----

    # read in country mapping file with ISOs (which has G7, G20, V20, V50 countries with ISOs)
    # read in GDP change file (should have ISOs)
    # add GDP loss in V20 and V50 countries to get total climate damages
    # change GDP sign from negative to positive for V countries (to reflect GDP gain)
    # read in emissions by countries (mainly relevant for G7 and G20 countries)
    # left join pools with emissions to get emission percentages by country pools
    # take total GDP loss and divide it among sender countries, then subtract it from their GDPs

    # full set of GCAM region country information ----
    iso_GCAM_regID %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(!region_GCAM3) %>% filter(iso != "rou") -> gcam_iso_region

    # GDP change mapping to GCAM regions for countries with data
    ldGDPchange %>%
      left_join(gcam_iso_region) %>%
      filter(iso != "-") %>% select(!country_name) -> ldGDPchange_gcam_iso

    # GDP change mapping to GCAM regions for "rest of" regions to be later used to map to GCAM countries
    ldGDPchange %>%
      filter(iso == "-") %>%
      left_join(ldGDPchange_regmap) -> ldGDPchange_gcam_restof

    # attach GCAM countries to "rest of" regions
    gcam_iso_region %>%
      left_join(select(ldGDPchange_gcam_restof, -c(iso))) %>% # joins in all rest of regions to gcam countries
      anti_join(ldGDPchange_gcam_iso, by = "iso") %>% # removes countries that are NOT "rest of" and have data
      # inner_join(ldGDPchange_gcam_iso) # check if there are overlapping countries, uncommenting this should return empty table
      select(!country) %>%
      rename(country = country_name) %>%
      tidyr::pivot_longer(cols = ends_with("C"), names_to = "temp_change", values_to = "gdp_change") %>%
      group_by(iso, country, GCAM_region_ID, region, temp_change) %>%
      summarise(gdp_change = mean(gdp_change)) %>%
      tidyr::pivot_wider(names_from = "temp_change", values_from = "gdp_change") %>%
      ungroup() -> ldGDPchange_gcam_restof_iso


    # GDP change by GCAM countries and regions in 4 climates ----
    rbind(ldGDPchange_gcam_restof_iso, ldGDPchange_gcam_iso) %>%
      arrange(GCAM_region_ID, iso) -> ldGDPchange_gcam

    # write.csv(ldGDPchange_gcam, file = "ldGDPchange_gcam.csv")

    # plot gdp change in each temperature change for each gcam region
    # df <- ldGDPchange_gcam %>% tidyr::pivot_longer(cols = ends_with("C"), names_to = "temp_change", values_to = "gdp_change")
    # ggplot(df, aes(x = gdp_change, y = temp_change)) +
    #   geom_point(aes(colour = temp_change)) +
    #   facet_wrap(~region, ncol=8) + theme_bw()
    # # facet_wrap(~country, ncol=10) + theme_bw()

    # Country's future GDP in each SSP  ----
    # These are PPP values in billion 2005 dollars (billion US$2005/yr)
    gdp_bilusd_cntry_Yfut <-
      filter(SSP_database_v9, MODEL == 'OECD Env-Growth' & VARIABLE == 'GDP|PPP') %>%
      standardize_iso('REGION') %>%
      change_iso_code('rou', 'rom') %>%
      left_join_error_no_match(iso_region32_lookup, by = 'iso') %>%
      protect_integer_cols %>%
      dplyr::select_if(function(x) {!any(is.na(x))}) %>% # filter NAs in SSP database
      unprotect_integer_cols %>%
      select(-MODEL, -VARIABLE, -UNIT) %>%
      gather_years(value_col = "gdp") %>%
      mutate(gdp = as.numeric(gdp),
             scenario = substr(SCENARIO, 1, 4)) %>% # trim extra scenario info from SSPs
      select(!SCENARIO) %>%
      ungroup() %>%
      filter(year >= 2020) # only keep future model years

    sender_pool = "E7"
    receiver_pool = "VinformVHH"
    # prepare GDP values files for each pool ----
    # gdp senders
    ldPools %>% filter(pool == "E7") %>% left_join(gdp_bilusd_cntry_Yfut) -> cntry_gdp_sender_e7
    ldPools %>% filter(pool == "E35") %>% left_join(gdp_bilusd_cntry_Yfut) -> cntry_gdp_sender_e35
    ldPools %>% filter(pool == "G7") %>% left_join(gdp_bilusd_cntry_Yfut) -> cntry_gdp_sender_g7
    # TODO: G20 has EU, map it to countries. It's OK for now because we will be modeling E7-VHH combination
    #ldPools %>% filter(pool == "G20") %>% left_join(gdp_bilusd_cntry_Yfut) -> cntry_gdp_sender_g20

    # gdp receivers
    ldPools %>% filter(pool == "VinformVH") %>% left_join(gdp_bilusd_cntry_Yfut) -> cntry_gdp_receiver_vh # 20 countries
    ldPools %>% filter(pool == "VinformVHH") %>% left_join(gdp_bilusd_cntry_Yfut) -> cntry_gdp_receiver_vhh # 37 countries
    # TODO: 4 countries for which we do not have SSP/GDP data. Drop those or figure out their mapping
    ldPools %>% filter(pool == "V20") %>% left_join(gdp_bilusd_cntry_Yfut) %>% filter(is.na(gdp) != T) -> cntry_gdp_receiver_v20

    # prepare GDP CHANGE files for each pool ----
    # gdp change file senders (don't need this as senders will get GDP based on vulnerable countries damages),
    # BUT THESE COULD BE USED TO MODEL CLIMATE DAMAGES BASELINE SCENARIO
    # ldPools %>% filter(pool == "E7") %>% left_join(select(ldGDPchange_gcam, !country), by = "iso") -> cntry_gdp_change_sender_e7
    # ldPools %>% filter(pool == "E35") %>% left_join(select(ldGDPchange_gcam, !country), by = "iso") -> cntry_gdp_change_sender_e35
    # ldPools %>% filter(pool == "G7") %>% left_join(select(ldGDPchange_gcam, !country), by = "iso") -> cntry_gdp_change_sender_g7
    # ldPools %>% filter(pool == "G20") %>% left_join(select(ldGDPchange_gcam, !country), by = "iso") -> cntry_gdp_change_sender_g20

    # gdp change file receivers
    ldPools %>% filter(pool == "VinformVH") %>%
      left_join(select(ldGDPchange_gcam, !country), by = "iso") -> cntry_gdp_change_receiver_vh # 20 countries
    ldPools %>% filter(pool == "VinformVHH") %>%
      left_join(select(ldGDPchange_gcam, !country), by = "iso") -> cntry_gdp_change_receiver_vhh # 37 countries
    # TODO: 4 countries for which we do not have SSP/GDP data. Drop those or figure out their mapping
    ldPools %>% filter(pool == "V20") %>%
      left_join(select(ldGDPchange_gcam, !country), by = "iso") %>% drop_na() -> cntry_gdp_change_receiver_v20

    # apply GDP change in receiver countries (VHH)
    cntry_gdp_receiver_vhh %>% left_join(cntry_gdp_change_receiver_vhh) %>%
      tidyr::pivot_longer(cols = ends_with("C"), names_to = "temp_change", values_to = "gdp_change") %>%
      # compensate damages of vulnerable countries
      mutate(gdp_adjust = gdp * (1 + abs((gdp_change/100))),
             gdp_change_usd = gdp_adjust - gdp) -> cntry_gdp_adjust_receiver_vhh # final adjust GDPs for reciever countries

    # calculate total damages in vulnerable countries (i.e., gdp change), DONT NEED THIS
    # receiver_vhh_gdp_adjust %>%
    #   group_by(country, iso, GCAM_region_ID, region, year, scenario, temp_change) %>%
    #   summarize(total_gdp_adjust = sum(gdp_adjust),
    #             total_gdp = sum(gdp)) %>%
    #   mutate(gdp_change_usd = total_gdp_adjust - total_gdp) %>%
    #   ungroup() -> receiver_vhh_gdp_change_value

    # check: sum(receiver_vhh_gdp_change_value$gdp_change_usd) should be equal to
    # (sum(receiver_vhh_gdp_adjust$gdp_adjust) - sum(receiver_vhh_gdp_adjust$gdp))
    cntry_gdp_adjust_receiver_vhh %>%
      group_by(year, scenario, temp_change) %>%
      summarize(total_gdp_change_usd = sum(gdp_change_usd)) %>%
      ungroup() -> receiver_vhh_total_gdp_change_usd

    # distribute it in sender countries according to their emissions share ----
    # prepare emissions file for sender countries
    ldPools %>% filter(pool == "E7") %>% left_join(country_emissions) %>%
      gather_years() %>% group_by(country, iso) %>%
      summarise(total_emissions = sum(value)) %>% ungroup() %>%
      mutate(emissions_share = total_emissions/sum(total_emissions)) -> sender_e7_emissions_share

    # check: the following should be equal to 100, sum(sender_e7_emissions_share$emissions_share)
    # senders new gdp accounting for damages payments
    cntry_gdp_sender_e7 %>%
      left_join(sender_e7_emissions_share) %>%
      left_join(receiver_vhh_total_gdp_change_usd) %>%
      rename(total_gdp_change_usd_receiver = total_gdp_change_usd) %>%
      mutate(sender_gdp_loss = total_gdp_change_usd_receiver * emissions_share,
             sender_gdp_adjust = gdp - sender_gdp_loss) -> cntry_gdp_adjust_sender_e7

    # complete the country level updated GDPs data table ----

    # combine updated GDPs of senders and receivers
    gdp_bilusd_cntry_Yfut %>%
      repeat_add_columns(tibble(temp_change = c("1C", "2C", "3C", "4C"))) %>%
      # bring back in the sender countries with unchanged GDPs
      left_join(select(cntry_gdp_adjust_sender_e7, c(iso, year, scenario, temp_change, sender_gdp_loss))) %>%
      replace_na(list(sender_gdp_loss = 0)) %>%
      mutate(gdp = gdp - sender_gdp_loss) %>%
      # bring back in the reciever countries with unchanged GDPs
      left_join(select(cntry_gdp_adjust_receiver_vhh, c(iso, year, scenario, temp_change, gdp_adjust))) %>%
      mutate(gdp = if_else(is.na(gdp_adjust), gdp, gdp_adjust)) %>%
      select(iso, GCAM_region_ID, year, scenario, temp_change, gdp) -> cntry_gdp_adjust_all

    # extract each RCP data frame and assign a new name ----
    cntry_gdp_adjust_all %>%
      dplyr::group_split(temp_change) -> cntry_gdp_adjust_all_list

    temp_list <- list()
    for (i in seq_along(cntry_gdp_adjust_all_list)) {
      # filter for each RCP and drop the temp_change column
      cntry_gdp_adjust_all_list[[i]] %>% select(!temp_change) -> temp_list[[i]]
      assign(paste0("cntry_gdp_adjust_", unique(cntry_gdp_adjust_all$temp_change[i])), temp_list[[i]])
    }

    # tie historical country level GDPs to LDfund processed GDPs
    SSP_database_v9 %>%
      filter(MODEL == 'OECD Env-Growth' & VARIABLE == 'GDP|PPP') %>%
      standardize_iso('REGION') %>%
      change_iso_code('rou', 'rom') %>%
      left_join_error_no_match(iso_region32_lookup, by = 'iso') %>%
      protect_integer_cols %>%
      dplyr::select_if(function(x) {!any(is.na(x))}) %>% # filter NAs in SSP database
      unprotect_integer_cols %>%
      select(-MODEL, -VARIABLE, -UNIT) %>%
      gather_years(value_col = "gdp") %>%
      mutate(gdp = as.numeric(gdp),
             scenario = substr(SCENARIO, 1, 4)) %>% # trim extra scenario info from SSPs
      select(!SCENARIO) %>%
      ungroup() %>%
      filter(year < 2020) %>% # only keep historical model years
    # attach ldfund processing. Change temp change here
    bind_rows(cntry_gdp_adjust_4C) -> cntry_gdp_adjust_4C_Yall

    ## Get the future GDP in the SSP scenarios. These are PPP values in 2005 dollars
    gdp_bilusd_rgn_Yfut <-
      # PUT-WHATEVER-THE-FINAL-LDFUND-PROCESSING-OUTPUT-IS
      cntry_gdp_adjust_4C_Yall %>%
      # filter(SSP_database_v9, MODEL == 'OECD Env-Growth' & VARIABLE == 'GDP|PPP') %>%
      # standardize_iso('REGION') %>%
      # change_iso_code('rou', 'rom') %>%
      # left_join_error_no_match(iso_region32_lookup, by = 'iso') %>%
      # protect_integer_cols %>%
      # dplyr::select_if(function(x) {!any(is.na(x))}) %>% # apparently the SSP database has some missing in it; filter these out.
      # unprotect_integer_cols %>%
      # select(-MODEL, -iso, -VARIABLE, -UNIT) %>%
      # gather_years(value_col = "gdp") %>%
      # mutate(gdp = as.numeric(gdp),
      #        scenario = substr(SCENARIO, 1, 4)) %>% # Trim the junk off the end of the scenario names, leaving us with just SSP1, SSP2, etc.
      group_by(scenario, GCAM_region_ID, year) %>%
      summarise(gdp = sum(gdp)) %>%
      select(scenario, GCAM_region_ID, year, gdp) %>%
      ungroup() %>%
      # The steps below write out the data to all future years, starting from the final socio historical year
      complete(nesting(scenario, GCAM_region_ID), year = c(socioeconomics.FINAL_HIST_YEAR, FUTURE_YEARS)) %>%
      group_by(scenario, GCAM_region_ID) %>%
      mutate(gdp = approx_fun(year, gdp)) %>%
      ungroup()
    ## Units are billions of 2005$


    gdp.mil90usd.SSP.rgn.yr <- join.gdp.ts(gdp_mil90usd_rgn, gdp_bilusd_rgn_Yfut, 'GCAM_region_ID')

    ## Get the IMF GDP growth rates.  Some countries are missing, so we have to
    ## add them in with an assumed zero growth rate.
    imfgdp.growth <-
      select(IMF_GDP_growth, one_of(c('ISO', socioeconomics.IMF_GDP_YEARS))) %>%
      standardize_iso('ISO') %>%
      change_iso_code('rou', 'rom') %>%
      gather(year, gdp.rate, -iso) %>%
      full_join(gdp_mil90usd_ctry %>% select(iso) %>% unique, by = 'iso') %>%
      mutate(gdp.rate = if_else(gdp.rate == 'n/a', '0', gdp.rate), # Treat string 'n/a' as missing.
             year = as.integer(year),
             gdp.rate = as.numeric(gdp.rate)) %>%
      #kbn 2020-03-26 Updated below to the last year in the IMF_GDP_YEARS so that latest GDP growth rates are picked up
      replace_na(list(year =  max(socioeconomics.IMF_GDP_YEARS))) %>% # have to do this for `complete` to work as expected.
      complete(iso, year) %>%
      replace_na(list(gdp.rate = 0.0))

    imfgdp.ratio <-
      imfgdp.growth %>%
      mutate(gdp.ratio = 1.0 + gdp.rate / 100.0) %>%
      arrange(year) %>% group_by(iso) %>%
      mutate(gdp = cumprod(gdp.ratio)) %>% # actually ratio of gdp to base-year
      ungroup() %>%
      # gdp, but we're calling it "gdp" so
      # that join.gdp.ts() can work with it.
      select(iso, year, gdp)


    gdp.mil90usd.imf.country.yr <-
    gdp_mil90usd_ctry %>%
      # filter gdp data so that it ends right at the first year of the IMF ratio data
      filter(year <= min(imfgdp.ratio$year), year >= min(HISTORICAL_YEARS)) %>%
      select(iso, year, gdp) %>%
      join.gdp.ts(imfgdp.ratio, 'iso')

    ## columns: iso, year, gdp

    ## Aggregate by GCAM region
    gdp.mil90usd.imf.rgn.yr <-
      left_join_error_no_match(gdp.mil90usd.imf.country.yr, iso_region32_lookup, by = 'iso') %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(gdp = sum(gdp)) %>%
      ungroup() %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS))
    ## columns:  GCAM_region_ID, year, gdp

    ## join the IMF near future up to the SSP distant future, rename scenarios
    ## to gSSP*
    gdp.mil90usd.gSSP.rgn.yr <-
      join.gdp.ts(gdp.mil90usd.imf.rgn.yr, gdp_bilusd_rgn_Yfut, 'GCAM_region_ID') %>%
      mutate(scenario = paste0('g', scenario))
    ## columns: scenario, GCAM_region_ID, year, gdp

    ## combine SSP and gSSP scenarios into a single table (this will be one of
    ## our final outputs)
    gdp.mil90usd.scen.rgn.yr <-
      bind_rows(gdp.mil90usd.SSP.rgn.yr, gdp.mil90usd.gSSP.rgn.yr)

    ## Construct a table of population by scenario, region, and year.  We have a
    ## table of historical population, and a table of future population by
    ## scenario, both in wide form.  Convert to long form and filter to the years
    ## we need.  Add a scenario column to historical years, and combine the
    ## whole thing into a single table.
    pop.thous.fut <-
      rename(L101.Pop_thous_Scen_R_Yfut, population = value) %>%
      filter(year %in% FUTURE_YEARS)
    pop.thous.hist <-
      rename(L101.Pop_thous_R_Yh, population = value) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      tidyr::crossing(scenario = unique(pop.thous.fut[['scenario']]))
    pop.thous.scen.rgn.yr <-
      bind_rows(pop.thous.hist, pop.thous.fut) %>%
      mutate(year = as.integer(year),
             population = as.numeric(population)) %>%
      select(scenario, GCAM_region_ID, year, population)

    ## calculate per-capita GDP.  This is another final output
    pcgdp.thous90usd.scen.rgn.yr <-
      left_join(gdp.mil90usd.scen.rgn.yr, pop.thous.scen.rgn.yr,
                by = c('scenario', 'GCAM_region_ID', 'year')) %>%
      mutate(pcgdp = gdp / population) %>%
      select(scenario, GCAM_region_ID, year, pcgdp)

    ## Calculate the PPP-MER conversion factor in base year for each region.
    ## Our PPP values are in billions of 2005$, so we make that conversion
    ## here too.
    #kbn 2020-03-26 Using model final base year here below
    PPP.MER.baseyr <- MODEL_FINAL_BASE_YEAR
    mer.rgn <- gdp_mil90usd_ctry %>%
      filter(year == PPP.MER.baseyr) %>%
      group_by(GCAM_region_ID) %>%
      mutate(MER = gdp * gdp_deflator(2005, 1990) * CONV_MIL_BIL) %>%
      summarise(MER = sum(MER))
    ## columns: GCAM_region_ID, MER

    ## The future data is given by SSP scenario, but the final table is scenario
    ## independent, as it should be, since this base year is meant to be a
    ## historical year.  Likewise, the GDP in the PPP/MER base year should also
    ## be scenario-independent, and mostly it is, except for the region containing
    ## the Palestinian Territories, which has four slightly different values across
    ## the 5 SSPs. (It's SSP 2 and 4 that are the same).  The value
    ## actually used in the old data system is the one for SSP1, so that's the
    ## one we'll use here.  Arguably we should average the values over the 5
    ## scenarios, but the differences are only 1 part in 10^4, so we can just
    ## let it slide.
    ppp.rgn <- gdp_bilusd_rgn_Yfut %>%
      ungroup %>%
      filter(year == PPP.MER.baseyr, scenario == 'SSP1') %>%
      rename(PPP = gdp) %>%
      select(GCAM_region_ID, PPP)

    ppp.mer.rgn <-
      left_join_error_no_match(mer.rgn, ppp.rgn, by = 'GCAM_region_ID') %>%
      mutate(PPP_MER = PPP / MER)

    # GDP by GCAM region from GCAM 3.0 GDPs.
    # Downscaling GCAM 3.0 GDP by GCAM 3.0 region to countries, using SSP2 GDP scenario
    # GDP by GCAM 3.0 region - downscale to country according to actual shares in the historical periods, and SSPbase in the future periods

    # Future GDP
    gdp_bilusd_ctry_Yfut <- SSP_database_v9 %>%
      filter(MODEL == 'OECD Env-Growth' & VARIABLE == 'GDP|PPP') %>%
      standardize_iso('REGION') %>%
      change_iso_code('rou', 'rom') %>%
      mutate(scenario = substr(SCENARIO, 1, 4)) %>%
      # Only Base SSP
      filter(scenario == socioeconomics.BASE_POP_SCEN) %>%
      gather_years %>%
      mutate(value = as.numeric(value)) %>%
      select(iso, year, value) %>%
      complete(nesting(iso), year = c(socioeconomics.FINAL_HIST_YEAR, FUTURE_YEARS)) %>%
      group_by(iso) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() %>%
      filter(year %in% FUTURE_YEARS)

    # Historical GDP
    gdp_mil90usd_ctry_Yh <- L100.gdp_mil90usd_ctry_Yh %>%
      filter(year %in% HISTORICAL_YEARS,
             iso %in% gdp_bilusd_ctry_Yfut$iso)

    gdp_mil90usd_ctry_Yh <- gdp_mil90usd_ctry_Yh %>%
      # Only take future GDP if iso has historical GDP
      bind_rows(gdp_bilusd_ctry_Yfut %>%
                  filter(iso %in% gdp_mil90usd_ctry_Yh$iso)) %>%
      left_join_error_no_match(iso_GCAM_regID %>%
                                 select(iso, region_GCAM3), by = "iso")

    # Aggregating GDP by GCAM3 region
    gdp_mil90usd_SSPbase_RG3_Y <- gdp_mil90usd_ctry_Yh %>%
      group_by(year, region_GCAM3) %>%
      summarise(agg_val = sum(value))

    # Calculate shares of each country within its region
    gdpshares_ctryRG3_Y <- gdp_mil90usd_ctry_Yh %>%
      group_by(year, region_GCAM3) %>%
      mutate(share = value / sum(value)) %>%
      ungroup()

    # Interpolate GCAM3 GDP data to all historical and future years
    gdp_mil90usd_GCAM3_RG3_Y <- iso_GCAM_regID %>%
      select(region_GCAM3) %>%
      distinct() %>%
      repeat_add_columns(tibble(year = as.integer(c(HISTORICAL_YEARS, FUTURE_YEARS)))) %>%
      left_join(GCAM3_GDP, by = c("region_GCAM3", "year")) %>%
      # Add in historical values to match in for NA values
      left_join_error_no_match(gdp_mil90usd_SSPbase_RG3_Y, by = c("region_GCAM3", "year")) %>%
      group_by(region_GCAM3) %>%
      # Extending GCAM 3.0 scenario to first historical year using historical GDP ratios by GCAM 3.0 region
      mutate(value = replace(value, year == min(year),
                             value[year == min(GCAM3_GDP$year)] * agg_val[year == min(year)] / agg_val[year == min(GCAM3_GDP$year)]),
             value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      select(region_GCAM3, year, GCAM3_value = value)

    # Multiply these GDP numbers by the shares of each country within GCAM region
    gdp_mil90usd_GCAM3_ctry_Y <- gdpshares_ctryRG3_Y %>%
      left_join_error_no_match(gdp_mil90usd_GCAM3_RG3_Y, by = c("year", "region_GCAM3")) %>%
      transmute(iso, year, value = share * GCAM3_value)

    # rebasing GDP to 2010 USD at country level
    # Calculate the GDP ratios from the first year in the projections. Use this ratio to project GDP from historical dataset in final historical period
    gdpRatio_GCAM3_ctry_Yfut <- gdp_mil90usd_GCAM3_ctry_Y %>%
      group_by(iso) %>%
      mutate(base = value[year == socioeconomics.FINAL_HIST_YEAR]) %>%
      ungroup() %>%
      filter(year %in% FUTURE_YEARS) %>%
      transmute(iso, year, ratio = value / base)

    # Use these ratios to build the GDP trajectories by old GCAM3
    gdp_mil90usd_GCAM3_ctry_Y <- gdp_mil90usd_ctry_Yh %>%
      left_join(gdpRatio_GCAM3_ctry_Yfut, by = c("iso", "year")) %>%
      group_by(iso) %>%
      mutate(value = if_else(year %in% FUTURE_YEARS, value[year == socioeconomics.FINAL_HIST_YEAR] * ratio, value)) %>%
      ungroup() %>%
      select(iso, year, value)

    # Aggregating by GCAM4 region
    gdp_mil90usd_GCAM3_R_Y <- gdp_mil90usd_GCAM3_ctry_Y %>%
      left_join_error_no_match(iso_region32_lookup, by = "iso") %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Calculate per-capita GDP
    pcgdp_thous90USD_GCAM3_R_Y <- gdp_mil90usd_GCAM3_R_Y %>%
      left_join_error_no_match(L101.Pop_thous_GCAM3_R_Y, by = c("year", "GCAM_region_ID")) %>%
      transmute(GCAM_region_ID, year, value = value.x / value.y)

    pcgdp_thous90USD_GCAM3_ctry_Y <- gdp_mil90usd_GCAM3_ctry_Y %>%
      left_join_error_no_match(L101.Pop_thous_GCAM3_ctry_Y, by = c("year", "iso")) %>%
      transmute(iso, year, value = value.x / value.y)

    # ===================================================

    ## Produce outputs
    # gdp.mil90usd.scen.rgn.yr_ldfund
    gdp.mil90usd.scen.rgn.yr %>%
      ungroup %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      rename(value = gdp) %>%
      mutate(year = as.integer(year)) %>%
      add_title("Gross Domestic Product (GDP) by scenario, region, and year.") %>%
      add_units("Millions of 1990 USD (MER)") %>%
      add_comments("For the SSP scenarios, SSP GDP projections are scaled to match ") %>%
      add_comments("historical values in the base year (2010).  For the gSSP scenarios ") %>%
      add_comments("IMF growth projections are applied from 2010-2020, and the SSP projections ") %>%
      add_comments("are scaled to match the 2020 values resulting from this process.") %>%
      add_legacy_name("L102.gdp_mil90usd_Scen_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP_database_v9",
                     "socioeconomics/IMF_GDP_growth",
                     "L100.gdp_mil90usd_ctry_Yh") ->
      L102.gdp_mil90usd_Scen_R_Y

    pcgdp.thous90usd.scen.rgn.yr %>%
      ungroup %>%
      filter(year %in% c(HISTORICAL_YEARS, FUTURE_YEARS)) %>%
      rename(value = pcgdp) %>%
      add_title("Gross Domestic Product (GDP) per capita, by scenario, region, and year.") %>%
      add_units("Thousands of 1990 USD (MER)") %>%
      add_comments("Computed as GDP/population.  Values prior to the base year (2010) are ") %>%
      add_comments("historical; values subsequent are from SSP projections.") %>%
      add_legacy_name("L102.pcgdp_thous90USD_Scen_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP_database_v9",
                     "socioeconomics/IMF_GDP_growth",
                     "L100.gdp_mil90usd_ctry_Yh",
                     "L101.Pop_thous_R_Yh",
                     "L101.Pop_thous_Scen_R_Yfut") ->
      L102.pcgdp_thous90USD_Scen_R_Y

    ppp.mer.rgn %>%
      add_title("Purchasing Power Parity (PPP) to Market Exchange Rate (MER) GDP conversions, by region") %>%
      add_units("unitless") %>%
      add_comments("Calculated as GDP(PPP) / GDP(MER) in the base year (2010) for each region.  The") %>%
      add_comments("table also contains PPP-GDP and MER-GDP in billions of 2005 USD for the base year, ") %>%
      add_comments("because they were included in the original table, but I'm not sure they are used for, ") %>%
      add_comments("or useful for, anything besides calculating the ratio.") %>%
      add_legacy_name("L102.PPP_MER_R") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP_database_v9",
                     "socioeconomics/IMF_GDP_growth",
                     "L100.gdp_mil90usd_ctry_Yh") ->
      L102.PPP_MER_R

    gdp_mil90usd_GCAM3_R_Y %>%
      add_title("GDP by GCAM3 Region") %>%
      add_units("Million 1990 USD") %>%
      add_comments("Scales historical SSP GDP data to GCAM3 GDP data.") %>%
      add_comments("Calculates future GDP based on ratio of GCAM3 future to 2010 value.") %>%
      add_legacy_name("L102.gdp_mil90usd_GCAM3_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP_database_v9",
                     "L100.gdp_mil90usd_ctry_Yh",
                     "socioeconomics/GCAM3_GDP") ->
      L102.gdp_mil90usd_GCAM3_R_Y

    gdp_mil90usd_GCAM3_ctry_Y %>%
      add_title("GCAM3 GDP by Country") %>%
      add_units("Million 1990 USD") %>%
      add_comments("Scales historical SSP GDP data to GCAM3 GDP data.") %>%
      add_comments("Calculates future GDP based on ratio of GCAM3 future to 2010 value.") %>%
      add_legacy_name("L102.gdp_mil90usd_GCAM3_ctry_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP_database_v9",
                     "L100.gdp_mil90usd_ctry_Yh",
                     "socioeconomics/GCAM3_GDP") ->
      L102.gdp_mil90usd_GCAM3_ctry_Y

    pcgdp_thous90USD_GCAM3_R_Y %>%
      add_title("Per-Capita GDP by GCAM3 Region") %>%
      add_units("Thousand 1990 USD") %>%
      add_comments("L102.gdp_mil90usd_GCAM3_R_Y divided by population from L101.Pop_thous_GCAM3_R_Y") %>%
      add_legacy_name("L102.pcgdp_thous90USD_GCAM3_R_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP_database_v9",
                     "L100.gdp_mil90usd_ctry_Yh",
                     "socioeconomics/GCAM3_GDP",
                     "L101.Pop_thous_GCAM3_R_Y") ->
      L102.pcgdp_thous90USD_GCAM3_R_Y

    pcgdp_thous90USD_GCAM3_ctry_Y %>%
      add_title("GCAM3 Per-Capita GDP by Country") %>%
      add_units("Thousand 1990 USD") %>%
      add_comments("L102.gdp_mil90usd_GCAM3_ctry_Y divided by population from L101.Pop_thous_GCAM3_ctry_Y") %>%
      add_legacy_name("L102.pcgdp_thous90USD_GCAM3_ctry_Y") %>%
      add_precursors("common/iso_GCAM_regID",
                     "socioeconomics/SSP_database_v9",
                     "L100.gdp_mil90usd_ctry_Yh",
                     "socioeconomics/GCAM3_GDP",
                     "L101.Pop_thous_GCAM3_ctry_Y") ->
      L102.pcgdp_thous90USD_GCAM3_ctry_Y

    return_data(L102.gdp_mil90usd_Scen_R_Y, L102.pcgdp_thous90USD_Scen_R_Y, L102.PPP_MER_R, L102.gdp_mil90usd_GCAM3_R_Y, L102.gdp_mil90usd_GCAM3_ctry_Y, L102.pcgdp_thous90USD_GCAM3_R_Y, L102.pcgdp_thous90USD_GCAM3_ctry_Y)
  } else {
    stop("Unknown command")
  }
}
