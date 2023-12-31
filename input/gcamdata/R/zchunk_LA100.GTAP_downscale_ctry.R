# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_LA100.GTAP_downscale_ctry
#'
#' Downscale GTAP region-level land value data to all countries.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.GTAP_LV_milUSD}. The corresponding file in the
#' original data system was \code{LA100.GTAP_downscale_ctry.R} (aglu level1).
#' @details This chunk downscales the GTAP region-level land value to all countries
#' based on production share by GLU and GTAP commodity class.
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter group_by left_join mutate right_join select summarise summarise_if
#' @importFrom tidyr replace_na
#' @importFrom stats na.omit
#' @author RC April 2017
module_aglu_LA100.GTAP_downscale_ctry <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "aglu/AGLU_ctry",
      FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
      "L100.LDS_value_milUSD",
      "L100.LDS_ag_prod_t")

  MODULE_OUTPUTS <-
    c("L100.GTAP_LV_milUSD")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {


    iso <- GTAP_region <- GTAP_use <- GLU <- value <- prod_ctry <- prod_rgn <-
        share <- NULL                   # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----

    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # Create the iso - GTAP_region mapping file
    # GTAP6 includes 87 regions, most of which are single countries, and 18 are aggregated regions of multiple countries.
    AGLU_ctry %>%
      select(iso, GTAP_region) %>%
      distinct(iso, .keep_all = TRUE) ->
      GTAP_ctry

    # Prepare land value for each entire GTAP region
    FAO_ag_items_PRODSTAT %>%
      select(GTAP_use) %>%
      distinct() %>%
      na.omit() %>%
      repeat_add_columns(distinct(L100.LDS_ag_prod_t[c("iso", "GLU")])) %>%
      left_join_error_no_match(GTAP_ctry, by = "iso") %>%
      # Match in the land value for the entire GTAP region. These will be multiplied by country shares
      left_join(L100.LDS_value_milUSD, by = c("GTAP_region", "GLU", "GTAP_use")) %>%
      replace_na(list(value = 0)) ->
      LV_Rgtap

    # Compute production by GTAP region
    L100.LDS_ag_prod_t %>%
      left_join_error_no_match(GTAP_ctry, by = "iso") %>%
      left_join(FAO_ag_items_PRODSTAT[c("GTAP_crop", "GTAP_use")], by = "GTAP_crop") %>%
      filter(!is.na(GTAP_use)) %>%
      group_by(GTAP_region, GLU, GTAP_use) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() %>%
      rename(prod_rgn = value) ->
      Ag_Prod_Rgtap

    # Compute production by ctry iso
    L100.LDS_ag_prod_t %>%
      left_join(FAO_ag_items_PRODSTAT[c("GTAP_crop", "GTAP_use")], by = "GTAP_crop") %>%
      filter(!is.na(GTAP_use)) %>%
      group_by(iso, GLU, GTAP_use) %>%
      summarise_if(is.numeric, sum) %>%
      ungroup() %>%
      rename(prod_ctry = value) %>%
      # Compute the country-within-GTAP region shares for each of the commodity classes
      # Share = production by country and GTAP use / production by GTAP region and GTAP use
      left_join_error_no_match(GTAP_ctry, by = "iso") %>%
      left_join_error_no_match(Ag_Prod_Rgtap, by = c("GTAP_region", "GLU", "GTAP_use")) %>%
      mutate(share = prod_ctry / prod_rgn) %>%
      replace_na(list(share = 0)) %>%
      # Multiply the land values by the shares
      right_join(LV_Rgtap, by = c("iso", "GTAP_region", "GLU", "GTAP_use")) %>%
      mutate(value = value * share) %>%
      replace_na(list(value = 0)) %>%
      select(-prod_ctry, -prod_rgn, -share) %>%

      # Produce outputs
      add_title("Land value by country / GLU / GTAP commodity class") %>%
      add_units("Million US Dollars") %>%
      add_comments("Compute the country-within-GTAP region's production share for each of the commodity classes") %>%
      add_comments("Downscale the GTAP region-level land value to countries by production shares") %>%
      add_legacy_name("L100.GTAP_LV_milUSD") %>%
      add_precursors("aglu/AGLU_ctry",
                     "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L100.LDS_value_milUSD",
                     "L100.LDS_ag_prod_t") ->
      L100.GTAP_LV_milUSD

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
