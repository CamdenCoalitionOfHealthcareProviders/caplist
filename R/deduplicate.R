#' allpayers_mco: clean AllPayersHIEID file for binding with result of allpayers_galileo function
#'
#' @return Tibble
#'
#' a = AllPayersHIEID file
#' c = CurrentHIEIDs file
#' @examples
#' allpayers_mco(a, c)
#'

# deduplicate:
# Combines results of allpayers_mco and allpayers_gal
# Keeps ACO records where HIE ID is NOT in AllPayers (MCO) data frame
deduplicate <-
  function(m, g) {
    g_clean_no_mco <- g %>% anti_join(m, by = "Patient ID HIE")
    g_clean_no_mco
  }

# Combine allpayers_mco and results of deduplicate
# Binds together MCO-only patient list (AllPayersHIEID file) and ACO patient list without MCO patients
combine_mco_aco <-
  function(a, d) {
    bind <- rbind(a, d)
    bind_hieid_dupe <- bind %>% mutate(hie_dupe = duplicated(bind$`Patient ID HIE`))
    bind_hieid_dupe_clean <- bind_hieid_dupe %>% filter(hie_dupe == FALSE) %>% select(-hie_dupe)
    bind_hieid_dupe_clean
  }

# Twins
# Needs `combined` function result
# Fine duplicate records based on `Patient ID HIE` field
twins <-
  function(c) {
    twins <- c %>% group_by(`Patient ID`) %>% mutate(hie_count = n()) %>% filter(hie_count > 1)
    twins
  }


# # Write files -------------------------------------------------------------
# write_csv(twins, "y:/cap_list_data/022018/twins.csv", na = "")
# write_csv(combine, "y:/cap_list_data/022018/tv_import_feb_2018.csv", na = "")
# write_csv(a_hie_join_bind, "y:/cap_list_data/022018/tv_import_feb_2018_allpayers_only.csv", na = "")
# write_csv(g_clean_bind, "y:/cap_list_data/022018/tv_import_feb_2018_aco_only.csv", na = "")

