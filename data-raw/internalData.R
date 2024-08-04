## code to prepare `DATASET` dataset goes here

resultTypeTabs <- dplyr::tribble(
  ~result_type, ~title, ~icon,
  "summarise_cohort_characteristics", "Cohort characteristics", "people-group"
)

usethis::use_data(resultTypeTabs, overwrite = TRUE, internal = TRUE)
