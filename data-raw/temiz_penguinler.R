library(palmerpenguins)
library(dplyr)

temiz_penguinler <- penguins |>
  dplyr::filter(!is.na(bill_length_mm),
                !is.na(bill_depth_mm),
                !is.na(flipper_length_mm),
                !is.na(body_mass_g),
                !is.na(sex))

usethis::use_data(temiz_penguinler, overwrite = TRUE)
