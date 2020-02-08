SciViews::R

source("R/import_yield.R")

fs::dir_ls("data/PAM initiales - 23-4-19/") %>%
  purrr::map_dfr(pam_yield) -> test

