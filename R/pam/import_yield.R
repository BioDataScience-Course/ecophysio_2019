
#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
pam_import <- function(file) {
  pam <- readr::read_delim(file,
    ";",
    escape_double = FALSE, trim_ws = TRUE,
    col_types = readr::cols(
      Date = readr::col_character(),
      Time = readr::col_character(),
      .default = readr::col_number()
    )
  )
  pam$file <- file
  ## delete last colums
  pam <- dplyr::select(pam, -dplyr::starts_with("X"))
  # rename the columns
  names(pam) -> t
  t <- stringr::str_replace(t, pattern = "PAR", replacement = "par")
  t <- stringr::str_replace(t, pattern = "No.", replacement = "numb")
  t <- stringr::str_replace(t, pattern = "Fm'", replacement = "fmax_")
  t <- stringr::str_replace(t, pattern = "F", replacement = "fmin_")
  t <- stringr::str_replace(t, pattern = "Y.II.", replacement = "yield_")
  t <- stringr::str_replace(t, pattern = "Y.NPQ.", replacement = "npq_")
  t <- stringr::str_replace(t, pattern = "Y.NO.", replacement = "no_")
  names(pam) <- t

  # change the date
  pam <- tidyr::unite(pam, "date", Date, Time, sep = " ")
  pam$date <- lubridate::dmy_hms(pam$date)
  pam
}
################################################################################
# file <- "data/raw/ip001/2018-07-19-bt9-y.csv"
pam_yield <- function(file, interested_var = c("fmin", "fmax", "yield")) {
  require(flow)
  pam_file <- pam_import(file = file)
  i <- 1
  t <- names(pam_file)
  pam_file$aio <- sum(stringr::str_count(t, interested_var[i]))

  var_struc <- c("date", "aio", "file")
  pam_file1 <- dplyr::select(pam_file, var_struc)

  vec <- vector(mode = "numeric", length = length(interested_var))
  names(vec) <- interested_var

  for (i in seq_along(interested_var))
    pam_file %>.%
      dplyr::select(., stringr::str_subset(t, interested_var[i])) %>.%
      apply(., MARGIN = 1, FUN = mean) -> vec[i]

  df <- dplyr::bind_cols(pam_file1, dplyr::as_data_frame(t(vec)))
  df
}
