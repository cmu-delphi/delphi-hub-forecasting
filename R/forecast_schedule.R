#' Production runs generated after their round's Wednesday, usually because of
#' a holiday. Each row maps the date a run was generated to the forecast date
#' (the round's Wednesday) it is for. Hub validation skips the submission window
#' check on these generation dates. Add a row before you start a delayed run.
DELAYED_PROD_RUNS <- tibble::tribble(
  ~generation_date, ~forecast_date, ~reason,
  "2025-12-29", "2025-12-24", "Christmas",
  "2026-11-12", "2026-11-11", "Veterans Day",
) %>%
  mutate(across(ends_with("_date"), as.Date))

#' The forecast date (a Wednesday) of the prod round that a run generated on
#' `generation_date` belongs to. Listed delayed runs use their forecast date in
#' `DELAYED_PROD_RUNS`; other dates use the Wednesday on or before them.
#' @param generation_date dates that runs are generated on.
prod_forecast_date <- function(generation_date) {
  generation_date <- as.Date(generation_date)
  delayed <- match(generation_date, DELAYED_PROD_RUNS$generation_date)
  if_else(
    is.na(delayed),
    floor_date(generation_date, "weeks", week_start = 3),
    DELAYED_PROD_RUNS$forecast_date[delayed]
  )
}

#' Whether a run generated on `date` is a listed delayed prod run.
is_delayed_prod_run <- function(date) {
  as.Date(date) %in% DELAYED_PROD_RUNS$generation_date
}
