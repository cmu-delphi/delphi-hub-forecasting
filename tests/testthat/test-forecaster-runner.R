suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

valid_forecast <- function() {
  tidyr::expand_grid(
    geo_value = c("ca", "tx"),
    target_end_date = as.Date(c("2024-11-23", "2024-11-30")),
    quantile = c(0.25, 0.5, 0.75)
  ) %>%
    mutate(
      forecast_date = as.Date("2024-11-20"),
      value = quantile * 100
    )
}

test_that("validate_forecast_output passes a well-formed forecast through", {
  fc <- valid_forecast()
  expect_identical(validate_forecast_output(fc, "test"), fc)
})

test_that("validate_forecast_output passes the empty insufficient-data result", {
  null_result <- tibble(
    geo_value = character(),
    forecast_date = lubridate::Date(),
    target_end_date = lubridate::Date(),
    quantile = numeric(),
    value = numeric()
  )
  expect_identical(validate_forecast_output(null_result, "test"), null_result)
})

test_that("validate_forecast_output catches missing columns", {
  expect_error(
    validate_forecast_output(valid_forecast() %>% select(-quantile), "test"),
    "missing column"
  )
})

test_that("validate_forecast_output catches NAs in required columns", {
  fc <- valid_forecast()
  fc$value[3] <- NA
  expect_error(validate_forecast_output(fc, "test"), "NA values")
})

test_that("validate_forecast_output catches negative values", {
  fc <- valid_forecast()
  fc$value[1] <- -1
  expect_error(validate_forecast_output(fc, "test"), "negative value")
})

test_that("validate_forecast_output catches crossing quantiles per task", {
  fc <- valid_forecast()
  # cross one task's median below its 25th percentile; other tasks stay valid
  fc$value[fc$geo_value == "ca" & fc$target_end_date == as.Date("2024-11-23") & fc$quantile == 0.5] <- 1
  expect_error(validate_forecast_output(fc, "test"), "quantiles cross in 1 task")
})

test_that("validate_forecast_output allows ties across quantiles", {
  fc <- valid_forecast() %>% mutate(value = 0)
  expect_identical(validate_forecast_output(fc, "test"), fc)
})

# Weekly Wednesday-labeled data for `geo_value`, ending `latest`, all reported
# one week after each time_value.
weekly_rows <- function(geo_value, latest, source = "nhsn", n_weeks = 8) {
  time_value <- seq(as.Date(latest) - 7 * (n_weeks - 1), as.Date(latest), by = 7)
  tibble(geo_value = geo_value, source = source, time_value = time_value, version = time_value + 7, value = 1)
}

latency_archive <- function() {
  bind_rows(
    weekly_rows("ca", "2026-11-04"),
    weekly_rows("tx", "2026-10-14"),
    weekly_rows("de", "2026-10-07"),
    weekly_rows("de", "2026-11-04", source = "nssp")
  ) %>%
    as_epi_archive(other_keys = "source", compactify = FALSE)
}

test_that("late_input_geos drops a geo after 4 missed reports but not after 3", {
  snapshot <- latency_archive() %>%
    epix_as_of(as.Date("2026-11-11"))
  attributes(snapshot)$metadata$as_of <- as.Date("2026-11-11")
  expect_identical(late_input_geos(snapshot, "nhsn", NULL, 4), "de")
})

test_that("late_input_geos counts only the primary source", {
  snapshot <- latency_archive() %>% epix_as_of(as.Date("2026-11-11"))
  attributes(snapshot)$metadata$as_of <- as.Date("2026-11-11")
  expect_identical(late_input_geos(snapshot, "nssp", NULL, 4), character(0))
})

test_that("late_input_geos drops nothing when the latency is unbounded", {
  snapshot <- latency_archive() %>% epix_as_of(as.Date("2026-11-11"))
  expect_identical(late_input_geos(snapshot, "nhsn", NULL, Inf), character(0))
})

test_that("late_input_geos reads an archive snapshot as of its forecast date", {
  current <- make_forecast_archive_snapshot(latency_archive(), "2026-11-11", "2026-11-11")
  expect_identical(late_input_geos(current, "nhsn", NULL, 4), "de")
  # On 2026-10-21, de had missed only one report, so a replay keeps it.
  replay <- make_forecast_archive_snapshot(latency_archive(), "2026-10-21", "2026-10-21")
  expect_identical(late_input_geos(replay, "nhsn", NULL, 4), character(0))
})

# nhsn current for every geo; the nssp column is current for ca, 4 weeks behind
# for tx, 5 weeks behind for de, and absent for wy.
extra_latency_archive <- function() {
  nssp <- bind_rows(
    weekly_rows("ca", "2026-11-04"),
    weekly_rows("tx", "2026-10-14"),
    weekly_rows("de", "2026-10-07")
  ) %>%
    select(geo_value, time_value, nssp = value)
  c("ca", "tx", "de", "wy") %>%
    map(\(geo) weekly_rows(geo, "2026-11-04")) %>%
    bind_rows() %>%
    left_join(nssp, by = join_by(geo_value, time_value)) %>%
    as_epi_archive(other_keys = "source", compactify = FALSE)
}

current_extra_snapshot <- function() {
  snapshot <- extra_latency_archive() %>% epix_as_of(as.Date("2026-11-11"))
  attributes(snapshot)$metadata$as_of <- as.Date("2026-11-11")
  snapshot
}

test_that("late_input_geos drops geos whose extra source is late or absent", {
  late <- late_input_geos(current_extra_snapshot(), "nhsn", "nssp", 4)
  expect_setequal(late, c("de", "wy"))
})

test_that("late_input_geos ignores extra columns that are not in extra_sources", {
  expect_identical(late_input_geos(current_extra_snapshot(), "nhsn", NULL, 4), character(0))
})

test_that("late_input_geos checks extra columns in an archive snapshot", {
  current <- make_forecast_archive_snapshot(extra_latency_archive(), "2026-11-11", "2026-11-11")
  expect_setequal(late_input_geos(current, "nhsn", "nssp", 4), c("de", "wy"))
})

test_that("run_forecaster drops geos whose joined extra data is late", {
  snapshot <- current_extra_snapshot() %>% select(-nssp)
  extra_data <- current_extra_snapshot() %>%
    as_tibble() %>%
    select(geo_value, time_value, nssp)
  original <- get_partially_applied_forecaster
  withr::defer(assign("get_partially_applied_forecaster", original, envir = globalenv()))
  assign("get_partially_applied_forecaster", function(...) {
    function(epi_data) {
      tidyr::expand_grid(geo_value = c("ca", "tx", "de", "wy"), quantile = c(0.25, 0.5, 0.75)) %>%
        mutate(forecast_date = as.Date("2026-11-11"), target_end_date = as.Date("2026-11-18"), value = quantile)
    }
  }, envir = globalenv())
  out <- run_forecaster(
    snapshot, "unused",
    aheads = 7, params = list(extra_sources = "nssp"), id = "test",
    join_extra_data = TRUE, extra_data = extra_data, max_latency_weeks = 4
  )
  expect_setequal(unique(out$geo_value), c("ca", "tx"))
})

test_that("run_forecaster drops late geos from the output", {
  snapshot <- latency_archive() %>% epix_as_of(as.Date("2026-11-11"))
  attributes(snapshot)$metadata$as_of <- as.Date("2026-11-11")
  # load_all.R sources into the global environment, so swap the binding there.
  original <- get_partially_applied_forecaster
  withr::defer(assign("get_partially_applied_forecaster", original, envir = globalenv()))
  assign("get_partially_applied_forecaster", function(...) {
    function(epi_data) {
      tidyr::expand_grid(geo_value = c("ca", "tx", "de"), quantile = c(0.25, 0.5, 0.75)) %>%
        mutate(forecast_date = as.Date("2026-11-11"), target_end_date = as.Date("2026-11-18"), value = quantile)
    }
  }, envir = globalenv())
  out <- run_forecaster(snapshot, "unused", aheads = 7, params = list(), id = "test", max_latency_weeks = 4)
  expect_setequal(unique(out$geo_value), c("ca", "tx"))
  kept <- run_forecaster(snapshot, "unused", aheads = 7, params = list(), id = "test")
  expect_setequal(unique(kept$geo_value), c("ca", "tx", "de"))
})
