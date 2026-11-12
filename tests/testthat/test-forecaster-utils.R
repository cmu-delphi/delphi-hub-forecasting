suppressPackageStartupMessages(source(here::here("R", "load_all.R")))

test_that("sanitize_args_predictors_trainer", {
  epi_data <- epidatasets::covid_case_death_rates
  # don't need to test validate_forecaster_inputs as that's inherited
  # testing args_list inheritance
  ex_args <- default_args_list()
  expect_error(sanitize_args_predictors_trainer(epi_data, "case_rate", c("case_rate"), 5, ex_args))
  argsPredictors <- sanitize_args_predictors_trainer(
    epi_data,
    "case_rate",
    c("case_rate", ""),
    parsnip::linear_reg(),
    ex_args
  )
  args_list <- argsPredictors[[1]]
  predictors <- argsPredictors[[2]]
  expect_equal(predictors, c("case_rate"))
})

test_that("id generation works", {
  # Same arguments but scrambled.
  # fmt: skip
  simple_ex <- list(
    dplyr::tribble(
      ~forecaster, ~trainer, ~pop_scaling, ~lags,
      "scaled_pop", "linreg", TRUE, c(1, 2)
    ), dplyr::tribble(
      ~forecaster, ~pop_scaling, ~trainer, ~lags,
      "scaled_pop", TRUE, "linreg", c(1, 2)
    ), dplyr::tribble(
      ~trainer, ~forecaster, ~pop_scaling,
      "linreg", "scaled_pop", TRUE,
    ), dplyr::tribble(
      ~trainer, ~pop_scaling, ~forecaster,
      "linreg", TRUE, "scaled_pop",
    )
  )
  same_ids <- map(simple_ex, add_id)
  expect_equal(same_ids[[1]]$id, same_ids[[2]]$id)
  expect_equal(same_ids[[3]]$id, same_ids[[4]]$id)
  # Same as above, but direct calls into get_single_id
  for (i in 1:4) {
    expect_equal(simple_ex[[i]] %>% purrr::transpose() %>% pluck(1) %>% get_single_id(), same_ids[[i]]$id)
  }
})

test_that("forecaster lookup selects the right rows", {
  param_grid_ex <- tibble(
    id = c("simian.irishsetter", "monarchist.thrip"),
    forecaster = rep("scaled_pop", 2),
    lags = list(NULL, c(0, 7, 14)),
    pop_scale = c(FALSE, TRUE),
  )
  # fmt: skip
  expect_equal(forecaster_lookup("monarchist", param_grid_ex), tribble(
    ~id, ~forecaster, ~lags, ~pop_scale,
    "monarchist.thrip", "scaled_pop", c(0, 7, 14), TRUE,
  ))
  # fmt: skip
  expect_equal(forecaster_lookup("irish", param_grid_ex), tribble(
    ~id, ~forecaster, ~lags, ~pop_scale,
    "simian.irishsetter", "scaled_pop", NULL, FALSE,
  ))
})

test_that("find_lagging_geos flags only geos that end early", {
  # fmt: skip
  epi_data <- tribble(
    ~geo_value, ~source,   ~time_value,           ~value, ~nssp,
    "ca",       "nhsn",    as.Date("2024-01-03"), 1,      1,
    "ca",       "nhsn",    as.Date("2024-01-10"), 1,      1,
    "ia",       "nhsn",    as.Date("2024-01-03"), 1,      1,
    "ia",       "nhsn",    as.Date("2024-01-10"), 1,      NA,
    "pr",       "nhsn",    as.Date("2024-01-03"), 1,      NA,
    "pr",       "nhsn",    as.Date("2024-01-10"), 1,      NA,
    "tx",       "nhsn",    as.Date("2024-01-03"), 1,      1,
    "tx",       "flusurv", as.Date("2024-01-03"), 1,      1,
    "tx",       "nhsn",    as.Date("2024-01-10"), 1,      1,
    "wy",       "nhsn",    as.Date("2024-01-03"), 1,      1,
    "wy",       "nhsn",    as.Date("2024-01-10"), NA,     1,
  ) %>%
    as_epi_df(other_keys = "source", as_of = as.Date("2024-01-17"))

  expect_equal(find_lagging_geos(epi_data, "nssp"), "ia")
  expect_equal(find_lagging_geos(epi_data, c("value", "nssp")), c("ia", "wy"))
  expect_equal(find_lagging_geos(epi_data, c("value", "nssp"), list(geo_value = "wy")), "ia")
  # A geo is not lagging if only an ignored source ends early.
  only_flusurv_late <- epi_data %>% filter(!(geo_value == "tx" & source == "nhsn"))
  expect_equal(find_lagging_geos(only_flusurv_late, "nssp", list(source = "flusurv")), "ia")
})

test_that("scaled_pop_seasonal: a lagging geo does not shift the lags of other geos", {
  jhu <- epidatasets::covid_case_death_rates %>%
    filter(time_value >= as.Date("2021-10-01"), geo_value %in% c("ca", "fl", "ny", "tx", "wa"))
  attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 1
  latest <- max(jhu$time_value)
  lagging <- jhu %>% mutate(death_rate = if_else(geo_value == "wa" & time_value == latest, NA, death_rate))
  run <- function(epi_data) {
    scaled_pop_seasonal(
      epi_data, "case_rate", "death_rate",
      ahead = 7L, pop_scaling = FALSE, lags = list(c(0, 7), c(0, 7)),
      scale_method = "none", center_method = "none", nonlin_method = "none"
    ) %>%
      filter(geo_value != "wa") %>%
      arrange(geo_value, quantile)
  }
  expect_equal(run(lagging), run(jhu))
})

# Weekly data with a sharp winter peak 5 weeks after the last observed week, so
# start dates later in the season than the forecast are near or after the peak.
early_ramp_data <- function() {
  last_week <- as.Date("2025-11-05")
  weeks <- seq(as.Date("2021-07-07"), last_week, by = 7)
  tidyr::expand_grid(geo_value = c("ca", "tx", "ny"), time_value = weeks) %>%
    mutate(
      peak = as.Date(paste0(lubridate::year(time_value - 182), "-12-10")),
      value = 1 + 50 * exp(-as.numeric(time_value - peak)^2 / (2 * 21^2)) + rnorm(dplyr::n(), sd = 0.2)
    ) %>%
    select(geo_value, time_value, value) %>%
    as_epi_df(as_of = last_week + 7)
}

test_that("scaled_pop_seasonal with fixed_seasonal_window does not pull the longest ahead down early in a season", {
  set.seed(11)
  epi_data <- early_ramp_data()
  medians <- function(fixed) {
    purrr::map(c(14, 21, 28), function(ahead) {
      scaled_pop_seasonal(
        epi_data, "value",
        ahead = ahead, pop_scaling = FALSE, lags = c(0, 7), seasonal_method = "window",
        fixed_seasonal_window = fixed, trainer = epipredict::quantile_reg(method = "fn"),
        scale_method = "none", center_method = "none", nonlin_method = "none"
      ) %>%
        filter(quantile == 0.5) %>%
        mutate(ahead = ahead)
    }) %>%
      bind_rows() %>%
      arrange(geo_value, ahead)
  }
  fixed <- medians(TRUE)
  growing <- medians(FALSE)
  expect_true(all(fixed %>% summarize(rising = all(diff(value) > 0), .by = geo_value) %>% pull(rising)))
  expect_false(isTRUE(all.equal(fixed$value, growing$value)))
})
