test_that("packaged data contain 51 complete reporting weeks for each country", {
  expect_equal(nrow(rollout_weeks), 102L)
  expect_equal(as.integer(table(rollout_weeks$country)), c(51L, 51L))
  expect_true(all(format(rollout_weeks$week_end, "%u") == "7"))
  expect_equal(anyDuplicated(rollout_weeks[c("country", "week_end")]), 0L)
})

test_that("summary handles empty data and an unreached threshold", {
  expect_equal(nrow(summarise_rollout(rollout_weeks[0, ])), 0L)
  result <- summarise_rollout(rollout_weeks, milestone = 100)
  expect_true(all(is.na(result[["Milestone observed on"]])))
  expect_true(all(is.na(result[["Weeks from milestone to peak"]])))
  expect_error(summarise_rollout(data.frame(country = "Australia")), "must be a data frame containing")
})

test_that("tied peaks select the earliest week and intervals are signed", {
  weeks <- subset(rollout_weeks, country == "Australia")[1:2, ]
  weeks$cases_per_100k <- c(10, 10)
  weeks$coverage_pct <- c(65, 70)
  weeks$coverage_date <- weeks$week_end
  result <- summarise_rollout(weeks[2:1, ], milestone = 70)
  expect_equal(result[["Peak week ending"]], format(weeks$week_end[1]))
  expect_equal(result[["Weeks from milestone to peak"]], -1)
  expect_equal(summarise_rollout(weeks, 65)[["Weeks from milestone to peak"]], 0)
  weeks$cases_per_100k[2] <- 20
  expect_equal(summarise_rollout(weeks, 65)[["Weeks from milestone to peak"]], 1)
})

test_that("windowed peaks can be compared with full-series milestones", {
  window <- subset(rollout_weeks, week_end >= as.Date("2021-07-04") &
                     week_end <= as.Date("2021-11-28"))
  result <- summarise_rollout(window, 70, milestone_data = rollout_weeks)
  australia <- subset(result, Country == "Australia")
  nz <- subset(result, Country == "New Zealand")
  expect_equal(australia[["Peak week ending"]], "2021-10-03")
  expect_equal(australia[["Weeks from milestone to peak"]], -7)
  expect_equal(nz[["Milestone observed on"]], "2021-12-05")
  expect_equal(nz[["Weeks from milestone to peak"]], -2)
  expect_true(is.na(subset(summarise_rollout(window), Country == "New Zealand")[["Milestone observed on"]]))
})
