#' Summarise a selected period by country
#'
#' @param data A subset of [rollout_weeks] with the original columns.
#' @param milestone Vaccination coverage threshold as a percentage from 0 to 100.
#' @param milestone_data Data with the original columns used to search for
#'   milestones. Defaults to `data`. Supply the full series to compare a
#'   selected peak window with milestones that may fall outside it.
#' @return A data frame with one row per country, or zero rows for empty input.
#'   Peak statistics use `data`. Milestone statistics use `milestone_data`. Tied case peaks use the earliest
#'   week. The milestone is the first weekly coverage observation at or above the
#'   threshold. Missing values are returned as NA.
#'
#' Returned columns are Country, Weeks, Weeks with case data, Peak weekly cases
#' per 100,000, Peak week ending, Coverage in peak week (%), Latest observed
#' coverage (%), Coverage observation date, Milestone observed on, and Cases
#' per 100,000 in milestone week, followed by Weeks from milestone to peak.
#' The interval compares the Sunday endings of the two reporting weeks. A
#' positive number means the peak week came later, a negative number means it
#' came earlier, and zero means both were in the same reporting week.
#' Dates are ISO strings and rates are rounded
#' to one decimal place.
#' @seealso [rollout_weeks], [run_app()]
#' @examples
#' summarise_rollout(rollout_weeks, milestone = 70)
#' @export
summarise_rollout <- function(data, milestone = 70, milestone_data = data) {
  required <- c("country", "week_end", "cases_per_100k", "coverage_pct", "coverage_date")
  if (!is.data.frame(data) || !all(required %in% names(data))) {
    stop("`data` must be a data frame containing: ", paste(required, collapse = ", "),
         call. = FALSE)
  }
  if (!is.data.frame(milestone_data) || !all(required %in% names(milestone_data))) {
    stop("`milestone_data` must be a data frame containing: ",
         paste(required, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(milestone) || length(milestone) != 1L ||
      !is.finite(milestone) || milestone < 0 || milestone > 100) {
    stop("`milestone` must be one percentage between 0 and 100.", call. = FALSE)
  }
  data <- data[order(data$week_end), , drop = FALSE]
  summary <- data.frame(
    Country = unique(as.character(data$country)),
    stringsAsFactors = FALSE
  )
  groups <- lapply(summary$Country, function(country) data[data$country == country, ])
  milestone_data <- milestone_data[order(milestone_data$week_end), , drop = FALSE]
  milestone_groups <- lapply(summary$Country, function(country) {
    milestone_data[milestone_data$country == country, ]
  })
  peak_index <- function(group) {
    which.max(group$cases_per_100k)[1L]
  }
  milestone_index <- function(group) {
    which(group$coverage_pct >= milestone)[1L]
  }
  summary$Weeks <- vapply(groups, nrow, integer(1))
  summary[["Weeks with case data"]] <- vapply(groups, function(group) {
    sum(!is.na(group$cases_per_100k))
  }, integer(1))
  summary[["Peak weekly cases per 100,000"]] <- vapply(groups, function(group) {
    round(group$cases_per_100k[peak_index(group)], 1)
  }, numeric(1))
  summary[["Peak week ending"]] <- vapply(groups, function(group) {
    format(group$week_end[peak_index(group)])
  }, character(1))
  summary[["Coverage in peak week (%)"]] <- vapply(groups, function(group) {
    group$coverage_pct[peak_index(group)]
  }, numeric(1))
  summary[["Latest observed coverage (%)"]] <- vapply(groups, function(group) {
    utils::tail(c(NA_real_, group$coverage_pct[!is.na(group$coverage_pct)]), 1L)
  }, numeric(1))
  summary[["Coverage observation date"]] <- vapply(groups, function(group) {
    format(utils::tail(c(as.Date(NA), group$coverage_date[!is.na(group$coverage_pct)]), 1L))
  }, character(1))
  summary[["Milestone observed on"]] <- vapply(milestone_groups, function(group) {
    format(group$coverage_date[milestone_index(group)])
  }, character(1))
  summary[["Cases per 100,000 in milestone week"]] <- vapply(milestone_groups, function(group) {
    round(group$cases_per_100k[milestone_index(group)], 1)
  }, numeric(1))
  summary[["Weeks from milestone to peak"]] <- vapply(seq_along(groups), function(i) {
    peak <- groups[[i]]
    history <- milestone_groups[[i]]
    as.numeric(peak$week_end[peak_index(peak)] -
                 history$week_end[milestone_index(history)], units = "days") / 7
  }, numeric(1))
  summary
}
