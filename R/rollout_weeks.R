#' Vaccination coverage and reported cases in 2021
#'
#' Complete Monday-to-Sunday weeks within 2021 for Australia and New Zealand.
#' @format A data frame with 102 rows and five columns:
#' \describe{
#'   \item{country}{Country name.}
#'   \item{week_end}{Sunday ending the reporting week.}
#'   \item{cases_per_100k}{Sum of reported new cases divided by population and
#'     multiplied by 100,000. NA if any daily source entry is missing.}
#'   \item{coverage_pct}{Most recent observed percentage completing the initial
#'     vaccination protocol during the week. NA if no observation is available.}
#'   \item{coverage_date}{Date of that coverage observation.}
#' }
#' @source \url{https://github.com/owid/covid-19-data/tree/master/public/data}
#' OWID-produced data are licensed under Creative Commons Attribution.
#' Third-party data remain subject to their original providers' terms.
#' Vaccination data citation: Mathieu et al. (2021), doi:10.1038/s41562-021-01122-8.
"rollout_weeks"
