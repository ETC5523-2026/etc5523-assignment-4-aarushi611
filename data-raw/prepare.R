# Source: Our World in Data COVID-19 archive at the pinned commit below.
# Run from the repository root. Optionally supply a local archive CSV as argument.
args <- commandArgs(trailingOnly = TRUE)
source_csv <- if (length(args)) args[1L] else
  "https://raw.githubusercontent.com/owid/covid-19-data/f0d53320b19703cf565940d8269181a8423d7032/public/data/owid-covid-data.csv"
# Import the archived daily records.
raw <- utils::read.csv(source_csv, na.strings = c("", "NA"))
raw$date <- as.Date(raw$date)
# Keep Australia and New Zealand in complete Monday-to-Sunday weeks.
daily <- raw[raw$iso_code %in% c("AUS", "NZL") &
               raw$date >= as.Date("2021-01-04") &
               raw$date <= as.Date("2021-12-26"), ]
daily$week_end <- daily$date + (7L - as.integer(format(daily$date, "%u")))
# Group by reporting week and country, then calculate rates and coverage.
groups <- split(daily, list(daily$week_end, daily$location), drop = TRUE)
rollout_weeks <- do.call(rbind, lapply(groups, function(week) {
  stopifnot(nrow(week) == 7L, !anyDuplicated(week$date),
            length(unique(week$population)) == 1L, week$population[1L] > 0)
  week <- week[order(week$date, decreasing = TRUE), ]
  latest <- which(!is.na(week$people_fully_vaccinated_per_hundred))[1L]
  data.frame(country = week$location[1L], week_end = week$week_end[1L],
             cases_per_100k = round(sum(week$new_cases) / week$population[1L] * 1e5, 8),
             coverage_pct = week$people_fully_vaccinated_per_hundred[latest],
             coverage_date = week$date[latest])
}))
rownames(rollout_weeks) <- NULL
stopifnot(nrow(rollout_weeks) == 102L)
# Serialize the validated dataset for package installation.
save(rollout_weeks, file = "data/rollout_weeks.rda", compress = "xz", version = 2)
