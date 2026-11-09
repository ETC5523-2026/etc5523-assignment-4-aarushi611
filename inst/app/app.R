library(shiny)

weeks <- rolloutcompare::rollout_weeks
countries <- unique(weeks$country)
colours <- c(Australia = "#076b6d", "New Zealand" = "#9c4724")

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(tags$style(HTML("
    body { background: #eef4f5; color: #173239; }
    .intro { background: #163e46; color: white; padding: 18px 24px;
             margin: 16px 0; border-radius: 12px; }
    .intro h1 { margin-top: 0; }
    .card-panel { background: white; padding: 16px 20px; margin: 16px 0;
                  border: 1px solid #d7e3e3; border-radius: 12px; }
    .table-wrap { overflow-x: auto; }
    .finding { border-left: 4px solid #076b6d; background: #edf7f6;
               padding: 10px 14px; margin-bottom: 12px; }
    .shiny-output-error-validation { color: #823c20; font-weight: 600; }
  "))),
  div(class = "intro", h1("Vaccination milestones and case peaks"),
      p("For public health students studying epidemiological surveillance in Australia and New Zealand."),
      p("Had each country reached the selected vaccination milestone before its highest reported case week in the selected period?")),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("countries", "Countries", countries, selected = countries),
      sliderInput("period", "Reporting weeks ending Sunday",
                  min = min(weeks$week_end), max = max(weeks$week_end),
                  value = range(weeks$week_end), step = 7, timeFormat = "%d %b"),
      sliderInput("milestone", "Vaccination milestone (%)",
                  min = 40, max = 80, value = 70, step = 5, post = "%"),
      div(class = "d-flex flex-wrap gap-2 mb-3",
          actionButton("full_year", "Full 2021"),
          actionButton("delta_window", "July–November"),
          actionButton("reset_all", "Reset all")),
      radioButtons("scale", "Case chart scale",
                   c("Shared across countries" = "shared", "Fit each country" = "local")),
      helpText("The milestone uses the full 2021 series. The week slider controls the charts and the period used to find the case peak.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Explore",
          div(class = "card-panel",
              h2("Milestones and case peaks"),
              p("A milestone precedes a peak week if the first qualifying coverage reading falls before the Monday that starts that week. Use the dates to compare timing. This comparison does not estimate vaccine effectiveness."),
              uiOutput("finding"),
              div(class = "table-wrap", tableOutput("summary"))),
          div(class = "card-panel",
              h2("Reported cases by country"), plotOutput("cases"),
              textOutput("scale_note"),
              p("Milestone lines appear when the observation falls within the selected reporting weeks.")),
          div(class = "card-panel",
              h2("Vaccination coverage"), plotOutput("coverage", height = "300px"),
              p("The dashed line marks your selected threshold. Rings show coverage in peak case weeks where readings are available."))),
        tabPanel("How to read this",
          div(class = "card-panel",
              h2("Try a comparison"),
              tags$ol(
                tags$li("Keep both countries selected and move the milestone from 50% to 70%."),
                tags$li("Compare each milestone observation with the Monday starting its peak reporting week."),
                tags$li("Switch the case scale to Fit each country to see each curve in detail. Use the shared scale to compare the size of the peaks.")),
              h3("Controls"),
              tags$dl(
                tags$dt("Countries"), tags$dd("Choose Australia, New Zealand, or both."),
                tags$dt("Reporting weeks"), tags$dd("Sets the plotted period and the weeks eligible for the case peak. For a closer look at the Delta-era period, try July through November."),
                tags$dt("Vaccination milestone"), tags$dd("Sets the coverage threshold. The app searches the full available 2021 series."),
                tags$dt("Case chart scale"), tags$dd("Use a shared vertical scale to compare magnitudes or fit each country to compare the shape of its curve.")),
              p("The scenario buttons restore all 2021 weeks, select July through November, or reset all controls to their defaults."),
              h3("Field guide"),
              tags$dl(
                tags$dt("Week ending"), tags$dd("Sunday at the end of a complete Monday-to-Sunday reporting week."),
                tags$dt("Weekly cases per 100,000"), tags$dd("Reported cases in the week divided by population and multiplied by 100,000."),
                tags$dt("Coverage"), tags$dd("Percentage of residents who completed the initial vaccination protocol. Each weekly point uses the last available reading in that week."),
                tags$dt("Milestone"), tags$dd("First weekly coverage observation at or above the chosen percentage in the available 2021 series."),
                tags$dt("Peak week"), tags$dd("Week with the highest reported case rate inside the selected range. Ties use the earliest week."),
                tags$dt("Weeks from milestone to peak"), tags$dd("Difference between the two reporting weeks. Positive means the peak week came later, negative means it came earlier.")))),
        tabPanel("Data & limitations",
          div(class = "card-panel",
              h2("Source and limitations"),
              p(tags$a("Our World in Data COVID-19 archive",
                       href = "https://github.com/owid/covid-19-data/tree/master/public/data"),
                "compiles WHO case reports and vaccination data from official sources."),
              tags$ul(
                tags$li("Only complete weeks inside 2021 are included. Australia's full-range peak falls in the final week ending 26 December. This dataset cannot show whether cases rose further afterward."),
                tags$li("The actual date coverage crossed a milestone may precede the first qualifying weekly observation."),
                tags$li("NA means unavailable or no qualifying milestone observation in the series."),
                tags$li("Testing and reporting influence case rates. The timing comparison cannot establish vaccine effectiveness.")))))
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$full_year, updateSliderInput(session, "period", value = range(weeks$week_end)))
  observeEvent(input$delta_window, {
    updateSliderInput(session, "period", value = as.Date(c("2021-07-04", "2021-11-28")))
  })
  observeEvent(input$reset_all, {
    updateCheckboxGroupInput(session, "countries", selected = countries)
    updateSliderInput(session, "period", value = range(weeks$week_end))
    updateSliderInput(session, "milestone", value = 70)
    updateRadioButtons(session, "scale", selected = "shared")
  })

  # Select the countries across the full series for milestone calculations.
  country_weeks <- reactive({
    validate(need(length(input$countries) > 0L, "Select at least one country."))
    weeks[weeks$country %in% input$countries, ]
  })

  # Limit plotted weeks and peak calculations to the date slider.
  selected <- reactive({
    data <- country_weeks()
    period <- as.Date(input$period, origin = "1970-01-01")
    validate(need(length(period) == 2L && !anyNA(period), "Choose a range of reporting weeks."))
    data <- data[data$week_end >= period[1L] & data$week_end <= period[2L], ]
    validate(need(nrow(data) > 0L, "Choose a range containing at least one reporting week."))
    data
  })

  # Combine window-specific peaks with full-series milestone observations.
  comparison <- reactive({
    peaks <- rolloutcompare::summarise_rollout(
      selected(), input$milestone, milestone_data = country_weeks()
    )
    peaks[, !names(peaks) %in% c("Weeks", "Weeks with case data",
                              "Latest observed coverage (%)", "Coverage observation date")]
  })

  output$summary <- renderTable({
    data <- comparison()
    data[["Weeks from milestone to peak"]] <-
      format(data[["Weeks from milestone to peak"]], trim = TRUE, nsmall = 0)
    data
  }, digits = 2, na = "NA", striped = TRUE, hover = TRUE)

  output$finding <- renderUI({
    data <- comparison()
    div(class = "finding", lapply(seq_len(nrow(data)), function(i) {
      end <- as.Date(data[["Peak week ending"]][i])
      milestone <- as.Date(data[["Milestone observed on"]][i])
      coverage <- data[["Coverage in peak week (%)"]][i]
      if (is.na(end)) {
        return(p(strong(data$Country[i]), "has no reported case rate in the selected period."))
      }
      timing <- if (is.na(milestone)) {
        sprintf("No weekly observation reached %s%% in the available 2021 series.", input$milestone)
      } else {
        interval <- data[["Weeks from milestone to peak"]][i]
        timing <- if (interval == 0) {
          "The peak and milestone were in the same reporting week."
        } else {
          sprintf("The peak was %d reporting %s %s the milestone.", abs(interval),
                  if (abs(interval) == 1) "week" else "weeks",
                  if (interval > 0) "after" else "before")
        }
        sprintf("The first weekly observation at or above %s%% was on %s. %s",
                input$milestone, format(milestone, "%d %b %Y"), timing)
      }
      coverage_note <- if (is.na(coverage)) "Coverage was unavailable that week." else
        sprintf("Recorded coverage that week was %.2f%%.", coverage)
      p(strong(data$Country[i]),
        sprintf("peaked at %.1f reported cases per 100,000 in the week ending %s.",
                data[["Peak weekly cases per 100,000"]][i], format(end, "%d %b %Y")),
        coverage_note, timing)
    }))
  })

  output$scale_note <- renderText({
    if (identical(input$scale, "local")) {
      "Each panel uses its own vertical scale. Compare the timing and shape of changes using the labelled axes."
    } else {
      "Both panels use the same vertical scale so you can compare the size of the peaks."
    }
  })

  output$cases <- renderPlot({
    data <- selected()
    annotations <- comparison()
    groups <- split(data, data$country)
    graphics::par(mfrow = c(length(groups), 1), mar = c(3.4, 4.5, 2, 1))
    for (country in names(groups)) {
      group <- groups[[country]]
      annotation <- annotations[annotations$Country == country, ]
      milestone <- as.Date(annotation[["Milestone observed on"]])
      peak_week <- as.Date(annotation[["Peak week ending"]])
      scale_data <- if (identical(input$scale, "local")) group else data
      limit <- max(c(1, scale_data$cases_per_100k), na.rm = TRUE)
      graphics::plot(group$week_end, group$cases_per_100k, type = "n",
                     xlim = range(data$week_end) + c(-6, 3), ylim = c(0, limit * 1.08),
                     main = country, xlab = "", ylab = "Cases per 100,000")
      graphics::grid(col = "#dbe6e8")
      if (!is.na(milestone) && milestone >= min(data$week_end) - 6 &&
          milestone <= max(data$week_end)) {
        graphics::abline(v = milestone, lty = 2, lwd = 2, col = "#526970")
      }
      graphics::lines(group$week_end, group$cases_per_100k, type = "o", lwd = 2,
                      pch = 16, col = colours[country])
      if (!is.na(peak_week)) {
        graphics::points(peak_week, group$cases_per_100k[group$week_end == peak_week],
                         pch = 1, cex = 1.8, lwd = 2, col = colours[country])
      }
      graphics::legend("topleft", c("Milestone observation", "Peak week"),
                       lty = c(2, NA), pch = c(NA, 1),
                       col = c("#526970", colours[country]), bty = "n")
    }
  }, height = function() if (length(input$countries) > 1L) 420 else 245,
     alt = "Weekly reported case rates by country. Dashed lines mark milestone observations and rings mark peak weeks. Dates and rates are in the comparison table.")

  output$coverage <- renderPlot({
    data <- selected()
    graphics::par(mar = c(3.4, 4.5, 1, 1))
    graphics::plot(data$week_end, data$coverage_pct, type = "n",
                   xlim = range(data$week_end) + c(-3, 3), ylim = c(0, 100),
                   xlab = "", ylab = "Residents (%)")
    graphics::grid(col = "#dbe6e8")
    graphics::abline(h = input$milestone, lty = 2, col = "#526970")
    shown <- unique(data$country)
    for (country in shown) {
      keep <- data$country == country
      graphics::lines(data$week_end[keep], data$coverage_pct[keep], type = "o",
                      lwd = 2, pch = if (country == "Australia") 16 else 17,
                      col = colours[country])
    }
    peaks <- comparison()
    graphics::points(as.Date(peaks[["Peak week ending"]]),
                     peaks[["Coverage in peak week (%)"]], pch = 1, cex = 1.8,
                     lwd = 2, col = colours[peaks$Country])
    graphics::legend("topleft", legend = shown, col = colours[shown],
                     pch = ifelse(shown == "Australia", 16, 17), bty = "n")
    if (all(is.na(data$coverage_pct))) {
      graphics::text(mean(range(data$week_end)), 45, "No coverage observations in these weeks.")
    }
  }, alt = "Vaccination coverage by reporting week. The dashed horizontal line is the threshold. Rings mark coverage in peak case weeks where readings are available.")
}

shinyApp(ui, server)
