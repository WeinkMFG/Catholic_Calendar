# Load packages -----------------------------------------------------------

source(here("Calendar_Calculations.R"))
library(openxlsx)

# Generate calendar -------------------------------------------------------

calendar <- catholic.calendar(special.days = special.days)

# Set up general calendar pages -------------------------------------------

# Set up helper functions -------------------------------------------------

# Distribute the date over the calendar rows ==============================

distribute.days <- function(calendar) {
  # Get the sequence of days of week and number them
  day.of.week.sequence <- calendar[["day.of.week"]]
  names(day.of.week.sequence) <- seq_along(day.of.week.sequence)

  # Find the first value with day of week = 1
  first.sunday <- min(as.numeric(names(day.of.week.sequence[
    day.of.week.sequence == 1
  ])))

  # Determine number of rows in page
  {
    if (first.sunday == 1) {
      n.rows <- length(day.of.week.sequence[day.of.week.sequence == 1])
    } else {
      n.rows <- length(day.of.week.sequence[day.of.week.sequence == 1]) + 1
    }
  }

  # Distribute data across rows of calendar page
  {
    if (first.sunday == 1) {
      calendar <- calendar %>%
        mutate(calendar.row = cumsum(day.of.week == 1))
    } else {
      calendar <- calendar %>%
        mutate(calendar.row = as.integer(cumsum(day.of.week == 1) + 1))
    }
  }

  return(calendar)
}

# Generate calendar page --------------------------------------------------

advent_page <- filter(calendar, time.of.year == "advent")
advent_page <- distribute.days(advent_page)

weekdays <- as.character(wday(1:7, label = TRUE, week_start = 7, abbr = FALSE))
tibble(
  !!!setNames(
    rep(list(character(max(advent_page[["calendar.row"]]))), 7),
    weekdays
  )
)
