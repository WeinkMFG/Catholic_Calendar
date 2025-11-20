# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(stringr)
source(here("Calendar_Calculations.R"))
library(openxlsx)

# Generate calendar -------------------------------------------------------

calendar <- catholic.calendar(special.days = special.days)

# Set up general calendar pages -------------------------------------------

# Set up helper functions -------------------------------------------------

# Distribute the dates over the calendar rows =============================

# Parameters:
# calendar: a calendar object as created by catholic.calendar()

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

  # Return results
  return(calendar)
}

# Distribute values across a calendar print grid ==========================

# Parameters:
# calendar: a calendar object as created by distribute.days()
# cal.locale: the locale used for weekday names and month names,
#             defaults to the locale of the OS
#             common other values are "fr_FR", "de_DE", "en_US", and "en_GB"

distribute.grid <- function(calendar, cal.locale = Sys.getlocale("LC_TIME")) {
  # Generate calendar grid
  weekdays <- as.character(wday(
    1:7,
    label = TRUE,
    week_start = 7,
    abbr = FALSE,
    locale = cal.locale
  ))
  calendar.dates.grid <- tibble(
    !!!setNames(
      rep(list(character(max(advent.page[["calendar.row"]]))), 7),
      weekdays
    )
  )

  # Perform distribution across grid
  for (i in seq_len(nrow(calendar))) {
    entry <- pull(calendar[i, "date"])
    entry <- paste(
      as.character(day(entry)),
      as.character(month(
        entry,
        label = TRUE,
        abbr = TRUE,
        locale = cal.locale
      )),
      sep = " "
    )
    row <- pull(calendar[i, "calendar.row"])
    col <- pull(calendar[i, "day.of.week"])
    calendar.dates.grid[row, col] <- entry
  }

  # Return results
  return(calendar.dates.grid)
}

# Generate calendar page --------------------------------------------------

advent.page <- filter(calendar, time.of.year == "advent")
advent.page <- distribute.days(advent.page)

weekdays <- as.character(wday(1:7, label = TRUE, week_start = 7, abbr = FALSE))
calendar.dates.grid <- tibble(
  !!!setNames(
    rep(list(character(max(advent.page[["calendar.row"]]))), 7),
    weekdays
  )
)

for (i in seq_len(nrow(advent.page))) {
  entry <- pull(advent.page[i, "date"])
  entry <- paste(
    as.character(day(entry)),
    as.character(month(entry, label = TRUE, abbr = TRUE)),
    sep = " "
  )
  row <- pull(advent.page[i, "calendar.row"])
  col <- pull(advent.page[i, "day.of.week"])
  calendar.dates.grid[row, col] <- entry
}
