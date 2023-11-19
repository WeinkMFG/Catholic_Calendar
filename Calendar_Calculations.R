# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(lunar)

# Generate raw calendar ---------------------------------------------------

# Parameters:
# year: integer value for year in which calendar should start

calendar.generation <- function(year) {
  # Calculate start and end date for calendar
  time.of.year <- list()
  
  ## Christmas date in current year for start date of calendar
  time.of.year$this.year$christmas$date <-
    ymd(paste(year, "-12-24", sep = ""))
  time.of.year$this.year$christmas$day.of.week <-
    wday(time.of.year$this.year$christmas$date, week_start = 7)
  time.of.year$this.year$advent.start <-
    time.of.year$this.year$christmas$date - days(27 - (7 - time.of.year$this.year$christmas$day.of.week))
  
  ## Christmas date in next year for end of calendar
  time.of.year$next.year$christmas$date <-
    ymd(paste(year + 1, "-12-24", sep = ""))
  time.of.year$next.year$christmas$day.of.week <-
    wday(time.of.year$next.year$christmas$date, week_start = 7)
  time.of.year$next.year$advent.start <-
    time.of.year$next.year$christmas$date - days(27 - (7 - time.of.year$next.year$christmas$day.of.week))
  
  # Generate date range for year
  day.range <- c(
    time.of.year$this.year$advent.start,
    time.of.year$next.year$advent.start %m-% days(1)
  )
  date.metadata <-
    tibble(date = seq(ymd(day.range[1]), ymd(day.range[2]), by = "days"))
  date.metadata <- date.metadata %>%
    mutate(day.of.week = wday(date, week_start = 7))
  
  # Return raw calendar
  return(date.metadata)
}

# Add information on time of year -----------------------------------------

test <- calendar.generation(year = 2023)

time.of.year.attribution <- function(calendar) {
  # Get year from calendar
  start.year = year(pull(calendar[1, "date"]))
  
  # Add column
  calendar <- calendar %>%
    mutate(time.of.year = vector(mode = "character", length = nrow(calendar)))
  
  # Advent
  christmas.row = which(calendar[["date"]] == paste(start.year, "12", "24", sep = "-"))
  calendar[1:christmas.row, "time.of.year"] <- "advent"
  
  # Christmas
  christmas.row <- christmas.row + 1
  christmas.end <-
    which(calendar[["date"]] == paste(start.year + 1, "01", "08", sep = "-"))
  calendar[christmas.row:christmas.end, "time.of.year"] <-
    "christmas"
  
  # Lent and Easter
  moon.search <- list()
  
  ## Find potential Easter dates
  moon.search$dates <-
    c(start = ymd(paste(start.year + 1, "03", "21", sep = "-")),
      end = ymd(paste(start.year + 1, "04", "25", sep = "-")))
  moon.search$range <-
    seq(from = moon.search$dates["start"],
        to = moon.search$dates["end"],
        by = "days")
  
  ## Determine moon phase and weekday for potential dates
  moon.search$phases <- lunar.phase(moon.search$range, name = FALSE)
  moon.search$weekdays <- wday(moon.search$range, week_start = 7)
  ## Find first full moon in spring
  moon.search$first.full <- min(which(moon.search$phases >= pi))
  
  ## Find first sunday that is at least one day after the first full moon
  moon.search$potential.easter <-
    moon.search$weekdays[moon.search$first.full + 1:(length(moon.search$weekdays) - moon.search$first.full)]
  moon.search$easter.sunday <- min(which(moon.search$potential.easter == 1))
  easter.sunday <- moon.search$range[moon.search$easter.sunday + moon.search$first.full]
  ash.wednesday <- easter.sunday %m-% days(46)

}
