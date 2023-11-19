# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)

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



calendar.generation(year = 2023)

