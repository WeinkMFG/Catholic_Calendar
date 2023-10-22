
# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)

# Generate raw calendar ---------------------------------------------------

calendar.generation <- function(year = 2023) {
  # Generate date range for year
  day.range <-
    c(paste(year, "-01-01", sep = ""), paste(year, "-12-31", sep = ""))
  date.metadata <-
    tibble(date = seq(ymd(day.range[1]), ymd(day.range[2]), by = "days"))
  date.metadata <- date.metadata %>%
    mutate(day.of.week = wday(date), week_start = 1)
  
  # Add information on time of year
  time.of.year <- list()
  time.of.year$christmas$date <- ymd(paste(year, "-12-24", sep = ""))
  time.of.year$christmas$day.of.week <- wday(time.of.year$christmas$date, week_start = 7)
  time.of.year$advent.start <- time.of.year$christmas$date - days(27 - (7 - time.of.year$christmas$day.of.week))
  
  date.metadata <- date.metadata %>%
    mutate(time.of.year = case_when(
      date >= time.of.year$advent.start & date <= time.of.year$christmas$date ~ "advent time",
      TRUE ~ "normal time"
    ))
}


