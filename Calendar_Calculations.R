# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(lubridate)
library(lunar)


# Read data ---------------------------------------------------------------

special.days <- read_excel(
  here("data/special_days.xlsx"),
  sheet = "Special_Days",
  col_types = c(rep("numeric", 2),
                rep("text", 3))
) %>%
  mutate(month = as.integer(month),
         day = as.integer(day))

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

# Parameters:
# calendar: a calendar object as returned by calendar.generation()

time.of.year.attribution <- function(calendar) {
  # Get year from calendar
  start.year = year(pull(calendar[1, "date"]))
  
  # Add column
  calendar <- calendar %>%
    mutate(time.of.year = vector(mode = "character", length = nrow(calendar)))
  
  # Advent
  christmas.row <-
    which(calendar[["date"]] == ymd(paste(start.year, "12", "24", sep = "-")))
  calendar[1:christmas.row, "time.of.year"] <- "advent"
  
  # Christmas
  epiphany <- list()
  epiphany$date <- ymd(paste(start.year + 1, "01", "06", sep = "-"))
  epiphany$day.of.week <- wday(epiphany$date, week_start = 7)
  christmas.end <- epiphany$date %m+% days(((epiphany$day.of.week - 7) + 1))
  christmas.end <-
    which(calendar[["date"]] == christmas.end)
  calendar[(christmas.row + 1):christmas.end, "time.of.year"] <-
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
  
  ## Find first Sunday that is at least one day after the first full moon
  moon.search$potential.easter <-
    moon.search$weekdays[moon.search$first.full + 1:(length(moon.search$weekdays) - moon.search$first.full)]
  moon.search$easter.sunday <-
    min(which(moon.search$potential.easter == 1))
  easter.sunday <-
    moon.search$range[moon.search$easter.sunday + moon.search$first.full]
  ash.wednesday <- easter.sunday %m-% days(46)
  pentecost.sunday <- easter.sunday %m+% days(49)
  
  ## Determine time intervals that result from Easter
  ash.row <- which(calendar[["date"]] == ash.wednesday)
  easter.row <- which(calendar[["date"]] == easter.sunday)
  pentecost.row <- which(calendar[["date"]] == pentecost.sunday)
  calendar[(christmas.end + 1):(ash.row - 1), "time.of.year"] <-
    "ordinary"
  calendar[ash.row:(easter.row - 1), "time.of.year"] <-
    "lent"
  calendar[easter.row:pentecost.row, "time.of.year"] <-
    "easter"
  calendar[(pentecost.row + 1):nrow(calendar), "time.of.year"] <-
    "ordinary"
  
  # Return calendar with time of year
  return(calendar)
}

# Add special days --------------------------------------------------------

# https://www.stundenbuch-online.de/home.php?p=305&dev=d
# https://www.stundenbuch-online.de/home.php?p=307
# https://www.stundenbuch-online.de/home.php#top
# test <- catholic.calendar(year = 2023)
# calendar <- test

# Parameters:
# calendar: a calendar object as returned by calendar.generation()
# special.days: a tibble with general dates of special days,
#               columns month, day, type, name_german, and name_english
#               by default stored in data/special_days.xlsx

special.days.attribution <-
  function(calendar, special.days = special.days) {
    # Get year from calendar
    start.year = year(pull(calendar[1, "date"]))
    
    # Prepare the special days
    special.days <- special.days %>%
      mutate(year = case_when(month == 12 ~ start.year,
                              TRUE ~ start.year + 1),
             .before = "month") %>%
      mutate(date = paste(
        year,
        str_pad(
          month,
          width = 2,
          side = "left",
          pad = "0"
        ),
        str_pad(
          day,
          width = 2,
          side = "left",
          pad = "0"
        ),
        sep = "-"
      ),
      .after = "day") %>%
      mutate(date = ymd(date)) %>%
      mutate(day.of.week = wday(date, week_start = 7),
             .after = "date")
    
    # TO FIX: This adds one to each date after 25. Feb, which is wrong.
    #         What I need is to leave the dates untouched but instead move all festivities.
    #         Example: Pentecost should be 19 May 2024, which is a Sunday, but with this code
    #                  is moved to 20 May because this becomes a Sunday ("2024-05-19" %m+% days(1))
    # Adapt all dates if dealing with leap year
    # if (leap_year(start.year + 1)) {
    #   calendar <- calendar %>%
    #     mutate(date = case_when(date <= ymd(
    #       paste(as.character(start.year + 1), "2", "25", sep = "-")
    #     ) ~ date,
    #     TRUE ~ date %m+% days(1)))
    # }
    
    # Write special days in calendar
    calendar <-
      left_join(
        calendar,
        select(special.days, date, type, name_german, name_english),
        by = c("date" = "date")
      )
    
    # Resolve collisions with moving days
    ## Baptism of the Lord
    calendar[max(which(calendar[["time.of.year"]] == "christmas")), "type"] <-
      "festivity"
    calendar[max(which(calendar[["time.of.year"]] == "christmas")), "name_german"] <-
      "Taufe des Herrn"
    calendar[max(which(calendar[["time.of.year"]] == "christmas")), "name_english"] <-
      "Baptism of the Lord"
    
    ## Easter and Pentecost
    calendar[min(which(calendar[["time.of.year"]] == "easter")), "type"] <-
      "high"
    calendar[min(which(calendar[["time.of.year"]] == "easter")), "name_german"] <-
      "Auferstehung des Herrn"
    calendar[min(which(calendar[["time.of.year"]] == "easter")), "name_english"] <-
      "Resurrection of the Lord"
    
    search.date <-
      pull(calendar[min(which(calendar[["time.of.year"]] == "easter")), "date"]) %m+% days(7)
    calendar[which(calendar[["date"]] == search.date), "type"] <-
      "festivity"
    calendar[which(calendar[["date"]] == search.date), "name_german"] <-
      "Sonntag der göttlichen Barmherzigkeit"
    calendar[which(calendar[["date"]] == search.date), "name_english"] <-
      "Sunday of Divine Mercy"
    
    calendar[max(which(calendar[["time.of.year"]] == "easter")), "type"] <-
      "high"
    calendar[max(which(calendar[["time.of.year"]] == "easter")), "name_german"] <-
      "Pfingsten"
    calendar[max(which(calendar[["time.of.year"]] == "easter")), "name_english"] <-
      "Pentecost"
    
    search.date <-
      pull(calendar[max(which(calendar[["time.of.year"]] == "easter")), "date"]) %m+% days(1)
    calendar[which(calendar[["date"]] == search.date), "type"] <-
      "memory"
    calendar[which(calendar[["date"]] == search.date), "name_german"] <-
      "Maria, Mutter der Kirche"
    calendar[which(calendar[["date"]] == search.date), "name_english"] <-
      "Mary, Mother of the Church"
    
    search.date <-
      pull(calendar[max(which(calendar[["time.of.year"]] == "easter")), "date"]) %m+% days(7)
    calendar[which(calendar[["date"]] == search.date), "type"] <-
      "high"
    calendar[which(calendar[["date"]] == search.date), "name_german"] <-
      "Dreifaltigkeitssonntag"
    calendar[which(calendar[["date"]] == search.date), "name_english"] <-
      "Holy Trinity Sunday"
    
    search.date <-
      pull(calendar[max(which(calendar[["time.of.year"]] == "easter")), "date"]) %m+% days(11)
    calendar[which(calendar[["date"]] == search.date), "type"] <-
      "high"
    calendar[which(calendar[["date"]] == search.date), "name_german"] <-
      "Fronleichnam"
    calendar[which(calendar[["date"]] == search.date), "name_english"] <-
      "Corpus Christi"
    
    ## Post-Pentecost
    search.date <-
      pull(calendar[max(which(calendar[["time.of.year"]] == "easter")), "date"]) %m+% days(19)
    calendar[which(calendar[["date"]] == search.date), "type"] <-
      "high"
    calendar[which(calendar[["date"]] == search.date), "name_german"] <-
      "Herz Jesu"
    calendar[which(calendar[["date"]] == search.date), "name_english"] <-
      "Heart of Christ"
    
    search.date <-
      pull(calendar[max(which(calendar[["time.of.year"]] == "easter")), "date"]) %m+% days(20)
    calendar[which(calendar[["date"]] == search.date), "type"] <-
      "memory"
    calendar[which(calendar[["date"]] == search.date), "name_german"] <-
      "Unbeflecktes Herz Mariens"
    calendar[which(calendar[["date"]] == search.date), "name_english"] <-
      "Immaculate Heart of Mary"
    
    ## Christ King
    calendar[max(which(calendar[["time.of.year"]] == "ordinary" &
                         calendar[["day.of.week"]] == 1)), "type"] <-
      "high"
    calendar[max(which(calendar[["time.of.year"]] == "ordinary" &
                         calendar[["day.of.week"]] == 1)), "name_german"] <-
      "Christkönigsonntag"
    calendar[max(which(calendar[["time.of.year"]] == "ordinary" &
                         calendar[["day.of.week"]] == 1)), "name_english"] <-
      "Christ the King Sunday"
    
    ## Holy family
    {
      if (wday(ymd(paste(start.year, "12", "25", sep = "-")), week_start = 7) == 1) {
        calendar[which(calendar[["date"]] == ymd(paste(start.year, "12", "30", sep = "-"))), "type"] <-
          "festivity"
        calendar[which(calendar[["date"]] == ymd(paste(start.year, "12", "30", sep = "-"))), "name_german"] <-
          "Fest der Heiligen Familie"
        calendar[which(calendar[["date"]] == ymd(paste(start.year, "12", "30", sep = "-"))), "name_english"] <-
          "Feast of the Holy Family"
      }
      else {
        calendar[min(which(calendar[["time.of.year"]] == "christmas" &
                             calendar[["day.of.week"]] == 1)), "type"] <-
          "festivity"
        calendar[min(which(calendar[["time.of.year"]] == "christmas" &
                             calendar[["day.of.week"]] == 1)), "name_german"] <-
          "Fest der Heiligen Familie"
        calendar[min(which(calendar[["time.of.year"]] == "christmas" &
                             calendar[["day.of.week"]] == 1)), "name_english"] <-
          "Feast of the Holy Family"
      }
    }
    
    #Return calendar with festivities
    return(calendar)
  }

# Utility function to combine all functions -------------------------------

# Parameters:
# year: integer value for year in which calendar should start
# special.days: a tibble with general dates of special days,
#               columns month, day, type, name_german, and name_english
#               by default stored in data/special_days.xlsx

catholic.calendar <- function(year, special.days) {
  # Generate raw calendar
  calendar <- calendar.generation(year = year)
  
  # Assign time of year
  calendar <- time.of.year.attribution(calendar)
  
  # Assign festivities
  calendar <- special.days.attribution(calendar, special.days)
  
  # Return results
  return(calendar)
}

