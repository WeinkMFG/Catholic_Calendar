# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(lubridate)
library(lunar)
library(stringr)


# Read data ---------------------------------------------------------------

special.days <- read_excel(
  here("data/special_days.xlsx"),
  sheet = "Special_Days",
  col_types = c(rep("numeric", 2),
                "text",
                "logical",
                rep("text", 2))
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
  christmas.end <-
    epiphany$date %m+% days(((epiphany$day.of.week - 7) + 1))
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

# Parameters:
# calendar: a calendar object as returned by time.of.year.attribution()
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
    
    # Write special days in calendar
    calendar <-
      left_join(
        calendar,
        select(
          special.days,
          date,
          type,
          is_martyr,
          name_german,
          name_english
        ),
        by = c("date" = "date")
      )
    
    # Resolve collisions with moving days
    # In order of priorities to overwrite when necessary
    
    ## Priority II.7: Festivities of Mary and general festivities (from special.days)
    search.date <-
      pull(calendar[max(which(calendar[["time.of.year"]] == "easter")), "date"]) %m+% days(1)
    calendar[which(calendar[["date"]] == search.date), "type"] <-
      "memory"
    calendar[which(calendar[["date"]] == search.date), "name_german"] <-
      "Maria, Mutter der Kirche"
    calendar[which(calendar[["date"]] == search.date), "name_english"] <-
      "Mary, Mother of the Church"
    
    search.date <-
      pull(calendar[max(which(calendar[["time.of.year"]] == "easter")), "date"]) %m+% days(20)
    calendar[which(calendar[["date"]] == search.date), "type"] <-
      "memory"
    calendar[which(calendar[["date"]] == search.date), "name_german"] <-
      "Unbeflecktes Herz Mariens"
    calendar[which(calendar[["date"]] == search.date), "name_english"] <-
      "Immaculate Heart of Mary"
    
    ## Priority II.6: Sundays in Christmas time and ordinary time festivities (but overruled by high festivities)
    sundays <- list()
    sundays$pos <-
      which(calendar[["time.of.year"]] == "ordinary" &
              calendar[["day.of.week"]] == 1)
    sundays$names.german <-
      paste(2:(length(sundays$pos) + 1), ". Sonntag im Jahreskreis", sep = "")
    sundays$names.english <-
      paste(2:(length(sundays$pos) + 1), "th Sunday of the year", sep = "")
    sundays$high <-
      which(calendar[["time.of.year"]] == "ordinary" &
              calendar[["day.of.week"]] == 1 & calendar[["type"]] == "high")
    if (length(sundays$high) > 0) {
      rm <- which(sundays$pos %in% sundays$high)
      sundays$pos <- sundays$pos[-rm]
      sundays$names.german <- sundays$names.german[-rm]
      sundays$names.english <- sundays$names.english[-rm]
    }
    calendar[sundays$pos, "is_martyr"] <- NA
    calendar[sundays$pos, "name_german"] <- sundays$names.german
    calendar[sundays$pos, "name_english"] <- sundays$names.english
    
    ## Priority II.5: Lord's festivities
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
    
    calendar[max(which(calendar[["time.of.year"]] == "christmas")), "type"] <-
      "festivity"
    calendar[max(which(calendar[["time.of.year"]] == "christmas")), "name_german"] <-
      "Taufe des Herrn"
    calendar[max(which(calendar[["time.of.year"]] == "christmas")), "name_english"] <-
      "Baptism of the Lord"
    
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
    
    calendar[max(which(calendar[["time.of.year"]] == "ordinary" &
                         calendar[["day.of.week"]] == 1)), "type"] <-
      "high"
    calendar[max(which(calendar[["time.of.year"]] == "ordinary" &
                         calendar[["day.of.week"]] == 1)), "name_german"] <-
      "Christkönigsonntag"
    calendar[max(which(calendar[["time.of.year"]] == "ordinary" &
                         calendar[["day.of.week"]] == 1)), "name_english"] <-
      "Christ the King Sunday"
    
    # Priority I.2: Christmas, Appearance of the Lord, Ascension Day, Pentecost, Advent Sundays,
    # Lent Sundays, Easter Sundays, Ash Wednesday
    sundays <-
      which(calendar[["time.of.year"]] == "advent" &
              calendar[["day.of.week"]] == 1)
    calendar[sundays, "is_martyr"] <- NA
    calendar[sundays, "name_german"] <-
      paste(1:4, ". Adventssonntag", sep = "")
    calendar[sundays, "name_english"] <-
      paste(1:4, "th Sunday of Advent", sep = "")
    
    sundays <-
      which(calendar[["time.of.year"]] == "lent" &
              calendar[["day.of.week"]] == 1)
    calendar[sundays, "is_martyr"] <- NA
    calendar[sundays, "name_german"] <-
      paste(1:length(sundays), ". Fastensonntag", sep = "")
    calendar[sundays, "name_english"] <-
      paste(1:length(sundays), "th Sunday of Lent", sep = "")
    
    sundays <-
      which(calendar[["time.of.year"]] == "easter" &
              calendar[["day.of.week"]] == 1)
    calendar[sundays, "is_martyr"] <- NA
    calendar[sundays, "name_german"] <-
      paste(1:length(sundays), ". Sonntag der Osterzeit", sep = "")
    calendar[sundays, "name_english"] <-
      paste(1:length(sundays), "th Sunday of Easter", sep = "")
    
    calendar[min(which(calendar[["time.of.year"]] == "lent")), "type"] <-
      "high"
    calendar[min(which(calendar[["time.of.year"]] == "lent")), "name_german"] <-
      "Aschermittwoch"
    calendar[min(which(calendar[["time.of.year"]] == "lent")), "name_english"] <-
      "Ash Wednesday"
    
    calendar[max(which(calendar[["time.of.year"]] == "easter")) - 10, "type"] <-
      "high"
    calendar[max(which(calendar[["time.of.year"]] == "easter")) - 10, "name_german"] <-
      "Christi Himmelfahrt"
    calendar[max(which(calendar[["time.of.year"]] == "easter")) - 10, "name_english"] <-
      "Ascension Day"
    
    calendar[max(which(calendar[["time.of.year"]] == "easter")), "type"] <-
      "high"
    calendar[max(which(calendar[["time.of.year"]] == "easter")), "name_german"] <-
      "Pfingsten"
    calendar[max(which(calendar[["time.of.year"]] == "easter")), "name_english"] <-
      "Pentecost"
    
    calendar[min(which(calendar[["time.of.year"]] == "easter")) - 7, "name_german"] <-
      "Palmsonntag"
    calendar[min(which(calendar[["time.of.year"]] == "easter")) - 7, "name_english"] <-
      "Palm Sunday"
    
    calendar[min(which(calendar[["time.of.year"]] == "easter")) - 3, "name_german"] <-
      "Gründonnerstag"
    calendar[min(which(calendar[["time.of.year"]] == "easter")) - 3, "name_english"] <-
      "Maundy Thursday"
    
    calendar[min(which(calendar[["time.of.year"]] == "easter")) - 2, "name_german"] <-
      "Karfreitag"
    calendar[min(which(calendar[["time.of.year"]] == "easter")) - 2, "name_english"] <-
      "Good Friday"
    
    calendar[min(which(calendar[["time.of.year"]] == "easter")) - 1, "name_german"] <-
      "Karsamstag"
    calendar[min(which(calendar[["time.of.year"]] == "easter")) - 1, "name_english"] <-
      "Holy Saturday"
    
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
    
    search.date <-
      pull(calendar[max(which(calendar[["time.of.year"]] == "easter")), "date"]) %m+% days(19)
    calendar[which(calendar[["date"]] == search.date), "type"] <-
      "high"
    calendar[which(calendar[["date"]] == search.date), "name_german"] <-
      "Herz Jesu"
    calendar[which(calendar[["date"]] == search.date), "name_english"] <-
      "Heart of Christ"
    
    
    #Return calendar with festivities
    return(calendar)
  }

# Add lithurgical colours -------------------------------------------------

# https://www.vivat.de/magazin/christliches-leben/gottesdienst/liturgische-farben/
# https://www.messdiener-budberg.de/Inhalte/Allgemein/liturgische_farben.htm

# Parameters:
# calendar: a calendar object as returned by special.days.attribution()

lithurgical.colours <- function(calendar) {
  # Create vector to hold information
  cols <- vector(mode = "character", length = nrow(calendar))
  
  # Add colours to vector
  ## Green
  pos <- which(calendar[["time.of.year"]] == "ordinary")
  cols[pos] <- "green"
  ## Violet / rose
  pos <-
    which(calendar[["time.of.year"]] == "advent" |
            calendar[["time.of.year"]] == "lent")
  cols[pos] <- "purple3"
  pos <-
    which(calendar[["time.of.year"]] == "lent" &
            calendar[["day.of.week"]] == 1)[4]
  pos <-
    append(pos, which(calendar[["time.of.year"]] == "advent" &
                        calendar[["day.of.week"]] == 1)[3])
  cols[pos] <- "violet"
  ## Black
  pos <- which(calendar[["name_german"]] == "Allerseelen")
  cols[pos] <- "black"
  ## Red
  pos <-
    which(calendar[["name_english"]] %in% c(
      "Palm Sunday",
      "Good Friday",
      "Pentecost",
      "Exaltation of the Cross"
    ))
  pos <- append(pos, which(calendar[["is_martyr"]] == TRUE))
  pos <-
    append(pos, str_which(calendar[["name_german"]], "Evgl\\."))
  pos <-
    append(pos, str_which(calendar[["name_german"]], "Apstl\\."))
  cols[pos] <- "red"
  ## White
  pos <- which(cols == "")
  pos <-
    append(pos, which(calendar[["type"]] == "high" &
                        !(
                          calendar[["name_english"]] %in% c(
                            "Palm Sunday",
                            "Good Friday",
                            "Pentecost",
                            "Exaltation of the Cross"
                          )
                        )))
  pos <- append(pos, str_which(calendar[["name_german"]], "Maria"))
  pos <- append(pos, str_which(calendar[["name_german"]], "Mariä"))
  pos <-
    append(pos, str_which(calendar[["name_german"]], "Mariens"))
  pos <-
    append(pos, which(
      calendar[["name_german"]] %in% c("Kathedra Petri", "Bekehrung des Apostel Paulus")
    ))
  cols[pos] <- "white"
  
  # Combine data
  calendar <- calendar %>%
    add_column(colour = as_factor(cols))
  
  # Return calendar with colours
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
  
  # Assign colours
  calendar <- lithurgical.colours(calendar)
  
  # Return results
  return(calendar)
}

# TO DO: When a High festivity is replaced by an automation, is it moved to the next free day that is not of ranks 1-8

test <- catholic.calendar(2023, special.days = special.days)
write_csv(test, here("calendars/test_calendar_2024.csv"))
