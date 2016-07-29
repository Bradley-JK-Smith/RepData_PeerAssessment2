# Load packages

require(data.table)
require(dplyr)
require(lubridate)
require(ggplot2)
require(grid)

fix_EVTYPE <- function(x, grep_string, replace_string) {
  idx <- grepl(grep_string, x$EVTYPE, ignore.case = TRUE )
  res <- x
  if( sum(idx) > 0 ) {
    cat('Found', paste0(unique(x[idx,'EVTYPE']), collapse=', '), '\n')
    cat('Replacing with', replace_string, '\n' )
    res[idx,'EVTYPE'] <- replace_string
  }
  return(res)
}

# WARNING: Need to check that setting stringsAsFactors to FALSE does not impact results

raw_data <- read.csv('repdata%2Fdata%2FStormData.csv.bz2', stringsAsFactors = FALSE)

clean_data <- as.data.table(raw_data) %>%
                mutate(YEAR = year(mdy_hms(BGN_DATE))) %>%
                mutate(PROPDMGVAL = PROPDMG * ifelse( PROPDMGEXP == 'K', 1e-3,
                                              ifelse( PROPDMGEXP == 'M', 1,
                                              ifelse( PROPDMGEXP == 'B', 1e3, 0))))


# GROUPING
# 1. TSTM or Thunderstorm or high wind or strong wind -> Thunderstorm
# 2. Hurricane -> Hurricane
# 3. Heat (eg. Excessive heat, heat wave, ) -> Heat
# 4. Flood (eg. flash flood, etc) -> Flood
# 5. Cold or Winter -> Cold
# 6. Rip -> Rip

clean_data <- fix_EVTYPE(clean_data, 'hurricane', 'HURRICANE')
clean_data <- fix_EVTYPE(clean_data, 'TSTM|thunderstorm', 'THUNDERSTORM')
clean_data <- fix_EVTYPE(clean_data, 'heat', 'HEAT')
clean_data <- fix_EVTYPE(clean_data, 'flood', 'FLOOD')
clean_data <- fix_EVTYPE(clean_data, 'cold|winter', 'COLD')
clean_data <- fix_EVTYPE(clean_data, 'rip', 'RIP')


time_data <- group_by(clean_data, YEAR) %>%
               summarise(total_death  = sum(FATALITIES),
                         total_damage = sum(PROPDMGVAL)
                        )

death_plot <- qplot(YEAR,
                    total_death,
                    data = time_data,
                    ylab = 'total fatalities',
                    geom = c('point', 'line')
                   )      

damage_plot <- qplot(YEAR,
                     total_damage,
                     data = time_data,
                     ylab = 'total damage (millions)',
                     geom = c('point', 'line')
                    )

grid.arrange( death_plot, damage_plot, ncol=1)

# Using recent data has 2 advantages
# Database is more complete
# Reflective of recent climatic conditions but more importantly
# reflective of CURRENT infrastructure and support.
# That is, don't want to have a response to damage/fatalities that
# have previously occurred where infrastructure and/or support
# has already been CHANGED

recent_data <- filter( clean_data, YEAR >= 1993 )

#table(recent_data$EVTYPE)

# Don't need to filter to just non-zero fatalities
# as the summation effectively does this.

death_data <- group_by(recent_data, EVTYPE) %>%
                summarise(total = sum(FATALITIES)) %>%
                arrange(desc(total))

# could use graph as well as table
# HEAT causes most deaths

print(death_data)

damage_data <- group_by(recent_data, EVTYPE) %>%
                summarise(total = sum(PROPDMGVAL)) %>%
                arrange(desc(total))

# could use graph as well as table
# FLOOD causes most damage. Should double check combination of
# hurricane based flooding.

print(damage_data)
                