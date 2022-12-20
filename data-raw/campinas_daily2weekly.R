# Convert Campinas, SP, Brazil daily precip to weekly
# Daily dataset extracted from CHIRPS v2.0, for coordinates lon -47, lat -23
# Daniel de Castro Victoria - 2022-12-19
# daniel.victoria@embrapa.br

cmp <- jsonlite::fromJSON('data-raw/daily_precip_campinas_chirps.json')
names(cmp) <- c('date', 'prcp')
cmp$date <- as.Date(cmp$date)

# using lubridate::wek in order to allways have 53 weeks per year
# My tests indicated that a TS object with a fractional frequency
# will not work with SPEI
cmp$year_week <- sprintf('%s-%02i', lubridate::year(cmp$date),
                         lubridate::week(cmp$date))

campinas <- aggregate(prcp ~ year_week,
                      data = cmp,
                      FUN = sum)

first_day <- aggregate(date ~ year_week,
          data = cmp,
          FUN = min)

campinas <- merge(campinas, first_day)
campinas <- campinas[c('year_week', 'date', 'prcp')]

save(campinas, file = 'data/campinas.rda')
