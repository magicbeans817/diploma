library(gtrendsR)
library(tidyverse)
library(dplyr)
library(lubridate)

get_daily_gtrend <- function(keyword = c('Taylor Swift', 'Kim Kardashian'), geo = 'US', 
                             from = '2013-01-01', to = '2019-08-15') {
  if (ymd(to) >= floor_date(Sys.Date(), 'month')) {
    to <- floor_date(ymd(to), 'month') - days(1)
    
    if (to < from) {
      stop("Specifying \'to\' date in the current month is not allowed")
    }
  }
  
  aggregated_data <- gtrends(keyword = keyword, geo = geo, time = paste(from, to))
  if(is.null(aggregated_data$interest_over_time)) {
    print('There is no data in Google Trends!')
    return()
  }
  
  mult_m <- aggregated_data$interest_over_time %>%
    mutate(hits = as.integer(ifelse(hits == '<1', '0', hits))) %>%
    group_by(month = floor_date(date, 'month'), keyword) %>%
    summarise(hits = sum(hits)) %>%
    ungroup() %>%
    mutate(ym = format(month, '%Y-%m'),
           mult = hits / max(hits)) %>%
    select(month, ym, keyword, mult) %>%
    as_tibble()
  
  pm <- tibble(s = seq(ymd(from), ymd(to), by = 'month'), 
               e = seq(ymd(from), ymd(to), by = 'month') + months(1) - days(1))
  
  raw_trends_m <- tibble()
  
  for (i in seq(1, nrow(pm), 1)) {
    curr <- gtrends(keyword, geo = geo, time = paste(pm$s[i], pm$e[i]))
    if(is.null(curr$interest_over_time)) next
    print(paste('for', pm$s[i], pm$e[i], 'retrieved', count(curr$interest_over_time), 'days of data (all keywords)'))
    raw_trends_m <- rbind(raw_trends_m,
                          curr$interest_over_time)
  }
  
  trend_m <- raw_trends_m %>%
    select(date, keyword, hits) %>%
    mutate(ym = format(date, '%Y-%m'),
           hits = as.integer(ifelse(hits == '<1', '0', hits))) %>%
    as_tibble()
  
  trend_res <- trend_m %>%
    left_join(mult_m) %>%
    mutate(est_hits = hits * mult) %>%
    select(date, keyword, est_hits) %>%
    as_tibble() %>%
    mutate(date = as.Date(date))
  
  return(trend_res)
}

data <- get_daily_gtrend(keyword = c('Taylor Swift', 'Kim Kardashian'),
                         geo = 'US', from = '2010-01-01', to = '2022-01-01')

data %>% head

View(data)








