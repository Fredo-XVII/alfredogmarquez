# Count R download Stats

library(cranlogs)
library(lubridate)
library(tidyverse)
library(tsibble)
library(gtrendsR)

# Download R download data 
cran_R_downloads_raw <- cranlogs::cran_downloads('R', from = "2010-01-01", to = lubridate::today())
cran_R_downloads <- cran_R_downloads_raw %>% 
  filter(!version %in% c('devel','release','release.exe','latest')) %>% 
  mutate(os_factor = forcats::fct_inorder(forcats::as_factor(os)),
         ver_factor =forcats::fct_inorder(forcats::as_factor(version)),
         ver_lvl = sub("*[.^]", "", version) %>% sub("\\..*", "", .) %>% as.integer(),
         ver_lvl_factor =forcats::fct_inorder(forcats::as_factor(ver_lvl)),
         ver_lvl_top = stringr::str_sub(version,1,1) %>% as.integer(),
         ver_lvl_top_factor =forcats::fct_inorder(forcats::as_factor(ver_lvl_top)),
         year = lubridate::year(date),
         month = lubridate::month(date),
         week = lubridate::week(date),
         yr_mo = tsibble::yearmonth(date),
         yr_wk = tsibble::yearweek(date)
  )
  


# filter out devel

# EDA
cran_R_downloads %>% head()

cran_R_downloads %>% select(os) %>% group_by(os) %>% summarise(os_cnts = n())

cran_R_downloads %>% select(date) %>% distinct() %>% nrow()

cran_R_downloads %>% select(ver_lvl) %>% dplyr::group_by(ver_lvl) %>% summarise(version_cnts = n())

# Build Aggregation Views
# What does the trend look like?
down_tot_by_d <- cran_R_downloads %>% 
  dplyr::group_by(yr_wk) %>% 
  dplyr::summarise(total = sum(count)) %>% 
  tsibble::as_tsibble()

down_tot_by_d %>% ggplot(aes(x = yr_wk, y = total)) +
  geom_line()

down_tot_by_d %>% ggplot(aes(x = yr_wk, y = log10(total))) +
  geom_line() 

# What does the trend look like by os
down_tot_by_os <- cran_R_downloads %>% 
  dplyr:: group_by(yr_wk,os_factor) %>% 
  dplyr::summarise(total = sum(count))

down_tot_by_os %>% ggplot(aes(x = yr_wk, y = total, col = os_factor, group = os_factor)) +
  geom_line() +
  facet_wrap(.~os_factor) 

down_tot_by_os %>% ggplot(aes(x = yr_wk, y = log10(total), col = os_factor, group = os_factor)) +
  geom_line()

# What does the trend look like by version
down_tot_by_ver <- cran_R_downloads %>% 
  dplyr:: group_by(yr_wk,ver_lvl_factor,ver_lvl_top_factor) %>% 
  dplyr::summarise(total = sum(count))

down_tot_by_ver %>% ggplot(aes(x = yr_wk, y = total, col = ver_lvl_factor, group = ver_lvl_factor)) +
  geom_line() +
  facet_wrap(.~ver_lvl_top_factor)

down_tot_by_ver %>% ggplot(aes(x = yr_wk, y = log10(total), col = ver_lvl_factor, group = ver_lvl_factor)) +
  geom_line() +
  facet_wrap(.~ver_lvl_factor)

## Top Level

# Google Trends: Why is there a bump in the fall of 2018.

R_gtrends <- gtrendsR::gtrends(c("R",'r'))

r_trends_t <- R_gtrends$interest_over_time %>% 
  dplyr::mutate(yr_wk = tsibble::yearweek(date),
                hits_q = as.numeric(hits))

r_trends_t %>% head()

r_trends_tot_wk <- r_trends_t %>% 
  dplyr::group_by(yr_wk) %>% 
  dplyr::summarise(total = sum(hits_q)) 

r_trends_tot_wk %>% ggplot(aes(x = yr_wk, y = total)) +
  geom_line()

r_trends_tot_wk %>% ggplot(aes(x = yr_wk, y = log10(total))) +
  geom_line()
