# Count R download Stats

library(cranlogs)
library(lubridate)
library(tidyverse)
library(tsibble)

# Download R download data 
cran_R_downloads_raw <- cranlogs::cran_downloads('R', from = "2015-01-01", to = lubridate::today())
cran_R_downloads <- cran_R_downloads_raw %>% 
  mutate(os_factor = forcats::fct_inorder(forcats::as_factor(os)),
         ver_factor =forcats::fct_inorder(forcats::as_factor(version)),
         year = lubridate::year(date),
         month = lubridate::month(date),
         week = lubridate::week(date),
         yr_mo = stringr::str_c(year,'_',month),
         yr_wk = stringr::str_c(year,'_',week)
  )

# filter out devel

# EDA
cran_R_downloads %>% head() %>% View()

cran_R_downloads %>% select(os) %>% group_by(os) %>% summarise(os_cnts = n())

cran_R_downloads %>% select(date) %>% distinct() %>% nrow()

cran_R_downloads %>% select(version) %>% dplyr::group_by(version) %>% summarise(version_cnts = n())

# Build Aggregation Views
# What does the trend look like?
down_tot_by_d <- cran_R_downloads %>% 
  dplyr::group_by(yr_wk) %>% 
  dplyr::summarise(total = sum(count)) %>% 
  tsibble::as_tsibble()

down_tot_by_d %>% ggplot(aes(x = yr_wk, y = total)) +
  geom_line() +
  scale_y_log10()

# What does the trend look like by os
down_tot_by_os <- cran_R_downloads %>% 
  dplyr:: group_by(date,os_factor) %>% 
  dplyr::summarise(total = sum(count))

down_tot_by_os %>% ggplot(aes(x = date, y = total, col = os_factor, group = os_factor)) +
  geom_line() +
  facet_wrap(.~os_factor) +
  scale_y_log10()

# What does the trend look like by version
down_tot_by_ver <- cran_R_downloads %>% 
  dplyr:: group_by(date,ver_factor) %>% 
  dplyr::summarise(total = sum(count))

down_tot_by_ver %>% ggplot(aes(x = date, y = total, col = ver_factor, group = ver_factor)) +
  geom_line() +
  facet_wrap(.~ver_factor) +
  scale_y_log10()
