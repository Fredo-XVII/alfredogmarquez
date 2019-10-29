# Count R download Stats


# Libraries
library(cranlogs)
library(lubridate)
library(gridExtra)
library(ggrepel)
library(tidyverse)
library(tsibble)
library(gtrendsR)
library(rversions)


# Download R download data and version dates
R_ver_hist_raw <- rversions::r_versions() 
R_ver_hist_raw$vers_d <- as.POSIXct(R_ver_hist_raw$date)

R_ver_hist <- R_ver_hist_raw %>% dplyr::select(-date) %>% 
  dplyr::mutate(greg_d = lubridate::as_date(vers_d),
                vers_i = forcats::as_factor(version),
                nickname = forcats::as_factor(nickname),
                yr_wk_ver = tsibble::yearweek(as.Date(greg_d))
  ) %>% 
  dplyr::select(version,vers_i,nickname,greg_d,yr_wk_ver) %>% 
  dplyr::filter(greg_d >= as.Date('2015-01-01')) # %>%
  #dplyr::filter(!vers_i %in% c("3.2.0","3.3.0","3.4.0","3.5.0","3.6.0"))

R_ver_hist_major <- R_ver_hist %>%  
  dplyr::filter(vers_i %in% c("3.2.0","3.3.0","3.4.0","3.5.0","3.6.0"))

Scran_R_downloads_raw <- cranlogs::cran_downloads('R', from = "2010-01-01", to = lubridate::today())
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
  ) %>% 
  left_join(R_ver_hist, by = c("yr_wk" = "yr_wk_ver")) 
  

# filter out devel

# EDA
cran_R_downloads %>% head()

cran_R_downloads %>% select(os) %>% group_by(os) %>% summarise(os_cnts = n())

cran_R_downloads %>% select(date) %>% distinct() %>% nrow()

cran_R_downloads %>% select(ver_lvl) %>% dplyr::group_by(ver_lvl) %>% summarise(version_cnts = n())

# Build Aggregation Views
# What does the trend look at by Year
down_tot_by_yr <- cran_R_downloads %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(total = sum(count)) %>% 
  dplyr::mutate(yoy = total/dplyr::lag(total),
                yoy_perc = (total/dplyr::lag(total) - 1)*100,
                ) %>% 
  tsibble::as_tsibble(index = year)

down_tot_by_yr %>% 
  ggplot(aes(x = year, y = total, color = year)) +
  geom_line()

# What does the trend look like?
down_tot_by_d <- cran_R_downloads %>% 
  dplyr::group_by(yr_wk,greg_d,forcats::fct_explicit_na(vers_i)) %>% 
  dplyr::summarise(total = sum(count)) %>% 
  tsibble::as_tsibble(index = yr_wk)

down_tot_by_d %>% 
  ggplot(aes(x = yr_wk, y = total, color = greg_d)) +
  geom_line() +
  labs(x = 'Week', y = 'Weekly Downloads') +
  geom_vline(data = R_ver_hist, aes(xintercept = greg_d), linetype = 4, color = "red", alpha = 0.5) +
  geom_vline(data = R_ver_hist_major, aes(xintercept = greg_d), linetype = 1, color = "blue") 



# What does the trend look like by os
down_tot_by_os <- cran_R_downloads %>% 
  dplyr:: group_by(os_factor,yr_wk) %>% 
  dplyr::summarise(total = sum(count)) %>% 
  dplyr::mutate(yoy = total/dplyr::lag(total, n = 52),
                yoy_perc = (total/dplyr::lag(total,n = 52) - 1)*100,
  )

g_os_lvl <- down_tot_by_os %>% 
  ggplot(aes(x = yr_wk, y = total, col = os_factor, group = os_factor)) +
  geom_line() +
  facet_grid(os_factor~., scales = 'free') +
  geom_vline(data = R_ver_hist, aes(xintercept = yr_wk_ver), linetype = 4, color = "red") +
  geom_text(data=R_ver_hist, mapping=aes(x=yr_wk_ver, y=0, label=version), 
            size=4, angle=90, vjust=-2.0, hjust=0)

g_os_yoy <- down_tot_by_os %>% 
  ggplot(aes(x = yr_wk, y = yoy_perc, col = os_factor, group = os_factor)) +
  geom_line() +
  facet_grid(os_factor~., scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_vline(data = R_ver_hist, mapping = aes(xintercept = greg_d), linetype = 4, color = "red") +
  annotate(geom = "text", x=R_ver_hist$yr_wk, y=0, label=R_ver_hist$version)

gridExtra::grid.arrange(g_os_lvl, g_os_yoy)

down_tot_by_os %>% dplyr::group_by(os_factor) %>% summarise_all(mean)

# What does the trend look like by version
down_tot_by_ver <- cran_R_downloads %>% 
  dplyr:: group_by(yr_wk,ver_lvl_factor,ver_lvl_top_factor,os) %>% 
  dplyr::summarise(total = sum(count))

down_tot_by_ver %>% ggplot(aes(x = yr_wk, y = total, col = ver_lvl_factor, group = ver_lvl_factor)) +
  geom_line() +
  facet_grid(os~ver_lvl_top_factor)+
  geom_vline(data = R_ver_hist, aes(xintercept = greg_d), linetype = 4, color = "red")

down_tot_by_ver %>% ggplot(aes(x = yr_wk, y = log2(total), col = ver_lvl_factor, group = ver_lvl_factor)) +
  geom_line() +
  facet_grid(os~ver_lvl_top_factor)

## Top Level

# Google Trends: Why is there a bump in the fall of 2018.

R_gtrends <- gtrendsR::gtrends(c("R",'r'))

r_trends_t <- R_gtrends$interest_over_time %>% 
  dplyr::mutate(yr_wk = tsibble::yearweek(date),
                hits_q = as.numeric(hits))

r_trends_t %>% head()

r_trends_tot_wk <- r_trends_t %>% 
  dplyr::group_by(yr_wk) %>% 
  dplyr::summarise(total = sum(hits_q, na.rm = TRUE)) 

r_trends_tot_wk %>% ggplot(aes(x = yr_wk, y = total)) +
  geom_line()

r_trends_tot_wk %>% ggplot(aes(x = yr_wk, y = log10(total))) +
  geom_line()
