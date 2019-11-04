# Count R download Stats

# References for blog
# https://cran.r-project.org/web/packages/cranlogs/cranlogs.pdf
# https://github.com/r-hub/cranlogs.app
# 


# Libraries
library(cranlogs)
library(lubridate)
library(gridExtra)
library(ggrepel)
library(tidyverse)
library(tsibble)
library(gtrendsR)
library(rversions)
library(scales)


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
  dplyr::filter(greg_d >= as.Date('2015-01-01')) 

R_ver_hist_major <- R_ver_hist %>%  
  dplyr::filter(vers_i %in% c("3.2.0","3.3.0","3.4.0","3.5.0","3.6.0"))

cran_R_downloads_raw <- cranlogs::cran_downloads('R', from = "2010-01-01", to = lubridate::today())
cran_R_downloads <- cran_R_downloads_raw %>% 
  dplyr::filter(!version %in% c('devel','release','release.exe','latest')) %>% 
  dplyr::mutate(os_factor = forcats::fct_inorder(forcats::as_factor(os)),
         ver_factor =forcats::fct_inorder(forcats::as_factor(version)),
         ver_lvl = sub("*[.^]", "", version) %>% sub("\\..*", "", .) %>% as.integer(),
         ver_lvl_factor =forcats::fct_inorder(forcats::as_factor(ver_lvl)),
         ver_lvl_top = stringr::str_sub(version,1,1) %>% as.integer(),
         ver_lvl_top_factor =forcats::fct_inorder(forcats::as_factor(ver_lvl_top)),
         year = lubridate::year(date),
         month = lubridate::month(date),
         week = lubridate::week(date),
         yr_mo = tsibble::yearmonth(date),
         yr_wk = tsibble::yearweek(date),
         ver_3_5_f = ifelse(date >= subset(R_ver_hist_major, version == '3.5.0')$greg_d,1,0)
  ) %>% 
  dplyr::filter(yr_wk != max(cran_R_downloads$yr_wk)) %>% 
  dplyr::left_join(R_ver_hist, by = c("yr_wk" = "yr_wk_ver")) 
  
summary(cran_R_downloads)
# filter out devel

# EDA
cran_R_downloads %>% head()

cran_R_downloads %>% select(os) %>% group_by(os) %>% summarise(os_cnts = n())

cran_R_downloads %>% select(date) %>% distinct() %>% nrow()

cran_R_downloads %>% select(ver_lvl_top_factor) %>% 
  dplyr::group_by(ver_lvl_top_factor) %>% summarise(version_cnts = n())

# Build Aggregation Views
# What does the trend look at by Year
down_tot_by_yr <- cran_R_downloads %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(total = sum(count)) %>% 
  dplyr::mutate(yoy = total/dplyr::lag(total),
                yoy_perc = (total/dplyr::lag(total) - 1)*100,
                ) %>% 
  tsibble::as_tsibble(index = year) 

g_tot_by_yr <- down_tot_by_yr %>% 
  ggplot(aes(x = year, y = total, color = year)) +
  geom_line() +
  theme_light() + theme(legend.position = "none") +
  xlim(2015, 2020) +
  labs(x = 'Year', y = 'Total Downloads', title = "R Downloads from RStudio Cranlogs by Year") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-05, digits = 4))


g_tot_by_yoy <- down_tot_by_yr %>% 
  ggplot(aes(x = year, y = yoy_perc, fill = yoy_perc)) +
  geom_col() +
  theme_light() + theme(legend.position = "none") +
  xlim(2015, 2020) +
  annotate(geom = "text", 
           x=down_tot_by_yr$year, 
           y=0, 
           label=paste0(round(down_tot_by_yr$yoy_perc,0),'%'),
           size=5, angle=0, vjust= -.25, hjust= 0.50) +
  labs(x = 'Year', y = 'YOY % Growth') +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

gridExtra::grid.arrange(g_tot_by_yr,g_tot_by_yoy)

# What does the trend look like?
down_tot_by_d <- cran_R_downloads %>% 
  dplyr::group_by(yr_wk,greg_d,forcats::fct_explicit_na(vers_i)) %>% 
  dplyr::summarise(total = sum(count)) %>% 
  tsibble::as_tsibble(index = yr_wk)

down_tot_by_d %>% 
  ggplot(aes(x = yr_wk, y = total)) +
  geom_line() +
  theme_light() +
  labs(x = 'Week', y = 'Weekly Downloads', title = "R Downloads from RStudio Cranlogs by Week",
       caption = "Blue Line: R minor version releases\n
       Red Line: R version patches \n
       Note: No major versions released during this time.
       ") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_vline(data = R_ver_hist, aes(xintercept = greg_d), linetype = 4, color = "red", alpha = 0.5) +
  geom_vline(data = R_ver_hist_major, aes(xintercept = greg_d), linetype = 1, color = "blue") +
  annotate(geom = "text", 
           x=subset(R_ver_hist_major, version == '3.5.0')$yr_wk, 
           y=0, label=subset(R_ver_hist_major, version == '3.5.0')$version,
           size=4, angle=90, vjust=-0.10, hjust=-0.10) 



# What does the trend look like by os
down_tot_by_os <- cran_R_downloads %>% 
  dplyr:: group_by(os_factor,yr_wk) %>% 
  dplyr::summarise(total = sum(count)) %>% 
  dplyr::mutate(yoy = total/dplyr::lag(total, n = 52),
                yoy_perc = (total/dplyr::lag(total,n = 52) - 1)*100
  )

g_os_lvl <- down_tot_by_os %>% 
  ggplot(aes(x = yr_wk, y = total, col = os_factor, group = os_factor)) +
  geom_line() +
  facet_grid(os_factor~., scales = 'free') +
  geom_vline(data = R_ver_hist, aes(xintercept = yr_wk_ver), linetype = 4, color = "red", alpha = 0.5) +
  geom_vline(data = R_ver_hist_major, aes(xintercept = greg_d), linetype = 1, color = "blue") +
  annotate(geom = "text", 
         x=subset(R_ver_hist_major, version == '3.5.0')$yr_wk, 
         y=0, label=subset(R_ver_hist_major, version == '3.5.0')$version,
         size=4, angle=90, vjust=-0.10, hjust=-0.10) +
  annotate(geom = "text", 
           x=subset(R_ver_hist, version == '3.5.1')$yr_wk, 
           y=0, label=subset(R_ver_hist, version == '3.5.1')$version,
           size=4, angle=90, vjust=-0.10, hjust=-0.10) +
  theme_light() + theme(legend.position = "none") +
  labs(x = 'Week', y = '', title = "R Downloads from RStudio Cranlogs by Week") 
g_os_lvl

g_os_yoy <- down_tot_by_os %>% 
  ggplot(aes(x = yr_wk, y = yoy_perc, col = os_factor, group = os_factor)) +
  #geom_line() +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(os_factor~., scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_vline(data = R_ver_hist, mapping = aes(xintercept = greg_d), linetype = 4, color = "red", alpha = 0.5) +
  geom_vline(data = R_ver_hist_major, aes(xintercept = greg_d), linetype = 1, color = "blue") +
  annotate(geom = "text", 
           x=subset(R_ver_hist_major, version == '3.5.0')$yr_wk, 
           y=0, label=subset(R_ver_hist_major, version == '3.5.0')$version,
           size=4, angle=90, vjust=-0.10, hjust=-0.10) +
  annotate(geom = "text", 
           x=subset(R_ver_hist, version == '3.5.1')$yr_wk, 
           y=0, label=subset(R_ver_hist, version == '3.5.1')$version,
           size=4, angle=90, vjust=-0.10, hjust=-0.10) +
  theme_light() + theme(legend.position = "none") +
  labs(x = 'Week', y = '', title = "R Downloads Year Over Year Change from RStudio Cranlogs by Week",
       caption = "Blue Line: R minor version releases\n
       Red Line: R version patches \n
       Note: No major versions released during this time.
       ")
g_os_yoy

gridExtra::grid.arrange(g_os_lvl, g_os_yoy)

down_tot_by_os %>% dplyr::group_by(os_factor) %>% summarise_all(mean,na.rm = TRUE)
down_tot_by_os %>% dplyr::left_join(cran_R_downloads %>% select(yr_wk,ver_3_5_f),
                                    by = "yr_wk") %>% 
  dplyr::group_by(os_factor,ver_3_5_f) %>% summarise_all(mean,na.rm = TRUE)

down_tot_by_os %>% 
  # dplyr::left_join(R_ver_hist_major, by = c('yr_wk' = 'yr_wk_ver')) %>% 
  dplyr::filter(yr_wk >= subset(R_ver_hist_major, version == '3.5.0')$yr_wk) %>%
  dplyr::mutate(yoy_gtr_10 = ifelse(yoy_perc >= 10,1,0),
                yoy_gtr_100 = ifelse(yoy_perc >= 100,1,0)) %>% 
  group_by(os_factor) %>% 
  dplyr::summarise(rows_after_3.5 = n(),
                   tot_over_10pct = sum(yoy_gtr_10),
                   perc_wks_10pct = tot_over_10pct/rows_after_3.5 * 100,
                   tot_over_100pct = sum(yoy_gtr_100),
                   perc_wks_100pct = tot_over_100pct/rows_after_3.5 * 100)

  
  

# ----- Not using any code below this ----- #
# ----------------------------------------- #

# What does the trend look like by os by version
down_tot_by_ver <- cran_R_downloads %>% 
  dplyr:: group_by(ver_lvl_top_factor,os,yr_wk) %>%
  #dplyr:: arrange(ver_lvl_top_factor,os,yr_wk) %>%
  dplyr::summarise(total = sum(count)) %>% 
  dplyr::mutate(yoy = total/dplyr::lag(total, n = 52),
                yoy_perc = (total/dplyr::lag(total,n = 52) - 1)*100
  )

g_ver_lvl <- down_tot_by_ver %>% ggplot(aes(x = yr_wk, y = total, col = ver_lvl_top_factor, group = ver_lvl_top_factor)) +
  geom_line() +
  facet_grid(os~ver_lvl_top_factor)+
  geom_vline(data = R_ver_hist, aes(xintercept = greg_d), linetype = 4, color = "red", alpha = 0.5) +
  geom_vline(data = R_ver_hist_major, aes(xintercept = greg_d), linetype = 1, color = "blue") +
  theme_light() +
  annotate(geom = "text", 
           x=subset(R_ver_hist_major, version == '3.5.0')$yr_wk, 
           y=0, label=subset(R_ver_hist_major, version == '3.5.0')$version,
           size=4, angle=90, vjust=-0.10, hjust=-0.10)
g_ver_lvl

g_ver_yoy <- down_tot_by_ver %>% filter(ver_lvl_top_factor == 3) %>% 
  ggplot(aes(x = yr_wk, y = yoy_perc, col = ver_lvl_top_factor, group = ver_lvl_top_factor)) +
  geom_line() +
  facet_grid(os~ver_lvl_top_factor, scales = 'free') +
  geom_hline(yintercept = 0) +
  geom_vline(data = R_ver_hist, mapping = aes(xintercept = greg_d), linetype = 4, color = "red", alpha = 0.5) +
  geom_vline(data = R_ver_hist_major, aes(xintercept = greg_d), linetype = 1, color = "blue") +
  theme_light() +
  annotate(geom = "text", 
           x=subset(R_ver_hist_major, version == '3.5.0')$yr_wk, 
           y=0, label=subset(R_ver_hist_major, version == '3.5.0')$version,
           size=4, angle=90, vjust=-0.10, hjust=-0.10)
g_ver_yoy

# add layered bar chart or wave

## Top Level

# Google Trends: Why is there a bump in the fall of 2018.

categ <- c(1299, 1252, 675, 31, 900)
R_gtrends <- gtrendsR::gtrends(keyword = c("R",'r','r-project','dplyr','tidyverse'),
                               geo = c("US"),
                               time = sprintf("2010-01-01 %s",lubridate::today())
                               #gprop = c("web","youtube"),
                               #category = categ
                               )

plot(R_gtrends)

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
