# Count R download Stats

# References for blog
# https://cran.r-project.org/web/packages/cranlogs/cranlogs.pdf
# https://github.com/r-hub/cranlogs.app
# 


# Libraries
library(cranlogs)
library(lubridate)
library(gridExtra)
library(tidyverse)
library(tsibble)
library(gtrendsR)
library(rversions)
library(scales)
library(corrr)


# Download R download data and version dates
R_ver_hist_raw <- rversions::r_versions() 
R_ver_hist_raw$vers_d <- as.POSIXct(R_ver_hist_raw$date)
#saveRDS(R_ver_hist_raw, file = paste0(file.path(getwd(),"data"), "/R_ver_hist_raw.rds"))
#R_ver_hist_raw.rds <- readRDS(file = paste0(file.path(getwd(),"data"), "/R_ver_hist_raw.rds"))

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

#cran_R_downloads_raw <- cranlogs::cran_downloads('R', from = "2010-01-01", to = lubridate::today())
#saveRDS(cran_R_downloads_raw, file = paste0(file.path(getwd(),"data"), "/cran_R_downloads_raw.rds"))
cran_R_downloads_raw <- readRDS(file = paste0(file.path(getwd(),"data"), "/cran_R_downloads_raw.rds"))
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
  dplyr::filter(yr_wk != max(.$yr_wk)) %>% 
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
  #geom_vline(aes(xintercept = as.Date('2018-07-09')), linetype = 1, color = "black") + useR2018
  annotate(geom = "text", 
           x=subset(R_ver_hist_major, version == '3.5.0')$yr_wk, 
           y=0, label=subset(R_ver_hist_major, version == '3.5.0')$version,
           size=4, angle=90, vjust=-0.10, hjust=-0.10, color = "blue", alpha = 0.75) +
  annotate(geom = "text", 
           x=subset(R_ver_hist, version == '3.5.1')$yr_wk, 
           y=0, label=subset(R_ver_hist, version == '3.5.1')$version,
           size=4, angle=90, vjust=-0.10, hjust=-0.10, color = "red", alpha = 0.5) 

### Impact of Tidyverse
tidy_fulcrum <- ggplot(down_tot_by_d, aes(x = yr_wk, y = total)) + 
  geom_line() +
  geom_vline(xintercept = hadley_tidyverse, color = 'steelblue', linetype = 4, alpha = 0.75) +
  geom_vline(xintercept = tidy_searches_beg, color = 'red') +
  geom_vline(xintercept = tidyvere_release_1_0, color = 'steelblue', linetype = 4, alpha = 0.75) +
  theme_bw() +
  annotate(geom = "text", 
           x=hadley_tidyverse, 
           y=0, label="Hadley @ useR!2016",
           size=4, angle=90, vjust=-0.8, hjust=-1.35,  color = "steelblue", alpha = 0.75) +
  annotate(geom = "text", 
           x=tidy_searches_beg, 
           y=0, label="Tidyverse Searches Begin",
           size=4, angle=90, vjust=-0.0, hjust=-0.5,  color = "red", alpha = 0.75) +
  annotate(geom = "text", 
           x=tidyvere_release_1_0, 
           y=0, label="Tidyverse 1.0.0",
           size=4, angle=90, vjust=1.5, hjust=-1.90,  color = "steelblue", alpha = 0.75) +
  #theme_light() + theme(legend.position = "none") +
  labs(x = 'Week', y = '', title = "Tidyverse Impacts R Downloads Trend",caption = "")

tidy_fulcrum

# What does the trend look like by os
down_tot_by_os <- cran_R_downloads %>% 
  dplyr:: group_by(os_factor,yr_wk) %>% 
  dplyr::summarise(total = sum(count)) %>% 
  dplyr::mutate(yoy = total/dplyr::lag(total, n = 52),
                yoy_perc = round((total/dplyr::lag(total,n = 52) - 1)*100,0)
  )

g_os_lvl <- down_tot_by_os %>% 
  ggplot(aes(x = yr_wk, y = total, col = os_factor, group = os_factor)) +
  geom_line() +
  facet_grid(os_factor~., scales = 'free') +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_vline(data = R_ver_hist, aes(xintercept = yr_wk_ver), linetype = 4, color = "red", alpha = 0.5) +
  geom_vline(data = R_ver_hist_major, aes(xintercept = greg_d), linetype = 1, color = "blue") +
  annotate(geom = "text", 
         x=subset(R_ver_hist_major, version == '3.5.0')$yr_wk, 
         y=0, label=subset(R_ver_hist_major, version == '3.5.0')$version,
         size=4, angle=90, vjust=-0.10, hjust=-0.10,  color = "blue", alpha = 0.75) +
  annotate(geom = "text", 
           x=subset(R_ver_hist, version == '3.5.1')$yr_wk, 
           y=0, label=subset(R_ver_hist, version == '3.5.1')$version,
           size=4, angle=90, vjust=-0.10, hjust=-0.10,  color = "red", alpha = 0.5) +
  theme_light() + theme(legend.position = "none") +
  labs(x = 'Week', y = '', title = "R Downloads from RStudio Cranlogs by Week",
       caption = "
       Current R major version is 3.x.x \n
       Blue Line: R minor version releases\n
       Red Dotted Line: R version patches \n       .
       ")
g_os_lvl

g_os_yoy <- down_tot_by_os %>% filter(os_factor == "osx") %>% 
  ggplot(aes(x = yr_wk, y = yoy_perc, col = os_factor, group = os_factor)) +
  #geom_line() +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(os_factor~., scales = 'free') +
  geom_hline(yintercept = 0) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_vline(data = R_ver_hist, mapping = aes(xintercept = greg_d), linetype = 4, color = "red", alpha = 0.5) +
  geom_vline(data = R_ver_hist_major, aes(xintercept = greg_d), linetype = 1, color = "blue") +
  annotate(geom = "text", 
           x=subset(R_ver_hist_major, version == '3.5.0')$yr_wk, 
           y=0, label=subset(R_ver_hist_major, version == '3.5.0')$version,
           size=4, angle=90, vjust=-0.10, hjust=-0.10,  color = "blue", alpha = 0.75) +
  annotate(geom = "text", 
           x=subset(R_ver_hist, version == '3.5.1')$yr_wk, 
           y=0, label=subset(R_ver_hist, version == '3.5.1')$version,
           size=4, angle=90, vjust=-0.10, hjust=-0.10,  color = "red", alpha = 0.5) +
  theme_light() + theme(legend.position = "none") +
  labs(x = 'Week', y = '', title = "R Downloads Year Over Year Change from RStudio Cranlogs by Week",
       caption = '
       Current R major version is 3.x.x \n
       Blue Line: R minor version releases\n
       Red Dotted Line: R version patches \n  
       ')
g_os_yoy

gridExtra::grid.arrange(g_os_lvl, g_os_yoy)

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


down_tot_by_os %>% dplyr::group_by(os_factor) %>% summarise_all(mean,na.rm = TRUE)
down_os_summary <- down_tot_by_os %>% dplyr::left_join(cran_R_downloads %>% select(yr_wk,ver_3_5_f),
                                    by = "yr_wk") %>% 
  dplyr::group_by(os_factor,ver_3_5_f) %>% summarise_all(mean,na.rm = TRUE)

down_os_summary
down_os_summary %>% filter(os_factor == 'win',ver_3_5_f == 0) %>% ungroup() %>% select(total) %>% round()

get_os_summary <- function(os, post351, metric) {
  down_os_summary %>% filter(os_factor == !!os,ver_3_5_f == !!post351) %>% 
    ungroup() %>% select(!!metric) %>% round()
}

get_os_summary(os = 'win', post351 = 1, metric = 'total')
  

# ----- THE PROBLEM WITH WEB TRENDS ----- #
# ----------------------------------------- #

# add layered bar chart or wave

## Top Level

# Google Trends: Why is there a bump in the fall of 2018.

# gtrends_search <- funtion( terms = terms) {
#   gtrendsR::gtrends(keyword = terms,
#                                     #geo = c("US"),
#                                     time = sprintf("2015-01-01 %s",lubridate::today())
#                     )
# }

terms <- c("R programming") # TIOBE
R_gtrends <- gtrendsR::gtrends(keyword = terms,
                               #geo = c("US"),
                               time = sprintf("2015-01-01 %s",lubridate::today())
                               )

terms <- c("dplyr")
dplyr_gtrends <- gtrendsR::gtrends(keyword = terms,
                               #geo = c("US"),
                               time = sprintf("2015-01-01 %s",lubridate::today())
)

terms <- c("tidyverse")
tverse_gtrends <- gtrendsR::gtrends(keyword = terms,
                                   #geo = c("US"),
                                   time = sprintf("2015-01-01 %s",lubridate::today())
)

terms <- c("ggplot2")
ggplot_gtrends <- gtrendsR::gtrends(keyword = terms,
                                    #geo = c("US"),
                                    time = sprintf("2015-01-01 %s",lubridate::today())
)

terms <- c("R tutorials")
pypl_gtrends <- gtrendsR::gtrends(keyword = terms,
                                    #geo = c("US"),
                                    time = sprintf("2015-01-01 %s",lubridate::today())
)

terms <- c("DataCamp")
camp_gtrends <- gtrendsR::gtrends(keyword = terms,
                                  #geo = c("US"),
                                  time = sprintf("2015-01-01 %s",lubridate::today())
)

plot(camp_gtrends)

r_trends_t <- R_gtrends$interest_over_time %>% 
  dplyr::mutate(yr_wk = tsibble::yearweek(date)) %>% 
  dplyr::select(yr_wk,hits,keyword)

dplyr_t <- dplyr_gtrends$interest_over_time %>% 
  dplyr::mutate(yr_wk = tsibble::yearweek(date)) %>% 
  dplyr::select(yr_wk,hits,keyword)

tverse_t <- tverse_gtrends$interest_over_time %>% 
  dplyr::mutate(yr_wk = tsibble::yearweek(date)) %>% 
  dplyr::select(yr_wk,hits,keyword)

ggplot_t <- ggplot_gtrends$interest_over_time %>% 
  dplyr::mutate(yr_wk = tsibble::yearweek(date)) %>% 
  dplyr::select(yr_wk,hits,keyword)

pypl_t <- pypl_gtrends$interest_over_time %>% 
  dplyr::mutate(yr_wk = tsibble::yearweek(date)) %>% 
  dplyr::select(yr_wk,hits,keyword)

camp_t <- camp_gtrends$interest_over_time %>% 
  dplyr::mutate(yr_wk = tsibble::yearweek(date)) %>% 
  dplyr::select(yr_wk,hits,keyword)

down_tot_by_d_t <- down_tot_by_d %>% 
  dplyr::ungroup() %>% 
  dplyr::select(yr_wk,total) %>% 
  dplyr::summarise(wkly_tot = sum(total)) %>% 
  dplyr::mutate(hits = (.$wkly_tot - min(down_tot_by_d$total)) / (max(down_tot_by_d$total) - min(down_tot_by_d$total))*100,
                keyword = "R Downloads"
                ) 

# Hadley Announces Tidyverse at useR 2016 conference week of: 2016-06-26
# resource: https://user2016.r-project.org//
# tidyverse released 1.0.0 on 2016-09-15: https://blog.rstudio.com/2016/09/15/tidyverse-1-0-0/
hadley_tidyverse <- as.Date('2016-06-26')
tidy_searches_beg <- as.Date('2016-07-31')
tidyvere_release_1_0 <- as.Date('2016-09-15')

all_trends <- dplyr::bind_rows(r_trends_t, pypl_t, dplyr_t, ggplot_t, tverse_t, camp_t,
                               down_tot_by_d_t %>% select(yr_wk,hits,keyword)) %>%
  dplyr::mutate(keyword_f = as_factor(keyword)) %>% 
  dplyr::select(yr_wk,hits,keyword_f) 

# Plot the trend of the search keywords and the 
ggplot(all_trends, aes(x = yr_wk, y = hits, group = keyword_f, col = keyword_f)) + 
  geom_line() +
  facet_grid(keyword_f~., scales = 'free') +
  geom_vline(xintercept = hadley_tidyverse) +
  geom_vline(xintercept = tidy_searches_beg, color = 'red') +
  geom_vline(xintercept = tidyvere_release_1_0, color = 'orange')

  
# 1 week differences
diff1_trends <- all_trends %>% 
  dplyr::group_by(keyword_f) %>% 
  dplyr::mutate(lag1_hits = dplyr::lag(hits),
                diff1_hits = difference(hits)
                )


ggplot(diff1_trends, aes(x = yr_wk, y = diff1_hits, group = keyword_f, col = keyword_f)) + 
  geom_line() +
  facet_grid(keyword_f~.) +
  geom_vline(xintercept = hadley_tidyverse) +
  geom_vline(xintercept = tidy_searches_beg, color = 'red') 
  


# Correlate differences
corr_trends_base <- diff1_trends %>% 
  dplyr::select(yr_wk,keyword_f, diff1_hits) %>% 
  tidyr::pivot_wider(names_from = keyword_f,
                     values_from = diff1_hits) %>% 
  dplyr::filter(.$yr_wk >= hadley_tidyverse)

corrr::correlate(corr_trends_base[,2:8])

broom::tidy(lm(formula = corr_trends_base$`R Downloads` ~ ., data = corr_trends_base[,2:5]))

broom::tidy(lm(formula = corr_trends_base$`R Downloads` ~ 
                 corr_trends_base$tidyverse +
                 lag(corr_trends_base$tidyverse, n = 1L) +
                 lag(corr_trends_base$tidyverse, n = 2L) +
                 lag(corr_trends_base$tidyverse, n = 3L) +
                 lag(corr_trends_base$tidyverse, n = 4L) +
                 lag(corr_trends_base$`R Downloads`, n = 1L) +
                 lag(corr_trends_base$`R Downloads`, n = 2L) +
                 lag(corr_trends_base$`R Downloads`, n = 3L) +
                 lag(corr_trends_base$`R Downloads`, n = 4L) 
               , data = corr_trends_base[,2:5]))

lm_4_diff <- function(col) {
  broom::tidy(lm(formula = corr_trends_base$`R Downloads` ~
                 col +
                 lag(col, n = 1L) +
                 lag(col, n = 2L) +
                 lag(col, n = 3L) +
                 lag(col, n = 4L) +
                 lag(corr_trends_base$`R Downloads`, n = 1L) +
                 lag(corr_trends_base$`R Downloads`, n = 2L) +
                 lag(corr_trends_base$`R Downloads`, n = 3L) +
                 lag(corr_trends_base$`R Downloads`, n = 4L) 
               , data = corr_trends_base[,2:5]))
}

lm_4_diff(col = corr_trends_base$ggplot2)
lm_4_diff(col = corr_trends_base$dplyr)
lm_4_diff(col = corr_trends_base$`R programming`)

broom::tidy(lm(formula = corr_trends_base$`R Downloads` ~ 
                 corr_trends_base$tidyverse +
                 lag(corr_trends_base$tidyverse, n = 1L) +
                 lag(corr_trends_base$tidyverse, n = 2L) +
                 lag(corr_trends_base$tidyverse, n = 3L) +
                 lag(corr_trends_base$tidyverse, n = 4L) +
                 corr_trends_base$ggplot2 +
                 lag(corr_trends_base$ggplot2, n = 1L) +
                 lag(corr_trends_base$ggplot2, n = 2L) +
                 lag(corr_trends_base$ggplot2, n = 3L) +
                 lag(corr_trends_base$ggplot2, n = 4L) +
                 corr_trends_base$tidyverse +
                 lag(corr_trends_base$dplyr, n = 1L) +
                 lag(corr_trends_base$dplyr, n = 2L) +
                 lag(corr_trends_base$dplyr, n = 3L) +
                 lag(corr_trends_base$dplyr, n = 4L) +
                 corr_trends_base$tidyverse +
                 lag(corr_trends_base$`R programming`, n = 1L) +
                 lag(corr_trends_base$`R programming`, n = 2L) +
                 lag(corr_trends_base$`R programming`, n = 3L) +
                 lag(corr_trends_base$`R programming`, n = 4L) +                 
                 lag(corr_trends_base$`R Downloads`, n = 1L) +
                 lag(corr_trends_base$`R Downloads`, n = 2L) +
                 lag(corr_trends_base$`R Downloads`, n = 3L) +
                 lag(corr_trends_base$`R Downloads`, n = 4L) 
               , data = corr_trends_base[,2:5])) %>% View()






# ---- NOT USIND CODE BELOW ---- #
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
