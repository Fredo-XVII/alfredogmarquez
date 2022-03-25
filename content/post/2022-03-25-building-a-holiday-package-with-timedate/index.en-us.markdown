---
title: Building a Holiday Package with timeDate
author: Alfredo G Marquez
date: '2022-03-25'
slug: index.en-us
categories:
  - R
  - Package Building
tags:
  - US Retail Holidays
  - timeDate
  - holiday.dates
  - r package
  - R
keywords:
  - R Package Holiday Dates
summary: Today I will discuss building custom dates with timeDate package.
draft: no    
  
---

<!--more-->




### Libraries

```r
library(timeDate)
library(holiday.dates)
```

### Introduction

It has been a while since my last post.  You can probably imagine why with all that is going on in the world these days.  Today I will talk about using the `timeDate` package to build custom dates for any project.  Then I will showcase my new `holiday.dates` package that leverages the functions in the `timeDate` package.

### Data Problem

In my work to build time series models I need to account for the seasonality that occurs in retail.  I discovered the `timeDate` package from another team at work.  After playing with the functions in the package for a bit, I realized how flexible the functions in the package can be for building custom holiday dates.

### The timeDate Package

Source: https://cran.r-project.org/web/packages/timeDate/timeDate.pdf

There are many useful functions in the `timeDate` package.  Today, I will only focus on a couple that allowed me to create custom dates that are important to my forecasting work.

First, I don't want to reproduce a holiday that already exists in the `timeDate` package.  Lets check to see what holidays are available in the `timeDate` package.


```r
timeDate::listHolidays()
```

```
##   [1] "Advent1st"                "Advent2nd"               
##   [3] "Advent3rd"                "Advent4th"               
##   [5] "AllSaints"                "AllSouls"                
##   [7] "Annunciation"             "Ascension"               
##   [9] "AshWednesday"             "AssumptionOfMary"        
##  [11] "BirthOfVirginMary"        "BoxingDay"               
##  [13] "CACanadaDay"              "CACivicProvincialHoliday"
##  [15] "CALabourDay"              "CaRemembranceDay"        
##  [17] "CAThanksgivingDay"        "CAVictoriaDay"           
##  [19] "CelebrationOfHolyCross"   "CHAscension"             
##  [21] "CHBerchtoldsDay"          "CHConfederationDay"      
##  [23] "CHKnabenschiessen"        "ChristmasDay"            
##  [25] "ChristmasEve"             "ChristTheKing"           
##  [27] "CHSechselaeuten"          "CorpusChristi"           
##  [29] "DEAscension"              "DEChristmasEve"          
##  [31] "DECorpusChristi"          "DEGermanUnity"           
##  [33] "DENewYearsEve"            "Easter"                  
##  [35] "EasterMonday"             "EasterSunday"            
##  [37] "Epiphany"                 "FRAllSaints"             
##  [39] "FRArmisticeDay"           "FRAscension"             
##  [41] "FRAssumptionVirginMary"   "FRBastilleDay"           
##  [43] "FRFetDeLaVictoire1945"    "GBBankHoliday"           
##  [45] "GBMayDay"                 "GBMilleniumDay"          
##  [47] "GBSummerBankHoliday"      "GoodFriday"              
##  [49] "ITAllSaints"              "ITAssumptionOfVirginMary"
##  [51] "ITEpiphany"               "ITImmaculateConception"  
##  [53] "ITLiberationDay"          "ITStAmrose"              
##  [55] "JPAutumnalEquinox"        "JPBankHolidayDec31"      
##  [57] "JPBankHolidayJan2"        "JPBankHolidayJan3"       
##  [59] "JPBunkaNoHi"              "JPChildrensDay"          
##  [61] "JPComingOfAgeDay"         "JPConstitutionDay"       
##  [63] "JPEmperorsBirthday"       "JPGantan"                
##  [65] "JPGreeneryDay"            "JPHealthandSportsDay"    
##  [67] "JPKeirouNOhi"             "JPKenkokuKinenNoHi"      
##  [69] "JPKenpouKinenBi"          "JPKinrouKanshaNoHi"      
##  [71] "JPKodomoNoHi"             "JPKokuminNoKyujitu"      
##  [73] "JPMarineDay"              "JPMidoriNoHi"            
##  [75] "JPNatFoundationDay"       "JPNationalCultureDay"    
##  [77] "JPNationHoliday"          "JPNewYearsDay"           
##  [79] "JPRespectForTheAgedDay"   "JPSeijinNoHi"            
##  [81] "JPShuubunNoHi"            "JPTaiikuNoHi"            
##  [83] "JPTennouTanjyouBi"        "JPThanksgivingDay"       
##  [85] "JPUmiNoHi"                "LaborDay"                
##  [87] "MassOfArchangels"         "NewYearsDay"             
##  [89] "PalmSunday"               "Pentecost"               
##  [91] "PentecostMonday"          "PresentationOfLord"      
##  [93] "Quinquagesima"            "RogationSunday"          
##  [95] "Septuagesima"             "SolemnityOfMary"         
##  [97] "TransfigurationOfLord"    "TrinitySunday"           
##  [99] "USChristmasDay"           "USColumbusDay"           
## [101] "USCPulaskisBirthday"      "USDecorationMemorialDay" 
## [103] "USElectionDay"            "USGoodFriday"            
## [105] "USInaugurationDay"        "USIndependenceDay"       
## [107] "USLaborDay"               "USLincolnsBirthday"      
## [109] "USMemorialDay"            "USMLKingsBirthday"       
## [111] "USNewYearsDay"            "USPresidentsDay"         
## [113] "USThanksgivingDay"        "USVeteransDay"           
## [115] "USWashingtonsBirthday"
```

From the list above we can see that the Valentine holiday is not included in the `timeDate` package.  We can build a custom function by copying one of the functions from the `timeDate` package and changing it to meet our needs.

One type of function builds dates based on the year and the number for the day of the year.  For example, if we look at how the `timeDate::NewYearsDay` function is built you can see that the date is 101, or 0101, meaning month 01 and day 01, or January first.


```r
timeDate::NewYearsDay
```

```
## function (year = getRmetricsOptions("currentYear")) 
## {
##     ans = year * 10000 + 101
##     timeDate(as.character(ans))
## }
## <bytecode: 0x000000001d925120>
## <environment: namespace:timeDate>
```

We are going to change the 101 in the function above to **0214**.  I like including the `0` because it feels more like a date when you look at the function.  The new USValentinesDay function is defined below.


```r
USValentinesDay <-
  function(year = timeDate::getRmetricsOptions("currentYear")) {
    ans = year*10000 + 0214
    timeDate::timeDate(as.character(ans)) }
```

The `year` argument for the function allows you to select any year to get the date for Valentines.  For example, for 2022 I would expect to get 2022-02-14 for Valentines day.  Lets try it with a vector of years:


```r
USValentinesDay(year = c(2019,2020,2021,2022))
```

```
## GMT
## [1] [2019-02-14] [2020-02-14] [2021-02-14] [2022-02-14]
```
The second type of function is based on a rule that determines when the holiday will happen.  Let's look into how the US Thanksgiving Day is built within the `timeDate` package:


```r
timeDate::USThanksgivingDay
```

```
## function (year = getRmetricsOptions("currentYear")) 
## {
##     ans = .nth.of.nday(year, 11, 4, 4)
##     timeDate(as.character(ans))
## }
## <bytecode: 0x000000001e007d40>
## <environment: namespace:timeDate>
```

You can see that the heart of this rule based solution is the `.nth.of.nday` function.  The functions takes the following arguments: year, month, day number, and week number. We can read the function arguments as: **For year and November, find the 4th day of the 4th week.**

For example, Mother's Day in the US is in May on the 2nd Sunday of the month.  This is how I built a solution for Mother's Day in the US:


```r
holiday.dates::USMothersDay
```

```
## function (year = timeDate::getRmetricsOptions("currentYear")) 
## {
##     ans = .nth.of.nday(year, 5, 7, 2)
##     timeDate::timeDate(as.character(ans))
## }
## <bytecode: 0x000000001e3d86c0>
## <environment: namespace:holiday.dates>
```

The arguments for the `.nth.of.nday` is read as: **For year and May, find the 7th day of the 2nd week.**

Let's test how it works for the years on [Mother's Day on Wikipedia](https://en.wikipedia.org/wiki/Mother%27s_Day_(United_States)), 2021 to 2024.  Here is a copy of the dates from the site:

```
Begins	2nd Sunday of May
Date	Second Sunday in May
2021 date	May 9
2022 date	May 8
2023 date	May 14
2024 date	May 12
Frequency	Annual
```


```r
holiday.dates::USMothersDay(c(2021, 2022, 2023, 2024))
```

```
## GMT
## [1] [2021-05-09] [2022-05-08] [2023-05-14] [2024-05-12]
```

As you can see that these dates match.

### holiday.dates package

Source: https://github.com/Fredo-XVII/holiday.dates

For my work, I need a consistent calendar with dates categorized as holidays that drive demand for consumer or retail goods.  I categorize holidays into 2 types.  **Federal holidays** are defined by the government and can be for many different reasons.  **Retail holidays** are days that drive consumer demand for retail goods.  Halloween and Valentine's Day are some examples of retail holidays.  Note that federal holidays can be retail holidays if they drive demand consumer goods, such as Thanksgiving.

The `timeDate` package functions are vectorized and so it is really nice for getting a vector of dates, but I wanted something more.  I want a data frame of dates for a given set of dates and holiday flags.  My solution is the `holiday.dates` package.

The workhorse in the `holiday.dates` package is the `holiday_data()` function.  The function requires a dataframe or tibble with one column of dates.  You can have 2 dates or series of dates, but the minimum and maximum of the dates will drive the output.  There are 2 options for output in the function.  You can have either days or weeks as the output.  I will provide an example of both.

**Days Output**

Here is an example with a series of dates:

```r
days_df <- tibble::tibble(weeks = seq.Date(as.Date('2022-01-01'), as.Date('2022-03-01'), by = 'day'))

days_series <- holiday.dates::holiday_data(df = days_df, out_df = "days") 
```

```
## Joining, by = c("greg_d", "day_n", "wk_beg_d", "wk_end_d")Adding missing
## grouping variables: `wk_end_d`Joining, by = c("wk_beg_d", "wk_end_d")
```

```r
days_series %>% head(10)
```

```
## # A tibble: 10 x 6
##    greg_d     day_n wk_beg_d   wk_end_d   holiday_names holiday_flag
##    <date>     <ord> <date>     <date>     <chr>                <dbl>
##  1 2022-01-01 Sat   2021-12-26 2022-01-01 NewYearsDay              1
##  2 2022-01-02 Sun   2022-01-02 2022-01-08 NoHoliday                0
##  3 2022-01-03 Mon   2022-01-02 2022-01-08 NoHoliday                0
##  4 2022-01-04 Tue   2022-01-02 2022-01-08 NoHoliday                0
##  5 2022-01-05 Wed   2022-01-02 2022-01-08 NoHoliday                0
##  6 2022-01-06 Thu   2022-01-02 2022-01-08 NoHoliday                0
##  7 2022-01-07 Fri   2022-01-02 2022-01-08 NoHoliday                0
##  8 2022-01-08 Sat   2022-01-02 2022-01-08 NoHoliday                0
##  9 2022-01-09 Sun   2022-01-09 2022-01-15 NoHoliday                0
## 10 2022-01-10 Mon   2022-01-09 2022-01-15 NoHoliday                0
```

Here is an example with with 2 dates:

```r
days_df <- tibble::tibble(weeks = c(as.Date('2022-01-01'), as.Date('2022-03-01')))

days_2_dates <- holiday.dates::holiday_data(df = days_df, out_df = "days") 
```

```
## Joining, by = c("greg_d", "day_n", "wk_beg_d", "wk_end_d")Adding missing
## grouping variables: `wk_end_d`Joining, by = c("wk_beg_d", "wk_end_d")
```

```r
days_2_dates %>% head(10)
```

```
## # A tibble: 10 x 6
##    greg_d     day_n wk_beg_d   wk_end_d   holiday_names holiday_flag
##    <date>     <ord> <date>     <date>     <chr>                <dbl>
##  1 2022-01-01 Sat   2021-12-26 2022-01-01 NewYearsDay              1
##  2 2022-01-02 Sun   2022-01-02 2022-01-08 NoHoliday                0
##  3 2022-01-03 Mon   2022-01-02 2022-01-08 NoHoliday                0
##  4 2022-01-04 Tue   2022-01-02 2022-01-08 NoHoliday                0
##  5 2022-01-05 Wed   2022-01-02 2022-01-08 NoHoliday                0
##  6 2022-01-06 Thu   2022-01-02 2022-01-08 NoHoliday                0
##  7 2022-01-07 Fri   2022-01-02 2022-01-08 NoHoliday                0
##  8 2022-01-08 Sat   2022-01-02 2022-01-08 NoHoliday                0
##  9 2022-01-09 Sun   2022-01-09 2022-01-15 NoHoliday                0
## 10 2022-01-10 Mon   2022-01-09 2022-01-15 NoHoliday                0
```

The 2 data frames are identical:

```r
identical(days_series,days_2_dates)
```

```
## [1] TRUE
```

**Weeks Output**

Here is an example with a series of week dates:

```r
weeks_df <- tibble::tibble(weeks = seq.Date(as.Date('2022-01-01'), as.Date('2022-03-01'), by = 'week'))

holiday_data(df = weeks_df, out_df = "week") %>% head(10)
```

```
## Joining, by = c("greg_d", "day_n", "wk_beg_d", "wk_end_d")Adding missing
## grouping variables: `wk_end_d`Joining, by = c("wk_beg_d", "wk_end_d")
```

```
## # A tibble: 9 x 4
## # Groups:   wk_beg_d [9]
##   wk_beg_d   wk_end_d   holiday_names holiday_flag
##   <date>     <date>     <chr>                <dbl>
## 1 2021-12-26 2022-01-01 NewYearsDay              1
## 2 2022-01-02 2022-01-08 NoHoliday                0
## 3 2022-01-09 2022-01-15 NoHoliday                0
## 4 2022-01-16 2022-01-22 MlkDay                   1
## 5 2022-01-23 2022-01-29 NoHoliday                0
## 6 2022-01-30 2022-02-05 NoHoliday                0
## 7 2022-02-06 2022-02-12 SuperBowl                1
## 8 2022-02-13 2022-02-19 ValentinesDay            1
## 9 2022-02-20 2022-02-26 NoHoliday                0
```


### The Last Word

I hope that you have learned how to build custom holiday functions for your part of the world.  If you use my new `holiday.dates` package, please let me know how you like it.  Leave your thoughts or any questions below. 

