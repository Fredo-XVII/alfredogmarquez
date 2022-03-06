---
title: Building a Holiday Package with timeDate
author: Alfredo G Marquez
date: '2022-03-05'
slug: building-a-holiday-package-with-timedate
categories:
  - R
  - Package Building
tags:
  - US Retail Holidays
  - holiday.dates
  - timeDate
  - r package
keywords:
  - R Package Holiday Dates
---

<!--more-->

**Coming Soon**

### Introduction

It has been a while since my last post.  You can probably imagine why with all that is going on in the world these days.  Today I will talk about using the timeDate package to build custome dates for any project.

### Data Problem

In my work to build time series models I need to account for the seasonality that occurs in retail.  I discovered the `timeDate` package from another team at work.  After playing with the functions in the package for a bit, I realized how flexible the functions in the package can be to building custom holiday dates.

### The timeDate Package

Source: https://cran.r-project.org/web/packages/timeDate/timeDate.pdf

There are many useful functions in the `timeDate` package.  Today, I will only focus on a couple that allowed me to create custom dates that are important to my forecasting work.

One type of function builds dates based on the year and the number of days from the beginning of the year.  If we look at how the `timeDate::NewYearsDay` function is built gives us template on how to create a holiday based on number of days in a year from the beginning of the year.

```
timeDate::NewYearsDay
```







