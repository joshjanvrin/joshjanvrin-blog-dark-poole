---
layout: post
title: Determining Active Periods in Silver Futures
---

Intraday traders seek out active markets.  How is 'active' defined?
Characteristics of an active market include:

* Relatively high volume
* Relatively wide range

When is the silver futures market active?

We'll look at periodicities ranging from one to 30 minutes and rank the most active periods.

First we'll read a local file containing the past two years of one-minute price data from the COMEX 5000-oz silver contract.

```r
# Read local file as an eXtensible time series and convert to ET
column_names <- c("Open", "High", "Low", "Close", "Volume")
asset.xts <- as.xts(read.zoo(filename,
                             sep = ",",
                             format="%Y%m%d %H%M%S",
                             tz="UTC"))
colnames(asset.xts) <- column_names
tzone(asset.xts) <- "America/New_York"
```

Next we convert the one-minute series to a range of periodicities and store the xts objects in a list.

```r
period.lst <- list(1, 3, 5, 10, 15, 30)
xts.lst <- period.lst %>%
  map(~ to.period(asset.xts, period = "minutes", k = ., name = NULL)) %>%
  set_names(paste("period", period.lst, sep = "."))
```

Let's work with the 5-minute periodicity as an example.

We'll calculate the percent range for the entire time series and then find the mean percent range and volume for each 5-minute
period of the day.

```r
tbl.summary.5min <- fortify.zoo(xts.lst[['period.5']]) %>%
  mutate(time = format(lubridate::ymd_hms(Index), format = "%H:%M:%S"),
         percent.range = (High - Low) / Close) %>%
  group_by(time) %>%
  summarise(mean.period.range = mean(percent.range),
            mean.period.volume = mean(Volume),
            samples = n()) %>%
  filter(samples > 100) %>%
  select(-samples) %>%
  arrange(desc(mean.period.range))
```

What 5-minute periods have the largest ranges and highest volume?

![Silver Futures 5-minute Range Volume Table](/assets/silver_futures_5min_range_volume.png)

Unsurprisingly, the 08:30 - 08:34 COMEX open and the first few periods after 09:30 are most 'active'.

A time-based perspective:

![Silver 5-minute Mean Range (%)](/assets/silver_ranges_5min.png)

In addition to calculating the mean percent range, we may also want to find a range for a trading window.  For example, let's say we are interested in trading during a 30-minute window.  Knowing which five minutes of the session is most active is handy (e.g. 09:30 - 09:34), but it may be more valuable to know which _30-minute window_ of 5-minute periods is most active (e.g. 9:30 - 10:00, 9:35 - 10:05, 9:40 - 10:10, etc.).

```r
CalculateWindowRanges <- function(x.ts, period, trading.window){
  # Get percent range/volume of trading window
  x.ts$window.high = rollapply(x.ts$High,
                               trading.window / period,
                               FUN = max,
                               align = "left")
  x.ts$window.low = rollapply(x.ts$Low,
                              trading.window / period,
                              FUN = min,
                              align = "left")
  x.ts$window.range = (x.ts$window.high - x.ts$window.low) / x.ts$Close
  x.ts$window.volume = rollapply(x.ts$Volume,
                                 trading.window / period,
                                 FUN = sum,
                                 align = "left")
  return(x.ts)
}

tbl.summary.30window.5min <- xts.lst[['period.5']] %>%
  CalculateWindowRanges(period = 5, trading.window = 30) %>%
  fortify.zoo(.) %>%
  mutate(time = format(lubridate::ymd_hms(Index), format = "%H:%M:%S")) %>%
  select(time, window.range, window.volume) %>%
  group_by(time) %>%
  summarise(mean.window.range = mean(window.range),
            mean.window.volume = mean(window.volume),
            samples = n()) %>%
  filter(samples > 100) %>%
  select(-samples) %>%
  arrange(desc(mean.window.range))

```
What 30-minute windows have the largest ranges and highest volume?

![Silver Futures 30-minute Window Range Volume Table](/assets/silver_futures_30min_window_range_volume.png)

This table tells a slightly different story.  Trading around the COMEX open is still active, but not as active as the few windows starting at 9:30.

The results of this data analysis confirm what we might have suspected were 'active' periods.  

However, further analysis can reveal which 30-minute windows may be best suited for trend or mean-reversion strategies.  We'll look at this in an upcoming post.