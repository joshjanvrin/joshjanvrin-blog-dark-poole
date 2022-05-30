---
layout: post
title: Determining Trending Periods in Silver Futures
---

In the previous post, we determined the most active periods for silver futures based on mean range and volume.

If trading a trend-following strategy, it may be useful to discover which periods are 'trendier', if any.  One would not expect to find an edge here - any statistically significant trending periods would likely be arbitraged away by algorithms.  If any periods did demonstrate more trendiness, it would be surprising if they were found in clusters.  Let's see what the data shows.

We'll need a couple of new functions to calculate and summarize trendiness.  The XTS inputs here are from ranges.lst, with rolling window range and volume calculations already applied.  The 'trendiness' value is brute-force calculated by getting the correlation of prices in the trading window to an incremental vector.

```r
CalculateTrendiness <- function(x.ts, period, trading.window){
  # Get correlation of prices in trading window to an incremental vector
  seq.uence <- 1:(trading.window / period)
  trendiness <- rollapply(x.ts$Close, trading.window / period, 
                               FUN = function(x) cor(x, seq.uence), align = "left")
  x.ts <- cbind(x.ts$Close, x.ts$volume, x.ts$window.range, x.ts$window.volume, trendiness)
  names(x.ts)[5] <- "trendiness"
  return(x.ts)
}

period.lst <- list(1, 3, 5, 10, 15, 30)
trendiness.lst <- ranges.lst %>%
  map2(., period.lst, CalculateTrendiness, trading.window = trading.window)
```

Now, we'll summarize the dataset, calculating a trend rating - an equal-weight factor of range and trendiness.  Low volume and low sample periods are filtered out.

```r
GetTrendRating <- function(df, volume.filter = TRUE){
  df <- df %>%
    mutate(time = format(lubridate::ymd_hms(Index), format = "%H:%M:%S")) %>%
    group_by(time) %>%
    summarise(mean.window.range = mean(window.range, na.rm = TRUE),
              mean.window.volume = mean(window.volume, na.rm = TRUE),
              mean.trendiness = abs(mean(trendiness, na.rm = TRUE)),
              trend.rating = mean.window.range * mean.trendiness * 1000,
              samples = n())
  threshold.volume <- df %>%
    select(mean.window.volume) %>%
    unlist %>%
    mean(na.rm = TRUE)
  threshold.samples <- df %>%
    select(samples) %>%
    unlist %>%
    mean(na.rm = TRUE)
  if(volume.filter){
    df <- df %>%
      filter(mean.window.volume > threshold.volume)
  }
  df <- df %>%
    filter(samples > threshold.samples) %>%
    arrange(desc(trend.rating))
  return(df)
}

# Get trend rating
trendiness.summary.lst <- trendiness.lst %>%
  map(fortify.zoo) %>%
  map(GetTrendRating, volume.filter = TRUE)
```

What 30-minute periods have the highest trend ratings?

![Silver Futures 30-minute Window Trend Rating](/assets/silver_futures_30min_window_trend_rating.png)

This result is somewhat surprising. There are in fact some clusters of trendy 30-minute periods, including the hard-to-ignore 11:30-12:00 window.

At the very least, this result warrants adapting some trend trading strategies, running them through quantstrat, and seeing if confining the time period to this window improves the profit factor.
