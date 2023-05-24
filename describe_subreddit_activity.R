# for R 4.0.2
library(ggplot2)
library(data.table)
library(arrow)

## submissions seems like a better measure of a commited contributor. 
## comment dynamics are also interesting though. 
source("helper.R")

byWeekSub <- load_weekly_posts()
## remember that I can't make pdfs on hyak.
## so let's just make one png for each and look at them one at a time.

for(sr in unique(byWeekSub$subreddit)){
    plot_subreddit_ts(byWeekSub[subreddit==sr])
}
    
