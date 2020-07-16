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

plot_subreddit_ts <- function(df,min_date,max_date){
    srname = first(df$subreddit)
    print(srname)
    outdir = file.path('plots','subreddit_posts_timeseries')

    if (! dir.exists(outdir)) dir.create(outdir)
    
    png(file.path(outdir,paste0(srname,".png")),height=800,width=1800)
    p <- ggplot(df, aes(x=week, y=N.authors)) + geom_line() + ggtitle(srname)
    p <- p + scale_x_datetime(date_breaks='3 months',date_minor_breaks='1 month',date_labels="%y-%m")
    print(p)
    dev.off()
}

for(sr in unique(byWeekSub$subreddit)){
    plot_subreddit_ts(byWeekSub[subreddit==sr])
}
    
