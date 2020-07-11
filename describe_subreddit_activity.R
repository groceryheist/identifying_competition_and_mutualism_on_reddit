# for R 4.0.2
library(ggplot2)
library(data.table)
library(arrow)

## # this package provides read_feather for feather 2.0
## library(arrow)


## this is using comments right now, but it will probably be better to use submissions
## submissions seems like a better measure of a commited contributor. 
## comment dynamics are also interesting though. 
## since i'm waiting for comments to download, let's look number of unique commentors each week.

parse.date <- function(s) as.POSIXct(s, format="%Y-%m-%d", tz='UTC')

print("loading data")
df <- data.table(read_feather('data/seattle_subreddit_submissions.feather',columns=c('author','CreatedAt','ups','downs','score','subreddit')))

print("Parsing Dates")

df <- df[,CreatedAt := parse.date(CreatedAt)]

df <- df[,week := as.POSIXct(as.character(cut(CreatedAt,"week")))]

print("Group by subreddit-week-author")

authorsByWeekSub <- df[,.(N.posts = .N,
                          total.ups = sum(ups),
                          total.downs = sum(downs),
                          total.score = sum(score),
                          mean.score = mean(score),
                          var.score = var(score)),
                       by=.(subreddit,week,author)]
                          
print("Group by subreddit-week")

byWeekSub <- authorsByWeekSub[,.(N.authors = .N,
                                 N.posts = sum(N.posts),
                                 total.score = sum(total.score),
                                 author.mean.score = mean(total.score),
                                 author.var.score = var(total.score))
                             ,
                              by=.(subreddit,week)]


## remember that I can't make pdfs on hyak.
## so let's just make one png for each and look at them one at a time.

plot_subreddit_ts <- function(df){
    srname = first(df$subreddit)
    print(srname)
    outdir = file.path('plots','subreddit_posts_timeseries')

    if (! dir.exists(outdir)) dir.create(outdir)
    
    pdf(file.path(outdir,paste0(srname,".pdf")))
    p <- ggplot(df, aes(x=week, y=N.authors)) + geom_line() + ggtitle(srname)
    print(p)
    dev.off()
}

for(sr in unique(byWeekSub$subreddit)){
    plot_subreddit_ts(byWeekSub[subreddit==sr])
}
    
