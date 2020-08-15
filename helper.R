library(data.table)
library(arrow)
load_weekly_posts <- function(infile='data/seattle_subreddit_submissions.feather'){
    print("loading data")
    df <- data.table(read_feather(infile,columns=c('author','CreatedAt','ups','downs','score','subreddit')))

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

    min_date <- min(byWeekSub$week)
    max_date <- max(byWeekSub$week)
    weeks <- data.table(week=as.POSIXct(levels(cut(c(min_date,max_date),breaks='week'))))
    srweeks <- list()
    for(sr in unique(byWeekSub$subreddit))
        srweeks <- rbind(srweeks,weeks[,.(subreddit=sr, week)])

    byWeekSub <- merge(byWeekSub,srweeks,by=c('subreddit','week'),all=TRUE)
    byWeekSub <- byWeekSub[is.na(N.authors), ":="(N.authors=0, N.posts=0, total.score=0)]
    return(byWeekSub)
}

plot_subreddit_ts <- function(df,min_date,max_date,outname='subreddit_posts_timeseries'){
    srname = first(df$subreddit)
    print(srname)
    outdir = file.path('plots',outname)

    if (! dir.exists(outdir)) dir.create(outdir)
    
    png(file.path(outdir,paste0(srname,".png")),height=800,width=1400)
    p <- ggplot(df, aes(x=week, y=N.authors)) + geom_line() + ggtitle(srname)
    p <- p + scale_x_datetime(date_breaks='3 months',date_minor_breaks='1 month',date_labels="%y-%m")
    print(p)
    dev.off()
}
