library(ggplot2)
library(data.table)
library(arrow)

load_weekly_comments <- function(infile='data/wallpaper_subreddit_comments.feather'){

    print('loading data')

    df <- data.table(read_feather(infile))
    ## cut.dates starts on Monday.
    df <- df[,week := as.Date(cut(as.Date(CreatedAt),"week"))]
    df <- df[author != '[deleted]']
    print("Group by subreddit-week-author")

    authorsByWeekSub <- df[,.(N.comments = .N),
                           by=.(subreddit,week,author)]
    
    print("Group by subreddit-week")

    byWeekSub <- authorsByWeekSub[,.(N.authors = .N
                                     ),
                                  by=.(subreddit,week)]

    reddit.overall <- data.table(read_feather("data/all_reddit_commenters_per_week.feather"))
    setnames(reddit.overall,old='count',new='N.authors')

    reddit.overall[,subreddit:='all.reddit']
    reddit.overall[,week := as.Date(cut(as.Date(week),'week'))]
    byWeekSub <- rbind(byWeekSub,reddit.overall)

    min_date <- min(byWeekSub$week,na.rm=T)
    max_date <- max(byWeekSub$week,na.rm=T)
    weeks <- data.table(week=as.Date(levels(cut(c(min_date,max_date),breaks='week'))))
    srweeks <- list()
    for(sr in unique(byWeekSub$subreddit))
        srweeks <- rbind(srweeks,weeks[,.(subreddit=sr, week)])

    byWeekSub <- merge(byWeekSub,srweeks,by=c('subreddit','week'),all=TRUE)
    byWeekSub <- byWeekSub[is.na(N.authors), ":="(N.authors=0)]


    return(byWeekSub)
}

load_weekly_posts <- function(infile='data/seattle_subreddit_submissions.feather'){
    print("loading data")
    df <- data.table(read_feather(infile,columns=c('author','CreatedAt','ups','downs','score','subreddit')))

    df <- df[,week := as.Date(cut(as.Date(CreatedAt),"week"))]

    print("Group by subreddit-week-author")

    authorsByWeekSub <- df[,.(N.posts = .N),
                           by=.(subreddit,week,author)]
    
    print("Group by subreddit-week")

    byWeekSub <- authorsByWeekSub[,.(N.authors = .N)
                                 ,
                                  by=.(subreddit,week)]

    reddit.overall <- data.table(read_feather("data/all_reddit_posters_per_week.feather"))
    reddit.overall[,week := as.Date(cut(as.Date(week),'week'))]
    setnames(reddit.overall,old='count',new='N.authors')

    reddit.overall[,subreddit:='all.reddit']

    byWeekSub <- rbind(byWeekSub,reddit.overall)

    min_date <- min(byWeekSub$week,na.rm=T)
    max_date <- max(byWeekSub$week,na.rm=T)
    weeks <- data.table(week=as.Date(levels(cut(c(min_date,max_date),breaks='week'))))
    srweeks <- list()
    for(sr in unique(byWeekSub$subreddit))
        srweeks <- rbind(srweeks,weeks[,.(subreddit=sr, week)])


    byWeekSub <- merge(byWeekSub,srweeks,by=c('subreddit','week'),all=TRUE)
    byWeekSub <- byWeekSub[is.na(N.authors), ":="(N.authors=0)]
    return(byWeekSub)
}

plot_subreddit_ts <- function(df,min_date,max_date,outname='subreddit_posts_timeseries'){
    df <- copy(df)
    df <- df[,week:=as.POSIXct(week)]
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
