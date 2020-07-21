library(data.table)
library(arrow)
load_weekly_posts <- function(){
    print("loading data")
    df <- data.table(read_feather('data/seattle_subreddit_submissions.feather',columns=c('author','CreatedAt','ups','downs','score','subreddit')))

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
