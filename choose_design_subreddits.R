library(ggplot2)
library(data.table)
library(arrow)

source("RemembR/R/RemembeR.R")
source("helper.R")


df <- load_weekly_posts('data/design_subreddit_submissions.feather')
df <- df[!is.na(week)]
for(sr in unique(df$subreddit)){
    plot_subreddit_ts(df[subreddit==sr],outname='design_timeseries')
}

include <- c('assholedesign','dataisugly','designporn','expectationvsreality','mildlyinfuriating','softwaregore','atbge','gtbae','antiassholedesign','clickshaming','antiassholedesign','crappydesign','notmyjob','wellthatsucks','onejob')

df.out <- df[subreddit %in% c(include)]

remember(length(unique(df.out$subreddit)), 'n.design.found')

df.out[N.authors > 0, min(week),by=.(subreddit)]

exclude <- c("atbge","gtbae",'antiassholedesign')

max.date <- max(df.out[(subreddit %in% exclude) & (N.authors > 0), .(week=min(week)),by=.(subreddit)]$week,na.rm=T)

remember(length(exclude), 'n.design.excluded')

df.out <- df.out[!(subreddit %in% exclude)]

min.date <- max(df.out[N.authors > 0, .(week=min(week)),by=.(subreddit)]$week,na.rm=T)

write_feather(df.out,'data/included_design.feather',compression="uncompressed")

