library(ggplot2)
library(data.table)
library(arrow)

source("RemembR/R/RemembeR.R")
source("helper.R")

df <- load_weekly_posts('data/whatsthis_subreddit_submissions.feather')
df <- df[!is.na(week)]

## for(sr in unique(df$subreddit)){
##     plot_subreddit_ts(df[subreddit==sr],outname='whatsthis_timeseries')
## }

tokeep <- c('archaeology','askdocs','askvet','fossilid','guns','helpmyfind','herpetology','identifythisfont','knives','mycology','namethatcar','swords','tipofmyjoystick','tipofmypenis','toys','translator','watches','whatisthisthing','whatisthisrock','whatisthisworth','whereisthis','tipofmytoungue.png')

weather_seasonality <- c('mycology','whatisthisbird','whatisthisplant','animalid')

to.exclude <- c('symbology','tipofmyfork','whatisthisfish') # we just exclude these three because they are obviously not active for long enough.

include <- c(tokeep,to.exclude)
remember(include, 'whatsthis_included_subreddits')

df.out <- df[subreddit %in% include]

write_feather(df.out,'data/whatsthis_included.feather',compression="uncompressed")
