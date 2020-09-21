library(ggplot2)
library(data.table)
library(arrow)

source("RemembR/R/RemembeR.R")
source("helper.R")


df <- load_weekly_posts('data/wallpaper_subreddit_submissions.feather')

## for(sr in unique(df$subreddit)){
##     plot_subreddit_ts(df[subreddit==sr],outname='wallpaper_timeseries')
## }



include <- c('wallpaper','wallpapers','all.reddit')
include.extended <- c('animewallpaper','iphonewallpapers','iwallpaper','livewallpapers','lolwallpaper','minimalwallpaper','mobilewallpaper','musicwallpapers','nsfw_wallpapers','offensive_wallpapers','phonewallpapers','sexywallpapers','ultrahdwallpapers','verticalwallpapers','animephonewallpapers','wallpaperdump','wallpaperrequests')

# maybe handle the iphone X wallpapers by merging them
maybe.include <- c('animephonewallpapers','furrywallpapers','gmbwallpapers','hiphopwallpapers','iphonewallpaper','iphonexwallpapers','iwallpaperrequests','kaynewallpapers','mobilewallpapers','motivationwallpapers','nsfwanimewallpaper','s10wallpapers','shittywallpapers','startrekwallpaper','threescreenwallpapers')

exclude <- grepl('u_.*',unique(df$subreddit))
df.out <- df[!(subreddit %in% exclude)]
remember(length(unique(df.out$subreddit)), "n.wallpaper.found")
df.out <- df[subreddit %in% c(include,include.extended)]


df.excluded <- df[!(subreddit %in% unique(df.out$subreddit))]

min.date <- as.Date("2014-11-03")
max.date <- as.Date("2019-10-28")
df.out <- df.out[,y:=year(week)]

include.2 <- df.out[(week >= min.date) & (week <= max.date),.(mean.has.posts = mean(N.authors > 0)),by=.(y,subreddit)][mean.has.posts>0.4,.(N=.N),by=.(subreddit)][N==6]$subreddit

df.out <- df[subreddit %in% include.2]

write_feather(df.out,'data/included_wallpaper.feather',compression="uncompressed")

df <- load_weekly_comments('data/wallpaper_subreddit_comments.feather')
df.out <- df[subreddit %in% c(include,include.extended)]
df.out <- df.out[!(subreddit %in% exclude)]
write_feather(df.out,'data/included_wallpaper_comments.feather',compression="uncompressed")

