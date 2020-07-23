## This is a script for 

## It contains logic for deciding which subreddits and time periods to include in the analysis
## These settings were chosen by inspecting time series plots of the number of submissions and comments to each subreddit.
## We wish to select data where the assumptions of a gaussian or poisson VAR(p) model are plausible.
## These assumptions are: 1. no time-varying parameters, 2. no omitted variables, 3. no marginal over-dispersion (for poisson), marginal lognormality (for gaussian).
## These assumptions are not testible.  Statistical approaches to testing them are flawed; are an active area of research, and are recommended neither by ecologists nor applied econometricians.

## based on plots of unique posters each week
# not enough activity
source("RemembR/R/RemembeR.R")
library(arrow)

def_exclude = c('anacortes','auburnwa','bainbridgeisland','ballard','bellevuewa','belltown','bothell','bremerton','burien','camaswashington','capitolhillsea','clallam','destinationwa','edmonds','emeraldcityfoodies','federalway','fishingwashington','foodseattle','forkswa','grandcoulee','graysharbor','kentwa','kexp','kingcounty','kirkland','kittitas','lewiscounty','longview','lynnwood','lynwoodwa','marysville','mount_rainier','mukilteo','newsofseattle','northseattle','oceanshores','olympicnationalpark','olympic_peninsula','ortingwa','piercecountywa','pnwfestivals','politicswa','portangeles','portorchard','porttownsend','poulsbo','pugetlist','pugetsound','puyallupwa','rainier','renton_wa','sammamish','sanjuanislands','seatac','seattleapartments','seattle_bicycling','seattlecars','seattledragons','seattleevents','seattleents','seattlehistory','seattlehomelessfires','seattlehousing','seattlejobs','seattlemovienight','seattlemusic','seattlephotograhy','seattlequestions','seattleseawolvesrugby','seattlestorm','seattlethunderbirds','seattleu','seattlewarecipes','seawa','shoreline','skagit','skagitcounty','skagitvalley','snohomishcounty','snohomish','snoqualmie','southseattle','stevenspass','the_seattle','tukwila','tulalip','tumwater','uwt','veganseattle','wabeer','wallawalla','waoutdoors','washingtonforsanders','whidbey','wholesomeseattle','wizardsuniteseattle','woodinville','wsucougars','yelm')

might_include = c('cwu','ewu','kitsap','redmond','renton','seawa','sonics','wenatchee','yakima')

include=c('bellevue','bellingham','cascadia','circlejerkseattle','eastside','everett','hiphopheadsnorthwest','olympia','pacificnorthwest','pnws','pokemongoseattle','seajobs','sealist','seattlebike','seaents','seattle','seattlewa','spokane','tacoma','tricitieswa','vancouverwa','wa_guns','washington')

# getting seasonality for the seattle reign is another hassle. But it would be cool to be inclusive.
might_include_seasonality = c('reignfc')

## huskies clearly has football season seasonality.
## udub just looks like college season, but there might be some football mixed in
## the same is probably true of wsu and wwu 
## maybe quarterly seasonality will be good enough for pnwriders and pnwhiking. Let's go with that for now so I don't have to write any new code. 
include_seasonality = c("huskies",'mariners','pnwhiking','pnwriders','seahawks','soundersfc','udub','wsu','wwu')
#huskies has 
potential_competitors_excluded = c('bellevuewa','seawa','seattlejobs','seattle_bicycling','wsucougars','seattleents')

source("helper.R")

df <- load_weekly_posts()

## a limitation of this analysis is that it ignores some subreddits that became active after 2012.
## notably a discontinuity.
## we can extend our approach using state space models in future work.

min_yearly_nonzero_prop <- 0.3

min_date <- as.POSIXct("2011-06-01",tz='UTC')

remember(min_date,"min_date")
remember(min_yearly_nonzero_prop,'min_yearly_nonzero_prop')

df <- df[,':='(w.year=year(week))]
df <- df[(week>=min_date)]

df2 <- df[,.(prop.0 = mean(N.authors==0)),by=.(w.year,subreddit)]
df2 <- df2[,':='(prop.0.too.high = (prop.0 >= min_yearly_nonzero_prop))]
include <- df2[,.(include = sum(prop.0.too.high) == 0),by=.(subreddit)]
include <- include[include==TRUE,.(subreddit)]
df <- df[subreddit %in% include$subreddit]

remember(include, 'included_subreddits')
## our inclusion criteria is having no more than 15% of weeks without posts each year from 2012 through 2019.
write_feather(df,'data/included_timeseries.feather',compression="uncompressed")


