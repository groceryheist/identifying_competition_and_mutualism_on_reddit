## This file contains logic for deciding which subreddits and time periods to include in the analysis
## These settings were chosen by inspecting time series plots of the number of submissions and comments to each subreddit.
## We wish to select data where the assumptions of a gaussian or poisson VAR(p) model are plausible.
## These assumptions are: 1. no time-varying parameters, 2. no omitted variables, 3. no marginal over-dispersion (for poisson), marginal lognormality (for gaussian).
## These assumptions are not testible.  Statistical approaches to testing them are flawed; are an active area of research, and are recommended neither by ecologists nor applied econometricians.

## based on plots of unique posters each week
# not enough activity
library(arrow)

def_exclude = c('anacortes','auburnwa','bainbridgeisland','ballard','bellevuewa','belltown','bothell','bremerton','burien','camaswashington','capitolhillsea','clallam','destinationwa','edmonds','emeraldcityfoodies','federalway','fishingwashington','foodseattle','forkswa','grandcoulee','graysharbor','kentwa','kexp','kingcounty','kirkland','kittitas','lewiscounty','longview','lynnwood','lynwoodwa','marysville','mount_rainier','mukilteo','newsofseattle','northseattle','oceanshores','olympicnationalpark','olympic_peninsula','ortingwa','piercecountywa','pnwfestivals','politicswa','portangeles','portorchard','porttownsend','poulsbo','pugetlist','pugetsound','puyallupwa','rainier','renton_wa','sammamish','sanjuanislands','seatac','seattleapartments','seattle_bicycling','seattlecars','seattledragons','seattleevents','seattleents','seattlehistory','seattlehomelessfires','seattlehousing','seattlejobs','seattlemovienight','seattlemusic','seattlephotograhy','seattlequestions','seattleseawolvesrugby','seattlestorm','seattlethunderbirds','seattleu','seattlewarecipes','seawa','shoreline','skagit','skagitcounty','skagitvalley','snohomishcounty','snohomish','snoqualmie','southseattle','stevenspass','the_seattle','tukwila','tulalip','tumwater','uwt','veganseattle','wabeer','wallawalla','waoutdoors','washingtonforsanders','whidbey','wholesomeseattle','wizardsuniteseattle','woodinville','wsucougars','yelm')

might_include = c('cwu','ewu','kitsap','redmond','renton','seawa','sonics','wenatchee','yakima')

include=c('bellevue','bellingham','cascadia','circlejerkseattle','eastside','everett','hiphopheadsnorthwest','olympia','pacificnorthwest','pnws','pokemongoseattle','seajobs','sealist','seattlebike','seaents','seattle','seattlewa','spokane','tacoma','tricitieswa','vancouverwa','wa_guns','washington')
might_include_seasonality = c('reignfc')
include_seasonality = c("huskies",'mariners','pnwhiking','pnwriders','seahawks','soundersfc','udub','wsu','wwu')
#huskies has 
potential_competitors_excluded = c('bellevuewa','seawa','seattlejobs','seattle_bicycling','wsucougars','seattleents')

source("helper.R")

df <- load_weekly_posts()

test_df <- df[subreddit %in% c("seattle","seattlewa","seahawks","washington","eastside","soundersfc")]

write_feather(test_df,'data/test_ts.feather',compression="uncompressed")
