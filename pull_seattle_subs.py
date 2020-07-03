# let's try using the pyarrow datasets feature
import pyarrow as pa
import pyarrow.parquet as pq
import pyarrow.dataset as ds
import pathlib

# creating a Dataset object loads nothing into memory, it only crawls the directory to find all the files and infer the schema. 

dataset = ds.dataset(pathlib.Path('/gscratch/comdata/output/reddit_comments.parquet/'), format='parquet', partitioning='hive')

# the sweet thing about a dataet is that we can filter by columns

main = ['Seattle','SeattleWA','SeaWA']

# I checked this on June 26 and July 1 2020 
seattlewa_sidebar = ["Seajobs","Seattlejobs","SeattleApartments","SeattleHousing","politicsWA","SeaList","EmeraldCityFoodies","FoodSeattle","FishingWashington","SeattlePhotography","HipHopHeadsNorthwest","KEXP","NewsOfSeattle","PNWfestivals","PNWHiking","PNWRiders","PokemonGoSeattle","SeaEnts","SeattleBike","SeattleHistory","SeattleMusic","WABeer","WA_Guns","VeganSeattle","DestinationWA","SeattleEvents","Seahawks","Mariners","SoundersFC","ReignFC","Sonics","SeattleStorm","WSUCougars","Huskies","SeattleThunderbirds","SeattleSeawolvesRugby","CWU","EWU","SeattleU","UDub","wsu","WWU","PacificNorthwest","Cascadia","Washington","KingCounty","Eastside","LewisCounty","Olympic_Peninsula","Kitsap","PierceCountyWA","SkagitValley","SnohomishCounty","sanjuanislands","TriCitiesWA","Spokane","CirclejerkSeattle","SeattleWARecipes","seattlehomelessfires"]

# excluding subs already listed in the sidebar
seattlewa_wiki=["Ballard","Belltown","CapitolHillSea","NorthSeattle","SouthSeattle","WestSeattle","PNWS","PugetList","SeattleCars","SeattleMovieNight","SeattleQuestions","SeattleWAXRay","WAOutdoors","WholesomeSeattle","WizardsUniteSeattle","UWT","Anacortes","AuburnWA","BainbridgeIsland","Bellevue","Bellingham","Bothell","Bremerton","Burien","CamasWashington","Edmonds","Everett","FederalWay","ForksWA","GrandCoulee","GraysHarbor","KentWA","Kirkland","Kitsap","Kittitas","Longview","Lynnwood","LynnwoodWA","Marysville","MercerIsland","Mount_Rainier","Mukilteo","OceanShores","Olympia","OlympicNationalPark","OrtingWA","PortAngeles","PortOrchard","PortTownsend","Poulsbo","PuyallupWA","Rainier","Redmond","Renton","Renton_WA","Salkum","Sammamish","SeaTac","Seattle","SheltonWA","Shoreline","Skagit","Snohomish","Snoqualmie","StevensPass","Tacoma","Tukwila","Tulalip","Tumwater","WallaWalla","Wenatchee","Whidbey","Woodinville","Yakima","Yelm"]

seattle_wiki = ["SportsRadioKJR","SeattleMoviePass","SkagitCounty","bellevuewa","clallam","pugetsound","vancouverwa"]

# search results
search = ["SeattleHNL","SeattleDragons","SeaWA","CoronavirusWA","The_Seattle","WashingtonForSanders","seattle_bicycling"]

# https://www.reddit.com/r/Seattle/comments/f1eny/list_of_other_seattle_reddits/
seattlethread = ["seattlents"]

subreddits_to_track = set(main + seattlewa_sidebar + seattlewa_wiki + seattle_wiki + seattlethread + search)

table = dataset.to_table(filter = ds.field('subreddit').isin(subreddits_to_track), columns=['id','subreddit','link_id','parent_id','edited','time_edited','CreatedAt','author','ups','downs','score','subreddit_type','subreddit_id','stickied','is_submitter'])

# then write it to a feather file
import pyarrow.feather as feather
feather.write_feather(table, 'data/seattle_subreddits.feather')

import pandas as pd

df = table.to_pandas()

df.to_csv("data/seattle_subreddits.csv")
