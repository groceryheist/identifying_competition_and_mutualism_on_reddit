library(argparse)
library(arrow)
library(data.table)

all.ts <- arrow::open_dataset("data/subreddit_timeseries.parquet")
scan <- all.ts$NewScan()
scanner <- scan$Finish()
all.ts <- scanner$ToTable()
all.ts <- as.data.frame(all.ts)
all.ts <- as.data.table(all.ts)

N.subs.author.clusters <- unique(all.ts,by=c('subreddit','author_cluster'))[,.(.N),by=.(author_cluster)]

min.ts.length <- 156
max.community.size <- 30
min.community.size <- 2

clean.globalenv <- function(){
    rm('df',pos=globalenv()) 
    rm('ylog',pos=globalenv())
    rm('boot',pos=globalenv())
    rm('n.test',pos=globalenv())
    rm('name',pos=globalenv())
    rm('path',pos=globalenv())
#    detach("package::vars",unload=TRUE)
}

for(clid in N.subs.author.clusters$author_cluster){
    print(clid)
    clean.globalenv()
    df <- all.ts[author_cluster == clid,.(subreddit,week,count)]

    lens <- df[,.N,by=.(subreddit)]
    keep <- lens[N >= min.ts.length]$subreddit
    df <- df[subreddit %in% keep]

    if(length(unique(df$subreddit)) < min.community.size)
        next

    if(length(unique(df$subreddit)) > max.community.size)
        next

    ## there's a bug in the library if names start with numbers
    df[,subreddit := paste0('X',subreddit)]

    setnames(df,old='count',new='value')
    name <- paste0('author_cluster_',clid)
    boot <- TRUE
    runs <- 200
    n.test <- 6*4
    source('var_ols.R')
}
