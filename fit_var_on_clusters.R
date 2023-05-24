library(argparse)
library(arrow)
library(data.table)
source("RemembR/R/RemembeR.R")
change.remember.file("remember_overlaps.RDS",clear=TRUE)
#subreddit_timeseries_authortf.parquet
all.ts <- arrow::open_dataset("data/subreddit_timeseries_authortf.parquet")
scan <- all.ts$NewScan()
scanner <- scan$Finish()
all.ts <- scanner$ToTable()
all.ts <- as.data.frame(all.ts)
all.ts <- as.data.table(all.ts)

N.subs.author.clusters <- unique(all.ts,by=c('subreddit','author_cluster'))[,.(.N),by=.(author_cluster)]

base_call = "Rscript var_ols.R --path=%s --forecast-length=%d --runs=%d --name=%s"
script = "var_jobs.sh"
write('#!/bin/bash',script,append=FALSE)
remember(length(N.subs.author.clusters$author_cluster),"N.clusters")
N.isolates.1 <- 10000-sum(N.subs.author.clusters[N!=1]$N)
N.isolates.2 <- sum(N.subs.author.clusters[author_cluster==-1]$N)
remember(max(N.isolates.1,N.isolates.2),"N.isolates")
N.subs.author.clusters <- N.subs.author.clusters[author_cluster!=-1]
remember(mean(N.subs.author.clusters[N!=1]$N),"mean.cluster.size")
remember(median(N.subs.author.clusters[N!=1]$N),"median.cluster.size")
overwrite = TRUE

biggest.cluster <- max(all.ts[author_cluster!=-1,.(n.subs=length(unique(.SD$subreddit))),by=.(author_cluster),.SDcols=c("subreddit")]$n.subs)

n.test <- 24

                                        #min.ts.length <- biggest.cluster + n.test + 3
min.ts.length <- 156

n.excluded.subs <- 0
n.excluded.clusters <- 0

excluded.clusters <- c()

all.ts[,.N, by=.(subreddit,author_cluster)][author_cluster != -1][,.(n.subs=.N),by=.(author_cluster)]

remember(min.ts.length, "min.ts.length")
remember(n.test,"n.test")

for(clid in sort(N.subs.author.clusters$author_cluster)){
    ##    clean.globalenv()
    df <- all.ts[author_cluster == clid,.(subreddit,week,count)]

    lens <- df[,.N,by=.(subreddit)]
    removed <- lens[N < min.ts.length]
    n.excluded.subs <- n.excluded.subs + nrow(removed)
    df <- df[subreddit %in% lens[N >= min.ts.length]$subreddit]

    n.subs <- length(unique(df$subreddit))
    if(n.subs < 2){
        n.excluded.subs <- n.excluded.subs + n.subs
        n.excluded.clusters <- n.excluded.clusters + 1
        next
    }

    ## there's a bug in the library if names start with numbers
    df[,subreddit := paste0('X',subreddit)]
    
    setnames(df,old='count',new='value')
    name <- paste0('author_cluster_',clid,"_tf")
    boot <- TRUE
    runs <- 200
    path <- file.path("tempdata_tf",paste0(name,'.feather'))
    dir.create("tempdata_tf", showWarnings = FALSE)
    fit.path <- file.path("ols_models",paste0("var_ols_",name,".RDS"))
    if(!file.exists(fit.path) | overwrite == TRUE){
        print(clid)
        write_feather(df,path)
        write(sprintf(base_call, path, n.test, runs, name),script,append=TRUE)
    }
}


remember(n.excluded.subs, "n.excluded.subreddits")
remember(n.excluded.clusters, "n.excluded.clusters")
