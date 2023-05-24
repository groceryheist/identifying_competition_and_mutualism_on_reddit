library(ggplot2)
library(data.table)
library(arrow)
library(scoringRules)
source("RemembR/R/RemembeR.R")
change.remember.file("remember_overlaps.RDS",clear=TRUE)

clusters <- data.table(read_feather("/gscratch/comdata/output/reddit_clustering/best_author-tf.feather"))

top.n <- fread("/gscratch/comdata/output/reddit_similarity/subreddits_by_num_comments.csv")
top.n.nsfw <- fread("/gscratch/comdata/output/reddit_similarity/subreddits_by_num_comments_nsfw.csv")

nonsfw_10k_comments <- top.n[comments_rank >= 10000,max(n_comments)]
n.nsfw.removed <- nrow(top.n.nsfw[n_comments >= nonsfw_10k_comments]) - 10000
remember(n.nsfw.removed, 'n.nsfw.removed')

load_var_coefs <- function(overwrite = FALSE){
 
    if(overwrite == TRUE){
        files <- list.files("ols_models")
        files <- files[grepl("var_ols_remember_author_cluster_(.*)_tf",files)]

        # we only have 710 here. should have 731
        ids <- sort(as.numeric(gsub(".RDS","",gsub("_tf","",gsub("var_ols_remember_author_cluster_","",files)))))

        coefs.tab <- list()

        for(clid in ids){
            rdata <- readRDS(file.path('ols_models', paste0("var_ols_remember_author_cluster_", clid, "_tf.RDS")))
            coefs <- rdata[[paste0('var.coef.author_cluster_',clid,"_tf")]]
            for(sub.i in names(coefs)){
                coef.i <- coefs[[sub.i]]
                coef.names <- rownames(coef.i)
                coef.i <- as.data.table(coef.i)
                coef.i[,'Term':=coef.names]
                coef.i[,'Subreddit':=sub.i]
                coef.i[,'clid':=clid]
                coefs.tab[[sub.i]] <- coef.i

            }
        }

        coefs.tab <- rbindlist(coefs.tab)

        coefs.tab[,Subreddit.j := gsub("\\.l?1$","",gsub("^X","",Term))]
        coefs.tab[,Subreddit.i := gsub("^X","",Subreddit)]
        coefs.tab[,Subreddit := NULL]
        coefs.tab[(Subreddit.j != Subreddit.i) & (Term != 'const') & (Term != 'trend'),.('avg'=mean(Estimate),'med'=median(Estimate),'var'=var(Estimate),.N)]

        coefs.tab[(Subreddit.j != Subreddit.i) & (Term != 'const') & (Term != 'trend') & (coefs.tab[['Pr(>|t|)']] < 0.05),.('avg'=mean(Estimate),'med'=median(Estimate),'var'=var(Estimate),.N)]

        write_feather(coefs.tab,"data/var_cluster_coefs.feather")
    } else {
        coefs.tab <- data.table(read_feather("data/var_cluster_coefs.feather"))
    }
    return(coefs.tab)
}

overall.scores <- function(overwrite = FALSE){
        files <- list.files("ols_models")
        files <- files[grepl("var_ols_remember_author_cluster_(.*)_tf.RDS",files)]
        ids <- sort(as.numeric(gsub(".RDS","",gsub("_tf","",gsub("var_ols_remember_author_cluster_","",files)))))
        all.crps.var <- c()
        all.crps.ar <- c()
        all.sqerr.var <- c()
        all.sqerr.ar <- c()


        for(clid in ids){
            name <- paste0('author_cluster_',clid,'_tf')
            path <- file.path('ols_models', paste0("var_ols_remember_",name,".RDS"))
            rdata <- readRDS(path)

            ## actually we should recompute the rmse, since before I aggregated it in a weird way
            var.rmse.ypred <- rdata[paste0("var.ypred.rmse.",name)]
            ar.rmse.ypred <- rdata[paste0("ar.ypred.rmse.",name)]

            var.crps.ypred <- rdata[paste0("var.ypred.crps.",name)]
            ar.crps.ypred <- rdata[paste0("ar.ypred.crps.",name)]

            ytest <- rdata[paste0('ytest.',name)]

            if(any(is.na(var.rmse.ypred[[1]])) | any(is.na(ar.rmse.ypred[[1]])) | any(is.na(var.crps.ypred[[1]])) | any(is.na(ar.crps.ypred[[1]])) | any(is.na(ytest[[1]]))){
                print(paste0("NA found in ",clid))
            }

            sqerr.var <- c()
            for(i in 1:length(var.rmse.ypred)){
                var.rmse.err <- ytest[[1]][i,]-var.rmse.ypred[[1]]$fcst[[i]][,1]

                all.sqerr.var <- c(all.sqerr.var, var.rmse.err**2)

                ar.rmse.err <- ytest[[1]][i,]-ar.rmse.ypred[[1]]$fcst[[i]][,1]

                all.sqerr.ar <- c(all.sqerr.ar, ar.rmse.err**2)

                var.fcast <- var.crps.ypred[[1]]$fcst[[i]][,1]
                var.se <- var.crps.ypred[[1]]$fcst[[i]][,4]
                
                all.crps.var <- c(all.crps.var, crps_norm(var.fcast, var.se))
                ar.fcast <- ar.crps.ypred[[1]]$fcst[[i]][,1]
                ar.se <- ar.crps.ypred[[1]]$fcst[[i]][,4]
                all.crps.ar <- c(all.crps.ar, crps_norm(ar.fcast, ar.se))

                if(any(is.na(c(var.rmse.err, ar.rmse.err, var.fcast, var.se)))){
                    print(paste0("NA found in ",clid))
                }
            }

            rmse.all.var <- sqrt(mean(all.sqerr.var))
            rmse.all.ar <- sqrt(mean(all.sqerr.ar))
            total.crps.var <- sum(all.crps.var)
            total.crps.ar <- sum(all.crps.ar)
        }
    
    return(list(rmse.all.var=rmse.all.var,
                rmse.all.ar=rmse.all.ar,
                total.crps.var=total.crps.var,
                total.crps.ar=total.crps.ar))
}

recomp.density <- function(clusters){
    author.similarities <- data.table(read_feather("/gscratch/comdata/output/reddit_similarity/subreddit_author_tf_similarities_10000.parquet"))
    dt <- melt(author.similarities,id.vars='subreddit',variable.name='subreddit.j',value.name='author.similarity')
    dt <- dt[(subreddit %in% clusters$subreddit) |
             (subreddit.j %in% clusters$subreddit)]
    dt <- dt[,.(author_density=mean(author.similarity)),by=.(subreddit)]
    return(dt)
}


local.author.density <- function(clusters){
#    author.similarities <- data.table(read_feather("/gscratch/comdata/output/reddit_similarity/comment_authors_10000.feather"))
    author.similarities <- data.table(read_feather("/gscratch/comdata/output/reddit_similarity/subreddit_comment_authors-tf_LSI/600.feather"))
    dt <- melt(author.similarities,id.vars='_subreddit',variable.name='subreddit.j',value.name='author.similarity')
    setnames(dt,old=c("_subreddit"),new=c("subreddit.i"))
    cluster.pairs <- clusters[,.(subreddit.j=subreddit),by=.(author_cluster)][clusters,on=c("author_cluster"),allow.cartesian=T]
    setnames(cluster.pairs,old=c("subreddit"),new=c("subreddit.i"))
    dt <- dt[cluster.pairs,on=c("subreddit.j","subreddit.i")]

    local.density <- dt[,.(local.author.density=mean(author.similarity)),by=.(author_cluster)]
    return(local.density)
}

author.wang.density <- function(clusters){
   author.intersections <- data.table(read_feather("/gscratch/comdata/output/reddit_similarity/wang_similarity_10000.feather"))
   mat <- as.matrix(author.intersections[,names(author.intersections)[names(author.intersections) != 'subreddit'],with=FALSE])
   rownames(mat) <- author.intersections$subreddit
   den <- diag(mat)
   density <- apply(mat, c(1), sum)
   density <- (density - den) / den
   return(density)
}

local.author.wang.density <- function(clusters){
   author.intersections <- data.table(read_feather("/gscratch/comdata/output/reddit_similarity/wang_similarity_10000_max10.feather"))
   author.intersections <- melt(author.intersections, id.vars=c("subreddit"), variable.name='subreddit.j')
   cluster.pairs <- clusters[,.(subreddit.j=subreddit),by=.(author_cluster)][clusters,on=c("author_cluster"),allow.cartesian=T]
   local.intersections <- author.intersections[cluster.pairs,on=c('subreddit','subreddit.j')]
   den <- local.intersections[subreddit == subreddit.j]
   overlaps <- local.intersections[,.(overlap=sum(value)), by=.(subreddit)]
   density <- den[overlaps, .(overlap = (overlap - value) / value, subreddit,author_cluster),on=c('subreddit')]
   return(density)
}

commensal.by.clusters <- function(coefs.tab, clusters){
    cluster.pairs <- clusters[,.(subreddit.j=subreddit),by=.(author_cluster)][clusters,on=c("author_cluster"),allow.cartesian=T]
    cluster.pairs <- setnames(cluster.pairs,old=c('subreddit','subreddit.j'),new=c("Subreddit.i","Subreddit.j"))

    coefs.by.clusters <- cluster.pairs[coefs.tab[(Term!='const')&(Term!='trend') &(Subreddit.i != Subreddit.j)],on=c("Subreddit.i","Subreddit.j")]
    commensal.by.clusters <- coefs.by.clusters[(Term!='const')&(Term!='trend') &(Subreddit.i != Subreddit.j),.(avg.commensalism=mean(Estimate),N.edges = .N,avg.abs.commensalism=mean(abs(Estimate)),abs.avg.commensalism=abs(mean(Estimate))),by=.(author_cluster)]

    return(commensal.by.clusters)
}

# these subreddits aren't assigned to clusters since they only have 1 user.
remember(10000 - nrow(clusters), "n.one.user.subs")
coefs.tab <- load_var_coefs(overwrite=TRUE)
remember(length(unique(coefs.tab$clid)),"N.fit.clusters")
all.ts <- arrow::open_dataset("data/subreddit_timeseries_authortf.parquet")
scan <- all.ts$NewScan()
scanner <- scan$Finish()
all.ts <- scanner$ToTable()
all.ts <- as.data.frame(all.ts)
all.ts <- as.data.table(all.ts)

# at the subreddit level. Overlaps here are globally defined (not within cluster)
# the outcome is the average number of distinct commenters in the last observed week.
all.ts <- all.ts[order(subreddit,week)]
all.ts <- all.ts[,week.idx := 1:.N, by=.(subreddit)]
all.ts <- all.ts[,week.idx.rev := .N - (1:.N) + 1, by=.(subreddit)]

remember(min(all.ts$week),'min.date')
remember(max(all.ts$week),'max.date')

clusters <- unique(all.ts,by=c("subreddit","author_cluster","term_cluster"))[,.(subreddit,author_cluster,term_cluster)]


df.1 <- all.ts[week.idx.rev == 24,.(subreddit, count,week)]
remember(max(df.1$week),'max.training.date')

df.1 <- df.1[,count.tminus.24 := count]
df.1 <- df.1[,count := NULL]
df.2 <- all.ts[week.idx.rev == 1,.(subreddit, week, count, term_cluster, author_cluster, term_density, author_density)]

# pause 
df <- df.1[df.2,on=c('subreddit')]
remember(nrow(df),'N.included.subreddits')

df <- df[,delta.count:=count - count.tminus.24]
df <- df[,delta.count.log:=log(count) - log(count.tminus.24)]

## the iv is the average Estimate
iv <- coefs.tab[(Term!='const')&(Term!='trend') &(Subreddit.i != Subreddit.j) ,.(avg.subreddit.commensalism=mean(Estimate),N.edges = .N,total.commensalism=sum(Estimate),avg.abs.commensalism=mean(abs(Estimate)),sum.abs.commensalism=sum(abs(Estimate))),by=.(Subreddit.i,clid)]

iv2 <- coefs.tab[(Term!='const')&(Term!='trend') &(Subreddit.i != Subreddit.j)&(coefs.tab[["Pr(>|t|)"]]<=0.05) ,.(avg.commensalism.sig=mean(Estimate),N.edges.sig = .N,total.commensalism.sig=sum(Estimate),positive.sig = sum(Estimate > 0), negative.sig = sum(Estimate < 0)),by=.(Subreddit.i)]

dt.commensal <- commensal.by.clusters(coefs.tab, clusters)

dt.density <- local.author.density(clusters)

dt <- dt.commensal[dt.density,on=.(author_cluster)]

dt <- dt[!is.na(N.edges)]

dt <- dt[,abs.sub.avg.commensalism := avg.abs.commensalism - avg.commensalism]

df <- dt[df,on=c("author_cluster")]

plot.dt <- melt(dt, id.vars=c("author_cluster","N.edges","local.author.density"),measure.vars=c("avg.commensalism","avg.abs.commensalism",'abs.sub.avg.commensalism'))

# subreddit level
pdf("author_tfidf_heatmaps.pdf",width=12,height=6)
remember(plot.dt,"author.tfidf.heatmap1.plotdata")
p <- ggplot(plot.dt, aes(x=local.author.density,y=value,group=variable)) + geom_bin2d(bins=50) + facet_wrap(.~variable,scales='free',ncol=3)
print(p)
dev.off()

dt[,diff.abs.avg := avg.abs.commensalism - avg.commensalism]

dt.cluster <- dt[,.(avg.commensalism=mean(avg.commensalism), avg.abs.commensalism=mean(avg.abs.commensalism), local.author.density = mean(local.author.density), abs.sub.avg.commensalism = mean(abs.sub.avg.commensalism),N.edges=first(N.edges)),by=.(author_cluster)]


pdf("subreddit_absxavg.pdf",width=12,height=6)
remember(dt, 'dt.subreddit.commensalism.pdf')
p <- ggplot(dt, aes(x=avg.commensalism, y=avg.abs.commensalism)) + geom_bin2d(bins=50)
print(p)
dev.off()

remember(dt.cluster, 'dt.cluster.commensalism.pdf')

dt[,diff.abs.avg := avg.abs.commensalism - avg.commensalism]

df <- iv[df,on=c("Subreddit.i"="subreddit")]
df <- iv2[df,on=c("Subreddit.i")]

for (i in names(df))
    df[is.na(get(i)),(i):=0]
###
author.similarities <- data.table(read_feather("/gscratch/comdata/output/reddit_similarity/comment_authors_10000.feather"))
term.similarities <- data.table(read_feather("/gscratch/comdata/output/reddit_similarity/comment_terms_10000.feather"))


library(ggeffects)


df <- df[,author_density := author_density/max(author_density)]
## df <- df[,author_density.l1p := log1p(author_density)]
olsmod.0.clusters <- lm(delta.count.log ~ author_density,df[Subreddit.i %in% unique(iv$Subreddit.i)])
olsmod <- lm(delta.count.log ~ poly(author_density,2,raw=TRUE),df)
olsmod.clusters <- lm(delta.count.log ~ poly(author_density,2,raw=TRUE),df[Subreddit.i %in% unique(iv$Subreddit.i)])
mod2.clusters <- lm(delta.count.log ~ avg.subreddit.commensalism,df[Subreddit.i %in% unique(iv$Subreddit.i)])
mod3.clusters <- lm(delta.count.log ~ poly(author_density,2,raw=TRUE) + avg.subreddit.commensalism,df[Subreddit.i %in% unique(iv$Subreddit.i)])

anova(olsmod.clusters,mod3.clusters,mod2.clusters)

ypred <- ggpredict(olsmod, terms=c('author_density'),typical='median')#,data=df)
yeffect <- ggeffect(olsmod, terms=c('author_density [n=100]'),data=df,typical='median')
remember(ypred,"ggpredict.mod.density.dependence")
remember(yeffect,"ggeffect.mod.density.dependence")

remember(olsmod.clusters,"mod.density.dependence.clusters")
remember(mod2.clusters,"mod.avg.commensalism")
remember(mod3.clusters,"mod.density.avg.commensalism")

library(stargazer)
stargazer(mod2.clusters, olsmod.clusters, mod3.clusters)

remember(olsmod,'mod.density.dependence')

pdf("densityxgrowth-subreddits.pdf")
remember(df,"df.subreddit")
p <- ggplot(df,aes(y=delta.count.log,x=log1p(author_density))) + geom_bin2d(bins=80) + geom_smooth(method='lm', formula=y~x)
print(p)
dev.off()

by.cluster <- df[,.(mean.growth = mean(delta.count.log),
                    mean.author.density = mean(author_density),
                    mean.local.author.density=(mean(local.author.density))),by=.(author_cluster)]
by.cluster <- by.cluster[!is.na(mean.local.author.density) & !is.na(mean.growth)]


cor(by.cluster[,.(mean.growth,mean.local.author.density)])

pdf("densityxgrowth.pdf")

p <- ggplot(by.cluster,aes(x=mean.local.author.density,y=mean.growth)) + geom_bin2d() + geom_smooth()
remember(by.cluster,"cluster.density.by.growth")
    
print(p)

dev.off()

forecast.scores <- overall.scores()
remember(forecast.scores,"forecast.scores")

## select example communities from a 2x2 of competition-dominant, mutualism-dominant, mixed, and void
## Pick a competition-dominant community as one where avg.commensalism is in the bottom 5%.
## Pick a mutualism-dominant community as one where avg.commensalism is in the top 75%.
## We use different bounds because most communities are mutualistic and the most strongly mutualistic communities tend to be smaller.
## pick a random one with at least 5 subreddits (just because larger clusters are a little more interesting?)
dt.cluster <- dt.cluster[order(avg.commensalism), commense.rank := .I]
dt.cluster <- dt.cluster[order(avg.abs.commensalism), abs.commense.rank := .I]
dt.cluster <- dt.cluster[,commense.percentile := commense.rank / .N]
dt.cluster <- dt.cluster[,abs.commense.percentile := abs.commense.rank / .N]

# 1144 car maintainance 
# pick 631: datascience.
sub.clusters <- unique(all.ts,by=c("subreddit","author_cluster"))
big.clusters <- dt.cluster[N.edges >= 8]
big.clusters <- big.clusters[order(avg.commensalism), commense.rank := .I]
big.clusters <- big.clusters[order(avg.abs.commensalism), abs.commense.rank := .I]
big.clusters <- big.clusters[,commense.percentile := commense.rank / .N]
big.clusters <- big.clusters[,abs.commensalism.percentile := abs.commense.rank / .N]
comp.percentile <- 0.1
remember(comp.percentile, 'comp.percentile')
comp.communities <- big.clusters[(commense.percentile <= comp.percentile)]
remember(nrow(comp.communities), 'n.comp.clusters')
comp.communities <- comp.communities[order(avg.abs.commensalism),abs.commense.rank := .I]
comp.communities <- comp.communities[,abs.commense.percentile := abs.commense.rank / .N]
strong.comp.communities <- comp.communities#[(abs.commense.percentile > 0.5)]
strong.comp.communities <- strong.comp.communities[order(avg.commensalism)]
for(cluster in strong.comp.communities$author_cluster){
    print(cluster)
    print(sub.clusters[author_cluster==cluster]$subreddit)
}

remember(nrow(strong.comp.communities),'n.strong.comp.clusters')
strong.comp.community <- strong.comp.communities[sample(1:nrow(strong.comp.communities),1)]
strong.comp.community <- strong.comp.communities[author_cluster==101]
print(sub.clusters[strong.comp.community,on='author_cluster',.(subreddit)])


# let's go with 320 (redpill)
# wow found something interesting in the "imagesof" subreddits (cluster 973). It seems like a kind of special case though. like an attempt to crowdsource accurate sets of images based on automatic submission. It's a kind of wild case.
mut.percentile <- 0.9
mut.communities <- big.clusters[(commense.percentile > mut.percentile)]
remember(mut.percentile,'mut.percentile')
remember(nrow(mut.communities),'n.mut.clusters')
mut.communities <- mut.communities[order(avg.abs.commensalism),abs.commense.rank := .I]
mut.communities <- mut.communities[,abs.commense.percentile := abs.commense.rank / .N]
strong.mut.communities <- mut.communities#[(abs.commense.percentile > 0.5)]
remember(strong.mut.communities,'strong.mut.clusters')
strong.mut.community <- strong.mut.communities[author_cluster==320]
print(sub.clusters[strong.mut.community,on='author_cluster',.(subreddit)])

for(cluster in strong.mut.communities$author_cluster){
    print(cluster)
    print(sub.clusters[author_cluster==cluster]$subreddit)
}


## a mixed community has an average commensalism that's near 0 but an abs-avg commensalism that's big
# 468 mental health
big.clusters <- big.clusters[order(abs(avg.commensalism)),abs.commensalism.rank := .I]
big.clusters <- big.clusters[,abs.commensalism.percentile := abs.commensalism.rank / .N]

mixed.percentile <- 0.1
remember(mixed.percentile,'mixed.percentile')
near0.clusters <- big.clusters[abs.commensalism.percentile < mixed.percentile]
remember(nrow(near0.clusters),'n.near0.clusters')
near0.clusters <- near0.clusters[order(avg.abs.commensalism),abs.commense.rank := .I]
near0.clusters <- near0.clusters[order(avg.abs.commensalism),abs.commense.percentile := abs.commense.rank / .N]
mixed.communities <- near0.clusters[abs.commense.percentile > 0.6]
mixed.community <- mixed.communities[author_cluster==468]
remember(nrow(mixed.communities), 'n.mixed.clusters')
print(sub.clusters[mixed.community,on='author_cluster',.(subreddit)])

for(cluster in mixed.communities$author_cluster){
    print(cluster)
    print(sub.clusters[author_cluster==cluster]$subreddit)
}

## a void community has an average commensalism that's near 0 and an abs-avg commensalism that's near 0
## if we pick one pick 599 call of duty
## if we pick one pick 1 (weight loss)
void.percentile <- 0.1
remember(void.percentile, 'void.percentile')
void.clusters <- near0.clusters[abs.commense.percentile <= void.percentile]
void.community <- void.clusters[author_cluster==599]
remember(nrow(void.clusters),"n.void.clusters")
print(sub.clusters[void.community,on='author_cluster',.(subreddit)])

for(cluster in void.clusters$author_cluster){
    print(cluster)
    print(sub.clusters[author_cluster==cluster]$subreddit)
}
