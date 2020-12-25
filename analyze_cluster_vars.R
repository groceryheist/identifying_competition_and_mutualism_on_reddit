library(ggplot2)
library(data.table)
library(arrow)
source("RemembR/R/RemembeR.R")

load_var_coefs <- function(overwrite = FALSE){
 
    if(overwrite == TRUE){
        change.remember.file("var_ols_remember.RDS",clear=TRUE)

        n <- names(r)
        expr <- regexpr("var\\.coef\\.author_cluster_(.*)",n,perl=T)
        matches <- regmatches(n,expr)
        cluster.ids <- as.numeric(gsub("var\\.coef\\.author_cluster_","",matches))

                                        # goal: dataset of clid subreddit.i subreddit.j coef std.err t value pvalue
                                        # also include the trends and consts for completeness

        coefs.tab <- list()

        for(clid in cluster.ids){
            coefs <- r[[paste0('var.coef.author_cluster_',clid)]]
            for(sub.i in names(coefs)){
                coef.i <- coefs[[sub.i]]
                coef.names <- rownames(coef.i)
                coef.i <- as.data.table(coef.i)
                coef.i[,'Term':=coef.names]
                coef.i[,'Subreddit':=sub.i]
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

local.author.density <- function(clusters){
    author.similarities <- data.table(read_feather("/gscratch/comdata/output/reddit_similarity/comment_authors_10000.feather"))
    dt <- melt(author.similarities,id.vars='subreddit',variable.name='subreddit.j',value.name='author.similarity')
    cluster.pairs <- clusters[,.(subreddit.j=subreddit),by=.(author_cluster)][clusters,on=c("author_cluster"),allow.cartesian=T]
    dt <- dt[cluster.pairs,on=c("subreddit.j","subreddit")]

    local.density <- dt[,.(local.author.density=mean(author.similarity)),by=.(author_cluster)]
    return(local.density)
}

author.wang.density <- function(clusters){
   author.intersections <- data.table(read_feather("/gscratch/comdata/output/reddit_similarity/wang_similarity_10000_max10.feather"))
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
    commensal.by.clusters <- coefs.by.clusters[(Term!='const')&(Term!='trend') &(Subreddit.i != Subreddit.j),.(avg.commensalism=mean(Estimate),N.edges = .N,avg.abs.commensalism=mean(abs(Estimate))),by=.(author_cluster)]

    return(commensal.by.clusters)
}

wang.overlaps <- author.wang.density(clusters)

# what's the correlation between overlaps and commensalisms? 
coefs.tab <- load_var_coefs(overwrite=TRUE)
all.ts <- arrow::open_dataset("data/subreddit_timeseries.parquet")
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

clusters <- unique(all.ts,by=c("subreddit","author_cluster","term_cluster"))[,.(subreddit,author_cluster,term_cluster)]

df.1 <- all.ts[week.idx.rev == 24,.(subreddit, count)]
df.1 <- df.1[,count.tminus.24 := count]
df.1 <- df.1[,count := NULL]
df.2 <- all.ts[week.idx.rev == 1,.(subreddit, week, count, term_cluster, author_cluster, term_density, author_density)]

df <- df.1[df.2,on=c('subreddit')]
df <- df[,delta.count:=count - count.tminus.24]
df <- df[,delta.count.log:=log(count) - log(count.tminus.24)]

## the iv is the average Estimate
iv <- coefs.tab[(Term!='const')&(Term!='trend') &(Subreddit.i != Subreddit.j) ,.(avg.commensalism=mean(Estimate),N.edges = .N,total.commensalism=sum(Estimate),avg.abs.commensalism=mean(abs(Estimate)),sum.abs.commensalism=sum(abs(Estimate))),by=.(Subreddit.i)]

iv2 <- coefs.tab[(Term!='const')&(Term!='trend') &(Subreddit.i != Subreddit.j)&(coefs.tab[["Pr(>|t|)"]]<=0.05) ,.(avg.commensalism.sig=mean(Estimate),N.edges.sig = .N,total.commensalism.sig=sum(Estimate),positive.sig = sum(Estimate > 0), negative.sig = sum(Estimate < 0)),by=.(Subreddit.i)]

dt.commensal <- commensal.by.clusters(coefs.tab, clusters)

dt.density <- local.author.density(clusters)

dt <- dt.commensal[dt.density,on=.(author_cluster)]

dt.density2 <- local.author.wang.density(clusters)

dt <- dt[dt.density2,on=c("author_cluster")]

dt <- dt[!is.na(N.edges)]

# what's dt?
df <- dt[df,on=c("subreddit")]

dt <- dt[,abs.sub.avg.commensalism := avg.abs.commensalism - avg.commensalism]

dt <- setnames(dt,old=c("overlap"), new=c("local.wang.overlap"))

cor(dt[,.(local.author.density, local.wang.overlap, avg.abs.commensalism, abs.sub.avg.commensalism, avg.commensalism)])

plot.dt <- melt(dt, id.vars=c("author_cluster","N.edges","local.author.density",'local.wang.overlap'),measure.vars=c("avg.commensalism","avg.abs.commensalism",'abs.sub.avg.commensalism'))

plot.dt <- melt(plot.dt, id.vars=c("author_cluster","N.edges","variable","value"),measure.vars=c("local.author.density","local.wang.overlap"),value.name='overlap',variable.name='overlap.type')

# subreddit level
pdf("author_tfidf_heatmaps.pdf",width=12,height=6)
p <- ggplot(plot.dt, aes(x=overlap,y=value,group=variable)) + geom_bin2d(bins=50) + facet_wrap(overlap.type~variable,scales='free',ncol=3)
print(p)
dev.off()

dt[,diff.abs.avg := avg.abs.commensalism - avg.commensalism]

# cluster level
dt.cluster <- dt[,.(avg.commensalism=mean(avg.commensalism), avg.abs.commensalism=mean(avg.abs.commensalism), local.author.density = mean(local.author.density), local.wang.overlap = mean(local.wang.overlap), abs.sub.avg.commensalism = mean(abs.sub.avg.commensalism),N.edges=first(N.edges)),by=.(author_cluster)]

cor(dt.cluster[,.(local.author.density, local.wang.overlap, avg.abs.commensalism, abs.sub.avg.commensalism, avg.commensalism)])

plot.dt <- melt(dt.cluster, id.vars=c("author_cluster","N.edges","local.author.density",'local.wang.overlap'),measure.vars=c("avg.commensalism","avg.abs.commensalism",'abs.sub.avg.commensalism'))

plot.dt <- melt(plot.dt, id.vars=c("author_cluster","N.edges","variable","value"),measure.vars=c("local.author.density","local.wang.overlap"),value.name='overlap',variable.name='overlap.type')

# subreddit level
pdf("author_tfidf_heatmaps-by-cluster.pdf",width=12,height=6)
p <- ggplot(plot.dt, aes(x=overlap,y=value,group=variable)) + geom_bin2d(bins=50) + facet_wrap(overlap.type~variable,scales='free',ncol=3)
print(p)
dev.off()


## # subreddit level
## pdf("author_tfidf_heatmaps.pdf",width=12,height=6)
## p <- ggplot(plot.dt, aes(x=overlap,y=value,group=variable)) + geom_bin2d(bins=50) + facet_wrap(overlap.type~variable,scales='free',ncol=3)
## print(p)
## dev.off()

pdf("subreddit_absxavg.pdf",width=12,height=6)
p <- ggplot(dt, aes(x=avg.commensalism, y=avg.abs.commensalism)) + geom_bin2d(bins=50)
print(p)
dev.off()


pdf("cluster_absxavg.pdf",width=12,height=6)
p <- ggplot(dt.cluster, aes(x=avg.commensalism, y=avg.abs.commensalism)) + geom_bin2d(bins=50)
print(p)
dev.off()


dt[,diff.abs.avg := avg.abs.commensalism - avg.commensalism]


pdf("wang_overlap_heatmaps.pdf")

print(p)
dev.off()

df <- iv[df,on=c("Subreddit.i"="subreddit")]
df <- iv2[df,on=c("Subreddit.i")]

for (i in names(df))
    df[is.na(get(i)),(i):=0]


df <- wang.density[df,on=c("subreddit"="Subreddit.i")]
df <- df[!is.na(N.edges)]

###
author.similarities <- data.table(read_feather("/gscratch/comdata/output/reddit_similarity/comment_authors_10000.feather"))
term.similarities <- data.table(read_feather("/gscratch/comdata/output/reddit_similarity/comment_terms_10000.feather"))

### cluster level wang overlaps will be a trick.

### build local cluster similarities
cor(df[,.(avg.commensalism, total.commensalism, term_density, author_density, overlap, count, delta.count, N.edges,positive.sig,negative.sig,avg.abs.commensalism,sum.abs.commensalism)],method='spearman')

heatmap1.plot <- function(){

    

}

ecofallacy_plot <- function(df){
    
    by.cluster <- df[,.(mean.growth = mean(delta.count.log),
                        mean.overlap = mean(overlap),
                        mean.local.author.density=(mean(local.author.density))),by=.(author_cluster)]

    cor(by.cluster[,.(mean.growth,mean.overlap)])

    by.cluster <- by.cluster[,overlap.scaled := scale(log1p(mean.overlap),center=T,scale=T)]

    p <- ggplot(by.cluster,aes(x=mean.local.author.density,y=mean.growth)) + geom_point() + geom_smooth()
    
    print(p)

    dev.off()
}

m0 <- lm(delta.count.log ~ total.commensalism,df)
summary(m0)

m1 <- lm(delta.count.log ~ term_density+overlap,df)
summary(m1)

m2<- lm(delta.count.log ~ term_density+overlap+total.commensalism,df)
#+poly(term_density,2)+poly(overlap,2),df)
summary(m2)
