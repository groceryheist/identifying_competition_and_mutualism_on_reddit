library(ggplot2)
library(arrow)
library(data.table)
library(scoringRules)  ## package with forecast evaluation functions
library(Matrix)
files <- list.files("data")
files <- files[grepl("ts_term_cluster.*.feather",files)]
for(infile in files){

    df <- data.table(read_feather(paste0("data/",infile)))
    p <- ggplot(df,aes(x=week,y=value,group=subreddit))
    p <- p + geom_line()
    p <- p + facet_wrap('~ subreddit')
#    p <- p + scale_y
    ggsave(paste0('plots/',infile,'png'),p,device='png',height=11.7,width=8.27)

    df[,delta:=c(NA,diff(value))] 
    p <- ggplot(df,aes(x=week,y=delta,group=subreddit))
    p <- p + geom_line()
    p <- p + facet_wrap('~ subreddit',ncol=2)

    ggsave(paste0('plots/',infile,'delta','.png'),p,device='png',height=22.7,width=8.27)

    
}

library(MARSS)
df <- as.data.table(read_feather("data/ts_term_cluster_386.feather"))

df[,weekid:= 1:.N, by=.(subreddit)]

y <- dcast(df,week ~ subreddit,value.var = 'value')
y <- y[,week:=NULL]
y <- t(as.matrix(y))

df <- dcast(df,week~subreddit,value.var='value')
df <- data.table(na.fill(df,0))
df <- melt(df,id.vars='week',variable.name='subreddit')
df <- df[,delta := c(NA,diff(as.numeric(value))),by=.(subreddit)]

diffdf <- df[!is.na(delta)]
diffdf <- dcast(diffdf,week ~ subreddit,value.var = 'delta')
ydiff <- diffdf[,week:=NULL]
ydiff <- t(as.matrix(ydiff))

ylog <- log1p(y)

model_gen <- list(B="unconstrained",Q="diagonal and unequal",R="diagonal and unequal", U="unequal",A='unconstrained')

n.test <- 30
ylog.test <- ylog[,(dim(ylog)[2]-n.test+1):dim(ylog)[2]]
ylog.train <- ylog[,1:(dim(ylog)[2]-n.test)]

first.edits <- as.array(apply(ylog > 0,c(1),function(x) min(which(x))))
all.0 <- first.edits==1
exogen <- apply(first.edits[first.edits!=1],c(1),function(x,ylen) c(1:(x-1),rep(0,ylen-x+1)),dim(ylog)[2])
exogen.test <- exogen[(dim(ylog)[2]-n.test+1):dim(ylog)[2],]
exogen.train <- exogen[1:(dim(ylog)[2]-n.test),]

n.vars <- dim(ylog.train)[1]
varm.base <- vars::VAR(y=t(ylog.train),type='both',lag.max=5,ic='SC',exogen=exogen.train)
lags <- varm.base$p
n.coefs <- nrow(coef(varm.base)[[1]])
K <- varm.base$K
retmat.var <- matrix(1,K,n.coefs)
retmat.var[,((n.coefs-K+sum(all.0)+1):n.coefs)] <- diag(1,K,K)[,!all.0]

varm <- restrict(varm.base,method='manual',resmat=retmat.var)

y.pred <- predict(varm,ylog.test,n.ahead=n.test,ci=0.682,dumvar=exogen.test)
avg.crps(ylog.test,y.pred)
avg.rmse(ylog.test,y.pred)

retmat <- diag(1,K,n.coefs)
retmat[,(K+1):(K+2)] <- 1
retmat[,(n.coefs-K+sum(all.0)+1):n.coefs] <- diag(1,K,K)[,!all.0]

baseline <- restrict(varm.base,method='manual',resmat=retmat)
y.pred.base <- predict(baseline, n.ahead=n.test,ci=0.682,dumvar=exogen.test)
avg.crps(ylog.test, y.pred.base)
avg.rmse(ylog.test, y.pred.base)

pdf('plots.pdf')
plot(varm)
dev.off()
pdf('irfs.pdf')
irfm <- irf(varm,ortho=F)
plot(irfm)
dev.off()

pdf('plots_baseline.pdf')
plot(baseline)
dev.off()

pdf('irfs.pdf')
irfm <- irf(varm,ortho=F)
plot(irfm)
dev.off()




delta_varm <- vars::VAR(y=t(ydiff),type='none',lag.max=9) #,type='trend')

pdf('delta_plots.pdf')
plot(delta_varm)
dev.off()

pdf('delta_irfs.pdf')
irfm <- irf(delta_varm,ortho=F)
plot(irfm)
dev.off()

crps_lnorm(ylog.test[1,],y.pred$fcst[[1]][,1],y.pred$fcst[[1]][,4])

crps_lnorm(ylog.test[1,],y.pred.base$fcst[[1]][,1],y.pred.base$fcst[[1]][,4])

marss_model <- MARSS(ytest,model_gen,control=list(conv.test.slope.tol=0.1))
resids <- MARSSresiduals(marss_model, type='tt1')$model.residuals

# MARSSparamCIs(marss_model,method='innovations')
