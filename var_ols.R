library(vars)
library(data.table)
library(ggplot2)
library(arrow)
library(scoringRules)
library(argparse) 
source("RemembR/R/RemembeR.R")

# y.pred is the output of predict(varest with ci = 682
avg.crps <- function(y.test, y.pred){
    crps.all <- c()
    for(i in 1:length(y.pred$fcst)){
        crps.i <- crps_lnorm(ylog.test[i,],y.pred$fcst[[i]][,1],y.pred$fcst[[i]][,4])
        crps.all <- c(crps.all,crps.i)
    }
    return(mean(crps.all))
}

rmse <- function(y.test, y.pred){
    sqerr <- c()
    for(i in 1:length(y.pred$fcst)){
        sqerr <- c(sqerr,(ylog.test[i,]-y.pred$fcst[[i]][,1])**2)
    }

    rmse.all <- sqrt(mean(sqerr))
    return(sum(rmse.all))
}

read.data <- function(filename){
   df <- as.data.table(read_feather(filename))
   df[,weekid:= 1:.N, by=.(subreddit)]
   return(df)
}

prep.data <- function(df){
    y <- dcast(df,week ~ subreddit,value.var = 'value')
    weeks <- y$week
    y <- y[,week:=NULL]
    y <- na.fill(y,0)
    y <- t(as.matrix(y))
    ylog <- log1p(y)
    return(list(ylog=ylog,weeks=weeks))
}

exogen.mat <- function(ylog){
    ## since we have a const and trend we'll have collinnearity if there are less than 2 weeks without edits:
    first.edits <- as.array(apply(ylog > 0,c(1),function(x) min(which(x))))
    all.0 <- (first.edits<=2) 
    exogen <- apply(first.edits[first.edits>2,drop=FALSE],c(1),function(x,ylen) c(1:(x-1),rep(0,ylen-x+1)),dim(ylog)[2])
    return(exogen)
}

train.test <- function(mat,n.test){
    if(is.null(dim(mat)) | dim(mat)[2]==0){
        return(list(train=NULL,test=NULL))
    }

    test <- mat[,(dim(mat)[2]-n.test+1):(dim(mat)[2]),drop=FALSE]
    train <- mat[,1:(dim(mat)[2]-n.test+1),drop=FALSE]
    return(list(train=train,test=test))
}

varm.restrictions <- function(ylog, K){
    first.edits <- as.array(apply(ylog > 0,c(1),function(x) min(which(x))))
    all.0 <- (first.edits<=2) 
    n.coefs <- K + 2 + (K-sum(all.0))
    retmat <- matrix(1,K,n.coefs)
    if(!all(all.0)){
        retmat[,((n.coefs-K+sum(all.0)+1):n.coefs)] <- diag(1,K,K)[,!all.0]
    }
    return(retmat)
}

baseline.restrictions <- function(ylog, K){
    first.edits <- as.array(apply(ylog > 0,c(1),function(x) min(which(x))))
    all.0 <- (first.edits<=2) 
    n.coefs <- K + 2 + (K-sum(all.0))
    retmat <- diag(1,K,n.coefs)
    retmat[,(K+1):(K+2)] <- 1
    if(!all(all.0)){
        retmat[,(n.coefs-K+sum(all.0)+1):n.coefs] <- diag(1,K,K)[,!all.0]
    }
    return(retmat)
}

plot.model.data <- function(varm,ylog,y.pred,weeks){
    df <- data.table(t(ylog))
    df <- df[,week := weeks]

    df <- melt(df, id.vars ='week', variable.name='subreddit', value.name='y')

    fitted <- data.table(fitted(varm))
    fitted <- fitted[,week:=weeks[1:(length(weeks)-n.test)]]

    fitted <- melt(fitted, id.vars = 'week', variable.name='subreddit', value.name='y.fit')
    df <- fitted[df,on=c('week','subreddit')]

    ypred.mean <- data.table(sapply(y.pred$fcst,function(l) l[,1]))
    ypred.mean[,week := weeks[(length(weeks)-n.test+1):length(weeks)]]
    ypred.mean <- melt(ypred.mean, id.vars='week', variable.name='subreddit', value.name='y.pred')

    ypred.lower <- data.table(sapply(y.pred$fcst,function(l) l[,2]))
    ypred.lower[,week := weeks[(length(weeks)-n.test+1):length(weeks)]]
    ypred.lower <- melt(ypred.lower, id.vars='week', variable.name='subreddit', value.name='y.pred.lower')

    ypred.upper <- data.table(sapply(y.pred$fcst,function(l) l[,3]))
    ypred.upper[,week := weeks[(length(weeks)-n.test+1):length(weeks)]]
    ypred.upper <- melt(ypred.upper, id.vars='week', variable.name='subreddit', value.name='y.pred.upper')

    df <- ypred.mean[df, on=c('week','subreddit')]
    df <- ypred.lower[df, on=c('week','subreddit')]
    df <- ypred.upper[df, on=c('week','subreddit')]
    return(df)
}

plot.model <- function(varm,ylog,y.pred,weeks){
    df <- plot.model.data(varm,ylog,y.pred,weeks)
    p <- ggplot(df, aes(x=week, y=y, group='subreddit'))
    p <- p + geom_line(color='black')
    p <- p + geom_line(aes(x=week,y=y.fit),color='red', linetype='dashed')
    p <- p + geom_line(aes(x=week,y=y.pred),color='red', linetype='dashed')
    p <- p + geom_ribbon(aes(x=week,ymax=y.pred.upper,ymin=y.pred.lower),alpha=0.8)
    p <- p + facet_wrap(.~subreddit,ncol=2)
    return(p=p)
}

plot.irf.data <- function(varm, ortho=FALSE, boot=TRUE, runs=1000, n.tries = 10, seed=NULL, ci=0.95){

    irf.data <- irf(varm,ortho=ortho,boot=boot,runs=runs,seed=seed,ci=ci)

    to.dt <- function(m,value.name){
        dt <- melt(data.table(m),variable.name='response.subreddit',measure.vars=colnames(m),value.name=value.name)
        dt <- dt[,x:= (1:.N)-1,by=.(response.subreddit)]
    }

    list.blocks <- lapply(irf.data$irf, to.dt, value.name='irf')

    if(!("Upper" %in% names(irf.data))){
        boot <- FALSE
    }

    if(boot == TRUE){
        upper.blocks <- lapply(irf.data$Upper,to.dt, value.name='irf.upper')
        lower.blocks <- lapply(irf.data$Lower,to.dt, value.name='irf.lower')
    }
    dt.blocks <- list()

    for(sr in names(list.blocks)){
        dt <- list.blocks[[sr]]
        dt <- dt[,impulse.subreddit := sr]
        if(boot == TRUE){
            up <- upper.blocks[[sr]]
            dt <- dt[,irf.upper := up$irf.upper]
            low <- lower.blocks[[sr]]
            dt <- dt[,irf.lower := low$irf.lower]
        }
        dt.blocks[[sr]] <- dt
    }

    dt <- rbindlist(dt.blocks)
    
    return(dt)
}

plot.irf <- function(varm, ortho=FALSE, boot=TRUE, runs=1000, data=NULL, seed=seed, ci=0.95){

    if(!(is.null(data))){
        df <- data
    }  else {
        df <- plot.irf.data(varm, ortho=ortho, boot=boot, runs=runs,seed=seed, ci=ci) 

    }

    if(!('irf.upper' %in% names(df)))
       boot <- FALSE

    if(boot == TRUE){
        p <- ggplot(df, aes(x=x,y=irf,ymax=irf.upper,ymin=irf.lower)) + geom_line()
        p <- p + geom_ribbon(alpha=0.8)
    } else{
        p <- ggplot(df, aes(x=x,y=irf)) + geom_line()
    }

    p <- p + facet_grid(impulse.subreddit ~ response.subreddit)

    return(p)
}

parser <- ArgumentParser(description="Fit var models using OLS on reddit data")
parser$add_argument("--path", type='character', help='path to a feather file containing data for a cluster')
parser$add_argument("--name", type='character', help='name to refer to this cluster')
parser$add_argument('--forecast-length', type='integer', help='length of forcast for evaluating forecast skill')
parser$add_argument('--runs', type='integer', help='number of bootstrap samples',default=1000)
args <- parser$parse_args() #("--path","tempdata_tf/author_cluster_17_tf.feather","--forecast-length","24", "--runs","200", "--name","author_cluster_17_tf"))

# check: 1881, 14, 17

print(args$name)

if(!exists('path'))
    path <- args$path

if(!exists('name')) 
   name <- args$name

if(!exists('n.test'))
    n.test <- args$forecast_length

if(!exists('boot'))
    runs <- 1000

dir.create("ols_models",showWarnings=FALSE)

change.remember.file(file.path("ols_models",paste0("var_ols_remember_",name,".RDS")), clear=TRUE)

df <- read.data(path)

if(!exists('ylog')){
    o <- prep.data(df)
    weeks <- o$weeks
    ylog <- o$ylog
}

exogen.1 <- exogen.mat(ylog)
o <- train.test(ylog,n.test)
ylog.test <- o$test
ylog.train <- o$train
o <- train.test(t(exogen.1),n.test)
if(!is.null(o$test)){
    exogen.test <<- t(o$test)
    exogen.train <<- t(o$train)
} else {
    exogen.test <<- NULL
    exogen.train <<- NULL
}

varm.base <- vars::VAR(y=t(ylog.train),type='both',lag.max=1,ic='SC',exogen=exogen.train)
K <- varm.base$K
n.coefs <- nrow(coef(varm.base)[[1]])

retmat.main <- varm.restrictions(ylog, K)
varm.main <- restrict(varm.base,method='manual',resmat=retmat.main)
remember(coef(varm.main),paste0("var.coef.",name))
saveRDS(varm.main,file.path("ols_models",paste0("var_ols_",name,".RDS")))

y.pred <- predict(varm.main,n.ahead=n.test,ci=0.682,dumvar=exogen.test)
remember(avg.crps(ylog.test, y.pred),paste0("var.crps.",name))
remember(y.pred, paste0("var.ypred.crps.",name))

y.pred <- predict(varm.main,n.ahead=n.test,dumvar=exogen.test)
remember(rmse(ylog.test, y.pred),paste0("var.rmse.",name))
remember(y.pred, paste0("var.ypred.rmse.",name))

p.main <- plot.model(varm.main, ylog, y.pred, weeks)
plot.data <- plot.model.data(varm.main, ylog, y.pred, weeks)
remember(plot.data, paste0("var.plot.data.",name))

max.tries <- 10

try.irf <- function(varm,ortho,boot,runs,max.tries,ci=0.95){
    if(max.tries == 0){
        print("FAILED TO FIT CI FOR IRF")
        return (plot.irf.data(varm,ortho=ortho,boot=FALSE,runs=runs,seed=max.tries,ci=ci))

    }
    tryCatch({
        irf.data <- plot.irf.data(varm,ortho=ortho,boot=boot,runs=runs,seed=max.tries,ci=ci)
        return(irf.data)
    },
    error = function(e){
        print(e)
        print(paste0("trying ",max.tries - 1,'more times'))
        try.irf(varm, ortho, boot, runs, max.tries - 1)
    }
    )
}

irf.ortho.data <- try.irf(varm.main,boot=TRUE, ortho=TRUE,runs,max.tries)
irf.ortho.data.90 <- try.irf(varm.main,boot=TRUE, ortho=TRUE,runs,max.tries,ci=0.9)
irf.ortho.data.85 <- try.irf(varm.main,boot=TRUE, ortho=TRUE,runs,max.tries,ci=0.85)

if(!is.null(irf.ortho.data)){
    remember(irf.ortho.data, paste0("irf.ortho.data.", name))
        
    p.ortho.irf <- plot.irf(varm.main,data=irf.ortho.data)
#    ggsave(paste0("plots/",name,"irf_ortho_main.pdf"),p.ortho.irf,width=16,height=11,units='in')
} else {
    remember('failed to estimate irf', paste0("irf.ortho.data.", name))
}

if(!is.null(irf.ortho.data.90)){
    remember(irf.ortho.data.90, paste0("irf.ortho.data.90.", name))
        
    p.ortho.irf <- plot.irf(varm.main,data=irf.ortho.data)
#    ggsave(paste0("plots/",name,"irf_ortho_main.pdf"),p.ortho.irf,width=16,height=11,units='in')
} else {
    remember('failed to estimate irf', paste0("irf.ortho.data.90.", name))
}

if(!is.null(irf.ortho.data.85)){
    remember(irf.ortho.data.85, paste0("irf.ortho.data.85.", name))
        
    p.ortho.irf <- plot.irf(varm.main,data=irf.ortho.data.85)
#    ggsave(paste0("plots/",name,"irf_ortho_main.pdf"),p.ortho.irf,width=16,height=11,units='in')
} else {
    remember('failed to estimate irf', paste0("irf.ortho.data.85.", name))
}


irf.data <- try.irf(varm.main,ortho=FALSE,boot=TRUE,runs,max.tries)
irf.data.90 <- try.irf(varm.main,ortho=FALSE,boot=TRUE,runs,max.tries,ci=0.9)
irf.data.85 <- try.irf(varm.main,ortho=FALSE,boot=TRUE,runs,max.tries,ci=0.85)

if(!is.null(irf.data)){
    remember(irf.data, paste0("irf.data.", name))
        
    p.irf <- plot.irf(varm.main,data=irf.data)
#    ggsave(paste0("plots/",name,"irf_ortho_main.pdf"),p.irf,width=16,height=11,units='in')
} else {
    remember('failed to estimate irf', paste0("irf.data.", name))
}


if(!is.null(irf.data.90)){
    remember(irf.data.90, paste0("irf.data.90.", name))
        
    p.irf <- plot.irf(varm.main,data=irf.data.90)
#    ggsave(paste0("plots/",name,"irf_ortho_main.pdf"),p.irf,width=16,height=11,units='in')
} else {
    remember('failed to estimate irf', paste0("irf.data.90.", name))
}


if(!is.null(irf.data.85)){
    remember(irf.data.85, paste0("irf.data.85.", name))
        
    p.irf <- plot.irf(varm.main,data=irf.data.85)
#    ggsave(paste0("plots/",name,"irf_ortho_main.pdf"),p.irf,width=16,height=11,units='in')
} else {
    remember('failed to estimate irf', paste0("irf.data.85.", name))
}

#ggsave(paste0("plots/",name,"_main.pdf"),p.main,width=16,height=11,units='in')

retmat.baseline <- baseline.restrictions(ylog,K)
varm.baseline <- restrict(varm.base,method='manual',resmat=retmat.baseline)
remember(coef(varm.baseline),paste0("ar.coef.",name))

saveRDS(varm.baseline,file.path("ols_models",paste0("baseline_ar_ols_",name,".RDS")))

y.pred <- predict(varm.baseline,n.ahead=n.test,ci=0.682,dumvar=exogen.test)
remember(y.pred, paste0("ar.ypred.crps.",name))
remember(avg.crps(ylog.test, y.pred),paste0("ar.crps.",name))

y.pred <- predict(varm.baseline,n.ahead=n.test,dumvar=exogen.test)
remember(y.pred, paste0("ar.ypred.rmse.",name))
remember(rmse(ylog.test, y.pred),paste0("ar.rmse.",name))
remember(ylog.test,paste0('ytest.',name))

p.baseline <- plot.model(varm.baseline, ylog, y.pred, weeks)
plot.data <- plot.model.data(varm.baseline, ylog, y.pred, weeks)

remember(plot.data, paste0("ar.plot.data.",name))

#ggsave(paste0("plots/",name,"_baseline.pdf"),p.baseline,width=16,height=11,units='in')
