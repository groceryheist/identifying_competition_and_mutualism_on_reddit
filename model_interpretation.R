library(arrow)
library(Matrix)
library(data.table)
library(ggplot2)
library(scoringRules)  ## package with forecast evaluation functions
library(parallel)
#setOption("mc.cores",24)
comp.stats <- function(draws){
    q <- quantile(draws,probs=c(0.025,0.05,0.075,0.1,0.15,0.175,0.2,0.5,0.8,0.85,0.875,0.90,0.925,0.95,0.975))
    result <- list("mean" = mean(draws),
                   "var" = var(draws))
    result <- c(result,q)
    return(result)
}

load.draws <- function(model.name){
    draws <- data.table(read_feather(paste0(file.path("stan_models",model.name),'.feather')))
    return(draws)
}

## validating impulse response functions requires permuting the order
reorder.mat <- function(m,order='random'){
    K = dim(m)[1]
    if(order=='random'){
        order <- sample(1:K)
    }
    pmat <- as(as.integer(order),"pMatrix")
    return(pmat%*%m%*%pmat)
}
    
extract.params <- function(draws){
    phi.cols <- names(draws)[grepl('phi',names(draws))]
    phi.max <- phi.cols[length(phi.cols)]
    fields <- strsplit(phi.max,',')[[1]]
    p <- as.integer(substr(fields[1],5,10000))
    m <- as.integer(substr(fields[2],1,10000))
    phi <- apply(draws[,phi.cols,with=F],1,function(df) array(as.list(as.numeric(df)),dim=c(m,m,p)))

    sigma.cols <- names(draws)[grepl('Sigma',names(draws))]
    sigma <- apply(draws[,sigma.cols,with=F],1,function(df) matrix(as.list(df),nrow=m))

    mu.cols <- names(draws)[grepl('^mu\\[',names(draws))]
    mu <- apply(draws[,mu.cols,with=F],1,function(df) as.array(as.list(df)))

    lambda.cols <- names(draws)[grepl('^lambda\\[',names(draws))]

    lambda <- draws[,lambda.cols,with=F]

    lambda.forecast.cols <- names(draws)[grepl('^lambda_new\\[', names(draws))]
    forecast.len <- as.integer(substr(strsplit(lambda.forecast.cols[length(lambda.forecast.cols)],',')[[1]],12,10000)[[1]])
    lambda.forecast <- draws[,lambda.forecast.cols,with=F]
    return(list(mu=mu, phi=phi, sigma=sigma, lambda=lambda, lambda.forecast=lambda.forecast))
}


irf.to.units <- function(irf.forecast, med.mu){
    result <- copy(irf.forecast)
    forecast.len <- length(result)
    ndraws <- length(irf.forecast[[1]])
    func <- function(ts, ndraws, result, med.mu){
        res <- list()
        for(draw_id in 1:ndraws){
            lambda <- result[[ts]][[draw_id]] + med.mu
            ## res[[draw_id]] <- apply(lambda,c(1,2),function(l) rpois(1,exp(l)))
            res[[draw_id]] <- exp(lambda)
        }
        return(res)
    }

#    return(mclapply(1:forecast.len,func, ndraws, result, med.mu, mc.cores=22))
    return(lapply(1:forecast.len,func, ndraws, result, med.mu))
}

get.stationary.distribution <- function(phi, mu){
    m <- length(mu[[1]])
    
    func <- function(draw.id, phi, mu, m){
        base::solve(diag(1, m) - matrix(as.numeric(phi[[draw.id]][,,]),nrow=m))%*%as.numeric(mu[[draw.id]])
    }
#    return(mclapply(1:length(phi), func, phi, mu, m,mc.cores=24))
    return(lapply(1:length(phi), func, phi, mu, m))
}

## impulse response function:
## input draws x m x m x p
## ouput draws x m x m x forecast.len 
get.irf.forecast <- function(phi, mu, forecast.len = 20){
    m <- dim(phi[[1]])[1]
    p <- dim(phi[[1]])[3]
    ndraws <- length(phi)

    
    # function getting the next irf for a specific draw
    next.irf <- function(irf.draw,mu.draw,phi.draw){
        if(length(irf.draw) == 0){
            return(diag(m))
        }

        res <- matrix(0,m,m)
        if(length(irf.draw) < p){
            for(i in 1:length(irf.draw)){
                res <- res + matrix(as.numeric(irf.draw[[length(irf.draw) - i + 1]]),m) %*% matrix(as.numeric(phi.draw[,,i]),m)
            }
            for(i in (1+length(irf.draw)):p){
                res <- res + diag(1,m) %*% matrix(as.numeric(phi.draw[,,i]),m)
            }
        } else {
            for(i in 1:p){
                res <- res + matrix(as.numeric(irf.draw[[length(irf.draw) - i + 1]]),m) %*% matrix(as.numeric(phi.draw[,,i]),m)
            }
        }

        return(res)
    }


    func <- function(id, t, phi, mu, irf){
            phi.draw <- phi[[id]]
            mu.draw <- mu[[id]]
            irf.draw <- list()
            if(t == 1){
                return(next.irf(irf.draw, mu.draw, phi.draw))
            } else {
                for(i in 1:(t-1)){
                    irf.draw <- c(irf.draw,list(irf[[i]][[id]]))
                }

                return(next.irf(irf.draw, mu.draw, phi.draw))
            }              
    }

    irf <- list()
    for(t in 1:forecast.len){
#        irf[[t]] <- mclapply(1:length(phi), func, t, phi, mu, irf)
        irf[[t]] <- lapply(1:length(phi), func, t, phi, mu, irf)
    }
    
    return(irf)
}

get.stationary.median <- function(stat.dist){
    m <- dim(stat.dist[[1]])[1]
    res <- c()
    
    for(i in 1:m){
        draws <- c()
        for(draw in 1:length(stat.dist)){
            draws <- c(draws,stat.dist[[draw]][i])
        }
        res <- c(res,median(draws))
    }
    return(res)
}

get.irf.ortho <- function(phi, mu, sigma, forecast.len = 20){
    m <- dim(phi[[1]])[1]
    p <- dim(phi[[1]])[3]
    ndraws <- length(phi)
    irf.forecast <- get.irf.forecast(phi, mu, forecast.len)
    result <- list()
    F <- lapply(sigma,function(s) chol(s))

    for(i in 1:forecast.len){
        result[[i]] <- mapply(function(p, f) matrix(as.numeric(p),m) %*% t(matrix(f,m)),
                            irf.forecast[[i]],
                            F,
                            SIMPLIFY=FALSE)
    }
            
    return(result)
}
# 1 2 3 4
# a a b b
# a b a b
irf.to.plotdata <- function(irf, matnames){

    if(length(matnames) != dim(irf[[1]][[1]])[1]){
        print("ERROR! Name length does not match dimesion of irf")
        return()
    }
    
    ## TARGET: subreddit i, subreddit j, 95%, 80%, 50%, 20%, 5%, mean, sd, x
    stat.rows <- function(d, matnames){

        m <- dim(d[[1]])[1]

        dflat <- unlist(d)
        colnames <- c(sapply(matnames,function(n) rep(n, m)))
        rownames <- rep(matnames, m)
        rows <- list()

        func <- function(i, m, dflat, colnames){
            if(i == m**2){
                idx <- 0
            } else {
                idx <- i
            }
            stats <- comp.stats(dflat[1:length(dflat) %% (m**2) == idx])
            return(c(list('name.row' = rownames[i],'name.col' = colnames[i]),stats))
        }

#        rows <- mclapply(1:m**2, func, m, dflat, colnames,mc.cores=24)
        rows <- lapply(1:m**2, func, m, dflat, colnames)

        return(rows)
    }

    result <- list()

 for(i in 1:length(irf)){
        rows <- stat.rows(irf[[i]], matnames)
        rows <- lapply(rows, function(r) c(r,list('x'=i)))
        result <- c(result,rows)
    }
    return(rbindlist(result))
}

plot.irf <- function(plotdata, matnames){
    plot.data <- copy(plotdata)
    
    plot.data <- plot.data[,":="(name.row = factor(name.row,levels=matnames),
                                 name.col = factor(name.col,levels=matnames))]
    setnames(plot.data,old=c("5%","95%"),new=c("lower","upper"),skip_absent=T)
    p <- ggplot(plot.data, aes(x=x, y=mean, ymin=lower,ymax=upper)) + geom_line() + geom_ribbon(alpha=0.5) + facet_grid(name.row ~ name.col)
    pdf('plots/test_irf.pdf')
    print(p)
    dev.off()

    return(p)
}

plot.irf.focal <- function(plotdata, matnames, focal.sub = 'seattle',level='95'){
    plot.data <- copy(plotdata)

    if(level=='95')
        setnames(plot.data,old=c('97.5%','2.5%'),new=c('upper','lower'),skip_absent=T)
    if(level=='90')
        setnames(plot.data,old=c('95%','5%'),new=c('upper','lower'),skip_absent=T)
    if(level=='85')
        setnames(plot.data,old=c('92.5%','7.5%'),new=c('upper','lower'),skip_absent=T)
    if(level=='80')
        setnames(plot.data,old=c('90%','10%'),new=c('upper','lower'),skip_absent=T)

    plot.data <- plot.data[(name.row == focal.sub) | (name.col==focal.sub)]
    plot.data <- plot.data[(name.row != name.col)]
    plot.data <- plot.data[,":="(name.row = factor(name.row,levels=matnames),
                                 name.col = factor(name.col,levels=matnames),
                                 dir=ifelse(name.row==focal.sub,paste0("on ",focal.sub),paste0('of ',focal.sub)),
                                 name=ifelse(name.row==focal.sub,name.col,name.row)
                                 )]
    


    p <- ggplot(plot.data, aes(x=x, y=mean, ymin=lower,ymax=upper)) + geom_line() + geom_ribbon(alpha=0.5) + facet_grid(name~dir)
    pdf('plots/test_irf_focal.pdf',width=6,height=18)
    print(p)
    dev.off()

    return(p)

}

set.sig.level <- function(df,level){
    if(level=='95')
        old <- c('97.5%','2.5%')
    if(level=='90')
        old <- c('95%','5%')
    if(level=='85')
        old <- c('92.5%','7.5%')
    if(level=='80')
        old <- c('90%','10%')

    new <- c('upper','lower')
    
    if('data.table' %in% class(df)){
        print(old)
        print(new)
        print(names(df))
        setnames(df,old=old,new=new)
    } 
    
    if('list' %in% class(df)){
        names(df)[names(df)==old[1]] <- new[1]
        names(df)[names(df)==old[2]] <- new[2]
    }

    return(df)
}

## we say the short-run relationship is the first one that is statistically significant
get.community.edgelists <- function(irf.plotdata,level='90'){
    irf.plotdata <- copy(irf.plotdata)
    irf.plotdata <- set.sig.level(irf.plotdata,level)

    first.negative <- irf.plotdata[upper < 0,.(x=min(x)),by=c("name.row","name.col")]
    first.negative <- first.negative[,'type':='competitor']
    first.positive <- irf.plotdata[lower > 0,.(x=min(x)),by=c("name.row","name.col")]
    first.positive <- first.positive[,'type':='mutualist']

    last.negative <-  irf.plotdata[upper < 0,.(x=max(x)),by=c("name.row","name.col")]

    last.negative <- last.negative[,'type':='competitor']
    last.positive <-  irf.plotdata[lower > 0,.(x=max(x)),by=c("name.row","name.col")]
    last.positive <- last.positive[,'type':='mutualist']
 
    long.run <- rbind(last.negative,last.positive)
    long.run <- long.run[long.run[,.(x=max(x)),by=.(name.row,name.col)],on=.(name.row,name.col,x)]
    short.run <- rbind(first.negative,first.positive)
    short.run <- short.run[short.run[,.(x=min(x)),by=.(name.row,name.col)],on=.(name.row,name.col,x)]
    
    return(list('long.run' = long.run, 'short.run' = short.run))
}

get.forecast.plotdata <- function(fit.df, forecast.df, params){

    subreddit.names <- unique(fit.df$subreddit)

    get.stats <- function(l, subreddit.names){
        rows <- list()
        i <- 1
        for(col in names(l)){
            x <- as.integer(gsub("[^0-9.-]","",strsplit(col,',')[[1]][1],8,10000)[[1]][1])
            subreddit_idx <- as.integer(gsub(']','',strsplit(col,',')[[1]][2]))
            subreddit <- subreddit.names[[subreddit_idx]]
            stats <- comp.stats(sapply(l[[col]],function(lambda) rpois(1,exp(lambda))))
            rows[[i]] <- c(list('subreddit'=subreddit,x=x),stats)
            i <- i + 1
        }
        return(rbindlist(rows))
    }

    lambda <- params$lambda
    lambda.forecast <- params$lambda.forecast
    model.length <- dim(lambda[[1]])[1]
    forecast.length <- dim(lambda.forecast[[1]])[1]

    fit.weeks <- unique(fit.df$week)
    forecast.weeks <- unique(forecast.df$week)

    fit.stats <- get.stats(lambda,subreddit.names)
    fit.stats <- fit.stats[order(subreddit,x)]
    fit.stats[,week := rep(fit.weeks, length(subreddit.names))]

    forecast.stats <- get.stats(lambda.forecast,subreddit.names)
    forecast.stats <- forecast.stats[order(subreddit,x)]

    forecast.stats <- forecast.stats[,week := rep(forecast.weeks, length(subreddit.names))]
    
    fit.stats[, type := 'fit']
    forecast.stats[, type := 'forecast']
    df <- rbind(fit.stats,forecast.stats)
    return(df)
}

plot.forecast <- function(pred.df, fit.df, holdout.df, level='95'){
    pred.df <- copy(pred.df)
    
    fit.df <- fit.df[,type:='fit']
    holdout.df <- holdout.df[,type:='forecast']
    df <- rbind(fit.df,holdout.df)
    pred.df <- set.sig.level(pred.df,level)
    pred.df[,median:=pred.df[['50%']]]
    pdf("plots/test_forecast.pdf",width=14,height=14)
    p <- ggplot() + geom_line(aes(x=week,y=mean),data=pred.df,color='red') + geom_ribbon(aes(x=week,y=median,ymax=upper,ymin=lower),data=pred.df,alpha=0.4)
    p <- p + geom_line(aes(x=week,y=N_authors,linetype=type),data=df)
    p <- p + facet_wrap(.~subreddit,scales='free_y')
    print(p)

    dev.off()

}

get.phi.stats <- function(draws, level='95'){
    phi.cols <- names(draws)[grepl('phi',names(draws))]
    phi.max <- phi.cols[length(phi.cols)]
    fields <- strsplit(phi.max,',')[[1]]
    
    p <- as.integer(substr(fields[1],5,10000))
    m <- as.integer(substr(fields[2],1,10000))

    means <- array(dim=c(m,m,p))
    medians <- array(dim=c(m,m,p))
    upper <- array(dim=c(m,m,p))
    lower <- array(dim=c(m,m,p))

    i <- 0

    for(col in phi.cols){
        fields <- strsplit(col,',')[[1]]

        pi <- as.integer(gsub('^.*\\[','',fields[[1]]))
        mi <- as.integer(fields[[2]])
        mj <- as.integer(gsub('\\]','',fields[[3]]))

        print(paste0('[',mi,',',mj,',',pi,']'))
        stats <- comp.stats(draws[[col]])
        stats <- set.sig.level(stats,level)
        means[mi,mj,pi] <- stats$mean
        medians[mi,mj,pi] <- stats[['50%']]
        upper[mi,mj,pi] <- stats$upper
        lower[mi,mj,pi] <- stats$lower
    }

    return(list('means'=means,'medians'=medians,'upper'=upper,'lower'=lower))
    
}

get.mu.stats <- function(draws,level='95'){
    mu.cols <- names(draws)[grepl('^mu\\[',names(draws))]
    m <- length(mu.cols)
    result <- list()
    for(col in mu.cols){
        stats <- comp.stats(draws[[col]])
        stats <- set.sig.level(stats,level)
        result[[col]] <- stats

   }
    rownames <- names(stats)
    result <- as.data.table(result)
    result[['stat']] <- rownames
    return(result)
}



get.forecast.scores <- function(draws,holdout.df){
    subreddits <- unique(holdout.df$subreddit)
    out.metrics <- list()

    for(sr in subreddits){
        y <- holdout.df[sr==subreddit,N_authors]
        sridx <- which(subreddits==sr)
        forecast.len <- length(y)

        par.draws <- draws[,grepl(paste0('lambda_new\\[.*,',sridx,'\\]'),names(draws),perl=T),with=F]
        lambda <- exp(par.draws)
        dat <- t(apply(lambda, c(1,2), function(l) rpois(1,l)))
        crps_est <- mean(crps_sample(y,dat=dat,method='edf'))
        logs_est <- mean(logs_sample(y,dat=dat))
        out.metrics[[paste0('crps_',sr)]] <- crps_est
        out.metrics[[paste0('logs_',sr)]] <- logs_est

    }
    y <- dcast(holdout.df,subreddit ~ week,value.var='N_authors')
    y[['subreddit']] <- NULL
    y <- as.matrix(y)

    for(i in 1:forecast.len){
        dat <- t(as.matrix(draws[,grepl(paste0('lambda_new\\[',i,',.*\\]'),names(draws),perl=T),with=F]))
        dat <- apply(exp(dat),c(1,2),function(l) rpois(1,l))
        out.metrics[[paste0('ESm_',i)]] <- es_sample(y=y[,i],dat=dat)
        out.metrics[['ESm']] <- mean(unlist(out.metrics[grepl('ESm.*',names(out.metrics))]))
    }
    return(out.metrics)
}

select_model <- function(models, holdout.df){

    forecast.stats <- list()

    for(model in models){
        draws <- load.draws(model)
        stat <- get.forecast.scores(draws,holdout.df)
        stat['model'] <- model
        forecast.stats <- append(forecast.stats, list(stat))
    }
    
    forecast.stats <- rbindlist(forecast.stats)
    return(forecast.stats)
}

select_wallpaper_model <- function(){

    models <- c("wallpaper/var_p1_all_stanmod",
                "wallpaper/var_p2_all_stanmod",
                "wallpaper/var_p3_all_stanmod")


    holdout.df <- data.table(read_feather("data/var_wallpaper_data_forecast.feather"))
    model.stats <- select_model(models, holdout.df)
    preferred.model <- model.stats[ESm == min(model.stats$ESm)]$model
    return(list(model.stats = 'model.stats', preferred.model = preferred.model))
}

remember.model <- function(draws.file, fit.file, forecast.file, prefix){

    draws <- load.draws(draws.file)
    params <- extract.params(draws)
    fit.df <- data.table(read_feather(fit.file))
    forecast.df <- data.table(read_feather(forecast.file))

    forecast.plot.data <- get.forecast.plotdata(fit.df, forecast.df, params)
    remember(forecast.plot.data, paste0(prefix,'.forecast.data'))
    remember(fit.df, paste0(prefix,'.fit.df'))
    remember(forecast.df, paste0(prefix,'.forecast.df'))
    plot.forecast(forecast.plot.data,fit.df,forecast.df)

    forecast.len <- first(unique(forecast.df[,.N,by=.(subreddit)])$N)

    phi <- params[['phi']]
    mu <- params[['mu']]
    sigma <- params[['sigma']]

    included.subreddits <- unique(fit.df$subreddit)

    irf.forecast <- get.irf.forecast(phi,mu,forecast.len)
    irf.forecast.plotdata <- irf.to.plotdata(irf.forecast, included.subreddits)
    remember(irf.forecast.plotdata, paste0(prefix,'.irf.forecast.data'))

    forecast.edgelist.95 <- get.community.edgelists(irf.forecast.plotdata, level='95')
    remember(forecast.edgelist.95, paste0(prefix,'.forecast.edgelist.95'))

    forecast.edgelist.90 <- get.community.edgelists(irf.forecast.plotdata, level='90')
    remember(forecast.edgelist.90, paste0(prefix,'.forecast.edgelist.90'))

    forecast.edgelist.85 <- get.community.edgelists(irf.forecast.plotdata, level='85')
    remember(forecast.edgelist.85, paste0(prefix,'.forecast.edgelist.85'))

    forecast.edgelist.80 <- get.community.edgelists(irf.forecast.plotdata, level='80')
    remember(forecast.edgelist.80, paste0(prefix,'.forecast.edgelist.80'))

    irf.ortho <- get.irf.ortho(phi,mu,sigma,forecast.len)
    irf.ortho.plotdata <- irf.to.plotdata(irf.ortho, included.subreddits)
    remember(irf.ortho.plotdata, paste0(prefix,'.irf.ortho.data'))

    ortho.edgelist.95 <- get.community.edgelists(irf.ortho.plotdata, level='95')
    remember(ortho.edgelist.95, paste0(prefix,'.ortho.edgelist.95'))

    ortho.edgelist.90 <- get.community.edgelists(irf.ortho.plotdata, level='90')
    remember(ortho.edgelist.90, paste0(prefix,'.ortho.edgelist.90'))

    ortho.edgelist.85 <- get.community.edgelists(irf.ortho.plotdata, level='85')
    remember(ortho.edgelist.85, paste0(prefix,'.ortho.edgelist.85'))

    ortho.edgelist.80 <- get.community.edgelists(irf.ortho.plotdata, level='80')
    remember(ortho.edgelist.80, paste0(prefix,'.ortho.edgelist.80'))
    
    mu.stats.80 <- get.mu.stats(draws,level='80')
    remember(mu.stats.80,paste0(prefix,'.mu.stats'))
    med.mu = mu.stats.80[stat=='50%']
    med.mu[['stat']] <- NULL
    med.mu <- unlist(med.mu)
    med.mu <- unlist(fit.df[,.(mu=median(N_authors)),by=.(subreddit)]$mu)
    remember(med.mu, paste0(prefix,'.med.mu'))

    irf.ortho.units <- irf.to.units(irf.ortho, log(med.mu))
    irf.ortho.units.plotdata <- irf.to.plotdata(irf.ortho.units, included.subreddits)

    irf.forecast.units <- irf.to.units(irf.forecast, log(med.mu))
    irf.forecast.units.plotdata <- irf.to.plotdata(irf.forecast.units, included.subreddits)

    remember(irf.forecast.units.plotdata, paste0(prefix,'.irf.forecast.units.data'))
     
    phi.stats.80 <- get.phi.stats(draws,level='80')
    remember(phi.stats.80,paste0(prefix,'.phi.stats.80'))

    phi.stats.85 <- get.phi.stats(draws,level='85')
    remember(phi.stats.85,paste0(prefix,'.phi.stats.85'))

    phi.stats.90 <- get.phi.stats(draws,level='90')
    remember(phi.stats.90,paste0(prefix,'.phi.stats.90'))

    phi.stats.95 <- get.phi.stats(draws,level='95')
    remember(phi.stats.95,paste0(prefix,'.phi.stats.95'))

    


}

if (sys.nframe() == 0){
#    model.name <- 'var_stan_p5_stanmod'
    o <- select_wallpaper_model()
    model.name <- o$preferred.model
    model.stats <- o$model.stats
    draws <- load.draws(model.name)
    params <- extract.params(draws)

    mu <- params[['mu']]
    phi <- params[['phi']]

    fit.df <- data.table(read_feather("data/var_wallpaper_data_fit.feather"))
    holdout.df <- data.table(read_feather("data/var_wallpaper_data_forecast.feather"))
    included.subreddits <- unique(fit.df$subreddit)

    pred.df <- get.forecast.plotdata(fit.df,holdout.df,params)

    plot.forecast(pred.df,fit.df,holdout.df)

    ## for(i in 1:length(phi)){
    ##     for(k in 1:dim(phi[[i]])[3]){
    ##         phi[[i]][,,k] <- matrix(as.numeric(phi[[i]][[k]] * matrix(c(0.5,0.5,-0.5,0.5),2)),2)
    ##     }
    ## }

    sigma <- params[['sigma']]
                                        # the forecast irf doesn't account for simultaneous covariance.
    irf.forecast <- get.irf.forecast(phi,8)
    irf.ortho <- get.irf.ortho(phi,sigma,8)

    pred.df <- irf.to.plotdata(irf.forecast,included.subreddits)

    plot.irf.focal(pred.df, included.subreddits,focal.sub = 'wallpaper')

    plot.data <- irf.to.plotdata(irf.ortho,included.subreddits)

    plot.irf.focal(plot.data, included.subreddits,focal.sub = 'wallpaper')

    edgelists <-get.community.edgelists(plot.data,level='95')

    ## remove self-edges
    shortrun <- edgelists$short
    shortrun <- shortrun[name.row != name.col]

    longrun <- edgelists$long
    longrun <- longrun[name.row != name.col]
}


