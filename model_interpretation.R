library(arrow)
library(Matrix)
library(data.table)

load.draws <- function(model.name){
    draws <- read_feather(paste0(file.path("stan_models",model.name),'.feather'))
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
    p <- as.integer(substr(fields[1],5,5))
    m <- as.integer(substr(fields[2],1,1))
    phi <- apply(draws[,phi.cols,with=F],1,function(df) array(as.list(as.numeric(df)),dim=c(m,m,p)))

    sigma.cols <- names(draws)[grepl('Sigma',names(draws))]
    sigma <- apply(draws[,sigma.cols,with=F],1,function(df) matrix(as.list(df),nrow=m))

    mu.cols <- names(draws)[grepl('^mu\\[',names(draws))]
    mu <- apply(draws[,mu.cols,with=F],1,function(df) as.array(as.list(df)))

    return(list(mu=mu, phi=phi, sigma=sigma))
}


## impulse response function:
## input draws x m x m x p
## ouput draws x m x m x forecast.len 
get.irf.forecast <- function(phi, forecast.len = 20){
    m <- dim(phi[[1]])[1]
    p <- dim(phi[[1]])[3]
    ndraws <- length(phi)

    ## they all start with the same initial condition (i.e. phi.0 = diag(m))
    ## but from there they each have their own path.
    phi.0s <- lapply(1:ndraws, function(c) diag(m))
    result <- list(phi.0s)
    for(i in 2:forecast.len){
#    result <- Reduce(function(phi.0s,i)
        result[[i]] <-mapply(
            function(phi.draw, phi.0)
                Reduce(function(x,y) matrix(as.numeric(x),m) + matrix(as.numeric(y),m),
                       apply(phi.draw, 3, function(phi.i) array(as.list(phi.0 %*% matrix(as.numeric(phi.i),m)),dim=c(m,m))),
                       init=matrix(0,m,m)),
            phi,
            phi.0s,
            SIMPLIFY=FALSE)
        phi.0s <- result[[i]]
    }
#        init = lapply(1:ndraws,function(c) phi.0s))

    return(result)
}

get.irf.ortho <- function(phi, sigma, forecast.len = 20){
    irf.forecast <- get.irf.forecast(phi, forecast.len)
    result <- list()
    F <- lapply(sigma,function(s) chol(s))
    for(i in 1:forecast.len){
        result[[i]] <- mapply(function(p, f) matrix(as.numeric(p),m) %*% matrix(f,m),
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

        comp.stats <- function(draws){
            q <- quantile(draws,probs=c(0.05,0.2,0.5,0.8,0.95))
            result <- list("mean" = mean(draws),
                           "var" = var(draws))
            result <- c(result,q)
            return(result)
        }

        m <- dim(d[[1]])[1]

        dflat <- unlist(d)
        colnames <- c(sapply(matnames,function(n) rep(n, m)))
        rownames <- rep(matnames, m)
        rows <- list()
        for(i in 1:m**2){
            if(i == 4){
                idx <- 0
            } else {
                idx <- i
            }
            stats <- comp.stats(dflat[1:length(dflat) %% m**2 == idx])
            rows[[i]] <- c(list('name.row' = rownames[i],'name.col' = colnames[i]),stats)
        }
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

plot.irf <- function(irf, matnames){
    
    plot.data <- irf.to.plotdata(irf.ortho, c('seattle','seahawks'))
    plot.data <- plot.data[,":="(name.row = factor(name.row,levels=matnames),
                                 name.col = factor(name.col,levels=matnames))]
    setnames(plot.data,old=c("5%","95%"),new=c("lower","upper"))
    p <- ggplot(plot.data, aes(x=x, y=mean, ymin=lower,ymax=upper)) + geom_line() + geom_ribbon(alpha=0.5) + facet_grid(name.col ~ name.row)
    pdf('plots/test_irf.pdf')
    print(p)
    dev.off()

    return(p)
}

model.name <- 'test_stan_stanmod'
draws <- load.draws(model.name)
draws <- as.data.table(draws)
params <- extract.params(draws)
mu <- params[['mu']]
phi <- params[['phi']]
sigma <- params[['sigma']]

# the forecast irf doesn't account for simultaneous covariance.
irf.forecast <- get.irf.forecast(phi,20)
matirf.ortho <- get.irf.ortho(phi,sigma,20)

plot.irf(irf.ortho, c('seattle','seahawks'))
