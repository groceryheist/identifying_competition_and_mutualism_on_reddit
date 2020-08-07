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

    
    # function getting the next irf for a specific draw
    next.irf <- function(irf.draw,phi.draw){
        if(length(irf.draw) == 0){
            return(diag(m))
        }

        res <- matrix(0,m,m)
        if(length(irf.draw) < p){
            for(i in 1:length(irf.draw)){
                res <- res + matrix(as.numeric(irf.draw[[length(irf.draw) - i + 1]]),m) %*% matrix(as.numeric(phi.draw[,,i]),m)
            }
            for(i in (1+length(irf.draw)):p){
                res <- res + diag(m) %*% matrix(as.numeric(phi.draw[,,i]),m)
            }
        } else {
            for(i in 1:p){
                res <- res + matrix(as.numeric(irf.draw[[length(irf.draw) - i + 1]]),m) %*% matrix(as.numeric(phi.draw[,,i]),m)
            }
        }
        return(res)
    }

    irf <- list()
    for(t in 1:forecast.len){
        irf[[t]] <- list()
        for(id in 1:length(phi)){
            phi.draw <- phi[[id]]
            irf.draw <- list()
            if(t == 1){
                irf[[t]][[id]] <- next.irf(irf.draw, phi.draw)
            } else {
                for(i in 1:(t-1)){
                    irf.draw <- c(irf.draw,list(irf[[i]][[id]]))
                }

                irf[[t]][[id]] <- next.irf(irf.draw, phi.draw)
            }
        }
    }
    
    return(irf)
}

get.irf.ortho <- function(phi, sigma, forecast.len = 20){
    irf.forecast <- get.irf.forecast(phi, forecast.len)
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
    
    plot.data <- irf.to.plotdata(irf, c('seattle','seahawks'))
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
for(i in 1:length(phi)){
    for(k in 1:length(phi[[i]])){
        phi[[i]][[k]] = phi[[i]][[k]] 
    
    }
}

sigma <- params[['sigma']]

# the forecast irf doesn't account for simultaneous covariance.
irf.forecast <- get.irf.forecast(phi,4*6)
irf.ortho <- get.irf.ortho(phi,sigma,4*6)

plot.irf(irf.ortho, c('seattle','seahawks'))
