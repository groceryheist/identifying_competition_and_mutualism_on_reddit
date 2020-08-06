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
    phi.0s <- lapply(1:ndraws,function(c) diag(m))

    result <- Reduce(function(phi.0s,i)
        mapply(
            function(phi.draw, phi.0)
                Reduce(function(x,y) matrix(as.numeric(x),m) + matrix(as.numeric(y),m),
                       apply(phi.draw, 3, function(phi.i) array(as.list(phi.0 %*% matrix(as.numeric(phi.i),m)),dim=c(m,m))),
                       init=matrix(0,m,m)),
            phi,
            phi.0s,
            SIMPLIFY=FALSE),
        1:forecast.len,
        init = lapply(1:ndraws,function(c) phi.0))

    return(result)
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
