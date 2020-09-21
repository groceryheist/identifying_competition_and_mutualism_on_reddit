library(arrow)
library(ggplot2)
library(data.table)
library(vars)

source("RemembR/R/RemembeR.R")
source("model_interpretation.R")

r2 <- readRDS("remember.RDS")

draws <- load.draws("large_simulated_stanmod")
params <- extract.params(draws)
df <- data.table(read_feather("data/large_simulation.feather"))
df[['week']] <- df$x
df[['subreddit']] <- as.integer(gsub('y','',df$variable))
df[['N_authors']] <- df$y

y <- dcast(df, x~subreddit, value.var='N_authors')
y[['x']] <- NULL
m.ols = VAR(log1p(y),p=1,type='const')

pdf("plots/test.pdf")
ggplot(df, aes(x=x,y=y,group=subreddit)) + geom_line() + facet_wrap(.~subreddit,nrow=2)
dev.off()

# the forecast len is hardcoded as 40
total.len <- max(df$x)
forecast.len <- 40

fit.df <- df[x <= total.len - forecast.len]
forecast.df <- df[x > total.len - forecast.len]

forecast.plot.data <- get.forecast.plotdata(fit.df, forecast.df, params)
remember(forecast.plot.data, 'large_simulation.forecast.data')
remember(fit.df, 'large_simulation.fit.df')
remember(forecast.df, 'large_simulation.forecast.df')
plot.forecast(forecast.plot.data,fit.df,forecast.df)

phi <- params[['phi']]
mu <- params[['mu']]
sigma <- params[['sigma']]

included.subreddits <- unique(df$subreddit)

irf.forecast <- get.irf.forecast(phi, mu, forecast.len)
irf.forecast.plotdata <- irf.to.plotdata(irf.forecast, included.subreddits)
remember(irf.forecast.plotdata, 'large_simulation.irf.forecast.data')

forecast.edgelist.95 <- get.community.edgelists(irf.forecast.plotdata, level='95')
remember(forecast.edgelist.95, 'large_simulation.forecast.edgelist.95')

forecast.edgelist.90 <- get.community.edgelists(irf.forecast.plotdata, level='90')
remember(forecast.edgelist.90, 'large_simulation.forecast.edgelist.90')

forecast.edgelist.80 <- get.community.edgelists(irf.forecast.plotdata, level='80')
remember(forecast.edgelist.80, 'large_simulation.forecast.edgelist.80')

irf.ortho <- get.irf.ortho(phi,mu,sigma,forecast.len)
irf.ortho.plotdata <- irf.to.plotdata(irf.forecast, included.subreddits)
remember(irf.ortho.plotdata, 'large_simulation.irf.ortho.data')

ortho.edgelist.95 <- get.community.edgelists(irf.ortho.plotdata, level='95')
remember(ortho.edgelist.95, 'large_simulation.ortho.edgelist.95')

ortho.edgelist.90 <- get.community.edgelists(irf.ortho.plotdata, level='90')
remember(ortho.edgelist.90, 'large_simulation.ortho.edgelist.90')

ortho.edgelist.80 <- get.community.edgelists(irf.ortho.plotdata, level='80')
remember(ortho.edgelist.80, 'large_simulation.ortho.edgelist.80')

phi.stats.80 <- get.phi.stats(draws,level='80')
remember(phi.stats.80,'large_simulation.phi.stats.80')

phi.stats.90 <- get.phi.stats(draws,level='90')
remember(phi.stats.90,'large_simulation.phi.stats.90')

phi.stats.95 <- get.phi.stats(draws,level='95')
remember(phi.stats.95,'large_simulation.phi.stats.95')
