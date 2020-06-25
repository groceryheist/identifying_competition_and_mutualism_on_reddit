library(data.table)
library(ggplot2)
theme_set(theme_minimal())

x1 <- rnbinom(100, size=4, mu=40)
p <- qplot(x=x1,geom='histogram')
p <- p + ylim(0,quantile(x1,.10))
ggsave('random_dist.png',width=4,height=2,units='cm')


N <- 120
lik1 <- c()
lik2 <- c()
for(n in 1:N){
  x1 <- rnbinom(200, size=100, mu=50)
  lik1<- append(lik1,sum(log(dnbinom(x1, size=100, mu=50) )))

  lik2 <- append(lik2, sum(log(dnbinom(x1,size=140,mu=60))))
}

(l1q <- quantile(lik1,c(0.025,0.5,0.975)))
(l2q <- quantile(lik2,c(0.025,0.5,0.975)))
df <- data.table(rbind(l1q,l2q))
names(df) <- c("ll",'m','hh')
df <- df[,'model':=c("model 2","model 1")]
p <- ggplot(df, aes(x=model,y=m,ymax=hh,ymin=ll)) + geom_pointrange() + coord_flip() + ylab("log likelihood")


ggsave('example_predlik.png',width=12,height=6,units='cm')
