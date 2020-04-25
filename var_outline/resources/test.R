library(data.table)
library(ggplot2)
theme_minimal()

x1 <- rnbinom(100, size=4, mu=40)
p <- qplot(x=x1,geom='histogram')
p <- p + ylim(0,quantile(x1,.10))
ggsave('random_dist.png',width=4,height=2,units='cm')
