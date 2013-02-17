options(width=230)
library(reshape2)
library(ggplot2)

std = function(x, s) {
    x = x[, s]
    sd(x) / sqrt(length(x))
}

m.mean = function(x, s) {
    x = x[, s]
    mean(x)
}

a = read.table('./ctxtA.tsv', header=F, sep='\t')
b = read.table('./ctxtB.tsv', header=F, sep='\t')

blacklist = c('21/06/2010')
holes = c(2, 3, 4)

bin.names = c()
for (i in 1:(ncol(a)-11)) { bin.names = c(bin.names, i) }
colnames(a) = c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'Date', 'hour', bin.names)
a = a[a$hole %in% holes, ]
a = a[!(a$Date %in% blacklist), ]
a$Date = as.Date(a$Date, format='%d/%m/%Y')
a = a[order(a$Date), ]
a$week = rep(1:8, each=nrow(a)/8)
a = a[, c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'week', 'Date', 'hour', bin.names)]

#dd = data.frame()
#ss = data.frame()

means = ddply(a, .(week, hole), .fun=m.mean, 13:ncol(a))
sems = ddply(a, .(week, hole), .fun=std, 13:ncol(a))

means = melt(means, id.vars=c('hole', 'week'), value.name='bin')
colnames(means) = c('hole', 'week', 'bin', 'Mean')
sems = melt(sems, id.vars=c('hole', 'week'), value.name='bin')
colnames(sems) = c('hole', 'week', 'bin', 'SEM')

dd = merge(means, sems, by=1:3)

a.plot = ggplot(data=dd, aes(x=bin, y=Mean, color=factor(hole))) +
         geom_line(aes(group=interaction(week, hole)), size=1) +
         #geom_point() +
         geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), color='black', size=0.25) +
         facet_wrap(~ week, nrow=4, ncol=2) +
         xlab('Time') + ylab('Mean Hole Entry') +
         theme(panel.background=element_rect(fill='white'))

print(a.plot)
