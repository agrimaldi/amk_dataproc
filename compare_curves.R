options(width=230)
library(reshape2)
library(ggplot2)

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

dd = data.frame()

for (hole in holes) {
    means = ddply(a[a$hole == hole, ], .(week), .fun=mean)
    means = means[order(unique(means$week)), ]
    sems = ddply(a[a$hole == hole, ], .(week), .fun=sd)
    sems = sems[order(unique(sems$week)), ]
    dd = rbind(dd, means)
}

colnames(dd) = colnames(a)

dd = melt(dd, id.vars=c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'week', 'Date', 'hour'))

a.plot = ggplot(data=dd, aes(x=variable, y=value, color=factor(hole))) +
         geom_line(aes(group=interaction(week, hole))) +
         geom_point() +
         facet_wrap(~ week) +
         xlab('bla')

print(a.plot)
