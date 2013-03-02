options(width=230)
library(plyr)
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

a = read.table('./ctxtA2.txt', header=F, sep='\t')
#a = read.table('./ctxtB.txt', header=F, sep='\t')

blacklist = c('21/06/2010')
holes = c(1, 2, 3, 4, 5)

bin.names = c()
for (i in 1:(ncol(a)-11)) { bin.names = c(bin.names, i) }
colnames(a) = c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'Date', 'hour', bin.names)
a = a[a$hole %in% holes, ]
a = a[!(a$Date %in% blacklist), ]
a$Date = as.Date(a$Date, format='%d/%m/%Y')
a = a[order(a$Date), ]
a$week = rep(1:8, each=nrow(a)/8)
a = a[, c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'week', 'Date', 'hour', bin.names)]

#bin.names = c()
#for (i in 1:(ncol(b)-11)) { bin.names = c(bin.names, i) }
#colnames(b) = c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'Date', 'hour', bin.names)
#b = b[b$hole %in% holes, ]
#b = b[!(b$Date %in% blacklist), ]
#b$Date = as.Date(b$Date, format='%d/%m/%Y')
#b = b[order(b$Date), ]
#b$week = rep(1:8, each=nrow(b)/8)
#b = b[, c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'week', 'Date', 'hour', bin.names)]

means = ddply(a, .(week, hole), .fun=m.mean, 13:ncol(a))
sems = ddply(a, .(week, hole), .fun=std, 13:ncol(a))

means = melt(means, id.vars=c('hole', 'week'), value.name='bin')
colnames(means) = c('hole', 'week', 'bin', 'Mean')
sems = melt(sems, id.vars=c('hole', 'week'), value.name='bin')
colnames(sems) = c('hole', 'week', 'bin', 'SEM')

dd = merge(means, sems, by=1:3)
a.plot = ggplot(data=dd, aes(x=bin, y=Mean, color=factor(hole))) +
         geom_ribbon(
                     aes(ymin=Mean-SEM, ymax=Mean+SEM, group=interaction(week, hole)),
                     color='black', fill='grey', alpha=0.5, size=0.2
                    ) +
         geom_line(aes(group=interaction(week, hole)), size=0.8) +
         #geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), color='black', size=0.25) +
         facet_wrap(~ week, nrow=4, ncol=2) +
         xlab('Time') + ylab('Mean Hole Entry') +
         labs(color='Hole') +
         theme(
               panel.background=element_rect(fill='white')
         )
print(a.plot)
#ggsave('learning-curve_A_manualMean.png', scale=1.5)

# Test with automatic smoothing
#dd = melt(a[, c('hole', 'week', bin.names)], id.vars=c('hole', 'week'))
#colnames(dd) = c('hole', 'week', 'bin', 'value')
#a.plot = ggplot(data=dd, aes(x=bin, y=value, color=factor(hole))) +
#         #geom_line(aes(group=interaction(week, hole)), size=0.8) +
#         #geom_point() +
#         geom_smooth(aes(group=interaction(week, hole)), level=0.99) +
#         #geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), color='black', size=0.25) +
#         facet_wrap(~ week, nrow=4, ncol=2) +
#         xlab('Time') + ylab('Mean Hole Entry') +
#         theme(panel.background=element_rect(fill='white'))

#ggsave('learning-curve_A_smooth-GMA.png', scale=1.5)
#print(a.plot)
