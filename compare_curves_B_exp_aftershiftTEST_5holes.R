options(width=230)
library(plyr)
library(reshape2)
library(grid)
library(ggplot2)

std = function(x, s) {
    x = x[, s]
    sd(x) / sqrt(length(x))
}

m.mean = function(x, s) {
    x = x[, s]
    mean(x)
}

a = read.table('./aftershiftB.txt', header=F, sep='\t')

#blacklist = c('21/06/2010')
whitelist = c('23/06/2010')
holes = c(1, 2, 3, 4, 5)
ctrl = c(5, 9, 11, 8, 10, 12)
exptl = c(1, 3, 7, 2, 4, 6)

bin.names = c()
for (i in 1:(ncol(a)-11)) { bin.names = c(bin.names, i) }
colnames(a) = c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'Date', 'hour', bin.names)
a = a[a$hole %in% holes, ]
a = a[a$Date %in% whitelist, ]
a$Date = as.Date(a$Date, format='%d/%m/%Y')
a$group = 'ctrl'
a[a$ratID %in% exptl, ]$group = 'exptl'
a = a[order(a$Date), ]
a = a[a$group == 'exptl', ]
a$group = NULL

means = ddply(a, .(Date, hole), .fun=m.mean, 13:ncol(a))
sems = ddply(a, .(Date, hole), .fun=std, 13:ncol(a))

means = melt(means, id.vars=c('hole', 'Date'), value.name='bin')
colnames(means) = c('hole', 'Date', 'bin', 'Mean')
sems = melt(sems, id.vars=c('hole', 'Date'), value.name='bin')
colnames(sems) = c('hole', 'Date', 'bin', 'SEM')

dd = merge(means, sems, by=1:3)
dd$Date = factor(dd$Date)
a.plot = ggplot(data=dd, aes(x=bin, y=Mean, linetype=factor(hole))) +
         geom_ribbon(
                     aes(ymin=Mean-SEM, ymax=Mean+SEM, group=interaction(Date, hole)),
                     alpha=0.2, size=0.2
                    ) +
         geom_line(aes(group=interaction(Date, hole)), size=0.8) +
         #geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), color='black', size=0.25) +
         scale_x_discrete(breaks=c(1, 15, 30, 45, 60), labels=c(1, 30, 60, 90, 120)) +
         scale_linetype_manual(breaks = 1:5, values = c('solid', 'dashed', 'dotted','dotdash', 'longdash'), labels = 1:5) +
         xlab('Time (sec)') + ylab('Mean Hole Entry') +
         ylim(0,1.5) +
         labs(linetype='Hole') +
         theme(
               panel.background=element_rect(fill='white'),
               panel.grid=element_blank(),
               axis.title=element_text(size=30),
               axis.title.x=element_text(vjust=-0.5),
               axis.title.y=element_text(vjust=0.3),
               axis.text=element_text(size=18),
               legend.title=element_text(size=32),
               legend.margin=unit(0.5,'cm'),
               legend.key = element_rect(fill="white"),
               legend.key.width=unit(3,'cm'),
               legend.key.height=unit(1,'cm'),
               legend.text=element_text(size=20)
         )
print(a.plot)
ggsave('learning-curve_B_exptl_aftershift_5holes.png', width=15, height=8, units="cm", scale=2)

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
