options(width=230)
library(reshape2)
library(ggplot2)
library(plyr)

std = function(x, s) {
    x = x[, s]
    sd(x) / sqrt(nrow(x))
}

m.mean = function(x, s) {
    x = x[, s]
    colMeans(x)
}

#a = read.table('./aftershiftA.txt', header=F, sep='\t')
a = read.table('./aftershiftB.txt', header=F, sep='\t')

#whitelist = c('23/06/2010')
holes = c(1, 2, 3, 4, 5)
ctrl = c(5, 9, 11, 8, 10, 12)
exptl = c(1, 3, 7, 2, 4, 6)

bin.names = c()
for (i in 1:(ncol(a)-11)) { bin.names = c(bin.names, i) }
colnames(a) = c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'Date', 'hour', bin.names)
a = a[a$hole %in% holes, ]
#a = a[a$Date %in% whitelist, ]
a$Date = as.Date(a$Date, format='%d/%m/%Y')
a$group = 'ctrl'
a[a$ratID %in% exptl, ]$group = 'exptl'
a = a[, c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'group', 'ratID', 'Date', 'hour', bin.names)]

a$activity = rowMeans(a[, c(13:ncol(a))])

dd = a[c('hole', 'activity', 'group', 'Date')]

pw.t.test = pairwise.t.test(dd$activity, dd$hole)

means = ddply(.data=a, .variables=.(hole, group, Date), .fun=m.mean, 13:ncol(a))
means = means[, c('hole', 'group', 'activity', 'Date')]
sems = ddply(.data=a, .variables=.(hole, group, Date), .fun=std, 13:ncol(a))
sems = sems[, c('hole', 'group', 'activity', 'Date')]

#d = data.frame(hole=holes, mmean=means$activity, sem=sems$activity)
#d$hole = as.factor(d$hole)

d = merge(means, sems, by=c('hole', 'group', 'Date'))
print(head(d))
colnames(d) = c('hole', 'group', 'Date', 'mean', 'sem')



myplot = ggplot(data=d, aes(x=Date, y=mean, fill=factor(group, labels=c('Control', 'Shifted')))) +
  geom_bar(color='black', position='dodge', stat='identity') +
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=0.25, position=position_dodge(width=0.9)) +
  facet_wrap(~ hole) +
  xlab('Hole') + ylab('Entry / second') +
  scale_fill_grey(start=0.9, end=0.3) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size=20),
    axis.title.x = element_text(vjust=-0.3),
    axis.title.y = element_text(vjust=0.2),
    axis.text = element_text(size=18),
    legend.title = element_blank(),
    legend.text = element_text(size=18)
  )

print(myplot)
