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

whitelist = c('18/06/2010')
holes = c(2, 3, 4)

bin.names = c()
for (i in 1:(ncol(a)-11)) { bin.names = c(bin.names, i) }
colnames(a) = c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'Date', 'hour', bin.names)
a = a[a$hole %in% holes, ]
a = a[a$Date %in% whitelist, ]
a$Date = as.Date(a$Date, format='%d/%m/%Y')

a$activity = rowMeans(a[, c(12:ncol(a))])

means = ddply(.data=a, .variables=.(hole), .fun=m.mean, 12:ncol(a))
sems = ddply(.data=a, .variables=.(hole), .fun=std, 12:ncol(a))

d = data.frame(hole=holes, mmean=means$activity, sem=sems$activity)
d$hole = as.factor(d$hole)

myplot = ggplot(data=d, aes(x=hole, y=mmean)) +
         geom_errorbar(aes(ymin=mmean-sem, ymax=mmean+sem), width=0.25) +
         geom_bar(fill='white', color='black', position='dodge', stat='identity') +
         xlab('Hole') + ylab('Entry / second') +
         theme_bw()

print(myplot)
