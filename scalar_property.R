options(width=230)
library(plyr)
library(reshape2)
library(ggplot2)
library(zoo)

std = function(x, s) {
    x = x[, s]
    sd(x) / sqrt(length(x))
}

m.mean = function(x, s) {
    x = x[, s]
    colMeans(x)
}

eta.square = function(ref, x) {
    overall.sum = sum((ref + x)^2)
    s.norm = sum(ref^2 + x^2)
    s.norm.sq = (sum(ref) + sum(x))^2 / (length(ref) + length(x))
    sst = overall.sum/2 - s.norm.sq
    sstot = s.norm - s.norm.sq
    eta.sq = sst / sstot
    return(eta.sq)
}

#Load files
a = read.table('./ctxtA2.txt', header=F, sep='\t')
b = read.table('./ctxtB.txt', header=F, sep='\t')

# List dates to keep
#whitelist = c('14/06/2010', '15/06/2010', '16/06/2010', '17/06/2010', '18/06/2010')
blacklist = c('21/06/2010')
# Holes to keep
aholes = c(2, 3)
bholes = c(3, 4)

# Format data for context A
bin.names = c()
for (i in 1:(ncol(a)-11)) { bin.names = c(bin.names, i) }
colnames(a) = c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'Date', 'hour', bin.names)
a = a[a$hole %in% aholes, ]
#a = a[(a$Date %in% whitelist), ]
a = a[!(a$Date %in% blacklist), ]
a$Date = as.Date(a$Date, format='%d/%m/%Y')
a = a[order(a$Date), ]
a$week = rep(1:8, each=nrow(a)/8)
a = a[, c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'week', 'Date', 'hour', bin.names)]

# Format data for context B
bin.names = c()
for (i in 1:(ncol(b)-11)) { bin.names = c(bin.names, i) }
colnames(b) = c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'Date', 'hour', bin.names)
b = b[b$hole %in% bholes, ]
#b = b[(b$Date %in% whitelist), ]
b = b[!(b$Date %in% blacklist), ]
b$Date = as.Date(b$Date, format='%d/%m/%Y')
b = b[order(b$Date), ]
b$week = rep(1:8, each=nrow(b)/8)
b = b[, c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'week', 'Date', 'hour', bin.names)]



# Mean for each hole
print(dim(a))
a.means = ddply(a, .(week, hole), .fun=m.mean, 13:ncol(a))
#print(head(a.means))
a.means = a.means[a.means$week==8, ]

# Smoothing
# width = window width
# by = step
a.hole3.scaled = rollapply(as.vector(unlist(c(a.means[1, 3:ncol(a.means)]))), width=1, by=1, FUN=mean, partial=T)
a.hole2.scaled = rollapply(as.vector(unlist(c(a.means[2, 3:ncol(a.means)]))), width=1, by=1, FUN=mean, partial=T)

# Y normalisation
a.hole3.scaled.ynorm = a.hole3.scaled / mean(a.hole3.scaled)
a.hole2.scaled.ynorm = a.hole2.scaled / mean(a.hole2.scaled)

# Take first 30 bins
a.hole3.scaled.ynorm = a.hole3.scaled.ynorm[1:30]
a.hole2.scaled.ynorm = a.hole2.scaled.ynorm[1:30]



# Mean for each hole
b.means = ddply(b, .(hole), .fun=m.mean, 13:ncol(b))

# Smoothing
b.hole3.scaled = rollapply(as.vector(unlist(c(b.means[1, 2:ncol(b.means)]))), width=3, by=2, FUN=mean, partial=T)
b.hole4.scaled = rollapply(as.vector(unlist(c(b.means[2, 2:ncol(b.means)]))), width=3, by=2, FUN=mean, partial=T)
#b.hole3.scaled = rollapply(as.vector(unlist(c(b.means[1, 2:ncol(b.means)]))), width=2, by=1, FUN=mean, partial=T)
#b.hole4.scaled = rollapply(as.vector(unlist(c(b.means[2, 2:ncol(b.means)]))), width=2, by=1, FUN=mean, partial=T)

# Y normalisation
b.hole3.scaled.ynorm = b.hole3.scaled / mean(b.hole3.scaled)
b.hole4.scaled.ynorm = b.hole4.scaled / mean(b.hole4.scaled)


# ETA squared
eta.sq = eta.square(b.hole3.scaled.ynorm, b.hole4.scaled.ynorm)


mydata = data.frame(a=a.hole2.scaled.ynorm, b=b.hole4.scaled.ynorm, bin=1:30)
mydata = melt(mydata, id=c("bin"))
myplot = ggplot(data=mydata, aes(x=bin, y=value, color=variable)) +
    geom_line(aes(group=variable))

print(myplot)
print(eta.sq)

#means.scaled = ddply(means, .(hole), .fun=rollapply, width=3, by=2, FUN=mean, partial=T)

#sems = ddply(a, .(week, hole), .fun=std, 13:ncol(a))

#means = melt(means, id.vars=c('hole', 'week'), value.name='bin')
#colnames(means) = c('hole', 'week', 'bin', 'Mean')
#sems = melt(sems, id.vars=c('hole', 'week'), value.name='bin')
#colnames(sems) = c('hole', 'week', 'bin', 'SEM')

#dd = merge(means, sems, by=1:3)
#a.plot = ggplot(data=dd, aes(x=bin, y=Mean, color=factor(hole))) +
         #geom_ribbon(
                     #aes(ymin=Mean-SEM, ymax=Mean+SEM, group=interaction(week, hole)),
                     #color='black', fill='grey', alpha=0.5, size=0.2
                    #) +
         #geom_line(aes(group=interaction(week, hole)), size=0.8) +
         ##geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), color='black', size=0.25) +
         #facet_wrap(~ week, nrow=4, ncol=2) +
         #xlab('Time') + ylab('Mean Hole Entry') +
         #labs(color='Hole') +
         #theme(
               #panel.background=element_rect(fill='white')
         #)
#ggsave('learning-curve_A_manualMean.png', scale=1.5)

## Test with automatic smoothing
#dd = melt(a[, c('hole', 'week', bin.names)], id.vars=c('hole', 'week'))
#colnames(dd) = c('hole', 'week', 'bin', 'value')
#a.plot = ggplot(data=dd, aes(x=bin, y=value, color=factor(hole))) +
         ##geom_line(aes(group=interaction(week, hole)), size=0.8) +
         ##geom_point() +
         #geom_smooth(aes(group=interaction(week, hole)), level=0.99) +
         ##geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), color='black', size=0.25) +
         #facet_wrap(~ week, nrow=4, ncol=2) +
         #xlab('Time') + ylab('Mean Hole Entry') +
         #theme(panel.background=element_rect(fill='white'))

#ggsave('learning-curve_A_smooth-GMA.png', scale=1.5)
#print(a.plot)
