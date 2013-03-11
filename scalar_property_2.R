options(width=250)
library(plyr)
library(reshape2)
library(ggplot2)
library(zoo)
library(xts)

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

format.data = function(rawdata, holes, dates) {
    bin.names = 1:(ncol(rawdata)-11)
    colnames(rawdata) = c('context', 'hole', 'env', 'user', 'prog', 'session',
                    'cage', 'pass', 'ratID', 'Date', 'hour', bin.names)
    rawdata = rawdata[rawdata$hole %in% holes, ]
    rawdata = rawdata[rawdata$Date %in% dates, ]
    rawdata$Date = as.Date(rawdata$Date, format='%d/%m/%Y')
    rawdata = rawdata[order(rawdata$Date), ]
}

scale.x = function(rdata, step, wdw) {
    bins = rdata[, 12:ncol(rdata)]
    bins = t(rollapply(t(bins), width=wdw, by=step, FUN=mean, partial=TRUE, by.column=TRUE))
    rdata = cbind(rdata[, 1:11], bins)
}

translate.x = function(rdata) {
    print(rdata)
}

y.norm = function(rdata, method=mean) {
    rdata$bin = rdata$bin / method(rdata$bin)
    return(rdata)
}


whitelist = c('14/06/2010', '15/06/2010', '16/06/2010', '17/06/2010', '18/06/2010')
aholes = c(2)
bholes = c(4)

# context A
a = read.table('./All_Data_A.txt', header=F, sep='\t')
a = format.data(a, aholes, whitelist)
a = scale.x(a, step=1, wdw=2)
#a = translate.x(a)
a = ddply(a, .(hole), .fun=m.mean, 12:ncol(a))
a = melt(a, id.vars='hole', value.name='bin')
a = a[1:30, ]
a = y.norm(a)
colnames(a) = c('hole', 'bin', 'Mean')

#context B
b = read.table('./All_Data_B.txt', header=F, sep='\t')
b = format.data(b, bholes, whitelist)
b = scale.x(b, step=2, wdw=3)
b = ddply(b, .(hole), .fun=m.mean, 12:ncol(b))
b = melt(b, id.vars='hole', value.name='bin')
b = y.norm(b)
colnames(b) = c('hole', 'bin', 'Mean')

# ETA squared
eta.sq = eta.square(a$Mean, b$Mean)


mydata = rbind(a, b)
colnames(mydata) = c('hole', 'bin', 'Mean')
mydata$hole = factor(mydata$hole)

myplot = ggplot(data=mydata, aes(x=bin, y=Mean)) +
    geom_line(aes(group=hole, linetype=hole)) +
    geom_text(aes(label=paste('eta2 = ', round(eta.sq, 3), sep=''), x=2, y=1.5, hjust=0))

print(myplot)
print(eta.sq)
