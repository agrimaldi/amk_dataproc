options(width=230)
library(ggplot2)

a = read.table('./ctxtA.tsv', header=F, sep='\t')
b = read.table('./ctxtB.tsv', header=F, sep='\t')

bin.names = c()
for (i in 1:(ncol(a)-11)) { bin.names = c(bin.names, i) }
colnames(a) = c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'date', 'hour', paste('bin', bin.names, sep='.'))
a = a[order(as.Date(a$date, format="%d/%m/%Y")),]

bin.names = c()
for (i in 1:(ncol(b)-11)) { bin.names = c(bin.names, i) }
colnames(b) = c('context', 'hole', 'env', 'user', 'prog', 'session', 'cage', 'pass', 'ratID', 'date', 'hour', paste('bin', bin.names, sep='.'))
b = b[order(as.Date(b$date, format="%d/%m/%Y")),]

dd = data.frame()

for (hole in 2:4) {
    d = ddply(a[a$hole == hole, ], .(date), .fun=mean)
    d$date = unique(a$date)
    dd = rbind(dd, d)
}

colnames(dd) = colnames(a)

