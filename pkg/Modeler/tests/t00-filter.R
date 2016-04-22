library(Modeler)
set.seed(246391)
data <- matrix(rnorm(1000*30), nrow=1000, ncol=30)

fm <- filterMean(1)
summary(fm(data))
summary(fm(as.data.frame(data)))

fd <- filterMedian(1)
summary(fd(data))

fs <- filterSD(1)
summary(fs(data))

fr <- filterRange(3)
summary(fr(data))

fx <- filterMax(2)
summary(fx(data))

fn <- filterMin(-1)
summary(fn(data))

lapply(list(fm, fd, fs, fr, fx, fn), function(f) summary(f(data)))

try(fm(1)) # fails
try(fs("A")) # fails

