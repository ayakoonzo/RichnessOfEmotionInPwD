(wd <- getwd())
#x <- read.csv(paste0(wd, "PerceivedIntensity.csv"))
x <- read.csv(paste0(wd, "/PerceivedIntensity.csv"))
x
str(x)
source(paste0(wd,"/anovakun_489.txt"))
#x1 <- x[,c(2,7)]
x1 <- x[,c(2:7)]
x1
head(x1)
anovakun(x1, "sA", 6, geta=T, auto=T, tech=TRUE)
