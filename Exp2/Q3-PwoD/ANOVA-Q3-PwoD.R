(wd <- getwd())
x <- read.csv(paste0(wd,"/Exp2/Q3-PwoD/PersonWithoutDementia.csv"))
x
source(paste0(wd, "/anovakun_489.txt"))
x1 <- x[,c(2:7)]
x1

anovakun(x1, "sA", 6, geta=T, auto=T, tech=TRUE, peta = T, cilmd = T )

capture.output(result, file = paste0(wd,"/Exp2/Q3-PwoD/result.txt") )
