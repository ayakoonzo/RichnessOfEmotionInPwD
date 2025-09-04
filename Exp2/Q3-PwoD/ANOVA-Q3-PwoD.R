(wd <- getwd())
x <- read.csv(paste0(wd,"/Exp2/Q3-PwoD/PersonWithoutDementia.csv"))
head(x)
source(paste0(wd, "/anovakun_489.txt"))
x1 <- x[,c(2:7)]
head(x1)

result <- anovakun(x1, "sA", 6, geta=T, auto=T, tech=TRUE, peta = T, cilmd = T )
print(result)
#capture.output(result, file = paste0(wd,"/Exp2/Q3-PwoD/result.txt") )
