url = "http://users.stat.ufl.edu/~winner/data/pgalpga2008.dat"
dat <- read.table(url,header=FALSE)
colnames(dat) <- c("averDis","accuracy","FM")
datF <- subset(dat, FM==1, select=1:2)
datM <- subset(dat, FM==2, select=1:2)

female.lm <- lm(datF$accuracy ~ datF$averDis)
summary(female.lm)

x2 <- c()
for(i in dat$FM){
        if(as.numeric(i) == 1){
             dat$FM <- 0
        }
        else  dat$FM <- 1
}
