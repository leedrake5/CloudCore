x <- c(1, 2, 3, 4, 5, 6, 7, 8)
y <- c(123, 75, 50, 30, 20, 10, 5, 2.5)

x.scale <- scales::rescale(x, to=c(0,1))
y.scale <- scales::rescale(y, to=c(0,1))

scale.frame <- data.frame(
x = c(1, 2, 3, 4, 5, 6, 7, 8),
y = c(123, 75, 50, 30, 20, 10, 5, 2.5)
,
scaleX = scales::rescale(x, to=c(0,1)),
scaleY = scales::rescale(y, to=c(0,1))
)

independent <- "x"
dependent <- "y"
dataframe <- scale.frame


scree_crunch <- function(dataframe, dependent, independent){

    simple.frame <- data.frame(
    newY = dataframe[,dependent]/max(dataframe[,dependent]),
    newX = dataframe[,independent]/max(dataframe[,independent]))

    sims <-data.frame(
        sims1 = seq(from=1, to=nrow(dataframe)-1, by=1),
        sims2 = seq(from=2, to=nrow(dataframe), by=1)
        )

    n <- seq(from=1, to=nrow(sims), by=1)

    lm.sims <- lapply(n, function(x) summary(lm(newY~newX, data=simple.frame[sims[,1][x]:sims[,2][x],])))


    slopes <- unlist(lapply(n, function(x) as.vector(lm.sims[[x]]["coefficients"][[1]][2])))

    #rsquared <- unlist(pbapply::pblapply(n, function(x) as.vector(lm.sims[[x]]["r.squared"])))

    greater.1 <- which(abs(slopes) > 1)

    greater.1[length(greater.1)]+1


}


test.frame <- data.frame(
newy = c(175, 100, 63, 5, 4, 2, 1),
newx = c(1, 2, 3, 4, 5, 6, 7)
)


screeCrunch(dataframe=test.frame, dependent="newy", independent="newx")

library(openxlsx)
proto.fish <- loadWorkbook(file="~/Dropbox/Documents/University of Utah/University of Utah Cores/XRF PDZ/Swap/RSP15BNetLight.xlsx")
just.fish <- readWorkbook(proto.fish, sheet=2)
colnames(just.fish)[1] <- "Spectrum"

smalls <- c("Al.K12", "Si.K12", "P.K12", "S.K12", "Cl.K12", "K.K12", "Ca.K12", "Ti.K12", "Mn.K12", "Fe.K12")


combos <- function(a.vector){
    
    so <- seq(from=2, to=length(a.vector), by=1)
    
    long <- lapply(so, function(x) combn(x=smalls, m=x))
    and <- lapply(long, function(x) plyr::alply(x, 2))
    thanks.for.all.the.fish <- do.call(list, unlist(and, recursive=FALSE))
    
    thanks.for.all.the.fish

}

thanks.for.all.the.fish <- combos(smalls)

list.of.frames <- lapply(thanks.for.all.the.fish, function(x) as.data.frame(just.fish[,x]))

optimal_k_chain <- function(a.data.frame){
    
    n <- if(nrow(a.data.frame)<30){
        nrow(a.data.frame)-5
    } else {
        30
    }
    
    xrf.pca.frame <- a.data.frame[complete.cases(a.data.frame),]
    
    wss <- (nrow(xrf.pca.frame)-1)*sum(apply(xrf.pca.frame,2,var))
    for (i in 2:n) wss[i] <- sum(kmeans(xrf.pca.frame,
    centers=i)$withinss)
    
    result <- data.frame(
    clustercount=seq(1, n, 1),
    wss=wss,
    percent=1-wss/max(wss))
    
    best.choice <- scree_crunch(dataframe=result, dependent="percent", independent="clustercount")
    
    result[best.choice,]

}




list.of.elbows <- pbapply::pblapply(list.of.frames, function(x) optimal_k_chain(x), cl=6L)
frame.of.elbows <- do.call("rbind", list.of.elbows)
result <- frame.of.elbows[which.max(frame.of.elbows$percent),]



