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


screeCrunch <- function(dataframe, dependent, independent){

simple.frame <- data.frame(
newY = dataframe[,dependent]/max(dataframe[,dependent]),
newX = dataframe[,independent]/max(dataframe[,independent]))

sims <-data.frame( 
sims1 = seq(from=1, to=nrow(dataframe)-1, by=1),
sims2 = seq(from=2, to=nrow(dataframe), by=1)
)

n <- seq(from=1, to=nrow(sims), by=1)

lm.sims <- pbapply::pblapply(n, function(x) summary(lm(newY~newX, data=simple.frame[sims[,1][x]:sims[,2][x],])))


slopes <- unlist(pbapply::pblapply(n, function(x) as.vector(lm.sims[[x]]["coefficients"][[1]][2])))

#rsquared <- unlist(pbapply::pblapply(n, function(x) as.vector(lm.sims[[x]]["r.squared"])))

greater.1 <- which(abs(slopes) > 1)

greater.1[length(greater.1)]


}


test.frame <- data.frame(
newy = c(175, 100, 63, 5, 4, 2, 1),
newx = c(1, 2, 3, 4, 5, 6, 7)
)


screeCrunch(dataframe=test.frame, dependent="newy", independent="newx")




wellsee <- function(x,y){
	
		x.scale <- scales::rescale(x, to=c(0,1))
	y.scale <- scales::rescale(y, to=c(0,1))
	
	for(i in 3:length(x)){
		
		
		
		
	}
	
	
	
}




