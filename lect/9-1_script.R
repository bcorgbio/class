
### make an atomic vector

x <- 1:10

### perform operations on a vector with a function to make new vectors

y <- x+runif(length(x))
xy.lm <- lm(y~x)
y.pred <- predict(xy.lm)

### see what a function does with ?



### plot vectors and see output
plot(x,y)
points(x,y.pred,type="l",col="red")


### Install a package
install.packages("ggplot2")

### load library 
library(ggplot2)


### plot a different way after making a data frame
df <- data.frame(x=x,y=y)
ggplot(data=df,aes(x,y))+geom_point()+geom_smooth(method="lm")

### for loop 



