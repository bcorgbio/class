
### make an atomic vector

x <- 1:10

### perform operations on a vector with a function to make new vectors

y <- x+runif(length(x))
xy_lm <- lm(y~x)
y_pred <- predict(xy_lm)

### see what a function does with ?
?predict


### plot vectors and see output
plot(x,y)
points(x,y_pred,type="l",col="red")


### Install a package
install.packages("ggplot2")

### load library 
library(ggplot2)


### plot a different way after making a data frame
df <- data.frame(x=x,y=y)

ggplot(data=df,aes(x,y))+
  geom_point()+
  geom_smooth(method="lm")

### for loop 

for(i in x) 
  print(i)

print(x[1])
print(x[2])

sapply(x,print)



