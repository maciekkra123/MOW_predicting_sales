head(cars)
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist(speed)")
par(mfrow=c(1,2))
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'

#Tworzenie modelu regresji
linearMod <- lm(dist ~ speed, data=cars)
print(linearMod)
summary(linearMod)
