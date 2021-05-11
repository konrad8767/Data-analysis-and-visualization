# Grafika tradycyjna

# Wykresy jednej zmiennej

data()
data(LifeCycleSavings)
d1<-LifeCycleSavings
head(d1)
summary(d1)


## Wykres 1

par(mfrow=c(2, 2), mar=c(4, 4, 1, 1))

plot(LifeCycleSavings$dpi, type="b")
plot(LifeCycleSavings$ddpi, type="h")
plot(LifeCycleSavings$sr, type="p")
plot(LifeCycleSavings$pop15, type="l")



## Wykres 2

library(plotrix)
png(file = "dpi_percentage.jpg")
lbl<-c("Srednia pop15","Srednia pop75")
piepercent<- round(100*x/sum(x), 1)

pie(x, labels = piepercent, main = "Srednia pop15 oraz pop75",col = rainbow(length(x)))
legend("topright", c("Srednia pop15","Srednia pop75"), cex = 0.8,
       fill = rainbow(length(x)))
dev.off()


## Wykres 3

plot(LifeCycleSavings$pop15,ylim=c(0, 50),type="p",col="red")
par(new=TRUE)
plot(LifeCycleSavings$pop75,ylim=c(0, 50),type="p",col="blue")


## Wykres 4

library(dplyr)
library(tibble)
library(cluster)
rownames(d1) <- c()
d1

df<-d1 %>% select(4)
df
data(df, package="rattle")
df1<-scale(df)
kmf<-kmeans(df1,3)
kmf$cluster
c1<-cbind(kmf$cluster)
c1
clusplot(d1, kmf$cluster, main="Pogrupowane pañstwa ze wzglêdu na dpi",labels=2, lines=0)
kmf$centers


# Wykres 5

pairs(d1[ , 2:3],
      col = "red",
      pch = 18,
      labels = c("pop15", "pop75"),
      main = "Wykresy rozrzutu pop15 oraz pop75") 


