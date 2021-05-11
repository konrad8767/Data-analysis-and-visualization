library('BCA')
library('car')
library('RcmdrMisc')
library('sandwich')
library('relimp')
library('corrplot')
library('ggplot2')
library('tidyverse')

data(Eggs, package="BCA")

# Wizualna analiza wszystkich kombinacji zmiennych.

pairs(Eggs, panel = panel.smooth, main = "Eggs")

head(Eggs)

df<-as.data.frame(Eggs)


df <- df %>% mutate(df[,3] = recode(df[,3], "No" = "0", "Yes" = "1"))

head(df)
summary(df)


ts.plot(Eggs[,6], main="Egg prices", col="red")
ts.plot(Eggs[,7], main="Beef prices")
ts.plot(Eggs[,8], main="Pork prices")
ts.plot(Eggs[,9], main="Chicken prices")
ts.plot(Eggs[,10], main="Cereal prices")


attach(mtcars)
par(mfrow=c(5,1))
ts.plot(Eggs[,6], main="Egg prices", col="red")
ts.plot(Eggs[,7], main="Beef prices", col="blue")
ts.plot(Eggs[,8], main="Pork prices", col="green")
ts.plot(Eggs[,9], main="Chicken prices", col="magenta")
ts.plot(Eggs[,10], main="Cereal prices", col="black")



par(mfrow=c(1,1))
par(bg="white")
matplot(df[,6:10], type = "l", pch=1, col= c("red", "blue", "darkgreen", "magenta", "black")) #plot
legend("topleft", legend = c("Egg price","Beef price", "Pork price", "Chicken price", "Cereal price"), col= c("red", "blue", "darkgreen", "magenta", "black"), pch=1) 

df$First.Week <- as.numeric(df$First.Week)
df$Easter <- as.numeric(df$Easter)
df$Month <- as.numeric(df$Month)
df


corrplot(cor(df), order="AOE", method="color", addCoef.col = "darkgray", tl.col = "black",  tl.cex = 0.9)


df %>%
        ggplot(aes(Week, Egg.Pr, colour=Cases)) +
        geom_point(size = 3, alpha=0.5)+
        geom_smooth()


df %>%
        ggplot(aes(Week, Egg.Pr)) +
        geom_polygon(colour="deepskyblue4", fill="azure3")+
        geom_point(aes(size=Egg.Pr,
                       colour=Cases))
