
library(tidyverse)
library(rio)
library(party)
library(ggplot2)
library(dplyr)
library(vcd)
library(webr)
library(plotrix)
library(MASS)
library(gginference)

data(cats)

# iris flowers datasets
HW <- cats
view(cats)
str(cats)
HW <- head(cats,144)
HW

#Convertion male and female to 0 and 1 values################################################
gen <- ifelse(HW$Sex == 'M', 1, 0)
gen

HWN <- data.frame(Hweight = HW$Hwt,Bweight = HW$Bwt, gen = gen)
HWN

#Simple regression ############################################################
plot(HWN$Bweight,HWN$Hweight,main= "BWeight vs Hweight",pch = 3,xlab = "BWeight", ylab = "HWeight", col = "Red")
abline(lm(Hweight~Bweight, data = HWN),col="grey")


sum<-lm(Hweight~Bweight+gen, data=HWN)
sum
summary(sum)
male<-subset(HWN,gen=="1",select=c("Hweight","Bweight","gen"))
male
female <- subset(HWN,gen=="0",select=c("Hweight","Bweight","gen"))
female



#common regression line########################################################################
regone <- plot(male$Bweight,male$Hweight,col="blue", main = "One Regression line",pch = 3, xlab = "Bweight", ylab = "Hweight")
points(female$Bweight,female$Hweight,col="red",pch = 2)
abline(lm(Hweight~Bweight, data = HWN),col="black")
legend("topleft",
       pch = c(3, 2),
       c("Male", "Female"),
       col = c("red", "blue"))

#Different regression lines
regtwo <- plot(male$Bweight,male$Hweight,col="blue",main="Two regression line",pch = 3)
points(female$Bweight,female$Hweight,col="red",pch = 2)
abline(a=-0.419,b=4.752,col="black", lw = 2)
abline(a=(-0.41-0.82),b=4.752,col="grey",lw = 2)
legend("topleft",
       pch = c(3, 2),
       c("Male", "Female"),
       col = c("BLue", "Red"))
legend("bottomright",
       lw = c(2, 2),
       c("Male regression", "Femaleregression"),
       col = c("black", "grey"))



plot(female$Bweight,female$Hweight,col="Red",main="Hweight vs Bweight - female",pch = 3)
abline(a=(-0.4149),b=4.752,col="blue")

plot(male$Bweight,male$Hweight,col="Blue",pch = 2,main="Hweight vs Bweight - male")
abline(a=(-0.41),b=4.752,col="red")




