rm(list=ls()) #clears global environment
library(ggplot2) #loads GGPLOT2 library
library(plyr) #loads plyr library


Projectmaster<-read.csv('https://raw.githubusercontent.com/Joshuatsmart/Project-/main/projectmaster.csv')
View(Projectmaster)
head(Projectmaster)
head(Projectmaster,3)
tail(Projectmaster)

summary(Projectmaster)
str(Projectmaster)
dim(Projectmaster)

class(Projectmaster)
class(Projectmaster$Date)
class(Projectmaster$XOM)

hist(Projectmaster$XOM)
hist(Projectmaster$CVX)
hist(Projectmaster$KMI)
hist(Projectmaster$WMB)
hist(Projectmaster$SP500)

#CLEANING DATA METHOD 

Projectmaster_A<-subset(Projectmaster, XOM<=25 & CVX<50 & KMI<35 & WMB<50 & SP500<100)
dim(Projectmaster_A)
(dim(Projectmaster)[1]-dim(Projectmaster_A)[1])/dim(Projectmaster)[1]

View(Projectmaster_A)
Projectmaster_A$XOM<-NULL

#Below I will run the code for Deletion when necessary 
Projectmaster_B<-Projectmaster
Projectmaster_B$XOM[Projectmaster_B$XOM>21]<-NA
View(Projectmaster_B)
#Assign NA values for invalid responses bc R can't handle the computation because of NA 
mean(Projectmaster$XOM)
mean(Projectmaster$CVX)
mean(Projectmaster$KMI)
mean(Projectmaster$WMB)
mean(Projectmaster$LNG)
mean(Projectmaster$SP500)
sapply(Projectmaster, mean, na.rm=TRUE)
library(Hmisc)
describe(Projectmaster)
mean(Projectmaster_B$XOM, na.rm = TRUE) ##tells R to ignore the NA 

##Assigns NA to outlier responses 
Projectmaster_B$XOM[Projectmaster_B$XOM>90]<-NA
Projectmaster_B$CVX[Projectmaster_B$CVX>450]<-NA
Projectmaster_B$KMI[Projectmaster_B$KMI>60]<-NA
Projectmaster_B$WMB[Projectmaster_B$WMB>45]<-NA
Projectmaster_B$LNG[Projectmaster_B$LNG>35]<-NA
Projectmaster_B$SP500[Projectmaster_B$SP500>60]<-NA
dim(Projectmaster_B) #No data has been lost
 
dim(Projectmaster)

#ggplot 

install.packages('ggplot2')
library(MASS)
library(ggplot2)
ggp

fortify(Projectmaster)

??Projectmaster
plot(Projectmaster$WMB, Projectmaster$SP500, col=factor(Projectmaster$Date)
plot(Projectmaster$XOM, Projectmaster$SP500)

ggplot(
  Projectmaster,
  aes(XOM) +
    geom_histogram(aes(y = ..density..), position = "identity", binwidth = 10)
  hist(Projectmaster$XOM)
  data(Projectmaster)
  ggplot(SP500, aes(x = Date, y = XOM)
  )

geom_violin(Projectmaster)      
   
  
$cd    
$ cd ..  
  