library(ggplot2)
library(plyr)
library(tseries)
data.matrix(Projectmaster)
load(Projectmaster)
data(Projectmaster)
df<-Projectmaster
View(df)

#MODEL 1
ggplot(df, aes(x = SP500, y = XOM, color = XOM)) +
  geom_point()
MODEL1<-lm(XOM ~ SP500, df)
summary(MODEL1)
MODEL1$coefficients
MODEL1$residuals
MODEL1$fitted.values
hist(MODEL1$residuals)
jarque.bera.test(MODEL1$residuals)
ggplot(df, aes(x = SP500, y = XOM)) +
  geom_point() +
  geom_smooth(method = 'lm')
#############################################ERROR
df$Date<- as.factor(df$Date)
df$XOM<- as.factor(df$XOM)

mean_xom <- ddply(df, "Date", summarise, mean_xom=mean(df$XOM)
ggplot(df, aes(x = XOM, fill = Date)) +
  geom_histogram(aes(y=..density..), position = "identity") +
  geom_density(alpha = 1/2) +
  geom_vline(data = mean_xom, aes(xintercept=mean(df$XOM), col=factor(Date)), linetype="dashed", size=1, show.legend = FALSE)
rlang::last_error()
rlang::last_trace
############################################ 

#MODEL 2
ggplot(df, aes(x = SP500, y = CVX, color = CVX)) + 
  geom_point() +
  geom_smooth(method = 'lm')
MODEL2<-lm(CVX ~ SP500, df)
summary(MODEL2)
MODEL2$coefficients
MODEL2$residuals
MODEL2$fitted.values
hist(MODEL2$residuals)
jarque.bera.test(MODEL2$residuals)

#MODEL 3
ggplot(df, aes(x = SP500, y = KMI, color = KMI)) + 
  geom_point() +
  geom_smooth(method = 'lm')
MODEL3<-lm(KMI ~ SP500, df)
summary(MODEL3)
MODEL3$coefficients
MODEL3$residuals
MODEL3$fitted.values
hist(MODEL3$residuals)
jarque.bera.test(MODEL3$residuals)

#MODEL4
ggplot(df, aes(x = SP500, y = WMB, color = WMB)) + 
  geom_point() +
  geom_smooth(method = 'lm')
MODEL4<-lm(WMB ~ SP500, df)
summary(MODEL4)
MODEL4$coefficients
MODEL4$residuals
MODEL4$fitted.values
hist(MODEL4$residuals)
jarque.bera.test(MODEL4$residuals)

#MODEL 5
ggplot(df, aes(x = SP500, y = LNG, color = LNG)) + 
  geom_point() + 
  geom_smooth(method = 'lm')
MODEL5<-lm(LNG ~ SP500, df)
summary(MODEL5)
MODEL5$coefficients
MODEL5$residuals
MODEL5$fitted.values
hist(MODEL5$residuals)   
jarque.bera.test(MODEL5$residuals)

df$lnXOM<-log(df$XOM)
p<-.6
obs_count<-dim(df)[1]
training_size <- floor(p * obs_count)
training_size
set.seed(176)
train_ind <- sample(obs_count, size = training_size)
Training <- df[train_ind, ]
Testing <- df[-train_ind, ]
dim(Training)
dim(Testing)
plot(XOM ~ SP500, df, xlim=c(1.5,7), ylim=c(10,45))
plot(XOM ~ SP500, Training, xlim=c(1.5,7), ylim=c(10,45), col = 'blue')
plot(XOM ~ SP500, Testing, xlim=c(1.5,7), ylim=c(10,45), col = 'red', pch=3)
points(Training$XOM, Training$SP500, col='blue')
points(Testing$XOM, Testing$SP500, col='red', pch=3)
Projectmaster<-read.csv('https://raw.githubusercontent.com/Joshuatsmart/Project-/main/projectmaster.csv', header = TRUE)
View(Projectmaster)         
SP500AVGByXOM<-aggregate(SP500 ~ Date, Projectmaster, mean) #Average % change in SP500 daily (for weekdays) and sums SP500 % change across XOM 
SP500AVGByCVX<-aggregate(CVX ~ Date, Projectmaster, mean)

head(SP500AVGByXOM)
x= c("1/10/19", "1/11/19", "1/14/19", "1/15/19", "1/16/19", "1/17/19")
y= c(-0.0001, -0.0053, 0.0107, 0.0022, 0.0076, 0.0132)
barplot(y, mainlab= SP500AVGByXOM= "SP500", xlab= "Date", ylab= "Frequency", 
        names.arg= x)
barplot(y, main = "SP500 from 1/10/19 - 1/17/19", xlab = "Date", ylab = "SP500",
        names.arg = x)
library(ggplot2)
data(Projectmaster)
df<-Projectmaster
df$XOM2<-df$XOM^2
df$<-df$WMB^3

hist(Projectmaster$LNG)
