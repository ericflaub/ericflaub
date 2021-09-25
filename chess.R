
work_dir <- "C:/Users/Eric Laub/Desktop"
setwd(work_dir)
data = read.table("chess.txt", header=T) 
## cleaning 
### removing draws
data1 <-subset(data, outcome!=2)
## var gen 
### rating diff
data1$diff <- data1$orating - data1$erating
###lags
data1$lag1 <- c(NA, head(data1$outcome, -1))
data1$lag2 <- c(NA, head(data1$lag1, -1))
data1$lag3 <- c(NA, head(data1$lag2, -1))
data1$lag4 <- c(NA, head(data1$lag3, -1))
### won last game?
data1$one <- ifelse(data1$lag1 > 0, 1, 0)
### won last two games?
data1$two <- ifelse(data1$lag1 + data1$lag2 > 1, 1, 0)
### won last three games?
data1$three <- ifelse(data1$lag1 + data1$lag2 + data1$lag3 > 2, 1, 0)
### won last four games?
data1$four <- ifelse(data1$lag1 + data1$lag2 + data1$lag3 + data1$lag4 > 3, 1, 0)
## visualize 

data1
## delete first four obs
data1 <- cbind(ID = 1:nrow(data1), data1)
data1
data1 <- data1[data1$ID > 4, ] 
data1 
## prelim reg - winning factors
m1 = lm(data1$outcome~data1$ecolor+data1$method+data1$diff)
summary(m1)
## one lag no controls
m2 = lm(data1$outcome~data1$one)
summary(m2)
## two lags no controls
m3 = lm(data1$outcome~data1$one+data1$two)
summary(m3)
## three lags no controls
m4 = lm(data1$outcome~data1$one+data1$two+data1$three)
summary(m4)
## four lags no controls
m5 = lm(data1$outcome~data1$one+data1$two+data1$three+data1$four)
summary(m5)
## one lag with controls
m6 = lm(data1$outcome~data1$one+data1$ecolor+data1$method+data1$diff)
summary(m6)
## two lag with controls
m7 = lm(data1$outcome~data1$one+data1$two+data1$ecolor+data1$method+data1$diff)
summary(m7)
## three lag with controls
m8 = lm(data1$outcome~data1$one+data1$two+data1$three+data1$ecolor+data1$method+data1$diff)
summary(m8)
## four lags with controls
m9 = lm(data1$outcome~data1$one+data1$two+data1$three+data1$four+data1$ecolor+data1$method+data1$diff)
summary(m9)







