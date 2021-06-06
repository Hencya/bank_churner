library(corrplot)
library(plyr)
#Memasukan data yang berasal dari csv

Data <- read.csv("BankChurners.csv")
str(Data)
#Total row dari data mentah 
nrow(Data)
#Total col dari data mentah
ncol(Data)

#Gunakan apply untuk memeriksa nomor jika ada nilai yang hilang di setiap kolom. 
#Mari kita hapus semua baris dengan nilai yang hilang.
sapply(Data, function(x) sum(is.na(x)))
clearData <- Data[complete.cases(Data),]

#Total row dari hasil wrangler data
nrow(clearData)

#Total col dari hasil wrangler data
ncol(clearData)

#Menampilkan 4 data teratas dari data yang sudah di wrangler
head(clearData,4)
str(clearData)

CreditCard <- subset(clearData, select=c(9))
barplot(table(CreditCard),
        main="Kredit Card",
        xlab="Card Colour",
        ylab="Count",
        border="red",
        col="blue",
        density=10126
)

Member <- subset(clearData, select=c(2))
barplot(table(Member),
        main="Member Category",
        xlab="Card Colour",
        ylab="Count",
        border="red",
        col="blue",
        density=10126
)

Educational <- subset(clearData, select=c(6))
barplot(table(Educational),
        main="Educational Level",
        xlab="Card Colour",
        ylab="Count",
        border="red",
        col="blue",
        density=10126
)

Marital <- subset(clearData, select=c(7))
barplot(table(Marital),
              main="Marital Status",
              xlab="Card Colour",
              ylab="Count",
              border="red",
              col="blue",
              density=10126
)

Income <- subset(clearData, select=c(8))
barplot(table(Income),
        main="Income Category",
        xlab="Card Colour",
        ylab="Count",
        border="red",
        col="blue",
        density=10126
)

clearData$Card_Category <- as.numeric(mapvalues(clearData$Card_Category,from=c("Blue","Silver","Gold","Platinum"),to=c(1,2,3,4)))
clearData$Income_Category <- as.numeric(mapvalues(clearData$Income_Category,from=c("Less than $40K","$40K - $60K","$60K - $80K","$80K - $120K","$120K +","Unknown"),to=c(40,60,80,120,140,0)))
clearData$Gender <- as.numeric(mapvalues(clearData$Gender,from=c("M","F"),to=c(1,2)))
clearData$Marital_Status <- as.numeric(mapvalues(clearData$Marital_Status,from=c("Single","Married","Divorced","Unknown"),to=c(1,2,3,0)))
clearData$Attrition_Flag <- as.numeric(mapvalues(clearData$Attrition_Flag,from=c("Attrited Customer","Existing Customer"),to=c(1,2)))
clearData$Education_Level <- as.numeric(mapvalues(clearData$Education_Level,from=c("College","Doctorate","Graduate","High School","Post-Graduate","Uneducated","Unknown"),to=c(1,2,3,4,5,6,0)))

clearData$CLIENTNUM <- NULL
clearData$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2 <- NULL
clearData$Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1 <- NULL

par(mfrow=c(1,1))
heatmap(cor(clearData[, sapply(clearData, is.numeric)]))

corrplot(cor(clearData),method="number")

x <- clearData$Total_Trans_Ct 
y <- clearData$Total_Trans_Amt
plot(x, y, main = "HUBUNGAN ANTARA CREDIT LIMIT DAN AVG OPEN TO BUY",
     xlab = "Total_Trans_Ct", ylab = "Total_Trans_Amt",
     pch = 19, frame = FALSE)
lines(lowess(x, y), col = "blue")

x1 <- clearData$Gender
y1 <- clearData$Income_Category
plot(x1, y1, main = "HUBUNGAN ANTARA Gender DAN Incomer Category",
     xlab = "Gender", ylab = "Income_Category",
     pch = 19, frame = FALSE)
lines(lowess(x1, y1), col = "red")


par(mfrow=c(2,2))


boxplot(x)
boxplot(y)

boxplot(x1)
boxplot(y1)

h<-hist(x, breaks=10, col="red", xlab="Total_Trans_Ct",main="Density Gram Total_Trans_Ct")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

h<-hist(y, breaks=10, col="red", xlab="Total_Trans_Ct",main="Density Gram Total_Trans_Amt")
xfit<-seq(min(y),max(y),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(y))
yfit <- yfit*diff(h$mids[1:2])*length(y)
lines(xfit, yfit, col="blue", lwd=2)

h<-hist(x1, breaks=10, col="blue", xlab="Gender",main="Density Gram Gender")
xfit<-seq(min(x1),max(x1),length=40)
yfit<-dnorm(xfit,mean=mean(x1),sd=sd(x1))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="red", lwd=2)

h<-hist(y1, breaks=10, col="blue", xlab="Income_Category",main="Density Gram Income_Category")
xfit<-seq(min(y1),max(y1),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(y))
yfit <- yfit*diff(h$mids[1:2])*length(y)
lines(xfit, yfit, col="red", lwd=2)




        