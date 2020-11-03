install.packages("readxl")
library("readxl")
library("tseries")

mydata19<- read_excel("C://Users/preinstalled/Desktop/DSlab/Progetto/Anno_2019.xlsx",sheet="Prezzi-Prices")
mydata18<- read_excel("C://Users/preinstalled/Desktop/DSlab/Progetto/Anno_2018.xlsx",sheet = "Prezzi-Prices")
mydata17<- read_excel("C://Users/preinstalled/Desktop/DSlab/Progetto/Anno_2017.xlsx",sheet = "Prezzi-Prices")
mydata16<-read_excel("C://Users/preinstalled/Desktop/DSlab/Progetto/Anno_2016.xlsx",sheet = "Prezzi-Prices")
dummies<-read.csv("C://Users/preinstalled/Desktop/DSlab/Progetto/dummies.csv")
dummiestrain=dummies[1:1096,]
dummiestest=dummies[1097:1116,]


ts16=mydata16[,c(1,2,3)]
ts17=mydata17[,c(1,2,3)]
ts18=mydata18[,c(1,2,3)]
ts19=mydata19[,c(1,2,3)]
concat1=rbind(ts17,ts18)
concat2=rbind(ts16,concat1)
dataset=rbind(concat2,ts19)
colnames(dataset)=c("Data","Ora","PUN")
dataset$Data=as.Date(as.character(dataset$Data),format="%Y%m%d")
prezzo16=subset(dataset,Ora==19)
plot(prezzo16$Data,prezzo16$PUN,type="l",)
acf(prezzo16$PUN)
par(mfrow=c(1,1))
library(forecast)
serie=msts(prezzo16$PUN,start=c(2016),seasonal.periods =c(7,365.25))
ts.plot(serie)
correlogrammi(serie)

###############################TBATS STIMATO CON E SENZA OUTLIERS
training <- prezzo16[,3][1:1096,]
test <- prezzo16[,3][1097:1116,]
traints=msts(training,start=c(2016),seasonal.periods =c(7,365.33))
testts=ts(test)
m2=tbats(traints)
m2
autoplot(traints, series="Training data") +
  autolayer(fitted(m2),
            series="fitted values")
plot(forecast(m2,h=length(testts)))
components <- tbats.components(m2)
plot(components)
checkresiduals(m2)
out=tsoutliers(serie)
out
data=prezzo16
data[,3][c(out$index),]=out$replacements
sosout=msts(data[,3],start=c(2016),seasonal.periods =c(7,365.25))
plot(sosout)
tno <- data[1:1096,]
tesno <- data[1097:1116,]
trainnoout=msts(tno[,3],seasonal.periods =c(7,365.33),start=c(2016),end=c(2019))
testnoout=ts(tesno[,3],frequency=365,start=c(2019))
plot(testnoout)
plot(trainnoout)
m3=tbats(trainnoout)
m4=tbats(trainnoout,xreg=dummiestrain$dummies)
m4
plot(forecast(m4,h=365))

checkresiduals(m4)
shapiro.test(residuals(m3))
shapiro.test(residuals(m2))
correlogrammi(residuals(m2))
correlogrammi(residuals(m4))
##################################################################


###################################################ARIMA CON SINUSOIDI STIMATO CON E SENZA OUTLIERS
#train2=ts(tno[,3])
#test2=tesno[,3]
train2=trainnoout
test2=testnoout

fit0 <- auto.arima(train2)
(bestfit <- list(aicc=fit0$aicc, i=0, j=0, fit=fit0))
fc0 <- forecast(fit0, h=length(test2))
plot(fc0)

for(i in 1:3) {
  for (j in 1:10){
    z1 <- fourier(ts(train2, frequency=7), K=i)
    z2 <- fourier(ts(train2, frequency=365.33), K=j)
    fit <- auto.arima(train2, xreg=cbind(z1, z2,dummiestrain$dummies), seasonal=F)
    if(fit$aicc < bestfit$aicc) {
      bestfit <- list(aicc=fit$aicc, i=i, j=j, fit=fit)
    }
  }
}
bestfit

autoplot(train2, series="Training data") +
  autolayer(fitted(fit),
            series="fitted values")

fc <- forecast(bestfit$fit, 
               xreg=cbind(
                 fourier(ts(train2, frequency=7), K=bestfit$i, h=length(test2)),
                 fourier(ts(train2, frequency=365.33), K=bestfit$j, h=length(test2)),dummiestrain$dummies))

par(mfrow=c(1,2))
plot(fc)
lines(testnoout,col="coral2")
checkresiduals(bestfit$fit)
mean(residuals(fit))
ftb=forecast(train2,model=m3,h=length(test2))
plot(ftb)

a1=accuracy(ftb,testnoout)
a2=accuracy(fc,testnoout)


####-GIO- parte da mettere nella presentazione
#valori fittati training test TBATS senza outliers
autoplot(trainnoout, series="Training data") +
  autolayer(fitted(m3),
            series="fitted values")


#forecast TBATS 365 giorni avanti con test set plottato
m3 %>%
  forecast(h=length(testnoout))%>%
  autoplot()+autolayer(testnoout)

#valori fittati training test arima con sinusoidi senza outliers
autoplot(train2, series="Training data") +
  autolayer(fitted(fit),
            series="fitted values")


checkresiduals(residuals(m3))
#forecast ARIMA 365 giorni in avanti con test set plottato
par(mfrow=c(1,1))
plot(fc)
lines(testnoout,col="coral2")

#i due dataframe coi valori di accuracy
as.data.frame.matrix(a1) 
as.data.frame.matrix(a2) 

#analisi dei residui
correlogrammi(residuals(m3))
correlogrammi(residuals(fit))

quan19<- read_excel("C://Users/preinstalled/Desktop/DSlab/Progetto/Anno_2019.xlsx",sheet="Vendite-Sales")
quan18<- read_excel("C://Users/preinstalled/Dsktop/DSlab/Progetto/Anno_2018.xlsx",sheet = "Vendite-Sales")
quan17<- read_excel("C://Users/preinstalled/Desktop/DSlab/Progetto/Anno_2017.xlsx",sheet = "Vendite-Sales")
quan16<-read_excel("C://Users/preinstalled/Desktop/DSlab/Progetto/Anno_2016.xlsx",sheet = "Vendite-Sales")

par(mfrow=c(2,2))
qs16=quan16[,c(1,2,3)]
qs17=quan17[,c(1,2,3)]
qs18=quan18[,c(1,2,3)]
qs19=quan19[,c(1,2,3)]
unq1=rbind(qs17,qs18)
unq2=rbind(qs16,unq1)
datasetq=rbind(unq2,qs19)
colnames(datasetq)=c("Data","Ora","Quantità")
primoq=datasetq[c(1:168),]
a=aggregate(primoq[, 3], list(primoq$Ora), mean)
plot(a$Quantità,type='l')
secondoq=datasetq[c(8785:8952),]
b=aggregate(secondoq[, 3], list(secondoq$Ora), mean)
plot(b$Quantità,type='l')
a
b
terzoq=datasetq[c(17545:17712),]
c=aggregate(terzoq[, 3], list(terzoq$Ora), mean)
plot(c$Quantità,type='l')
quartoq=datasetq[c(26305:26472),]
d=aggregate(quartoq[, 3], list(quartoq$Ora), mean)
plot(d$Quantità,type='l')
