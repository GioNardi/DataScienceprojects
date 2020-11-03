library(lasso2)
data(Prostate)
dati=Prostate
attach(dati)

library(funModeling)
tesi1=data.frame(df_status(dati))
#pre-processing, ricodifico le varibile age e svi
dati$svi=as.factor(svi)
dati$levelage[dati$age<=62]="young"
dati$levelage[dati$age>62]="old"
dati$levelage=as.factor(dati$levelage)
df_status(dati)
#elimino la variabile age
dati=dati[,-c(3)]

#analisi descrittive
attach(dati)
summary(lcavol)
h<-hist(lcavol,breaks=15)
xhist<-c(min(h$breaks),h$breaks)
yhist<-c(0,h$density,0)
xfit<-seq(min(lcavol),max(lcavol),length=40)
yfit<-dnorm(xfit,mean=mean(lcavol),sd=sd(lcavol))
plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)),
     main="Histogram of lcavol")
lines(xfit,yfit,col="red")

ad.test(lcavol)
par(mfrow=c(1,1))
counts=table(dati$svi,dati$levelage)
barplot(counts,legend=rownames(counts),col=c("darkblue","red"),main="Distribution by levelage and svi")
datanum=dati[,-c(1,4,9)]
df_status(datanum)
par(mfrow=c(2,4))
v=colnames(datanum)
for (i in 1:7){
  boxplot(datanum[,i],main=v[i])
}
library(car)
scatterplot(lcavol~lpsa|svi,main="lcavol vs. lpsa by svi")
col1<-c("red", "darkblue")

par(mfrow=c(1,1))
plot(dati$lcavol,
     main="lcavol by age", ylab="lcavol",
     col = col1[dati$levelage])
legend("topleft", title="age", cex = 0.8,
       legend=levels(dati$levelage), fill = col1)
abline(h=2)
#analisi della correlazione 
C=cor(datanum)
library(corrplot)
corrplot(C,method="number")



#.....................
#ridge regression
x=model.matrix(lcavol~.,dati)[,-1]
y=dati$lcavol
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
set.seed(123)
train=sample(1:nrow(dati),nrow(dati)*0.75)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
#visualizzazione dei coefficienti per diversi valori di lambda
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
tesi2=as.data.frame(cbind(coef(ridge.mod)[,50],coef(ridge.mod)[,60]))
colnames(tesi2)=c("lambda=11498","lambda=705")
set.seed(123)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
#MSE modello Ridge
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
ridge=mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
ridge.coef=predict(out,type="coefficients",s=bestlam,dati)
ridge.coef
plot(ridge.mod)



#lasso
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(123)
cv.out=cv.glmnet(x[train,],y[train],alpha=1,nfolds=10)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
#Mse modello Lasso
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
lasso=mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef
cbind((ridge.coef),(lasso.coef),summary(mod)$coefficients)



#modello lineare
training=dati[-c(1,7,16,18,22,29,32,33,34,40,54,55,59,61,62,63,64,67,68,71,72,73,92,93,97),]
testing=dati[c(1,7,16,18,22,29,32,33,34,40,54,55,59,61,62,63,64,67,68,71,72,73,92,93,97),]
mod=lm(lcavol~lweight+lbph+svi+lcp+gleason+pgg45+lpsa+levelage,data=training)
tesi4=as.data.frame(summary(mod)$coefficients)
summary(mod)
library(car)
tesi3=as.data.frame(vif(mod))
#MSE modello lineare 
mod.pred=predict(mod,testing)
modlin=mean((mod.pred-y.test)^2)
#stepwise selection del modello lineare
library(MASS)
mod2=stepAIC(mod)
summary(mod2)
tesi6=as.data.frame(summary(mod2)$coefficients)
#MSE del modello dopo stepAIC
mod2.pred=predict(mod2,testing)
modstep=mean((mod2.pred-y.test)^2)


vett=c(lasso,modstep)
tesi6=round(data.frame(t(vett)),4)
colnames(tesi6)=c("mod.lasso","mod.stepAIC")
rownames(tesi6)=c("MSE")
