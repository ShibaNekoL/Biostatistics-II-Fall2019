##1
#build data.frame of smokers and nonsmokers
smokers <- data.frame(Age=c(40,50,60,70,80),Deaths=c(32,104,206,186,102),py=c(52407,43248,28612,12663,5317),smoke=c(1,1,1,1,1))
nonsmokers<-data.frame(Age=c(40,50,60,70,80),Deaths=c(2,12,28,28,31),py=c(18790,10673,5710,2585,1462),smoke=c(0,0,0,0,0))
#create new variable of unit person-year
smokers$Deaths_py<-smokers$Deaths/smokers$py*100000
nonsmokers$Deaths_py<-nonsmokers$Deaths/nonsmokers$py*100000
#plot the data point
plot(smokers$Age,smokers$Deaths_py,pch=1,xlim=c(40,85),ylim=c(0,4000),xlab="Age",ylab="Deaths per 100000 person-year")
points(nonsmokers$Age,nonsmokerss$Deaths_py,xlim=c(40,85),ylim=c(0,4000),pch=16)
legend("bottomright",legend=c("smokers","non-smokers"),pch=c(1,16))

##2
#fit the poisson model
countdata<-rbind(smokers,nonsmokers)
fit<-glm(Deaths~smoke+Age,offset=log(py),family=poisson,data=countdata)
summary(fit)

##3
#testing
md1<-glm(Deaths~smoke*Age,offset=log(py),family=poisson,data=countdata)
md2<-glm(Deaths~Age,offset=log(py),family=poisson,data=countdata)
anova(md2,md1,test="Chisq")
qchisq(0.95,df=2)

##4
#plot mean response
curve(predict(md1,data.frame(Age=x,smoke=1,py=100000),type="response"),lty=1,xlim=c(40,85),ylim=c(0,4000),xlab="Age",ylab="Mean response")
curve(predict(md1,data.frame(Age=x,smoke=0,py=100000),type="response"),lty=2,xlim=c(40,85),ylim=c(0,4000),xlab="Age",ylab="Mean response",add=T)
legend("bottomright",legend=c("smokers","non-smokers"),lty=c(1,2))
#plots overlay
plot(smokers$Age,smokers$Deaths_py,pch=1,xlim=c(40,85),ylim=c(0,4000),xlab="Age",ylab="Deaths per 100000 person-year")
points(nonsmokers$Age,nonsmokers$Deaths_py,xlim=c(40,85),ylim=c(0,4000),pch=16)
curve(predict(md1,data.frame(Age=x,smoke=1,py=100000),type="response"),lty=1,xlim=c(40,85),ylim=c(0,4000),xlab="Age",ylab="Mean response",add=T)
curve(predict(md1,data.frame(Age=x,smoke=0,py=100000),type="response"),lty=2,xlim=c(40,85),ylim=c(0,4000),xlab="Age",ylab="Mean response",add=T)
legend("bottomright",legend=c("smokers","non-smokers"),lty=c(1,2))
legend("topright",legend=c("smokers","non-smokers"),pch=c(1,16))