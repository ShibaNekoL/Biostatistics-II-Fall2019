lbw<-read.csv(file.choose())
View(lbw)

#2
lbw$race.w<-ifelse(lbw$race=="1",1,0)
lbw$race.b<-ifelse(lbw$race=="2",1,0)
md1<-glm(low~lwt*race.w+lwt*race.b,family=binomial,data=lbw)
summary(md1)
md1$coefficients[2]+md1$coefficients[5]
md1$coefficients[2]+md1$coefficients[6]

#3
md0<-glm(low~1,family=binomial,data=lbw)
anova(md0,md1,test="Chisq")


#4
plot(lbw$lwt,lbw$low,main="lwt vs low\n(by race)",xlab="lwt",ylab="low")
attach(lbw)
curve(predict(md1,data.frame(lwt=x,race.w=1,race.b=0),type="response"),add=T,col="red",lty=1,lwd=2)
curve(predict(md1,data.frame(lwt=x,race.w=0,race.b=1),type="response"),add=T,col="dodgerblue2",lty=5,lwd=2)
curve(predict(md1,data.frame(lwt=x,race.w=0,race.b=0),type="response"),add=T,col="green4",lty=6,lwd=2)
legend("topright",c("white", "black", "other"),col=c("red","dodgerblue2","green4"),lty=c(1,5,6))
detach(lbw)