data <-read.csv(file.choose())
data$X <- ifelse(data$WBC<median(data$WBC),0,1)
library(survival)
survfit<-survfit(Surv(data$Last.Observed.Time,data$Censoring.Status)~data$X)
summary(survfit,censor=TRUE)

data0<-subset(data,data$X==0)
data1<-subset(data,data$X==1)

plot(survfit,mark.time = F,conf.int = F,
     col=c("blue","red"),lty=c(1,2),
     main="KM curves",
     xlab="Follow-up time",
     ylab="survival probability"
)
legend("bottomright",c("X*=0","X*=1"),col=c("blue","red"),lty=c(1,2))


hw8<-read.csv(file.choose())
hw8$X<-ifelse(hw8$WBC<median(hw8$WBC),0,1)
b<-Surv(hw8$Remission.Time)
fit<-survfit(b~hw8$X)

plot(survfit(b~hw8$X),
     mark.time=TRUE,
     conf.int=FALSE,
     lty=c(4,5),col=c("green2","purple"))

par(new=T)
plot(survfit,mark.time = F,conf.int = F,
     col=c("blue","red"),lty=c(1,2),
     main="KM curves",
     xlab="Follow-up time",
     ylab="survival probability"
)
legend("bottomright",
       c("hw8 X*=0","hw8 X*=1","hw9 X*=0","hw9 X*=1"),
       col=c("green2","purple","blue","red"),
       lty=c(4,5,1,2))