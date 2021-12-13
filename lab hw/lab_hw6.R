library(survival)

##1
#datasets in the package
data(ovarian,package="survival")
force(ovarian)
View(ovarian)

fit<-survfit(Surv(ovarian$futime,ovarian$fustat)~ovarian$rx+ovarian$resid.ds)
summary(fit)

plot(fit,conf.int =F ,
     lty=c(1,2,3,4),lwd=c(2,2,2,2),
     main="Kaplan-Meier survival curves",
     xlab="Follow-up time(days)",
     ylab="Survival probability")
legend("bottomleft",c("rx=1 & resid.ds=1","rx=1 & resid.ds=2","rx=2 & resid.ds=1","rx=2 & resid.ds=2"),
       lty=c(1,2,3,4),lwd=c(2,2,2,2))


##2
survdiff(Surv(ovarian$futime,ovarian$fustat)~ovarian$rx+ovarian$resid.ds)

##3
fit3<-coxph(Surv(ovarian$futime,ovarian$fustat)~
              ovarian$rx+ovarian$age+ovarian$resid.ds+ovarian$ecog.ps,ties="breslow",data=ovarian)
summary(fit3)

##4
contrasts(factor(ovarian$rx))

##5
fit5<-coxph(Surv(futime,fustat)~
              rx+age+resid.ds+ecog.ps,ties="breslow",data=ovarian)
summary(fit5)
new <- with(ovarian,data.frame(rx=c(1,2),
                               age=rep(median(age),2),
                               resid.ds=c(2,2),
                               ecog.ps=rep(median(ecog.ps),2)))

cc<-survfit(fit5,newdata=new)
plot(cc,conf.int =F,main="Kaplan-Meier survival curves",xlab="Follow-up time (days)",
     ylab="Survival probability",lty=c(1,2),lwd=c(2,2))
legend("bottomleft",c("rx=1","rx=2"),lty=c(1,2),lwd=c(2,2))

##6
cox<-coxph(Surv(ovarian$futime,ovarian$fustat)~
             rx+age+resid.ds+ecog.ps,ties="breslow",data=ovarian)
summary(cox)
test.ph<-cox.zph(cox)
print(test.ph)

