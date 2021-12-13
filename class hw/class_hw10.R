studata<-read.csv(file.choose())
library(survival)

##1
studata$WBC_new<-ifelse(studata$WBC<median(studata$WBC),"Low","High")
s<-Surv(studata$Last.Observed.Time,studata$Censoring.Status)

studata$WBC_factor<-factor(studata$WBC_new,levels=c("Low","High"),labels=c(0,1))
studata$Treatment<-as.factor(studata$Treatment)
contrasts(studata$Treatment)  
  
  
survdiff(s~studata$WBC_factor)

##2
survdiff(s~studata$Treatment+studata$WBC_factor)


##3
survdiff(s~studata$Treatment+strata(studata$WBC_factor))


##4

fit<-survfit(s~studata$Treatment+studata$WBC_factor)
plot(fit,mark.time=F,conf.int = F,
     main="survival curves",
     xlab="Follow-up time",
     ylab="Survival probability",
     col=c("red","blue","red3","blue4"),lty=c(1,2,3,4),
     lwd=c(3,3,3,3)
)
legend("bottomright", legend = c("Treatment=0, WBC-status=0", 
                                 "Treatment=0, WBC-status=1", 
                                 "Treatment=1, WBC-status=0", 
                                 "Treatment=1, WBC-status=1"), 
       lty = c(1, 2, 3, 4), col=c("red","blue","red3","blue4"),lwd =c(3,3,3,3))


## Cox regression
coxmodel.1<-coxph(s~Treatment+WBC_factor,ties="breslow",data=studata)
summary(coxmodel.1)
coxmodel.2<-coxph(s~Treatment*WBC_factor,ties="breslow",data=studata)
summary(coxmodel.2)

c1<-cox.zph(coxmodel.1)
print(c1)

anova(coxmodel.1,coxmodel.2,test="Chisq")


## meanresponse no int
coxfit<-survfit(coxmodel.1,newdata=data.frame(Treatment=c(0,0,1,1),WBC_factor=c(0,1,0,1)))
plot(coxfit,mark.time=F,conf.int = F,
     main="survival curves",
     xlab="Follow-up time",
     ylab="Survival probability",
     col=c(1,2,3,4),lty=c(1,2,3,4),
     lwd=c(3,3,3,3)
     )
legend("topright",c("Treatment=0,WBC-status=0","Treatment=0,WBC-status=1","Treatment=1,WBC-status=0","Treatment=1,WBC-status=1"),col=c(1,2,3,4),lty=c(1,2,3,4))

## meanresponse int
coxfit2<-survfit(coxmodel.2,newdata=data.frame(Treatment=c(0,0,1,1),WBC_factor=c(0,1,0,1)))
plot(coxfit2,mark.time=F,conf.int = F,
     main="survival curves",
     xlab="Follow-up time",
     ylab="Survival probability",
     col=c(1,2,3,4),lty=c(1,2,3,4),
     xlim=c(0,35),ylim=c(0,1),
     lwd=c(2,2,2,2)
)
legend("topright",c("Treatment=0,WBC-status=0","Treatment=0,WBC-status=1","Treatment=1,WBC-status=0","Treatment=1,WBC-status=1"),col=c(1,2,3,4),lty=c(1,2,3,4))



#Create survival curves
fit<-survfit(s~studata$Treatment+studata$WBC_factor) 
## plot Kaplan-Meier survival curves

plot(fit,mark.time=F,conf.int = F,
     main="survival curves",
     xlab="Follow-up time",
     ylab="Survival probability",
     col=c(1,2,3,4),lty=c(1,2,3,4),
     xlim=c(0,35),ylim=c(0,1)
)
legend("topright",c("Treatment=0,WBC-status=0","Treatment=0,WBC-status=1","Treatment=1,WBC-status=0","Treatment=1,WBC-status=1"),col=c(1,2,3,4),lty=c(1,2,3,4))

par(new=T)
