survdata<-read.csv(file.choose())
attach(survdata)
survdata$x<-ifelse(WBC<median(WBC),0,1)
View(survdata)

s<-survfit(Surv(survdata$Remission.Time)~survdata$x) #以age_g分層
summary(s)

##summarized survival data
print(s,print.rmean=T)

##plot curve
#mark.time=座標點
#conf.int=信賴區間
plot(s,mark.time = F,conf.int = F,
     col=c("lightblue","thistle2"),lty=c(1,4),lwd=c(2,2),
     main="Kaplan-Meier survival curves",
     xlab="Follow-up time (days)",
     ylab="colon cancer survival probability"
)

median(a$Remission.Time)
median()

subset(survdata,survdata$x==1)
a<-subset(survdata,survdata$x==0)

(1)+(1-2*(1/22))+(1-4*(1/22))+(1-5*(1/22))+(1-6*(1/22))+(1-8*(1/22))+(1-11*(1/22))+(1-12*(1/22))+(1-15*(1/22))+(1-16*(1/22))+(1-17*(1/22))+(1-18*(1/22))+(1-19*(1/22))+3*(1-20*(1/22))+(1-21*(1/22))


##Extra
data<-read.csv(file.choose())
data$x<-ifelse(data$WBC<median(data$WBC),0,1)
data$t=ifelse(data$Remission.Time<median(data$Remission.Time),0,1)

#(a)
chisq.test(data$t,data$x)

#(b)
var.test(data$Remission.Time~data$x)
t.test(data$Remission.Time~data$x,var.equal=F)

#(c)
cor.test(data$Remission.Time, data$WBC,method="pearson")

#(d)
model<-lm(data$Remission.Time ~ data$WBC)
summary(model)

#(e)
model_log<-glm(data$t ~ data$WBC, family=binomial)
summary(model_log)

#(f)
model_poi=glm(data$Remission.Time~data$WBC,family=poisson)
summary(model_poi)