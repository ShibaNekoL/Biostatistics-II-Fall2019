install.packages("survival")
library(survival)

data(package="survival") #把survival的所有資料叫出來
data(colon) #讀出survival中的cancer資料
force(data(colon)) #RStudio要打force才能讀取
head(colon,10) #讀10筆資料

colon$age_g<-ifelse(18<=colon$age & colon$age<=29,1,
                    ifelse(30<=colon$age & colon$age<=49,2,
                           ifelse(50<=colon$age & colon$age<=69,3,4)
                           )
                    )
colon$age_g<-as.factor(colon$age_g)

s<-survfit(Surv(colon$time,colon$status)~colon$age_g) #以age_g分層
summary(s)

##summarized survival data
print(s,print.rmean=T)

##plot curve
#mark.time=座標點
#conf.int=信賴區間
plot(s,mark.time = F,conf.int = F,
     col=c("lightblue","thistle2","lightcoral","yellowgreen"),lty=c(1,4,6,5),lwd=c(2,2,2,2),
     main="Kaplan-Meier survival curves",
     xlab="Follow-up time (days)",
     ylab="colon cancer survival probability"
)
legend("bottomright",c("18-29歲","30-49歲","50-69歲","70歲以上"),col=c("lightblue","thistle2","lightcoral","yellowgreen"),lty=c(1,4,6,5),lwd=c(2,2,2,2))


