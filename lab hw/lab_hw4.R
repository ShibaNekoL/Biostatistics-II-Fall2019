##1
cholera<-read.csv(file.choose())

# build new data.frame
district_levels<-c(levels(cholera$District))
company_levels<-c(1,2)
district<-vector()
company<-vector()
deaths<-vector()
py<-vector()

k<-rep(c(1:9),each=2)
company<-rep(c(1:2),9)
for(i in c(1:18)){
  district[i]<-district_levels[k[i]]
}
for(i in seq(1,18,2)){
  deaths[i]<-subset(cholera,cholera$District==district_levels[k[i]] & cholera$Company==company_levels[1] & cholera$DeathIndicator==1)$ Count
  py[i]<-sum(subset(cholera,cholera$District==district_levels[k[i]] & cholera$Company==company_levels[1])$ Count)
}
for(i in seq(2,18,2)){
  deaths[i]<-subset(cholera,cholera$District==district_levels[k[i]] & cholera$Company==company_levels[2] & cholera$DeathIndicator==1)$ Count
  py[i]<-sum(subset(cholera,cholera$District==district_levels[k[i]] & cholera$Company==company_levels[2])$ Count)
}
cholera_new<-data.frame(district,company,deaths,py)
cholera_new$district<-as.factor(district)
cholera_new$company<-as.factor(company)
# delete which total counts=0
cholera_noNA<-subset(cholera_new,cholera_new$py!=0)

# fit the model
cholera_noNA$district<-relevel(cholera_noNA$district,"St. Savior, Southwark")
contrasts(cholera_noNA$district)
contrasts(cholera_noNA$company)
fit<-glm(deaths~company+district,offset=log(py),family=poisson,data=cholera_noNA)
summary(fit)
round(exp(coef(fit)),3)

##2
#plot mean response
cholera_noNA$deaths_py<-cholera_noNA$deaths/cholera_noNA$py
cholera_noNA_sub<-subset(cholera_noNA,cholera_noNA$district=="St. George, Southwark"|cholera_noNA$district=="Lambeth")
plot(cholera_noNA$district,cholera_noNA$deaths_py,type="n")
points(cholera_noNA_sub$district,cholera_noNA_sub$deaths_py)

attach(cholera_noNA_sub)

meanresponse<-cbind(
  predict(fit,data.frame(district="St. George, Southwark",company="1",py=1),type="response"),
  predict(fit,data.frame(district="St. George, Southwark",company="2",py=1),type="response"),
  predict(fit,data.frame(district="Lambeth",company="1",py=1),type="response"),
  predict(fit,data.frame(district="Lambeth",company="2",py=1),type="response")
)
meanresponse_num<-meanresponse[1,]
district_mean<-c("St. George, Southwark","St. George, Southwark","Lambeth","Lambeth")
company_mean<-c("1","2","1","2")
meanresponseframe<-data.frame(district_mean,company_mean,meanresponse_num)
plot(meanresponseframe$district_mean,meanresponseframe$meanresponse_num,xlab="districts",ylab="deaths rate")
points(meanresponseframe$district_mean,meanresponseframe$meanresponse_num,pch=c(1,16,1,16))

legend("topleft",legend=c("Company1","Company2"),pch=c(1,16))
