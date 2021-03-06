library(ggplot2)
library(segmented)
#trying to replicate the analysis from "Threshold Evolution in Extoic POpulations of a Polyphenic Beetle, Moczek et al 2002"
dung<- read.table("o_nuchicornis_male_measurements_set1_feb17_2016.csv", header= TRUE , sep= ",")
dung$latitude <- as.factor(dung$latitude)
dung$longitude <- as.factor(dung$longitude)
dung$farm.code <- as.factor(dung$farm.code)
dung$location.code <- as.factor(dung$location.code)
dung$year <- as.factor(dung$year)
dung <-subset(dung, location != "BC")
dung<- dung[order(dung$latitude),]
dung$horn.log<-log(dung$horn.l)
dung$pronot.log<-log(dung$pronot.l)
str(dung)

wa <- subset(dung, location=="wa")
or <- subset(dung, location=="or")

plot(wa$pronot.l,wa$horn.l, col="red")
points(or$pronot.l,or$horn.l, col="blue")

lin.mod <-lm(horn.l~pronot.l, data=dung)
plot(dung$horn.l ~ dung$pronot.l)
segmented.mod <- segmented(lin.mod, seg.Z = ~pronot.l, psi=0.85)
summary(segmented.mod)
slope(segmented.mod)


lin.mod.wa <- lm(horn.l~pronot.l, data=wa)
segmented.mod.wa <- segmented(lin.mod.wa, seg.Z = ~pronot.l, psi=0.8)
summary(segmented.mod.wa)
slope(segmented.mod.wa)
plot(wa$horn.l ~ wa$pronot.l)


lin.mod.or <- lm(horn.l~pronot.l, data=or)
segmented.mod.or <- segmented(lin.mod.or, seg.Z = ~pronot.l, psi=0.8)
summary(segmented.mod.or)
slope(segmented.mod.or)

#where y is horn length, it will calculate the model for the curve and test for differences. 
#The thing about it, though is we need to extract a few things and establish the initial parameters for the model....
#i.e. we need to get the values for beta3 and beta4 out of our own curves. 
#Namely, beta3 needs to be the slope coefficient of the initial curve 
#beta4 needs to be the body size at the inflection of the sigmoid curve we fit. 


fullmodel<-nls(horn.l~ y0 +((a*pronot.l^b)/(c^b+pronot.l^b)),
    data=dung,
    start=list(y0=min(dung$horn.l), a=range(dung$horn.l), b=1.09, c=0.872))
dung$pred<-predict(fullmodel, newdata=dung)

shapiro.test(residuals(fullmodel))

wa.model<-nls(horn.l~ y0 +((a*pronot.l^b)/(c^b+pronot.l^b)),
              data=wa,
              start=list(y0=min(wa$horn.l), a=range(wa$horn.l), b=18.4, c=0.802), nls.control(warnOnly=TRUE))
shapiro.test(residuals(wa.model))
wa$pred<-predict(wa.model, wa)
plot(wa$pronot.l, wa$pred)
##this one breaks and I'm not sure whyyyyy
or.model<-nls(horn.l~ y0 +((a*pronot.l^b)/(c^b+pronot.l^b)),
              data=or,
              start=list(y0=min(or$horn.l), a=range(or$horn.l), b=18.4, c=0.802), nls.control(warnOnly=TRUE))
or$pred<-predict(or.model, or)
plot(or$pronot.l, or$pred)
ggplot(or, aes(x=pronot.l, y=pred))+
  geom_line()+
  geom_line(data=wa, aes(x=pronot.l, y=pred), color="blue")


shapiro.test(residuals(or.model))

ks.test(residuals(wa.model), residuals(or.model))

or$resid <- as.numeric(residuals(or.model))
wa$resid <-as.numeric(residuals(wa.model))

ggplot(or, aes(x=pronot.l, y=resid))+
  geom_point(color="red")+
  geom_point(data=wa, aes(x=pronot.l, y=resid), color="blue")

ggplot(or, aes(x=pronot.log, y=horn.log))+
  geom_point(color="red")+
  geom_point(data=wa, aes(x=pronot.log, y=horn.log), color="blue")

library(nls2)
library(nlstools)
