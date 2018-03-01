setwd("C:/Users/rober/Dropbox/Grad School Research/Manuscripts/O nuchicornis paper")
library("smatr")
library("cowplot")
library("ggplot2")
library(lsmeans)
library(wesanderson)

dung<- read.table("o_nuchicornis_male_measurements_set1_feb17_2016.csv", header= TRUE , sep= ",")
dung$latitude <- as.factor(dung$latitude)
dung$longitude <- as.factor(dung$longitude)
dung$farm.code <- as.factor(dung$farm.code)
dung$location.code <- as.factor(dung$location.code)
dung$year <- as.factor(dung$year)
str(dung)

#this code generates all the slopes for each location, that i can manually extract. 
#it tests whether each one is equal to 1, but that's not important
dung.slopes<- sma(horn.l~pronot.l+location.code, log="xy", data=dung, slope.test = 1)
summary(dung.slopes)

#this code removes BC as a factor due to few observations
#it also calculates allometry between or and wa
dung <-subset(dung, location != "BC")
dung<- dung[order(dung$latitude),]
dung.allom.loc<- sma(horn.l~pronot.l+location, log="xy", data=dung)
summary(dung.allom.loc)
#the allometric slopes are different, but that's using a linear model
#i'm not used to dealing with sigmoidal data comparisons, ask teiya
dung$horn.log<-log(dung$horn.l)
dung$pronot.log<-log(dung$pronot.l)

reg.dung.1 <- lm(horn.log~location/pronot.log -1, data=dung)
summary(reg.dung.1)

reg.dung.2 <- lm(horn.log~pronot.log*location, data=dung)
summary(reg.dung.2)
#let's interpret this output
#(Intercept) is the, well, intercept for the 'or' data, s0 -0.08 on a logscale 
#the slope of the or data is 6.05
#locationWA tells the CHANGE in intercept for location WA
#so (-0.8 + 0.15) = the interecept for WA, i.e. -0.65290
#the line under that is the change in SLOPE between the two factors
#so (6.05254 +0.53530)=6.58784
#more importantly the significance is looking at what the model is testing. 
#it is basically looking first to see whether the base data (or) is nonzero 
#the bottom two lines though indicate that the slope AND intercept of the WA data is different
#nice

##TRY AND FIND A WAY TO GRAPH A HISTOGRAM OF THE DISTRIBUTIONS OF HORN LENGTHS BASED ON THAT ONE PAPER
ggplot(data=dung, aes(horn.l, fill=location))+geom_histogram()
ggplot(data=dung, aes(pronot.l, fill=location))+geom_histogram()
locations <- data.frame(summary(dung$location.code))

#in a non-R step, I took all the slopes generated and matched them up with their latitudes
#importing that file
dung.slopes.l<- na.omit(read.table("o_nuchicornis_male_measurements_slopes.csv", header= TRUE , sep= ","))
str(dung.slopes.l)
summary(dung.slopes.l)
dung.slopes.l <-subset(dung.slopes.l, location != "BC")
dung.slopes.l<- dung.slopes.l[order(dung.slopes.l$latitude),]



dung.slopes.m1<-lm(slope~latitude+location+type+year, data=dung.slopes.l)
summary(dung.slopes.m1)
anova(dung.slopes.m1)


dung.slopes.or <-subset(dung.slopes.l, location=="or")
dung.slopes.wa <- subset(dung.slopes.l, location=="wa")

var.test(dung.slopes.or$slope, dung.slopes.wa$slope)
t.test(dung.slopes.or$slope, dung.slopes.wa$slope)





#NON LOG TRANSFORMED
ggplot(dung, aes(x=pronot.l, y=horn.l, shape=location))+
  geom_point(aes(color=location))+
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling"))+
  theme_grey()+
  labs(x="Pronotal Width (mm)", y="Horn Length (mm)")

#log transformed
#use wes anderson palettes
ggplot(dung, aes(x=log(pronot.l), y=log(horn.l), shape=location))+
  geom_point(aes(color=location))+
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling"))+
  theme_grey()+
  labs(x="Log Pronotal Width", y="Log Horn Length")


ggplot(dung.slopes.l, aes(x=latitude, y=slope, shape=location)) +
  geom_point() +
  theme_grey()+
  geom_smooth(method='lm', se=FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  labs(x="Latitude", y="Slope")

ggplot(dung.slopes.wa, aes(x=latitude, y=slope)) +
  geom_point() +
  theme_grey()+
  geom_smooth(method='lm', se=FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  ggtitle("Washington Slopes")+
  labs(x="Latitude", y="Slope")

wa.slope.lm <- lm(slope~latitude, data=dung.slopes.wa)
summary(wa.slope.lm)

or.slope.lm <- lm(slope~latitude, data=dung.slopes.or)
summary(or.slope.lm)

ggplot(dung.slopes.or, aes(x=latitude, y=slope)) +
  geom_point() +
  theme_grey()+
  geom_smooth(method='lm', se=FALSE)+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  ggtitle("Oregon Slopes")+
  labs(x="Latitude", y="Slope")

ggplot(dung.slopes.l, aes(x=location, y=slope, fill=location))+
  geom_boxplot()+
  scale_fill_manual(values=wes_palette(n=2, name="Darjeeling"))+
  theme_grey()+
  labs(x="Location", y="Slope")

##model assumptions for initial linear modle
par(mfrow=c(2,2))
plot(reg.dung.1)
par(mfrow=c(1,1))

##model assumptions for slope by latitude
par(mfrow=c(2,2))
plot(dung.slopes.m1)
par(mfrow=c(1,1))


####playground
ggplot(subset(dung, location=="or"), aes(x=log(pronot.l), y=log(horn.l)))+
  geom_point(aes(color=location))+
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling"))+
  geom_smooth()
theme_grey()+
  labs(x="Log Pronotal Width", y="Log Horn Length")