###########
#This code takes data from the Seagrass and the Water Column Experiments
#It analyzes the data using linear models, correlation tests, and GAMs
#Created by Bridget Shayka
##########


##Load libraries -------------
library(tidyverse)
library(nortest) #for testing model assumptions
library(MuMIn) #for dredge
library(mgcv) #for GAMs
library(broom) #for AIC comparisons



##Load functions ------------
stand <- function(X) { (X-mean(X,na.rm=T))/(2*sd(X,na.rm=T)) }#function


##Load data -----------------

growthdata <- read_csv('processed_data/combined_growth.csv',
                       col_types = cols(.default = "d", n = "f", n.s = "f", expt = "f", nfac = "f"))
#ntrt, p, np are dbl; nstand, p.s, np.s are dbl standardized values; n is factor of ntrt; n.s is factor of nstand; nfac is factor of n values as chrs (a,b,c)



seagrassgrowth <- growthdata %>%
  filter(expt == "seagrass") 
phytogrowth <- growthdata %>%
  filter(expt == "phyto")


seagrassalldata <- read_csv('processed_data/ratio_data.csv',
                            col_types = cols(.default = "d", n = "f", n.s = "f", Site = "f", nfac = "f"))



##Q1 analysis: Linear model ------

#n as dbl, expt as factor and fixed effect
growthmodel2b <- lm(growth.s ~ (nstand * p.s * expt) + (log(np.s+10) * expt), data=growthdata) #assumptions are good
summary(growthmodel2b)


#checks model assumptions
quartz()
rm=resid(growthmodel2b)
fm=fitted(growthmodel2b)
model2=lm(rm~fm)
par(mfrow=c(3,2))
plot(model2)
hist(rm)
plot(c(0,5),c(0,5))
text(3,3.5,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
text(3,2,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)



##Q1 analysis: Model selection analysis ------

#Full model
#this runs through all iterations of the model and ranks based on AIC
options(na.action=na.fail) #dredge doesn't like na.omit, so this changes the default na.action to na.fail; make sure there aren't any NAs in the dataset before this point
dredge(growthmodel2b)
options(na.action=na.omit) #this sets the NA situation back to the default na.omit

#best model
bestfull <- lm(growth.s ~ (nstand * expt) + (p.s * expt) + (log(np.s + 10) * expt), data = growthdata) #good assumptions
summary(bestfull)

#checks model assumptions
quartz()
rm=resid(bestfull)
fm=fitted(bestfull)
model2=lm(rm~fm)
par(mfrow=c(3,2))
plot(model2)
hist(rm)
plot(c(0,5),c(0,5))
text(3,3.5,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
text(3,2,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)


 
#Seagrass model
growthmodelseagrass <- lm(growth.s ~ nstand * p.s + log(np.s +1), data=seagrassgrowth) #assumptions are good
summary(growthmodelseagrass)

#checks model assumptions
quartz()
rm=resid(growthmodelseagrass)
fm=fitted(growthmodelseagrass)
model2=lm(rm~fm)
par(mfrow=c(3,2))
plot(model2)
hist(rm)
plot(c(0,5),c(0,5))
text(3,3.5,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
text(3,2,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)

#this runs through all iterations of the model and ranks based on AIC
options(na.action=na.fail) #dredge doesn't like na.omit, so this changes the default na.action to na.fail; make sure there aren't any NAs in the dataset before this point
dredge(growthmodelseagrass)
options(na.action=na.omit) #this sets the NA situation back to the default na.omit

#best model
bestseagrass <- lm(growth.s ~ p.s, data = seagrassgrowth) #good assumptions
summary(bestseagrass)

#checks model assumptions
quartz()
rm=resid(bestseagrass)
fm=fitted(bestseagrass)
model2=lm(rm~fm)
par(mfrow=c(3,2))
plot(model2)
hist(rm)
plot(c(0,5),c(0,5))
text(3,3.5,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
text(3,2,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)



#Phytoplankton
growthmodelphyto <- lm(growth.s ~ nstand * p.s + log(np.s + 10), data=phytogrowth) #assumptions are good
summary(growthmodelphyto)

#checks model assumptions
quartz()
rm=resid(growthmodelphyto)
fm=fitted(growthmodelphyto)
model2=lm(rm~fm)
par(mfrow=c(3,2))
plot(model2)
hist(rm)
plot(c(0,5),c(0,5))
text(3,3.5,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
text(3,2,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)

#this runs through all iterations of the model and ranks based on AIC
options(na.action=na.fail) #dredge doesn't like na.omit, so this changes the default na.action to na.fail; make sure there aren't any NAs in the dataset before this point
dredge(growthmodelphyto)
options(na.action=na.omit) #this sets the NA situation back to the default na.omit

#best model
bestphyto <- lm(growth.s ~ nstand + log(np.s + 10), data = phytogrowth) #good assumptions
summary(bestphyto)

#checks model assumptions
quartz()
rm=resid(bestphyto)
fm=fitted(bestphyto)
model2=lm(rm~fm)
par(mfrow=c(3,2))
plot(model2)
hist(rm)
plot(c(0,5),c(0,5))
text(3,3.5,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
text(3,2,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)

#this is to see whether N or NP has stronger effect - it is N
bestphyto2 <- lm(growth.s ~ nstand + np.s, data = phytogrowth) #good assumptions
summary(bestphyto2)

#checks model assumptions
quartz()
rm=resid(bestphyto2)
fm=fitted(bestphyto2)
model2=lm(rm~fm)
par(mfrow=c(3,2))
plot(model2)
hist(rm)
plot(c(0,5),c(0,5))
text(3,3.5,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
text(3,2,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)



##Q1 analysis: Correlation tests -------

corrplots1 <-  seagrassalldata %>%
  select(avggrowthweight, avgbladearea, avgheight, avgepweightperarea, avgbitespersa, species, D, tshoots, sshoots) %>% 
  ggpairs()

corrplots2 <-  seagrassalldata %>%
  select(avggrowthweight, meanC, meanN, meanP, CN, CP, NP) %>% 
  ggpairs()

cor.test(seagrassalldata$avggrowthweight, seagrassalldata$avgbladearea, method = "pearson")
cor.test(seagrassalldata$avggrowthweight, seagrassalldata$avgheight, method = "pearson")




##Q2 analysis: GAMs ------

#AIC comparison between GAM and linear model

#seagrass
gamseagrass <- mgcv::gam(data = seagrassgrowth, growth ~ s(np), method = "REML")
summary(gamseagrass)
plot(gamseagrass, se=T)

lmseagrass <- lm(growth ~ np, data=seagrassgrowth)

sgmodels <- list(gamseagrass, lmseagrass)
AIC(gamseagrass, lmseagrass)
summary(gamseagrass) #higher r2
summary(lmseagrass) #assumptions fine
broom::glance(gamseagrass) #lower AIC
broom::glance(lmseagrass) 

#check model assumptions for linear models
quartz()
rm=resid(lmseagrass)
fm=fitted(lmseagrass)
model2=lm(rm~fm)
par(mfrow=c(3,2))
plot(model2)
hist(rm)
plot(c(0,5),c(0,5))
text(3,3.5,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
text(3,2,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)



#phyto
gamphyto <- mgcv::gam(data = phytogrowth, growth ~ s(np), method = "REML")
summary(gamphyto)
plot(gamphyto, se=T)

lmphyto <- lm(growth ~ np, data=phytogrowth)

phmodels <- list(gamphyto, lmphyto)
AIC(gamphyto, lmphyto)
summary(gamphyto) #same r2
summary(lmphyto) #assumptions good #same r2
broom::glance(gamphyto) #same AIC
broom::glance(lmphyto) #same

#check model assumptions for linear models
quartz()
rm=resid(lmphyto)
fm=fitted(lmphyto)
model2=lm(rm~fm)
par(mfrow=c(3,2))
plot(model2)
hist(rm)
plot(c(0,5),c(0,5))
text(3,3.5,paste("shapiro = ",round(shapiro.test(rm)[[2]],3)),cex=2)
#library(nortest)
text(3,2,paste("lillie = ",round(lillie.test(rm)[[2]],3)),cex=2)



#this gives np for max growth rates
gamseagrass_pred %>%
  filter(predicted_values == max(predicted_values))

gamphyto_pred %>%
  filter(predicted_values == max(predicted_values))







