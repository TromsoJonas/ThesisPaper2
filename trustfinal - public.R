setwd("C:/Users/jse022/Dropbox/Documents/Doktorgrad UiT/Tillit2")
getwd()
library(foreign)
library(stringr)
library(plyr)
library(lme4)
library(readstata13)
library(tidyverse)
library(nlme)
rm(list=ls())

tillit <- read.dta13("trust_public.dta")
tillit <- tillit %>% filter(!is.na(ntrust))
tillit <- select(tillit, -SPA)
tillit <- select(tillit, -nowork)
tillit$ntrust<-as.numeric(tillit$ntrust)
tillit$ltrust<-as.numeric(tillit$ltrust)
tillit$income<-as.numeric(tillit$income)
tillit <- tillit %>% mutate(difftrust = ltrust-ntrust)
head(tillit)
glimpse(tillit)
table(tillit$Fylke)

## Adding regional variables level 2----
library(readxl)
gdp <- read_excel("C:/Users/jse022/Dropbox/Documents/Doktorgrad UiT/Tillit2/gdp.xlsx")
d2 <- merge(tillit,gdp, by="Fylke") 
head(d2)
tillit <- d2

reise2 <- read_excel("C:/Users/jse022/Dropbox/Documents/Doktorgrad UiT/Tillit2/reise.xlsx")
d2 <- merge(tillit,reise2, by="kom") 
head(d2)
tillit <- d2

library(haven)
FivaHalseNatvik2017 <- read_dta("C:/Users/jse022/Dropbox/Documents/Doktorgrad UiT/Datasett/FivaHalseNatvik2017.dta")
e<- FivaHalseNatvik2017
e <- e %>% filter(year==2015)
e <- e %>% select(knr, unemployment)
e$kom <- e$knr
e$kom_unemployment <- e$unemployment*100
e <- e %>% select(kom, kom_unemployment)
d2 <- merge(tillit,e, by="kom") 
tillit <- d2

gator <- read_dta("C:/Users/jse022/Dropbox/Documents/Doktorgrad UiT/Datasett/gator.dta")
f <- gator
f <- f %>% filter(year==2015)
f <- f %>% select(komnr, kommune, innvandrer)
f$kom <- f$komnr
f <- subset(f, kommune!="Longyearbyen")
f <- f %>% select(kom, innvandrer)
d3 <- merge(tillit,f, by="kom") 
tillit <- d3
tillit$oslodistance <- tillit$oslodistance/100

## New Regression models----
mod_tom <- lmer(ntrust ~ 1 + (1|kom), data=tillit)
summary(mod_tom)
library(sjstats)
icc(mod_tom)

model1 <- lmer(ntrust~north+west+rurality+(1|kom), data=tillit)
summary(model1)
model1 <- lme(ntrust~north+west+rurality, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model1b)
stargazer(model1, model1b, type="text")

model2 <- lme(ntrust~north+west+rurality+hedu+income+voluntary, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model2)

model3 <- lme(ntrust~north+west+rurality+hedu+income+voluntary+statesat, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model3)

model4 <- lme(ntrust~north+west+rurality+hedu+income+voluntary+statesat+frp+government+novote, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model4)

model4b <- lme(ntrust~north+west+rurality+hedu+income+voluntary+statesat+frp+government+novote+GDP+kom_unemployment+innvandrer, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model4b)

model4b <- lme(ntrust~north+west+rurality+hedu+income+voluntary+statesat+frp+government+novote+GDP+kom_unemployment+innvandrer, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model4b)

model4d <- lme(ntrust~north+west+rurality+hedu+income+voluntary+statesat+frp+government+novote+GDP+kom_unemployment+innvandrer+oslodistance, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model4d)

model4e <- lme(ntrust~north+west+rurality+hedu+income+voluntary+statesat+frp+government+novote+GDP+kom_unemployment+innvandrer+oslodistance+oslodistance:north, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model4e)

model4f <- lme(ntrust~west+rurality+hedu+income+voluntary+statesat+frp+government+novote+GDP+kom_unemployment+innvandrer+oslodistance, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model4f)

modLM <- lm(ntrust~north+west+rurality+hedu+income+voluntary+statesat+frp+government+novote+GDP+kom_unemployment+innvandrer, data=tillit, na.action=na.omit)
summary(modLM)


## Local democratic models ----
model5 <- lme(difftrust~north+west+rurality, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model5)

model6 <- lme(difftrust~north+west+rurality+hedu+income+voluntary, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model6)

model7 <- lme(difftrust~north+west+rurality+hedu+income+voluntary+statesat, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model7)

model8 <- lme(difftrust~north+west+rurality+hedu+income+voluntary+statesat+frp+government+novote, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model8)

model9 <- lme(difftrust~north+west+rurality+hedu+income+voluntary+statesat+frp+government+novote+GDP+kom_unemployment+ innvandrer, random= ~1|kom, data=tillit, na.action=na.omit, method="ML")
summary(model9)

library(stargazer)

stargazer(model5, model6, model7, model8, model9, title="Table 2: Regression Results", align=TRUE, digits =2,
          out = "table2.html",
          covariate.labels = c("Northern Norway", "South-West  Norway", "Rurality",
                               "Higher Education", "Income", "Membership in voluntary associations", "Satisfaction with governmental services", "Right wing voter", "Government party voter", "Abstainer", "Regional GDP", "Municipal unemployment rate", "Percentage of pop classified as immigrants"), dep.var.labels = "Difference in trust between national and local politicians", notes = "Standard errors in parenthesis", model.numbers          = FALSE, column.labels = c("(6)", "(7)", "(8)", "(9)", "(10)"))

stargazer(model1, model4, model4b, model5, model8, model9, title="Table 1: Regression Results", align=TRUE, digits =2,
          out = "table1.html",
          covariate.labels = c("Northern Norway", "South-West  Norway", "Rurality",
                               "Higher Education", "Income", "Membership in voluntary associations", "Satisfaction with government services", "Right wing voter", "Government party voter", "Abstainer", "Regional GDP", "Municipal unemployment", "Percentage of population classifed as immigrants"),
          dep.var.labels = c("Trust in national politicians", "Difference in trust between national and local"), notes = "Standard errors in parenthesis")


## descriptive statistics----
tillit2 <- tillit
tillit2 <- select(tillit2, -munsat)
tillit2 <- select(tillit2, -statesat_m)
tillit2 <- select(tillit2, -kom)
tillit2 <- select(tillit2, -year)

summary(tillit2)
stargazer(tillit2, type="text")
stargazer(tillit2, type = "html", title = "Appendix 1: Descriptive statistics of variables", align = TRUE, digits=2, out ="appendix.html",           
          covariate.labels = c("Trust in local politicians", "Trust in national politicians", "Income",
                             "Rurality", "Higher education", "Voting for parties in government", "Abstain",
                               "Voted for right-wing party", "Northern Norway", "South West Norway", "Membership in voluntary associations", "Satisfaction with government services", "Difference in trust local-national"))

