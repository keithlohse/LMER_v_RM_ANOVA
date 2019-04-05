# Loading the essential libraries. 
library("ggplot2"); library("lme4"); library("car"); 
library("dplyr"); library("ez"); library("lmerTest")


##----------------------- Data Cleaning and QA ---------------------------------
## Setting the Directory -------------------------------------------------------
DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/LMER_v_RM_ANOVA/master/data_JMLD.csv")
head(DATA)

## ----------------------- Basic Data Visualization ----------------------------
## FIM scores by group and time -----------------------------------------------
g1<-ggplot(DATA, aes(x = time, y = score)) +
  geom_point(aes(fill=as.factor(subID)), pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~Group)
g2<-g1+scale_x_continuous(name = "Time (Months)", limits=c(0,20)) +
  scale_y_continuous(name = "Score (0-100)",limits=c(0,100))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position="none")

plot(g3)
## -----------------------------------------------------------------------------



# Creating our Time variable ---------------------------------------------------
summary(DATA$time)
DATA$year<-DATA$time/12
# Centering Time on the first point for each participant
DATA$year.0<-DATA$year-min(DATA$year)
summary(DATA$year.0)
# Quadratic and Cubic trnasformations
DATA$year.0_sq<-DATA$year.0^2
DATA$year.0_cu<-DATA$year.0^3

## LMER 
time_LMER<-lmer(score~
                # Fixed-effects
                1+time+I(time^2)+I(time^3)+
                # Random-effects
                (1+time|subID), data=DATA, REML=FALSE)
Anova(time_LMER, type="III")
summary(time_LMER)



## NLMER
library(nlme)

str(Theoph)
str(Orange)
head(Orange)

plot(Orange, outer=~1)
logist<-function(x, Asym, xmid, scal) Asym/(1+exp(-(x-xmid)/scal))

logist<-deriv(~Asym/(1+exp(-(x-xmid)/scal)), c("Asym","xmid","scal"),
              function(x, Asym, xmid, scal){})
Asym <- 180; xmid = 700; scal <- 300

logist(Orange$age, Asym, xmid, scal)


fm10ran.nls<-nls(circumference~logist(age, Asym, xmid, scal),
                 data = Orange, start = c(Asym=170, xmid=700, scal=500))
summary(fm10ran.nls)

plot(fm10ran.nls)
plot(fm10ran.nls, Tree~resid(.), abline=0)


fm10ran.lis<-nlsList(circumference~SSlogis(age, Asym, xmid, scal)|Tree, 
                     data=Orange)
summary(fm10ran.lis)


fm10ran.lis.noSS<-nlsList(circumference~SSlogis(age, Asym, xmid, scal)|Tree, 
                     data=Orange,
                     start=c(Asym=170, xmid=700, scal=500))
summary(fm10ran.lis.noSS)

plot(intervals(fm10ran.lis), layout=c(3,1))



