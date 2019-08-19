# ACRM 2018 Longitudinal Data Analysis Workshop
# By Keith Lohse, Neurorehabilitation Informatics Lab, 2018-10-19

# Loading the essential libraries. 
library("ggplot2"); library("lme4"); library("car"); 
library("dplyr"); library("ez"); library("lmerTest")

# If these packages are not installed already, run the following code:
# install.packages("ggplot2"); install.packages("lme4"); install.packages("car"); 
# install.packages("dplyr"); install.packages("ez");


##----------------------- Data Cleaning and QA ---------------------------------
## Setting the Directory -------------------------------------------------------
getwd()
setwd("C:/Users/u6015231/Box Sync/Collaboration/Al Kozlowski/")
list.files()
# Make sure that the file data_session1.csv is saved in your working directory.

# Import the .csv file into R. 
# We will save this file in the R environment as an object called "DATA".
DATA<-read.csv("./data_JMLD_SKI.csv", header = TRUE, sep=",",  
               na.strings=c("NA","NaN"," ",""))

# Use the head() function to check the structure of the data file.
head(DATA)


# UPDATE
# Alternately you can also download the data file from the web here:
# DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/LMER_v_RM_ANOVA/master/data_JMLD.csv")

## ----------------------- Basic Data Visualization ----------------------------
## Figure 1. Rank by Age and Time ----------------------------------------------
ggplot(DATA, aes(x = year, y = rank)) +
  geom_line(aes(group=subID, col=as.factor(age_2018)))+ 
  # geom_point(aes(fill=as.factor(age_2018)), pch=21, size=2, stroke=1.25)
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "National Points") +
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  guides(col=guide_legend(title="Age in 2018"))



ggplot(data = DATA, 
       mapping = aes(x = as.factor(year), y = rank)) +
  geom_jitter(aes(fill=as.factor(age_2018)), 
              position=position_jitterdodge(dodge.width=0.7), 
              pch=21, size=1.5, stroke=1, col="black", alpha = .8) + 
  geom_boxplot(alpha = .8, notch=FALSE, col="black", lwd=1, outlier.shape=NA)+
  scale_x_discrete(name = "Year", breaks=c(2010, 2012, 2014, 2016, 2018)) +
  scale_y_continuous(name = "National Points") +
  theme(axis.text=element_text(size=16, colour="black"),
        axis.title=element_text(size=16, colour="black", face="bold"),
        legend.position = "right")+
  guides(fill=guide_legend(title="Age in 2018"))
## -----------------------------------------------------------------------------




## --------------- Unconditional and Conditional Models ------------------------
## Unconditional Models --------------------------------------------------------
DATA<-subset(DATA, rank != "NA")
DATA$year2018_sq <- DATA$year2018^2
DATA$year2018_cu <- DATA$year2018^3


## LMER 
time_linear<-lmer(rank~
                # Fixed-effects
                1+year2018+
                # Random-effects
                (1+year2018|subID), data=DATA, REML=FALSE)
Anova(time_linear, type="III")
summary(time_linear)

time_quad<-lmer(rank~
                  # Fixed-effects
                  1+year2018+year2018_sq+
                  # Random-effects
                  (1+year2018|subID), data=DATA, REML=FALSE)
Anova(time_quad, type="III")
summary(time_quad)


time_cube<-lmer(rank~
                  # Fixed-effects
                  1+year2018+year2018_sq+year2018_cu+
                  # Random-effects
                  (1+year2018|subID), data=DATA, REML=FALSE)
Anova(time_cube, type="III")
summary(time_cube)


anova(time_linear, time_quad, time_cube)
AIC(time_quad)-AIC(time_linear)
AIC(time_cube)-AIC(time_quad)




ggplot(DATA, aes(x = year, y = rank)) +
  stat_smooth(aes(group=subID, col=as.factor(age_2018)), method="lm",
              se=FALSE, lwd=0.5)+ 
  # geom_point(aes(fill=as.factor(age_2018)), pch=21, size=2, stroke=1.25)
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "National Ranking") +
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  guides(col=guide_legend(title="Age in 2018"))






## Conditional Models ----------------------------------------------------------
## Random Effects Model
RE_LMER<-lmer(rank~
                  # Fixed-effects
                  1+year2018+year2018_sq+
                  # Random-effects
                  (1+year2018|subID), data=DATA, REML=FALSE)
summary(RE_LMER)

## LMER
head(DATA)
cond_LMER<-lmer(rank~
                # Fixed-effects
                1+year2018*hours.btw.c*age_2018.c+
                  year2018_sq*hours.btw.c*age_2018.c+
                  hours.wtn.c+
                # Random-effects
                (1+year2018|subID), data=DATA, REML=FALSE)

Anova(cond_LMER, type="III")
summary(cond_LMER)

## Creating a Level 2 Dataset ----
LVL2<-DATA %>% 
  group_by(subID) %>%
    summarise(ave_rank = mean(rank, na.rm=TRUE),
              age = age_2018[1],
              ave_hours = hours.btw[1])

mean(LVL2$age)
mean(LVL2$ave_hours)

## Figure 2: Plot of Individual Slopes and Intercepts --------------------------
m0<-lmer(rank~
           # Fixed-effects
           1+year2018+year2018_sq+
           # Random-effects
           (1+year2018|subID), data=DATA, REML=FALSE)


coef(m0)

df<-tibble::rownames_to_column(coef(m0)$subID)
head(df)
names(df) <- c("subID", "Intercept", "Year", "Year_sq")
df$subID<-factor(df$subID)
head(df)
head(DATA)
df2<-unique(DATA[,c("subID","age_engagement", "ave_rank", "hours.btw",
                    "age_2018")])
head(df2)

df<-right_join(x=df, y=df2, by="subID")
head(df)

ggplot(data = df, 
       mapping = aes(x = hours.btw, y = Intercept)) +
  geom_point(aes(fill=hours.btw), pch=21, size=1, stroke=1, 
             col="black", alpha = .8) + 
  stat_smooth(method="lm", se=TRUE, col="black", lty=2) + 
  scale_x_continuous(name="Average Hours/Year over Career\n(in Hundreds of Hours)", 
                     limits = c(3,8))+
  scale_y_continuous(name="Points in 2018 (Intercept)")+
  theme(axis.text.x=element_text(size=16, colour="black"),
        axis.text.y=element_text(size=16, colour="black"),
        title=element_text(size=16, face="bold"),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")


## Comparing LMER to RM ANOVA Models -------------------------------------------
DAT2<-read.csv("./data_JMLD_SKI_anova.csv", header = TRUE, sep=",",  
               na.strings=c("NA","NaN"," ",""))

DAT2$year2018_sq <- DAT2$year2018^2


## Random Effects Model
RE_lin<-lmer(rank~
                # Fixed-effects
                1+year2018+
                # Random-effects
                (1+year2018|subID), data=DAT2, REML=FALSE)
summary(RE_lin)

RE_sq<-lmer(rank~
                # Fixed-effects
                1+year2018+year2018_sq+
                # Random-effects
                (1+year2018|subID), data=DAT2, REML=FALSE)
summary(RE_sq)

anova(RE_lin, RE_sq)
# The linear model is a better explanation of the data when the range of time 
# is restricted.


## Figure 3. Truncated Dataset -------------------------------------------------
ggplot(DAT2, aes(x = year, y = rank)) +
  geom_line(aes(group=subID, col=as.factor(age_2018)))+ 
  # geom_point(aes(fill=as.factor(age_2018)), pch=21, size=2, stroke=1.25)
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "National Ranking") +
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  guides(col=guide_legend(title="Hours in Practice"))



ggplot(data = DAT2, 
       mapping = aes(x = as.factor(year), y = rank)) +
  geom_jitter(aes(fill=as.factor(age_2018)), 
              position=position_jitterdodge(dodge.width=0.7), 
              pch=21, size=1.5, stroke=1, col="black", alpha = .8) + 
  geom_boxplot(alpha = .8, notch=FALSE, col="black", lwd=1, outlier.shape=NA)+
  scale_x_discrete(name = "Year", breaks=c(2015, 2016, 2017, 2018)) +
  scale_y_continuous(name = "National Ranking") +
  theme(axis.text=element_text(size=16, colour="black"),
        axis.title=element_text(size=16, colour="black", face="bold"),
        legend.position = "right")+
  guides(fill=guide_legend(title="Hours in Practice"))


## Categorical Effect of Hours -------------------------------------------------
head(DAT2)

DAT2$hours_cat<-relevel(DAT2$hours_cat, ref="low")

m1<-lmer(rank~
               # Fixed-effects
               1+year2018*age_2018.c*hours.btw.c+
               # Random-effects
               (1+year2018|subID), data=DAT2, REML=FALSE)
summary(m1)
Anova(m1, type="III")

DAT2$year_cat<-as.factor(DAT2$year)
DAT2$age_cat<-factor(ntile(DAT2$age_2018,2))
summary(DAT2$age_cat)
head(DAT2)

xtabs(~year_cat+hours_cat+age_cat, data=DAT2)
xtabs(~subID+year_cat, data=DAT2)
xtabs(~hours_cat+subID, data=DAT2)


time_aov<- ezANOVA(data = DAT2,
                   dv = .(rank), 
                   wid = .(subID), 
                   within = .(year_cat), 
                   between = .(hours_cat,age_cat),
                   type = 3,
                   detailed = TRUE,
                   return_aov = TRUE)
time_aov




