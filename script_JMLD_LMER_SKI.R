# Title: Modeling Longitudinal Outcomes: A Contrast of Two Methods
# Journal: Currently under review at the Journal of Motor Learning and Development
# By Keith Lohse and Jincheng Shen, 2019-11-02

# Loading the essential libraries. 
library("ggplot2"); library("lme4"); library("car"); 
library("dplyr"); library("ez"); library("lmerTest")

# If these packages are not installed already, run the following code:
# install.packages("ggplot2"); install.packages("lme4"); install.packages("car"); 
# install.packages("dplyr"); install.packages("ez");


##----------------------- Data Cleaning and QA ---------------------------------
## Setting the Directory -------------------------------------------------------
getwd()
# Set your working directory to the file location on your computer. 
# Make sure that you have also downloaded all of the data from GitHub.
setwd("/Set_File_Path_Here")
list.files()
# Make sure that the file data_JMLD_SKI.csv is saved in your working directory.

# Import the .csv file into R. 
# We will save this file in the R environment as an object called "DATA".
DATA<-read.csv("./data_JMLD_SKI.csv", header = TRUE, sep=",",  
               na.strings=c("NA","NaN"," ",""))

# Use the head() function to check the structure of the data file.
head(DATA)


# UPDATE
# Alternately you can also download the data file from the web here:
# DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/LMER_v_RM_ANOVA/master/data_JMLD_SKI.csv")

## ----------------------- Basic Data Visualization ----------------------------
## Figure 1. Rank by Age and Time ----------------------------------------------
ggplot(DATA, aes(x = year, y = rank)) +
  geom_line(aes(group=subID, col=as.factor(age_2018)))+ 
  # geom_point(aes(fill=as.factor(age_2018)), pch=21, size=2, stroke=1.25)
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "National Points") +
  theme_classic()+
  scale_color_manual(values=c("#f0f0f0", "#d9d9d9", "#bdbdbd", "#969696",
                              "#737373", "#525252", "#252525"))+
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold"), 
        strip.text.x = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))+
  guides(col=guide_legend(title="Age in 2018"))



ggplot(data = DATA, 
       mapping = aes(x = as.factor(year), y = rank)) +
  geom_jitter(aes(fill=as.factor(age_2018)), 
              position=position_jitterdodge(dodge.width=0.7), 
              pch=21, size=1.5, stroke=1, col="black", alpha = .8) + 
  geom_boxplot(alpha = .8, notch=FALSE, col="black", lwd=1, outlier.shape=NA)+
  scale_x_discrete(name = "Year", breaks=c(2010, 2012, 2014, 2016, 2018)) +
  scale_y_continuous(name = "National Points") +
  theme_classic()+
  scale_fill_manual(values=c("#f0f0f0", "#d9d9d9", "#bdbdbd", "#969696",
                              "#737373", "#525252", "#252525"))+
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold"), 
        strip.text.x = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))+
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
  geom_point(pch=21, size=1, stroke=1, col="black", alpha = .8) + 
  stat_smooth(method="lm", se=TRUE, col="black", lty=2) + 
  scale_x_continuous(name="Average Hours/Year over Career\n(in Hundreds of Hours)", 
                     limits = c(3,8))+
  scale_y_continuous(name="Points in 2018 (Intercept)")+
  theme_classic()+
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold"), 
        strip.text.x = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position="none")


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
  theme_classic()+
  scale_color_manual(values=c("#bdbdbd", "#969696",
                             "#737373", "#525252", "#252525"))+
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold"), 
        strip.text.x = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))+
  guides(col=guide_legend(title="Age in 2018"))



ggplot(data = DAT2, 
       mapping = aes(x = as.factor(year), y = rank)) +
  geom_jitter(aes(fill=as.factor(age_2018)), 
              position=position_jitterdodge(dodge.width=0.7), 
              pch=21, size=1.5, stroke=1, col="black", alpha = .8) + 
  geom_boxplot(alpha = .8, notch=FALSE, col="black", lwd=1, outlier.shape=NA)+
  scale_x_discrete(name = "Year", breaks=c(2015, 2016, 2017, 2018)) +
  scale_y_continuous(name = "National Ranking") +
  theme_classic()+
  scale_fill_manual(values=c("#bdbdbd", "#969696",
                             "#737373", "#525252", "#252525"))+
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold"), 
        strip.text.x = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))+
  guides(fill=guide_legend(title="Age in 2018"))


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













## Nonlinear Mixed-Effects Models Using a Negative Exponential Function
library(mvtnorm); library(lme4); library(MASS);
library(ggplot2); library(dplyr); library(nlme);

# Because nonlinear models use an iterative testing procedure to find 
# parameter values that reduce the deviance, we will want to set a "seed"
# so that we get consistent results.
set.seed(100)

# Additionally, in order to ensure our nonlinear model converge, we need to set
# starting values for our different parameters. These starting values can often
# be determined from a visual inspection of the data or from summary statistics,
# see Pineheiro & Bates. 

# For our particular data, each row represents 1 observation within a person
# within data you need at least: y - outcome, grp - group indicator, t - time.
# For the given 4 cases, we use starting values of: 
##             case1: start = c(0, 0, 90, 0, -0.3, -0.3),
##             case2: start = c(0, 10, 90, -10, -0.5, 0),
##             case3: start = c(90, 0, -90, 0, -0.3, 0.3),
##             case4: start = c(90, -10, -90, 10, -0.3, 0),
# Where:
# The first value is an estimate for the asmyptote
# The second value is an estimate for the group difference in asymptote
# The third value is an estimate of the change (asymptote to psuedo intercept)
# The fourth value is an estimate of the group difference in change
# The fifth values is an estimate of the rate parameter
# The sixth value is an estimate of the group difference in the rate parameter.


# Case 1: Error/time as the DV; groups differ in rate: 
fdat1<-read.csv("./data_NLME_case01.csv", header=TRUE)
head(fdat1)
neg_exp_rand_mod <- nlme(y ~ b_1i + b_2*grp + (b_3 + b_4*grp)*(exp(b_5i * t + b_6 * grp * t)),
                         data = fdat1,
                         fixed = b_1i + b_2 + b_3 + b_4 + b_5i + b_6 ~ 1,
                         random = b_1i + b_5i ~ 1,
                         groups = ~ ID,
                         start = c(0, 0, 90, 0, -0.3, -0.3),
                         na.action = na.omit)
summary(neg_exp_rand_mod)

fdat1$grp_cat <- factor(fdat1$grp)
MEANS <- fdat1 %>%
  group_by(grp_cat, t) %>%
  summarize(y = mean(y, na.rm=TRUE))
head(MEANS)

g1<-ggplot(fdat1, aes(x = t, y = y)) +
  geom_line(aes(group=ID, col=grp_cat), alpha=0.6)+
  geom_line(data=MEANS, aes(col=grp_cat), lwd=1.25)+
  scale_color_manual(values=c("#000000", "#969696"), labels=c("A","B"))
g2<-g1+scale_x_continuous(name = "Time (Arbitrary Units)", limits=c(0,20)) +
  scale_y_continuous(name = "Performance (Arbitrary Units)", limits=c(-2,100))
g3 <- g2 + theme_classic() + 
  labs(color = "Group") +
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold"), 
        legend.text=element_text(size=14,face="bold"),
        legend.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14), 
        legend.position="right")

print(g3)


# Case 2: Error/time as the DV; groups differ in asmyptote: 
fdat2<-read.csv("./data_NLME_case02.csv", header=TRUE)
head(fdat2)
neg_exp_rand_mod <- nlme(y ~ b_1i + b_2*grp + (b_3 + b_4*grp)*(exp(b_5i * t + b_6 * grp * t)),
                         data = fdat2,
                         fixed = b_1i + b_2 + b_3 + b_4 + b_5i + b_6 ~ 1,
                         random = b_1i + b_5i ~ 1,
                         groups = ~ ID,
                         start = c(0, 10, 90, -10, -0.5, 0),
                         na.action = na.omit)
summary(neg_exp_rand_mod)

fdat2$grp_cat <- factor(fdat2$grp)
MEANS <- fdat2 %>%
  group_by(grp_cat, t) %>%
  summarize(y = mean(y, na.rm=TRUE))
head(MEANS)
xtabs(~t, data=fdat2)

g1<-ggplot(fdat2, aes(x = t, y = y)) +
  geom_line(aes(group=ID, col=grp_cat), alpha=0.6)+
  geom_line(data=MEANS, aes(col=grp_cat), lwd=1.25)+
  scale_color_manual(values=c("#000000", "#969696"), labels=c("A","B"))
g2<-g1+scale_x_continuous(name = "Time (Arbitrary Units)", limits=c(0,20)) +
  scale_y_continuous(name = "Performance (Arbitrary Units)", limits=c(-2,100))
g3 <- g2 + theme_classic() + 
  labs(color = "Group") +
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold"), 
        legend.text=element_text(size=14,face="bold"),
        legend.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14), 
        legend.position="right")
print(g3)




# Case 3: Accuracy as the DV; groups differ in rate: 
fdat3<-read.csv("./data_NLME_case03.csv", header=TRUE)
head(fdat3)
neg_exp_rand_mod <- nlme(y ~ b_1i + b_2*grp + (b_3 + b_4*grp)*(exp(b_5i * t + b_6 * grp * t)),
                         data = fdat3,
                         fixed = b_1i + b_2 + b_3 + b_4 + b_5i + b_6 ~ 1,
                         random = b_1i + b_5i ~ 1,
                         groups = ~ ID,
                         start = c(90, 0, -90, 0, -0.3, 0.3),
                         na.action = na.omit)
summary(neg_exp_rand_mod)

fdat3$grp_cat <- factor(fdat3$grp)
MEANS <- fdat3 %>%
  group_by(grp_cat, t) %>%
  summarize(y = mean(y, na.rm=TRUE))
head(MEANS)

g1<-ggplot(fdat3, aes(x = t, y = y)) +
  geom_line(aes(group=ID, col=grp_cat), alpha=0.6)+
  geom_line(data=MEANS, aes(col=grp_cat), lwd=1.25)+
  scale_color_manual(values=c("#000000", "#969696"), labels=c("A","B"))
g2<-g1+scale_x_continuous(name = "Time (Arbitrary Units)", limits=c(0,20)) +
  scale_y_continuous(name = "Performance (Arbitrary Units)", limits=c(-2,100))
g3 <- g2 + theme_classic() + 
  labs(color = "Group") +
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold"), 
        legend.text=element_text(size=14,face="bold"),
        legend.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14), 
        legend.position="right")
print(g3)







# Case 4: Accuracy as the DV; groups differ in asymptote: 
fdat4<-read.csv("./data_NLME_case04.csv", header=TRUE)

head(fdat4)
neg_exp_rand_mod <- nlme(y ~ b_1i + b_2*grp + (b_3 + b_4*grp)*(exp(b_5i * t + b_6 * grp * t)),
                         data = fdat4,
                         fixed = b_1i + b_2 + b_3 + b_4 + b_5i + b_6 ~ 1,
                         random = b_1i + b_5i ~ 1,
                         groups = ~ ID,
                         start = c(90, -10, -90, 10, -0.3, 0),
                         na.action = na.omit)
summary(neg_exp_rand_mod)

fdat4$grp_cat <- factor(fdat4$grp)
MEANS <- fdat4 %>%
  group_by(grp_cat, t) %>%
  summarize(y = mean(y, na.rm=TRUE))
head(MEANS)

g1<-ggplot(fdat4, aes(x = t, y = y)) +
  geom_line(aes(group=ID, col=grp_cat), alpha=0.6)+
  geom_line(data=MEANS, aes(col=grp_cat), lwd=1.25)+
  scale_color_manual(values=c("#000000", "#969696"), labels=c("A","B"))
g2<-g1+scale_x_continuous(name = "Time (Arbitrary Units)", limits=c(0,20)) +
  scale_y_continuous(name = "Performance (Arbitrary Units)", limits=c(-2,100))
g3 <- g2 + theme_classic() + 
  labs(color = "Group") +
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold"), 
        legend.text=element_text(size=14,face="bold"),
        legend.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 14), 
        legend.position="right")
print(g3)



