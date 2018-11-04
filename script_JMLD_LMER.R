# ACRM 2018 Longitudinal Data Analysis Workshop
# By Keith Lohse, Neurorehabilitation Informatics Lab, 2018-10-19

# Loading the essential libraries. 
library("ggplot2"); library("lme4"); library("car"); 
library("dplyr"); library("ez");

# If these packages are not installed already, run the following code:
install.packages("ggplot2"); install.packages("lme4"); install.packages("car"); 
install.packages("dplyr"); install.packages("ez");


##----------------------- Data Cleaning and QA ---------------------------------
## Setting the Directory -------------------------------------------------------
getwd()
setwd("C:/Users/kelop/Documents/GitHub/LMER_v_RM_ANOVA/")
list.files()
# Make sure that the file data_session1.csv is saved in your working directory.

# Import the .csv file into R. 
# We will save this file in the R environment as an object called "DATA".
DATA<-read.csv("./data_JMLD.csv", header = TRUE, sep=",",  
               na.strings=c("NA","NaN"," ",""))

# Use the head() function to check the structure of the data file.
head(DATA)


# UPDATE
# Alternately you can also download the data file from the web here:
DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/LMER_v_RM_ANOVA/master/data_JMLD.csv")



## ----------------------- Basic Data Visualization ----------------------------
## FIM scores by group and time -----------------------------------------------
g1<-ggplot(DATA, aes(x = time, y = score)) +
  geom_point(aes(fill=as.factor(subID)), pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~Group)
g2<-g1+scale_x_continuous(name = "Time (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Score (0-100)",limits=c(0,100))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position="none")

plot(g3)
## -----------------------------------------------------------------------------

## Linear Fit for each person --------------------------------------------------
g1<-ggplot(DATA, aes(x = time, y = score)) +
  geom_point(aes(fill=as.factor(subID)), pch=21, size=2, stroke=1.25) +
  stat_smooth(aes(col=subID), se=FALSE, method="lm") +
  facet_wrap(~Group)
g2<-g1+scale_x_continuous(name = "Time (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Score (0-100)",limits=c(0,100))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position="none")

plot(g3)
## -----------------------------------------------------------------------------


# Understanding basic random-effects ------------------------------------------
# Random Intercepts Model ----
raneff_00<-lmer(score~
                  # Fixed-effects
                  1+
                  # Random-effects
                  (1|subID), data=DATA, REML=FALSE)
summary(raneff_00)

# Recall that the fixed-effect for the intercept is the overall, "group-level"
# intercept in our model.
# However, we also have a random-effect of subject for the intercenpt. This means
# that our model estimates a deviate for each subject from the group-level 
# intercept. To see these random-effects using the raneff function. 

ranef(raneff_00)

# Remember that these are deviates from the fixed-effect, so if we want to see
# what the model is actually estimating for each person, we need to add the 
# fixed-effect back in:
fixef(raneff_00)

# We could do this manually, adding the fixef() output to the ranef() output, 
# but we can also get the indivudal values using the coef() function:
coef(raneff_00)$subID

# If you want the actual predictions of the model, rather than the estimated
# effects, you can use the fitted() function. 
# Note the difference in the size of these arrays. The fitted() function gives
# us a prediction for each person at each point.
fitted(raneff_00)


# To help us understand the model, we can plot these predictions for each person.
# First, lets take a subset of the first 6 people:
head(DATA)
first06<-DATA[c(1:36),]
head(first06)

# Second, we'll make a smaller dataset with the predictions for these 10:
PRED<-data.frame(subID=c("s01","s02","s03","s04","s05","s06"),
                 Intercepts=c(coef(raneff_00)$subID[c(1:6),]))
PRED


g1<-ggplot(first06, aes(x = time, y = score)) +
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~subID, ncol=2)
g2<-g1+scale_x_continuous(name = "Time (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Score (0-100)",limits=c(0,100))
print(g2)
g3<-g2+geom_abline(aes(intercept=Intercepts, slope=0), col="red", lwd=1.5, PRED)
plot(g3)


# Hopefully this plot will clearly show that our model is estimating a different
# intercept for each person, but everyone has the same slope, because we have not 
# allowed FIM scores to vary over time. 


# Fixed Slope Model ----
raneff_01<-lmer(score~
                  # Fixed-effects
                  1+time+
                  # Random-effects
                  (1|subID), data=DATA, REML=FALSE)
summary(raneff_01)

# Note that when we run the ranef() function, there is still only a random effect
# of the intercept:
ranef(raneff_01)

# When we run the fixef() function, however, now have both a slopes and intercept:
fixef(raneff_01)

# When we run the coef() function, you can see that each person has a unique 
# intercept, but everyone has the same slope:
coef(raneff_01)$subID


# As before, let's make a smaller dataset with the predictions for these 10 people:
PRED<-data.frame(subID=c("s01","s02","s03","s04","s05","s06"),
                 Intercepts=c(coef(raneff_01)$subID[c(1:6),1]),
                 Slopes=c(coef(raneff_01)$subID[c(1:6),2]))
PRED

# We can now plot the predictions for each person, note that although each person
# has a different intercept, they all have the same slope. 
g1<-ggplot(first06, aes(x = time, y = score)) +
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~subID, ncol=2)
g2<-g1+scale_x_continuous(name = "Time (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Score (0-100)",limits=c(0,100))
g3<-g2+geom_abline(aes(intercept=Intercepts, slope=Slopes), col="red", lwd=1.5, PRED)
plot(g3)






# Random Slopes and Intercepts Model ----
raneff_02<-lmer(score~
                  # Fixed-effects
                  1+time+
                  # Random-effects
                  (1+time|subID), data=DATA, REML=FALSE)
summary(raneff_02)

# Now when we run the ranef() function, there is random effect for both the 
# slope and the intercept:
ranef(raneff_02)

# When we run the fixef() function, it returns the group-level intercept and slope:
fixef(raneff_02)

# When we run the coef() function, the random-effects (deviates) are combined
# with the group-level fixed-effects to get a unique slope and intercept 
# for each person:
coef(raneff_02)$subID


# As before, let's make a smaller dataset with the predictions for these 10 people:
PRED<-data.frame(subID=c("s01","s02","s03","s04","s05","s06"),
                 Intercepts=c(coef(raneff_02)$subID[c(1:6),1]),
                 Slopes=c(coef(raneff_02)$subID[c(1:6),2]))
PRED

# Now we can plot the predictions for each person, note that each person has a 
# different intercept and slope. 
g1<-ggplot(first06, aes(x = time, y = score)) +
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~subID, ncol=2)
g2<-g1+scale_x_continuous(name = "Time (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Score (0-100)",limits=c(0,100))
g3<-g2+geom_abline(aes(intercept=Intercepts, slope=Slopes, col=subID), lwd=1.5, PRED)
plot(g3)





## --------------- Unconditional and Conditional Models ------------------------
# Creating our Time variable ---------------------------------------------------

summary(DATA$time)
# Note the wide range in the month variable (1-18). 
# There are two issues with this:
# 1. Time starts at 1 instead of 0
# 2. This scale is a lot larger than other variables we might want to include.

# To address these issues, we will convert Months to Years and try "centering"
# the time variable in different locations. 

# The first time variable, year0 will set the first assessment equal to 0
DATA$year<-DATA$time/12
DATA$year.0<-DATA$year-min(DATA$year)
summary(DATA$year.0)

# Next, we will create a mean centered time variable, year.c:
mean(DATA$year)
DATA$year.c<-DATA$year-mean(DATA$year)
summary(DATA$year.c)

# Let's look at the effects these two different time variables have on the 
# fixed-effects and random-effects. 

# Time 0 = the first time time point ----
time_00<-lmer(score~
            # Fixed-effects
            1+year.0+
            # Random-effects
            (1+year.0|subID), data=DATA, REML=FALSE)
summary(time_00)

# Time 0 = the mean time of 0.79 years ----
time_01<-lmer(score~
                # Fixed-effects
                1+year.c+
                # Random-effects
                (1+year.c|subID), data=DATA, REML=FALSE)
summary(time_01)

# Compare the fixed-effects of the intercept and the slope between the two models.
# Compare the random-effects between the two models.


# Centering the time-variable on the first time point makes a lot of sense 
# conceptually, but it is important to remember what that means when interpreting 
# other variables. 

# As you might recall from multiple regression in previous courses, when 
# interaction terms are included in a model, the effect of one variable is 
# being interpreted when the other variable is equal to 0. 
# Thus, if we had year.0 in our model, that would mean the effects of other 
# variables are being evaluated when time = 0, at the initial assessment. 
# Conversely, if we use year.c in our model, that would mean the effect of other
# variables are being evaluated when time = 0, which is at 0.79 yrs (or 9.5 months).


## Conditional Model: Does the slope change as a function of AIS grade? ----
# With our new variable of time in years, we want to see if we can explain the
# residual variation in subject's slopes and intercepts.


## Comparing Linear and Curvilinear Effects of Time ----------------------------
DATA$year.0_sq<-DATA$year.0^2
DATA$year.0_cu<-DATA$year.0^3

# Effect of Grade on Cubic Time
cond_03<-lmer(score~
                # Fixed-effects
                1+year.0*Group+year.0_sq*Group+year.0_cu*Group+
                # Random-effects
                (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)

Anova(cond_03, type="III")
summary(cond_03)


##################################
#ezANOVA() using the {ez} package#
##################################
head(DATA)
DATA$time_cat<-as.factor(DATA$time)
summary(DATA$Group)
mod_ANOVA <- ezANOVA(data = DATA,
                  dv = .(score), 
                  wid = .(subID),
                  within = .(time_cat),
                  between =.(Group),
                  type = 3,
                  detailed = TRUE,
                  return_aov = TRUE)
mod_ANOVA




## ------------------- Visualizing Missing Data --------------------------------
## Scores with complete data ---------------------------------------------------
g1<-ggplot(DATA, aes(x = time, y = score)) +
  geom_point(aes(fill=as.factor(subID)), pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~Group)
g2<-g1+scale_x_continuous(name = "Time (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Score (0-100)",limits=c(0,100))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position="none")

plot(g3)



## Scores with data missing random ---------------------------------------------
g1<-ggplot(DATA, aes(x = time, y = score_MAR)) +
  geom_point(aes(fill=as.factor(subID)), pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~Group)
g2<-g1+scale_x_continuous(name = "Time (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Score (0-100)",limits=c(0,100))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position="none")

plot(g3)






## Scores with data missing not at random --------------------------------------
g1<-ggplot(DATA, aes(x = time, y = score_MNAR)) +
  geom_point(aes(fill=as.factor(subID)), pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~Group)
g2<-g1+scale_x_continuous(name = "Time (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Score (0-100)",limits=c(0,100))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position="none")

plot(g3)


## Scores with Last Observation Carried Forward --------------------------------
g1<-ggplot(DATA, aes(x = time, y = score_LOCF)) +
  geom_point(aes(fill=as.factor(subID)), pch=21, size=2, stroke=1.25) +
  geom_line(aes(group=subID)) +
  facet_wrap(~Group)
g2<-g1+scale_x_continuous(name = "Time (Months)", breaks=c(0:18)) +
  scale_y_continuous(name = "Score (0-100)",limits=c(0,100))
g3 <- g2 + theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+
  theme(legend.position="none")

plot(g3)



## -------------- The Effects of Missingness on Time ---------------------------
# Cubic model with complete data
complete<-lmer(score~
                 # Fixed-effects
                 1+year.0*Group+year.0_sq*Group+year.0_cu*Group+
                 # Random-effects
                 (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)

comp_ANOVA <- ezANOVA(data = DATA,
                     dv = .(score), 
                     wid = .(subID),
                     within = .(time_cat),
                     between =.(Group),
                     type = 3,
                     detailed = TRUE,
                     return_aov = TRUE)
comp_ANOVA

# Cubic model with data Missing at Random
MAR<-lmer(score_MAR~
            # Fixed-effects
            1+year.0*Group+year.0_sq*Group+year.0_cu*Group+            
            # Random-effects
            (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)


# Cubic model with data Missing Not at Random
MNAR<-lmer(score_NMAR~
             # Fixed-effects
             1+year.0*Group+year.0_sq*Group+year.0_cu*Group+             
             # Random-effects
             (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)



# Cubic model with Last Observation Carried Forward
LOCF<-lmer(score_LOCF~
             # Fixed-effects
             1+year.0*Group+year.0_sq*Group+year.0_cu*Group+             
             # Random-effects
             (1+year.0+year.0_sq+year.0_cu|subID), data=DATA, REML=FALSE)

LOCF_ANOVA <- ezANOVA(data = DATA,
                      dv = .(score_LOCF), 
                      wid = .(subID),
                      within = .(time_cat),
                      between =.(Group),
                      type = 3,
                      detailed = TRUE,
                      return_aov = TRUE)
LOCF_ANOVA



summary(complete)
summary(MAR)
summary(MNAR)
summary(LOCF)










