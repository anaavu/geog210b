
setwd("~/Desktop/Anagha/UCSB/Classes/GEOG 210B")
library(readr)
SmallHHfile <- read_csv("SmallHHfile.csv")
summary(SmallHHfile)
install.packages("psych")
library(psych)
Descr <- describe(SmallHHfile) #different way to describe the data available
write.csv(Descr, file = "description_SmallHHfile.csv") #storing this description for lab report

library(xts)
library(regclass)
#creating linear regression model
linearMod <- lm(MilesPr ~ Mon + Tue + Wed + Thu + Fri+ Sat + HHVEH + HHSIZ + suburb + exurb+ rural, data=SmallHHfile)
#initial visual check of model
residuals <- resid(linearMod)
summary(residuals)
#mean of residuals 0
plot(resid(linearMod),fitted(linearMod))
#It looks like there is a trend line in the residual plot, indicating that the regression line might not have been computed correctly. The data is also definitely heteroscedastic.
plot(linearMod)
# We see that the qq-plot is definitely not normal; in fact, it is highly skewed

qf(.95,df1 = 11,df2 = 42419) #f statistic = 1.788874; amount that ind variables are able to explain variation in x

#talk about both t value and p-value

anova(linearMod)

possible_regressions(linearMod)
check_regression(linearMod)
aov
VIF(linearMod)
lmreport <- summary(linearMod)
confint(linearMod, level = 0.95)
#rmse, goodness of fit, generalization_error()
plot(residuals(linearMod))
capture.output(lmreport, file = "lmreport_MilesPr.txt")
all_correlations(SmallHHfile2)
# Equation: 19.0766418 - 0.6796353*Mon + 0.7147255*Tue + 0.9586046*Wed + 0.6662031*Thu + 3.7856796*Fri + 3.5689737*Sat + 4.8943467*HHVEH - 2.3604597*HHSIZ + 3.1183357*suburb + 6.2514926*exurb + 6.9114198*rural

linearMod2 <- lm(TrpPrs~.-HTRIPS, data=SmallHHfile)
lmreport2 <- summary(linearMod2)
capture.output(lmreport2, file="lmreport2_TrpPrs.txt")

#every statistical test: t tests, multiple r squared as an indicator, f test, z score, heteroskedacity bp test, durbin-watson test serially correlated, white's correction

#################################################################
# 2. 3 Write a short summary of the model in a similar fashion as our
# discussion in class highlighting which coefficients are significantly
# different than zeroand what they tell us.
# For the first model, interestingly, the number of miles traveled
# (per day per person in household) is not correlated with the weekdays
# Monday-Thursday, but they are correlated with Friday and Saturday.
# For example, for each day that is Friday, 3.78 more miles will be
# traveled. One can speculate that people are more likely to travel
# during the end of the workweek and early weekend, perhaps for chores
# such as grocery shopping or for short weekend trips. Additionally, 
#number of cars held by the household and number of persons in the
# household are both significant variables. For each additional car,
# 4.89 more miles will be traveled by each person in the household.
# The location of the home is also quite significant, regardless of
# whether it is suburban, rural or exurban.


# 3.2 Write a comparison summary between Model 1 and Model 2. 
#For the second model modeling TrpPrs, or number of trips taken per
#person, different days became significant - this time, Monday and Tuesday
#(and perhaps Wednesday also).  Trips per person and trips per household
#would covary, so I removed that from my model. Here, total distance
# traveled by the household, whether the household has a high income,
# HHEMP, HHSTU, HHLIC, DOW, number of cars and bikes owned, whether
# new cars were bought, CARBUY (car bought in last five years), whether
# or not they live in a single-family house, and MilesPr are very
# statistically significant. It makes sense, for instance, that TrpPrs
# and HHSIZ are negatively correlated. For each additional person in
# the household, the average number of trips per person in the household
# would decrease (by 2.8 trips).
# I would speculate that people drive greater distances over the weekend
# (perhaps weekend trips and such) whereas people take more trips early
# in the week (for instance for work).