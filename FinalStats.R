# Final Analysis for Honeyeater FID project
setwd("~/Desktop/Curtin Ethics/honeyeater")  #Sets our working directory
data <- read.csv("final.csv")  #puts our data from the .csv into the datafram 'data'
library("ggplot2") #loads the library ggplot 2 - we won't use this until later
head(data) #take a glimpse at headers and first rows of our data to make sure it's correct

# Q1 Does NH Honeyeater FID vary with Time of Day?
# Predictions are:
# Morning: Short
# Midday: Long
# Evening: Short

# Dependent Var: FIDm
hist(data$FIDm) # Data are roughly normally distributed

# Independent Vars: Time (as ToD and ToD2 for x^2)
# Covariate: SDm
hist(data$SDm) #Nothing unusual

# Remember, lm and glm don't treat covariates any differently, so we effectively have 2 Ind Vars here

# Time is trickier - we use a 24hr clock so we need to convert it to the continuous running format R
# maintains, starting with 1/1/1970, I think:
data$PlotTime=as.POSIXct(data$RawTime,format="%H:%M",tz="Australia/Perth")
# Actually, as it turns out, this works and it doesn't. For our actual modeling, we're going to use
# Time since midnight as fractions of hours, e.g. 7:30 am = 7.5, 5pm = 17.0. The ratios and 
# relationships are the same, but the large scale from POSIXct being a continuous integer causes
# problems in some of the calculations, esp. with coefficients and odds ratios. Since the plotting
# works equally well with POSIXct, however, we'll use it to graph - it makes cleaner, more intuitive
# graphs using a 24:00 clock for our purposes.

# Random effects: N/A
# No random effects and a normally distributed DV means we can use a vanilla (general) linear model. 

#Lets visualize that Y ~ X relationship with a scatterplot, and also demonstrate how ToD and POSIXct
# work the same for plotting purposes:
plot(FIDm ~ ToD, data = data)
plot(FIDm ~ PlotTime, data = data)

# And for the covariate, just to check for anything unusual:
plot(FIDm ~ SDm, data = data)

# Note that we hypothesized an inverted U-shape for our relationship with time (and it looks like we 
# probably have that), so first we'll check whether a linear or quadratic version of PlotTime 
# (y ~ x vs. y~x^2+x) fit the data better.

# We can use poly(X, #) as shorthand for polynomials, which is especially useful for higher-order
# polynomials. For consistency and very slight rounding differences, however, since I've only got
# a quadratic term, I'm going to write it out. Also makes interaction terms a little more obvious
# as we test them. We'll ignore SDm for the time being. 
# First, make models for a quadratic term and a linear term:
Time_quad <- lm(FIDm ~ ToD2 + ToD, data = data)
Time_linear <- lm(FIDm ~ ToD, data = data)

# Let's sneak a peak at how our basic models perform:
summary(Time_quad)
#Residual standard error: 2.434 on 83 degrees of freedom
#Multiple R-squared:  0.3455,	Adjusted R-squared:  0.3298 
#F-statistic: 21.91 on 2 and 83 DF,  p-value: 2.287e-08
summary(Time_linear)
#Residual standard error: 2.774 on 84 degrees of freedom
#Multiple R-squared:  0.1394,	Adjusted R-squared:  0.1292 
#F-statistic: 13.61 on 1 and 84 DF,  p-value: 0.0003989

# Great! Both models are significant, but we have a clear winner: The quadratic (polynomial) version 
# of the model has an adj R^2 of 0.33, vs just 0.13 for the monomial version.

# To verify this, let's compare the models using a Chi Sq likelihood ratio test, using the anova() 
# function. In general, it's best practice to put the more basic (i.e., fewer terms) model first;
# however, it does NOT change the ultimate result - though it might change some signs. If you end up
# with negative Df and Sum of Sq values, just multiply by -1, or re-run your anova() with the model 
# order switched. Note too that we need to specify the test type so we get an appropriate output:

anova(Time_linear,Time_quad, test = "Chisq")
#  Res.Df    RSS Df Sum of Sq  Pr(>Chi)    
#1     84 646.59                           
#2     83 491.72  1    154.87 3.173e-07 ***

# Great again! Verifying what we see above, our models are significantly different. As indicated by
# the R^2s and lower RSS, model #2 - the quadratic version - better fits the data. While
# we COULD compare it to a cubic polynomial (i.e. x^3), or even higher order polynomials, that isn't 
# what we predicted or what the data appear to show, and we run the risk of overfitting our data.

# Now that we know our IV has a quadratic relationship with the data, it's time to bring in our
# covariate, SDm. First make a full quadratic model with the covariate, and check the stats:

Full_quad <- lm(FIDm ~ ToD2*SDm+ToD*SDm, data = data)
summary(Full_quad)
#Residual standard error: 2.402 on 80 degrees of freedom
#Multiple R-squared:  0.3858,	Adjusted R-squared:  0.3474 
#F-statistic: 10.05 on 5 and 80 DF,  p-value: 1.728e-07

# Excellent, we have a significant model - but we're not done yet. Per Engvist (2006), we should 
# eliminate nonsignficant interaction terms. Lets test them, one at a time:

Quad_no_polyint <- lm(FIDm ~ ToD2 + ToD*SDm, data = data)

# Now, another Chi Sq likelihood ratio test:
anova(Quad_no_polyint, Full_quad, test = "Chisq")
#  Res.Df    RSS Df Sum of Sq Pr(>Chi)
#1     81 461.56                      
#2     80 461.49  1  0.066702   0.9144

# Our full model and model missing the least significant interaction do not differ! Let's drop the
# next interaction, and check it:

Quad_no_int <- lm(FIDm ~ ToD2*SDm+ToD, data = data)
anova(Quad_no_int, Full_quad, test = "Chisq")
#  Res.Df    RSS Df Sum of Sq Pr(>Chi)
#1     81 461.77                      
#2     80 461.49  1   0.27387   0.8275

# Also not significant. This means our final model contains only the main effects from our IV and 
# covariate. Let's make it official:

Final_model <- lm(FIDm ~ ToD2 + ToD + SDm, data = data)
summary(Final_model)
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -16.75607    3.61563  -4.634 1.33e-05 ***
#ToD2         -0.12713    0.02490  -5.106 2.09e-06 ***
#ToD           3.42201    0.62238   5.498 4.24e-07 ***
#SDm           0.13260    0.06693   1.981   0.0509 .  
#---
#Residual standard error: 2.392 on 82 degrees of freedom
#Multiple R-squared:  0.3754,	Adjusted R-squared:  0.3526 
#F-statistic: 16.43 on 3 and 82 DF,  p-value: 1.893e-08

anova(Final_model)
#           Df Sum Sq Mean Sq F value    Pr(>F)    
#ToD2       1  77.94  77.938 13.6192 0.0004017 ***
#ToD        1 181.67 181.673 31.7465 2.406e-07 ***
#SDm        1  22.46  22.461  3.9249 0.0509273 .  
#Residuals 82 469.25   5.723

# So overall, our quadratic and linear terms for time are significant, and our covariate has
# a nonsignficant trend. We have our final model, and all the stats we need. Lets check and make sure
# our model meets all the appropriate assumptions of linear models: 

# Heteroscedasticity (say this 3x fast into a mirror at midnight and Ronald Fisher 
# appears and hurls racial epithets at you)

plot(fitted(Final_model), residuals(Final_model))
# This could maybe be problematic... Lets do a check with a Breush-Pagan Test and Non-Constant Variance (NCV) test:
# Slightly more here: https://datascienceplus.com/how-to-detect-heteroscedasticity-and-rectify-it/

#install.packages('lmtest')
library(lmtest)
bptest(Final_model)
#studentized Breusch-Pagan test
#BP = 22.954, df = 3, p-value = 4.129e-05

#install.packages('car')
library(car)
ncvTest(Final_model)
#Non-constant Variance Score Test 
#Chisquare = 8.859196    Df = 1     p = 0.00291616  

# Both tests indicate heteroscedasticity in our model, which is problematic. We'll come back to this.
# It's worth pointing out that we can check these against our predictor variables:
plot(data$ToD, residuals(Final_model))
plot(data$SDm, residuals(Final_model))
#So, as a rough estimate, these are being driven by measures at the end of the day and at higher SDs

# Multicollinearity is when one or more factors linearly predicts others. It is bad:
library(car)
vif(Final_model)
#                       GVIF Df GVIF^(1/(2*Df))
#      ToD2       ToD       SDm 
# 85.660634 85.823523  1.029071 
# VIF values >4-5 are  considered collinear, so we're safe - wait what? But we have values in the 
# 80s! Except... it's from a term that is the addition of a power! We are safe to ignore it.
# See: https://statisticalhorizons.com/multicollinearity

# Regular linearity:
qqnorm(residuals(Final_model))
# Boom. Looking good.

###############################
###############################
# NOTE: This Did not work. Jump to the next double hashes. Don't know if I applied
# incorrectly, but the next method is far simpler and also acceptable
###############################
###############################

# OK. Back to the fact that we have violate the assumption of homoscedasticity. Go read: 
# Cleasby, I.R. & Nakagawa, S. (2011) Neglected biological patterns in the residuals: a behavioural 
# ecologist’s guide to co-operating with heteroscedasticity. Behav. Ecol. Sociobiol., 65, 2361 –2372.
# Per their recommendation, let's try tackling this with a generalised least squares regression
# using gls() from the package 'nlme':
#install.packages('nlme')
#library(nlme)

# The same model, as we had it, assuming homoscedasticity:
#homosc_model <- gls(FIDm ~ poly(PlotTime, 2) + SDm, data = data)

# A new model, allowing the variance to differ with SDm and over PlotTime:
#hetero_model <- gls(FIDm ~ poly(PlotTime,2) + SDm, weights = varIdent(form= ~1|PlotTime*SDm), data = data)

#Compare the models:
#anova(homosc_model, hetero_model)

#OK, this appears not to have worked. For one, the weighted model is far worse!
# Lets look at the new residuals plots to see what's going on. The 'homoscedastic' model should look
# the same as the lm() version, while the heteroscedastic (weighted mean squares) version would
# hopefully have reduced heteroscedasticity/be homoscedastic
#plot(fitted(homosc_model), residuals(homosc_model))
#plot(fitted(hetero_model), residuals(hetero_model))

# The 'homoscedastic' model looks the same, but the weighted mean squares version has not improved.

###############################
###############################
# Resume your regularly scheduled stats
###############################
###############################

# Next, we'll try the second method suggested by Cleasby and Nakagawa: Heteroscedasticity-Consistent
# Standard Error (HCSE) estimators (Pinheiro and Bates, 2000???). Note, per Cleasby&Nakagawa: This does
# NOT eliminate heteroscedasticity in the model; however, it will correct our standard errors. Other
# models are potentially at risk from other problems, but ours meets all the other assumptions of
# linear models and is unlikely to be significantly affected.

# First, we need an Ordinary Least Squares regression version of the model which, conveniently, is
# what we've already done:
library(lmtest) # I've already loaded it in this workthrough, but it doesn't hurt...
#install.packages('sandwich') # So we can get the function coeftest()
library(sandwich)

Final_model
summary(Final_model)

# Confirm (again, I've already run this, but for the sake of completion...) heteroscedasticity via
# the Breusch-Pagan Test:

bptest(Final_model) # note that when reporting, BP = ChiSq
#BP = 22.954, df = 3, p-value = 4.129e-05

# Yep, just a lot heteroscedastic. Let's try and correct with the HCSE estimators. There are various
# estimators (Reviewed in Cleasby and Nakagawa), termed H0-H4; C&N recommend HC3, or H4 if you have
# high-leverage data points. We'll use HC3:
?coeftest
coeftest(Final_model, vcov = vcovHC(Final_model, method = "HC3"))
#                Estimate Std. Error t value  Pr(>|t|)    
#  (Intercept) -16.756069   3.056684 -5.4818 4.538e-07 ***
#  ToD2         -0.127130   0.022665 -5.6090 2.675e-07 ***
#  ToD           3.422009   0.543041  6.3016 1.396e-08 ***
#  SDm           0.132597   0.079233  1.6735   0.09804 .  

summary(Final_model)
#Residual standard error: 2.392 on 82 degrees of freedom
#Multiple R-squared:  0.3754,	Adjusted R-squared:  0.3526 
#F-statistic: 16.43 on 3 and 82 DF,  p-value: 1.893e-08

anova(Final_model)
#           Df Sum Sq Mean Sq F value    Pr(>F)    
#ToD2       1  77.94  77.938 13.6192 0.0004017 ***
#ToD        1 181.67 181.673 31.7465 2.406e-07 ***
#SDm        1  22.46  22.461  3.9249 0.0509273 .  
#Residuals 82 469.25   5.723   

# Boom.

#Lets make publication-quality plots for these using ggplot2:
library(ggplot2)

#Without doing an X-Y-Z 3D plot, we can't plot two predictors on the same graph. Let's plot time:
ggplot(data, aes(x = PlotTime, y = FIDm)) + geom_point() + 
  geom_smooth(method=lm, formula = y~poly(x,2),se = FALSE, colour = 1)+ 
  labs(x = "Time of Day (24 hr)", y = "FID (m)") + theme_classic()

# And SDm:
ggplot(data, aes(x = SDm, y = FIDm)) +geom_point() + 
  geom_smooth(method=lm, formula = y~x,se = FALSE, colour = 1)+ 
  labs(x = "Start Distance (m)", y = "FID (m)") + theme_classic()

################
################
################
################
################
################
################
################
################

# Question #2: Does honeyeater escape method change throughout the day?

# DV: Flight (0 = hop, 1 = Flight; Alternatively, Flight.Behaviour is coded H/F but R has a quirk;
#   when given noninteger groups, R defaults to alphabetical order to assign the groups. Thus, F=0
#   and H = 1. It changes nothing about the stats but the order. For intuitive interpretation, I
#   prefer 0 for hop and 1 for flight, but it doesn't really matter.)

# IV: ToD
# Covariate: SDm

# STOP! What's different about these data? Plot it!
plot(Flight ~ ToD, data = data)
plot(ToD ~ Flight, data = data)

# Aha... Binomial (specifically, a Bernoulli distribution). No biggie - we can still use linear 
# models; we just need to tell it to run a little differently. This time we'll use a Generalized 
# Linear Model (GLM). The "-ized" indicates that we're working with data that use something other 
# than a normal (gaussian) distribution, which we specify by defining the family in the function 
# glm(). In this case, we have binomial, so we'll call it out with "family = binomial" - others 
# you might use include poisson, exponential, log-normal, etc. This is where the true power of
# the General Linear Model family comes into play - ANOVAs are SOL here, whereas we tweak a term
# and are off to the races. BTW - if you'd like a demonstration of how GLMs are still linear
# models, run the previous model using the glm() function instead of lm(), and specify 'family = 
# gaussian'. I'll wait. If you get stuck at any point in this, remember that ?glm will pull up the 
# operators for the function. For a little more on different distributions, see: 
# https://blog.cloudera.com/blog/2015/12/common-probability-distributions-the-data-scientists-crib-sheet/

#############
#############

#############
#############

# Just like last time, we have TSS and SDm. Again, we predicted a quadratic relationship 
# (Hop - fly - hop) over Time, so let's check that first:

depart_quad <- glm(data = data, Flight ~ ToD2 + ToD, family=binomial)
depart_linear <- glm(data = data, Flight ~ ToD, family=binomial)
summary(depart_quad)
summary(depart_linear)

anova(depart_linear,depart_quad, test = 'Chisq')
#   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1        84     83.854                     
# 2        83     82.729  1   1.1245   0.2889


# The models don't differ, so IMO we should take simpler model. glm() doesn't offer R^2 statistics
# by default, but it does offer AIC - I don't advocate for AIC for model fitting like this, but in
# case it floats your boat, AIC is ever-so-slightly lower for the linear version of the model.

# Add in our covariate, just like before:
depart_cov <- glm(data = data, Flight ~ ToD*SDm, family = binomial)

# And check to see if we need to retain that interaction, per Engqvist:
depart_cov_reduced <- glm(data=data, Flight ~ ToD + SDm, family = binomial)

anova(depart_cov_reduced, depart_cov, test = 'Chisq')
#   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1        83     80.209                     
# 2        82     78.216  1   1.9927   0.1581

# Just like last time, the interaction term doesn't add much - we should drop it and test only
# main effects. Let's make it our final model:
depart_final <- depart_cov_reduced

# Lets check our assumptions. This is 'logistic modeling', so the assumptions are a bit different
# from our assumptions for linear models. I'm following the assumptions as explained by 
# http://www.statisticssolutions.com/assumptions-of-logistic-regression/:
# 1. must be binary. Check
# 2. LR assumes that y=1 is the probability of something happening. Ours is coded as h and f, and 
#    since R defaults to alphabetical order, f=0 and h=1. Check
# 3. Don't overfit - we did that. This isn't really an assumption, it's just good model building.
# 4. Error terms are independent. Check - no observations are dependent on each other.
# 5. Presented with #4, the model should not be multicollinear

# Multicollinearity is when one or more factors linearly predicts others. It is bad:
library(car)
vif(depart_final)
#     TSS      SDm 
#1.000013 1.000013
# Remember, VIFs of 4-5 are bad. We're good.

# Now that all our assumptions are checked out, lets get statistics for the whole model.

summary(depart_final)
#     Null deviance: 97.805  on 85  degrees of freedom
# Residual deviance: 80.209  on 83  degrees of freedom
# AIC: 86.209
anova(depart_final)

# You'll notice that glm() doesn't give R^2 by default. We still ought to produce a goodness-of-fit
# measure to report; luckily, we can do it by hand. In this case, we'll use McFadden's (1973) 
# pseudo R^2: 1-[loglikelihood(model)/loglikelihood(null)]. Now, I think this can be approximated
# using the null/residual deviance reports, but we can also code it out explicitly. First, make a
# null model:
depart_null <- glm(data=data, Flight.Behaviour~1, family = binomial)
pseudo <- 1-logLik(depart_final)/logLik(depart_null)
pseudo
#'log Lik.' 0.1799095 (df=3)

# vs our approximation method:
1-80.209/97.805
# 0.179909

# So yes, within a very very tiny margin of error (due to rounding), we can code it or approximate 
# it from the deviance values. However, I'm a ditz and coding it makes explicit the df, so probably
# best to do it that way lest we forget to include something.

# We can use that null model to get a p-value for the whole thing as well:
anova(depart_null,depart_final, test = 'Chisq')
#   Resid. Df Resid. Dev Df Deviance Pr(>Chi)    
# 1        85     97.805                         
# 2        83     80.209  2   17.596 0.000151 ***

# Since this is a situation where we're looking at the odds of one thing or another happening, we
# can calculate the Odds Ratio. Odds ratios are a way of relating how likely one outcome or the
# other is to happen. Now, this became a problem when using POSIXct because it's a single, running,
# MASSIVE integer, counting up from 1/1/1970. To get things on a scale this calculation can handle,
# I've switched it around from time of day to Time Since Sunrise (ToD); for consistency with the 
# first calculation, I think I should actually do it as TSM
exp(cbind(Odds_ratios = coef(depart_final), confint(depart_final)))
confint(depart_final, level = 0.95)

            #Odds_ratios     2.5 %      97.5 %
#(Intercept)  35.8120926 3.3756512 521.1482172
#ToD           0.7859284 0.6726755   0.9037447
#SDm           0.8693040 0.7412573   1.0036261

#We want this the other way around, for interpretation's sake:
#ToD
1/0.7859284 # OR      1.272381
1/0.9037447 # CI 2.5  1.106507
1/0.6726755 # CI 97.5 1.486601

# R defaults calculating toward 0; thus, honeyeaters are increasingly less likely to hop over the
# day. We can invert these (1/x) to get a more easily interpretable value (e.g. 6x likely to fly)

# Let's make some publication graphs:

#give us a minimal model that only calls one variable at a time, here Time
depart_time <- glm(data = data, Flight ~ PlotTime, family=binomial)
depart_tss <- glm(data = data, Flight ~ TSS, family=binomial)
?theme_classic
ggplot(data, aes(x= PlotTime, y = Flight.Behaviour)) + geom_point()+ geom_line(aes(y=1+fitted(depart_time))) +
  theme_classic() + scale_y_discrete(limits = rev(levels(data$Flight.Behaviour)), labels = c("h" = "Hop", "f" = "Fly")) +
  labs(x = "Time of Day (24 hr)", y = "Departure")

# alternate option using geom_smooth:  geom_smooth(method = "glm", method.args = list(family = poisson), aes(color = "poisson"), se = FALSE)

ggplot(data, aes(x= TSS, y = Flight)) + geom_point()+ geom_line(aes(y=fitted(depart_tss))) +
  theme_classic()

SDdeparture <- glm(data=data, Flight.Behaviour ~ SDm, family = binomial)
ggplot(data, aes(x= SDm, y = Flight.Behaviour)) + geom_point() + geom_line(aes(y=1+fitted(SDdeparture))) + theme_classic()

