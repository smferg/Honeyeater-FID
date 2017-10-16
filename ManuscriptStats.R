data <- read.csv("final.csv")
library("ggplot2")
library(car)

# Question #2: Does honeyeater escape method change throughout the day?

# DV: FIDm

# IV: ToD2 (ToD squared), ToD
# Covariate: SDm

hist(data$FIDm) # Data are roughly normally distributed

data$PlotTime=as.POSIXct(data$RawTime,format="%H:%M",tz="Australia/Perth")
# Use it to graph - it makes cleaner, more intuitive graphs using a 24:00 clock

plot(FIDm ~ ToD, data = data) #Fixed factor
plot(FIDm ~ SDm, data = data) #Covariate

# Note that we hypothesized an inverted U-shape for our relationship with time so first 
# check whether a linear or quadratic version (y ~ x vs. y~x^2+x) fit the data better.

Time_quad <- lm(FIDm ~ ToD2 + ToD, data = data)
Time_linear <- lm(FIDm ~ ToD, data = data)

summary(Time_quad)
#Residual standard error: 2.434 on 83 degrees of freedom
#Multiple R-squared:  0.3455,	Adjusted R-squared:  0.3298 
#F-statistic: 21.91 on 2 and 83 DF,  p-value: 2.287e-08
summary(Time_linear)
#Residual standard error: 2.774 on 84 degrees of freedom
#Multiple R-squared:  0.1394,	Adjusted R-squared:  0.1292 
#F-statistic: 13.61 on 1 and 84 DF,  p-value: 0.0003989

# Compare the models using a Chi Sq likelihood ratio test
anova(Time_linear,Time_quad, test = "Chisq")
#  Res.Df    RSS Df Sum of Sq  Pr(>Chi)    
#1     84 646.59                           
#2     83 491.72  1    154.87 3.173e-07 ***

# Our models are significantly different. As indicated by the R^2s and lower RSS, model #2 - the 
# quadratic version - better fits the data. While we COULD compare it to a cubic polynomial 
# (i.e. x^3), or even higher order polynomials, that isn't what we predicted or what the data appear 
# to show, and we run the risk of overfitting our data.

# Add a covariate, SDm:
Full_quad <- lm(FIDm ~ ToD2*SDm+ToD*SDm, data = data)
summary(Full_quad)
#Residual standard error: 2.402 on 80 degrees of freedom
#Multiple R-squared:  0.3858,	Adjusted R-squared:  0.3474 
#F-statistic: 10.05 on 5 and 80 DF,  p-value: 1.728e-07

# Per Engvist (2006), we should eliminate nonsignficant interaction terms:
Quad_no_polyint <- lm(FIDm ~ ToD2 + ToD*SDm, data = data)
Quad_no_int <- lm(FIDm ~ ToD2*SDm+ToD, data = data)

# Chi Sq likelihood ratio test:
anova(Quad_no_polyint, Full_quad, test = "Chisq")
#  Res.Df    RSS Df Sum of Sq Pr(>Chi)
#1     81 461.56                      
#2     80 461.49  1  0.066702   0.9144

anova(Quad_no_int, Full_quad, test = "Chisq")
#  Res.Df    RSS Df Sum of Sq Pr(>Chi)
#1     81 461.77                      
#2     80 461.49  1   0.27387   0.8275

# Neither is significant, so our final model contains only the main effects from our IV and covariate:

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

Anova(Final_model) #Anova() from 'car' uses Type II
# Df Sum Sq Mean Sq F value    Pr(>F)    
# ToD2      149.22  1 26.0746 2.095e-06 ***
# ToD       173.00  1 30.2306 4.239e-07 ***
# SDm        22.46  1  3.9249   0.05093 .  

# Check the assumptions of linear models: 

# Heteroscedasticity
plot(fitted(Final_model), residuals(Final_model))
# Apparent heteroscedasticity

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

# Both tests indicate heteroscedasticity in our model. We'll come back to this.
# We can check these against our predictor variables:
plot(data$ToD, residuals(Final_model))
plot(data$SDm, residuals(Final_model))
#As a rough estimate, these are being driven by measures at the end of the day and at higher SDs

# Multicollinearity
library(car)
vif(Final_model)
#                       GVIF Df GVIF^(1/(2*Df))
#      ToD2       ToD       SDm 
# 85.660634 85.823523  1.029071 
# VIF values >4-5 are  considered collinear. We are safe to ignore collinearity between related terms.
# See: https://statisticalhorizons.com/multicollinearity

# Linearity:
qqnorm(residuals(Final_model))

# Correction for heteroscedasticity
# Heteroscedasticity-Consistent Standard Error (HCSE) estimators. 
# Note, per Cleasby&Nakagawa: This does NOT eliminate heteroscedasticity in the model; however, it 
# will correct our standard errors. Other models are potentially at risk from other problems, but 
# ours meets all the other assumptions of linear models and is unlikely to be significantly affected.

# First, we need an Ordinary Least Squares regression version of the model which, conveniently, is
# what we've already done:
library(lmtest) 
#install.packages('sandwich') # So we can get the function coeftest()
library(sandwich)

# There are various estimators (Reviewed in Cleasby and Nakagawa), termed H0-H4; C&N recommend HC3, 
# or H4 if you have high-leverage data points. We'll use HC3:
library(coeftest)
coeftest(Final_model, vcov = vcovHC(Final_model, method = "HC3"))
#                Estimate Std. Error t value  Pr(>|t|)    
#  (Intercept) -16.756069   3.056684 -5.4818 4.538e-07 ***
#  ToD2         -0.127130   0.022665 -5.6090 2.675e-07 ***
#  ToD           3.422009   0.543041  6.3016 1.396e-08 ***
#  SDm           0.132597   0.079233  1.6735   0.09804 .  

# Can also get F-statistics
Anova(Final_model, white.adjust = TRUE)

#Graphing

#FID ~ Time:
ggplot(data, aes(x = PlotTime, y = FIDm)) + geom_point() + 
  geom_smooth(method=lm, formula = y~poly(x,2),se = FALSE, colour = 1)+ 
  labs(x = "Time of Day (24 hr)", y = "FID (m)") + theme_classic(base_size = 14)

ggsave("Fig1.tiff", plot = last_plot(), device = "tiff", path = NULL,
       scale = 1, width = 177, height = 177, units = c("mm"),
       dpi = 300, limitsize = TRUE)


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

# DV: Flight (0 = hop, 1 = Flight)

# IV: ToD
# Covariates: SDm FIDm

# Note that FIDm might have an effect (Rodriguez-Prieto et al, 2008), and should be considered as a
# covariate. However, FIDm is constrained by SDm (envelope effect) and should be checked for
# correlation as a test of the assumptions of linear modeling.
cor.test(data$SDm,data$FIDm)
#  t = 2.3801, df = 84, p-value = 0.01957
# sample estimates:
#  cor 0.2513538 

# SDm and FIDm are significantly correlated - thus, we should not use both of them. FIDm is 
# what Rodriguez-Prieto et al used, but we've used SDm already so I will maintain it here.

depart_cov <- glm(data = data, Flight ~ ToD*SDm, family = binomial)

# And check to see if we need to retain those interactions, per Engqvist:
depart_cov_reduced <- glm(data=data, Flight ~ ToD + SDm, family = binomial)

anova(depart_cov_reduced, depart_cov, test = 'Chisq')
#   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1        83     80.209                     
# 2        82     78.216  1   1.9927   0.1581

# Nonsignificant, remove the interaction from our final model:
depart_final <- glm(data = data, Flight ~ ToD + SDm, family = binomial)

# Check linear model assumptions. This is 'logistic modeling', so the assumptions are a bit different
# from our assumptions for vanilla linear models. I'm following the assumptions as explained by 
# http://www.statisticssolutions.com/assumptions-of-logistic-regression/:
# 1. must be binary. 
# 2. LR assumes that y=1 is the probability of something happening.
# 3. Don't overfit - we did that. This isn't really an assumption, it's just good model building.
# 4. Error terms are independent. Check - no observations are dependent on each other.
# 5. Presented with #4, the model should not be multicollinear

# Multicollinearity
vif(depart_final)
#  ToD     FIDm 
# 1.000013 1.000013  

# Statistics for the whole model.

summary(depart_final)
#     Null deviance: 97.805  on 85  degrees of freedom
# Residual deviance: 80.209  on 83  degrees of freedom
# AIC: 86.209
Anova(depart_final)

# glm() doesn't give R^2 by default. In this case, we'll use McFadden's (1973) pseudo R^2 as goodness 
# of fit: 1-[loglikelihood(model)/loglikelihood(null)]. 
# First, make a null model:
depart_null <- glm(data=data, Flight~1, family = binomial)
pseudo <- 1-logLik(depart_final)/logLik(depart_null)
pseudo
#'log Lik.' 0.1799095 (df=3)

# We can use that null model to get a p-value for the whole thing as well:
anova(depart_null,depart_final, test = 'Chisq')
#   Resid. Df Resid. Dev Df Deviance Pr(>Chi)    
# 1        85     97.805                          
# 2        83     81.236  2   16.569 0.0002524 ***

summary(depart_final)

# We can calculate the Odds Ratio. Odds ratios are a way of relating how likely one outcome or the
# other is to happen. Important to use fractional hours past midnight here (ToD; e.g. 7:30 = 7.5, 
# 14:45= 14.75); POSIXct fails.
exp(cbind(Odds_ratios = coef(depart_final), confint(depart_final)))
confint(depart_final, level = 0.95)

#Odds_ratios     2.5 %      97.5 %
# (Intercept)  0.02792353 0.00191884 0.2962391
# ToD          1.27238052 1.10650713 1.4866009
# SDm          1.15034563 0.99638704 1.3490592

#Graphs:

#give us a minimal model that only calls one variable at a time, here Time
depart_time <- glm(data = data, Flight ~ PlotTime, family=binomial)
depart_tss <- glm(data = data, Flight ~ TSS, family=binomial)

ggplot(data, aes(x= PlotTime, y = Flight)) + geom_point()+ geom_line(aes(y=fitted(depart_time))) +
  theme_classic(base_size = 14) + labs(x = "Time of Day (24 hr)", y = "Probability of escape strategy") +
  annotate(geom="text", x = as.POSIXct("2017-10-13 07:15:00"), y = .95, label= "Fly", size = 5) + 
  annotate(geom="text", x = as.POSIXct("2017-10-13 07:15:00"), y = .07, label= "Hop", size = 5)