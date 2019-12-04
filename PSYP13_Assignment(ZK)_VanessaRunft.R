################################################################################################################
######################## R-Code for PSYP13 Home Assignments (ZK) - Vanessa Runft ###############################
################################################################################################################

# load packages

require(psych)
require(lsr)
require(tidyverse)
require(car)
require(olsrr)
require(ggpubr)
require(readxl)
require(cAIC4)
require(r2glmm)
require(lme4)
require(lmerTest)
require(MuMIn)
require(lm.beta)
require(influence.ME)
require(lattice)


# Get and view dataset 1
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")
View(data_sample_1)


#### Check for coding errors

# Descriptive statistics
str(data_sample_1)
data_sample_1 %>% summary() 

# Exclusion of subjects with coding errors
clean_data<-data_sample_1[!data_sample_1$household_income < 0,]  #exclude person with negative income
clean_data_2<-clean_data[!clean_data$STAI_trait < 20,]           #exclude person with unrealistic STAI_trait value 



######################################################################################################################
################################### ASSIGNMENT 1 #####################################################################

# Build linear models
model_1 <- lm(pain ~ sex + age, data = clean_data_2)
model_2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = clean_data_2)


##### Check assumptions for linear models

## OUTLIERS

# Cook's D
ols_plot_cooksd_bar(model_1)
ols_plot_cooksd_bar(model_2)


## MULTICOLLINEARITY

# check for variance inflation factors
vif(model_1)    
vif(model_2)       
                    
cor(clean_data_2$cortisol_serum, clean_data_2$cortisol_saliva) 

# exclude cortisol_saliva
model_2_2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = clean_data_2)


## NORMAL DISTRIBUTION OF RESIDUALS

# Histograms
windows()
hist( x = residuals( model_1 ),
      xlab = "Value of residual", 
      main = "",
      breaks = 20 
      )

windows()
hist( x = residuals( model_2_2 ),
      xlab = "Value of residual", 
      main = "",
      breaks = 20 
      )

# QQ-Plots
windows()
plot( x = model_1, which = 2 )      
plot( x = model_2_2, which = 2 )

# Shapiro-Wilk-Tests
shapiro.test(residuals(model_1))        
shapiro.test(residuals(model_2_2))


## LINEARITY OF THE RELATIONSHIP

# plots
windows()
yhat_1 <- fitted.values( object = model_1 )      
plot( x = yhat_1,
        y = clean_data_2$pain,
        xlab = "Fitted Values",
        ylab = "Observed Values"
        )

windows()
yhat_2 <- fitted.values( object = model_2_2 )
plot( x = yhat_2,
      y = clean_data_2$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values"
      )

# Curvature tests and Tukey test
windows()
residualPlots( model = model_1 )
residualPlots( model = model_2_2 )


## HOMOSCEDASTICITY

# plots
windows()
plot(x = model_1, which = 3)
windows()
plot(x = model_2_2, which = 3)

# non-constant variance test
ncvTest(model_1)               
ncvTest(model_2_2)


##### Model comparison

# Model test statistics
summary (model_1)
summary (model_2_2)

# Print model coefficients
model_2_2


## Tables

# load custom functions
coef_table = function(model){
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")
  mod_sum_table["(Intercept)","Std.Beta"] = "0"
  return(mod_sum_table)
}

# print tables
coef_table(model_1)
coef_table(model_2_2)

# adjusted R^2
summary(model_1)$adj.r.squared  
summary(model_2_2)$adj.r.squared

# Compare model fit -> Akaike information criterion
AIC(model_1)
AIC(model_2_2)

# Compare residual error -> anova
anova(model_1, model_2_2)



######################################################################################################################
################################### ASSIGNMENT 2 #####################################################################

# Initial model
model_initial <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = clean_data_2)


#### Check assumptions for linear models 

# OUTLIERS

# Cook's D
ols_plot_cooksd_bar(model_initial)  


# NORMAL DISTRIBUTION OF RESIDUALS

# Histograms
hist( x = residuals( model_initial ),
      xlab = "Value of residual", 
      main = "",
      breaks = 20 
)

# QQ-Plot
plot( x = model_initial, which = 2 )     

# Shapiro-Wilk-Test
shapiro.test(residuals(model_initial))  


# LINEARITY OF THE RELATIONSHIP

# plot
yhat_3 <- fitted.values( object = model_initial )    
plot( x = yhat_1,
      y = clean_data_2$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values"
)

# Curvature tests and Tukey test
residualPlots( model = model_initial )    


# HOMOSCEDASTICITY

# plots
windows()
plot(x = model_initial, which = 3)

# non-constant variance test
ncvTest(model_initial)             


# MULTICOLLINEARITY

# check for variance inflation factors
vif(model_initial)      

                      

#### Backward regression

step( object = model_initial,
      direction = "backward" 
      )

# backward model
model_backward <- lm(pain ~ sex + age + pain_cat + cortisol_serum + mindfulness + weight, data = clean_data_2)

coefTable(model_backward) # coefficient table


# theory-based model: Model_2_2 from assignment 1
model_theorybased <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = clean_data_2)


#### Model comparison 

# Model test statistics
summary (model_initial)
summary (model_backward)
summary (model_theorybased)

# adjusted R^2
summary(model_initial)$adj.r.squared 
summary(model_backward)$adj.r.squared     
summary(model_theorybased)$adj.r.squared

# Compare model fit -> Akaike information criterion
AIC(model_initial)
AIC(model_backward) 
AIC(model_theorybased)

# Compare residual errors of models
anova(model_initial, model_backward)


#### Test on new dataset --------------------------------------------------

# get dataset 2
data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")
View(data_sample_2)

# Descriptive statistics
str(data_sample_2)
data_sample_2 %>% summary()
             

# Predictions of models on dataset 2
data_sample_2$prediction_backward <- predict(model_backward, newdata = data_sample_2)
data_sample_2$prediction_theorybased <- predict(model_theorybased, newdata = data_sample_2)

# compare predictions with actual pain ratings
summary(data_sample_2$prediction_backward)
summary(data_sample_2$prediction_theorybased)
summary(data_sample_2$pain)
#sum of squared differences between predictions
sum((data_sample_2$pain - data_sample_2$prediction_backward)^2)       
sum((data_sample_2$pain - data_sample_2$prediction_theorybased)^2)



######################################################################################################################
################################### ASSIGNMENT 3 ####################################################################

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

# Get dataset 3
data_sample_3 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv ")
View(data_sample_3)

# asign hospital as a grouping factor
data_sample_3 %>% mutate(hospital = factor(hospital))

# data check
summary(data_sample_3)

# transform "Female" into "female"
data_sample_3B <- data_sample_3 %>% mutate(sex = droplevels(replace(sex, sex == "Female", "female")))

# exclude ID 195, as that person had 6.05 on mindfulness (highest possible score is 6.00)
data_sample_3B <- data_sample_3B[-c(195),]

# double-checking
View(data_sample_3B)
str(data_sample_3B)


# Build random intercept linear mixed model based on dataset 3

model_mixed = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness +( 1 | hospital), data = data_sample_3B)


#### Model diagnostics linear mixed model

# OUTLIERS

influence_observation <- influence(model_mixed, obs = T)$alt.fixed
influence_group <- influence(model_mixed, group = "hospital")$alt.fixed

#plot
windows ()
data_plot_influence = as_tibble(influence_group) %>% gather(colnames(influence_group), value = coefficient, key = predictor)
data_plot_influence %>% ggplot() + aes(x = 1, y = coefficient, group = predictor) + geom_violin() + facet_wrap(~predictor, scales = "free")
         

# NORMAL DISTRIBUTION OF RESIDUALS

#QQ-Plot
windows()
qqmath(residuals(model_mixed), id = 0.05)


# LINEARITY OF THE RELATIONSHIP

# scatterplot of the standardized residuals and the predicted values
windows()
plot(model_mixed, arg = "pearson")

windows()
data_sample_3B %>% ggplot() + aes(x = sex, y = residuals(model_mixed)) +
  geom_point()

windows()
data_sample_3B %>% ggplot() + aes(x = age, y = residuals(model_mixed)) +
  geom_point()

windows()
data_sample_3B %>% ggplot() + aes(x = STAI_trait, y = residuals(model_mixed)) +
  geom_point()

windows()
data_sample_3B %>% ggplot() + aes(x = pain_cat, y = residuals(model_mixed)) +
  geom_point()

windows()
data_sample_3B %>% ggplot() + aes(x = cortisol_serum, y = residuals(model_mixed)) +
  geom_point()

windows()
data_sample_3B %>% ggplot() + aes(x = mindfulness, y = residuals(model_mixed)) +
  geom_point()


# HOMOSCEDASTICITY

# plot
windows()
plot(model_mixed, arg = "pearson")

homosced_mod = lm(residuals(model_mixed)^2 ~ hospital, data = data_sample_3B)
summary(homosced_mod)


# MULTICOLLINEARITY

windows()
pairs.panels(data_sample_3B[, c("sex", "age",
                                 "STAI_trait", "pain_cat", "cortisol_serum", "mindfulness")], col = "red", lm = T)



#### Model coefficients and statistics

model_mixed
summary(model_mixed)

# compute 95% confidence interval for coefficients
confint(model_mixed)
 
# beta coefficients
stdCoef.merMod(model_mixed)

# variance explained by fixed factors
r2beta(model_mixed, method = "nsj", data = data_sample_3B)

# marginal and conditional R squared values
r.squaredGLMM(model_mixed)


#### Predict pain in new dataset

# get dataset 4
data_sample_4 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")
View(data_sample_4)

summary(data_sample_4)


# calculate explained variance in dataset 4

# TSS: create a null model
model_null <- lm(pain ~ 1, data=data_sample_4)

# Prediction
data_sample_4 <- data_sample_4 %>%
  mutate (pred_model_mixed = predict(model_mixed, newdata=data_sample_4, allow.new.levels=T)) %>%
  mutate (pred_model_null = predict(model_null, newdata=data_sample_4, allow.new.levels=T))

TSS <- sum((data_sample_4$pred_model_null - data_sample_4$pain)^2)  # total sum of squared differences
RSS <- sum((data_sample_4$pred_model_mixed - data_sample_4$pain)^2) # residual sum of squared differences

# Variance explained in new data
Rsq = 1-(RSS/TSS)
Rsq


#### Build new model

# most influencial predictor?
model_mixed

# random slope model with cortisol_serum
mod_rnd_slope = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital),
                     data = data_sample_3B)

# regression line random slope model
pred_slope = predict(mod_rnd_slope)

# plot
windows()
data_sample_3B %>% ggplot() + aes(y = pain, x = cortisol_serum,
                                    group = hospital) + geom_point(aes(color = hospital), size = 2) +
  geom_line(color = "red", aes(y = pred_slope, x = cortisol_serum)) +
  facet_wrap(~hospital, ncol = 2)



######################################################################################################################
######################################################################################################################
