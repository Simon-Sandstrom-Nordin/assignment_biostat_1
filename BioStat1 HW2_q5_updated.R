pacman::p_load(tidyverse,rms,haven,mgcv,epitools,logistf,nlpred,geepack,
               skimr,pROC,tableone,emmeans,glmtoolbox,CalibrationCurves,mice,dcurves)
data(wcgs)
library(dplyr)
wcgs <- as_tibble(wcgs)
wcgs$dibpat0f<-factor(wcgs$dibpat0,levels=0:1,label=c("B","A"))
wcgs$agegroup <- cut(wcgs$age0,breaks=c(39,45,55,60),include.lowest = T,right = FALSE)
wcgs$smoker <- ifelse(wcgs$ncigs0>0,1,0)
wcgs$smokerf <- factor(wcgs$smoker,levels=c(0,1),labels=c("No","Yes"))
wcgs$heightcm <- wcgs$height0*2.54
wcgs$weightkg <- wcgs$weight0*0.45359237
wcgs$bmi <- wcgs$weightkg / (wcgs$heightcm/100)^2
wcgs$bmicat <- cut(wcgs$bmi,breaks=c(0,25,30,40),include.lowest = T,right = FALSE)
wcgs$cholmmol <- wcgs$chol0/39
wcgs$sbp10 <- wcgs$sbp0/10
wcgs$sbpcat <- cut(wcgs$sbp0,breaks=c(0,140,240),include.lowest = T,right = FALSE)
wcgs$chd69f <- factor(wcgs$chd69,levels=c(0,1),labels=c("No","Yes"))
wcgs$cholmmol <- ifelse(wcgs$cholmmol<15,wcgs$cholmmol,NA)
d <- wcgs[, c("id", "agegroup", "age0", "cholmmol", "sbp10", "bmi", "smokerf", "arcus0", "dibpat0f", "chd69")]
#d <- wcgs %>% select(id,agegroup,age0,cholmmol,sbp10,bmi,smokerf,arcus0,dibpat0f,chd69)
di <- local({
  set.seed(154550)
  imp <- mice(d, m=1, maxit=0)
  predM<-imp$predictorMatrix
  # Leave out the ID column (the first column)
  predM[, 1] <- 0
  meth<-imp$method
  dimp <- mice(d, method= "pmm" ,m=1,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
  complete(dimp,1)
})

## 1. Table 1: The original data is d(a subset of wcgs)
CreateTableOne(data=di)
#2. Overall risk or overall rate
#a What is the outcome we are interested in? chd69
#b.What are the known risk factors for our outcome of interest? all var
#agegroup/age0
#* cholmmol
#* sbp10
#* bmi
#* smokerf
#* arcus0 
#* dibpat0f
#(c) n = 3154
#(d)overall risk or rate/ revalence of the disease in our cohort
sum(di$chd69)/nrow(di)
#3
# fit a simple logistic model
model <- glm(chd69 ~ . -id -agegroup, family=binomial, data=di)
summary(model)

model2 <- step(model, scope = c(lower = chd69 ~ 1, upper = chd69 ~ (.-id-agegroup)^2))
summary(model2)
model2$aic

di$predicted_risk <- predict(model2, data=di, type="response")

#4 (b)
myroc <- roc(chd69 ~ predicted_risk,
             data=di,
             levels=c(control=0, case=1),
             direction="<",
             auc=T,
             ci=T)
plot(myroc)
myroc$ci
max_index <- which.max(myroc$sensitivities+myroc$specificities)
myroc$thresholds[max_index]
myroc$sensitivities[max_index]
sum(di$chd69 == (di$predicted_risk>myroc$thresholds[max_index]))/nrow(di)

#(c) AUC adjusted for optimism.
library(pROC)

original_auc <- auc(myroc)# Original AUC
n_boot <- 200 # bootstrap repetitions
optimism <- numeric(n_boot)

# Bootstrap 
set.seed(123)
for (i in 1:n_boot) {
  # Resample indices
  boot_indices <- sample(1:nrow(di), replace = TRUE)
  boot_data <- di[boot_indices, ]
  
  # Fit the model 
  boot_model <- glm(chd69 ~ . -id -agegroup, family = binomial, data = boot_data)
  
  # training AUC
  boot_pred <- predict(boot_model, boot_data, type = "response")
  boot_roc <- roc(boot_data$chd69, boot_pred)
  train_auc <- auc(boot_roc)
  
  # test AUC
  test_pred <- predict(boot_model, di, type = "response")
  test_roc <- roc(di$chd69, test_pred)
  test_auc <- auc(test_roc)
  
  optimism[i] <- train_auc - test_auc
}

# Adjusted AUC
mean_optimism <- mean(optimism)
adjusted_auc <- as.numeric(original_auc) - mean_optimism

# CI for the adjusted AUC
ci_adjusted <- quantile(original_auc - optimism, probs = c(0.025, 0.975))

# Output results
cat("Original AUC:", as.numeric(original_auc), "\n")
cat("Optimism-Adjusted AUC:", adjusted_auc, "\n")
cat("95% Confidence Interval for Adjusted AUC:", ci_adjusted, "\n")
#The difference between the unadjusted and adjusted AUCs is very small.
#The model is not overly complex or overfit.
#The model's performance on the training data is a good representation of its performance on unseen data.
#The small difference between the unadjusted and adjusted AUCs 
#highlights the robustness of the logistic regression model.

#(d)
install.packages("‘caret’")
library(caret)
library(pROC)

set.seed(123)

# Create 10 folds
folds <- split(sample(seq_len(nrow(di))), rep(1:10, length.out = nrow(di)))

# Initialization
auc_values <- numeric(length(folds))

# 10-fold CV
for (i in seq_along(folds)) {
  # Split data into training and validation sets
  train_indices <- setdiff(seq_len(nrow(di)), folds[[i]])
  train_data <- di[train_indices, ]
  validation_data <- di[folds[[i]], ]
  
  # Fit the logistic regression model on training data
  cv_model <- glm(chd69 ~ . -id -agegroup, family = binomial, data = train_data)
  
  # Predict probabilities for the validation set
  pred_probs <- predict(cv_model, validation_data, type = "response")
  
  # Compute AUC for the validation set
  auc_values[i] <- auc(validation_data$chd69, pred_probs)
}

# Compute mean AUC
cv_auc <- mean(auc_values)

cat("10-Fold Cross-Validated AUC:", cv_auc, "\n")
#10-Fold Cross-Validated AUC: 0.7445533 
original_auc #0.7578
adjusted_auc #0.7501229
cv_auc #0.7445533
############################################
# The slight decrease in AUC values from 0.75 to 0.74 indicates, a slight overfitted model and the 10 K-fold cross validation gave us more reliable estiamte of the model's true performance 
#5. a Calibration Curve

library(ggplot2)
library(dplyr)

di$predicted_risk <- predict(model2, di, type = "response")

# Create decile groups 
di <- di %>% mutate(decile = ntile(predicted_risk, 10))


### (a)Modification version
### Calibration Curve. To evaluate the performance of the prediction model we have computed the AUC
#from ROC analysis, now please plot the calibration curve and report the slope and the intercept of the
#calibration curve. Use the model you chose before.

# The slope of our calibrated model is 8.699679, whic is much larger than 1. This indicates that 
#predicted risks were too extreme in the sense of underestimating for patients at high risk while overestimating for patients at low risk
#and is indicative of underfitting of the model.
# The intercept of our calibrated model is -3.342517, which means the model is overestimating the likelihood of positive outcomes.


# Fit logistic regression to evaluate calibration
calibration_logistic_model <- glm(chd69 ~ predicted_risk, family = binomial, data = di)

logistic_slope <- coef(calibration_logistic_model)[2]
logistic_intercept <- coef(calibration_logistic_model)[1]

# Predicted calibration line
di$calibration_line <- predict(calibration_logistic_model, type = "response")

# Plot 
ggplot(di, aes(x = predicted_risk, y = chd69)) +
  geom_point(alpha = 0.2, color = "blue", size = 1) +  # Scatterplot of individual points
  geom_line(aes(x = predicted_risk, y = calibration_line), color = "black", size = 1, linetype = "solid") +  # Calibration line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +  # Perfect calibration line
  labs(
    title = "Logistic Calibration Curve",
    x = "Predicted Probability",
    y = "Observed Outcome"
  ) +
  theme_minimal()

cat("Logistic Calibration Slope:", logistic_slope, "\n")
cat("Logistic Calibration Intercept:", logistic_intercept, "\n")


# Through the Hosmer-Lemeshow test(the null hypothesis is the model fits the data well) outcome, we get that p-value is 0.6796 ( >0.05), which indicates we fail to reject the null hypothesis.
#The model shows no significant lack of fit.
#5.(b)  the method of Hosmer and Lemeshow

install.packages("ResourceSelection")
library(ResourceSelection)

# Predicted probabilities from model2 (logistic regression)
di$predicted_risk <- predict(model2, di, type = "response")

# Hosmer-Lemeshow test
hl_test <- hoslem.test(di$chd69, di$predicted_risk, g = 10)  # g = 10 for deciles

print(hl_test)
 
#5.(c)
#The model using agegroup as the sole predictor has an AUC value of 0.6063. This suggests that the model has limited discriminatory power, indicating that it needs further refinement.

agegroup_model <- glm(chd69 ~ agegroup, family = binomial, data = di)
summary(agegroup_model)

# Predict probabilities
di$agegroup_predicted_risk <- predict(agegroup_model, di, type = "response")

# ROC curve and AUC
agegroup_roc <- roc(chd69 ~ agegroup_predicted_risk,
                    data = di,
                    levels = c(0, 1),
                    direction = "<")

plot(agegroup_roc, main = "ROC Curve: Agegroup Only Model")
cat("AUC for Agegroup Model:", auc(agegroup_roc), "\n")
#AUC for Agegroup Model: 0.6063021 


#Q5.c , d
###To compare the discrimination between the model we used before and the only agegroup model with a statistical test,
#we use the DeLong’s test to assess if the difference in AUC between the two models is statistically significant.
# Through the outcome shows below, we found that Z = 8.7945, which indicates that the difference in AUCs is large.
# p-value < 2.2e-16 is extremely small, meaning that the difference in AUCs is highly statistically significant.
# The 95% confidence interval (CI) is (0.118 to 0.185), which does not include 0, meaning the difference in AUCs is statistically significant.

# Predicted probabilities from both models
di$full_model_pred <- predict(model2, di, type = "response")  # Full model
di$agegroup_pred <- predict(agegroup_model, di, type = "response")  # Agegroup-only model

# ROC curves
full_model_roc <- roc(di$chd69, di$full_model_pred)
agegroup_roc <- roc(di$chd69, di$agegroup_pred)

# Compare AUCs 
roc_test <- roc.test(full_model_roc, agegroup_roc, method = "delong")
print(roc_test)

#Q5 e


# full model
plot(full_model_roc, col = "blue", lwd = 2, main = "ROC Curves: Full Model vs Agegroup-Only Model")
# agegroup-only model
plot(agegroup_roc, col = "red", lwd = 2, add = TRUE)

legend("bottomright", 
       legend = c("Full Model\n(AUC = 0.7578)", "Agegroup-Only Model\n(AUC = 0.6063)"),
       col = c("blue", "red"), 
       lwd = 2,
       cex = 0.8,  # Adjust the text size
       bty = "n") 



# 
# a)

library(dcurves) 
 
dca(chd69 ~ predicted_risk, 
    data = di, 
    thresholds = seq(0, 0.35, by = 0.01)) %>%
  plot(smooth = TRUE) +
  ggtitle("Net benefits plot for selected model, tresholds 0-0.35") + 
  theme_classic() + 
  coord_cartesian(xlim = c(0, 0.35),
                  ylim = c(0,0.082)) +
  scale_x_continuous(expand = c(0, 0)) #+ 
#scale_y_continuous(expand = c(0, 0))


dca(chd69 ~ predicted_risk, 
    data = di, 
    thresholds = seq(0.35, 0.8, by = 0.01)) %>%
  plot(smooth = TRUE)+
  ggtitle("Net benefits plot for selected model, tresholds 0.35-0.80") + 
  theme_classic() + 
  coord_cartesian(xlim = c(0.35, 0.8),
                  ylim = c(0,0.082)) +
  scale_x_continuous(expand = c(0, 0))  


# The plots show the net benefits as a function of the threshold probability, for different ranges of threshold probability. Three scenarios are compared: 
#   - treating everyone (curve "Treat All", red), 
# - treating no one (curve "Treat None", green),
# - treating according to the patients' risks estimated by our selected model (curve "predicted_risk", blue).
# 
# b) The model's net benefit curve is above the treat-all net benefit curve. The model is thus clinically beneficial.\
# Threshold probabilities such that the model is clinically beneficial are 0%-26%. For these thresholds, the model's net benefits are positive and greater than the treat-all net benefits. For higher thresholds, the model's net benefits are comparable to zero.\
# Thresholds in the second figure are not clinically relevant (ASK).
# 
# c)


#agegroup_model <- glm(chd69 ~ agegroup, family = binomial, data = di)
di$predicted_risk_agemodel <- predict(agegroup_model, data=di, type="response")

dca(chd69 ~ predicted_risk + predicted_risk_agemodel, 
    data = di, 
    thresholds = seq(0, 0.35, by = 0.01)) %>%
  plot(smooth = TRUE)+
  ggtitle("Net benefits plot, models comparison, tresholds 0-0.35") + 
  theme_classic() + 
  coord_cartesian(xlim = c(0, 0.35),
                  ylim = c(0,0.082)) +
  scale_x_continuous(expand = c(0, 0))

dca(chd69 ~ predicted_risk + predicted_risk_agemodel, 
    data = di, 
    thresholds = seq(0.35, 0.8, by = 0.01)) %>%
  plot(smooth = TRUE)+
  ggtitle("Net benefits plot, models comparison, tresholds 0.35-0.80") + 
  theme_classic() + 
  coord_cartesian(xlim = c(0.35, 0.8),
                  ylim = c(0,0.082)) +
  scale_x_continuous(expand = c(0, 0))


# The plots show the net benefits as a function of the threshold probability, for different ranges of threshold probability. Four scenarios are compared: 
#   - treating everyone (curve "Treat All", red), 
# - treating no one (curve "Treat None", green), 
# - treating according to the patients' risks estimated by our selected model (curve "predicted_risk", blue), 
# - treating according to the patients' risks estimated by the model that considers only age group (curve "predicted_risk_agemodel", purple).
# 
# The model based only on age group as a predictor is beneficial for threshold probabilities between 6% and 14% (positive net benefit above the treat-all strategy's net benefit). 
# Compared to it, our selected model (model2, considering age, cholesterol level, systolic blod pressure, 
# body mass index, smoking behavior, indicator of corneal arcus, dichotomous behavior pattern) has higher net benefit values 
# and is beneficial for a wider range of thresholds. 



#Test our model on other independent datasets from different populations or institutions, through comparing AUC and calibration curves to assess generalizability.





