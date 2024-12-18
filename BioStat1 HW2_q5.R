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
#5. a Calibration Curve

library(ggplot2)
library(dplyr)

di$predicted_risk <- predict(model2, di, type = "response")

# Create decile groups 
di <- di %>% mutate(decile = ntile(predicted_risk, 10))

# Calculate average predicted probabilities 
calibration_data <- di %>%
  group_by(decile) %>%
  summarise(
    mean_predicted = mean(predicted_risk),
    observed_proportion = mean(chd69)
  )

# linear regression 
calibration_model <- lm(observed_proportion ~ mean_predicted, data = calibration_data)
slope <- coef(calibration_model)[2]
intercept <- coef(calibration_model)[1]

# calibration curve
ggplot(calibration_data, aes(x = mean_predicted, y = observed_proportion)) +
  geom_point(size = 2, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Calibration Curve",
    x = "Mean Predicted Probability",
    y = "Observed Proportion"
  ) +
  theme_minimal()

cat("Calibration Slope:", slope, "\n")
cat("Calibration Intercept:", intercept, "\n")


#5.(b)  the method of Hosmer and Lemeshow

install.packages("ResourceSelection")
library(ResourceSelection)

# Predicted probabilities from model2 (logistic regression)
di$predicted_risk <- predict(model2, di, type = "response")

# Hosmer-Lemeshow test
hl_test <- hoslem.test(di$chd69, di$predicted_risk, g = 10)  # g = 10 for deciles

print(hl_test)
 
#5.(c)

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












