# 6) Decision curve analysis
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


