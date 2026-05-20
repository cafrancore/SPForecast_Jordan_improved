

data_labor <- read_dta(file.path(dataLocation,"JLMPS 2016 rep xs v1.1.dta"))



df_list <- as.list(data_labor)
label_list <- list()  

for (i in 1:(length(df_list) - 2)) {
  label <- attr(df_list[[i]], "label")
  if (!is.null(label)) {
    label_list[[i]] <- label
  } else {
    label_list[[i]] <- ""  
  }
}

DF1 <- as.data.frame(unlist(label_list))

selected_data_labor <- data_labor %>%
  dplyr::select(
    
    indiv_id = 1,
    hh_id = 2,
#    stratum = 3,
#    cluster = 4,
    weight = 5,
#    governorate_code = 26,
#    district = 29,
    
    # Employment outcomes
#    v199 = 199,   # Employed (market def.)
#    v200 = 200,   # Employed (extended def.)
#    v201 = 201,   # Employed (3-month ref.)
    v202 = 202,   # Employed (3-month extended)
    
    # Demographics
    sex = 39,
    age = 46,
#    age_group = 47,
    nationality = 42,
    relation_to_head = 49,
#    hh_type = 19,
    hh_size = 50,
    
    # Education
#    educ_years = 132,
    educ_level1 = 128,
#    educ_level2 = 129,
#    ever_school = 133,
    
    # Family background
#    father_educ = 135,
    father_emp = 136,
#    father_occ = 137,
#    mother_educ = 143,
    mother_emp = 144,
#    mother_occ = 145,
    
    # Living standards
#   wealth_score = 54,
#    wealth_decile = 55,
    wealth_quintile = 56,
    own_fridge = 91,
#   own_car = 93,
#   own_computer = 96,
#   housing_own = 80,
    water_access = 81,
#   sanitation = 82,
#   lighting = 83,
#   sewage = 84,
    
    # Geographic
    governorate = 28,
    urban_rural = 30,
    
    # Other
#    marital_status = 51,
    disability = 203,
#    forced_migrant = 44,
#    recent_migrant = 45,
#    health_center = 472,
#    child_health_visit = 473
)

glimpse(selected_data_labor)

filtered_data_labor <- selected_data_labor %>%
  filter(indiv_id == 2016) %>%        
  filter(nationality == 400) %>%
  filter(age >= 15 & age <= 64)

filtered_data_labor$disability[is.na(filtered_data_labor$disability)] <- 0

filtered_data_labor$v202[is.na(filtered_data_labor$v202)] <- 0

#filtered_data_labor$educ_years[is.na(filtered_data_labor$educ_years)] <- 0
filtered_data_labor$educ_level1[is.na(filtered_data_labor$educ_level1)] <- 0

filtered_data_labor$father_emp[is.na(filtered_data_labor$father_emp)] <- 6

filtered_data_labor$mother_emp[is.na(filtered_data_labor$mother_emp)] <- 6

#table(filtered_data_labor$v199, useNA = "ifany")
#table(filtered_data_labor$disability)
#colSums(is.na(filtered_data_labor))
#colSums(is.na(filtered_data_labor))[colSums(is.na(filtered_data_labor)) > 0]
#sum(table(data_labor$age)[as.numeric(names(table(data_labor$age))) < 18])

colSums(is.na(filtered_data_labor))

#regression_vars <- filtered_data_labor %>%
#  dplyr::select(-indiv_id, -hh_id, -weight, -nationality, -relation_to_head, -hh_size, -father_emp, -mother_emp, -own_fridge, -water_access, -urban_rural, -disability)

regression_vars <- filtered_data_labor %>%
  dplyr::select(-indiv_id, -hh_id, -weight, -nationality, -relation_to_head, -father_emp, -mother_emp, -own_fridge, -water_access, -urban_rural, -disability)


formula_probit <- as.formula(paste("v202 ~", paste(colnames(regression_vars)[!colnames(regression_vars) %in% "v202"], collapse = " + ")))

probit_model <- glm(formula_probit, data = filtered_data_labor, family = binomial(link = "probit"))

summary(probit_model)

predicted_probs <- predict(probit_model, type = "response")

model_data_labor <- model.frame(probit_model)

actual <- model_data_labor$v202

roc_obj <- roc(actual, predicted_probs)

plot(roc_obj, col = "blue", main = "ROC Curve - Probit Model")
auc(roc_obj)


predicted_class <- ifelse(predicted_probs > 0.5, 1, 0)
table(Predicted = predicted_class, Actual = actual)

conf_mat <- table(Predicted = predicted_class, Actual = actual)

TP <- conf_mat["1", "1"]
TN <- conf_mat["0", "0"]
FP <- conf_mat["1", "0"]
FN <- conf_mat["0", "1"]

accuracy <- (TP + TN) / sum(conf_mat)
sensitivity <- TP / (TP + FN)  
specificity <- TN / (TN + FP)  

cat("Accuracy:", round(accuracy, 3), "\n")
cat("Sensitivity (Recall):", round(sensitivity, 3), "\n")
cat("Specificity:", round(specificity, 3), "\n")

print(conf_mat)

# Logit
logit_model <- glm(formula_probit, data = filtered_data_labor, family = binomial(link = "logit"))
summary(logit_model)
logit_probs <- predict(logit_model, type = "response")
model_data_labor_logit <- model.frame(logit_model)
actual_logit <- model_data_labor_logit$v202
logit_pred_class <- ifelse(logit_probs > 0.5, 1, 0)
logit_conf_mat <- table(Predicted = logit_pred_class, Actual = actual_logit)

TP <- logit_conf_mat["1", "1"]
TN <- logit_conf_mat["0", "0"]
FP <- logit_conf_mat["1", "0"]
FN <- logit_conf_mat["0", "1"]

accuracy <- (TP + TN) / sum(logit_conf_mat)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)

cat("Logit Accuracy:", round(accuracy, 3), "\n")
cat("Logit Sensitivity:", round(sensitivity, 3), "\n")
cat("Logit Specificity:", round(specificity, 3), "\n")

print(logit_conf_mat)


# Add probability 
model_used_data_labor <- model.frame(logit_model)
predicted_probs <- predict(logit_model, type = "response")
model_used_data_labor$predicted_prob_employment <- predicted_probs
filtered_data_labor$row_id <- seq_len(nrow(filtered_data_labor))
model_rows <- as.numeric(rownames(model.frame(logit_model)))
filtered_data_labor$predicted_prob_employment <- NA  
filtered_data_labor$predicted_prob_employment[model_rows] <- predicted_probs

filtered_data_labor %>%
  group_by(v202) %>%
  summarise(
    count = n(),
    mean_prob = mean(predicted_prob_employment, na.rm = TRUE),
    median_prob = median(predicted_prob_employment, na.rm = TRUE),
    sd_prob = sd(predicted_prob_employment, na.rm = TRUE),
    min_prob = min(predicted_prob_employment, na.rm = TRUE),
    max_prob = max(predicted_prob_employment, na.rm = TRUE)
  )



ggplot(filtered_data_labor, aes(x = predicted_prob_employment, fill = factor(v202))) +
  geom_density(alpha = 0.5) +
  labs(title = "Predicted Employment Probability by Employment Status (v202)",
       x = "Predicted Probability",
       y = "Density",
       fill = "Actual Employment (v202)") +
  theme_minimal()

ggplot(filtered_data_labor, aes(x = factor(v202), y = predicted_prob_employment, fill = factor(v202))) +
  geom_boxplot() +
  labs(x = "Actual Employment Status (v202)", y = "Predicted Probability",
       title = "Predicted Probability vs Actual Employment") +
  theme_minimal()

print("Labor characteristics estimated")