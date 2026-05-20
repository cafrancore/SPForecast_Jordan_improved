

# Model 1 - VAR ##### 
root_dir        <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
userLocation    <- root_dir
inputLocation   <- file.path(root_dir, "Input")
dataLocation    <- file.path(root_dir, "Data")
outputLocation  <- file.path(root_dir, "New_Output")
data_macro <- read_excel(file.path(dataLocation,"data_jor.xlsx"), sheet = "Data_jor")
data_macro <- data_macro[(5:nrow(data_macro)),]
variables <- c("GDP", "Expenditure", "GDP_capita", "Inflation", "Unemployment", "Investment")

data_macro[variables] <- lapply(data_macro[variables], as.numeric)

data_macro_long <- pivot_longer(data_macro, cols = all_of(variables), names_to = "Variable", values_to = "Value")

ggplot(data_macro_long, aes(x = Year, y = Value)) +
  geom_line(color = "steelblue") +
  facet_wrap(~Variable, scales = "free_y") +
  labs(title = "Time Series of Economic Indicators",
       x = "Year", y = "Value") +
  theme_minimal()


results_pp_level <- data.frame(
  Variable = variables,
  PP_Test_Statistic = NA,
  Critical_5pct = -3.45,
  Conclusion = NA,
  stringsAsFactors = FALSE
)




for (i in seq_along(variables)) {
  var <- variables[i]
  var_data <- data_macro[[var]]
  
  pp_res_level <- pp.test(var_data, alternative = "stationary", type = "Z(t_alpha)")
  
  results_pp_level$PP_Test_Statistic[i] <- as.numeric(pp_res_level$statistic)
  results_pp_level$Conclusion[i] <- ifelse(results_pp_level$PP_Test_Statistic[i] < results_pp_level$Critical_5pct[i], "Stationary", "Non-stationary")
}



cat("Phillips-Perron Test Results - Levels:\n")
print(results_pp_level)

growth_vars <- c("GDP", "Expenditure", "GDP_capita", "Investment")
level_vars <- c("Unemployment", "Inflation")


log_diff_data_macro <- data.frame(Year = data_macro[[1]])
for (var in growth_vars) {
  log_diff_data_macro[[paste0("dlog_", var)]] <- c(NA, diff(log(data_macro[[var]])))
}

log_diff_data_macro$Unemployment <- data_macro$Unemployment
log_diff_data_macro$Inflation <- data_macro$Inflation

log_diff_data_macro <- log_diff_data_macro %>%
  mutate(diff_Unemployment = Unemployment - lag(Unemployment))

log_diff_data_macro <- log_diff_data_macro %>%
  mutate(diff_Inflation = Inflation - lag(Inflation))

log_diff_data_macro <- na.omit(log_diff_data_macro)

log_diff_data_macro$diff_Unemployment[is.na(log_diff_data_macro$diff_Unemployment)] <- 0

variables <- c("dlog_GDP", "dlog_Expenditure", "dlog_GDP_capita", "dlog_Investment", "diff_Unemployment","diff_Inflation")

results_pp_level <- data.frame(
  Variable = variables,
  PP_Test_Statistic = NA,
  Critical_5pct = -3.45,
  Conclusion = NA,
  stringsAsFactors = FALSE
)


data_macro <- log_diff_data_macro

for (i in seq_along(variables)) {
  var <- variables[i]
  var_data <- data_macro[[var]]
  
  pp_res_level <- pp.test(var_data, alternative = "stationary", type = "Z(t_alpha)")
  
  results_pp_level$PP_Test_Statistic[i] <- as.numeric(pp_res_level$statistic)
  results_pp_level$Conclusion[i] <- ifelse(results_pp_level$PP_Test_Statistic[i] < results_pp_level$Critical_5pct[i], "Stationary", "Non-stationary")
}

cat("Phillips-Perron Test Results - Levels:\n")
print(results_pp_level)


log_diff_data_macro[variables] <- lapply(log_diff_data_macro[variables], as.numeric)

data_macro_long_tr <- pivot_longer(log_diff_data_macro, cols = all_of(variables), names_to = "Variable", values_to = "Value")

ggplot(data_macro_long_tr, aes(x = Year, y = Value)) +
  geom_line(color = "steelblue") +
  facet_wrap(~Variable, scales = "free_y") +
  labs(title = "Time Series of Economic Indicators",
       x = "Year", y = "Value") +
  theme_minimal()



variables <- c("dlog_Expenditure", "dlog_GDP_capita", "dlog_Investment", "diff_Unemployment","diff_Inflation")

# VAR Model
var_data_macro <- log_diff_data_macro[, variables]
lag_selection <- VARselect(var_data_macro, lag.max = 4, type = "const")

print(lag_selection$selection)


VAR_model <- VAR(var_data_macro, p = 1, type = "const")
print(summary(VAR_model))

# Diagnostics
print(serial.test(VAR_model, lags.pt = 16, type = "PT.asymptotic"))
print(roots(VAR_model))

# Impulse Response Functions
irf_plot <- irf(VAR_model, impulse = "dlog_GDP_capita", response = "diff_Unemployment", n.ahead = 10, boot = TRUE)
irf_plot <- irf(VAR_model, impulse = "dlog_Investment", response = "diff_Unemployment", n.ahead = 5, boot = TRUE)
irf_plot <- irf(VAR_model, impulse = "dlog_Expenditure", response = "diff_Unemployment", n.ahead = 5, boot = TRUE)

#plot(irf_plot)
#  sd(var_data_macro$dlog_GDP_capita)

cumulative_irf <- sum(irf_plot$irf$dlog_GDP_capita[1:3])  # Sum first 3 periods
#cumulative_effect_1pct <- (cumulative_irf / 0.1226) * increase_gdp_percap

#fevd_result <- fevd(VAR_model, n.ahead = 10)
#plot(fevd_result)

#saveRDS(cumulative_effect_1pct, file = paste0(inputLocation,"Models/Unemployment/cumulative_effect_1pct.rds"))
