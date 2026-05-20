# --- binaries & base vars ---
#2.42	2025 JOD see Hassan file based on world bank info. File in Input

pov_rate=2.42
data <- cleanData %>%
  mutate(
    old_member_bin         = as.integer(`HH 65y or more` >= 1),
    disab_member_bin       = as.integer(`HH Disabled or chronically ill` >= 1),
    HH_F_Divorced_WIDOWED  = as.integer(`HH Female Widow` >= 1)
  ) %>%
  rename(SP_program = Eligibility) %>%
  mutate(
    Value = case_when(
      SP_program == "Beneficiary" & `HH Size` == 1 ~ 40,
      SP_program == "Beneficiary" & `HH Size` > 1 & `HH Size` <= 5 ~ pmin(40 + (`HH Size` - 1) * 15, 100),
      SP_program == "Beneficiary" & `HH Size` >= 6 ~ 100,
      TRUE ~ 0
    ),
    Added_value       = as.integer(HH_F_Divorced_WIDOWED == 1 | old_member_bin == 1 | disab_member_bin == 1),
    Total_Value_CT_0  = if_else(SP_program == "Beneficiary", Value + if_else(Added_value == 1, 35, 0), Value),
    SCORE             = log(`Impute Income`/`HH Size`),
    Annual_expenditure    = exp(SCORE) * `HH Size` * 12 / 1.12,
    Monthly_expenditure   = Annual_expenditure / 12,
    Total_Value_CT_0      = replace(Total_Value_CT_0, is.na(Total_Value_CT_0), 0),
    Share_benefit_expenditure = Total_Value_CT_0 / Monthly_expenditure * 100
  )

Total_amount_to_be_paid_monthly_usd <- sum(data$Total_Value_CT_0) / (10^6) * exc_rateJOD_USD
dataCH2 <- data

# --- CH3: prep ---
data <- dataCH2 %>%
  mutate(weights = 1, match_type = SP_program) %>%
  rename(HH.Size = `HH Size`, Owns.a.car = `Owns a car`) %>%
  tidyr::drop_na(SCORE)
Pop <- nrow(data)

# --- poverty line (closest_threshold) --- 



closest_threshold <- log(pov_rate*30)
PL <- closest_threshold
data <- data %>% mutate(poor = as.integer(SCORE < PL))
Poverty <- mean(data$poor) * 100

result <- data %>%
  dplyr::group_by(SP_program) %>%
  dplyr::summarise(total = dplyr::n(), poor_count = sum(poor), .groups = "drop") %>%
  dplyr::rename(match_type = SP_program)

PL_JOD <- exp(PL); Poverty

data <- data %>%
  mutate(
    SP_program_combo = case_when(
      SP_program == "Beneficiary"  & poor == 1 ~ "Beneficiary & poor",
      SP_program == "Beneficiary"  & poor == 0 ~ "Beneficiary & non-poor",
      SP_program == "Eligible"     & poor == 1 ~ "Eligible & poor",
      SP_program == "Eligible"     & poor == 0 ~ "Eligible & non-poor",
      SP_program == "Non-Eligible" & poor == 1 ~ "Non-Eligible & poor",
      TRUE ~ "Non-Eligible & non-poor"
    ),
    SP_program_combo = factor(SP_program_combo,
                              levels = c("Beneficiary & poor","Beneficiary & non-poor",
                                         "Eligible & poor","Eligible & non-poor",
                                         "Non-Eligible & poor","Non-Eligible & non-poor"))
  )

# --- Scenario 1 ---
in_ct_set <- c("Beneficiary & poor","Beneficiary & non-poor","Eligible & poor","Non-Eligible & poor")
data <- data %>%
  mutate(
    Value_CT_1 = case_when(
      SP_program_combo %in% in_ct_set & HH.Size == 1 ~ 40,
      SP_program_combo %in% in_ct_set & HH.Size > 1 & HH.Size <= 5 ~ pmin(40 + (HH.Size - 1) * 15, 100),
      SP_program_combo %in% in_ct_set & HH.Size >= 6 ~ 100,
      TRUE ~ 0
    ),
    Total_Value_CT_1 = if_else(
      SP_program_combo %in% in_ct_set,
      Value_CT_1 + if_else(Added_value == 1, 35, 0),
      Value_CT_1
    ),
    Share_benefit_expenditure_CT_1 = Total_Value_CT_1 / Monthly_expenditure * 100
  )

result_2 <- data %>%
  group_by(SP_program_combo) %>%
  summarise(count = n(), Freq = (sum(weights, na.rm = TRUE)/Pop) * 100, .groups = "drop")

result_3 <- data %>%
  group_by(Total_Value_CT_1) %>%
  summarise(
    Monthly_AVG_EXP_JOD           = sum(Monthly_expenditure, na.rm = TRUE) / n(),
    Monthly_AVG_BENEFIT_SHARE_JOD = sum(Share_benefit_expenditure_CT_1 * weights, na.rm = TRUE) / n(),
    AVG_HH_SIZE                   = sum(HH.Size) / n(),
    FREQUENCY                     = n()/Pop * 100,
    .groups = "drop"
  )

print(result_3)
Total_amount_to_be_paid_monthly_usd_CT1 <- sum(data$Total_Value_CT_1) / (10^6) * exc_rateJOD_USD

# --- Inflation shock ---
PL_JOD_new <- PL_JOD * (1 + shock_inflation/100)
PL_new <- log(PL_JOD_new)

data <- data %>%
  mutate(
    poor_new = as.integer(SCORE < PL_new),
    DELTA    = poor_new - poor
  )

Poverty_new <- mean(data$poor_new) * 100; Poverty_new

result_new <- data %>%
  group_by(SP_program) %>%
  summarise(total = n(), poor_count = sum(poor_new), .groups = "drop") %>%
  rename(combination = SP_program)

data <- data %>%
  mutate(
    SP_program_combo_new = case_when(
      SP_program == "Beneficiary"  & poor_new == 1 ~ "Beneficiary & poor",
      SP_program == "Beneficiary"  & poor_new == 0 ~ "Beneficiary & non-poor",
      SP_program == "Eligible"     & poor_new == 1 ~ "Eligible & poor",
      SP_program == "Eligible"     & poor_new == 0 ~ "Eligible & non-poor",
      SP_program == "Non-Eligible" & poor_new == 1 ~ "Non-Eligible & poor",
      TRUE ~ "Non-Eligible & non-poor"
    ),
    SP_program_combo_new = factor(SP_program_combo_new, levels = levels(SP_program_combo))
  )

# --- Scenario 2 ---
data <- data %>%
  mutate(
    Value_CT_2 = case_when(
      SP_program_combo_new %in% in_ct_set & HH.Size == 1 ~ 40,
      SP_program_combo_new %in% in_ct_set & HH.Size > 1 & HH.Size <= 5 ~ pmin(40 + (HH.Size - 1) * 15, 100),
      SP_program_combo_new %in% in_ct_set & HH.Size >= 6 ~ 100,
      TRUE ~ 0
    ),
    Total_Value_CT_2 = if_else(
      SP_program_combo_new %in% in_ct_set,
      Value_CT_2 + if_else(Added_value == 1, 35, 0),
      Value_CT_2
    ),
    Share_benefit_expenditure_CT_2 = Total_Value_CT_2 / Monthly_expenditure * 100
  )

result_5 <- data %>%
  group_by(Total_Value_CT_2) %>%
  summarise(
    Monthly_AVG_EXP_JOD           = sum(Monthly_expenditure, na.rm = TRUE) / n(),
    Monthly_AVG_BENEFIT_SHARE_JOD = sum(Share_benefit_expenditure_CT_2, na.rm = TRUE) / n(),
    AVG_HH_SIZE                   = sum(HH.Size, na.rm = TRUE) / n(),
    FREQUENCY                     = n()/Pop * 100,
    .groups = "drop"
  )


Total_amount_to_be_paid_monthly_usd_CT_2 <- sum(data$Total_Value_CT_2) / (10^6) * exc_rateJOD_USD

data <- data %>%
  mutate(
    PL_monthly_JOD = case_when(
      HH.Size <= 5 ~ 100 * HH.Size,
      HH.Size <= 9 ~ 500 + (HH.Size - 5) * 50,
      TRUE ~ 750
    ),
    poor_test = as.integer(Monthly_expenditure < PL_monthly_JOD)
  )
Poverty_test <- mean(data$poor_test) * 100

result_1a <- data %>%
  group_by(SP_program) %>%
  summarise(
    count = n(),
    Freq  = n()/Pop * 100,
    value_monthly_usd_CT = sum(Total_Value_CT_0) / (10^6) * exc_rateJOD_USD,
    .groups = "drop"
  )

result_2 <- data %>%
  group_by(SP_program_combo) %>%
  summarise(
    count = n(),
    Freq  = n()/Pop * 100,
    value_monthly_usd_CT = sum(Total_Value_CT_1) / (10^6) * exc_rateJOD_USD,
    .groups = "drop"
  )

result_4 <- data %>%
  group_by(SP_program_combo_new) %>%
  summarise(
    count = n(),
    Freq  = n()/Pop * 100,
    value_monthly_usd_CT = sum(Total_Value_CT_2) / (10^6) * exc_rateJOD_USD,
    .groups = "drop"
  )

table1 <- full_join(
  result_2, result_4,
  by = c("SP_program_combo" = "SP_program_combo_new"),
  suffix = c("_pre", "_post")
) %>%
  mutate(
    count_diff                 = count_post - count_pre,
    FREQUENCY_diff            = Freq_post - Freq_pre,
    value_monthly_usd_CT_diff = value_monthly_usd_CT_post - value_monthly_usd_CT_pre
  )
print("table1")
print(table1)
Total_amount_to_be_paid_monthly_usd_CT0 <- sum(data$Total_Value_CT_0 * data$weights) / (10^6) * exc_rateJOD_USD
Total_amount_to_be_paid_monthly_usd_CT1 <- sum(data$Total_Value_CT_1 * data$weights) / (10^6) * exc_rateJOD_USD
Total_amount_to_be_paid_monthly_usd_CT2 <- sum(data$Total_Value_CT_2 * data$weights) / (10^6) * exc_rateJOD_USD

BeneficiariesData$SCORE   <- log(BeneficiariesData$Imputed_Income   / BeneficiariesData$HH_Size)
EligiblesData$SCORE       <- log(EligiblesData$Imputed_Income       / EligiblesData$HH_Size)
Non_eligiblesData$SCORE   <- log(Non_eligiblesData$Imputed_Income   / Non_eligiblesData$HH_Size)

combined_data <- data.frame(
  SCORE   = c(BeneficiariesData$SCORE, EligiblesData$SCORE, Non_eligiblesData$SCORE),
  Dataset = rep(c("Beneficiaries","Eligibles","Non- Eligibles"),
                times = c(length(BeneficiariesData$SCORE),
                          length(EligiblesData$SCORE),
                          length(Non_eligiblesData$SCORE)))
)

dataCH3 <- data
