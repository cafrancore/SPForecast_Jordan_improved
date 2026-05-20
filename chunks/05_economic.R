cumulative_effect_1pct <- employment_impact()

# --- Wealth proxy (PCA) ---

data3 <- data3 %>% dplyr::mutate(wealth_quintile = dplyr::ntile(Impute.Income, 5))

# --- Applicants features for unemployment model ---
varNames <- colnames(ApplicantsData_back)
case1 <- varNames %in% c("Employment_Status_Name","Education_Level_Name","Gender_Code","HH_Size","head_age","GOV_DESC")
case2 <- startsWith(varNames, "request_id")
chosenVariables <- varNames[case1 | case2]

ApplicantsData <- ApplicantsData_back %>% dplyr::select(dplyr::all_of(chosenVariables)) %>% tidyr::drop_na()
cleanDataX <- ApplicantsData %>%
  dplyr::mutate(
    v202     = as.integer(Employment_Status_Name %in% c("Business Owner","Employed","Irregular","Regular")),
    sex      = ifelse(Gender_Code == "Male", 1L, 2L),
    hh_size  = HH_Size,
    educ_level1 = dplyr::recode(as.character(Education_Level_Name),
                                "Illiterate"=1, "Read & Write"=2, "Primary"=3, "Basic Education"=3,
                                "Failed secondary"=4, "Secondary"=4, "Technical and vocational Bachelor"=5,
                                "University"=6, "Graduate studies"=7, .default = 1
    ),
    GOV_DESC_matched = dplyr::recode(as.character(GOV_DESC), "Jerash"="Jarash", "Tafila"="Tafileh", "Maan"="Ma'an"),
    age = head_age
  )

gov_name_to_code <- c("Amman"=11,"Balqa"=12,"Zarqa"=13,"Madaba"=14,"Irbid"=21,"Mafraq"=22,
                      "Jarash"=23,"Ajloun"=24,"Karak"=31,"Tafileh"=32,"Ma'an"=33,"Aqaba"=34,
                      "Other"=97,"Don't Know"=98)

cleanDataX <- cleanDataX %>% dplyr::mutate(governorate = gov_name_to_code[GOV_DESC_matched])

selected_vars <- c("request_id","v202","sex","hh_size","educ_level1","governorate","age")
cleanData_selected <- cleanDataX %>% dplyr::select(dplyr::all_of(selected_vars))
cols_to_add <- unique(cleanData_selected)

data3 <- data3 %>% dplyr::left_join(cols_to_add, by = "request_id")

# --- Probit predictions & simple diagnostics ---
probit_model <- readRDS(file.path(inputLocation, "Models/Unemployment/probit_model.rds"))
data3 <- data3 %>%
  dplyr::mutate(
    probit_predicted_prob  = predict(probit_model, newdata = ., type = "response"),
    probit_predicted_class = as.integer(probit_predicted_prob > 0.5)
  )
cm <- table(Predicted = data3$probit_predicted_class, Actual = data3$v202); cm
accuracy    <- sum(diag(cm)) / sum(cm)
sensitivity <- cm["1","1"] / sum(cm[,"1"])
specificity <- cm["0","0"] / sum(cm[,"0"])
cat("Accuracy:", round(accuracy,3), "\n",
    "Sensitivity (TPR):", round(sensitivity,3), "\n",
    "Specificity (TNR):", round(specificity,3), "\n")

# --- Deterministic employment shock (keep signature/outputs) ---
apply_income_shock_deterministic <- function(df, varname, newvarname, base_prob) {
  df <- df %>% dplyr::mutate(row_id = dplyr::row_number())
  df_to_shock <- df %>% dplyr::filter(!!rlang::sym(varname) > 0, SP_program != "Beneficiary")
  n_shock <- floor(sum(df$Formal.income.earners[df$Formal.income.earners > 0]) * base_prob)
  
  shock_ids <- df_to_shock %>%
    dplyr::arrange(risk_weights_1) %>%
    dplyr::slice_head(n = n_shock) %>%
    dplyr::pull(row_id)
  
  df %>%
    dplyr::mutate(
      !!rlang::sym(newvarname) := dplyr::if_else(
        row_id %in% shock_ids,
        pmax(!!rlang::sym(varname) - 1, 0),
        !!rlang::sym(varname)
      )
    ) %>%
    dplyr::select(-row_id)
}

set.seed(123)
data3$risk_weights_1 <- data3$probit_predicted_prob
print(cumulative_effect_1pct * -1)

data3_shocked <- apply_income_shock_deterministic(
  df = data3,
  varname = "Formal.income.earners",
  newvarname = "Formal.income.earners_postshock_SC4",
  base_prob = (cumulative_effect_1pct * -1)
) %>% dplyr::mutate(Formal.income.earners_sq_shocked_SC4 = Formal.income.earners_postshock_SC4^2)

# --- Post-shock score (Scenario 4) ---
HIECS_data_common_F_shocked_SC4 <- data3_shocked %>%
  dplyr::mutate(
    score_postshock_SC4 =
      4.6424438743 +
      0.0664108035 * HH.30y.or.less +
      0.1255826359 * HH.Female.Married +
      0.0745691391 * HH.65y.or.more +
      0.0784850573 * HH.Female.Widow +
      -0.0157724069 * HH.Disabled.or.chronically.ill +
      0.1382000152 * HH.Minimal.education +
      0.1897090558 * HH.Advanced.education +
      0.2834299184 * HH.Intermediate.education +
      0.0194523658 * Working.age.members +
      -0.1975628170 * Disabled.working.age.members +
      0.0821832809 * Male.working.age_18.t.44y +
      -0.1389526948 * HH.Size +
      0.0040141935 * HH.Size_sq +
      -0.1915595349 * Dependency.ratio +
      -0.3311827577 * HH.owns.livestock +
      0.0543048142 * Imputed.livestock.productivity +
      0.1069436887 * HH.owns.land +
      0.4435800352 * Owns.a.car +
      0.1643461081 * Private.cars +
      -0.0274362948 * Age.newest.car +
      0.0004164393 * Age.newest.car_sq +
      0.1368038748 * Working.cars +
      0.0669149014 * Owns.commercial.property +
      0.2114789823 * Owns.stocks +
      0.0524404940 * House.type +
      0.0059023126 * Area.per.capita +
      0.1496706129 * Water.and.electricity.bills +
      -0.0725899939 * HH.lives.in.rural.area +
      0.0755378872 * Formal.income.earners_postshock_SC4 +
      -0.0108665062 * Formal.income.earners_sq +
      -0.1050082539 * Governorate_Mafraq +
      0.0648695943 * Governorate_Amman +
      -0.1504316724 * Governorate_Tafilah +
      0.1297149377 * Governorate_Zarqa +
      -0.0916247636 * Governorate_Balqa +
      -0.0153260026 * Governorate_Maan +
      -0.1001710720 * Governorate_Aqaba +
      -0.0730428536 * Governorate_Karak +
      -0.0008941587 * Governorate_Jarash +
      0.0459832082 * Governorate_Madaba +
      -0.0099285333 * Governorate_Ajlun
  ) %>%
  dplyr::mutate(delta_score_SC4 = score_postshock_SC4 - SCORE)

summary_stats <- HIECS_data_common_F_shocked_SC4 %>%
  dplyr::group_by(SP_program) %>%
  dplyr::summarise(
    avg_score_pre  = mean(SCORE, na.rm = TRUE),
    avg_score_post = mean(score_postshock_SC4, na.rm = TRUE),
    avg_change     = mean(delta_score_SC4, na.rm = TRUE),
    min_change     = min(delta_score_SC4, na.rm = TRUE),
    max_change     = max(delta_score_SC4, na.rm = TRUE),
    .groups = "drop"
  )
print(summary_stats)
sum(HIECS_data_common_F_shocked_SC4$SCORE)
sum(HIECS_data_common_F_shocked_SC4$score_postshock_SC4)

# --- Poverty metrics under Scenario 4 ---
PL_monthly <- exp(PL)         # PL provided earlier
data2 <- HIECS_data_common_F_shocked_SC4
PL_JOD <- exp(PL)

poverty_by_region <- data2 %>%
  dplyr::mutate(poor = as.integer(SCORE < PL)) %>%
  dplyr::group_by(Governorate) %>%
  dplyr::summarise(Poverty = sum(poor * weights) / sum(weights) * 100, .groups = "drop")
poverty_by_region

data2 <- data2 %>%
  dplyr::mutate(
    SCORE_NEW_EMP = score_postshock_SC4,
    poor_new_EMP  = as.integer(SCORE_NEW_EMP < PL)
  )
Poverty_new_EMP <- sum(data2$poor_new_EMP * data2$weights) / sum(data2$weights) * 100

poverty_by_region_NEW <- data2 %>%
  dplyr::group_by(Governorate) %>%
  dplyr::summarise(Poverty = sum(poor_new_EMP * weights) / sum(weights) * 100, .groups = "drop")
poverty_by_region; poverty_by_region_NEW

# --- Program combos & transfers for Scenario 4 ---
data2 <- data2 %>%
  dplyr::mutate(
    SP_program_combo_new_EMP = dplyr::case_when(
      SP_program == "Beneficiary"  & poor_new_EMP == 1 ~ "Beneficiary & poor",
      SP_program == "Beneficiary"  & poor_new_EMP == 0 ~ "Beneficiary & non-poor",
      SP_program == "Eligible"     & poor_new_EMP == 1 ~ "Eligible & poor",
      SP_program == "Eligible"     & poor_new_EMP == 0 ~ "Eligible & non-poor",
      SP_program == "Non-Eligible" & poor_new_EMP == 1 ~ "Non-Eligible & poor",
      TRUE ~ "Non-Eligible & non-poor"
    ),
    SP_program_combo_new_EMP = factor(
      SP_program_combo_new_EMP,
      levels = c("Beneficiary & poor","Beneficiary & non-poor","Eligible & poor",
                 "Eligible & non-poor","Non-Eligible & poor","Non-Eligible & non-poor")
    ),
    Value_CT_4 = dplyr::case_when(
      SP_program_combo_new_EMP %in% c("Beneficiary & poor","Beneficiary & non-poor","Eligible & poor","Non-Eligible & poor") & HH.Size == 1 ~ 40,
      SP_program_combo_new_EMP %in% c("Beneficiary & poor","Beneficiary & non-poor","Eligible & poor","Non-Eligible & poor") & HH.Size > 1 & HH.Size <= 5 ~ pmin(40 + (HH.Size - 1) * 15, 100),
      SP_program_combo_new_EMP %in% c("Beneficiary & poor","Beneficiary & non-poor","Eligible & poor","Non-Eligible & poor") & HH.Size >= 6 ~ 100,
      TRUE ~ 0
    ),
    Total_Value_CT_4 = dplyr::case_when(
      SP_program_combo_new_EMP %in% c("Beneficiary & poor","Beneficiary & non-poor","Eligible & poor","Non-Eligible & poor") ~
        Value_CT_4 + ifelse(Added_value == 1, 35, 0),
      TRUE ~ Value_CT_4
    ),
    Share_benefit_expenditure_CT_4 = Total_Value_CT_4 / Monthly_expenditure * 100
  )



