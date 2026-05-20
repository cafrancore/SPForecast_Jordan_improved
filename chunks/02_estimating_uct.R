###CHAPTER 2 PL ANALYSIS



# --------------------------- TRANSFER & SCORE ---------------------------------------
cleanData$old_member_bin   <- ifelse(cleanData$`HH 65y or more` >= 1, 1, 0)
cleanData$disab_member_bin <- ifelse(cleanData$`HH Disabled or chronically ill` >= 1, 1, 0)
cleanData$HH_F_Divorced_WIDOWED <- ifelse(cleanData$`HH Female Widow` >= 1, 1, 0)

data <- cleanData %>% rename(SP_program = Eligibility)
# Base CT value rules
data <- data %>% mutate(
  Value = dplyr::case_when(
    SP_program == "Beneficiary" & `HH Size` == 1 ~ 40,
    SP_program == "Beneficiary" & `HH Size` > 1 & `HH Size` <= 5 ~ pmin(40 + (`HH Size` - 1) * 15, 100),
    SP_program == "Beneficiary" & `HH Size` >= 6 ~ 100,
    TRUE ~ 0
  ),
  Added_value = ifelse(HH_F_Divorced_WIDOWED == 1 | old_member_bin == 1 | disab_member_bin == 1, 1, 0),
  Total_Value_CT_0 = dplyr::case_when(
    SP_program == "Beneficiary" ~ Value + ifelse(Added_value == 1, 35, 0),
    TRUE ~ Value
  )
)
# Score from (monthly imputed income per capita)
data <- data %>% mutate(SCORE = log(`Impute Income`/`HH Size`))
# Expenditure & shares
data$Annual_expenditure  <- exp(data$SCORE) * data$`HH Size` * 12 / 1.12
data$Monthly_expenditure <- data$Annual_expenditure/12
data$Total_Value_CT_0[is.na(data$Total_Value_CT_0)] <- 0



