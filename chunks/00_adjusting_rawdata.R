#library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)
library(openxlsx)


#------------------------------------------------------------------
# 0) Load template Beneficiary (DatabaseB) -------------------------
#------------------------------------------------------------------

dataLocation <- "Data"  # adjust if different

Beneficiaries_wide <- read_excel("Data/Admin_record/Data_Beneficiaries101225.xlsx", sheet = "HH")
Beneficiaries_long <- read_excel("Data/Admin_record/Data_Beneficiaries101225.xlsx", sheet = "parameters")



## 1. Harmonise ID variable name in the long file
Beneficiaries_long <- Beneficiaries_long %>%
  rename(Request_ID = Request_id)

## 2. Go from long -> wide
##    - Drop Run_serial and Parent_Parameter
##    - Columns = Parameter
##    - Values  = Parameter_Value
##    - If there are duplicates for the same (Request_ID, Parameter),
##      we keep the first value.
Beneficiaries_long_wide <- Beneficiaries_long %>%
  dplyr::select(Request_ID, Parameter, Parameter_Value) %>%
  group_by(Request_ID, Parameter) %>%
  summarise(Parameter_Value = dplyr::first(Parameter_Value),
            .groups = "drop") %>%
  pivot_wider(
    names_from  = Parameter,
    values_from = Parameter_Value
  )

## 3. Left join to Beneficiaries_wide (main DB)
Beneficiaries_merged <- Beneficiaries_wide %>%
  left_join(Beneficiaries_long_wide, by = "Request_ID")





Exampleben <- read_excel(
  file.path(dataLocation, "Admin_record/Beneficiary.xlsx"),
  na = "NULL"
)
DatabaseB <- Exampleben


library(dplyr)

map_marital <- c(
  "متزوج/ متزوجة"               = "Married",
  "أعزب / عزباء"                 = "Single",
  "مطلق / مطلقة"                 = "Divorced",
  "ارمل / ارملة"                 = "Widowed",
  "متزوجه من اجنبي"              = "Married",
  "ارملة الاجنبي"                = "Widowed",
  "زوجة أجنبي (أبناء أردني)"     = "Married"
)

map_gender <- c("ذكر" = "Male", "انثى" = "Female")

coalesce_num <- function(x, y = 0) {
  ifelse(is.na(x), y, x)
}

valid_edu_levels <- c(
  "توجيهي",
  "أساسي (عاشر فأقل)",
  "أمي",
  "توجيهي راسب",
  "اول ثانوي",
  "دبلوم",
  "بكالوريوس",
  "دكتوراه",
  "ماجستير"
)

DatabaseA_prepped <- Beneficiaries_merged %>%
  mutate(
    # ---- ID ----
    request_id = Request_ID,
    
    # ---- Core demographic / HH size / income ----
    HH_Size  = coalesce_num(`حجم الاسرة من واقع الاحوال المدنية`),
    head_age = coalesce_num(`العمر`),
    
    # Keep Imputed_Income as is (already in merged data)
    Imputed_Income = coalesce_num(Imputed_Income),
    
    Family_Income_Value = coalesce_num(`قيمة دخل الاسرة`),
    Total_HHH_Income    = Family_Income_Value,
    
    Family_Income_Value_per_capita = dplyr::if_else(
      HH_Size > 0,
      Family_Income_Value / HH_Size,
      NA_real_
    ),
    
    Family_Other_Formal_Income = NA_real_,  # not available here
    Family_Income_Category     = NA_character_,  # to be filled if you have a rule
    
    # ---- Gender & marital status ----
    Gender_Code = recode(`جنس رب الاسرة`, !!!map_gender),
    
    Marital_status = recode(`الحالة الاجتماعية`, !!!map_marital),
    
    # ---- Health status (DatabaseB wants Arabic categories) ----
    # DatabaseB: "طبيعي | عاجز | مريض مرض مزمن | ذوي احتياجات خاصة"
    Health_Condition_Name = case_when(
      `الوضع الصحي` %in% c("طبيعي", "عاجز", "مريض مرض مزمن", "ذوي احتياجات خاصة") ~ `الوضع الصحي`,
      TRUE ~ NA_character_
    ),
    
    # ---- Governorate (DatabaseB uses Arabic GOV_DESC) ----
    GOV_DESC = `المحافظة`,
    
    # ---- Education (Arabic → harmonised Arabic factor) ----
    Education_Level_Name = dplyr::recode(
      `المستوى التعليمي لرب الاسرة`,
      # direct matches
      "توجيهي"            = "توجيهي",
      "أساسي (عاشر فأقل)" = "أساسي (عاشر فأقل)",
      "أمي"               = "أمي",
      "توجيهي راسب"       = "توجيهي راسب",
      "اول ثانوي"         = "اول ثانوي",
      "دبلوم"             = "دبلوم",
      "بكالوريوس"         = "بكالوريوس",
      "دكتوراه"           = "دكتوراه",
      "ماجستير"           = "ماجستير",
      # recodes
      "يقرأ ويكتب"        = "أساسي (عاشر فأقل)",
      "NULL"              = NA_character_,
      .default            = NA_character_
    ),
    Education_Level_Name = factor(Education_Level_Name, levels = valid_edu_levels),
    
    # ---- Employment status (we just copy Arabic for now) ----
    Employment_Status_Name = Employment_Status_Name_AR,
    
    # ---- “Model” placeholders present in DatabaseB ----
    ps1      = NA_real_,
    ps2      = NA_real_,
    X_Status = NA_character_
    
    # NOTE: all the @v_ and @v_PTindC_ variables are
    # already in Beneficiaries_merged -> we keep them as they are,
    # no need to recompute.
  )





# DatabaseB already exists and is your "template"
missing_cols <- setdiff(names(DatabaseB), names(DatabaseA_prepped))

for (nm in missing_cols) {
  template <- DatabaseB[[nm]]
  
  if (is.numeric(template)) {
    DatabaseA_prepped[[nm]] <- NA_real_
  } else if (is.character(template)) {
    DatabaseA_prepped[[nm]] <- NA_character_
  } else if (is.logical(template)) {
    DatabaseA_prepped[[nm]] <- NA
  } else {
    DatabaseA_prepped[[nm]] <- NA
  }
}



# Align types of A to B for all common columns --------------------

common_cols <- intersect(names(DatabaseB), names(DatabaseA_prepped))

for (nm in common_cols) {
  target <- DatabaseB[[nm]]         # the "gold standard"
  
  # Factor in B  → factor in A with same levels
  if (is.factor(target)) {
    DatabaseA_prepped[[nm]] <- factor(
      as.character(DatabaseA_prepped[[nm]]),
      levels = levels(target)
    )
    
    # Numeric (double) in B
  } else if (is.numeric(target) && !is.integer(target)) {
    DatabaseA_prepped[[nm]] <- as.numeric(DatabaseA_prepped[[nm]])
    
    # Integer in B
  } else if (is.integer(target)) {
    DatabaseA_prepped[[nm]] <- as.integer(DatabaseA_prepped[[nm]])
    
    # Character in B
  } else if (is.character(target)) {
    DatabaseA_prepped[[nm]] <- as.character(DatabaseA_prepped[[nm]])
    
    # Logical in B
  } else if (is.logical(target)) {
    DatabaseA_prepped[[nm]] <- as.logical(DatabaseA_prepped[[nm]])
  }
}

Beneficiaries <- DatabaseA_prepped[, names(DatabaseB)]

