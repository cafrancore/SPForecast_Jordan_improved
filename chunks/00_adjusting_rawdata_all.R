#library(readxl)
library(readxl)
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

Exampleben <- read_excel(
  file.path(dataLocation, "Admin_record/Beneficiary.xlsx"),
  na = "NULL"
)
DatabaseB <- Exampleben  # template structure

#------------------------------------------------------------------
# 1) Helper mappings & utilities ----------------------------------
#------------------------------------------------------------------

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

#------------------------------------------------------------------
# 2) Function: from HH + parameters Excel → merged dataset --------
#------------------------------------------------------------------

build_admin_dataset <- function(file_path,
                                sheet_hh = "HH",
                                sheet_par = "parameters") {
  
  wide <- read_excel(file_path, sheet = sheet_hh)
  long <- read_excel(file_path, sheet = sheet_par)
  
  long_wide <- long %>%
    rename(Request_ID = Request_id) %>%
    dplyr::select(Request_ID, Parameter, Parameter_Value) %>%
    group_by(Request_ID, Parameter) %>%
    summarise(Parameter_Value = dplyr::first(Parameter_Value),
              .groups = "drop") %>%
    pivot_wider(
      names_from  = Parameter,
      values_from = Parameter_Value
    )
  
  merged <- wide %>%
    left_join(long_wide, by = "Request_ID")
  
  merged
}

#------------------------------------------------------------------
# 3) Function: convert merged dataset → DatabaseB structure --------
#------------------------------------------------------------------

prep_to_DB <- function(merged_df, DatabaseB_template) {
  
  # ---- Build DatabaseA_prepped (content) ----
  DatabaseA_prepped <- merged_df %>%
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
      
      Family_Other_Formal_Income = NA_real_,      # not available here
      Family_Income_Category     = NA_character_, # can be filled later if needed
      
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
      Education_Level_Name = factor(Education_Level_Name,
                                    levels = valid_edu_levels),
      
      # ---- Employment status (copy Arabic for now) ----
      Employment_Status_Name = Employment_Status_Name_AR,
      
      # ---- “Model” placeholders present in DatabaseB ----
      ps1      = NA_real_,
      ps2      = NA_real_,
      X_Status = NA_character_
      
      # NOTE: all the @v_ and @v_PTindC_ variables are
      # already in merged_df -> we keep them as they are,
      # no need to recompute.
    )
  
  # ---- Add missing columns (by name) to match DatabaseB ----
  missing_cols <- setdiff(names(DatabaseB_template),
                          names(DatabaseA_prepped))
  
  for (nm in missing_cols) {
    template <- DatabaseB_template[[nm]]
    
    if (is.numeric(template)) {
      DatabaseA_prepped[[nm]] <- NA_real_
    } else if (is.character(template)) {
      DatabaseA_prepped[[nm]] <- NA_character_
    } else if (is.logical(template)) {
      DatabaseA_prepped[[nm]] <- NA
    } else if (is.factor(template)) {
      DatabaseA_prepped[[nm]] <- factor(NA, levels = levels(template))
    } else {
      DatabaseA_prepped[[nm]] <- NA
    }
  }
  
  # ---- Align types of common columns (A to B) ----
  common_cols <- intersect(names(DatabaseB_template),
                           names(DatabaseA_prepped))
  
  for (nm in common_cols) {
    target <- DatabaseB_template[[nm]]  # the "gold standard"
    
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
  
  # ---- Final: reorder columns to match DatabaseB exactly ----
  final_df <- DatabaseA_prepped[, names(DatabaseB_template)]
  
  final_df
}

#------------------------------------------------------------------
# 4) Build the three groups: Beneficiaries, Eligibles, NonEligibles
#------------------------------------------------------------------

## 4.1 Beneficiaries ----------------------------------------------
Beneficiaries_merged <- build_admin_dataset(
  file.path(dataLocation, "Admin_record/Data_Beneficiaries101225.xlsx"),
  sheet_hh  = "HH",
  sheet_par = "parameters"
)

Beneficiaries <- prep_to_DB(Beneficiaries_merged, DatabaseB)

## 4.2 Eligibles ---------------------------------------------------
# adjust the file name if yours is different
Eligibles_merged <- build_admin_dataset(
  file.path(dataLocation, "Admin_record/Data_Beneficiaries101225.xlsx"),
  sheet_hh  = "HH",
  sheet_par = "parameters"
)

Eligibles <- prep_to_DB(Eligibles_merged, DatabaseB)

## 4.3 Non-eligibles ----------------------------------------------
# adjust the file name if yours is different
NonEligibles_merged <- build_admin_dataset(
  file.path(dataLocation, "Admin_record/Data_Beneficiaries101225.xlsx"),
  sheet_hh  = "HH",
  sheet_par = "parameters"
)

NonEligibles <- prep_to_DB(NonEligibles_merged, DatabaseB)
