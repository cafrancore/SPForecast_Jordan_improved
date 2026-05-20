# R/chunks/01_data_prep.R
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
})

if (!exists(".sp_cache", envir = .GlobalEnv, inherits = FALSE)) {
  assign(".sp_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
}
.sp_cache <- get(".sp_cache", envir = .GlobalEnv)

.cache_get <- function(key, loader) {
  if (!exists(key, envir = .sp_cache, inherits = FALSE)) {
    assign(key, loader(), envir = .sp_cache)
  }
  get(key, envir = .sp_cache, inherits = FALSE)
}

# language: 1 = English, 2 = Arabic  (matches tu app)
# sample_dups: si TRUE, toma 1 registro por request_id
#data_prep_load <- function(inputLocation, dataLocation, language = 1, sample_dups = TRUE) {
 # stopifnot(dir.exists(inputLocation), dir.exists(dataLocation))
 
# --------------------------- PATHS --------------------------------------------------
scriptLocation      <- userLocation
inputLocation       <- file.path(userLocation, "Input")
dataLocation        <- file.path(userLocation, "Data")
Gislocation         <- file.path(inputLocation, "GIS")
dictionaryLocation  <- file.path(inputLocation, "Dictionary")


# --------------------------- DICTIONARIES -------------------------------------------
VarLabels <- .cache_get("dict_dashboard_A1_C180", function() {
  as.matrix(read_excel(file.path(dictionaryLocation, "DictionaryR.xlsx"),
                       sheet = "Dashboard", range = "A1:C180"))
})
VarLabels[is.na(VarLabels)] <- ""
VarLabels[,1] <- trimws(VarLabels[,1])

Employment_Status <- .cache_get("dict_category_F1_I6", function() {
  as.matrix(read_excel(file.path(dictionaryLocation, "DictionaryR.xlsx"),
                       sheet = "Category", range = "F1:I6"))
})
Education_Level   <- .cache_get("dict_category_A1_D10", function() {
  as.matrix(read_excel(file.path(dictionaryLocation, "DictionaryR.xlsx"),
                       sheet = "Category", range = "A1:D10"))
})
Health_Level      <- .cache_get("dict_category_K1_N5", function() {
  as.matrix(read_excel(file.path(dictionaryLocation, "DictionaryR.xlsx"),
                       sheet = "Category", range = "K1:N5"))
})
Gender_Level      <- .cache_get("dict_category_P1_S3", function() {
  as.matrix(read_excel(file.path(dictionaryLocation, "DictionaryR.xlsx"),
                       sheet = "Category", range = "P1:S3"))
})
Governorates      <- .cache_get("dict_category_U1_X13", function() {
  as.matrix(read_excel(file.path(dictionaryLocation, "DictionaryR.xlsx"),
                       sheet = "Category", range = "U1:X13"))
})
VariableNames     <- .cache_get("dict_category_Z1_AC50", function() {
  as.matrix(read_excel(file.path(dictionaryLocation, "DictionaryR.xlsx"),
                       sheet = "Category", range = "Z1:AC50"))
})
Variable_X        <- .cache_get("dict_category_AZ1_BC41", function() {
  as.matrix(read_excel(file.path(dictionaryLocation, "DictionaryR.xlsx"),
                       sheet = "Category", range = "AZ1:BC41"))
})

# --------------------------- LOAD ADMIN RECORDS -------------------------------------
#BeneficiariesData <- read_excel(file.path(dataLocation, "Admin_record/Beneficiary.xlsx"), na = "NULL") %>% mutate(Eligibility = "Beneficiary")
#EligiblesData <- read_excel(file.path(dataLocation, "Admin_record/Elegible.xlsx"), na = "NULL") %>%
 #mutate(Eligibility = "Eligible")
#Non_eligiblesData <- read_excel(file.path(dataLocation, "Admin_record/Non-Elegible.xlsx"), na = "NULL") %>%
 #mutate(Eligibility = "Non-Eligible")


BeneficiariesData <- Beneficiaries %>%  mutate(Eligibility = "Beneficiary") 

EligiblesData <- Eligibles  %>% mutate(Eligibility = "Eligible")
Non_eligiblesData <- NonEligibles  %>% mutate(Eligibility = "Non-Eligible")


ApplicantsData <- bind_rows(BeneficiariesData, EligiblesData, Non_eligiblesData)
# Deduplicate by request_id
set.seed(123)

#⚠️#remove with real data
#ApplicantsData <- ApplicantsData %>% group_by(request_id) %>% slice_sample(n = 1) %>% ungroup()

ApplicantsData <- ApplicantsData %>%
  drop_na(GOV_DESC)

ApplicantsData_back <- ApplicantsData

# Label factors on *_back
ApplicantsData_back$Employment_Status_Name <- factor(ApplicantsData_back$Employment_Status_Name,
                                                     levels = Employment_Status[,2], labels = Employment_Status[,4 - language])
ApplicantsData_back$Education_Level_Name <- factor(ApplicantsData_back$Education_Level_Name,
                                                   levels = Education_Level[,2], labels = Education_Level[,4 - language])
ApplicantsData_back$Health_Condition_Name <- factor(ApplicantsData_back$Health_Condition_Name,
                                                    levels = Health_Level[,2], labels = Health_Level[,4 - language])
ApplicantsData_back$Gender_Code <- factor(ApplicantsData_back$Gender_Code,
                                          levels = Gender_Level[,2], labels = Gender_Level[,4 - language])
ApplicantsData_back$GOV_DESC <- factor(ApplicantsData_back$GOV_DESC,
                                       levels = Governorates[,2], labels = Governorates[,4 - language])

# Keep curated variables + request_id + Impute(d)_Income
varNames <- colnames(ApplicantsData)
case3   <- startsWith(varNames, "Imputed_Income") | startsWith(varNames, "Impute Income")
case7   <- startsWith(varNames, "request_id")
keep_names <- varNames %in% c(
  "Eligibility", "@v_Heads_age_30_years", "@v_Head_is_married_female", "@v_Heads_age_65_years",
  "@v_NAF_beneficiary_status", "@v_rural_area", "@v_HEAD_sexmar_femwid", "@v_head_disabill",
  "@v_HEAD_edu_elem", "@v_HEAD_edu_basicsecvoc", "@v_HEAD_edu_bapostba", "@v_hh_size",
  "@v_hh_size2", "@v_hhsh_dep", "@v_hhn_workage", "@v_hhshworkage_disab",
  "@v_hhshworkage_agecat_m1844", "@v_livestockimpute2_yn", "@v_lnlivestockimpute2", "@v_land_own",
  "@v_land_cultiv", "@v_as_car_yn", "@v_as_car_num", "@v_car_age0", "@v_car_age0_sq",
  "@v_as_cargovehtaxibus_num", "@v_as_residcombuild_yn", "@v_as_stocks_yn", "@v_housetype_appt",
  "@v_houseareapc", "@v_lnexp_elecwaterwaste_pc",
  "@v_count_family_members_Pension_and_Formal_Income",
  "@v_count_family_members_Pension_and_Formal_Income_q", "GOV_DESC"
)
chosenVariables <- varNames[keep_names | case3 | case7]
ApplicantsData  <- ApplicantsData %>% dplyr::select(all_of(chosenVariables)) %>% tidyr::drop_na()
# Map admin names to readable names via VariableNames
colnames(ApplicantsData) <- as.character(factor(colnames(ApplicantsData),
                                                levels = VariableNames[,2], labels = VariableNames[,4 - 1]))
ApplicantsData$Governorate <- factor(ApplicantsData$Governorate,
                                     levels = Governorates[,2], labels = Governorates[,4 - language])
names(ApplicantsData)[1] <- "request_id"


ApplicantsData<-ApplicantsData %>% filter(`Impute Income`>0)
  
data_macro <- .cache_get("data_jor_sheet_data_jor", function() {
  read_excel(file.path(dataLocation, "data_jor.xlsx"), sheet = "Data_jor")
})

#}
