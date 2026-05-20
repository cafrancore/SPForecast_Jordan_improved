main_server <- function(input, output, session) {
  
  observeEvent(input$translation, {
    dir <- if (input$translation == 2) "rtl" else "ltr"
    session$sendCustomMessage("setClass", dir)
  })
  
  
  # Reactive values to store simulation results
  simResults <- reactiveValues(
    finalTable = NULL,
    finalTable_gov = NULL
  )
  
  # Reactive: filtered data by scenario
  filtered_gov_data <- reactive({
    req(simResults$finalTable_gov, input$scenarioFilter)
    
    simResults$finalTable_gov %>%
      filter(Scenario_name == input$scenarioFilter) %>%
      mutate(Governorate = recode(Governorate,
                                  "Tafila" = "Tafilah",
                                  "Jerash" = "Jarash",
                                  "Ajloun" = "Ajlun",
                                  "Maan" = "Ma`an"))
  })
  
  
  employment_impact <- reactive({
    if(input$employment_input_method == "gdp") {
      # Calculate from GDP growth
      increase_gdp_percap <- (input$increase_gdp_percap)/100
      
      
      
      source(paste0(inputLocation,"Models/Unemployment/MacroEconShock.R"))
      cumulative_effect_1pct <- (cumulative_irf / 0.1226) * (input$increase_gdp_percap)/100
      
      
    } else {
      # Use direct input
      cumulative_effect_1pct <- input$cumulative_effect_1pct_user/100
    }
    return(cumulative_effect_1pct)
  })
  
  
  
  # This should be OUTSIDE of other observers
  observeEvent(input$goToSimParams, {
    updateTabItems(session, "tabs", "setting_simulation_parameters")
  })
  
  observeEvent(input$goToShockparams, {
    updateTabItems(session, "tabs", "params")
  })
  
  
  
  observeEvent(input$runSimulation, {
    showNotification(textOutput("newtranslation23"), type = "message")
    
    
    # 1.0 Preliminaries ------------------------------------------------------
    # Extract parameters from inputs
    target_poverty_rate <- input$targetPoverty 
    exc_rateJOD_USD <- input$exchangeRate
    shock_inflation <- input$inflationShock 
    
    
    
    
    # 1.1 Uploading dictionary -------------------------------------------------------
    
    
    VarLabels <- as.matrix(read_excel(paste0(dictionaryLocation, '/DictionaryR.xlsx'), sheet = 'Dashboard', range = 'A1:C150'))
    VarLabels[is.na(VarLabels)] <- ''
    VarLabels[,1] <- trimws(VarLabels[,1])  # <<< THIS IS THE FIX
    
    dataPlots <- read_excel(paste0(inputLocation,'Dictionary/DictionaryR.xlsx'), sheet = 'labels')
    dataPlots[is.na(dataPlots)] <- ''
    
    
    
    BeneficiariesData=read_excel((paste0(dataLocation,'Admin_record/Beneficiary.xlsx')),na = "NULL")
    BeneficiariesData$Eligibility <- "Beneficiary"
    
    EligiblesData=read_excel((paste0(dataLocation,'Admin_record/Elegible.xlsx')),na = "NULL")
    EligiblesData$Eligibility <- "Eligible"
    
    Non_eligiblesData=read_excel((paste0(dataLocation,'Admin_record/Non-Elegible.xlsx')),na = "NULL")
    Non_eligiblesData$Eligibility <- "Non-Eligible"
    
    ApplicantsData=rbind(BeneficiariesData, EligiblesData,Non_eligiblesData)
    
    ApplicantsData_back=rbind(BeneficiariesData, EligiblesData,Non_eligiblesData)
    
    
    ##There are 16 hh with repeated information. random selection of only one record 
    ApplicantsData <- ApplicantsData %>%
      group_by(request_id) %>%
      slice_sample(n = 1) %>%
      ungroup()
    
    ApplicantsData_back =ApplicantsData
    
    
    ## Dictionary labels
    #data comes from PMT analysis
    #transfer_data_common <- 
    #transfer_data_common <- read.csv("TFData.csv")
    Employment_Status       <- as.matrix(read_excel(paste0(inputLocation,'Dictionary/DictionaryR.xlsx'), sheet = 'Category', range = 'F1:I6'))
    Education_Level         <- as.matrix(read_excel(paste0(inputLocation,'Dictionary/DictionaryR.xlsx'), sheet = 'Category', range = 'A1:D10'))
    Health_Level         <- as.matrix(read_excel(paste0(inputLocation,'Dictionary/DictionaryR.xlsx'), sheet = 'Category', range = 'K1:N5'))
    Gender_Level         <- as.matrix(read_excel(paste0(inputLocation,'Dictionary/DictionaryR.xlsx'), sheet = 'Category', range = 'P1:S3'))
    Governorates         <- as.matrix(read_excel(paste0(inputLocation,'Dictionary/DictionaryR.xlsx'), sheet = 'Category', range = 'U1:X13'))
    VariableNames         <- as.matrix(read_excel(paste0(inputLocation,'Dictionary/DictionaryR.xlsx'), sheet = 'Category', range = 'Z1:AC50'))
    RankingPriorities         <- as.matrix(read_excel(paste0(inputLocation,'Dictionary/DictionaryR.xlsx'), sheet = 'Category', range = 'AE1:AH5'))
    Variable_X         <- as.matrix(read_excel(paste0(inputLocation,'Dictionary/DictionaryR.xlsx'), sheet = 'Category', range = 'AZ1:BC41'))
    
    
    
    
    #Add the relevant factor variables
    ApplicantsData_back$Employment_Status_Name=factor(ApplicantsData_back$Employment_Status_Name,
                                                      levels = Employment_Status[,2],
                                                      labels = Employment_Status[,4-language])
    
    ApplicantsData_back$Education_Level_Name=factor(ApplicantsData_back$Education_Level_Name,
                                                    levels = Education_Level[,2],
                                                    labels = Education_Level[,4-language])
    
    ApplicantsData_back$Health_Condition_Name=factor(ApplicantsData_back$Health_Condition_Name,
                                                     levels = Health_Level[,2],
                                                     labels = Health_Level[,4-language])
    
    ApplicantsData_back$Gender_Code=factor(ApplicantsData_back$Gender_Code,
                                           levels = Gender_Level[,2],
                                           labels = Gender_Level[,4-language])
    
    ApplicantsData_back$GOV_DESC=factor(ApplicantsData_back$GOV_DESC,
                                        levels = Governorates[,2],
                                        labels = Governorates[,4-language])
    
    
    # 2.0 Data preparation ------------------------------------------------------
    # 2.1 Administrative records (UCT Jordan)------------------------------------------------------
    
    varNames=colnames(ApplicantsData)
    case1=startsWith(varNames,"@v_")
    case2=startsWith(varNames,"@v_PTindC")
    case3=startsWith(varNames,"Imputed_Income")
    #case4=startsWith(varNames,"GOV_DESC")
    #case5=varNames%in%c("Eligibility","@v_Heads_age_30_years", "@v_Head_is_married_female", 
    #                   "@v_Heads_age_65_years",  
    #                  "@v_HEAD_sexmar_femwid", "@v_head_disabill", 
    #                 "@v_HEAD_edu_elem", "@v_HEAD_edu_basicsecvoc", "@v_HEAD_edu_bapostba", 
    #                "@v_hh_size", "@v_hh_size2", "@v_hhsh_dep", "@v_hhn_workage", 
    #               "@v_hhshworkage_disab", "@v_hhshworkage_agecat_m1844",
    #              "@v_rural_area", "@v_count_family_members_Pension_and_Formal_Income", "@v_count_family_members_Pension_and_Formal_Income_q",
    #             "@v_as_car_yn", "@v_as_car_num","GOV_DESC")
    
    
    case5=varNames%in%c("Eligibility","@v_Heads_age_30_years", "@v_Head_is_married_female", "@v_Heads_age_65_years", "
                    @v_NAF_beneficiary_status", "@v_rural_area", "@v_HEAD_sexmar_femwid", "@v_head_disabill", "@v_HEAD_edu_elem", 
                        "@v_HEAD_edu_basicsecvoc", "@v_HEAD_edu_bapostba", "@v_hh_size", "@v_hh_size2", "@v_hhsh_dep", 
                        "@v_hhn_workage", "@v_hhshworkage_disab", 
                        "@v_hhshworkage_agecat_m1844", "@v_livestockimpute2_yn", "@v_lnlivestockimpute2", "@v_land_own",
                        "@v_land_cultiv", "@v_as_car_yn", "@v_as_car_num", "@v_car_age0", "@v_car_age0_sq", "@v_as_cargovehtaxibus_num",
                        "@v_as_residcombuild_yn", "@v_as_stocks_yn", "@v_housetype_appt", "@v_houseareapc", "@v_lnexp_elecwaterwaste_pc", 
                        "@v_count_family_members_Pension_and_Formal_Income", "@v_count_family_members_Pension_and_Formal_Income_q", "GOV_DESC")
    
    
    case6=startsWith(varNames,"X_Status")
    case7=startsWith(varNames,"request_id")
    case8=startsWith(varNames,"X")
    case9 <- varNames[startsWith(varNames, "@v_") & !startsWith(varNames, "@v_PTindC")]
    
    
    ApplicantsData_Vvar <- ApplicantsData_back %>%
      dplyr::select(all_of(case9))
    # drop_na()
    
    ApplicantsData_Xvar <- ApplicantsData_back %>% dplyr::select(X1:X40)
    colnames(ApplicantsData_Xvar)=as.character(factor(colnames(ApplicantsData_Xvar),
                                                      levels = Variable_X[,1],
                                                      labels = Variable_X[,2]))
    
    ApplicantsData_Xvar = cbind(ApplicantsData_back$request_id,ApplicantsData_Xvar)
    colnames(ApplicantsData_Xvar)=as.character(factor(colnames(ApplicantsData_Xvar),
                                                      levels = Variable_X[,1],
                                                      labels = Variable_X[,2]))
    
    ApplicantsData_Xvar = cbind(ApplicantsData_back$request_id,ApplicantsData_Xvar)
    
    
    
    chosenVariables=varNames[case5|case3|case7]
    
    ApplicantsData=ApplicantsData%>%
      dplyr::select(all_of(chosenVariables))%>%
      drop_na()
    
    
    colnames(ApplicantsData)=as.character(factor(colnames(ApplicantsData),
                                                 levels = VariableNames[,2],
                                                 labels = VariableNames[,4-1]))
    ApplicantsData$Governorate=factor(ApplicantsData$Governorate,
                                      levels = Governorates[,2],
                                      labels = Governorates[,4-language])
    
    names(ApplicantsData)[1] <- "request_id"
    
    cleanData=ApplicantsData
    
    #cleanData <- cleanData[,c(-1,-3)]
    
    #cleanData_B <- cleanData[,c(22,23)]
    #cleanData <- cleanData[,-c(22,23)]
    
    ###CHAPTER 2 PL ANALYSIS
    
    # exc_rateJOD_USD= 1.41
    
    ######### Analysis OF PL AND ECONOMIC SHOCK 
    # Read the data
    
    ###⚠️CHECK VARIABLES AND VALUES OF THE CASH TRANSFER
    
    
    cleanData$old_member_bin <- ifelse(cleanData$`HH 65y or more` >= 1, 1, 0)
    cleanData$disab_member_bin <- ifelse(cleanData$`HH Disabled or chronically ill` >= 1, 1, 0)
    ###⚠️ create the variable since this is not necessarily includes divorced F hh
    cleanData$HH_F_Divorced_WIDOWED <- ifelse(cleanData$`HH Female Widow` >= 1, 1, 0)
    
    data=cleanData
    
    data <- data %>% rename(SP_program = Eligibility)
    
    
    
    ##Create the value of the transfer according to the  program condition.
    data <- data %>%
      mutate(
        Value = case_when(
          SP_program == "Beneficiary" & `HH Size` == 1 ~ 40,  
          SP_program == "Beneficiary" & `HH Size` > 1 & `HH Size` <= 5 ~ pmin(40 + (`HH Size` - 1) * 15, 100),  
          SP_program == "Beneficiary" & `HH Size` >= 6 ~ 100,  
          TRUE ~ 0  
        )
      )
    
    data <- data %>%
      mutate(
        Added_value = ifelse(data$HH_F_Divorced_WIDOWED == 1 | data$old_member_bin == 1 | data$disab_member_bin == 1, 1, 0)  
      )
    
    data <- data %>%
      mutate(
        Total_Value_CT_0 = case_when(
          SP_program == "Beneficiary" ~ data$Value + ifelse(data$Added_value == 1, 35, 0),
          TRUE ~ Value 
        )
      )
    
    # EXP of SCORE is equivalent to per capita monthly value of expenditure in monetary value (assuming that in admin data Impute Income is Monthly Family Income)
    # EXP (score) * HH_size is equivalent to Household monthly value of expenditure  
    
    data <- data %>%
      mutate(SCORE=log(`Impute Income`/`HH Size`)) 
    
    data$Annual_expenditure <- exp(data$SCORE)*data$`HH Size`*12/1.12
    data$Monthly_expenditure <- data$Annual_expenditure/12
    
    data$Total_Value_CT_0 <- replace(data$Total_Value_CT_0, is.na(data$Total_Value_CT_0), 0)
    data$Share_benefit_expenditure <- data$Total_Value_CT_0/data$Monthly_expenditure*100
    
    
    ##output
    Total_amount_to_be_paid_monthly_usd <- sum(data$Total_Value_CT_0)/(10^6)*exc_rateJOD_USD
    
    
    dataCH2=data
    
    
    ##CHAPTER 3 PL AND INFLATION SHOCK ANALYSIS
    
    ##Calling dataset
    data= dataCH2
    
    data$weights=1
    data$match_type= data$SP_program
    data <- data %>%  rename(HH.Size= `HH Size`)
    data <- data %>%  rename(Owns.a.car= `Owns a car`)
    
    #BeneficiariesData <- BeneficiariesData %>%  rename(HH_Size= `HH Size`)
    Pop <- nrow(data)
    
    
    ####POVERTY + PMT
    
    calculate_poverty_rate <- function(threshold) {
      data$below_threshold <- ifelse(data$SCORE < threshold, 1, 0)
      poverty_rate <- sum(data$below_threshold ) / sum(data$weights) * 100
      return(poverty_rate)
    }
    
    data= na.omit(data)
    
    threshold_range <- seq(min(data$SCORE), max(data$SCORE), length.out = 1000)  
    closest_threshold <- NA
    closest_diff <- Inf
    
    for (threshold_value in threshold_range) {
      poverty_rate <- calculate_poverty_rate(threshold_value)
      
      if (abs(poverty_rate - target_poverty_rate) < closest_diff) {
        closest_diff <- abs(poverty_rate - target_poverty_rate)
        closest_threshold <- threshold_value
      }
    }
    
    PL <- closest_threshold
    data$poor <- ifelse(data$SCORE < PL, 1, 0)
    Poverty <- sum(data$poor)/nrow(data)*100
    result <- aggregate(poor ~ SP_program, data, function(x) c(total = length(x), poor_count = sum(x)))
    result <- data.frame(match_type = result$SP_program,
                         total = result$poor[, "total"],
                         poor_count = result$poor[, "poor_count"])
    #colnames(result) <- gsub("^poor.", "", colnames(result))
    
    PL_JOD <- exp(PL)
    Poverty
    
    data <- data %>%
      mutate(
        SP_program_combo = case_when(
          SP_program == "Beneficiary" & poor == 1 ~ "Beneficiary & poor",
          SP_program == "Beneficiary" & poor == 0 ~ "Beneficiary & non-poor",
          SP_program == "Eligible" & poor == 1 ~ "Eligible & poor",
          SP_program == "Eligible" & poor == 0 ~ "Eligible & non-poor",
          SP_program == "Non-eligible" & poor == 1 ~ "Non-Eligible & poor",
          TRUE ~ "Non-Eligible & non-poor"
        )
      )
    
    data$SP_program_combo <- factor(data$SP_program_combo, 
                                    levels = c("Beneficiary & poor", 
                                               "Beneficiary & non-poor", 
                                               "Eligible & poor", 
                                               "Eligible & non-poor", 
                                               "Non-Eligible & poor",
                                               "Non-Eligible & non-poor"))
    
    
    
    
    
    ###Scenario 1: covering all the poor household
    data <- data %>%
      mutate(
        Value_CT_1 = case_when(
          SP_program_combo %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") & HH.Size == 1 ~ 40,  
          SP_program_combo %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") & HH.Size > 1 & HH.Size <= 5 ~ pmin(40 + (HH.Size - 1) * 15, 100),  
          SP_program_combo %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") & HH.Size >= 6 ~ 100,  
          TRUE ~ 0  
        )
      )
    
    data <- data %>%
      mutate(
        Total_Value_CT_1 = case_when(
          SP_program_combo %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") ~ Value_CT_1 + ifelse(Added_value == 1, 35, 0),
          TRUE ~ Value_CT_1
        )
      )
    
    
    data$Share_benefit_expenditure_CT_1 <- data$Total_Value_CT_1/data$Monthly_expenditure*100
    
    result_2 <- data %>%
      group_by(SP_program_combo) %>%
      summarize(
        count = n(),  
        FREQUENCY = (sum(weights, na.rm = TRUE)/Pop)*100  ,
      )
    
    
    result_3 <- data %>%
      group_by(Total_Value_CT_1) %>%
      summarize(
        Monthly_AVG_EXP_JOD = sum(Monthly_expenditure, na.rm = TRUE) /n(),
        Monthly_AVG_BENEFIT_SHARE_JOD = sum(Share_benefit_expenditure_CT_1 * weights, na.rm = TRUE) / n(),
        AVG_HH_SIZE = sum(HH.Size )/ n(),
        FREQUENCY = (n()/Pop)*100)
    
    Total_amount_to_be_paid_monthly_usd_CT1 <- sum(data$Total_Value_CT_1)/(10^6)*exc_rateJOD_USD
    
    
    ##Estimation after shock
    ## Applying inflation [5%] - one dimension shock at the demand level 
    ## Cost-Push Inflation (Supply Shock)
    ## For example, if there is a significant increase in the price of oil, transportation and production costs rise, which causes businesses to pass these costs onto consumers in the form of higher prices.
    ## 2/3 dimensions shocks - not only that also unemployment rise in some areas and another climate shock 
    
    PL_JOD_new <- PL_JOD*(1+shock_inflation/100)
    PL_new <- log(PL_JOD_new)
    data$poor_new <- ifelse(data$SCORE < PL_new, 1, 0)
    Poverty_new <- sum(data$poor_new)/nrow(data)*100
    Poverty_new
    result_new <- aggregate(poor_new ~ SP_program, data, function(x) c(total = length(x), poor_count = sum(x)))
    result_new <- data.frame(combination = result_new$SP_program,
                             total = result_new$poor_new[, "total"],
                             poor_count = result_new$poor_new[, "poor_count"])
    #colnames(result_new) <- gsub("^poor.", "", colnames(result_new))
    
    
    data$DELTA <- data$poor_new-data$poor
    
    data <- data %>%
      mutate(
        SP_program_combo_new = case_when(
          SP_program == "Beneficiary" & poor_new == 1 ~ "Beneficiary & poor",
          SP_program == "Beneficiary" & poor_new == 0 ~ "Beneficiary & non-poor",
          SP_program == "Eligible" & poor_new == 1 ~ "Eligible & poor",
          SP_program == "Eligible" & poor_new == 0 ~ "Eligible & non-poor",
          SP_program == "Non-eligible" & poor_new == 1 ~ "Non-Eligible & poor",
          TRUE ~ "Non-Eligible & non-poor"
        )
      )
    
    data$SP_program_combo_new <- factor(data$SP_program_combo_new, 
                                        levels = c("Beneficiary & poor", 
                                                   "Beneficiary & non-poor", 
                                                   "Eligible & poor", 
                                                   "Eligible & non-poor", 
                                                   "Non-Eligible & poor",
                                                   "Non-Eligible & non-poor"))
    
    
    
    
    
    ###Scenario 2: covering all the poor household after SHOCK
    data <- data %>%
      mutate(
        Value_CT_2 = case_when(
          SP_program_combo_new %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") & HH.Size == 1 ~ 40,  
          SP_program_combo_new %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") & HH.Size > 1 & HH.Size <= 5 ~ pmin(40 + (HH.Size - 1) * 15, 100),  
          SP_program_combo_new %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") & HH.Size >= 6 ~ 100,  
          TRUE ~ 0  
        )
      )
    
    data <- data %>%
      mutate(
        Total_Value_CT_2 = case_when(
          SP_program_combo_new  %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") ~ data$Value_CT_2 + ifelse(data$Added_value == 1, 35, 0),
          TRUE ~ Value_CT_2 
        )
      )
    
    data$Share_benefit_expenditure_CT_2 <- data$Total_Value_CT_2/data$Monthly_expenditure*100
    
    
    result_5 <- data %>%
      group_by(Total_Value_CT_2) %>%
      summarize(
        Monthly_AVG_EXP_JOD = sum(Monthly_expenditure , na.rm = TRUE) / n(),
        Monthly_AVG_BENEFIT_SHARE_JOD = sum(Share_benefit_expenditure_CT_2 , na.rm = TRUE) / n(),
        AVG_HH_SIZE = sum(HH.Size , na.rm = TRUE)/ n(),
        FREQUENCY = (n()/Pop)*100)
    
    Total_amount_to_be_paid_monthly_usd_CT_2 <- sum(data$Total_Value_CT_2)/(10^6)*exc_rateJOD_USD
    
    data$PL_monthly_JOD <- sapply(data$HH.Size, function(x) {
      if (x == 1) return(100)
      else if (x == 2) return(200)
      else if (x == 3) return(300)
      else if (x == 4) return(400)
      else if (x == 5) return(500)
      else if (x == 6) return(550)
      else if (x == 7) return(600)
      else if (x == 8) return(650)
      else if (x == 9) return(700)
      else return(750)  
    })
    
    data$poor_test <- ifelse(data$Monthly_expenditure < data$PL_monthly_JOD, 1, 0)
    Poverty_test <- sum(data$poor_test)/nrow(data)*100
    
    result_1a <- data %>%
      group_by(SP_program) %>%
      summarize(
        count = n(),  
        Freq = (n()/Pop)*100,
        value_monthly_usd_CT=sum(Total_Value_CT_0)/(10^6)*exc_rateJOD_USD,
      )
    
    
    result_2 <- data %>%
      group_by(SP_program_combo) %>%
      summarize(
        count = n(),  
        Freq = (n()/Pop)*100,
        value_monthly_usd_CT=sum(Total_Value_CT_1 )/(10^6)*exc_rateJOD_USD,
      ) 
    
    
    result_4 <- data %>%
      group_by(SP_program_combo_new) %>%
      summarize(
        count = n(),  
        Freq = (n()/Pop)*100,
        value_monthly_usd_CT=sum(Total_Value_CT_2 )/(10^6)*exc_rateJOD_USD,
      )
    
    # 1. Combine result_2 and result_4
    table1 <- full_join(result_2, result_4, 
                        by = c("SP_program_combo"=  "SP_program_combo_new"), 
                        suffix = c( "_pre", "_post"))
    
    # Calculate differences
    table1 <- table1 %>%
      mutate(
        count_diff = count_post - count_pre,
        FREQUENCY_diff = Freq_post - Freq_pre,
        value_monthly_usd_CT_diff = value_monthly_usd_CT_post- value_monthly_usd_CT_pre
      )
    
    
    
    Total_amount_to_be_paid_monthly_usd_CT0 <- sum(data$Total_Value_CT_0 * data$weights)/(10^6)*exc_rateJOD_USD
    Total_amount_to_be_paid_monthly_usd_CT1 <- sum(data$Total_Value_CT_1 * data$weights)/(10^6)*exc_rateJOD_USD
    Total_amount_to_be_paid_monthly_usd_CT2 <- sum(data$Total_Value_CT_2 * data$weights)/(10^6)*exc_rateJOD_USD
    
    
    
    
    
    BeneficiariesData$SCORE <-log(BeneficiariesData$Imputed_Income/BeneficiariesData$HH_Size) 
    EligiblesData$SCORE <-log(EligiblesData$Imputed_Income/EligiblesData$HH_Size) 
    Non_eligiblesData$SCORE <-log(Non_eligiblesData$Imputed_Income/Non_eligiblesData$HH_Size) 
    
    
    combined_data <- data.frame(
      SCORE = c(BeneficiariesData$SCORE, EligiblesData$SCORE, Non_eligiblesData$SCORE),
      Dataset = rep(c("Beneficiaries", "Eligibles","Non- Eligibles"), 
                    times = c(length(BeneficiariesData$SCORE), length(EligiblesData$SCORE), length(Non_eligiblesData$SCORE)))
    )
    
    
    
    dataCH3=data
    
    
    ###CLIMATE SHOCK ANALYSIS
    #Irbid Mafraq Amman Tafila Zarqa Balqa Maan Aqaba Karak Jerash Madaba Ajloun
    
    Gislocation <- paste0(inputLocation,"GIS")
    
    
    #dataCH2 <- dataCH2 %>%  rename(id_transfer= request_id)
    # dataCH3 <- dataCH3 %>%  rename(id_transfer= request_id)
    
    
    dataCH2 <- dataCH2 %>%
      mutate(Governorate = recode(Governorate, 
                                  "Tafila" = "Tafilah", 
                                  "Jerash" = "Jarash", 
                                  "Aljoun" = "Ajlun"))
    
    
    dataCH3 <- dataCH3 %>%
      mutate(Governorate = recode(Governorate, 
                                  "Tafila" = "Tafilah", 
                                  "Jerash" = "Jarash", 
                                  "Ajloun" = "Ajlun"))
    
    HIECS_data_common_F= dataCH3
    
    HIECS_data_common_F<- HIECS_data_common_F %>%
      rename_with(~ gsub(" ", ".", .x))
    
    
    tif_file <- paste0(Gislocation,"/VA_NC_Annual.tif")
    
    r <- raster(tif_file)
    
    raster_df <- as.data.frame(rasterToPoints(r), xy = TRUE)
    print(r)
    crs(r)
    
    
    jordan_governorates <- st_read(paste0(Gislocation,"/StatPlanet_Jordan/map/Jordan/JOR_adm1.shp"))
    
    
    colnames(jordan_governorates)
    head(jordan_governorates)
    crs(jordan_governorates)
    
    crs_raster <- crs(r)
    crs_governorates <- st_crs(jordan_governorates)
    
    if (crs_raster != crs_governorates) {
      jordan_governoratess <- st_transform(jordan_governorates, crs = crs_raster)
    }
    
    plot(r)
    plot(st_geometry(jordan_governoratess), add = TRUE, border = "black", lwd = 2)  
    
    
    # Load raster using terra
    raster_layer <- rast(paste0(Gislocation, "/VA_NC_Annual.tif"))
    
    # Match CRS from governorates
    crs_epsg <- st_crs(jordan_governorates)$epsg
    
    # Downsample raster to reduce resolution (if needed)
    raster_downsampled <- aggregate(raster_layer, fact = 5)
    
    # Project raster to same CRS as polygons
    raster_wgs84 <- project(raster_downsampled, paste0("EPSG:", crs_epsg))
    
    # Convert raster to data frame with coordinates
    raster_df <- as.data.frame(raster_wgs84, xy = TRUE, na.rm = TRUE)
    
    # Convert raster coordinates to sf points
    raster_points_sf <- st_as_sf(raster_df, coords = c("x", "y"), crs = st_crs(jordan_governorates))
    
    # Spatial join: attach polygon info to raster points
    raster_joined <- st_join(raster_points_sf, jordan_governorates[, c("ID_1")], left = FALSE)
    
    # Drop geometry to get clean data frame
    raster_with_polygon_ids <- raster_joined %>%
      st_drop_geometry() %>%
      rename(polygon_id = ID_1)
    
    # Compute average VA per polygon
    average_df <- raster_with_polygon_ids %>%
      filter(!is.na(VA_NC_Annual)) %>%
      group_by(polygon_id) %>%
      summarize(VA_NC_Annual = mean(VA_NC_Annual, na.rm = TRUE))
    
    
    average_df_cleaned <- average_df %>%
      filter(!is.na(polygon_id))
    
    print(average_df_cleaned)
    
    ### Database with the Vulnerability Assessment
    jordan_governorates_with_VA <- jordan_governorates %>%
      left_join(average_df_cleaned, by = c("ID_1" = "polygon_id"))
    
    centroids <- st_centroid(jordan_governorates_with_VA)
    
    
    
    Prep <- jordan_governorates_with_VA[,c(5,17)]
    Prep <- as.data.frame(Prep)
    Prep <- Prep[, -c(3)]
    colnames(Prep)[1] <- "Region"
    
    
    Prep$Region[8] <- "Maan"
    
    region_counts <- HIECS_data_common_F %>% 
      count(Governorate)
    
    Combine <- cbind(Prep, region_counts)
    Combine <- Combine[,-c(3)]
    
    total_n <- sum(Combine$n)
    
    Combine$frequency <- (Combine$n / total_n) * 100
    
    ##Categorization of the risk // Parameter based on SCORE estimation
    Combine <- Combine %>%
      mutate(Asset_risk = case_when(
        VA_NC_Annual >= 7 ~ "High_risk",
        VA_NC_Annual >= 5 ~ "Moderate_risk",
        TRUE ~ "Low_risk"
      ))
    
    
    
    HIECS_data_common_F <- HIECS_data_common_F %>%
      left_join(Combine, by = c("Governorate" = "Region"))
    
    
    
    # Create shock parameters list
    shock_params <- list(
      livestock_loss = input$livestockLoss,
      land_affected = input$landAffected,
      job_loss = input$jobLoss,
      car_sale = input$carSale,
      stock_sale = input$stockSale,
      commercial_property_sale = input$propertySale
    )
    
    # Step 2: Define risk weights
    # Create risk weights list
    risk_weights <- list(
      "High_risk" = input$highRiskLoss,
      "Moderate_risk" = input$moderateRiskLoss,
      "Low_risk" = input$lowRiskLoss)
    
    
    
    # Step 1: Assign random values to shock parameters
    "   shock_params <- list(
      livestock_loss = runif(1, 0, 1),
      land_affected = runif(1, 0, 1),
      job_loss = runif(1, 0, 1),
      car_sale = runif(1, 0, 1),
      stock_sale = runif(1, 0, 1),
      commercial_property_sale = runif(1, 0, 1)
    )
    "
    # Step 2: Define risk weights
    # Create risk weights list
    #  risk_weights <- list(
    #   "High_risk" = 0.1,
    #  "Moderate_risk" = 0.1,
    # "Low_risk" = 0.1
    #)
    
    
    
    #Assets risk is high but also affect mainly to the poorest?
    HIECS_data_common_F <- HIECS_data_common_F %>%
      mutate(Risk_loosing_asset = case_when(
        Asset_risk == "High_risk" & SCORE < 5.5 ~ 0.4,
        Asset_risk == "Moderate_risk" & SCORE < 5.5 ~ 0.3,
        Asset_risk == "Low_risk" & SCORE < 5.5 ~ 0.2,
        TRUE ~ 0  
      ))
    
    
    
    apply_binary_shock <- function(df, varname, newvarname, base_prob, risk_weights) {
      # Convert risk_weights to a named vector for easier access
      risk_weights_vec <- unlist(risk_weights)
      
      df <- df %>%
        rowwise() %>%
        mutate(
          !!sym(newvarname) := {
            if (get(varname) == 1) {
              weight <- risk_weights_vec[as.character(Asset_risk)]
              weight <- ifelse(is.na(weight), 0, weight)
              rbinom(1, 1, prob = base_prob * weight)
            } else {
              0
            }
          }
        ) %>%
        ungroup()
      
      return(df)
    }
    
    
    
    
    # Step 4: Numeric reduction function (for income earners)
    apply_income_shock <- function(df, varname, newvarname, base_prob, risk_weights) {
      # Convert risk_weights to a named vector for easier access
      risk_weights_vec <- unlist(risk_weights)
      
      df <- df %>%
        mutate(
          !!sym(newvarname) := case_when(
            !!sym(varname) > 0 ~ {
              # Safely get the weight value
              weight <- ifelse(as.character(Asset_risk) %in% names(risk_weights_vec),
                               risk_weights_vec[as.character(Asset_risk)],
                               0)
              shock <- rbinom(n(), 1, prob = base_prob * weight)
              pmax(!!sym(varname) - shock, 0)
            },
            TRUE ~ !!sym(varname)
          )
        )
      return(df)
    }
    
    
    
    validate(
      need(all(unique(HIECS_data_common_F$Asset_risk) %in% names(risk_weights)),
           "Some Asset_risk values don't have corresponding weights")
    )
    
    # Step 5: Apply shocks to your dataset 
    set.seed(123)
    
    
    HIECS_data_common_F_shocked <- HIECS_data_common_F %>%
      filter(!is.na(Asset_risk)) %>% 
      mutate(Asset_risk = as.character(Asset_risk)) %>%
      apply_binary_shock("HH.owns.livestock", "HH.owns.livestock_postshock", 1 - shock_params$livestock_loss, risk_weights) %>%
      apply_binary_shock("HH.cultivates.land", "HH.cultivates.land_postshock", 1 - shock_params$land_affected, risk_weights) %>%
      apply_binary_shock("Owns.a.car", "Owns.a.car_postshock", 1 - shock_params$car_sale, risk_weights) %>%
      apply_binary_shock("Owns.stocks", "Owns.stocks_postshock", 1 - shock_params$stock_sale, risk_weights) %>%
      apply_binary_shock("Owns.commercial.property", "Owns.commercial.property_postshock", 1 - shock_params$commercial_property_sale, risk_weights) %>%
      apply_income_shock("Formal.income.earners", "Formal.income.earners_postshock", shock_params$job_loss, risk_weights)
    
    # Step 6: Summary comparison (optional)
    summary_table_changes_vars <- HIECS_data_common_F_shocked %>%
      group_by(SP_program) %>%
      
      summarise(
        Livestock_before = sum(HH.owns.livestock, na.rm = TRUE),
        Livestock_after = sum(HH.owns.livestock_postshock, na.rm = TRUE),
        Cultivation_before = sum(HH.cultivates.land, na.rm = TRUE),
        Cultivation_after = sum(HH.cultivates.land_postshock, na.rm = TRUE),
        Cars_before = sum(Owns.a.car, na.rm = TRUE),
        Cars_after = sum(Owns.a.car_postshock, na.rm = TRUE),
        Income_before = sum(Formal.income.earners, na.rm = TRUE),
        Income_after = sum(Formal.income.earners_postshock, na.rm = TRUE)
      )
    
    
    
    
    # Step 1: Ensure squared variables exist
    HIECS_data_common_F_shocked <- HIECS_data_common_F_shocked %>%
      mutate(
        HH.Size_sq = HH.Size^2,
        Age.newest.car_sq = Age.newest.car^2,
        Formal.income.earners_sq = Formal.income.earners_postshock^2
      )
    
    # Step 1: Create governorate dummy variables (reference = Irbid)
    HIECS_data_common_F_shocked <- HIECS_data_common_F_shocked %>%
      mutate(
        Governorate_Mafraq = ifelse(Governorate == "Mafraq", 1, 0),
        Governorate_Amman = ifelse(Governorate == "Amman", 1, 0),
        Governorate_Tafilah = ifelse(Governorate == "Tafila", 1, 0), # add if in other dataset
        Governorate_Zarqa = ifelse(Governorate == "Zarqa", 1, 0),
        Governorate_Balqa = ifelse(Governorate == "Balqa", 1, 0),
        Governorate_Maan = ifelse(Governorate == "Maan", 1, 0),
        Governorate_Aqaba = ifelse(Governorate == "Aqaba", 1, 0),
        Governorate_Karak = ifelse(Governorate == "Karak", 1, 0),
        Governorate_Jarash = ifelse(Governorate == "Jerash", 1, 0), # only if needed
        Governorate_Madaba = ifelse(Governorate == "Madaba", 1, 0),
        Governorate_Ajlun = ifelse(Governorate == "Ajloun", 1, 0)  # only if needed
      )
    
    ##⚠️ Change with the new PMT
    # Step 2: Calculate postshock score using dot notation
    HIECS_data_common_F_shocked <- HIECS_data_common_F_shocked %>%
      mutate(
        score_postshock =
          4.6424438743 +
          0.0664108035 * HH.30y.or.less +
          0.1255826359 * HH.Female.Married +
          0.0745691391 * HH.65y.or.more +
          #0.0000071991 * NAF.beneficiary.status +#
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
          -0.3311827577 * HH.owns.livestock_postshock +
          0.0543048142 * Imputed.livestock.productivity +
          0.1069436887 * HH.owns.land +
          # HH.cultivates.land excluded (dropped in model)
          0.4435800352 * Owns.a.car_postshock +
          0.1643461081 * Private.cars +
          -0.0274362948 * Age.newest.car +
          0.0004164393 * Age.newest.car_sq +
          0.1368038748 * Working.cars +
          0.0669149014 * Owns.commercial.property_postshock +
          0.2114789823 * Owns.stocks_postshock +
          0.0524404940 * House.type +
          0.0059023126 * Area.per.capita +
          0.1496706129 * Water.and.electricity.bills +
          -0.0725899939 * HH.lives.in.rural.area +
          0.0755378872 * Formal.income.earners_postshock +
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
      )
    
    # Step 3: Compute difference between pre and post scores
    HIECS_data_common_F_shocked <- HIECS_data_common_F_shocked %>%
      mutate(delta_score = score_postshock - SCORE)
    
    
    summary_stats <- HIECS_data_common_F_shocked %>%
      group_by(SP_program)%>%
      summarise(
        avg_score_pre = mean(SCORE, na.rm = TRUE),
        avg_score_post = mean(score_postshock, na.rm = TRUE),
        avg_change = mean(delta_score, na.rm = TRUE),
        min_change = min(delta_score, na.rm = TRUE),
        max_change = max(delta_score, na.rm = TRUE)
      )
    
    print(summary_stats)
    
    
    
    ###Estimating the impact on poverty. 
    PL <- closest_threshold
    #Monthly per capita poverty line (expenditure based)
    PL_monthly <- exp(PL)
    # while noting that the international poverty line in Jordan is equivalent to 40 JOD
    data2= HIECS_data_common_F_shocked
    PL_JOD <- exp(PL)
    
    poverty_by_region <- data2 %>%
      mutate(poor = ifelse(SCORE < PL, 1, 0)) %>%
      group_by(Governorate) %>%
      summarise(Poverty = sum(poor * weights) / sum(weights) * 100)
    
    poverty_by_region
    
    
    data2$SCORE_NEW_climate <- data2$score_postshock
    data2$poor_new_climate <- ifelse(data2$SCORE_NEW_climate < PL, 1, 0)
    Poverty_new_climate <- sum(data2$poor_new_climate*data2$weights)/sum(data2$weights)*100
    
    Poverty
    
    
    poverty_by_region_NEW <- data2 %>%
      mutate(poor_new_climate = ifelse(SCORE_NEW_climate < PL, 1, 0)) %>%
      group_by(Governorate) %>%
      summarise(Poverty = sum(poor_new_climate * weights) / sum(weights) * 100)
    
    poverty_by_region
    poverty_by_region_NEW
    
    
    
    
    data2 <- data2 %>%
      mutate(
        SP_program_combo_new_climate = case_when(
          SP_program == "Beneficiary" & poor_new_climate == 1 ~ "Beneficiary & poor",
          SP_program == "Beneficiary" & poor_new_climate == 0 ~ "Beneficiary & non-poor",
          SP_program== "Eligible" & poor_new_climate == 1 ~ "Eligible & poor",
          SP_program== "Eligible" & poor_new_climate == 0 ~ "Eligible & non-poor",
          SP_program == "Non-eligible" & poor_new_climate == 1 ~ "Non-Eligible & poor",
          TRUE ~ "Non-Eligible & non-poor"
        )
      )
    
    data2$SP_program_combo_new_climate <- factor(data2$SP_program_combo_new_climate, 
                                                 levels = c("Beneficiary & poor", 
                                                            "Beneficiary & non-poor", 
                                                            "Eligible & poor", 
                                                            "Eligible & non-poor", 
                                                            "Non-Eligible & poor",
                                                            
                                                            "Non-Eligible & non-poor"))
    
    data2 <- data2 %>%
      mutate(
        Value_CT_3 = case_when(
          SP_program_combo_new_climate %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") & HH.Size == 1 ~ 40,  
          SP_program_combo_new_climate %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") & HH.Size > 1 & HH.Size <= 5 ~ pmin(40 + (HH.Size - 1) * 15, 100),  
          SP_program_combo_new_climate %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") & HH.Size >= 6 ~ 100,  
          TRUE ~ 0  
        )
      )
    
    data2 <- data2 %>%
      mutate(
        Total_Value_CT_3 = case_when(
          SP_program_combo_new_climate  %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") ~ data2$Value_CT_3 + ifelse(data2$Added_value == 1, 35, 0),
          TRUE ~ Value_CT_3 
        )
      )
    
    data2$Share_benefit_expenditure_CT_3 <- data2$Total_Value_CT_3/data2$Monthly_expenditure*100
    
    
    #########################ECONOMIC SHOCK
    
    cumulative_effect_1pct <- employment_impact()
    
    
    source(paste0(inputLocation,"Models/Unemployment/Labor_characteristics.R"))
    #   source(paste0(inputLocation,"Models/Unemployment/MacroEconShock.R"))
    
    
    data3 = data2
    
    
    
    asset_vars <- data3 %>%
      dplyr::select(
        Owns.a.car,
        Owns.commercial.property,
        Owns.stocks,
        HH.owns.livestock,
        HH.owns.land,
        Area.per.capita,
        Water.and.electricity.bills,
        House.type
      )
    
    asset_vars_clean <- asset_vars %>%
      drop_na() %>%
      mutate_all(~as.numeric(.)) %>%  
      scale()                         
    
    pca_result <- principal(asset_vars_clean, nfactors = 1, rotate = "none")
    
    data3$wealth_quintile <- NA
    data3$wealth_quintile[complete.cases(asset_vars)] <- as.numeric(pca_result$scores)
    
    data3 <- data3 %>%
      mutate(wealth_quintile = ntile(wealth_quintile, 5))
    
    varNames=colnames(ApplicantsData_back)
    
    case1=varNames%in%c("Employment_Status_Name",
                        "Education_Level_Name",
                        "Gender_Code",
                        "HH_Size",
                        "head_age",
                        "GOV_DESC")
    
    case2=startsWith(varNames,"request_id")
    
    chosenVariables=varNames[case1|case2]
    
    ApplicantsData=ApplicantsData_back%>%
      dplyr::select(all_of(chosenVariables))%>%
      drop_na()
    
    cleanDataX <- ApplicantsData
    
    cleanDataX$v202 <- ifelse(
      cleanDataX$Employment_Status_Name %in% c("Business Owner", "Employed", "Irregular", "Regular"),
      1,
      0
    )
    
    cleanDataX$sex <- ifelse(cleanDataX$Gender_Code == "Male", 1, 2)
    
    cleanDataX$hh_size <- cleanDataX$HH_Size
    
    cleanDataX$educ_level1 <- dplyr::recode(as.character(cleanDataX$Education_Level_Name),
                                            "Illiterate" = 1,
                                            "Read & Write" = 2,
                                            "Primary" = 3,
                                            "Basic Education" = 3,
                                            "Failed secondary" = 4,
                                            "Secondary" = 4,
                                            "Technical and vocational Bachelor" = 5,
                                            "University" = 6,
                                            "Graduate studies" = 7,
                                            .default = NA_real_)
    
    cleanDataX$educ_level1[is.na(cleanDataX$educ_level1)] <- 1
    
    gov_name_to_code <- c(
      "Amman"   = 11,
      "Balqa"   = 12,
      "Zarqa"   = 13,
      "Madaba"  = 14,
      "Irbid"   = 21,
      "Mafraq"  = 22,
      "Jarash"  = 23, 
      "Ajloun"  = 24,
      "Karak"   = 31,
      "Tafileh" = 32,  
      "Ma'an"   = 33,  
      "Aqaba"   = 34,
      "Other"       = 97,
      "Don't Know"  = 98
    )
    
    cleanDataX <- cleanDataX %>%
      mutate(GOV_DESC_matched = recode(as.character(GOV_DESC),
                                       "Jerash" = "Jarash",
                                       "Tafila" = "Tafileh",
                                       "Maan"   = "Ma'an"),
             governorate = gov_name_to_code[GOV_DESC_matched])
    
    cleanDataX$age <- cleanDataX$head_age
    
    selected_vars <- c("request_id", "v202", "sex", "hh_size", "educ_level1", "governorate", "age")
    
    cleanData_selected <- cleanDataX %>%
      dplyr::select(all_of(selected_vars))
    
    cols_to_add <- cleanData_selected %>%
      dplyr::select(request_id, v202, sex, hh_size, educ_level1, governorate, age)
    
    
    data3 <- data3 %>%
      left_join(cols_to_add, by =  "request_id") 
    #, by = c("id_transfer" = "request_id"))
    
    
    #cc <- data3 %>%
    #     count(id_transfer) %>%
    #     filter(n > 0)
    
    
    
    
    data3$probit_predicted_prob <- predict(probit_model, newdata = data3, type = "response")
    
    data3$probit_predicted_class <- ifelse(data3$probit_predicted_prob > 0.5, 1, 0)
    
    cm <- table(Predicted = data3$probit_predicted_class, Actual = data3$v202)
    cm
    
    accuracy    <- sum(diag(cm)) / sum(cm)
    sensitivity <- cm["1", "1"] / sum(cm[, "1"])  
    specificity <- cm["0", "0"] / sum(cm[, "0"])  
    
    cat("Accuracy: ", round(accuracy, 3), "\n")
    cat("Sensitivity (TPR): ", round(sensitivity, 3), "\n")
    cat("Specificity (TNR): ", round(specificity, 3), "\n")
    
    
    apply_income_shock_deterministic <- function(df, varname, newvarname, base_prob) {
      df <- df %>%
        mutate(row_id = row_number())  
      
      # Elligible criteria: Focus on households that contain employed individuals and those that are non-beneficiaries from SP programs
      df_to_shock <- df %>%
        filter(!!sym(varname) > 0, SP_program != "Beneficiary")
      
      # How many rows to shock? 10% of the those who have emloyed individuals, regardless of SP program status, because simply anyone employed can lose its job, regardless if they are benefiting or not from an SP program 
      n_shock <- floor(sum(df$Formal.income.earners[df$Formal.income.earners > 0]) * base_prob)
      
      shock_ids <- df_to_shock %>%
        arrange(risk_weights_1) %>%
        slice_head(n = n_shock) %>%
        pull(row_id)
      
      # Apply the shock: subtract 1 employed individual
      df <- df %>%
        mutate(
          !!sym(newvarname) := if_else(
            row_id %in% shock_ids,
            pmax(!!sym(varname) - 1, 0),
            !!sym(varname)
          )
        ) %>%
        dplyr::select(-row_id)  
      
      return(df)
    }
    
    
    set.seed(123)  
    
    data3$risk_weights_1 <- data3$probit_predicted_prob
    
    
    
    print(cumulative_effect_1pct*-1)
    
    data3_shocked <- apply_income_shock_deterministic(
      df = data3,
      varname = "Formal.income.earners",
      newvarname = "Formal.income.earners_postshock_SC4",
      base_prob = (cumulative_effect_1pct*-1) 
    )
    
    data3_shocked <- data3_shocked %>%
      mutate(Formal.income.earners_sq_shocked_SC4 = Formal.income.earners_postshock_SC4^2)
    
    HIECS_data_common_F_shocked_SC4 <- data3_shocked %>%
      mutate(
        score_postshock_SC4 =
          4.6424438743 +
          0.0664108035 * HH.30y.or.less +
          0.1255826359 * HH.Female.Married +
          0.0745691391 * HH.65y.or.more +
          #0.0000071991 * NAF.beneficiary.status +#
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
          # HH.cultivates.land excluded (dropped in model)
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
          -0.0108665062 * Formal.income.earners_sq + # There is an issue in the coefficient
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
      )
    
    
    # Step 3: Compute difference between pre and post scores
    HIECS_data_common_F_shocked_SC4 <- HIECS_data_common_F_shocked_SC4 %>%
      mutate(delta_score_SC4 = score_postshock_SC4 - SCORE)
    
    summary_stats <- HIECS_data_common_F_shocked_SC4 %>%
      group_by(SP_program)%>%
      summarise(
        avg_score_pre = mean(SCORE, na.rm = TRUE),
        avg_score_post = mean(score_postshock_SC4, na.rm = TRUE),
        avg_change = mean(delta_score_SC4, na.rm = TRUE),
        min_change = min(delta_score_SC4, na.rm = TRUE),
        max_change = max(delta_score_SC4, na.rm = TRUE)
      )
    
    print(summary_stats)
    sum(HIECS_data_common_F_shocked_SC4$SCORE)
    sum(HIECS_data_common_F_shocked_SC4$score_postshock_SC4)
    
    
    
    #Monthly per capita poverty line (expenditure based)
    PL_monthly <- exp(PL)
    # while noting that the international poverty line in Jordan is equivalent to 40 JOD
    data2= HIECS_data_common_F_shocked_SC4
    PL_JOD <- exp(PL)
    
    poverty_by_region <- data2 %>%
      mutate(poor = ifelse(SCORE < PL, 1, 0)) %>%
      group_by(Governorate) %>%
      summarise(Poverty = sum(poor * weights) / sum(weights) * 100)
    
    poverty_by_region
    
    
    data2$SCORE_NEW_EMP<- data2$score_postshock_SC4
    data2$poor_new_EMP <- ifelse(data2$SCORE_NEW_EMP < PL, 1, 0)
    Poverty_new_EMP <- sum(data2$poor_new_EMP*data2$weights)/sum(data2$weights)*100
    
    poverty_by_region_NEW <- data2 %>%
      mutate(poor_new_EMP = ifelse(SCORE_NEW_EMP < PL, 1, 0)) %>%
      group_by(Governorate) %>%
      summarise(Poverty = sum(poor_new_EMP * weights) / sum(weights) * 100)
    
    poverty_by_region
    poverty_by_region_NEW
    
    
    
    data2 <- data2 %>%
      mutate(
        SP_program_combo_new_EMP = case_when(
          SP_program == "Beneficiary" & poor_new_EMP == 1 ~ "Beneficiary & poor",
          SP_program == "Beneficiary" & poor_new_EMP == 0 ~ "Beneficiary & non-poor",
          SP_program== "Eligible" & poor_new_EMP == 1 ~ "Eligible & poor",
          SP_program== "Eligible" & poor_new_EMP == 0 ~ "Eligible & non-poor",
          SP_program == "Non-eligible" & poor_new_EMP == 1 ~ "Non-Eligible & poor",
          TRUE ~ "Non-Eligible & non-poor"
        )
      )
    
    data2$SP_program_combo_new_EMP <- factor(data2$SP_program_combo_new_EMP, 
                                             levels = c("Beneficiary & poor", 
                                                        "Beneficiary & non-poor", 
                                                        "Eligible & poor", 
                                                        "Eligible & non-poor", 
                                                        "Non-Eligible & poor",
                                                        "Non-Eligible & non-poor"))
    
    data2 <- data2 %>%
      mutate(
        Value_CT_4 = case_when(
          SP_program_combo_new_EMP %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") & HH.Size == 1 ~ 40,  
          SP_program_combo_new_EMP %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") & HH.Size > 1 & HH.Size <= 5 ~ pmin(40 + (HH.Size - 1) * 15, 100),  
          SP_program_combo_new_EMP %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") & HH.Size >= 6 ~ 100,  
          TRUE ~ 0  
        )
      )
    
    data2 <- data2 %>%
      mutate(
        Total_Value_CT_4 = case_when(
          SP_program_combo_new_EMP  %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") ~ data2$Value_CT_4 + ifelse(data2$Added_value == 1, 35, 0),
          TRUE ~ Value_CT_4 
        )
      )
    
    data2$Share_benefit_expenditure_CT_4 <- data2$Total_Value_CT_4/data2$Monthly_expenditure*100
    
    
    
    data2 <- data2 %>%
      mutate(
        # Scenario 0: Original SP_program
        Scenario_0 = case_when(
          SP_program == "Beneficiary" ~ "UCT Beneficiaries",
          TRUE ~ "Non-UCT Beneficiaries"
        ),
        
        # Scenario 1: SP_program_combo (PMT + Poverty)
        Scenario_1 = case_when(
          SP_program_combo %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") ~ "UCT Beneficiaries",
          TRUE ~ "Non-UCT Beneficiaries"
        ),
        
        # Scenario 2: SP_program_combo_new (PMT + Poverty Inflation shocked)
        Scenario_2 = case_when(
          SP_program_combo_new %in% c("Beneficiary & poor", "Beneficiary & non-poor", "Eligible & poor", "Non-Eligible & poor") ~ "UCT Beneficiaries",
          TRUE ~ "Non-UCT Beneficiaries"
        ),
        # Scenario 3: SP_program_combo_new_climate (Water scarcity shock)
        Scenario_3 = case_when(
          SP_program_combo_new_climate %in% c("Beneficiary & poor", "Beneficiary & non-poor", 
                                              "Eligible & poor", "Non-Eligible & poor") ~ "UCT Beneficiaries",
          TRUE ~ "Non-UCT Beneficiaries"
        ), 
        # Scenario 4: SP_program_combo_new_EMP
        Scenario_4 = case_when(
          SP_program_combo_new_EMP %in% c("Beneficiary & poor", "Beneficiary & non-poor", 
                                          "Eligible & poor", "Non-Eligible & poor") ~ "UCT Beneficiaries",
          TRUE ~ "Non-UCT Beneficiaries"
        )
        
      )
    
    
    Pop <- nrow(data2)
    
    # Now create the summary table for all three scenarios
    results <- bind_rows(
      # Scenario 0
      data2 %>%
        group_by(Scenario = "Scenario 0", Category = Scenario_0) %>%
        summarize(
          count = n(),  
          
          Freq = (n()/Pop)*100,
          value_yearly_jod_CT = sum(Total_Value_CT_0 )/(10^6)*12,
          value_yearly_usd_CT = sum(Total_Value_CT_0 )/(10^6)*exc_rateJOD_USD*12,
          .groups = "drop"
        ),
      
      # Scenario 1
      data2 %>%
        group_by(Scenario = "Scenario 1", Category = Scenario_1) %>%
        summarize(
          count = n(),  
          
          Freq = (n()/Pop)*100,
          value_yearly_jod_CT = sum(Total_Value_CT_1 * weights)/(10^6)*12,
          value_yearly_usd_CT = sum(Total_Value_CT_1 * weights)/(10^6)*exc_rateJOD_USD*12,
          .groups = "drop"
        ),
      
      # Scenario 2
      data2 %>%
        group_by(Scenario = "Scenario 2", Category = Scenario_2) %>%
        summarize(
          count = n(),  
          
          Freq = (n()/Pop)*100,
          value_yearly_jod_CT = sum(Total_Value_CT_2 * weights)/(10^6)*12,
          value_yearly_usd_CT = sum(Total_Value_CT_2 * weights)/(10^6)*exc_rateJOD_USD*12,
          .groups = "drop"
        ),
      
      # Scenario 3
      data2 %>%
        group_by(Scenario = "Scenario 3", Category = Scenario_3) %>%
        summarize(
          count = n(),  
          
          Freq = (n()/Pop)*100,
          value_yearly_jod_CT = sum(Total_Value_CT_3 * weights)/(10^6)*12,
          value_yearly_usd_CT = sum(Total_Value_CT_3 * weights)/(10^6)*exc_rateJOD_USD*12,
          .groups = "drop"
        ), 
      #  Scenario 4 
      data2 %>%
        group_by(Scenario = "Scenario 4", Category = Scenario_4) %>%
        summarize(
          count = n(),  
          
          Freq = (n()/Pop)*100,
          value_yearly_jod_CT = sum(Total_Value_CT_4 * weights)/(10^6)*12,
          value_yearly_usd_CT = sum(Total_Value_CT_4 * weights)/(10^6)*exc_rateJOD_USD*12,
          .groups = "drop"
        )
      
    )
    
    
    
    results <- results %>%  
      mutate(
        Scenario_name = case_when(
          Scenario == "Scenario 0" ~ "Current status",
          Scenario == "Scenario 1" ~ "Households under poverty line",
          Scenario == "Scenario 2" ~ paste0("Households under PL after inflation shock ", shock_inflation, "%"),
          Scenario == "Scenario 3" ~ "WaterShock",
          TRUE ~ if (input$employment_input_method == "gdp") {
            paste0("GDP shock ", input$increase_gdp_percap, "%")
          } else {
            paste0("Unemployment shock ", input$cumulative_effect_1pct_user, "percentage points")
          }
        )
      ) %>%
      relocate(Scenario_name, .before = 1) %>%
      filter(Category == "UCT Beneficiaries")
    
    
    
    
    
    simResults$finalTable <- results %>%
      mutate(variation_count = ifelse(Scenario_name == "Current status", 0, 
                                      (count - count[Scenario_name == "Current status"]) / count[Scenario_name == "Current status"] * 100))%>%
      dplyr:: select (Scenario_name,  count, Freq, variation_count, value_yearly_jod_CT, value_yearly_usd_CT) 
    
    
    
    # Now create the summary table for all three scenarios with governorate breakdown
    results_gov <- bind_rows(
      # Scenario 0
      data2 %>%
        filter(Scenario_0 == "UCT Beneficiaries") %>%
        group_by(Scenario = "Scenario 0", Governorate, Category = Scenario_0) %>%
        summarize(
          count = n(),
          value_yearly_jod_CT = sum(Total_Value_CT_0)/(10^6)*12,
          value_yearly_usd_CT = sum(Total_Value_CT_0)/(10^6)*exc_rateJOD_USD*12,
          .groups = "drop"
        ) %>%
        group_by(Scenario) %>%
        mutate(Freq = (count/sum(count))*100),
      
      # Scenario 1
      data2 %>%
        filter(Scenario_1 == "UCT Beneficiaries") %>%
        group_by(Scenario = "Scenario 1", Governorate, Category = Scenario_1) %>%
        summarize(
          count = n(),
          value_yearly_jod_CT = sum(Total_Value_CT_1 * weights)/(10^6)*12,
          value_yearly_usd_CT = sum(Total_Value_CT_1 * weights)/(10^6)*exc_rateJOD_USD*12,
          .groups = "drop"
        ) %>%
        group_by(Scenario) %>%
        mutate(Freq = (count/sum(count))*100),
      
      # Scenario 2
      data2 %>%
        filter(Scenario_2 == "UCT Beneficiaries") %>%
        group_by(Scenario = "Scenario 2", Governorate, Category = Scenario_2) %>%
        summarize(
          count = n(),
          value_yearly_jod_CT = sum(Total_Value_CT_2 * weights)/(10^6)*12,
          value_yearly_usd_CT = sum(Total_Value_CT_2 * weights)/(10^6)*exc_rateJOD_USD*12,
          .groups = "drop"
        ) %>%
        group_by(Scenario) %>%
        mutate(Freq = (count/sum(count))*100),
      
      # Scenario 3
      data2 %>%
        filter(Scenario_3 == "UCT Beneficiaries") %>%
        group_by(Scenario = "Scenario 3", Governorate, Category = Scenario_3) %>%
        summarize(
          count = n(),
          value_yearly_jod_CT = sum(Total_Value_CT_3 * weights)/(10^6)*12,
          value_yearly_usd_CT = sum(Total_Value_CT_3 * weights)/(10^6)*exc_rateJOD_USD*12,
          .groups = "drop"
        ) %>%
        group_by(Scenario) %>%
        mutate(Freq = (count/sum(count))*100),
      
      # Scenario 4
      data2 %>% 
        filter(Scenario_4 == "UCT Beneficiaries") %>%
        group_by(Scenario = "Scenario 4", Governorate, Category = Scenario_4) %>%
        summarize(
          count = n(),
          value_yearly_jod_CT = sum(Total_Value_CT_4 * weights)/(10^6)*12,
          value_yearly_usd_CT = sum(Total_Value_CT_4 * weights)/(10^6)*exc_rateJOD_USD*12,
          .groups = "drop"
        ) %>%
        group_by(Scenario) %>%
        mutate(Freq = (count/sum(count))*100)
    ) %>%  
      mutate(
        Scenario_name = case_when(
          Scenario == "Scenario 0" ~ "Current status",
          Scenario == "Scenario 1" ~ "Households under poverty line",
          Scenario == "Scenario 2" ~ paste0("Households under PL after inflation shock ", shock_inflation, "%"),
          Scenario == "Scenario 3" ~ "WaterShock",
          TRUE ~ if (input$employment_input_method == "gdp") {
            paste0("GDP shock ", input$increase_gdp_percap, "%")
          } else {
            paste0("Unemployment shock ", input$cumulative_effect_1pct_user, "percentage points")
          }
          
        )
      ) %>%
      relocate(Scenario_name, .before = 1) %>%
      filter(Category == "UCT Beneficiaries")
    
    
    jordan_governorates_with_VA$NAME_1[8] <- "Maan"
    VA_data <- jordan_governorates_with_VA |>
      st_drop_geometry() |>
      dplyr::select(Governorate = NAME_1, VA_NC_Annual)
    
    
    
    simResults$finalTable_gov <- results_gov %>%
      group_by(Governorate) %>%
      mutate(variation_count = ifelse(Scenario_name == "Current status", 0, 
                                      (count - count[Scenario_name == "Current status"]) / count[Scenario_name == "Current status"] * 100))%>%
      left_join(VA_data, by = "Governorate")%>%
      mutate (Governorate, recode(Governorate, "Ma`an"="Maan" )) %>% 
      dplyr:: select (Scenario_name, Governorate, count, Freq, variation_count, value_yearly_jod_CT, value_yearly_usd_CT, VA_NC_Annual) 
    
    showNotification("Simulation complete!", type = "message")
    
    print("todo va bien") 
  }
  
  )
  
  
  
  #######OUTPUT######
  
  # Dashboard value boxes
  
  # Static map for Current Status (not affected by scenario filter)
  output$BeneficiariesMap_current <- renderLeaflet({
    req(simResults$finalTable_gov)
    req(translated_title())
    
    jordan_path <- paste0(Gislocation, "/StatPlanet_Jordan/map/Jordan/JOR_adm1.shp")
    if (!file.exists(jordan_path)) {
      return(leaflet() %>% addControl("Jordan shapefile not found", position = "topright"))
    }
    
    # Filter for Current status directly (no reactive input needed)
    current_data <- simResults$finalTable_gov %>%
      filter(Scenario_name == "Current status") %>%
      mutate(Governorate = recode(Governorate,
                                  "Tafila" = "Tafilah",
                                  "Jerash" = "Jarash",
                                  "Ajloun" = "Ajlun",
                                  "Maan" = "Ma`an"))
    
    jordan <- st_read(jordan_path) %>%
      mutate(NAME_1 = recode(NAME_1,
                             "Tafila" = "Tafilah",
                             "Jerash" = "Jarash",
                             "Aljoun" = "Ajlun",
                             "Maan"  = "Ma`an"
      )) %>%
      left_join(current_data, by = c("NAME_1" = "Governorate"))
    
    pal <- colorBin("viridis", domain = jordan$count, bins = 7, na.color = "#f0f0f0")
    
    leaflet(jordan) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(count),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste(NAME_1, "beneficiaries:", round(count, 1), ", % Total benef: ", round(Freq,1))
      ) %>%
      addLegend(
        pal = pal,
        values = jordan$count,
        opacity = 0.7,
        title = translated_title(),  # MUST call the reactive with ()
        position = "bottomright"
      )
  })
  
  
  
  output$currentBeneficiaries <- renderValueBox({
    if (is.null(simResults$finalTable)) {
      value <- 0
    } else {
      value <- formatC(simResults$finalTable$count[1], big.mark = ",")
    }
    valueBox(
      value, 
      textOutput("newtranslation27"),
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$currentCostJOD <- renderValueBox({
    if (is.null(simResults$finalTable)) {
      value <- 0
    } else {
      value <- formatC(simResults$finalTable$value_yearly_jod_CT[1], digits = 1, format = "f")
    }
    valueBox(
      paste0(value, "M"), 
      textOutput("newtranslation28"),
      icon = icon("money-bill"),
      color = "green"
    )
  })
  
  output$currentCostUSD <- renderValueBox({
    if (is.null(simResults$finalTable)) {
      value <- 0
    } else {
      value <- formatC(simResults$finalTable$value_yearly_usd_CT[1], digits = 1, format = "f")
    }
    valueBox(
      paste0("$", value, "M"), 
      textOutput("newtranslation30"), 
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$womenPercentage <- renderValueBox({
    valueBox(
      "34%", 
      textOutput("newtranslation29"), 
      icon = icon("venus"),
      color = "purple"
    )
    
  })
  
  
  output$totalBeneficiaries <- renderValueBox({
    if (is.null(simResults$finalTable)) {
      value <- 0
    } else {
      value <- sum(simResults$finalTable$count[1], na.rm = TRUE)
    }
    valueBox(
      formatC(value, big.mark = ","), 
      textOutput("newtranslation56"), 
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$costUSD <- renderValueBox({
    if (is.null(simResults$finalTable)) {
      value <- 0
    } else {
      value <- sum(simResults$finalTable$value_yearly_usd_CT[1], na.rm = TRUE)
    }
    valueBox(
      paste0("$", formatC(value, digits = 2, format = "f")), 
      textOutput("newtranslation57"), 
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })
  
  output$povertyRate <- renderValueBox({
    if (is.null(simResults$finalTable)) {
      value <- 0
    } else {
      value <- round(simResults$finalTable$Freq[1])
    }
    valueBox(
      paste0(value, "%"), 
      textOutput("newtranslation58"), 
      icon = icon("percent"),
      color = "red"
    )
  })
  
  
  #Results table
  
  getLabel <- function(row) {
    req(input$translation)                             # make sure a language is chosen
    lang  <- 1 + as.numeric(input$translation)         # 0 = first language, 1 = second … etc.
    if (row > nrow(VarLabels) || lang > ncol(VarLabels))
      return("??")                                     # safe‑guard
    VarLabels[row, lang]
  }
  output$resultsTable <- DT::renderDataTable({
    
    if (is.null(simResults$finalTable))
      return(NULL)
    
    # Pick the VarLabels rows that correspond to each column
    # (replace these row numbers with the ones that match your file)
    header_rows <- 43:48  # adjust based on your actual VarLabels row numbers
    
    translated_headers <- sapply(header_rows, function(row) {
      as.matrix(VarLabels[row, 1 + as.numeric(input$translation)])
    })
    
    DT::datatable(
      simResults$finalTable,
      rownames  = FALSE,
      colnames  = translated_headers,      # <‑‑ dynamic labels here
      options   = list(
        pageLength = 10,
        autoWidth  = TRUE,
        scrollX    = TRUE
      )
    ) %>% 
      formatRound(columns = 3:6, digits = 2)  # still OK because we use indices
  })
  
  
  
  
  ################
  # Drop down menu for scenarios
  output$scenarioFilterUI <- renderUI({
    req(simResults$finalTable_gov)
    
    scenarios <- unique(simResults$finalTable_gov$Scenario_name)
    selectInput("scenarioFilter", textOutput("newtranslation41"),
                choices = scenarios,
                selected = "Current status")
  })
  
  # Add this right after your existing output definitions
  output$govTable <- DT::renderDataTable({
    
    if (is.null(simResults$finalTable))
      return(NULL)
    
    # Pick the VarLabels rows that correspond to each column
    # (replace these row numbers with the ones that match your file)
    header_rows2 <- 112:118  # adjust based on your actual VarLabels row numbers
    
    translated_headers <- sapply(header_rows2, function(row) {
      as.matrix(VarLabels[row, 1 + as.numeric(input$translation)])
    })
    
    DT::datatable(
      filtered_gov_data(),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      rownames = FALSE,
      extensions = 'Buttons',
      colnames  = translated_headers,
      filter = 'top'
    ) %>%
      formatRound(columns = c(4:7), digits = 2)
  })
  observe({
    req(simResults$finalTable_gov)
    print(names(simResults$finalTable_gov))
  })
  
  # Vulnerability map
  # Replace the map rendering functions with these corrected versions:
  
  output$BeneficiariesMap <- renderLeaflet({
    req(filtered_gov_data())
    req(translated_title2())
    
    jordan_path <- paste0(Gislocation, "/StatPlanet_Jordan/map/Jordan/JOR_adm1.shp")
    if (!file.exists(jordan_path)) {
      return(leaflet() %>% addControl("Jordan shapefile not found", position = "topright"))
    }
    
    jordan <- st_read(jordan_path) %>%
      mutate(NAME_1 = recode(NAME_1,
                             "Tafila" = "Tafilah",
                             "Jerash" = "Jarash",
                             "Aljoun" = "Ajlun",
                             "Maan"  = "Ma`an"
      )) %>%
      left_join(filtered_gov_data(), by = c("NAME_1" = "Governorate"))
    
    pal <- colorBin("viridis", domain = jordan$count, bins = 7, na.color = "#f0f0f0")
    
    leaflet(jordan) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(count),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste(NAME_1, "beneficiaries:", round(count, 1), "% tot_benef", round(Freq,1) )
      ) %>%
      addLegend(
        pal = pal,
        values = jordan$count,
        opacity = 0.7,
        title = translated_title2(),  # MUST call the reactive with ()
        position = "bottomright"
      )
    
  })
  ############
  
  
  
  #### Dashboard Tab Graphics
  #Button
  
  observe({
    req(input$inflationShock, input$translation, input$employment_input_method)
    
    if (input$employment_input_method == "gdp") {
      req(input$increase_gdp_percap)
    } else if (input$employment_input_method == "direct") {
      req(input$cumulative_effect_1pct_user)
    }
    
    # Ensure input$translation is either "1" for EN or "2" for AR
    lang_index <- switch(input$translation,
                         "1" = 2,  # EN column
                         "2" = 3,  # AR column
                         2)        # fallback to EN
    
    # Convert and trim code column
    code_column <- trimws(VarLabels[, "Code"])
    
    # Function to get translated text for a given code
    getText <- function(code_number) {
      row_index <- which(code_column == as.character(code_number))
      if (length(row_index) == 0 || is.na(row_index)) return("MISSING")
      text <- VarLabels[row_index, lang_index]
      if (is.na(text)) return("MISSING")
      return(trimws(text))
    }
    
    # Fetch texts
    text61a <- getText(61)
    text62a <- getText(62)
    text63a <- getText(63)
    text64a <- getText(64)
    text65a <- getText(65)
    text134a <- getText(134)
    text135a <- getText(135)
    text136a <- getText(136)
    text137a <- getText(137)
    
    # Combine inflation shock into label
    shock_label <- paste0(text63a, " ", input$inflationShock, text64a)
    GDP_label   <- paste0(text134a, " ", input$increase_gdp_percap , text135a)
    Uemp_pp     <- paste0(text136a, " ", input$cumulative_effect_1pct_user , " ", text137a)
    
    employment_label <- if (input$employment_input_method == "gdp") {
      GDP_label
    } else {
      Uemp_pp
    }
    print("==> Updating checkbox with these labels:")
    print(c(
      text61a,
      text62a,
      shock_label,
      text65a,
      employment_label
    ))
    
    # Update the checkbox UI
    updateCheckboxGroupInput(
      session,
      inputId = "selected_scenarios_count",
      choices = c(
        paste0(text61a),
        paste0(text62a),
        shock_label,
        paste0(text65a),
        employment_label
      ))
    
  })
  
  observe({
    req(input$inflationShock, input$translation, input$employment_input_method)
    
    if (input$employment_input_method == "gdp") {
      req(input$increase_gdp_percap)
    } else if (input$employment_input_method == "direct") {
      req(input$cumulative_effect_1pct_user)
    }
    
    # Ensure input$translation is either "1" for EN or "2" for AR
    lang_index <- switch(input$translation,
                         "1" = 2,  # EN column
                         "2" = 3,  # AR column
                         2)        # fallback to EN
    
    # Convert and trim code column
    code_column <- trimws(VarLabels[, "Code"])
    
    # Function to get translated text for a given code
    getText <- function(code_number) {
      row_index <- which(code_column == as.character(code_number))
      if (length(row_index) == 0 || is.na(row_index)) return("MISSING")
      text <- VarLabels[row_index, lang_index]
      if (is.na(text)) return("MISSING")
      return(trimws(text))
    }
    
    # Fetch texts
    text61a <- getText(61)
    text62a <- getText(62)
    text63a <- getText(63)
    text64a <- getText(64)
    text65a <- getText(65)
    text134a <- getText(134)
    text135a <- getText(135)
    text136a <- getText(136)
    text137a <- getText(137)
    
    # Combine inflation shock into label
    shock_label <- paste0(text63a, " ",input$inflationShock, text64a)
    GDP_label   <- paste0(text134a, " ", input$increase_gdp_percap , text135a)
    Uemp_pp     <- paste0(text136a, " ", input$cumulative_effect_1pct_user , " ", text137a)
    
    employment_label <- if (input$employment_input_method == "gdp") {
      GDP_label
    } else {
      Uemp_pp
    }
    
    # Update the checkbox UI
    updateCheckboxGroupInput(
      session,
      inputId = "selected_scenarios_value",
      choices = c(
        paste0(text61a),
        paste0(text62a),
        shock_label,
        paste0(text65a),
        employment_label
      ))
    
  })
  
  
  #COmapirson (1)
  
  # filter the data to use it
  output$UCT_compare_count <- renderUI({
    df <- filtered_data()
    boxes <- lapply(1:nrow(df), function(i) {
      valueBox(
        value = formatC(df[["count"]][i], format = "d", big.mark = ","),
        subtitle = df[["Scenario_name"]][i],
        color = "aqua",
        icon = icon("users")
      )
    })
    fluidRow(boxes)
  })
  
  
  #Scenario Comparison UCT count
  
  filtered_count <- reactive({
    req(simResults$finalTable, input$selected_scenarios_count)
    
    df <- simResults$finalTable
    df$Scenario_name <- trimws(df$Scenario_name)
    
    filtered_df <- df[df$Scenario_name %in% input$selected_scenarios_count, ]
    
    print("Filtered data structure for selected scenarios:")
    print(str(filtered_df))
    
    print("Filtered data rows:")
    print(filtered_df)
    
    filtered_df
    
    #df[df$Scenario_name %in% input$selected_scenarios_count, ]
    
  })
  output$scenarioBarChart <- renderPlot({
    df <- filtered_count()
    req(nrow(df) > 0)
    
    # Create a color gradient for the bars
    color_scale <- colorRampPalette(c("lightblue", "deepskyblue", "dodgerblue3"))
    bar_colors <- color_scale(length(df$count))
    
    # Draw barplot with custom colors and spacing
    bp <- barplot(
      height = df$count,
      #names <- df[["Scenario_name"]] # error
      names.arg = as.character(seq_along(df$count)),
      col = bar_colors,
      border = "white",
      space = 0.6,
      ylim = c(0, max(df$count) * 1.25),
      
      ylab = "Count",
      las = 1, # horizontal y-axis labels
      # border = "white",  # Clean bar borders
      cex.names = 1.5
    )
    
    # Add numeric labels on top of bars
    text(
      x = bp,
      y = df$count,
      labels = format(df$count, big.mark = ","),
      pos = 3,
      cex = 1,
      col = "black",
      font = 2
    )
    #Optional: Draw line showing trend of variation
    lines(x = bp, y = df$count, type = "b", lwd = 2, col = "darkblue", pch = 16)
    
    # Add percentage variation labels between bars
    for (i in 2:length(df$count)) {
      pct_change <- round((df$count[i] - df$count[i - 1]) / df$count[i - 1] * 100, 1)
      label <- paste0(ifelse(pct_change >= 0, "+", ""), pct_change, "%")
      
      # Calculate midpoint between bars for x positioning
      x_pos <- (bp[i] + bp[i - 1]) / 2
      y_pos <- max(df$count[i], df$count[i - 1]) * 1.05  # adjust if needed
      
      text(x = x_pos, y = y_pos, labels = label, col = "darkred", cex = 1.5)
    }
  })
  
  
  #Comparison (2)
  
  # filter the data to use it
  output$UCT_compare_value <- renderUI({
    df <- filtered_data()
    boxes <- lapply(1:nrow(df), function(i) {
      valueBox(
        value = formatC(df[["value_yearly_usd_CT"]][i], format = "d", big.mark = ","),
        subtitle = df[["Scenario_name"]][i],
        color = "aqua",
        icon = icon("users")
      )
    })
    fluidRow(boxes)
  })
  
  #Scenario Comparison UCT VALUE
  
  filtered_value <- reactive({
    req(simResults$finalTable, input$selected_scenarios_value)
    
    df <- simResults$finalTable
    df$Scenario_name <- trimws(df$Scenario_name)
    df[df$Scenario_name %in% input$selected_scenarios_value, ]
  })
  
  output$scenarioBarChart2 <- renderPlot({
    df <- filtered_value()
    req(nrow(df) > 0)
    
    heights <- df[["value_yearly_usd_CT"]]
    #names <- df[["Scenario_name"]]
    
    # Color gradient to highlight variations
    color_scale <- colorRampPalette(c("lightblue", "deepskyblue", "dodgerblue3"))
    bar_colors <- color_scale(length(heights))
    
    # Plot with better spacing and styling
    bp <- barplot(
      height = heights,
      names.arg = as.character(seq_along(df$count)),,
      col = bar_colors,
      
      ylim = c(0, max(heights, na.rm = TRUE) * 1.25),
      border = "white",  # Clean bar borders
      cex.names = 1.5
    )
    
    # Add numeric labels on top of each bar
    text(
      x = bp,
      y = heights,
      labels = format(heights, big.mark = ","),
      pos = 3,
      cex = 1,
      col = "black"
    )
    
    # Optional: Draw line showing trend of variation
    lines(x = bp, y = heights, type = "b", lwd = 2, col = "darkblue", pch = 16)
    
    # Add percentage variation labels between bars
    for (i in 2:length(heights)) {
      pct_change <- round((heights[i] - heights[i - 1]) / heights[i - 1] * 100, 1)
      label <- paste0(ifelse(pct_change >= 0, "+", ""), pct_change, "%")
      
      # Calculate midpoint between bars for x positioning
      x_pos <- (bp[i] + bp[i - 1]) / 2
      y_pos <- max(heights[i], heights[i - 1]) * 1.05  # adjust if needed
      
      text(x = x_pos, y = y_pos, labels = label, col = "darkred", cex = 1.5)
    }
  })
  
  
  #Button
  label106 <- reactive({
    as.matrix(VarLabels[106, 1 + as.numeric(input$translation)])
  })
  label107 <- reactive({
    as.matrix(VarLabels[107, 1 + as.numeric(input$translation)])
  })
  label108 <- reactive({
    as.matrix(VarLabels[108, 1 + as.numeric(input$translation)])
  })
  label100 <- reactive({
    as.matrix(VarLabels[100, 1 + as.numeric(input$translation)])
  })
  label102 <- reactive({
    as.matrix(VarLabels[102, 1 + as.numeric(input$translation)])
  })
  output$employment_input_ui <- renderUI({
    radioButtons("employment_input_method",
                 label = label106(),
                 choices = setNames(
                   c("gdp", "direct"),
                   c(label107(), label108())
                 ),
                 selected = "gdp")
  })
  output$employment_numeric_ui <- renderUI({
    req(input$employment_input_method)
    
    if (input$employment_input_method == "gdp") {
      numericInput("increase_gdp_percap",
                   label = label100(),
                   value = 1, min = 0, max = 100, step = 1)
    } else if (input$employment_input_method == "direct") {
      numericInput("cumulative_effect_1pct_user",
                   label = label102(),
                   value = 1, min = 0, max = 100, step = 1)
    }
  })
  
  
  # #CHANGE THE TABLE SCENARIO NAMES
  observe({
    req(simResults$finalTable, input$inflationShock, input$translation,  input$increase_gdp_percap)
    
    # Fetch translated scenario names directly from VarLabels
    text61 <- as.matrix(VarLabels[61,])[1 + as.numeric(input$translation), 1]
    text62 <- as.matrix(VarLabels[62,])[1 + as.numeric(input$translation), 1]
    text63 <- as.matrix(VarLabels[63,])[1 + as.numeric(input$translation), 1]
    text64 <- as.matrix(VarLabels[64,])[1 + as.numeric(input$translation), 1]
    text65 <- as.matrix(VarLabels[65,])[1 + as.numeric(input$translation), 1]
    text134 <- as.matrix(VarLabels[134,])[1 + as.numeric(input$translation), 1]
    text135 <- as.matrix(VarLabels[135,])[1 + as.numeric(input$translation), 1]
    text136 <- as.matrix(VarLabels[136,])[1 + as.numeric(input$translation), 1]
    text137 <- as.matrix(VarLabels[137,])[1 + as.numeric(input$translation), 1]
    
    shock_label <- paste0(text63, " ",input$inflationShock, text64)
    GDP_label <- paste0(text134, " ", input$increase_gdp_percap , text135)
    Uemp_pp     <- paste0(text136, " ", input$cumulative_effect_1pct_user , " ", text137)
    
    employment_label <- if (input$employment_input_method == "gdp") {
      GDP_label
    } else {
      Uemp_pp
    }
    translated_names <- c(text61, text62, shock_label, text65, employment_label)
    
    # Replace scenario names in the table (only for the first 5 rows)
    for (i in seq_along(translated_names)) {
      if (i <= nrow(simResults$finalTable)) {
        simResults$finalTable$Scenario_name[i] <- translated_names[i]
      }
    }
    
  })
  
  
  
  
  ###################
  observe({
    lang_class <- ifelse(input$translation == 1, "ltr", "rtl")
    session$sendCustomMessage("setClass", lang_class)
    
    search_button_label <- ifelse(input$translation == 1, "Search", "بحث")
    session$sendCustomMessage("updateSearchButton", search_button_label)
  })
  
  
  #Insert Dictionary 
  #Tab 1
  #Parameters
  output$newtranslation1 <- renderText({ 
    textToPut=as.matrix(VarLabels[1,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  }) 
  #Mock
  output$newtranslation2 <- renderText({ 
    textToPut=as.matrix(VarLabels[2,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  output$dynamic_header <- renderUI({
    header_title <- as.matrix(VarLabels[109,])[1 + as.numeric(input$translation), 1]
    
    dashboardHeader(
      title = span(header_title, style = "align: center; font-size: 20px;")
    )
  })
  #Developed by
  output$newtranslation3 <- renderText({ 
    textToPut=as.matrix(VarLabels[3,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Shock Parameters
  output$newtranslation4 <- renderText({ 
    textToPut=as.matrix(VarLabels[4,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Inflation Parameters
  output$newtranslation5 <- renderText({ 
    textToPut=as.matrix(VarLabels[5,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Annual inflation
  output$newtranslation6 <- renderText({ 
    textToPut=as.matrix(VarLabels[6,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Employment
  output$newtranslation7 <- renderText({ 
    textToPut=as.matrix(VarLabels[7,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Annual empl
  output$newtranslation8 <- renderText({ 
    textToPut=as.matrix(VarLabels[8,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #User parameters
  output$newtranslation9 <- renderText({ 
    textToPut=as.matrix(VarLabels[9,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Target poverty 
  output$newtranslation10 <- renderText({ 
    textToPut=as.matrix(VarLabels[10,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #set the desired
  output$newtranslation11 <- renderText({ 
    textToPut=as.matrix(VarLabels[11,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #JOD to USD
  output$newtranslation12 <- renderText({ 
    textToPut=as.matrix(VarLabels[12,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Current exchange
  output$newtranslation13 <- renderText({ 
    textToPut=as.matrix(VarLabels[13,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  # # EMPTY
  # output$newtranslation14 <- renderText({ 
  #   textToPut=as.matrix(VarLabels[14,])
  #   temp=textToPut[1+as.numeric(input$translation),1]
  #   return(temp) 
  # })
  #Simulation
  output$newtranslation15 <- renderText({ 
    textToPut=as.matrix(VarLabels[15,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Dashboard Overview
  output$newtranslation16 <- renderText({ 
    textToPut=as.matrix(VarLabels[16,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Text 1
  output$newtranslation17 <- renderText({ 
    textToPut=as.matrix(VarLabels[17,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Scenarios included:
  output$newtranslation18 <- renderText({ 
    textToPut=as.matrix(VarLabels[18,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #text 2
  output$newtranslation19 <- renderText({ 
    textToPut=as.matrix(VarLabels[19,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #text 2
  output$newtranslation19b <- renderText({ 
    textToPut=as.matrix(VarLabels[93,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #text 3
  output$newtranslation20 <- renderText({ 
    textToPut=as.matrix(VarLabels[20,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #text 3
  output$newtranslation20b <- renderText({ 
    textToPut=as.matrix(VarLabels[94,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #text 4
  output$newtranslation21 <- renderText({ 
    textToPut=as.matrix(VarLabels[21,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #text 4
  output$newtranslation21b <- renderText({ 
    textToPut=as.matrix(VarLabels[95,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #text 5
  output$newtranslation22 <- renderText({ 
    textToPut=as.matrix(VarLabels[22,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #text 5
  output$newtranslation22b <- renderText({ 
    textToPut=as.matrix(VarLabels[96,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  # Running simulation
  output$newtranslation23 <- renderText({
    textToPut=as.matrix(VarLabels[23,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp)
  })
  #Run simulation
  output$newtranslation24 <- renderText({ 
    textToPut=as.matrix(VarLabels[24,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Go to simulaiton
  output$newtranslation25 <- renderText({ 
    textToPut=as.matrix(VarLabels[25,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Key metrics
  output$newtranslation26 <- renderText({ 
    textToPut=as.matrix(VarLabels[26,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Number of UCT
  output$newtranslation27 <- renderText({ 
    textToPut=as.matrix(VarLabels[27,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Total value
  output$newtranslation28 <- renderText({ 
    textToPut=as.matrix(VarLabels[28,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Percentage female
  output$newtranslation29 <- renderText({ 
    textToPut=as.matrix(VarLabels[29,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Total cost
  output$newtranslation30 <- renderText({ 
    textToPut=as.matrix(VarLabels[30,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Number of current beneficiaries
  translated_title <- reactive({
    req(input$translation)  # make sure input is available
    
    lang_index <- ifelse(input$translation == 1, 2, 3)  # 2=English, 3=Arabic based on your VarLabels
    as.character(VarLabels[31, lang_index])  # ensure it's character, not factor or other object
  })
  #Beneficiaries map
  output$newtranslation32 <- renderText({ 
    textToPut=as.matrix(VarLabels[32,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  # Simulation Completed
  output$newtranslation33 <- renderText({
    textToPut=as.matrix(VarLabels[33,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp)
  })
  
  
  
  #Tab 2
  # Governorate Breakdown
  output$newtranslation34 <- renderText({ 
    textToPut=as.matrix(VarLabels[34,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Governorate - Level
  output$newtranslation35 <- renderText({ 
    textToPut=as.matrix(VarLabels[35,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  
  output$newtranslation35a <- renderText({ 
    textToPut=as.matrix(VarLabels[35,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Maps
  output$newtranslation36 <- renderText({ 
    textToPut=as.matrix(VarLabels[36,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  # Beneficiaries by scenario
  output$newtranslation37 <- renderText({ 
    textToPut=as.matrix(VarLabels[37,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #EMPTY
  # output$newtranslation38 <- renderText({ 
  #   textToPut=as.matrix(VarLabels[38,])
  #   temp=textToPut[1+as.numeric(input$translation),1]
  #   return(temp) 
  # })
  # Number of beneficiaries by Scenario
  translated_title2 <- reactive({
    req(input$translation)  # make sure input is available
    
    lang_index <- ifelse(input$translation == 1, 2, 3)  # 2=English, 3=Arabic based on your VarLabels
    as.character(VarLabels[39, lang_index])  # ensure it's character, not factor or other object
  })
  #EMPTY
  # output$newtranslation40 <- renderText({ 
  #   textToPut=as.matrix(VarLabels[40,])
  #   temp=textToPut[1+as.numeric(input$translation),1]
  #   return(temp) 
  # })
  #Select Scenarios:
  output$newtranslation41 <- renderText({ 
    textToPut=as.matrix(VarLabels[41,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #EmPTY
  # output$newtranslation42 <- renderText({ 
  #   textToPut=as.matrix(VarLabels[42,])
  #   temp=textToPut[1+as.numeric(input$translation),1]
  #   return(temp) 
  # })
  #Scenario Name
  output$newtranslation43 <- renderText({ 
    textToPut=as.matrix(VarLabels[43,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  # Number UCT
  output$newtranslation44 <- renderText({ 
    textToPut=as.matrix(VarLabels[44,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  # Freq UCT
  output$newtranslation45 <- renderText({ 
    textToPut=as.matrix(VarLabels[45,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  # Variation
  output$newtranslation46 <- renderText({ 
    textToPut=as.matrix(VarLabels[46,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  # TOtal cost (JOD)
  output$newtranslation47 <- renderText({ 
    textToPut=as.matrix(VarLabels[47,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Total cost USD
  output$newtranslation48 <- renderText({ 
    textToPut=as.matrix(VarLabels[48,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Current sttaus
  output$newtranslation49 <- renderText({ 
    textToPut=as.matrix(VarLabels[49,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #HH under PL
  output$newtranslation50 <- renderText({ 
    textToPut=as.matrix(VarLabels[50,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #HH under PL and inflation
  output$newtranslation51 <- renderText({ 
    textToPut=as.matrix(VarLabels[51,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #%
  output$newtranslation52 <- renderText({ 
    textToPut=as.matrix(VarLabels[52,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Watershock
  output$newtranslation53 <- renderText({ 
    textToPut=as.matrix(VarLabels[53,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Governorate
  output$newtranslation54 <- renderText({ 
    textToPut=as.matrix(VarLabels[54,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  
  
  #Tab 3
  #Comparing scenarios
  
  #Comparing scenarios
  output$newtranslation55 <- renderText({ 
    textToPut=as.matrix(VarLabels[55,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Current total beneficiaries
  output$newtranslation56 <- renderText({ 
    textToPut=as.matrix(VarLabels[56,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Current total cost USD
  output$newtranslation57 <- renderText({ 
    textToPut=as.matrix(VarLabels[57,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Current Freq 
  output$newtranslation58 <- renderText({ 
    textToPut=as.matrix(VarLabels[58,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Select scenarios
  output$newtranslation59 <- renderText({ 
    textToPut=as.matrix(VarLabels[59,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Select scenarios
  output$newtranslation59a <- renderText({ 
    textToPut=as.matrix(VarLabels[59,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Compare scenarios:
  output$newtranslation60 <- renderText({ 
    textToPut=as.matrix(VarLabels[60,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Compare scenarios:
  output$newtranslation60a <- renderText({ 
    textToPut=as.matrix(VarLabels[60,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Current status
  output$newtranslation61 <- renderText({ 
    textToPut=as.matrix(VarLabels[61,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Current status
  # output$newtranslation61a <- renderText({ 
  #   textToPut=as.matrix(VarLabels[61,])
  #   temp=textToPut[1+as.numeric(input$translation),1]
  #   return(temp) 
  # })
  
  
  # output$newtranslation61a <- renderText({ text61a() })
  # output$newtranslation62a <- renderText({ text62a() })
  # output$newtranslation63a <- renderText({ text63a() })
  # output$newtranslation64a <- renderText({ text64a() })
  # output$newtranslation65a <- renderText({ text65a() })
  
  
  #HH under PL
  output$newtranslation62 <- renderText({ 
    textToPut=as.matrix(VarLabels[62,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #HH under PL
  # output$newtranslation62a <- renderText({ 
  #   textToPut=as.matrix(VarLabels[62,])
  #   temp=textToPut[1+as.numeric(input$translation),1]
  #   return(temp) 
  # })
  #HH under Pl and inflation
  output$newtranslation63 <- renderText({ 
    textToPut=as.matrix(VarLabels[63,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #HH under Pl and inflation
  # output$newtranslation63a <- renderText({ 
  #   textToPut=as.matrix(VarLabels[63,])
  #   temp=textToPut[1+as.numeric(input$translation),1]
  #   return(temp) 
  # })
  #%
  output$newtranslation64 <- renderText({ 
    textToPut=as.matrix(VarLabels[64,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  # output$newtranslation64a <- renderText({ 
  #   textToPut=as.matrix(VarLabels[64,])
  #   temp=textToPut[1+as.numeric(input$translation),1]
  #   return(temp) 
  # })
  #Watershock
  output$newtranslation65 <- renderText({ 
    textToPut=as.matrix(VarLabels[65,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Watershock
  # output$newtranslation65a <- renderText({ 
  #   textToPut=as.matrix(VarLabels[65,])
  #   temp=textToPut[1+as.numeric(input$translation),1]
  #   return(temp) 
  # })
  #UCT Beneficiary count by scenario
  output$newtranslation66 <- renderText({ 
    textToPut=as.matrix(VarLabels[66,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #UCT Beneficiary count by scenario USD
  output$newtranslation67 <- renderText({ 
    textToPut=as.matrix(VarLabels[67,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  
  
  #Tab 4
  #Settings
  #EMPTY
  # output$newtranslation68 <- renderText({ 
  #   textToPut=as.matrix(VarLabels[68,])
  #   temp=textToPut[1+as.numeric(input$translation),1]
  #   return(temp) 
  # })
  #settings
  output$newtranslation69 <- renderText({ 
    textToPut=as.matrix(VarLabels[69,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #climate shock
  output$newtranslation70 <- renderText({ 
    textToPut=as.matrix(VarLabels[70,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #livestock loss
  output$newtranslation71 <- renderText({ 
    textToPut=as.matrix(VarLabels[71,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #land affected
  output$newtranslation72 <- renderText({ 
    textToPut=as.matrix(VarLabels[72,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #job loss
  output$newtranslation73 <- renderText({ 
    textToPut=as.matrix(VarLabels[73,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #car sale
  output$newtranslation74 <- renderText({ 
    textToPut=as.matrix(VarLabels[74,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #stock sale
  output$newtranslation75 <- renderText({ 
    textToPut=as.matrix(VarLabels[75,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #property
  output$newtranslation76 <- renderText({ 
    textToPut=as.matrix(VarLabels[76,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #EMPTY
  # output$newtranslation77 <- renderText({ 
  #   textToPut=as.matrix(VarLabels[77,])
  #   temp=textToPut[1+as.numeric(input$translation),1]
  #   return(temp) 
  # })
  #asset risk
  output$newtranslation78 <- renderText({ 
    textToPut=as.matrix(VarLabels[78,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #high risk
  output$newtranslation79 <- renderText({ 
    textToPut=as.matrix(VarLabels[79,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #moderate risk
  output$newtranslation80 <- renderText({ 
    textToPut=as.matrix(VarLabels[80,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #low risk
  output$newtranslation81 <- renderText({ 
    textToPut=as.matrix(VarLabels[81,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #empty
  # output$newtranslation82 <- renderText({ 
  #   textToPut=as.matrix(VarLabels[82,])
  #   temp=textToPut[1+as.numeric(input$translation),1]
  #   return(temp) 
  # })
  #gO BACK
  output$newtranslation83 <- renderText({ 
    textToPut=as.matrix(VarLabels[83,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Please click
  output$newtranslation92 <- renderText({ 
    textToPut=as.matrix(VarLabels[92,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #for example
  output$newtranslation100 <- renderText({ 
    textToPut=as.matrix(VarLabels[100,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #text direct employment
  output$newtranslation101 <- renderText({ 
    textToPut=as.matrix(VarLabels[101,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #text direct employment
  output$newtranslation101b <- renderText({ 
    textToPut=as.matrix(VarLabels[97,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #unemployment increase
  output$newtranslation102 <- renderText({ 
    textToPut=as.matrix(VarLabels[102,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #text Idiosyncrsatic
  output$newtranslation103 <- renderText({ 
    textToPut=as.matrix(VarLabels[103,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  observe({
    cat("Current working directory:", getwd(), "\n")
  })
  #employment impact
  output$newtranslation106 <- renderText({ 
    textToPut=as.matrix(VarLabels[106,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #change in GDP
  output$newtranslation107 <- renderText({ 
    textToPut=as.matrix(VarLabels[107,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Percentage point
  output$newtranslation108 <- renderText({ 
    textToPut=as.matrix(VarLabels[108,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Percentage point
  output$newtranslation108 <- renderText({ 
    textToPut=as.matrix(VarLabels[108,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Ajlun
  output$newtranslation120 <- renderText({ 
    textToPut=as.matrix(VarLabels[120,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Amman
  output$newtranslation121 <- renderText({ 
    textToPut=as.matrix(VarLabels[121,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Aqaba
  output$newtranslation122 <- renderText({ 
    textToPut=as.matrix(VarLabels[122,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Balqa
  output$newtranslation123 <- renderText({ 
    textToPut=as.matrix(VarLabels[123,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Irbid
  output$newtranslation124 <- renderText({ 
    textToPut=as.matrix(VarLabels[124,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Jarash
  output$newtranslation125 <- renderText({ 
    textToPut=as.matrix(VarLabels[125,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Karak
  output$newtranslation126 <- renderText({ 
    textToPut=as.matrix(VarLabels[126,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Maan
  output$newtranslation127 <- renderText({ 
    textToPut=as.matrix(VarLabels[127,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Madaba
  output$newtranslation128 <- renderText({ 
    textToPut=as.matrix(VarLabels[128,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Mafraq
  output$newtranslation129 <- renderText({ 
    textToPut=as.matrix(VarLabels[129,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Tafilah
  output$newtranslation130 <- renderText({ 
    textToPut=as.matrix(VarLabels[130,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #Zarqa
  output$newtranslation131 <- renderText({ 
    textToPut=as.matrix(VarLabels[131,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #GDP shock
  output$newtranslation134 <- renderText({ 
    textToPut=as.matrix(VarLabels[134,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  #%
  output$newtranslation135 <- renderText({ 
    textToPut=as.matrix(VarLabels[135,])
    temp=textToPut[1+as.numeric(input$translation),1]
    return(temp) 
  })
  
  
}
