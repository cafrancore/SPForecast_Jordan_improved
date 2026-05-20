
#Aux_function

n_years = input$n_years
p_num_0_14 = input$p_num_0_14
p_a_14 = input$p_a_14
p_a_64 = input$p_a_64



.safe_int <- function(x) {
  x <- ifelse(is.na(x), 0, x)
  as.integer(pmax(0, x))
}

compute_all <- function(df, PL) {
  
  df <- df %>%
    dplyr::mutate(
  score_postshock_SC5 =
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

    )

  df <- df %>%
    dplyr::mutate(
      poor_new_demo1  = as.integer(score_postshock_SC5 < PL)
    )
  
  
  
  df$SP_program_combo_demo1 <- dplyr::case_when(
    df$SP_program == "Beneficiary" & df$poor_new_demo1 == 1 ~ "Beneficiary & poor",
    df$SP_program == "Beneficiary" & df$poor_new_demo1 == 0 ~ "Beneficiary & non-poor",
    df$SP_program == "Eligible" & df$poor_new_demo1 == 1 ~ "Eligible & poor",
    df$SP_program == "Eligible" & df$poor_new_demo1 == 0 ~ "Eligible & non-poor",
    df$SP_program == "Non-Eligible" & df$poor_new_demo1 == 1 ~ "Non-Eligible & poor",
    TRUE ~ "Non-Eligible & non-poor"
  )
  
  df$Value_CT_5<- dplyr::case_when(
    df$SP_program_combo_demo1  %in% c("Beneficiary & poor","Beneficiary & non-poor","Eligible & poor","Non-Eligible & poor") & df$HH.Size == 1 ~ 40,
    df$SP_program_combo_demo1  %in% c("Beneficiary & poor","Beneficiary & non-poor","Eligible & poor","Non-Eligible & poor") & df$HH.Size > 1 & df$HH.Size <= 5 ~ pmin(40 + (df$HH.Size - 1) * 15, 100),
    df$SP_program_combo_demo1  %in% c("Beneficiary & poor","Beneficiary & non-poor","Eligible & poor","Non-Eligible & poor") & df$HH.Size >= 6 ~ 100,
    TRUE ~ 0
  )
  

  df <- df %>%
    mutate(
      old_member_bin_demo1  = ifelse(HH.65y.or.more >= 1,1,0),
      Added_value_demo1 = ifelse(HH_F_Divorced_WIDOWED == 1 | old_member_bin_demo1 == 1 | disab_member_bin == 1, 1, 0)
    )
      
  df$Total_Value_CT_5<- dplyr::case_when(
    df$SP_program_combo_demo1  %in% c("Beneficiary & poor","Beneficiary & non-poor","Eligible & poor","Non-Eligible & poor") ~
      df$Value_CT_5 + ifelse(df$Added_value_demo1 == 1, 35, 0),
    TRUE ~ df$Value_CT_5
  )
  
  return(df)
}



set.seed(5)

# Load Data ####
df <- data2




df$Dependents= df$HH.Size - df$Working.age.members




df <- df %>%
  mutate(
    age_original = age,
    HH.30y.or.less_original = HH.30y.or.less,
    HH.65y.or.more_original = HH.65y.or.more,
    Dependency.ratio_original = Dependency.ratio,
    Working.age.members_original = Working.age.members,
    poor_original = poor,
    Value_CT_original = Total_Value_CT_0
  )


# Simulate function for One Year with Aging & Mortality Modules ####
simulate_year <- function(df, year_suffix, p_num_0_14 , p_a_14 ,  p_a_64, mortality_rate ) {
  
  sp_col <- df$SP_program
  
  # Age heads
  df <- df %>%
    mutate(
      age = ifelse(is.na(age), age, age + 1),
      HH.30y.or.less = ifelse(!is.na(HH.30y.or.less) & HH.30y.or.less == 1 & !is.na(age) & age > 30, 0, HH.30y.or.less),
      HH.65y.or.more = ifelse(!is.na(HH.65y.or.more) & HH.65y.or.more == 0 & !is.na(age) & age >= 65, 1, HH.65y.or.more)
    )
  
  
  # Aging children and retirement
  df <- df %>%
    mutate(
      Dependents = ifelse(is.na(Dependents), 0, Dependents),
      n_0_14     = .safe_int(round(Dependents * p_num_0_14)),
      n_65_plus  = .safe_int(Dependents - n_0_14)
    )
  
  children_aging   <- rbinom(nrow(df), .safe_int(df$n_0_14), p_a_14)
  eligible_workers <- .safe_int((ifelse(is.na(df$Working.age.members), 0, df$Working.age.members)) - 1)
  workers_retiring <- rbinom(nrow(df), eligible_workers, p_a_64)
  head_retires     <- as.integer(!is.na(df$age) & df$age == 65)
  
  
  df <- df %>%
    mutate(
      Working.age.members = pmax(0, ifelse(is.na(Working.age.members), 0, Working.age.members) + children_aging - workers_retiring - head_retires),
      Dependents = pmax(0, n_0_14 + n_65_plus - children_aging + workers_retiring + head_retires),
      Dependency.ratio = ifelse(Working.age.members > 0, Dependents / Working.age.members, Dependents)
    )
  
  # Mortality module 
  total_individuals <- sum(df$HH.Size)
  total_individuals <- sum(ifelse(is.na(df$HH.Size), 0, df$HH.Size), na.rm = TRUE)
  total_deaths <- ceiling(total_individuals * mortality_rate)
  
  df$flag <- as.integer( (ifelse(is.na(df$age), 0, df$age) < 65 & df$n_65_plus > 0) |
                           (ifelse(is.na(df$age), 0, df$age) >= 65 & df$n_65_plus > 1) )
  
  if (sum(df$flag, na.rm = TRUE) > 0) {
    deaths_to_assign <- min(total_deaths, sum(df$flag, na.rm = TRUE))
    death_households <- sample(which(df$flag == 1), deaths_to_assign)
    
    df$HH.Size[death_households]    <- pmax(0, ifelse(is.na(df$HH.Size[death_households]), 0, df$HH.Size[death_households] - 1))
    df$n_65_plus[death_households]  <- pmax(0, df$n_65_plus[death_households] - 1)
    
    df$Dependents[death_households] <- pmax(
      0,
      df$n_0_14[death_households] +
        df$n_65_plus[death_households] -
        children_aging[death_households] +
        workers_retiring[death_households] +
        head_retires[death_households]
    )
    
    df$Dependency.ratio[death_households] <- ifelse(
      df$Working.age.members[death_households] > 0,
      df$Dependents[death_households] / df$Working.age.members[death_households],
      df$Dependents[death_households]
    )
  }
    

  
  df$flag <- NULL
  
  df$SP_program <- sp_col

  df <- compute_all(df, PL)
  
  
  
  df <- df %>%
    mutate(
      !!paste0("score_year", year_suffix)                  := score_postshock_SC5,
      # !!paste0("Income_year", year_suffix)               := Income,
      !!paste0("poor_year", year_suffix)                   := poor_new_demo1,
      !!paste0("SP_program_combo_demo1_year", year_suffix) := SP_program_combo_demo1,
      !!paste0("Total_Value_CT_5_year", year_suffix)       := Total_Value_CT_5,
      !!paste0("age_year", year_suffix)                    := age,
      !!paste0("HH.30y.or.less_year", year_suffix)         := HH.30y.or.less,
      !!paste0("HH.65y.or.more_year", year_suffix)         := HH.65y.or.more,
      !!paste0("Dependency.ratio_year", year_suffix)       := Dependency.ratio,
      !!paste0("Working.age.members_year", year_suffix)    := Working.age.members,
      !!paste0("Dependents_year", year_suffix)             := Dependents,
      !!paste0("HH.Size_year", year_suffix)                := HH.Size
    ) %>%
    mutate(
      !!paste0("Scenario_5_year", year_suffix) :=
        dplyr::case_when(
          .data[[paste0("SP_program_combo_demo1_year", year_suffix)]] %in%
            c("Beneficiary & poor", "Beneficiary & non-poor",
              "Eligible & poor", "Non-Eligible & poor") ~ "UCT Beneficiaries",
          TRUE ~ "Non-UCT Beneficiaries"
        )
    )
  
  
  df$n_0_14 <- NULL
  df$n_65_plus <- NULL
  
  return(df)
}


# Run simulation for a multitude of years ####

history <- vector("list", n_years)

for (y in 1:n_years) {
  df <- simulate_year(df, year_suffix = y,  p_num_0_14 , p_a_14 ,  p_a_64, mortality_rate )
  history[[y]] <- df
}


summary_df <- data.frame(
  Year = 0:n_years,
  n_poor = NA,
  total_CT = NA
)


summary_df$n_poor[1]   <- sum(ifelse(is.na(df$poor_original), 0, df$poor_original))
summary_df$total_CT[1] <- sum(ifelse(is.na(df$Value_CT_original), 0, df$Value_CT_original))

for (y in 1:n_years) {
  summary_df$n_poor[y + 1]   <- sum(ifelse(is.na(df[[paste0("poor_year", y)]]), 0, df[[paste0("poor_year", y)]]))
  summary_df$total_CT[y + 1] <- sum(ifelse(is.na(df[[paste0("Total_Value_CT_5_year", y)]]), 0, df[[paste0("Total_Value_CT_5_year", y)]]))
}

data2=df
