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
    ),
    Scenario_5 = case_when(
      SP_program_combo_demo1  %in% c("Beneficiary & poor", "Beneficiary & non-poor", 
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
    ),
  #  Scenario 5 
  data2 %>%
    group_by(Scenario = "Scenario 5", Category = Scenario_5) %>%
    summarize(
      count = n(),  
      
      Freq = (n()/Pop)*100,
      value_yearly_jod_CT = sum(Total_Value_CT_5_year1 * weights)/(10^6)*12,
      value_yearly_usd_CT = sum(Total_Value_CT_5_year1 * weights)/(10^6)*exc_rateJOD_USD*12,
      .groups = "drop"
    )
  
  
)



results <- results %>%  
  mutate(
    Scenario_name = case_when(
      Scenario == "Scenario 0" ~ "Current status",
      Scenario == "Scenario 1" ~ "Households under poverty line",
      Scenario == "Scenario 2" ~ paste0("Households under PL after inflation shock ", shock_inflation, "%"),
      Scenario == "Scenario 3" ~ "Climate change impact on water scarcity",
      Scenario == "Scenario 5" ~ paste0("Households under PL after aging and change in mortality ",mortality_rate*1000, "x 1000 rate"),
      
      
      TRUE ~ if (input$employment_input_method == "gdp") {
        paste0("GDP shock ", input$increase_gdp_percap, "%")
      } else {
        paste0("Unemployment shock ", input$cumulative_effect_1pct_user, "percentage points")
      }
    )
  ) %>%
  relocate(Scenario_name, .before = 1) %>%
  filter(Category == "UCT Beneficiaries")





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
      value_yearly_jod_CT = sum(Total_Value_CT_1 *1)/(10^6)*12,
      value_yearly_usd_CT = sum(Total_Value_CT_1 * 1)/(10^6)*exc_rateJOD_USD*12,
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
    mutate(Freq = (count/sum(count))*100),
  
  
  # Scenario 5
  data2 %>% 
    filter(Scenario_5 == "UCT Beneficiaries") %>%
    group_by(Scenario = "Scenario 5", Governorate, Category = Scenario_4) %>%
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
      Scenario == "Scenario 3" ~ "Climate change impact on water scarcity",
      Scenario == "Scenario 5" ~ paste0("Households under PL after aging and change in mortality ",mortality_rate, "x 1000 rate"),
      
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




##Newtable
mortality_rate_1000 <- mortality_rate*1000

results_dynamic_tot <- map_dfr(seq_len(n_years), function(year_suffix) {
  scen_col <- paste0("Scenario_5_year", year_suffix)
  val_col  <- paste0("Total_Value_CT_5_year", year_suffix)
  
  data2 %>%
    group_by(Category = .data[[scen_col]]) %>%
    summarize(
      count = n(),
      Freq  = (n() / Pop) * 100,
      value_yearly_jod_CT = sum(.data[[val_col]] * weights, na.rm = TRUE) / 1e6 * 12,
      value_yearly_usd_CT = sum(.data[[val_col]] * weights, na.rm = TRUE) / 1e6 * exc_rateJOD_USD * 12,
      .groups = "drop"
    ) %>%
    mutate(
      Scenario = paste0("Scenario 5 - Year ", year_suffix),
      Scenario_name = paste0("Aging and mortality rate:", mortality_rate_1000, "x1000 - Year ", year_suffix),
      year = year_suffix
    ) %>%
    relocate(Scenario_name, .before = 1) %>%
    filter(Category == "UCT Beneficiaries")
})



results_dynamic_gov <- map_dfr(seq_len(n_years), function(year_suffix) {
  scen_col <- paste0("Scenario_5_year", year_suffix)
  val_col  <- paste0("Total_Value_CT_5_year", year_suffix)
  
  data2 %>%
    filter(.data[[scen_col]] == "UCT Beneficiaries") %>%
    group_by(
      Scenario   = paste0("Scenario 5 - Year ", year_suffix),
      Governorate,
      Category   = .data[[scen_col]]
    ) %>%
    summarize(
      count = n(),
      value_yearly_jod_CT = sum(.data[[val_col]] * weights, na.rm = TRUE) / 1e6 * 12,
      value_yearly_usd_CT = sum(.data[[val_col]] * weights, na.rm = TRUE) / 1e6 * exc_rateJOD_USD * 12,
      .groups = "drop"
    ) %>%
    group_by(Scenario) %>%
    mutate(Freq = (count / sum(count)) * 100) %>%
    ungroup() %>%
    mutate(
      Scenario_name = paste0("Aging and mortality rate: ",  mortality_rate_1000, "x1000 - Year ", year_suffix),
      year = year_suffix
    ) %>%
    relocate(Scenario_name, .before = 1)
})

results_dynamic_all     <- dplyr::bind_rows(results[1:2,], results_dynamic_tot)
results_dynamic_gov <- dplyr::bind_rows(results_gov[1:2,], results_dynamic_gov)
print(results_dynamic_all)
