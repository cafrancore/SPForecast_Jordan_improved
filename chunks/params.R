


language            <- 1                 # 1 = English, 2 = Arabic
target_poverty_rate <- 35                # %
exc_rateJOD_USD     <- 1.41              # JOD per USD
shock_inflation     <- 5                 # % (inflation shock for Scenario 2)

params <- list(
  livestockLoss            = 0.11,
  landAffected             = 0.35,
  jobLoss                  = 0.10,  # baseline fraction for employment shock before weights
  carSale                  = 0.20,
  stockSale                = 0.30,
  propertySale             = 0.15,
  highRiskLoss             = 0.40,
  moderateRiskLoss         = 0.30,
  lowRiskLoss              = 0.20,
  employment_input_method  = "gdp",  # "gdp" or "direct"
  increase_gdp_percap      = 1.0,    # when method == "gdp"
  cumulative_effect_1pct_user = 0.5  # percentage points when method == "direct"
)
n_years <- 5
PL=4.570772

p_num_0_14 = 80
p_a_14 = 10
p_a_64 =5
mortality_rate = 3 #death rate per 1000 according to WB  https://data.worldbank.org/indicator/SP.DYN.CDRT.IN?locations=JO


p_num_0_14 = p_num_0_14/100
p_a_14 = p_a_14/100  
p_a_64 = p_a_64/100 
mortality_rate = mortality_rate/1000
