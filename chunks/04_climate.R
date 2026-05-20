# --- Shock parameters & weights (same inputs/objects) ---
shock_params <- list(
  livestock_loss = input$livestockLoss,
  land_affected = input$landAffected,
  job_loss = input$jobLoss,
  car_sale = input$carSale,
  stock_sale = input$stockSale,
  commercial_property_sale = input$propertySale
)

# --- Normalize governorate names (same mapping, applied to both) ---
.recode_gov <- function(df) {
  df %>%
    dplyr::mutate(
      Governorate = dplyr::recode(
        Governorate,
        "Tafila"  = "Tafilah",
        "Jerash"  = "Jarash",
        "Aljoun"  = "Ajlun",
        "Ajloun"  = "Ajlun"
      )
    )
}

dataCH2 <- .recode_gov(dataCH2)
dataCH3 <- .recode_gov(dataCH3)

HIECS_data_common_F <- dataCH3 %>% dplyr::rename_with(~ gsub(" ", ".", .x))

# --- Raster & polygons (use terra + sf; preserve objects/prints) ---
tif_file <- file.path(Gislocation, "VA_NC_Annual.tif")
r <- terra::rast(tif_file)            # replace raster::raster() with terra::rast()
raster_df <- as.data.frame(r, xy = TRUE)  # keep object name created earlier
print(r)
crs(r)

jordan_governorates <- sf::st_read(
  file.path(Gislocation, "StatPlanet_Jordan/map/Jordan/JOR_adm1.shp"),
  quiet = TRUE
)

crs(jordan_governorates)

# if-branch preserved (define jordan_governoratess either way to avoid undefined var)
if (sf::st_crs(jordan_governorates) != sf::st_crs(r)) {
  jordan_governoratess <- sf::st_transform(jordan_governorates, crs = sf::st_crs(r))
} else {
  jordan_governoratess <- jordan_governorates
}

plot(r)
plot(sf::st_geometry(jordan_governoratess), add = TRUE, border = "black", lwd = 2)

# --- Downsample, project, and join raster-to-polygons ---
raster_layer <- r
crs_epsg <- sf::st_crs(jordan_governorates)$epsg

raster_downsampled <- terra::aggregate(raster_layer, fact = 5)
raster_wgs84 <- terra::project(raster_downsampled, paste0("EPSG:", crs_epsg))

raster_df <- as.data.frame(raster_wgs84, xy = TRUE, na.rm = TRUE)

raster_points_sf <- sf::st_as_sf(
  raster_df, coords = c("x", "y"), crs = sf::st_crs(jordan_governorates)
)

raster_joined <- sf::st_join(raster_points_sf, jordan_governorates[, c("ID_1")], left = FALSE)

raster_with_polygon_ids <- raster_joined %>%
  sf::st_drop_geometry() %>%
  dplyr::rename(polygon_id = ID_1)

average_df <- raster_with_polygon_ids %>%
  dplyr::filter(!is.na(VA_NC_Annual)) %>%
  dplyr::group_by(polygon_id) %>%
  dplyr::summarize(VA_NC_Annual = mean(VA_NC_Annual, na.rm = TRUE), .groups = "drop")

average_df_cleaned <- average_df %>% dplyr::filter(!is.na(polygon_id))
print(average_df_cleaned)

jordan_governorates_with_VA <- jordan_governorates %>%
  dplyr::left_join(average_df_cleaned, by = c("ID_1" = "polygon_id"))

centroids <- sf::st_centroid(jordan_governorates_with_VA)

# Keep column-by-index selection to preserve exact shape/names downstream
Prep <- jordan_governorates_with_VA[, c(5, 17)] %>%
  as.data.frame()
Prep <- Prep[, -c(3)]
colnames(Prep)[1] <- "Region"
Prep$Region[8] <- "Maan"

region_counts <- HIECS_data_common_F %>% dplyr::count(Governorate)

# Preserve Combine columns and order
Combine <- cbind(Prep, region_counts)
Combine <- Combine[, -c(3)]

total_n <- sum(Combine$n)
Combine$frequency <- (Combine$n / total_n) * 100

Combine <- Combine %>%
  dplyr::mutate(
    Asset_risk = dplyr::case_when(
      VA_NC_Annual >= 7 ~ "High_risk",
      VA_NC_Annual >= 5 ~ "Moderate_risk",
      TRUE ~ "Low_risk"
    )
  )

HIECS_data_common_F <- HIECS_data_common_F %>%
  dplyr::left_join(Combine, by = c("Governorate" = "Region"))



risk_weights <- list(
  "High_risk"     = max(1e-9, highRiskLoss    ),  # or default to 1 if you prefer
  "Moderate_risk" = max(1e-9, moderateRiskLoss),
  "Low_risk"      = max(1e-9, lowRiskLoss     )
)


# --- Helper functions (logic preserved) ---
apply_binary_shock <- function(df, varname, newvarname, base_loss_prob, risk_weights) {
  risk_weights_vec <- unlist(risk_weights)
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      !!rlang::sym(newvarname) := {
        if (get(varname) == 1) {
          w <- risk_weights_vec[as.character(Asset_risk)]
          w <- ifelse(is.na(w), 1, w)                         # default to 1, not 0
          loss_p  <- pmin(pmax(base_loss_prob * w, 0), 1)     # clamp to [0,1]
          keep_p  <- 1 - loss_p
          rbinom(1, 1, prob = keep_p)                         # 1=keep, 0=lose
        } else {
          0
        }
      }
    ) %>%
    dplyr::ungroup()
}


apply_income_shock <- function(df, varname, newvarname, base_loss_prob, risk_weights) {
  rw <- unlist(risk_weights)
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      !!rlang::sym(newvarname) := {
        w <- ifelse(as.character(Asset_risk) %in% names(rw), rw[as.character(Asset_risk)], 1)
        loss_p <- pmin(pmax(base_loss_prob * w, 0), 1)
        size   <- max(0, as.integer(get(varname)))
        kept   <- rbinom(1, size = size, prob = 1 - loss_p)   # binomial thinning
        kept
      }
    ) %>% dplyr::ungroup()
}


# --- Validation & shocks (same seed and sequence) ---
validate(
  need(all(unique(HIECS_data_common_F$Asset_risk) %in% names(risk_weights)),
       "Some Asset_risk values don't have corresponding weights")
)

set.seed(123)
HIECS_data_common_F_shocked <- HIECS_data_common_F %>%
  dplyr::filter(!is.na(Asset_risk)) %>%
  dplyr::mutate(Asset_risk = as.character(Asset_risk)) %>%
  apply_binary_shock("HH.owns.livestock",            "HH.owns.livestock_postshock",        shock_params$livestock_loss,           risk_weights) %>%
  apply_binary_shock("HH.cultivates.land",           "HH.cultivates.land_postshock",       shock_params$land_affected,            risk_weights) %>%
  apply_binary_shock("Owns.a.car",                   "Owns.a.car_postshock",               shock_params$car_sale,                 risk_weights) %>%
  apply_binary_shock("Owns.stocks",                  "Owns.stocks_postshock",              shock_params$stock_sale,               risk_weights) %>%
  apply_binary_shock("Owns.commercial.property",     "Owns.commercial.property_postshock", shock_params$commercial_property_sale, risk_weights) %>%
  apply_income_shock("Formal.income.earners",        "Formal.income.earners_postshock",    shock_params$job_loss,                 risk_weights)

summary_table_changes_vars <- HIECS_data_common_F_shocked %>%
  dplyr::group_by(SP_program) %>%
  dplyr::summarise(
    Livestock_before   = sum(HH.owns.livestock, na.rm = TRUE),
    Livestock_after    = sum(HH.owns.livestock_postshock, na.rm = TRUE),
    Cultivation_before = sum(HH.cultivates.land, na.rm = TRUE),
    Cultivation_after  = sum(HH.cultivates.land_postshock, na.rm = TRUE),
    Cars_before        = sum(Owns.a.car, na.rm = TRUE),
    Cars_after         = sum(Owns.a.car_postshock, na.rm = TRUE),
    Income_before      = sum(Formal.income.earners, na.rm = TRUE),
    Income_after       = sum(Formal.income.earners_postshock, na.rm = TRUE),
    .groups = "drop"
  )

# --- Squares, dummies, score_postshock (unchanged coefficients/logic) ---
HIECS_data_common_F_shocked <- HIECS_data_common_F_shocked %>%
  dplyr::mutate(
    HH.Size_sq = HH.Size^2,
    Age.newest.car_sq = Age.newest.car^2,
    Formal.income.earners_sq = Formal.income.earners_postshock^2
  ) %>%
  dplyr::mutate(
    Governorate_Mafraq  = ifelse(Governorate == "Mafraq", 1, 0),
    Governorate_Amman   = ifelse(Governorate == "Amman", 1, 0),
    Governorate_Tafilah = ifelse(Governorate == "Tafila", 1, 0),
    Governorate_Zarqa   = ifelse(Governorate == "Zarqa", 1, 0),
    Governorate_Balqa   = ifelse(Governorate == "Balqa", 1, 0),
    Governorate_Maan    = ifelse(Governorate == "Maan", 1, 0),
    Governorate_Aqaba   = ifelse(Governorate == "Aqaba", 1, 0),
    Governorate_Karak   = ifelse(Governorate == "Karak", 1, 0),
    Governorate_Jarash  = ifelse(Governorate == "Jerash", 1, 0),
    Governorate_Madaba  = ifelse(Governorate == "Madaba", 1, 0),
    Governorate_Ajlun   = ifelse(Governorate == "Ajloun", 1, 0)
  ) %>%
  dplyr::mutate(
    score_postshock =
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
      -0.3311827577 * HH.owns.livestock_postshock +
      0.0543048142 * Imputed.livestock.productivity +
      0.1069436887 * HH.owns.land +
      0.4435800352 * Owns.a.car_postshock +
      0.1643461081 * Private.cars +
      -0.0274362948 * Age.newest.car +
      0.0004164393  * Age.newest.car_sq +
      0.1368038748  * Working.cars +
      0.0669149014  * Owns.commercial.property_postshock +
      0.2114789823  * Owns.stocks_postshock +
      0.0524404940  * House.type +
      0.0059023126  * Area.per.capita +
      0.1496706129  * Water.and.electricity.bills +
      -0.0725899939 * HH.lives.in.rural.area +
      0.0755378872  * Formal.income.earners_postshock +
      -0.0108665062 * Formal.income.earners_sq +
      -0.1050082539 * Governorate_Mafraq +
      0.0648695943  * Governorate_Amman +
      -0.1504316724 * Governorate_Tafilah +
      0.1297149377  * Governorate_Zarqa +
      -0.0916247636 * Governorate_Balqa +
      -0.0153260026 * Governorate_Maan +
      -0.1001710720 * Governorate_Aqaba +
      -0.0730428536 * Governorate_Karak +
      -0.0008941587 * Governorate_Jarash +
      0.0459832082  * Governorate_Madaba +
      -0.0099285333 * Governorate_Ajlun
  ) %>%
  dplyr::mutate(delta_score = score_postshock - SCORE)

summary_stats <- HIECS_data_common_F_shocked %>%
  dplyr::group_by(SP_program) %>%
  dplyr::summarise(
    avg_score_pre  = mean(SCORE, na.rm = TRUE),
    avg_score_post = mean(score_postshock, na.rm = TRUE),
    avg_change     = mean(delta_score, na.rm = TRUE),
    min_change     = min(delta_score, na.rm = TRUE),
    max_change     = max(delta_score, na.rm = TRUE),
    .groups = "drop"
  )
print(summary_stats)

# --- Poverty calculations (unchanged logic/objects) ---
PL <- closest_threshold
PL_monthly <- exp(PL)
data2 <- HIECS_data_common_F_shocked
PL_JOD <- exp(PL)

poverty_by_region <- data2 %>%
  dplyr::mutate(poor = ifelse(SCORE < PL, 1, 0)) %>%
  dplyr::group_by(Governorate) %>%
  dplyr::summarise(Poverty = sum(poor * weights) / sum(weights) * 100, .groups = "drop")

poverty_by_region

data2$SCORE_NEW_climate <- data2$score_postshock
data2$poor_new_climate  <- ifelse(data2$SCORE_NEW_climate < PL, 1, 0)
Poverty_new_climate <- sum(data2$poor_new_climate * data2$weights) / sum(data2$weights) * 100

Poverty

poverty_by_region_NEW <- data2 %>%
  dplyr::mutate(poor_new_climate = ifelse(SCORE_NEW_climate < PL, 1, 0)) %>%
  dplyr::group_by(Governorate) %>%
  dplyr::summarise(Poverty = sum(poor_new_climate * weights) / sum(weights) * 100, .groups = "drop")

poverty_by_region
poverty_by_region_NEW

data2 <- data2 %>%
  dplyr::mutate(
    SP_program_combo_new_climate = dplyr::case_when(
      SP_program == "Beneficiary"  & poor_new_climate == 1 ~ "Beneficiary & poor",
      SP_program == "Beneficiary"  & poor_new_climate == 0 ~ "Beneficiary & non-poor",
      SP_program == "Eligible"     & poor_new_climate == 1 ~ "Eligible & poor",
      SP_program == "Eligible"     & poor_new_climate == 0 ~ "Eligible & non-poor",
      SP_program == "Non-Eligible" & poor_new_climate == 1 ~ "Non-Eligible & poor",
      TRUE ~ "Non-Eligible & non-poor"
    )
  )

data2$SP_program_combo_new_climate <- factor(
  data2$SP_program_combo_new_climate,
  levels = c("Beneficiary & poor", "Beneficiary & non-poor",
             "Eligible & poor", "Eligible & non-poor",
             "Non-Eligible & poor", "Non-Eligible & non-poor")
)

data2 <- data2 %>%
  dplyr::mutate(
    Value_CT_3 = dplyr::case_when(
      SP_program_combo_new_climate %in% c("Beneficiary & poor", "Beneficiary & non-poor",
                                          "Eligible & poor", "Non-Eligible & poor") & HH.Size == 1 ~ 40,
      SP_program_combo_new_climate %in% c("Beneficiary & poor", "Beneficiary & non-poor",
                                          "Eligible & poor", "Non-Eligible & poor") & HH.Size > 1 & HH.Size <= 5 ~ pmin(40 + (HH.Size - 1) * 15, 100),
      SP_program_combo_new_climate %in% c("Beneficiary & poor", "Beneficiary & non-poor",
                                          "Eligible & poor", "Non-Eligible & poor") & HH.Size >= 6 ~ 100,
      TRUE ~ 0
    ),
    Total_Value_CT_3 = dplyr::case_when(
      SP_program_combo_new_climate %in% c("Beneficiary & poor", "Beneficiary & non-poor",
                                          "Eligible & poor", "Non-Eligible & poor") ~
        Value_CT_3 + ifelse(Added_value == 1, 35, 0),
      TRUE ~ Value_CT_3
    ),
    Share_benefit_expenditure_CT_3 = Total_Value_CT_3 / Monthly_expenditure * 100
  )
