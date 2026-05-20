library(shiny)
library(shinydashboard)
source("global.R", local = TRUE)  # asegura que todo está cargado


#UI Definition --------------------------------------------------------------------------------------------------

# Main UI definition using shinydashboard's dashboardPage layout
ui <- dashboardPage(
  
  #  HEADER 
  dashboardHeader(
    title = uiOutput("dynamic_header"),  # Dynamic title that changes based on language or scenario
    
    # Right-aligned logos inside the header
    tags$li(
      class = "dropdown",  # Necessary class for correct positioning in the header
      tags$div(
        style = "display: flex; align-items: center; gap: 10px; padding-top: 10px;",
        
        # Logos for NAF, MSD, ESCWA
        tags$img(src = "https://www.naf.gov.jo/ebv4.0/root_storage/ar/eb_homepage/naf_logo2.png", height = "35px"),
        tags$img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRYcX_iLK1Srwu35rKNwfB3Eu1kwZm81TlAjA&s", height = "35px"),
        tags$img(src = "https://unttc.org/sites/unttc/files/2020-11/escwa.png", height = "35x")
      )
    )
  ),
  
  #SIDEBAR 
  dashboardSidebar(
    # CSS customization for RTL/LTR layout and active tab styling
    tags$head(
      tags$style(HTML("
        .sidebar-toggle { display: none !important; }  /* Hide sidebar toggle button */
        
        /* RTL adjustments */
        .rtl .main-sidebar { right: 0; left: auto; }
        .rtl .content-wrapper, .rtl .main-footer { margin-right: 230px; margin-left: 0; }
        .rtl .sidebar-toggle { left: auto; right: 0; }
        
        /* LTR adjustments */
        .ltr .main-sidebar { left: 0; right: auto; }
        .ltr .content-wrapper, .ltr .main-footer { margin-left: 230px; margin-right: 0; }
        .ltr .sidebar-toggle { left: 0; right: auto; }
        
        /* Direction and alignment */
        .rtl { direction: rtl; text-align: right; }
        .ltr { direction: ltr; text-align: left; }

        /* Active menu item color customization */
        .sidebar-menu li.active a {
          background-color: #34495e !important;
          color: #037ffc !important;
        }
      "))
    ),
    
    # Sidebar menu navigation
    sidebarMenu(
      id = "tabs",  # Allows tracking of active tab
      menuItem(textOutput("newtranslation1"), tabName = "params", icon = icon("sliders")),
      menuItem(textOutput("newtranslation34"), tabName = "gov_analysis", icon = icon("map")),
      menuItem(textOutput("newtranslation55"), tabName = "comparing_scenarios", icon = icon("balance-scale")),
      menuItem(textOutput("newtranslation149"), tabName = "dynamic", icon = icon("chart-bar")),
      menuItem(textOutput("newtranslation69"), tabName = "setting_simulation_parameters", icon = icon("screwdriver-wrench"))
    )
  ),
  
  # BODY
  dashboardBody(
    # JavaScript handlers for UI dynamic updates
    tags$head(
      tags$script(HTML("
        // Updates label text of a search button dynamically
        Shiny.addCustomMessageHandler('updateSearchButton', function(label) {
          $('#searchButton').text(label);
        });

        // Sets the body class for RTL or LTR language direction
        Shiny.addCustomMessageHandler('setClass', function(lang) {
          $('body').removeClass('rtl ltr').addClass(lang);
        });

        // Changes body background based on active tab
        $(document).ready(function() {
          Shiny.addCustomMessageHandler('updateBodyClass', function(tabName) {
            if (tabName === 'toolbox') {
              $('body').addClass('toolbox-bg');
            } else {
              $('body').removeClass('toolbox-bg');
            }
          });
        });
      "))
    ),
    
    #  TAB CONTENT 
    tabItems(
      
      # --- PARAMETERS TAB ---
      tabItem(tabName = "params",
              
              # Logo and partner display row
              fluidRow(
                box(
                  width = 12,
                  tags$div(
                    style = "display: flex; justify-content: space-between; margin-bottom: 20px;",
                    
                    # Left side logos
                    tags$div(
                      style = "display: flex; align-items: center;",
                      tags$img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTQR8PhauB_dMJoV8qLWwqYc53ifTm_-uw7Fg&s", height = "100px", style = "margin-right: 30px;"),
                      tags$img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcR9PnQs7HlsQRCmbcYnHLx3oMuNcIgRri_1dQ&s", height = "80px")
                    ),
                    
                    # Right side text and ESCWA logo
                    tags$div(
                      style = "display: flex; align-items: center;",
                      tags$span(textOutput("newtranslation3"), style = "margin-right: 10px;"),
                      tags$img(src = "https://msme-resurgence.unctad.org/sites/smesurge/files/2021-07/unescwa%20main%20logo.png", height = "70px")
                    )
                  )
                )
              ),
              
              # Title row
              fluidRow(
                box(
                  width = 12,
                  h1(textOutput("newtranslation2"), 
                     style = "text-align: center; color: #2c3e50; font-weight: bold;")
                )
              ),
              
              # Language selection
              fluidRow(
                radioGroupButtons("translation", h3(""),
                                  choices = c("English" = 1, "العربية" = 2),
                                  selected = 1,
                                  status = "primary",
                                  justified = TRUE
                )
              ),
              
              # Input controls for shocks and parameters
              fluidRow(
                column(width = 6,
                       box(title = textOutput("newtranslation4"), 
                           numericInput("inflationShock", textOutput("newtranslation5"), value = 1, min = 0, max = 20),
                           helpText(textOutput("newtranslation6")),
                           
                           
                           # Dynamic UI for employment input
                           uiOutput("employment_input_ui"),
                           uiOutput("employment_numeric_ui")
                           
                       ) ,
                       
                       box( title= textOutput("newtranslation147"), 
                            # Numeric input for mortality shock
                            numericInput("mortalityShock", textOutput("newtranslation138"), value = 3, min = 0, max = 100),
                            helpText(textOutput("newtranslation139"))
                       ),                      
                       
                       box(title = textOutput("newtranslation9"), 
                           tagList(
                             #numericInput("targetPoverty", textOutput("newtranslation10"), value = 35, min = 5, max = 50),
                             #helpText(textOutput("newtranslation11")),
                             
                             numericInput("exchangeRate", textOutput("newtranslation12"), value = 1.41, min = 1, max = 2, step = 0.01),
                             helpText(textOutput("newtranslation13"))
                           )
                       )
                ),
                
                # Informational box with program description
                box(title = textOutput("newtranslation15"), width = 6, background = "teal",
                    tags$div(style = "font-family: 'Arial', sans-serif; font-size: 14px; color: #ffffff;",
                             tags$h3(textOutput("newtranslation16"), style = "font-size: 16px; font-weight: bold;"),
                             tags$p(textOutput("newtranslation17")),
                             tags$h3(textOutput("newtranslation92"), style = "font-size: 15px; font-weight: bold; margin-top: 10px;"),
                             br(),
                             tags$h3(textOutput("newtranslation18"), style = "font-size: 16px; font-weight: bold; margin-top: 10px;"),
                             tags$ul(
                               tags$li(tags$b(textOutput("newtranslation19")), tags$p(textOutput("newtranslation19b"))),
                               tags$li(tags$b(textOutput("newtranslation20")), tags$p(textOutput("newtranslation20b"))),
                               tags$li(tags$b(textOutput("newtranslation21")), tags$p(textOutput("newtranslation21b"))),
                               tags$li(tags$b(textOutput("newtranslation22")), tags$p(textOutput("newtranslation22b"))),
                               tags$li(tags$b(textOutput("newtranslation101")), tags$p(textOutput("newtranslation101b"))),
                               tags$li(tags$b(textOutput("newtranslation103")), tags$p(textOutput("newtranslation104")))
                             )
                    )
                )
              ),
              
              # Action buttons row
              fluidRow(
                column(width = 8,
                       actionButton("runSimulation", textOutput("newtranslation24"), icon = icon("play"), class = "btn-primary"),
                       actionButton("goToSimParams", textOutput("newtranslation25"), icon = icon("cog"), class = "btn-default")
                )
              ),
              
              # Summary statistics and map row
              fluidRow(
                box(title = textOutput("newtranslation26"), width = 6,
                    fluidRow(
                      valueBoxOutput("currentBeneficiaries", width = 6),
                      valueBoxOutput("womenPercentage", width = 6)
                    ),
                    fluidRow(
                      valueBoxOutput("currentCostJOD", width = 6),
                      valueBoxOutput("currentCostUSD", width = 6)
                    )
                ),
                box(title = textOutput("newtranslation32"), width = 6,
                    leafletOutput("BeneficiariesMap_current")
                )
              )
      ),
      tabItem(tabName = "setting_simulation_parameters",
              fluidRow(
                box(title = textOutput("newtranslation70"), width = 6,
                    sliderInput("livestockLoss", textOutput("newtranslation71"), 
                                min = 0, max = 1, value = 0.11),
                    sliderInput("landAffected", textOutput("newtranslation72"), 
                                min = 0, max = 1, value = 0.35),
                    sliderInput("jobLoss", textOutput("newtranslation73"), 
                                min = 0, max = 1, value = 0.1),
                    sliderInput("carSale", textOutput("newtranslation74"), 
                                min = 0, max = 1, value = 0.2),
                    sliderInput("stockSale", textOutput("newtranslation75"), 
                                min = 0, max = 1, value = 0.3),
                    sliderInput("propertySale", textOutput("newtranslation76"), 
                                min = 0, max = 1, value = 0.15)
                ),
                box(title = textOutput("newtranslation78"), width = 6,
                    sliderInput("highRiskLoss", textOutput("newtranslation79"), 
                                min = 0, max = 1, value = 0.4, step = 0.05),
                    sliderInput("moderateRiskLoss", textOutput("newtranslation80"), 
                                min = 0, max = 1, value = 0.3, step = 0.05),
                    sliderInput("lowRiskLoss", textOutput("newtranslation81"), 
                                min = 0, max = 1, value = 0.2, step = 0.05)
                ),
                
                
                box(title = textOutput("newtranslation142"), width = 6,
                    sliderInput("n_years", textOutput("newtranslation143"), 
                                min = 0, max = 5, value = 5),
                    sliderInput("p_num_0_14", textOutput("newtranslation144"), 
                                min = 0, max = 1, value =0.9),
                    sliderInput("p_a_14", textOutput("newtranslation145"), 
                                min = 0, max = 1, value = 0.1),
                    sliderInput("p_a_64", textOutput("newtranslation146"), 
                                min = 0, max = 1, value = 0.05)
                    
                ),
                
              ),
              fluidRow(
                
                actionButton("goToShockparams", textOutput("newtranslation83"), 
                             icon = icon("sliders"), class = "btn-default")
              )
      ),
      tabItem(tabName = "comparing_scenarios",
              fluidRow(
                box(
                  title = div(
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    textOutput("newtranslation151"),
                    tags$img(
                      src = "https://upload.wikimedia.org/wikipedia/commons/c/c0/Flag_of_Jordan.svg",
                      height = "20px"
                    )
                  ),
                  width = 12,
                  background = "light-blue"
                )
              )
              ,
              fluidRow(
                column(
                  width = 3,
                  box(title = textOutput("newtranslation155"), width = 12,status = "primary", solidHeader = TRUE,
                      checkboxGroupInput("selected_scenarios_count",
                                         label = textOutput("newtranslation156"),
                                         choices = NULL,
                      ))),
                column(
                  width = 9,
                  box(
                    title = textOutput("newtranslation66"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    plotOutput("scenarioBarChart")
                  ))
              ),
              fluidRow(
                column(
                  width = 3,
                  box(title = textOutput("newtranslation59"), width = 12,status = "primary", solidHeader = TRUE,
                      checkboxGroupInput("selected_scenarios_value",
                                         label = textOutput("newtranslation60"),
                                         choices = NULL
                      ))),
                column(
                  width = 9,
                  box(
                    title = textOutput("newtranslation67"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    plotOutput("scenarioBarChart2")
                  ))
              ),
              # fluidRow(
              #   box(plotOutput("scoreDistribution"), width = 6),
              #   box(plotOutput("beneficiaryMap"), width = 6)
              # )
      ),
      
      tabItem(tabName = "dynamic",
              
              fluidRow(
                box(
                  title = div(
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    textOutput("newtranslation150"),
                    tags$img(
                      src = "https://upload.wikimedia.org/wikipedia/commons/c/c0/Flag_of_Jordan.svg",
                      height = "20px"
                    )
                  ),
                  width = 12,
                  background = "light-blue"
                )
              ),
              
              fluidRow(
                box(DT::dataTableOutput("table_dyn_all"), width = 12)
                
              ),
              
              fluidRow(
                column(
                  width = 3,
                  box(title = textOutput("newtranslation41"), width = 12,status = "primary", solidHeader = TRUE,
                      checkboxGroupInput("selected_scenarios_count_dynamic",
                                         label = textOutput("newtranslation43"),
                                         choices = NULL,
                      ))),
                column(
                  width = 9,
                  box(
                    title = textOutput("newtranslation152"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    plotOutput("dynamicBarChart")
                  ))
              ),
              
              fluidRow(
                column(
                  width = 3,
                  box(title = textOutput("newtranslation59a"), width = 12,status = "primary", solidHeader = TRUE,
                      checkboxGroupInput("selected_scenarios_value_dynamic",
                                         label = textOutput("newtranslation60a"),
                                         choices = NULL,
                      ))),
                column(
                  width = 9,
                  box(
                    title = textOutput("newtranslation153"),
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    plotOutput("dynamicBarChart2")
                  ))
              )
              
      ),
      
      
      # Combined Governorate Analysis tab
      tabItem(tabName = "gov_analysis",
              
              fluidRow(
                box(
                  title = div(
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    textOutput("newtranslation154"),
                    tags$img(
                      src = "https://upload.wikimedia.org/wikipedia/commons/c/c0/Flag_of_Jordan.svg",
                      height = "20px"
                    )
                  ),
                  width = 12,
                  background = "light-blue"
                )
              ),
              
              
              
               fluidRow(
                box(DT::dataTableOutput("resultsTable"), width = 12)
                
              ),
              fluidRow(
                box(width = 12,
                    uiOutput("scenarioFilterUI")  # Dynamic UI for scenario selection
                )
              ),
              fluidRow(
                box(width = 12,
                    title = textOutput("newtranslation136"),
                    tabBox(width = 12,
                           tabPanel(textOutput("newtranslation37"),
                                    leafletOutput("BeneficiariesMap"))
                    )
                )
              ),
              fluidRow(
                box(width = 12,
                    title = textOutput("newtranslation35a"),
                    DTOutput("govTable")
                )
              )
      )
    )
  )
)



#Server--------------------

server <- function(input, output, session) {
  
  
  #Observer------------------- 
  observeEvent(input$translation, {
    dir <- if (input$translation == 2) "rtl" else "ltr"
    session$sendCustomMessage("setClass", dir)
  })
  
  # Reactive values to store simulation results
  simResults <- reactiveValues(
    finalTable = NULL,
    finalTable_gov = NULL,
    results_dynamic_all = NULL,
    results_dynamic_gov = NULL
  )
  
  
  finalTable_gov_i18n <- reactive({
    req(simResults$finalTable_gov, input$translation)
    df <- simResults$finalTable_gov
    if (as.numeric(input$translation) == 2) {
      df <- df %>%
        dplyr::mutate(
          Governorate  = dplyr::recode(Governorate, !!!gov_en2ar),
          Scenario_name = scenario_i18n(Scenario_name)
        )
    }
    df
  })
  
  
  # Map-safe version (English keys)
  filtered_gov_data <- reactive({
    req(simResults$finalTable_gov, input$scenarioFilter)
    simResults$finalTable_gov %>%
      filter(Scenario_name == input$scenarioFilter) %>%
      mutate(Governorate = recode(Governorate,
                                  "Tafila" = "Tafilah",
                                  "Jerash" = "Jarash",
                                  "Ajloun" = "Ajlun",
                                  "Maan"   = "Ma`an"
      ))
  })
  
  # Table version (translated)
  filtered_gov_data_translated <- reactive({
    req(filtered_gov_data(), input$translation)
    df <- filtered_gov_data()
    if (as.numeric(input$translation) == 2) {
      df <- df %>%
        mutate(
          Governorate = recode(Governorate,
                               "Ajlun"="عجلون","Amman"="عمان","Aqaba"="العقبة","Balqa"="البلقاء",
                               "Irbid"="إربد","Jarash"="جرش","Karak"="الكرك","Maan"="معان","Ma`an"="معان",
                               "Madaba"="مادبا","Mafraq"="المفرق","Tafilah"="الطفيلة","Zarqa"="الزرقاء"
          ),
          Scenario_name = scenario_i18n(Scenario_name) # your existing translation logic
        )
    }
    df
  })
  # 
  
  
  employment_impact <- reactive({
    if(input$employment_input_method == "gdp") {
      # Calculate from GDP growth
      increase_gdp_percap <- (input$increase_gdp_percap)/100
      
      
      source(file.path(inputLocation,"Models/Unemployment/MacroEconShock.R"))
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
  
  observe({ req(simResults$finalTable_gov); print(names(simResults$finalTable_gov)) })
  
  
  # --- helpers ---------------------------------------------------------------
  get_lbl <- function(i) {
    idx <- 1 + as.numeric(input$translation) # 2=EN, 3=AR in your VarLabels
    as.matrix(VarLabels[i, ])[idx, 1]
  }
  
  # Governorate display map (UI/table only). Map/shapefile keeps English keys.
  gov_en2ar <- c(
    "Ajlun"="عجلون","Amman"="عمان","Aqaba"="العقبة","Balqa"="البلقاء",
    "Irbid"="إربد","Jarash"="جرش","Karak"="الكرك","Maan"="معان","Ma`an"="معان",
    "Madaba"="مادبا","Mafraq"="المفرق","Tafilah"="الطفيلة","Zarqa"="الزرقاء"
  )
  
  # Scenario label translator: returns the same English string (translation==1),
  # or Arabic using VarLabels + current inputs (translation==2).
  scenario_i18n <- function(x) {
    if (as.numeric(input$translation) == 1) return(x)  # show original English
    # Arabic pieces from VarLabels
    s0 <- get_lbl(61)   # Current status
    s1 <- get_lbl(62)   # Households under PL
    s2L <- get_lbl(63)  # "HH under PL after inflation shock"
    s3L <- get_lbl(140)  # Household 
    pct <- get_lbl(64)  # "%"
    rate <- get_lbl(141) # rate
    gdpL <- get_lbl(134)
    gdpPct <- get_lbl(135)  # "%"
    uempL <- get_lbl(136)
    pp   <- get_lbl(137)    # "percentage points"
    
    infl_val <- input$inflationShock
    
    gdp_val  <- input$increase_gdp_percap
    uemp_val <- input$cumulative_effect_1pct_user
    mort_val <- input$mortalityShock
    
    vapply(x, function(s) {
      if (grepl("^Current status$", s))                return(s0)
      if (grepl("^Households under poverty line$", s)) return(s1)
      if (grepl("^Households under PL after inflation shock", s))
        return(paste0(s2L, " ", infl_val, " ", pct))
      
      if (grepl("^Climate change impact on water scarcity", s))                    return(get_lbl(65))
      if (grepl("^GDP shock", s))                      return(paste0(gdpL, " ", gdp_val, " ", gdpPct))
      if (grepl("^Unemployment shock", s))             return(paste0(uempL, " ", uemp_val, " ", pp))
      if (grepl("^Households under PL after aging and change in mortality", s))
        return(paste0(s3L, " ", mort_val, " ", rate))
      
      s
    }, FUN.VALUE = character(1))
  }
  
  
  
  ##Observer SIMULATION ----------------------------------------------------------------  
  observeEvent(input$runSimulation, {
    showNotification(textOutput("newtranslation23"), type = "message")
    # 1.0 Preliminaries -
    # Extract parameters from inputs
    target_poverty_rate <- input$targetPoverty 
    exc_rateJOD_USD <- input$exchangeRate
    shock_inflation <- input$inflationShock 
    mortality_rate <- input$mortalityShock 
    n_years = input$n_years
    p_num_0_14 = input$p_num_0_14
    p_a_14 = input$p_a_14
    p_a_64 = input$p_a_64
    
    
    highRiskLoss     = input$highRiskLoss    
    moderateRiskLoss = input$moderateRiskLoss
    lowRiskLoss           = input$lowRiskLoss     

    
    
    source(file.path(root_dir,"chunks/00_adjusting_rawdata_all.R"), local = TRUE)
    
    #Reading files
    source(file.path(root_dir,"chunks/01_data_prep.R"), local = TRUE)
    cleanData <- ApplicantsData
    
    #Parameters UCT  
    source(file.path(root_dir,"chunks/02_estimating_uct.R"), local = TRUE)
    dataCH2=data
    #Poverty&Inflation   
    source(file.path(root_dir,"chunks/03_povertyline.R"), local = TRUE)
    dataCH2 <- data
    #Climate
    source(file.path(root_dir,"chunks/04_climate.R"), local = TRUE)
    data3 = data2
    
    # Economic
    source(file.path(root_dir,"chunks/05_economic.R"), local = TRUE)
    #demographics
    source(file.path(root_dir,"chunks/06_demographics1.R"), local = TRUE)   
    
    ##Final results
    source(file.path(root_dir,"chunks/07_final_tables01.R"), local = TRUE)
    
    
    simResults$finalTable <- results %>%
      mutate(variation_count = ifelse(Scenario_name == "Current status", 0, 
                                      (count - count[Scenario_name == "Current status"]) / count[Scenario_name == "Current status"] * 100))%>%
      dplyr:: select (Scenario_name,  count, Freq, variation_count, value_yearly_jod_CT, value_yearly_usd_CT) 
    
    
    
    simResults$finalTable_gov <- results_gov %>%
      group_by(Governorate) %>%
      mutate(variation_count = ifelse(Scenario_name == "Current status", 0, 
                                      (count - count[Scenario_name == "Current status"]) / count[Scenario_name == "Current status"] * 100))%>%
      left_join(VA_data, by = "Governorate")%>%
      mutate (Governorate, recode(Governorate, "Ma`an"="Maan" )) %>% 
      dplyr:: select (Scenario_name, Governorate, count, Freq, variation_count, value_yearly_jod_CT, value_yearly_usd_CT) 
    
    
    
    print(simResults$finalTable_gov)
    
    simResults$results_dynamic_all     <-  results_dynamic_all  %>%
      dplyr::mutate(
        year = dplyr::if_else(is.na(.data$year), 0L, as.integer(.data$year)),
        Scenario_name = trimws(Scenario_name)
      )
    
    
    
    
    
    simResults$results_dynamic_gov <- results_dynamic_gov %>%
      dplyr::mutate(
        year = dplyr::if_else(is.na(.data$year), 0L, as.integer(.data$year)),
        Scenario_name = trimws(Scenario_name),
        Governorate = dplyr::recode(Governorate,
                                    "Tafila"="Tafilah","Jerash"="Jarash","Aljoun"="Ajlun","Maan"="Ma`an")
      )
    
    print(simResults$results_dynamic_all)
  
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
  
  jordan_path <- file.path(Gislocation, "StatPlanet_Jordan/map/Jordan/JOR_adm1.shp")
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
output$scenarioFilterUI <- renderUI({
  req(simResults$finalTable_gov)
  canon <- unique(simResults$finalTable_gov$Scenario_name)      # EN values (stable)
  labels <- scenario_i18n(canon)                                # display labels (EN/AR)
  # Named choices: names = what user sees, values = canonical EN used for filtering
  selected_val <- isolate(input$scenarioFilter)
  if (is.null(selected_val) || !(selected_val %in% canon)) selected_val <- "Current status"
  selectInput("scenarioFilter", get_lbl(41), choices = stats::setNames(canon, labels), selected = selected_val)
})



output$govTable <- DT::renderDataTable({
  
  req(simResults$finalTable_gov)
  header_rows2 <- 112:118
  translated_headers <- sapply(header_rows2, function(row) {
    as.matrix(VarLabels[row, 1 + as.numeric(input$translation)])
  })
  
  DT::datatable(
    req(filtered_gov_data_translated()),   # <- translated view, map stays on canonical
    options = list(
      pageLength = 10, autoWidth = TRUE, scrollX = TRUE,
      dom = 'Bfrtip', buttons = c('copy','csv','excel','pdf','print')
    ),
    rownames = FALSE, extensions = 'Buttons',
    colnames = translated_headers, filter = 'top'
  ) %>% DT::formatRound(columns = 4:7, digits = 2)
})

# Vulnerability map
# Replace the map rendering functions with these corrected versions:

output$BeneficiariesMap <- renderLeaflet({
  req(filtered_gov_data())
  req(translated_title2())
  
  jordan_path <- file.path(Gislocation, "StatPlanet_Jordan/map/Jordan/JOR_adm1.shp")
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
  req(input$inflationShock,input$mortalityShock, input$translation, input$employment_input_method)
  
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
  text140a <- getText(140)
  text141a <- getText(141)
  
  # Combine inflation shock into label
  shock_label <- paste0(text63a, " ", input$inflationShock, text64a)
  
  GDP_label   <- paste0(text134a, " ", input$increase_gdp_percap , text135a)
  Uemp_pp     <- paste0(text136a, " ", input$cumulative_effect_1pct_user , " ", text137a)
  
  employment_label <- if (input$employment_input_method == "gdp") {
    GDP_label
  } else {
    Uemp_pp
  }
  mort_label <- paste0(text140a, " ", input$mortalityShock, text141a)
  
  print("==> Updating checkbox with these labels:")
  print(c(
    text61a,
    text62a,
    shock_label,
    text65a,
    employment_label,
    mort_label
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
      employment_label,
      mort_label
    ))
  
})

observe({
  req(input$inflationShock,input$mortalityShock, input$translation, input$employment_input_method)
  
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
  text140a <- getText(140)
  text141a <- getText(141)
  
  # Combine inflation shock into label
  shock_label <- paste0(text63a, " ",input$inflationShock, text64a)
  
  GDP_label   <- paste0(text134a, " ", input$increase_gdp_percap , text135a)
  Uemp_pp     <- paste0(text136a, " ", input$cumulative_effect_1pct_user , " ", text137a)
  
  employment_label <- if (input$employment_input_method == "gdp") {
    GDP_label
  } else {
    Uemp_pp
  }
  mort_label <- paste0(text140a, " ",input$mortalityShock, text141a)
  
  # Update the checkbox UI
  updateCheckboxGroupInput(
    session,
    inputId = "selected_scenarios_value",
    choices = c(
      paste0(text61a),
      paste0(text62a),
      shock_label,
      paste0(text65a),
      employment_label,
      mort_label
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


#####Dynamic comparison



output$table_dyn_all <- DT::renderDataTable({
  req(simResults$results_dynamic_all)
  
  # Keep only UCT Beneficiaries rows
  df <- simResults$results_dynamic_all %>%
    
    
    dplyr::select(
      Scenario_name,                 # 1) Scenario Name
      count,                         # 2) Number UCT Beneficiaries
      Freq,                          # 3) Freq UCT Beneficiaries
      value_yearly_jod_CT,           # 5) Total cost Value (JOD M)
      value_yearly_usd_CT            # 6) Total cost (USD M)
    )
  
  
  #   Variation relative to Current PMT %, Total cost Value (JOD M), Total cost (USD M)]
  header_rows <- c(43, 44, 45,  47, 48)  # 
  
  translated_headers <- sapply(header_rows, function(row) {
    # language: 1=EN, 2=AR in your app; VarLabels: col 2=EN, col 3=AR
    as.matrix(VarLabels[row, 1 + as.numeric(input$translation)])
  })
  
  DT::datatable(
    df,
    rownames = FALSE,
    colnames = translated_headers,
    options = list(
      pageLength = 10,
      autoWidth  = TRUE,
      scrollX    = TRUE
    )
  ) %>%
    # one decimal place per your preference
    DT::formatRound(columns = c("Freq",  "value_yearly_jod_CT", "value_yearly_usd_CT"), digits = 1)
})


observe({
  req(input$mortalityShock, input$translation, input$n_years, simResults$results_dynamic_all)
  
  # --- language selection already set by your code ---
  lang_index <- switch(input$translation, "1" = 2, "2" = 3, 2)
  code_column <- trimws(VarLabels[, "Code"])
  getText <- function(code_number) {
    row_index <- which(code_column == as.character(code_number))
    if (!length(row_index)) return("MISSING")
    txt <- VarLabels[row_index, lang_index]
    ifelse(is.na(txt), "MISSING", trimws(txt))
  }
  
  # Helper to format one dynamic label for a given year
  mort_label_year <- function(y) {
    paste0(getText(148), input$mortalityShock, "x1000 - Year ", y)
  }
  
  mort_label_year_short <- function(y) {
    paste0( input$mortalityShock, "x1000 - Year ", y)
  }
  
  # Base scenario labels (0 and 1)
  text61a <- getText(61)  # "Current status"
  text62a <- getText(62)  # "Households under poverty line"
  
  n <- as.integer(input$n_years)
  mort_labels <- vapply(seq_len(n), mort_label_year, character(1))
  mort_labels_short <- vapply(seq_len(n), mort_label_year_short, character(1))
  
  
  scenario_labels <- c(text61a, text62a, mort_labels)
  scenario_labels_short <- c(text61a, text62a, mort_labels_short)
  
  
  # IMPORTANT: make the choice "values" equal to the visible labels.
  # This way you can match directly on Scenario_name later.
  names(scenario_labels) <- scenario_labels  # named vector (label -> value)
  names(scenario_labels_short) <- scenario_labels_short  # named vector (label -> value)
  
  
  for (i in seq_along(scenario_labels)) {
    if (i <= nrow(simResults$results_dynamic_all)) {
      simResults$results_dynamic_all$Scenario_name[i] <-scenario_labels[i]
      simResults$results_dynamic_all$Short_name[i] <-scenario_labels_short[i]
      
    }
  }
  
  # If you want to preselect all, use 'selected = scenario_labels'; or none -> NULL
  updateCheckboxGroupInput(
    session,
    inputId  = "selected_scenarios_count_dynamic",
    choices  = scenario_labels,
    selected = scenario_labels   # or NULL if you want nothing preselected
  )
})





filtered_count_dynamic <- reactive({
  req(simResults$results_dynamic_all, input$selected_scenarios_count_dynamic)
  
  df_dyn <- simResults$results_dynamic_all
  df_dyn$Scenario_name <- trimws(df_dyn$Scenario_name)
  
  df_out <- dplyr::filter(df_dyn, Scenario_name %in% input$selected_scenarios_count_dynamic)
  
  # If you want it ordered like the checkbox list:
  ord <- match(df_out$Scenario_name, input$selected_scenarios_count_dynamic)
  df_out <- df_out[order(ord, na.last = TRUE), , drop = FALSE]
  df_out
})



output$dynamicBarChart <- renderPlot({
  df_dyn <- filtered_count_dynamic()
  req(nrow(df_dyn) > 0)
  
  # Make room for long x labels
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mar = c(10, 4, 3, 1))  # enlarge bottom margin for rotated names
  
  # Colors
  color_scale <- colorRampPalette(c("lightgreen", "seagreen3", "darkgreen"))
  
  bar_colors <- color_scale(nrow(df_dyn))
  
  # Use labels directly
  bp <- barplot(
    height = df_dyn$count,
    names.arg = df_dyn$Short_name,  # <-- this was your commented 'error'
    col = bar_colors,
    border = "white",
    space = 0.6,
    ylim = c(0, max(df_dyn$count, na.rm = TRUE) * 1.25),
    ylab = "Count",
    las = 2,           # rotate x labels vertical to fit
    cex.names = 0.9    # slightly smaller names
  )
  
  # Values on top
  text(
    x = bp,
    y = df_dyn$count,
    labels = format(df_dyn$count, big.mark = ","),
    pos = 3,
    cex = 0.9
  )
  
  # Optional trend line
  lines(x = bp, y = df_dyn$count, type = "b", lwd = 2, col = "darkblue", pch = 16)
  
  # Optional % change labels between bars
  if (nrow(df_dyn) >= 2) {
    for (i in 2:nrow(df_dyn)) {
      prev <- df_dyn$count[i - 1]
      curr <- df_dyn$count[i]
      if (prev > 0 && is.finite(prev) && is.finite(curr)) {
        pct_change <- round((curr - prev) / prev * 100, 1)
        label <- paste0(ifelse(pct_change >= 0, "+", ""), pct_change, "%")
        x_pos <- (bp[i] + bp[i - 1]) / 2
        y_pos <- max(curr, prev) * 1.05
        text(x = x_pos, y = y_pos, labels = label, col = "darkred", cex = 0.9)
      }
    }
  }
})








observe({
  req(input$mortalityShock, input$translation, input$n_years, simResults$results_dynamic_all)
  
  # --- language selection already set by your code ---
  lang_index <- switch(input$translation, "1" = 2, "2" = 3, 2)
  code_column <- trimws(VarLabels[, "Code"])
  getText <- function(code_number) {
    row_index <- which(code_column == as.character(code_number))
    if (!length(row_index)) return("MISSING")
    txt <- VarLabels[row_index, lang_index]
    ifelse(is.na(txt), "MISSING", trimws(txt))
  }
  
  # Helper to format one dynamic label for a given year
  mort_label_year <- function(y) {
    paste0(getText(148), input$mortalityShock, "x1000 - Year ", y)
  }
  
  mort_label_year_short <- function(y) {
    paste0( input$mortalityShock, "x1000 - Year ", y)
  }
  
  # Base scenario labels (0 and 1)
  text61a <- getText(61)  # "Current status"
  text62a <- getText(62)  # "Households under poverty line"
  
  n <- as.integer(input$n_years)
  mort_labels <- vapply(seq_len(n), mort_label_year, character(1))
  mort_labels_short <- vapply(seq_len(n), mort_label_year_short, character(1))
  
  
  scenario_labels <- c(text61a, text62a, mort_labels)
  scenario_labels_short <- c(text61a, text62a, mort_labels_short)
  

  # IMPORTANT: make the choice "values" equal to the visible labels.
  # This way you can match directly on Scenario_name later.
  names(scenario_labels) <- scenario_labels  # named vector (label -> value)
  names(scenario_labels_short) <- scenario_labels_short  # named vector (label -> value)
  
  
  for (i in seq_along(scenario_labels)) {
    if (i <= nrow(simResults$results_dynamic_all)) {
      simResults$results_dynamic_all$Scenario_name[i] <-scenario_labels[i]
      simResults$results_dynamic_all$Short_name[i] <-scenario_labels_short[i]
      
      }
  }
  
  # If you want to preselect all, use 'selected = scenario_labels'; or none -> NULL
  updateCheckboxGroupInput(
    session,
    inputId  = "selected_scenarios_value_dynamic",
    choices  = scenario_labels,
    selected = scenario_labels   # or NULL if you want nothing preselected
  )
})





filtered_value_dynamic <- reactive({
  req(simResults$results_dynamic_all, input$selected_scenarios_value_dynamic)
  
  df_dyn <- simResults$results_dynamic_all
  df_dyn$Scenario_name <- trimws(df_dyn$Scenario_name)
  
  df_out <- dplyr::filter(df_dyn, Scenario_name %in% input$selected_scenarios_value_dynamic)
  
  # If you want it ordered like the checkbox list:
  ord <- match(df_out$Scenario_name, input$selected_scenarios_value_dynamic)
  df_out <- df_out[order(ord, na.last = TRUE), , drop = FALSE]
  df_out
})



output$dynamicBarChart2 <- renderPlot({
  df_dyn <- filtered_value_dynamic()
  req(nrow(df_dyn) > 0)
  
  # Make room for long x labels
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mar = c(10, 4, 3, 1))  # enlarge bottom margin for rotated names
  
  # Colors
  color_scale <- colorRampPalette(c("lightgreen", "seagreen3", "darkgreen"))
  bar_colors <- color_scale(nrow(df_dyn))
  
  # Use labels directly
  bp <- barplot(
    height = df_dyn$value_yearly_usd_CT,
    names.arg = df_dyn$Short_name,  
    col = bar_colors,
    border = "white",
    space = 0.6,
    ylim = c(0, max(df_dyn$value_yearly_usd_CT, na.rm = TRUE) * 1.25),
    ylab = "USD year",
    las = 2,           # rotate x labels vertical to fit
    cex.names = 0.9    # slightly smaller names
  )
  
  # Values on top
  text(
    x = bp,
    y = df_dyn$value_yearly_usd_CT,
    labels = format(df_dyn$value_yearly_usd_CT, big.mark = ","),
    pos = 3,
    cex = 0.9
  )
  
  # Optional trend line
  lines(x = bp, y = df_dyn$value_yearly_usd_CT, type = "b", lwd = 2, col = "darkblue", pch = 16)
  
  # Optional % change labels between bars
  if (nrow(df_dyn) >= 2) {
    for (i in 2:nrow(df_dyn)) {
      prev <- df_dyn$value_yearly_usd_CT[i - 1]
      curr <- df_dyn$value_yearly_usd_CT[i]
      if (prev > 0 && is.finite(prev) && is.finite(curr)) {
        pct_change <- round((curr - prev) / prev * 100, 1)
        label <- paste0(ifelse(pct_change >= 0, "+", ""), pct_change, "%")
        x_pos <- (bp[i] + bp[i - 1]) / 2
        y_pos <- max(curr, prev) * 1.05
        text(x = x_pos, y = y_pos, labels = label, col = "darkred", cex = 0.9)
      }
    }
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
  req(simResults$finalTable, input$inflationShock,input$mortalityShock, input$translation,  input$increase_gdp_percap)
  
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
  text140 <- as.matrix(VarLabels[140,])[1 + as.numeric(input$translation), 1]
  text141 <- as.matrix(VarLabels[141,])[1 + as.numeric(input$translation), 1]
  
  
  shock_label <- paste0(text63, " ",input$inflationShock, text64)
  GDP_label <- paste0(text134, " ", input$increase_gdp_percap , text135)
  Uemp_pp     <- paste0(text136, " ", input$cumulative_effect_1pct_user , " ", text137)
  
  employment_label <- if (input$employment_input_method == "gdp") {
    GDP_label
  } else {
    Uemp_pp
  }
  mort_label <- paste0(text140, " ",input$mortalityShock, text141)
  translated_names <- c(text61, text62, shock_label, text65, employment_label, mort_label)
  
  # Replace scenario names in the table (only for the first 5 rows)
  for (i in seq_along(translated_names)) {
    if (i <= nrow(simResults$finalTable)) {
      simResults$finalTable$Scenario_name[i] <- translated_names[i]
    }
  }
  
})



# --- tiny helpers --------------------------------------------------------------
get_label <- function(idx) {
  # mirrors your original as.matrix(...) indexing
  txt <- as.matrix(VarLabels[idx, ])
  txt[1 + as.numeric(input$translation), 1]
}
mk_text_output <- function(id, idx) {
  # defines output[[id]] <- renderText({ get_label(idx) })
  local({
    .id  <- id; .idx <- idx
    output[[.id]] <- renderText({ get_label(.idx) })
  })
}

# --- language direction + search button label ---------------------------------
observe({
  lang_class <- ifelse(input$translation == 1, "ltr", "rtl")
  session$sendCustomMessage("setClass", lang_class)
  session$sendCustomMessage("updateSearchButton", if (input$translation == 1) "Search" else "بحث")
})

# --- dynamic header ------------------------------------------------------------
output$dynamic_header <- renderUI({
  header_title <- get_label(109)
  dashboardHeader(title = span(header_title, style = "align: center; font-size: 20px;"))
})

# --- one-liner reactives that you already use elsewhere -----------------------
translated_title  <- reactive({ as.character(get_label(31)) })
translated_title2 <- reactive({ as.character(get_label(39)) })

# --- batch-generate all the simple renderText() labels ------------------------
# IDs that follow the "newtranslation{number}" pattern:
numeric_ids <- c(
  1:13,                      # 1..13
  15:23,                     # 15..23
  24:37,                     # 24..37
  41:53,                     # 41..53
  54:67,                     # 54..67
  69:76,                     # 69..76
  78:81,                     # 78..81
  83, 92,                    # singles
  100:104,                   # 100..104
  106:108,                   # 106..108
  120:131,                   # 120..131 (governorates)
  134, 135,
  138:139,
  142:160
)

# Create outputs newtranslation{n} <- renderText({ ... }) for those numbers
invisible(lapply(numeric_ids, function(i) mk_text_output(paste0("newtranslation", i), i)))

# Aliases/special IDs that point to different rows in VarLabels:
alias_map <- list(
  newtranslation19b  = 93,
  newtranslation20b  = 94,
  newtranslation21b  = 95,
  newtranslation22b  = 96,
  newtranslation35a  = 35,   # same text as 35
  newtranslation59a  = 59,   # same text as 59
  newtranslation60a  = 60,   # same text as 60
  newtranslation101b = 97
)
invisible(lapply(names(alias_map), function(id) mk_text_output(id, alias_map[[id]])))

# --- optional: keep your debug print ------------------------------------------
observe({ cat("Current working directory:", getwd(), "\n") })


}

# Run the application
shinyApp(ui, server)






