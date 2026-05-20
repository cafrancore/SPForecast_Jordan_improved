
library(shiny)
library(shinydashboard)

################## UI Definition
ui <- dashboardPage(
  
  dashboardHeader(
    title = uiOutput("dynamic_header"),
    
    # Add logos on the right side
    tags$li(
      class = "dropdown",  # required for correct rendering
      tags$div(
        style = "display: flex; align-items: center; gap: 10px; padding-top: 10px;",
        
        tags$img(
          src = "https://www.naf.gov.jo/ebv4.0/root_storage/ar/eb_homepage/naf_logo2.png",
          height = "35px"
        ),
        tags$img(
          src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRYcX_iLK1Srwu35rKNwfB3Eu1kwZm81TlAjA&s",
          height = "35px"
        ),
        tags$img(
          src = "https://unttc.org/sites/unttc/files/2020-11/escwa.png",
          height = "35x"
        )))
  ),
  
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
      .sidebar-toggle {
          display: none !important;
        }
        .rtl .main-sidebar { right: 0; left: auto; }
        .rtl .content-wrapper, .rtl .main-footer { margin-right: 230px; margin-left: 0; }
        .rtl .sidebar-toggle { left: auto; right: 0; }  /* Adjust the position of the hamburger icon in RTL */
        
        .ltr .main-sidebar { left: 0; right: auto; }
        .ltr .content-wrapper, .ltr .main-footer { margin-left: 230px; margin-right: 0; }
        .ltr .sidebar-toggle { left: 0; right: auto; }  /* Default positioning for the hamburger icon */
        
        .rtl {
          direction: rtl;
          text-align: right;
        }

        .ltr {
          direction: ltr;
          text-align: left;
        }

        .sidebar-menu li.active a {
          background-color: #34495e !important;
          color: #037ffc !important;
        }
      "))
    ),
    sidebarMenu(
      id = "tabs",
      menuItem(textOutput("newtranslation1"), tabName = "params", icon = icon("sliders")),
      menuItem(textOutput("newtranslation34"), tabName = "gov_analysis", icon = icon("map")),
      menuItem(textOutput("newtranslation55"), tabName = "comparing_scenarios", icon = icon("balance-scale")),
      menuItem(textOutput("newtranslation69"), tabName = "setting_simulation_parameters", icon = icon("screwdriver-wrench"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$script(HTML("
      Shiny.addCustomMessageHandler('updateSearchButton', function(label) {
        $('#searchButton').text(label);
      });
      Shiny.addCustomMessageHandler('setClass', function(lang) {
        $('body').removeClass('rtl ltr').addClass(lang);
      });
        $(document).ready(function() {
          Shiny.addCustomMessageHandler('updateBodyClass', function(tabName) {
            if (tabName === 'toolbox') {
              $('body').addClass('toolbox-bg');
            } else {
              $('body').removeClass('toolbox-bg');
            }
          });
        });
      "))),
    tabItems(
      tabItem(tabName = "params",
              fluidRow(
                box(
                  width = 12,
                  tags$div(
                    style = "display: flex; justify-content: space-between; margin-bottom: 20px;",
                    tags$div(
                      style = "display: flex; align-items: center;",
                      tags$img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTQR8PhauB_dMJoV8qLWwqYc53ifTm_-uw7Fg&s", height = "100px", style = "margin-right: 30px;"),
                      tags$img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcR9PnQs7HlsQRCmbcYnHLx3oMuNcIgRri_1dQ&s", height = "80px")
                    ),
                    tags$div(
                      style = "display: flex; align-items: center;",
                      tags$span(textOutput("newtranslation3"), style = "margin-right: 10px;"),
                      tags$img(src = "https://msme-resurgence.unctad.org/sites/smesurge/files/2021-07/unescwa%20main%20logo.png", height = "70px")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  h1(textOutput("newtranslation2"), 
                     style = "text-align: center; color: #2c3e50; font-weight: bold;")
                )
              ),
              fluidRow(
                radioGroupButtons("translation", h3(""),
                                  choices = c("English" = 1, "العربية" = 2),
                                  selected = 1,
                                  status = "primary",
                                  justified = TRUE
                )),
              
              fluidRow(
                column(width = 6,
                       box(title = textOutput("newtranslation4"), 
                           numericInput("inflationShock", textOutput("newtranslation5"), 
                                        value = 5, min = 0, max = 20),
                           helpText(textOutput("newtranslation6")),
                           
                           #Button
                           uiOutput("employment_input_ui"),
                           uiOutput("employment_numeric_ui")
                           
                       ),
                       
                       box(title = textOutput("newtranslation9"), 
                           tagList(
                             numericInput("targetPoverty", textOutput("newtranslation10"), 
                                          value = 35, min = 5, max = 50),
                             helpText(textOutput("newtranslation11")),
                             
                             numericInput("exchangeRate", textOutput("newtranslation12"), 
                                          value = 1.41, min = 1, max = 2, step = 0.01),
                             helpText(textOutput("newtranslation13"))
                           ))
                ),
                box(title = textOutput("newtranslation15"), width = 6, 
                    background = "teal",
                    tags$div(style = "font-family: 'Arial', sans-serif; font-size: 14px; color: #ffffff;",
                             tags$h3(textOutput("newtranslation16"), style = "font-size: 16px; font-weight: bold;"),
                             tags$p(textOutput("newtranslation17")),
                             tags$h3(textOutput("newtranslation92"), style = "font-size: 15px; font-weight: bold; margin-top: 10px;"),
                             br(),
                             tags$h3(textOutput("newtranslation18"), style = "font-size: 16px; font-weight: bold; margin-top: 10px;"),
                             tags$ul(
                               tags$li(tags$b(textOutput("newtranslation19")),
                                       tags$p(textOutput("newtranslation19b"))),
                               tags$li(tags$b(textOutput("newtranslation20")),
                                       tags$p(textOutput("newtranslation20b"))),
                               tags$li(tags$b(textOutput("newtranslation21")),
                                       tags$p(textOutput("newtranslation21b"))),
                               tags$li(tags$b(textOutput("newtranslation22")),
                                       tags$p(textOutput("newtranslation22b"))),
                               tags$li(tags$b(textOutput("newtranslation101")),
                                       tags$p(textOutput("newtranslation101b"))),
                               tags$li(tags$b(textOutput("newtranslation103")))
                             )
                    )
                )
              ),
              
              # Rest of your UI remains the same...
              fluidRow(
                column(width = 8,
                       actionButton("runSimulation", textOutput("newtranslation24"), 
                                    icon = icon("play"), class = "btn-primary"),
                       actionButton("goToSimParams", textOutput("newtranslation25"), 
                                    icon = icon("cog"), class = "btn-default")
                )
              ),
              
              fluidRow(
                box(
                  title = textOutput("newtranslation26"),
                  width = 6,
                  fluidRow(
                    valueBoxOutput("currentBeneficiaries", width = 6),
                    valueBoxOutput("womenPercentage", width = 6) 
                  ),
                  fluidRow(
                    valueBoxOutput("currentCostJOD", width = 6),
                    valueBoxOutput("currentCostUSD", width = 6)
                  )
                ),
                box(
                  title = textOutput("newtranslation32"),
                  width = 6,
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
                )
              ),
              fluidRow(
                
                actionButton("goToShockparams", textOutput("newtranslation83"), 
                             icon = icon("sliders"), class = "btn-default")
              )
      ),
      tabItem(tabName = "comparing_scenarios",
              fluidRow(
                valueBoxOutput("totalBeneficiaries"),
                valueBoxOutput("costUSD"),
                valueBoxOutput("povertyRate")
              ),
              fluidRow(
                column(
                  width = 3,
                  box(title = textOutput("newtranslation59"), width = 12,status = "primary", solidHeader = TRUE,
                      checkboxGroupInput("selected_scenarios_count",
                                         label = textOutput("newtranslation60"),
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
                  box(title = textOutput("newtranslation59a"), width = 12,status = "primary", solidHeader = TRUE,
                      checkboxGroupInput("selected_scenarios_value",
                                         label = textOutput("newtranslation60a"),
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
      
      
      # Combined Governorate Analysis tab
      tabItem(tabName = "gov_analysis",
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

