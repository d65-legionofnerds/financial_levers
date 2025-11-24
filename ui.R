library(plotly)
library(billboarder)
library(shinydashboard)
library(shiny)
library(bslib)
library(readr)

###############
# Load data
finance_data_levers <- read_csv(
  "data/finance data - levers.csv", 
  col_types = cols(FY27 = col_number())
)
coh_calc <- read_csv("data/coh_calc.csv")

# ---- Sidebar Inputs ----
finance_plan <- selectInput(
  inputId = "finance_plan",
  label = "Finance plan",
  choices = unique(finance_data_levers$Source),
  selected = "NERDS"
)

source_select <- selectInput(
  inputId = "source",
  label = "COH Source",
  choices = unique(coh_calc$Source),
  selected = unique(coh_calc$Source)[1]
)

admin_slider <- sliderInput(
  inputId = "admin_bloat",
  label = "Admin Bloat Reduction",
  min = 0,
  max = 50,
  value = 30,
  step = 10,
  post = "%"
)

cpi_slider <- sliderInput(
  inputId = "CPI",
  label = "CPI",
  min = 2,
  max = 5,
  value = 2.5,
  step = 0.1
)

exp_slider <- sliderInput(
  inputId = "ExpensesGrowth",
  label = "Expenses Growth",
  min = 2,
  max = 5.25,
  value = 2.5,
  step = 0.25
)

# Group sidebar inputs with headings
sidebar_inputs <- tagList(
  finance_plan,
  source_select,
  admin_slider,
  cpi_slider,
  exp_slider
)

# ---- UI ----
ui <- page_sidebar(
  title = "Financial Levers",
  sidebar = sidebar_inputs,
  
  navset_tab(
    id = "main_tabs",
    
    # ---- Dashboard Tab ----
    nav_panel(
      title = "Dashboard",
      
      div(
        style = "max-width: 80%; margin: 0 auto;",
        
        # Top two cards in columns
        layout_columns(
          widths = c(4, 8),
          
          card(
            full_screen = FALSE,
            card_header("FY27 Levers Value"),
            style = "height: 250px; margin-bottom: 15px;",
            billboarderOutput("gg_plot", height = "230px")
          ),
          
          card(
            full_screen = FALSE,
            card_header("Days cash on hand"),
            style = "height: 250px; margin-bottom: 15px;",
            plotlyOutput("coh_plot")
          )
        ),
        
        # Full-width card below
        card(
          full_screen = TRUE,
          card_header("Expenditures vs Revenue"),
          style = "height: 400px; margin-bottom: 15px;",
          plotlyOutput("rev_exp_adjusted", height = "180px")
        )
      )
    ),
    
    # ---- About Tab ----
    nav_panel(
      title = "About",
      div(
        style = "max-width: 80%; margin: 0 auto;",
        card(
          full_screen = FALSE,
          card_header("About this App"),
          style = "margin-bottom: 15px;",
          p("This Shiny app shows financial levers, revenue and expenditure projections, and other interactive analyses."),
          p("Use the sidebar to select the finance plan, adjust CPI and Expenses Growth sliders, and explore the results.")
        )
      )
    ),
    
    ## Raw data
    nav_panel(
      title = "Source Data",
      div(
        style = "max-width: 80%; margin: 0 auto;",
        card(
          full_screen = FALSE,
          card_header("Data from the analysis"),
          style = "margin-bottom: 15px;",
          p("This table shows the data used in the calculations. you can filter by typing in the models you want. "),
          DT::dataTableOutput("finance_table")
        )
      )
    )
  )
)