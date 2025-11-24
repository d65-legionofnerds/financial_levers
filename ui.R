library(billboarder)
library(shinydashboard)
library(gghighlight)
library(shiny)
library(bslib)
library(readr)

###############
# Load data
finance_data_levers <- read_csv(
  "data/finance data - levers.csv", 
  col_types = cols(FY27 = col_number())
)

# ---- Sidebar Inputs ----
finance_plan <- selectInput(
  inputId = "finance_plan",
  label = "Finance plan",
  choices = unique(finance_data_levers$Source),
  selected = "NERDS"
)

other_input <- selectInput(
  inputId = "admin_bloat",
  label = "Admin Bloat Reduction",
  choices = c("No Change", "Adjust to median", "Adjust below median"),
  selected = "Adjust to median"
)

slider1 <- sliderInput(
  inputId = "CPI",
  label = "CPI",
  min = 2,
  max = 3,
  value = 2.5,
  step = 0.1
)

slider2 <- sliderInput(
  inputId = "ExpensesGrowth",
  label = "Expenses Growth",
  min = 2,
  max = 5.25,
  value = 2.5,
  step = 0.25
)

# Group sidebar inputs with headings
sidebar_inputs <- tagList(
  h4("Finance Plan"),
  finance_plan,
  
  h4("Admin Settings"),
  other_input,
  
  h4("Economic Assumptions"),
  slider1,
  slider2
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
        style = "max-width: 80%; margin: 0 auto;",  # centers content, 80% width
        
        # Top two cards in columns
        layout_columns(
          widths = c(4, 8),
          
          card(
            full_screen = FALSE,
            card_header("FY27 Levers Value"),
            style = "height: 150px; margin-bottom: 15px;",
            billboarderOutput("gg_plot", height = "120px")
          ),
          
          card(
            full_screen = FALSE,
            card_header("Days cash on hand"),
            style = "height: 150px; margin-bottom: 15px;",
            plotOutput("bill_depth", height = "120px")
          )
        ),
        
        # Full-width card below
        card(
          full_screen = TRUE,
          card_header("Expenditures vs Revenue"),
          style = "height: 400px; margin-bottom: 15px;",
          plotOutput("rev_exp_adjusted", height = "180px")
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
    )
  )
)
