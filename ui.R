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
  value = 3.1,
  step = 0.1
)

exp_slider <- sliderInput(
  inputId = "ExpensesGrowth",
  label = "Expenses Growth",
  min = 2,
  max = 5,
  value = 3.1,
  step = 0.1
)

# Lock checkbox
lock_checkbox <- checkboxInput(
  inputId = "lock_sliders",
  label = "🔗 Lock CPI & Expenses Growth together",
  value = TRUE
)

# Group sidebar inputs
sidebar_inputs <- sidebar(
  finance_plan,
  admin_slider,
  cpi_slider,
  exp_slider,
  lock_checkbox,  # Added lock checkbox
  
  # Spacer to push note to bottom
  tags$div(style = "flex-grow: 1;"),
  
  # Bottom note
  tags$div(
    style = "border-top: 1px solid #e0e0e0; padding: 15px 10px 25px 10px; margin-top: 20px; font-size: 11px;",
    tags$p(
      style = "margin: 5px 0; text-align: center;",
      tags$a(
        href = "https://d65-legionofnerds.github.io",
        target = "_blank",
        style = "color: #5839BF; text-decoration: none;",
        "Learn more Legion of Nerds →"
      )
    ),
    tags$p(
      style = "margin: 8px 0 0 0; text-align: center; color: #999; font-size: 10px;",
      "FY27-30 finance projections"
    )
  )
)

# ---- UI ----
ui <- page_sidebar(
  title = tags$div(
    style = "display: flex; align-items: center; gap: 15px;",
    tags$img(
      src = "https://d65-legionofnerds.github.io/assets/favicon.png",
      height = "40px",
      alt = "Logo"
    ),
    "Financial Levers"
  ), 
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
            billboarderOutput("gg_plot", height = "190")
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
          p("Use the sidebar to select the finance plan, adjust Admin bloat, CPI, and Expenses Growth sliders, and explore the results."),
          p(
            "We use data from the ",
            tags$a(
              href = "https://github.com/d65-legionofnerds/financial_levers/tree/main/sources",
              target = "_blank",
              style = "color: #5839BF; text-decoration: none;",
              "District presentations"
            ),
            " to match the financial scenarios."
          ),
          p("For each of the ranges, we use variables backed by data to inform the range:"),
          tags$ul(
            tags$li(
              tags$strong("Admin Bloat:"), 
              " This is the proportion of reduction of our admin staff -- we are approximately 33% above the mean (",
              tags$a(
                href = "https://d65-legionofnerds.github.io/dataanalysis/salary-data.html",
                target = "_blank",
                style = "color: #5839BF;",
                "source"
              ),
              ")."
            ),
            tags$li(
              tags$strong("CPI:"), 
              " The values have ranged between 2 and 8 since Aug 2022 (",
              tags$a(
                href = "https://www.bls.gov/regions/midwest/news-release/consumerpriceindex_chicago.html",
                target = "_blank",
                style = "color: #5839BF;",
                "source"
              ),
              ")."
            ),
            tags$li(
              tags$strong("Expenditures:"), 
              " Growth has had a wide range: ",
              tags$a(
                href = "https://resources.finalsite.net/images/v1698044735/district65net/pvugtsrzlbtmlydyei3g/FY23FinalBudgetBook.pdf",
                target = "_blank",
                style = "color: #5839BF;",
                "22 percent from 2021-2022 to 2022-2023"
              ),
              ", but ",
              tags$a(
                href = "https://resources.finalsite.net/images/v1698044759/district65net/ps4qwpexqsa8x1wtw1fi/FY19Budget.pdf",
                target = "_blank",
                style = "color: #5839BF;",
                "4 percent from 2017-2018 to 2018-2019"
              ),
              "."
            )
          )
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
          p("This table shows the data used in the calculations. You can filter by typing in the values you want."),
          DT::dataTableOutput("finance_table")
        )
      )
    )
  )
)