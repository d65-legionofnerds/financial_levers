library(billboarder)
library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(scales)
library(tidyverse)
library(plotly)

coh_calc <- read_csv("data/coh_calc.csv")
finance_levers_yr <- read_csv("data/finance_levers_yr.csv")
finance_data_levers <- read_csv("data/finance data - levers.csv", 
                                col_types = cols(FY27 = col_number(), 
                                                 FY28 = col_number(), 
                                                 FY29 = col_number(),
                                                 FY30 = col_number()))

server <- function(input, output, session) {
  
  # ---- SLIDER LOCKING ----
  observeEvent(input$CPI, {
    if(input$lock_sliders) {
      updateSliderInput(session, "ExpensesGrowth", value = input$CPI)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$ExpensesGrowth, {
    if(input$lock_sliders) {
      updateSliderInput(session, "CPI", value = input$ExpensesGrowth)
    }
  }, ignoreInit = TRUE)
  
  # ---- GAUGE CHART ----
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$finance_plan)
    
    if (input$finance_plan == "NERDS") {
      finance_data_levers %>%
        filter(Source %in% c("NERDS", "Nov3"))
    } else {
      finance_data_levers %>%
        filter(Source == input$finance_plan)
    }
  })
  
  output$gg_plot <- renderBillboarder({
    
    df <- filtered_data()
    
    # Calculate sum
    fy27_sum <- sum(df$FY27, na.rm = TRUE)
    fy28_sum <- sum(df$FY28, na.rm = TRUE)
    data_sum <- (fy27_sum + fy28_sum ) / 1e6 #+ (input$admin_bloat / 100)*540000 
    
    # Ensure numeric and valid
    data_sum <- round(as.numeric(data_sum), 2)
    if (is.na(data_sum)) data_sum <- 0
    
    billboarder() %>%
      bb_gaugechart(
        value = data_sum,
        min = 0,
        max = 11,
        color = "#5839BF"
      ) %>%
      bb_gauge(
        label = list(
          format = htmlwidgets::JS(
            "function(value, ratio) { return value.toFixed(1) + ' M'; }"
          )
        )
      )
  })
  
  
  # Output table showing filtered data including FY27
  output$finance_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data(),
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 50,  # Default to 50 rows
        lengthMenu = c(10, 50, 100, 200)  # Options in the dropdown
      )
    )
  })
  
  
  # ---- COH ----
  # Filter data by selected source
  filtered_coh <- reactive({
    coh_calc %>% filter(
      Source == input$finance_plan,
      CPI == input$CPI,
      ExpensesGrowth == input$ExpensesGrowth,
      # dplyr::near(AdminBloat, input$admin_bloat / 100)
    )
  })
  
  
  output$coh_plot <- renderPlotly({
    df <- filtered_coh()
    
    # Create scenario labels if not already present
    if(!"scenario" %in% names(df)) {
      df <- df %>%
        mutate(scenario = paste0("CPI:", CPI, "% | ExpGrowth:", ExpensesGrowth)) # , "% | Admin:", AdminBloat, "%"))
    }
    
    # Add color based on COH threshold
    df <- df %>%
      mutate(
        status = if_else(coh >= 90, "Healthy (≥90 days)", "At Risk (<90 days)"),
        bar_color = if_else(coh >= 90, "#5839BF", "#FF6B6B")
      )
    
    # Get one row per year (to avoid duplicate labels)
    df_labels <- df %>%
      group_by(year) %>%
      slice(1) %>%
      ungroup()
    
    plot_ly(
      data = df,
      x = ~year,
      y = ~coh,
      color = ~status,
      colors = c("Healthy (≥90 days)" = "#5839BF", "At Risk (<90 days)" = "#FF6B6B"),
      type = "bar",
      hovertext = ~paste0("COH: ", round(coh, 0), " days<br>",
                          "Status: ", status, "<br>",
                          "Scenario: ", scenario, "<br>",
                          "Fund Balance: $", scales::comma(round(fund_bal, 0), big.mark = ","), "<br>",
                          "Revenue: $", scales::comma(round(revenue, 0), big.mark = ","), "<br>",
                          "Expenditure: $", scales::comma(round(expenditure, 0), big.mark = ",")),
      hovertemplate = paste0("<b>%{x}</b><br>%{hovertext}<extra></extra>"),
      showlegend = FALSE
    ) %>%
      # Add text annotations inside bars
      add_annotations(
        data = df_labels,
        x = ~year,
        y = ~coh / 2,  # Middle of bar
        text = ~paste0("<b>", round(coh, 0), "</b>"),
        showarrow = FALSE,
        font = list(size = 14, color = "white"),
        xanchor = "center",
        yanchor = "middle"
      ) %>%
      # Add reference line at 90 days
      add_trace(
        x = unique(df$year),
        y = 90,
        type = "scatter",
        mode = "lines",
        line = list(color = "black", dash = "dash", width = 2),
        name = "90-day threshold",
        hoverinfo = "skip",
        showlegend = FALSE
      ) %>%
      layout(
        title = paste("Cash on Hand by Year -", input$finance_plan),
        xaxis = list(title = "Fiscal Year"),
        yaxis = list(title = "Days of Cash on Hand"),
        barmode = "group",
        hovermode = "closest",
        showlegend = FALSE
      )
  })
  
  # ---- expenditures ----
  expenses_all <- read_csv("data/expenses_yr.csv")
  
  # Apply CPI and Expenses Growth adjustments to revenue and expenditure
  expenses_filtered <- reactive({
    req(input$finance_plan, input$CPI, input$ExpensesGrowth)
    
    expenses_all %>%
      filter(
        Source == input$finance_plan,
        CPI == input$CPI,
        ExpensesGrowth == input$ExpensesGrowth,
        # dplyr::near(AdminBloat, input$admin_bloat / 100)
      )
  })
  
  finance_filtered <- reactive({
    req(input$finance_plan)
    
    if (input$finance_plan == "NERDS") {
      finance_levers_yr %>%
        filter(Source %in% c("NERDS", "Nov3"))
    } else {
      finance_levers_yr %>%
        filter(Source == input$finance_plan)
    }
  })
  
  output$rev_exp <- renderPlot({
    df <- finance_filtered()
    
    # convert FY27 → 27, FY28 → 28 for numeric x-axis
    df <- df %>%
      mutate(year_num = as.numeric(gsub("FY", "", year)))
    
    # Step 1: Build smoothed values for each lever type
    smoothed <- df %>%
      group_by(`Lever type`) %>%
      arrange(year_num) %>%
      do({
        smooth_obj <- loess(total ~ year_num, data = ., span = 0.75)
        data.frame(
          year_num = .$year_num,
          smooth_y = predict(smooth_obj)
        )
      })
    
    # Step 2: Merge smoothed values back into df
    df_smooth <- df %>%
      left_join(smoothed, by = c("Lever type", "year_num"))
    
    ggplot() +
      
      # ---- SHADED AREA UNDER EACH CURVE ----
    geom_ribbon(
      data = df_smooth,
      aes(x = year_num, ymin = 0, ymax = smooth_y, fill = `Lever type`),
      alpha = 0.25
    ) +
      
      # ---- SMOOTHED LINE ----
    geom_line(
      data = df_smooth,
      aes(x = year_num, y = smooth_y, color = `Lever type`),
      size = 1.4
    ) +
      
      # ---- BASELINE DASHED LINE ----
    geom_line(
      data = df_smooth %>% filter(Source == "baseline"),
      aes(x = year_num, y = smooth_y),
      linetype = "dashed",
      color = "black",
      linewidth = 1
    ) +
      
      # ---- MANUAL COLORS ----
    scale_color_manual(
      values = c("revenue" = "#5839BF", "expenditure" = "red")
    ) +
      scale_fill_manual(
        values = c("revenue" = "#5839BF", "expenditure" = "red")
      ) +
      
      # ---- THEMES AND LABELS ----
    scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
      theme_minimal(base_size = 16) +
      labs(
        title = "Revenue & Expenditure by Fiscal Year",
        x = "Fiscal Year",
        y = "Amount"
      )
  })
  
  output$rev_exp_adjusted <- renderPlotly({
    df <- expenses_filtered() %>% 
      # Aggregate to single value per year per lever type
      group_by(`Lever type`, year) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(year_num = as.numeric(gsub("FY", "", year))) %>%
      arrange(`Lever type`, year_num)
    
    # Pivot wider to get revenue and expenditure as separate columns
    df_wide <- df %>%
      pivot_wider(names_from = `Lever type`, values_from = value) %>%
      mutate(
        # Determine which is higher
        fill_color = ifelse(revenue > expenditure, "surplus", "deficit")
      )
    
    # Plot with ribbon between the two lines
    p <- ggplot(df_wide, aes(x = year_num)) +
      geom_ribbon(aes(ymin = pmin(revenue, expenditure), 
                      ymax = pmax(revenue, expenditure),
                      fill = fill_color), 
                  alpha = 0.3) +
      geom_line(aes(y = revenue, color = "revenue"), size = 1.2) +
      geom_line(aes(y = expenditure, color = "expenditure"), size = 1.2) +
      scale_fill_manual(name = "Status",
                        values = c("surplus" = "#5839BF", "deficit" = "red")) +
      scale_color_manual(name = "Lever type",
                         values = c("revenue" = "#5839BF", "expenditure" = "red")) +
      scale_y_continuous(
        n.breaks = 6, 
        limits = c(165000000, NA),
        labels = scales::label_dollar(scale = 1e-6, suffix = "M")
      ) +
      theme_minimal(base_size = 16)  +
      labs(title = "Adjusted Revenue & Expenditure", x = "Fiscal Year", y = "Amount")
    
    ggplotly(p) %>%
      layout(
        legend = list(
          orientation = "h",       # horizontal
          x = 0.5,                 # center horizontally
          xanchor = "center",
          y = -0.15,               # just below the plot (mobile-friendly)
          yanchor = "top",
          font = list(size = 12),  # readable but small for mobile
          itemwidth = 30           # keeps items compact
        ),
        margin = list(b = 80)      # add bottom margin so legend doesn't clip
      )
    
  })
  
}