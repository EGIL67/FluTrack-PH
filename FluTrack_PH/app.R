# ============================================================
# FluTrack PH - Final Phase 5 Healthcare Dashboard
# ============================================================

# Auto-set working directory if your project folder exists
if (dir.exists("X:/PROJECTS/RStudio/FluTrack_PH")) {
  setwd("X:/PROJECTS/RStudio/FluTrack_PH")
}

# Load libraries
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)

# ------------------------------------------------------------
# File checks
# ------------------------------------------------------------

required_files <- c(
  "data/processed/model_ready_flu.csv",
  "outputs/model_comparison.csv",
  "outputs/xgboost_4week_forecast.csv",
  "outputs/xgboost_feature_importance.csv",
  "outputs/predicted_vs_actual_2024.csv"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(paste(
    "Missing required file(s):",
    paste(missing_files, collapse = ", ")
  ))
}

# ------------------------------------------------------------
# Load Phase 4 outputs
# ------------------------------------------------------------

model_data      <- read_csv("data/processed/model_ready_flu.csv",          show_col_types = FALSE)
model_comparison <- read_csv("outputs/model_comparison.csv",                show_col_types = FALSE)
forecast_data   <- read_csv("outputs/xgboost_4week_forecast.csv",           show_col_types = FALSE)
feature_data    <- read_csv("outputs/xgboost_feature_importance.csv",        show_col_types = FALSE)
predicted_data  <- read_csv("outputs/predicted_vs_actual_2024.csv",         show_col_types = FALSE)

# ------------------------------------------------------------
# Prepare data
# ------------------------------------------------------------

model_data <- model_data %>%
  mutate(
    iso_weekstartdate = as.Date(iso_weekstartdate),
    iso_year          = as.integer(iso_year)
  )

forecast_data  <- forecast_data  %>% mutate(date = as.Date(date))
predicted_data <- predicted_data %>% mutate(date = as.Date(date))

# Summary values for metric cards
dashboard_total_cases  <- sum(model_data$total_cases, na.rm = TRUE)
dashboard_total_weeks  <- nrow(model_data)
dashboard_latest_year  <- max(model_data$iso_year,          na.rm = TRUE)
dashboard_latest_date  <- max(model_data$iso_weekstartdate, na.rm = TRUE)
dashboard_current_risk <- forecast_data$risk_level[1]

best_model <- model_comparison %>% arrange(RMSE) %>% slice(1) %>% pull(Model)
best_rmse  <- model_comparison %>% arrange(RMSE) %>% slice(1) %>% pull(RMSE)
best_mae   <- model_comparison %>% arrange(RMSE) %>% slice(1) %>% pull(MAE)
best_mape  <- model_comparison %>% arrange(RMSE) %>% slice(1) %>% pull(MAPE)

# ------------------------------------------------------------
# Small reusable UI helpers
# ------------------------------------------------------------

help_icon <- function(text) {
  tags$span(
    class        = "help-tip",
    `data-tooltip` = text,
    icon("question-circle")
  )
}

box_title <- function(title, help_text) {
  tagList(span(title), help_icon(help_text))
}

metric_card <- function(label, value, note, accent = "blue") {
  div(
    class = paste("metric-card", accent),
    div(class = "metric-label", label),
    div(class = "metric-value", value),
    div(class = "metric-note",  note)
  )
}

# ------------------------------------------------------------
# User Interface
# ------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title      = span(icon("heartbeat"), " FluTrack PH"),
    titleWidth = 280
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Executive Summary",   tabName = "overview",   icon = icon("home")),
      menuItem("Surveillance Trends", tabName = "trends",     icon = icon("line-chart")),
      menuItem("Model Evaluation",    tabName = "evaluation", icon = icon("bar-chart")),
      menuItem("Predictive Drivers",  tabName = "features",   icon = icon("stethoscope")),
      menuItem("4-Week Outlook",      tabName = "forecast",   icon = icon("calendar")),
      menuItem("Data Explorer",       tabName = "tables",     icon = icon("table")),
      menuItem("User Guide",          tabName = "guide",      icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("

      /* ── Base ──────────────────────────────────────────── */
      body, .content-wrapper, .right-side {
        font-family: 'Segoe UI', Arial, sans-serif;
        background: #eef5f8;
      }

      /* ── Header & sidebar ──────────────────────────────── */
      .main-header .logo,
      .main-header .navbar {
        background: linear-gradient(135deg, #005f73, #0a9396) !important;
        font-weight: 800;
        letter-spacing: 0.4px;
      }

      .skin-blue .main-sidebar {
        background: #073b4c;
      }

      .skin-blue .sidebar-menu > li > a {
        color: #dff7ff;
        font-weight: 600;
        padding: 14px 16px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li:hover > a {
        background: #118ab2;
        border-left-color: #06d6a0;
      }

      /* ── Page content ──────────────────────────────────── */
      .content {
        padding: 20px 22px;
      }

      /* ── Hero card ─────────────────────────────────────── */
      .hero-card {
        background: linear-gradient(135deg, #005f73, #118ab2);
        color: white;
        border-radius: 16px;
        padding: 26px 30px;
        margin-bottom: 20px;
        box-shadow: 0 8px 24px rgba(0, 80, 120, 0.18);
      }

      .hero-title {
        font-size: 26px;
        font-weight: 800;
        margin-bottom: 8px;
        line-height: 1.3;
      }

      .hero-subtitle {
        font-size: 14px;
        line-height: 1.7;
        opacity: 0.95;
      }

      /* ── Metric cards ──────────────────────────────────── */
      /*
         Fixed height ensures all four cards align uniformly.
         display:flex + flex-direction:column lets the note
         sit flush at the bottom without manual margin tricks.
      */
      .metric-card {
        background: white;
        border-radius: 14px;
        padding: 20px 20px 18px;
        height: 148px;                 /* uniform height */
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        box-shadow: 0 6px 18px rgba(7, 59, 76, 0.09);
        border-left: 6px solid #118ab2;
        margin-bottom: 20px;
        overflow: hidden;
        box-sizing: border-box;
      }

      .metric-card.green  { border-left-color: #06d6a0; }
      .metric-card.yellow { border-left-color: #ffd166; }
      .metric-card.red    { border-left-color: #ef476f; }
      .metric-card.purple { border-left-color: #6a4c93; }

      .metric-label {
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.9px;
        color: #607d8b;
        font-weight: 800;
      }

      .metric-value {
        font-size: 26px;
        font-weight: 800;
        color: #073b4c;
        line-height: 1.15;
        word-break: break-word;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }

      .metric-note {
        font-size: 11.5px;
        color: #78909c;
        line-height: 1.45;
        overflow: hidden;
        text-overflow: ellipsis;
        display: -webkit-box;
        -webkit-line-clamp: 2;
        -webkit-box-orient: vertical;
      }

      /* ── Boxes ─────────────────────────────────────────── */
      .box {
        border-radius: 14px;
        border-top: 0;
        box-shadow: 0 6px 18px rgba(7, 59, 76, 0.09);
        margin-bottom: 20px;
        overflow: visible;
      }

      .box.box-primary > .box-header {
        background: #ffffff;
        color: #073b4c;
        border-bottom: 1px solid #e3eef3;
        padding: 13px 18px;
        border-radius: 14px 14px 0 0;
      }

      .box-title {
        font-weight: 800 !important;
        font-size: 15px !important;
        display: inline-flex;
        align-items: center;
        gap: 4px;
      }

      .box-body {
        padding: 16px 18px;
      }

      /* ── Section note ──────────────────────────────────── */
      .section-note {
        background: #f4fafb;
        border-left: 4px solid #06d6a0;
        border-radius: 0 10px 10px 0;
        padding: 12px 16px;
        margin-bottom: 14px;
        color: #29434e;
        font-size: 13.5px;
        line-height: 1.65;
        box-shadow: none;
      }

      /* ── Risk badge ────────────────────────────────────── */
      .risk-badge {
        display: inline-block;
        padding: 5px 14px;
        border-radius: 999px;
        background: #d8f3dc;
        color: #1b5e20;
        font-weight: 800;
        font-size: 13px;
        vertical-align: middle;
        margin-left: 6px;
      }

      /* ── Help tooltip ──────────────────────────────────── */
      /*
         Tooltip is scoped inside .box-header so it never
         overflows the viewport horizontally. The :hover::after
         pseudo-element now opens BELOW the icon instead of to
         the right, preventing clipping at narrow widths.
      */
      .help-tip {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        margin-left: 6px;
        width: 18px;
        height: 18px;
        border-radius: 50%;
        background: #e6f6f8;
        color: #006d77;
        font-size: 12px;
        cursor: help;
        position: relative;
        vertical-align: middle;
        flex-shrink: 0;
      }

      .help-tip:hover::after {
        content: attr(data-tooltip);
        position: absolute;
        top: calc(100% + 8px);   /* opens below the icon */
        left: 50%;
        transform: translateX(-50%);
        width: 280px;
        background: #073b4c;
        color: white;
        padding: 10px 13px;
        border-radius: 8px;
        font-size: 12px;
        line-height: 1.5;
        font-weight: 500;
        z-index: 9999;
        white-space: normal;
        box-shadow: 0 6px 18px rgba(0, 0, 0, 0.20);
        pointer-events: none;
      }

      /* small arrow pointing up toward the icon */
      .help-tip:hover::before {
        content: '';
        position: absolute;
        top: calc(100% + 2px);
        left: 50%;
        transform: translateX(-50%);
        border: 5px solid transparent;
        border-bottom-color: #073b4c;
        z-index: 9999;
        pointer-events: none;
      }

      /* ── Guide cards ───────────────────────────────────── */
      .guide-card {
        background: white;
        border-radius: 14px;
        padding: 18px 22px;
        margin-bottom: 16px;
        box-shadow: 0 6px 18px rgba(7, 59, 76, 0.07);
        border-left: 5px solid #118ab2;
      }

      .guide-title {
        font-weight: 800;
        color: #073b4c;
        font-size: 16px;
        margin-bottom: 6px;
      }

      .guide-text {
        color: #455a64;
        font-size: 13.5px;
        line-height: 1.65;
      }

      /* ── DataTables ────────────────────────────────────── */
      .dataTables_wrapper {
        font-size: 13px;
      }

      /* ── Responsive: 1080p laptop ──────────────────────── */
      @media (max-width: 1280px) {
        .hero-title        { font-size: 22px; }
        .hero-subtitle     { font-size: 13.5px; }
        .metric-value      { font-size: 22px; }
        .metric-card       { height: 140px; }

        /* Tooltip opens upward when near the right edge */
        .box-header .help-tip:hover::after {
          left: auto;
          right: 0;
          transform: none;
        }
        .box-header .help-tip:hover::before {
          left: auto;
          right: 6px;
          transform: none;
        }
      }

      @media (max-width: 992px) {
        .hero-title    { font-size: 19px; }
        .metric-value  { font-size: 19px; }
        .metric-card   { height: auto; min-height: 130px; }
        .content       { padding: 14px 16px; }
      }

    "))),
    
    tabItems(
      
      # ====================================================
      # EXECUTIVE SUMMARY
      # ====================================================
      
      tabItem(
        tabName = "overview",
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "hero-card",
              div(class = "hero-title", "FluTrack PH: Influenza Surveillance and Forecasting Dashboard"),
              div(
                class = "hero-subtitle",
                "A healthcare analytics dashboard for monitoring influenza trends, evaluating statistical forecasting models, identifying key predictive drivers, and presenting short-term influenza risk outlooks for public health decision-making in the Philippines."
              )
            )
          )
        ),
        
        fluidRow(
          column(width = 3, metric_card(
            "Total Cases",
            format(round(dashboard_total_cases), big.mark = ","),
            "Total confirmed influenza counts in the processed surveillance dataset.",
            "blue"
          )),
          column(width = 3, metric_card(
            "Surveillance Weeks",
            format(dashboard_total_weeks, big.mark = ","),
            "Total weekly observations used in the model-ready dataset.",
            "green"
          )),
          column(width = 3, metric_card(
            "Forecast Method",
            best_model,
            paste0("Selected by lowest RMSE: ", round(best_rmse, 2)),
            "yellow"
          )),
          column(width = 3, metric_card(
            "Current Risk",
            dashboard_current_risk,
            paste("Latest surveillance date:", dashboard_latest_date),
            "red"
          ))
        ),
        
        fluidRow(
          box(
            title = box_title(
              "Public Health Interpretation",
              "This section summarizes what the dashboard is for and why XGBoost was selected as the main forecasting method."
            ),
            width = 12, status = "primary", solidHeader = TRUE,
            div(
              class = "section-note",
              "FluTrack PH integrates cleaned WHO FluNet surveillance data and Phase 4 model outputs. It supports public health monitoring by displaying historical influenza activity, model validation results, predictive drivers, and four-week statistical forecast outputs."
            ),
            DTOutput("overview_metrics")
          )
        )
      ),
      
      # ====================================================
      # SURVEILLANCE TRENDS
      # ====================================================
      
      tabItem(
        tabName = "trends",
        
        fluidRow(
          box(
            title = box_title(
              "Filter Surveillance Period",
              "Use the slider to choose the year range shown in the trend chart below."
            ),
            width = 12, status = "primary", solidHeader = TRUE,
            sliderInput(
              inputId = "year_range",
              label   = "Select year range:",
              min     = min(model_data$iso_year, na.rm = TRUE),
              max     = max(model_data$iso_year, na.rm = TRUE),
              value   = c(2010, dashboard_latest_year),
              sep     = "",
              width   = "100%"
            )
          )
        ),
        
        fluidRow(uiOutput("trend_summary")),
        
        fluidRow(
          box(
            title = box_title(
              "Weekly Influenza Case Trend",
              "Hover over the line to inspect weekly case counts. Use the range slider under the chart to zoom into specific time periods."
            ),
            width = 12, status = "primary", solidHeader = TRUE,
            div(
              class = "section-note",
              "This visualization shows long-term influenza activity by surveillance week. It helps identify seasonal surges, unusual spikes, and low-activity periods."
            ),
            plotlyOutput("trend_plot", height = "520px")
          )
        )
      ),
      
      # ====================================================
      # MODEL EVALUATION
      # ====================================================
      
      tabItem(
        tabName = "evaluation",
        
        fluidRow(
          box(
            title = box_title(
              "Model Comparison",
              "Choose RMSE, MAE, or MAPE to compare forecasting performance. Lower values indicate better performance."
            ),
            width = 5, status = "primary", solidHeader = TRUE,
            selectInput(
              inputId  = "metric_choice",
              label    = "Select evaluation metric:",
              choices  = c("RMSE", "MAE", "MAPE"),
              selected = "RMSE"
            ),
            plotlyOutput("comparison_plot", height = "360px")
          ),
          
          box(
            title = box_title(
              "Model Evaluation Metrics",
              "This table contains the numerical model performance values generated during Phase 4."
            ),
            width = 7, status = "primary", solidHeader = TRUE,
            div(
              class = "section-note",
              "The 2024 hold-out test results show that XGBoost outperformed ARIMA across all evaluation metrics."
            ),
            DTOutput("comparison_table"),
            br(),
            downloadButton("download_model_metrics", "Download Model Metrics")
          )
        ),
        
        fluidRow(
          box(
            title = box_title(
              "Actual vs Predicted Weekly Cases, 2024",
              "This chart compares actual 2024 surveillance observations against ARIMA and XGBoost predictions."
            ),
            width = 12, status = "primary", solidHeader = TRUE,
            plotlyOutput("prediction_plot", height = "500px")
          )
        )
      ),
      
      # ====================================================
      # FEATURE IMPORTANCE
      # ====================================================
      
      tabItem(
        tabName = "features",
        
        fluidRow(
          box(
            title = box_title(
              "XGBoost Predictive Drivers",
              "Feature importance explains which variables contributed most to XGBoost predictions. Higher gain means stronger predictive contribution."
            ),
            width = 12, status = "primary", solidHeader = TRUE,
            div(
              class = "section-note",
              "The strongest predictive features indicate which recent surveillance signals are most useful in forecasting influenza activity."
            ),
            plotlyOutput("importance_plot", height = "520px")
          )
        ),
        
        fluidRow(
          box(
            title = box_title(
              "Feature Importance Table",
              "Gain = contribution to model performance. Cover = how often observations are affected. Frequency = how often the feature is used in splits."
            ),
            width = 12, status = "primary", solidHeader = TRUE,
            DTOutput("importance_table"),
            br(),
            downloadButton("download_feature_importance", "Download Feature Importance")
          )
        )
      ),
      
      # ====================================================
      # FOUR-WEEK OUTLOOK
      # ====================================================
      
      tabItem(
        tabName = "forecast",
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "hero-card",
              div(class = "hero-title", "Four-Week Influenza Outlook"),
              div(
                class = "hero-subtitle",
                HTML(paste0(
                  "Latest model-generated forecast risk: ",
                  "<span class='risk-badge'>", dashboard_current_risk, "</span>",
                  "<br>The forecast table and chart show predicted influenza activity for the next four surveillance weeks."
                ))
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = box_title(
              "Forecast Table",
              "Shows the next four forecasted surveillance weeks, predicted case counts, and assigned risk level."
            ),
            width = 6, status = "primary", solidHeader = TRUE,
            DTOutput("forecast_table"),
            br(),
            downloadButton("download_forecast", "Download Forecast")
          ),
          
          box(
            title = box_title(
              "Predicted Cases by Forecast Week",
              "Hover over each point to see the forecast date and predicted cases."
            ),
            width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("forecast_plot", height = "370px")
          )
        )
      ),
      
      # ====================================================
      # DATA EXPLORER
      # ====================================================
      
      tabItem(
        tabName = "tables",
        
        fluidRow(
          box(
            title = box_title(
              "Model-Ready Surveillance Dataset",
              "The cleaned and feature-engineered dataset used for Phase 4 modeling and Phase 5 dashboard visualization."
            ),
            width = 12, status = "primary", solidHeader = TRUE,
            div(
              class = "section-note",
              "Use the search bar to inspect specific years, weeks, case values, or engineered predictors."
            ),
            DTOutput("data_table"),
            br(),
            downloadButton("download_model_data", "Download Processed Dataset")
          )
        )
      ),
      
      # ====================================================
      # USER GUIDE
      # ====================================================
      
      tabItem(
        tabName = "guide",
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "hero-card",
              div(class = "hero-title", "How to Use FluTrack PH"),
              div(
                class = "hero-subtitle",
                "This guide explains the purpose of each dashboard section and how to interact with the visualizations."
              )
            )
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            div(class = "guide-card",
                div(class = "guide-title", "1. Executive Summary"),
                div(class = "guide-text", "See the main project indicators: total cases, total surveillance weeks, best model, and current forecast risk.")
            ),
            div(class = "guide-card",
                div(class = "guide-title", "2. Surveillance Trends"),
                div(class = "guide-text", "Use the year slider to explore weekly influenza activity over time. Hover over the line chart to inspect specific weekly values.")
            ),
            div(class = "guide-card",
                div(class = "guide-title", "3. Model Evaluation"),
                div(class = "guide-text", "Compare ARIMA and XGBoost using RMSE, MAE, and MAPE. Lower values indicate better forecasting performance.")
            )
          ),
          column(
            width = 6,
            div(class = "guide-card",
                div(class = "guide-title", "4. Predictive Drivers"),
                div(class = "guide-text", "See which variables were most important in the XGBoost forecasting model based on gain, cover, and frequency.")
            ),
            div(class = "guide-card",
                div(class = "guide-title", "5. 4-Week Outlook"),
                div(class = "guide-text", "View the latest four-week ahead forecast and its assigned risk classification.")
            ),
            div(class = "guide-card",
                div(class = "guide-title", "6. Data Explorer"),
                div(class = "guide-text", "Inspect the processed dataset and verify the fields used for analysis and modeling.")
            )
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  filtered_trend_data <- reactive({
    model_data %>%
      filter(
        iso_year >= input$year_range[1],
        iso_year <= input$year_range[2]
      )
  })
  
  output$trend_summary <- renderUI({
    data <- filtered_trend_data()
    if (nrow(data) == 0) return(NULL)
    
    total_filtered <- sum(data$total_cases, na.rm = TRUE)
    peak_cases     <- max(data$total_cases, na.rm = TRUE)
    peak_date      <- data$iso_weekstartdate[which.max(data$total_cases)]
    
    fluidRow(
      column(width = 4, metric_card(
        "Filtered Total Cases",
        format(round(total_filtered), big.mark = ","),
        "Total cases within the selected year range.",
        "blue"
      )),
      column(width = 4, metric_card(
        "Peak Weekly Cases",
        format(round(peak_cases), big.mark = ","),
        paste("Peak date:", peak_date),
        "red"
      )),
      column(width = 4, metric_card(
        "Selected Years",
        paste(input$year_range[1], "\u2013", input$year_range[2]),
        "Current year range displayed in the trend chart.",
        "green"
      ))
    )
  })
  
  output$overview_metrics <- renderDT({
    datatable(
      model_comparison,
      options  = list(pageLength = 5, dom = "t"),
      rownames = FALSE
    ) %>% formatRound(columns = c("RMSE", "MAE", "MAPE"), digits = 2)
  })
  
  output$trend_plot <- renderPlotly({
    data <- filtered_trend_data()
    
    plot_ly(
      data,
      x    = ~iso_weekstartdate,
      y    = ~total_cases,
      type = "scatter",
      mode = "lines",
      fill = "tozeroy",
      fillcolor = "rgba(17,138,178,0.18)",
      line      = list(color = "#006d77", width = 2),
      text      = ~paste0(
        "Date: ",  iso_weekstartdate,
        "<br>Year: ", iso_year,
        "<br>Week: ", iso_week,
        "<br>Cases: ", total_cases
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = "<b>Weekly Influenza Cases in the Philippines</b>",
          font = list(color = "#073b4c", size = 15)
        ),
        xaxis = list(
          title       = "Surveillance Date",
          rangeslider = list(visible = TRUE),
          showgrid    = TRUE,
          gridcolor   = "#e8f1f4"
        ),
        yaxis     = list(title = "Total Cases", showgrid = TRUE, gridcolor = "#e8f1f4"),
        hovermode = "x unified",
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })
  
  output$comparison_plot <- renderPlotly({
    selected_metric <- input$metric_choice
    
    p <- ggplot(
      model_comparison,
      aes(
        x    = Model,
        y    = .data[[selected_metric]],
        text = paste(
          "Model:", Model,
          "<br>", selected_metric, ":", round(.data[[selected_metric]], 2)
        )
      )
    ) +
      geom_col(fill = "#118ab2", width = 0.55) +
      labs(
        title = paste("Model Comparison \u2013", selected_metric),
        x     = "Model",
        y     = selected_metric
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", color = "#073b4c"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$comparison_table <- renderDT({
    datatable(
      model_comparison,
      options  = list(pageLength = 5, autoWidth = TRUE),
      rownames = FALSE
    ) %>% formatRound(columns = c("RMSE", "MAE", "MAPE"), digits = 2)
  })
  
  output$prediction_plot <- renderPlotly({
    series_colors <- c(
      "actual_cases" = "#073b4c",
      "ARIMA"        = "#ffd166",
      "XGBoost"      = "#118ab2"
    )
    series_labels <- c(
      "actual_cases" = "Actual Cases",
      "ARIMA"        = "ARIMA",
      "XGBoost"      = "XGBoost"
    )
    
    plot_ly() %>%
      add_trace(
        data      = predicted_data,
        x         = ~date,
        y         = ~actual_cases,
        type      = "scatter",
        mode      = "lines",
        name      = "Actual Cases",
        line      = list(color = "#073b4c", width = 2),
        text      = ~paste0("Date: ", date, "<br>Series: Actual Cases<br>Cases: ", round(actual_cases, 2)),
        hoverinfo = "text"
      ) %>%
      add_trace(
        data      = predicted_data,
        x         = ~date,
        y         = ~ARIMA,
        type      = "scatter",
        mode      = "lines",
        name      = "ARIMA",
        line      = list(color = "#ffd166", width = 2, dash = "dash"),
        text      = ~paste0("Date: ", date, "<br>Series: ARIMA<br>Cases: ", round(ARIMA, 2)),
        hoverinfo = "text"
      ) %>%
      add_trace(
        data      = predicted_data,
        x         = ~date,
        y         = ~XGBoost,
        type      = "scatter",
        mode      = "lines",
        name      = "XGBoost",
        line      = list(color = "#118ab2", width = 2, dash = "dot"),
        text      = ~paste0("Date: ", date, "<br>Series: XGBoost<br>Cases: ", round(XGBoost, 2)),
        hoverinfo = "text"
      ) %>%
      layout(
        title = list(
          text = "<b>Actual vs Predicted Weekly Influenza Cases \u2013 2024 Testing Period</b>",
          font = list(color = "#073b4c", size = 15)
        ),
        xaxis     = list(title = "Date",         showgrid = TRUE, gridcolor = "#e8f1f4"),
        yaxis     = list(title = "Weekly Cases",  showgrid = TRUE, gridcolor = "#e8f1f4"),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.15),
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  })
  
  output$importance_plot <- renderPlotly({
    p <- feature_data %>%
      arrange(Gain) %>%
      ggplot(aes(
        x    = reorder(Feature, Gain),
        y    = Gain,
        text = paste(
          "Feature:",   Feature,
          "<br>Gain:",  round(Gain,      4),
          "<br>Cover:", round(Cover,     4),
          "<br>Freq:",  round(Frequency, 4)
        )
      )) +
      geom_col(fill = "#06d6a0", width = 0.70) +
      coord_flip() +
      labs(
        title = "XGBoost Feature Importance Based on Gain",
        x     = "Feature",
        y     = "Gain"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold", color = "#073b4c"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$importance_table <- renderDT({
    datatable(
      feature_data,
      options  = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    ) %>% formatRound(columns = c("Gain", "Cover", "Frequency"), digits = 4)
  })
  
  output$forecast_table <- renderDT({
    forecast_display <- forecast_data %>%
      select(forecast_week, iso_year, iso_week, date, predicted_cases, risk_level)
    
    datatable(
      forecast_display,
      options  = list(pageLength = 4, dom = "t", autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  output$forecast_plot <- renderPlotly({
    all_zero <- all(forecast_data$predicted_cases == 0, na.rm = TRUE)
    
    if (all_zero) {
      # Show a clean placeholder instead of a meaningless flat line at 0
      plot_ly() %>%
        layout(
          title = list(
            text = "<b>Four-Week Ahead Influenza Forecast</b>",
            font = list(color = "#073b4c", size = 15)
          ),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(list(
            text      = "Forecast values are all zero.\nPlease re-run the Phase 4 XGBoost forecasting script\nto generate valid predicted_cases.",
            x         = 0.5, y = 0.5,
            xref      = "paper", yref = "paper",
            showarrow = FALSE,
            font      = list(size = 14, color = "#607d8b")
          )),
          plot_bgcolor  = "white",
          paper_bgcolor = "white"
        )
    } else {
      upper_limit <- max(forecast_data$predicted_cases, na.rm = TRUE) * 1.25
      
      plot_ly(
        forecast_data,
        x         = ~date,
        y         = ~predicted_cases,
        type      = "scatter",
        mode      = "lines+markers",
        line      = list(color = "#ef476f", width = 2),
        marker    = list(color = "#ef476f", size = 9),
        text      = ~paste0(
          "Forecast week: ", forecast_week,
          "<br>Date: ",      date,
          "<br>Predicted: ", predicted_cases,
          "<br>Risk: ",      risk_level
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          title = list(
            text = "<b>Four-Week Ahead Influenza Forecast</b>",
            font = list(color = "#073b4c", size = 15)
          ),
          xaxis     = list(title = "Forecast Date", showgrid = TRUE, gridcolor = "#e8f1f4"),
          yaxis     = list(title = "Predicted Cases", range = c(0, upper_limit),
                           showgrid = TRUE, gridcolor = "#e8f1f4"),
          hovermode = "x unified",
          plot_bgcolor  = "white",
          paper_bgcolor = "white"
        )
    }
  })
  
  output$data_table <- renderDT({
    datatable(
      model_data,
      options  = list(pageLength = 12, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  # ── Download handlers ──────────────────────────────────────
  
  output$download_model_metrics <- downloadHandler(
    filename = function() "model_comparison.csv",
    content  = function(file) write_csv(model_comparison, file)
  )
  
  output$download_feature_importance <- downloadHandler(
    filename = function() "xgboost_feature_importance.csv",
    content  = function(file) write_csv(feature_data, file)
  )
  
  output$download_forecast <- downloadHandler(
    filename = function() "xgboost_4week_forecast.csv",
    content  = function(file) write_csv(forecast_data, file)
  )
  
  output$download_model_data <- downloadHandler(
    filename = function() "model_ready_flu.csv",
    content  = function(file) write_csv(model_data, file)
  )
}

# ------------------------------------------------------------
# Run App
# ------------------------------------------------------------

shinyApp(ui = ui, server = server)