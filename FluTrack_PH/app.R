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
# Dashboard UI (shown after login)
# ------------------------------------------------------------

dashboard_ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title      = span(icon("heartbeat"), " FluTrack PH"),
    titleWidth = 280,
    tags$li(
      class = "dropdown",
      actionLink(
        "logout",
        label = tagList(icon("sign-out-alt"), span("Logout")),
        class = "logout-btn"
      )
    )
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
    
    tags$head(
      tags$link(
        rel  = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Rajdhani:wght@400;500;600;700&family=Inter:wght@300;400;500;600&family=Share+Tech+Mono&display=swap"
      ),
      tags$style(HTML("

      /* ================================================================
         FLUTRACK PH — Futuristic Surveillance Theme
         Fonts:
           Rajdhani     — headings, titles, metric values (techy)
           Inter        — body text, labels, notes (clean readable)
           Share Tech Mono — data values, badges (terminal feel)
         Palette:
           bg-deep:    #080a0f   page background (near-black blue tint)
           bg-surface: #0e1117   card / box surface
           bg-raised:  #161b24   elevated elements
           bg-border:  #1e2735   dividers
           crimson:    #be123c   primary accent
           crimson-lt: #f43f5e   glow / hover accent
           crimson-dim: rgba(190,18,60,0.12)  fill tint
           amber:      #f59e0b   warning accent
           emerald:    #10b981   positive accent
           cyan-dim:   rgba(6,214,160,0.08)   subtle grid tint
           text-hi:    #e8eaf0   primary text
           text-mid:   #8892a4   secondary text
           text-lo:    #4a5568   muted / label text
      ================================================================ */

      /* ── Reset & base ─────────────────────────────────── */
      *, *::before, *::after { box-sizing: border-box; }

      html, body {
        height: auto !important;
        min-height: 100%;
        overflow-x: hidden;
        overflow-y: auto !important;
      }

      /* Neutralize the top-level fluidPage wrapper padding */
      body > .container-fluid,
      .container-fluid:has(> #app_ui) {
        padding: 0 !important;
        margin: 0 !important;
        width: 100%;
      }

      body, .content-wrapper, .right-side, .wrapper {
        font-family: 'Inter', 'Segoe UI', sans-serif;
        background: #080a0f !important;
        color: #e8eaf0;
      }

      /* ── Page background ──────────────────────────────── */
      body { background: #080a0f !important; }

      #flutrack-bg {
        position: fixed;
        top: 0; left: 0;
        width: 100vw; height: 100vh;
        z-index: 0;
        pointer-events: none;
      }

      /* Stacking + scroll: header fixed on top, sidebar fixed at left,
         content scrolls naturally with left margin to clear the sidebar. */
      .wrapper {
        position: relative;
        z-index: 1;
        overflow: visible !important;
      }

      .main-header {
        position: fixed !important;
        top: 0; left: 0; right: 0;
        z-index: 1030 !important;
        border-bottom: 1px solid #be123c !important;
        box-shadow: 0 1px 12px rgba(190, 18, 60, 0.25) !important;
      }
      .main-header .navbar { z-index: 1030 !important; min-height: 50px; }
      .main-header .logo   { z-index: 1031 !important; }

      .main-sidebar {
        position: fixed !important;
        top: 50px; left: 0; bottom: 0;
        z-index: 1020 !important;
        overflow-y: auto;
      }

      .content-wrapper {
        position: relative;
        z-index: 1;
        margin-top: 50px !important;
        margin-left: 280px !important;
        min-height: calc(100vh - 50px);
        background: transparent !important;
        overflow: visible !important;
      }

      .right-side {
        z-index: 1;
        background: transparent !important;
      }

      /* Collapsed-sidebar safety: when shinydashboard collapses sidebar */
      .sidebar-collapse .content-wrapper { margin-left: 0 !important; }
      .sidebar-collapse .main-header .navbar { margin-left: 0 !important; }

      @media (max-width: 768px) {
        .content-wrapper { margin-left: 0 !important; }
        .main-header .navbar { margin-left: 0 !important; }
      }

      /* ── Header ───────────────────────────────────────── */
      .main-header .logo {
        background: #0e1117 !important;
        border-bottom: 1px solid #1e2735 !important;
        border-right: 1px solid #1e2735 !important;
        font-family: 'Rajdhani', sans-serif !important;
        font-weight: 700 !important;
        letter-spacing: 1.5px !important;
        color: #e8eaf0 !important;
        font-size: 17px !important;
        line-height: 50px !important;
        height: 50px !important;
        text-transform: uppercase;
      }

      .main-header .logo .logo-lg { color: #e8eaf0 !important; }
      .main-header .logo .fa { color: #f43f5e !important; margin-right: 4px; }

      .main-header .navbar {
        background: #0e1117 !important;
        margin-left: 280px;
        min-height: 50px;
      }

      /* Keep the logout menu visible on the right, never collapsing */
      .main-header .navbar-custom-menu {
        float: right !important;
        display: block !important;
        position: static !important;
      }
      .main-header .navbar-custom-menu .nav,
      .main-header .navbar-nav {
        display: flex !important;
        flex-direction: row !important;
        align-items: center;
        margin: 0 !important;
        float: none !important;
      }
      .main-header .navbar-nav > li {
        float: none !important;
        display: inline-flex !important;
        align-items: center;
      }
      .main-header .navbar-nav > li > a { line-height: 50px; }

      /* Sidebar hamburger toggle — left side, themed */
      .main-header .sidebar-toggle {
        float: left !important;
        color: #8892a4 !important;
        background: transparent !important;
        line-height: 50px !important;
        height: 50px;
        padding: 0 18px !important;
        transition: color 0.18s, background 0.18s;
      }
      .main-header .sidebar-toggle .fa { color: inherit !important; }
      .main-header .sidebar-toggle:hover,
      .main-header .sidebar-toggle:focus {
        color: #f43f5e !important;
        background: rgba(190,18,60,0.08) !important;
      }

      .main-header .navbar .nav > li > a {
        color: #8892a4 !important;
      }

      /* ── Sidebar ──────────────────────────────────────── */
      .skin-blue .main-sidebar,
      .skin-blue .left-side {
        background: #080a0f !important;
        border-right: 1px solid #1e2735;
      }

      .skin-blue .sidebar-menu > li > a {
        font-family: 'Inter', sans-serif !important;
        color: #8892a4 !important;
        font-weight: 500;
        font-size: 12.5px;
        letter-spacing: 0.3px;
        padding: 12px 16px;
        border-left: 2px solid transparent;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        transition: all 0.2s;
      }

      .skin-blue .sidebar-menu > li > a:hover {
        background: rgba(190,18,60,0.08) !important;
        color: #e8eaf0 !important;
        border-left-color: rgba(190,18,60,0.5);
      }

      .skin-blue .sidebar-menu > li.active > a {
        background: rgba(190,18,60,0.12) !important;
        color: #f43f5e !important;
        border-left: 2px solid #be123c !important;
        font-weight: 600;
        letter-spacing: 0.4px;
      }

      .skin-blue .sidebar-menu > li > a .fa {
        color: inherit !important;
        width: 20px;
        margin-right: 8px;
        opacity: 0.75;
      }

      .skin-blue .sidebar-menu > li.active > a .fa { opacity: 1; }

      .sidebar-menu li.header {
        font-family: 'Share Tech Mono', monospace !important;
        color: #4a5568 !important;
        font-size: 10px;
        letter-spacing: 1.4px;
        text-transform: uppercase;
        padding: 12px 16px 6px;
      }

      /* ── Page content ─────────────────────────────────── */
      .content { padding: 22px 24px 48px; }

      /* Override shinydashboard bg override from content-wrapper */
      .content-wrapper {
        background: transparent !important;
      }

      /* ── Hero card ─────────────────────────────────────── */
      .hero-card {
        background: #0e1117;
        border: 1px solid #1e2735;
        border-left: 3px solid #be123c;
        border-radius: 12px;
        padding: 24px 28px;
        margin-bottom: 20px;
        position: relative;
        overflow: hidden;
      }

      /* Corner bracket decoration top-right */
      .hero-card::before {
        content: '';
        position: absolute;
        top: 10px; right: 10px;
        width: 20px; height: 20px;
        border-top: 2px solid rgba(190,18,60,0.5);
        border-right: 2px solid rgba(190,18,60,0.5);
        pointer-events: none;
      }

      /* Corner bracket decoration bottom-left */
      .hero-card::after {
        content: '';
        position: absolute;
        bottom: 10px; left: 10px;
        width: 20px; height: 20px;
        border-bottom: 2px solid rgba(190,18,60,0.3);
        border-left: 2px solid rgba(190,18,60,0.3);
        pointer-events: none;
      }

      .hero-title {
        font-family: 'Rajdhani', sans-serif;
        font-size: 24px;
        font-weight: 700;
        color: #e8eaf0;
        letter-spacing: 1px;
        text-transform: uppercase;
        margin-bottom: 8px;
        line-height: 1.25;
      }

      .hero-subtitle {
        font-family: 'Inter', sans-serif;
        font-size: 13px;
        line-height: 1.75;
        color: #8892a4;
        font-weight: 300;
      }

      /* ── Metric cards ─────────────────────────────────── */
      .metric-card {
        background: #0e1117;
        border: 1px solid #1e2735;
        border-radius: 10px;
        padding: 18px 20px 16px;
        height: 148px;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        margin-bottom: 20px;
        overflow: hidden;
        position: relative;
      }

      /* Glowing top accent bar */
      .metric-card::before {
        content: '';
        position: absolute;
        top: 0; left: 0; right: 0;
        height: 2px;
        background: #be123c;
        box-shadow: 0 0 8px rgba(190,18,60,0.7);
      }

      /* Corner bracket top-right */
      .metric-card::after {
        content: '';
        position: absolute;
        top: 8px; right: 8px;
        width: 10px; height: 10px;
        border-top: 1px solid rgba(190,18,60,0.4);
        border-right: 1px solid rgba(190,18,60,0.4);
        pointer-events: none;
      }

      .metric-card.green::before  { background: #10b981; box-shadow: 0 0 8px rgba(16,185,129,0.6); }
      .metric-card.yellow::before { background: #f59e0b; box-shadow: 0 0 8px rgba(245,158,11,0.6); }
      .metric-card.red::before    { background: #f43f5e; box-shadow: 0 0 8px rgba(244,63,94,0.7); }
      .metric-card.purple::before { background: #8b5cf6; box-shadow: 0 0 8px rgba(139,92,246,0.6); }

      .metric-label {
        font-family: 'Share Tech Mono', monospace;
        font-size: 10px;
        text-transform: uppercase;
        letter-spacing: 1.4px;
        color: #4a5568;
      }

      .metric-value {
        font-family: 'Rajdhani', sans-serif;
        font-size: 28px;
        font-weight: 700;
        color: #e8eaf0;
        letter-spacing: 0.5px;
        line-height: 1.1;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }

      .metric-note {
        font-family: 'Inter', sans-serif;
        font-size: 11px;
        color: #4a5568;
        line-height: 1.45;
        overflow: hidden;
        text-overflow: ellipsis;
        display: -webkit-box;
        -webkit-line-clamp: 2;
        -webkit-box-orient: vertical;
      }

      /* ── Boxes ────────────────────────────────────────── */
      .box {
        background: #0e1117 !important;
        border: 1px solid #1e2735 !important;
        border-top: 0 !important;
        border-radius: 10px !important;
        margin-bottom: 20px;
        overflow: visible;
      }

      .box.box-primary > .box-header {
        background: #0e1117 !important;
        color: #e8eaf0 !important;
        border-bottom: 1px solid #1e2735 !important;
        border-radius: 10px 10px 0 0 !important;
        padding: 12px 18px;
        position: relative;
      }

      /* Crimson glow line under box header */
      .box.box-primary > .box-header::after {
        content: '';
        position: absolute;
        bottom: -1px; left: 18px; right: 18px;
        height: 1px;
        background: linear-gradient(to right, #be123c, transparent);
        opacity: 0.5;
      }

      .box-title {
        font-family: 'Rajdhani', sans-serif !important;
        font-weight: 600 !important;
        font-size: 14px !important;
        letter-spacing: 0.8px !important;
        text-transform: uppercase !important;
        color: #e8eaf0 !important;
        display: inline-flex;
        align-items: center;
        gap: 6px;
      }

      .box-body {
        padding: 16px 18px;
        background: #0e1117;
        border-radius: 0 0 10px 10px;
      }

      /* ── Section note ─────────────────────────────────── */
      .section-note {
        background: rgba(190,18,60,0.05);
        border-left: 2px solid #be123c;
        border-radius: 0 6px 6px 0;
        padding: 10px 14px;
        margin-bottom: 14px;
        font-family: 'Inter', sans-serif;
        color: #8892a4;
        font-size: 12.5px;
        line-height: 1.7;
        letter-spacing: 0.1px;
      }

      /* ── Risk badge ───────────────────────────────────── */
      .risk-badge {
        display: inline-block;
        padding: 3px 12px;
        border-radius: 3px;
        background: rgba(16,185,129,0.1);
        color: #10b981;
        border: 1px solid rgba(16,185,129,0.3);
        font-family: 'Share Tech Mono', monospace;
        font-weight: 400;
        font-size: 12px;
        vertical-align: middle;
        margin-left: 8px;
        letter-spacing: 1px;
        text-transform: uppercase;
      }

      /* ── Help tooltip ─────────────────────────────────── */
      .help-tip {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        margin-left: 6px;
        width: 16px;
        height: 16px;
        border-radius: 50%;
        background: transparent;
        color: #4a5568;
        font-size: 11px;
        cursor: help;
        position: relative;
        vertical-align: middle;
        flex-shrink: 0;
        border: 1px solid #1e2735;
        transition: color 0.15s, border-color 0.15s;
      }

      .help-tip:hover {
        color: #f43f5e;
        border-color: rgba(190,18,60,0.5);
      }

      .help-tip:hover::after {
        content: attr(data-tooltip);
        position: absolute;
        top: calc(100% + 8px);
        left: 50%;
        transform: translateX(-50%);
        width: 280px;
        background: #0e1117;
        color: #8892a4;
        border: 1px solid #1e2735;
        border-left: 2px solid #be123c;
        padding: 10px 13px;
        border-radius: 6px;
        font-family: 'Inter', sans-serif;
        font-size: 11.5px;
        line-height: 1.6;
        font-weight: 400;
        z-index: 9999;
        white-space: normal;
        pointer-events: none;
      }

      .help-tip:hover::before {
        content: '';
        position: absolute;
        top: calc(100% + 2px);
        left: 50%;
        transform: translateX(-50%);
        border: 5px solid transparent;
        border-bottom-color: #1e2735;
        z-index: 9999;
        pointer-events: none;
      }

      /* ── Buttons ──────────────────────────────────────── */
      .btn-default,
      .shiny-download-link {
        background: transparent !important;
        color: #8892a4 !important;
        border: 1px solid #1e2735 !important;
        border-radius: 4px !important;
        font-family: 'Share Tech Mono', monospace !important;
        font-size: 12px !important;
        font-weight: 400 !important;
        letter-spacing: 0.8px !important;
        padding: 7px 16px !important;
        text-transform: uppercase !important;
        transition: all 0.2s;
      }

      .btn-default:hover,
      .shiny-download-link:hover {
        border-color: #be123c !important;
        color: #f43f5e !important;
        background: rgba(190,18,60,0.06) !important;
        box-shadow: 0 0 8px rgba(190,18,60,0.2) !important;
      }

      /* ── Inputs & selects ─────────────────────────────── */
      .selectize-input,
      .form-control,
      select {
        background: #080a0f !important;
        color: #e8eaf0 !important;
        border: 1px solid #1e2735 !important;
        border-radius: 6px !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 13px !important;
      }

      .selectize-dropdown {
        background: #0e1117 !important;
        border: 1px solid #1e2735 !important;
        color: #e8eaf0 !important;
        border-radius: 6px !important;
      }

      .selectize-dropdown .option:hover,
      .selectize-dropdown .active {
        background: rgba(190,18,60,0.1) !important;
        color: #f43f5e !important;
      }

      /* Slider */
      .irs--shiny .irs-bar,
      .irs--shiny .irs-handle {
        background: #be123c !important;
        border-color: #be123c !important;
      }

      .irs--shiny .irs-from,
      .irs--shiny .irs-to,
      .irs--shiny .irs-single {
        background: #be123c !important;
        font-family: 'Share Tech Mono', monospace !important;
        font-size: 11px !important;
      }

      .irs--shiny .irs-line {
        background: #1e2735 !important;
        border-color: #1e2735 !important;
      }

      /* ── DataTables ───────────────────────────────────── */
      .dataTables_wrapper {
        font-family: 'Inter', sans-serif;
        font-size: 12.5px;
        color: #8892a4;
      }

      table.dataTable thead th {
        background: #080a0f !important;
        color: #4a5568 !important;
        border-bottom: 1px solid #1e2735 !important;
        font-family: 'Share Tech Mono', monospace !important;
        font-weight: 400 !important;
        font-size: 11px !important;
        text-transform: uppercase !important;
        letter-spacing: 1px !important;
      }

      table.dataTable tbody tr {
        background: #0e1117 !important;
        color: #8892a4 !important;
      }

      table.dataTable tbody tr:hover {
        background: rgba(190,18,60,0.06) !important;
        color: #e8eaf0 !important;
      }

      table.dataTable tbody td {
        border-top: 1px solid #1e2735 !important;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #4a5568 !important;
        font-family: 'Share Tech Mono', monospace !important;
        font-size: 11px !important;
        border-radius: 3px !important;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: transparent !important;
        border: 1px solid #be123c !important;
        color: #f43f5e !important;
        border-radius: 3px !important;
      }

      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        background: #080a0f !important;
        color: #e8eaf0 !important;
        border: 1px solid #1e2735 !important;
        border-radius: 4px !important;
        font-family: 'Inter', sans-serif !important;
      }

      .dataTables_wrapper .dataTables_info {
        color: #4a5568 !important;
        font-family: 'Share Tech Mono', monospace !important;
        font-size: 11px !important;
        letter-spacing: 0.5px !important;
      }

      /* ── Guide cards ──────────────────────────────────── */
      .guide-card {
        background: #0e1117;
        border: 1px solid #1e2735;
        border-left: 3px solid #be123c;
        border-radius: 8px;
        padding: 16px 20px;
        margin-bottom: 14px;
        position: relative;
      }

      .guide-card::after {
        content: '';
        position: absolute;
        top: 8px; right: 8px;
        width: 8px; height: 8px;
        border-top: 1px solid rgba(190,18,60,0.35);
        border-right: 1px solid rgba(190,18,60,0.35);
      }

      .guide-title {
        font-family: 'Rajdhani', sans-serif;
        font-weight: 600;
        color: #e8eaf0;
        font-size: 15px;
        letter-spacing: 0.6px;
        text-transform: uppercase;
        margin-bottom: 6px;
      }

      .guide-text {
        font-family: 'Inter', sans-serif;
        color: #8892a4;
        font-size: 12.5px;
        line-height: 1.7;
        font-weight: 300;
      }

      /* ── Scrollbar ────────────────────────────────────── */
      ::-webkit-scrollbar { width: 5px; height: 5px; }
      ::-webkit-scrollbar-track { background: #080a0f; }
      ::-webkit-scrollbar-thumb { background: #1e2735; border-radius: 2px; }
      ::-webkit-scrollbar-thumb:hover { background: #be123c; }

      /* ── Header right-side menu (theme + logout) ──────── */
      .main-header .navbar-custom-menu,
      .main-header .navbar-nav {
        float: right !important;
      }

      .main-header .navbar-nav > .dropdown {
        display: inline-block;
      }

      .logout-btn {
        display: inline-flex !important;
        align-items: center;
        gap: 7px;
        color: #8892a4 !important;
        font-family: 'Share Tech Mono', monospace !important;
        font-size: 12px !important;
        letter-spacing: 0.6px;
        text-transform: uppercase;
        padding: 0 18px !important;
        line-height: 50px !important;
        height: 50px;
        text-decoration: none !important;
        transition: color 0.18s, background 0.18s;
      }

      .logout-btn .fa { font-size: 13px; }

      .logout-btn:hover,
      .logout-btn:focus {
        color: #f59e0b !important;
        background: rgba(245,158,11,0.10) !important;
      }

      /* ── Responsive ───────────────────────────────────── */
      @media (max-width: 1280px) {
        .hero-title    { font-size: 20px; }
        .metric-value  { font-size: 24px; }
        .metric-card   { height: 140px; }
        .help-tip:hover::after { left: auto; right: 0; transform: none; }
        .help-tip:hover::before { left: auto; right: 6px; transform: none; }
      }

      @media (max-width: 992px) {
        .hero-title   { font-size: 17px; }
        .metric-value { font-size: 20px; }
        .metric-card  { height: auto; min-height: 130px; }
        .content      { padding: 14px 16px; }
      }

    "))),
    
    tags$canvas(id = "flutrack-bg"),
    tags$script(HTML("
      (function() {
        var canvas = document.getElementById('flutrack-bg');
        var ctx    = canvas.getContext('2d');
        var W, H;

        /* Dark surveillance palette */
        var C   = 'rgba(190,18,60,';
        var CLT = 'rgba(244,63,94,';
        function gridColor() { return 'rgba(30,39,53,0.85)'; }
        function bgColor()   { return '#080a0f'; }

        function resize() {
          W = canvas.width  = window.innerWidth;
          H = canvas.height = window.innerHeight;
        }
        window.addEventListener('resize', resize);
        resize();

        /* ── Mouse state ──────────────────────────────── */
        var mouse = { x: -999, y: -999, active: false };
        document.addEventListener('mousemove', function(e) {
          mouse.x = e.clientX; mouse.y = e.clientY; mouse.active = true;
        });

        /* ── Click ripples ────────────────────────────── */
        var ripples = [];
        document.addEventListener('click', function(e) {
          ripples.push({ x: e.clientX, y: e.clientY, r: 0, alpha: 0.7, life: 1.0 });
          nodes.push({
            x: e.clientX, y: e.clientY,
            vx: (Math.random() - 0.5) * 0.5,
            vy: (Math.random() - 0.5) * 0.5,
            r: 2.5, pulse: 0, ttl: 220, maxTtl: 220, click: true
          });
        });

        function drawRipples() {
          for (var i = ripples.length - 1; i >= 0; i--) {
            var rp = ripples[i];
            rp.r += 2.2; rp.life -= 0.022; rp.alpha = rp.life * 0.65;
            if (rp.life <= 0) { ripples.splice(i, 1); continue; }
            ctx.beginPath();
            ctx.arc(rp.x, rp.y, rp.r, 0, Math.PI * 2);
            ctx.strokeStyle = C + rp.alpha + ')';
            ctx.lineWidth = 1.2; ctx.stroke();
            if (rp.r > 14) {
              ctx.beginPath();
              ctx.arc(rp.x, rp.y, rp.r * 0.45, 0, Math.PI * 2);
              ctx.strokeStyle = CLT + (rp.alpha * 0.5) + ')';
              ctx.lineWidth = 0.7; ctx.stroke();
            }
            var tick = 6;
            [[rp.r,0],[-rp.r,0],[0,rp.r],[0,-rp.r]].forEach(function(d) {
              ctx.beginPath();
              ctx.moveTo(rp.x + d[0] - (d[1] ? tick : 0), rp.y + d[1] - (d[0] ? tick : 0));
              ctx.lineTo(rp.x + d[0] + (d[1] ? tick : 0), rp.y + d[1] + (d[0] ? tick : 0));
              ctx.strokeStyle = CLT + (rp.alpha * 0.8) + ')';
              ctx.lineWidth = 1; ctx.stroke();
            });
          }
        }

        /* ── Constellation nodes (denser) ─────────────── */
        var nodes = [];
        var BASE_COUNT = 52;
        for (var i = 0; i < BASE_COUNT; i++) {
          nodes.push({
            x: Math.random() * window.innerWidth,
            y: Math.random() * window.innerHeight,
            vx: (Math.random() - 0.5) * 0.26,
            vy: (Math.random() - 0.5) * 0.26,
            r:  Math.random() * 1.5 + 0.7,
            pulse: Math.random() * Math.PI * 2,
            ttl: Infinity, maxTtl: Infinity, click: false
          });
        }

        function drawNodes() {
          var LINK = 135;
          var MLNK = 210;
          var lineBase = 0.16;

          for (var i = 0; i < nodes.length; i++) {
            for (var j = i + 1; j < nodes.length; j++) {
              var dx = nodes[i].x - nodes[j].x,
                  dy = nodes[i].y - nodes[j].y,
                  d  = Math.sqrt(dx*dx + dy*dy);
              if (d < LINK) {
                ctx.strokeStyle = C + ((1 - d/LINK) * lineBase) + ')';
                ctx.lineWidth = 0.6;
                ctx.beginPath();
                ctx.moveTo(nodes[i].x, nodes[i].y);
                ctx.lineTo(nodes[j].x, nodes[j].y);
                ctx.stroke();
              }
            }
          }

          if (mouse.active) {
            for (var i = 0; i < nodes.length; i++) {
              var dx = mouse.x - nodes[i].x,
                  dy = mouse.y - nodes[i].y,
                  d  = Math.sqrt(dx*dx + dy*dy);
              if (d < MLNK) {
                ctx.strokeStyle = CLT + ((1 - d/MLNK) * 0.55) + ')';
                ctx.lineWidth = 0.9;
                ctx.beginPath();
                ctx.moveTo(mouse.x, mouse.y);
                ctx.lineTo(nodes[i].x, nodes[i].y);
                ctx.stroke();
              }
            }
            ctx.beginPath();
            ctx.arc(mouse.x, mouse.y, 2.8, 0, Math.PI * 2);
            ctx.fillStyle = CLT + '0.75)';
            ctx.fill();
            var ch = 9;
            ctx.strokeStyle = CLT + '0.4)';
            ctx.lineWidth = 0.8;
            ctx.beginPath(); ctx.moveTo(mouse.x - ch, mouse.y); ctx.lineTo(mouse.x + ch, mouse.y); ctx.stroke();
            ctx.beginPath(); ctx.moveTo(mouse.x, mouse.y - ch); ctx.lineTo(mouse.x, mouse.y + ch); ctx.stroke();
          }

          for (var i = nodes.length - 1; i >= 0; i--) {
            var n = nodes[i];
            if (n.click) { n.ttl--; if (n.ttl <= 0) { nodes.splice(i, 1); continue; } }
            n.pulse += 0.018;
            var life = n.click ? (n.ttl / n.maxTtl) : 1;
            var b    = (0.4 + 0.6 * (Math.sin(n.pulse) * 0.5 + 0.5)) * life;
            ctx.beginPath();
            ctx.arc(n.x, n.y, n.click ? n.r * life : n.r, 0, Math.PI * 2);
            ctx.fillStyle = (n.click ? CLT : C) + (b * 0.6) + ')';
            ctx.fill();
            n.x += n.vx; n.y += n.vy;
            if (!n.click) {
              if (n.x < 0 || n.x > W) n.vx *= -1;
              if (n.y < 0 || n.y > H) n.vy *= -1;
            }
          }
        }

        /* ── ECG heartbeat ────────────────────────────── */
        var ecgX = 0, ecgY, ecgBuf = [], ecgMax = 320;
        function ecgPoint(x) {
          var p = x % 200;
          if (p > 80 && p < 84)  return 11;
          if (p > 84 && p < 88)  return -62;
          if (p > 88 && p < 95)  return 28;
          if (p > 95 && p < 100) return 8;
          return 0;
        }
        function drawECG() {
          ecgY = H * 0.82;
          ecgX = (ecgX + 2.2) % W;
          ecgBuf.push({ x: ecgX, y: ecgY + ecgPoint(ecgX) });
          if (ecgBuf.length > ecgMax) ecgBuf.shift();
          for (var i = 1; i < ecgBuf.length; i++) {
            ctx.strokeStyle = C + ((i / ecgBuf.length) * 0.6) + ')';
            ctx.lineWidth = 1.4;
            ctx.beginPath();
            ctx.moveTo(ecgBuf[i-1].x, ecgBuf[i-1].y);
            ctx.lineTo(ecgBuf[i].x, ecgBuf[i].y);
            ctx.stroke();
          }
          var last = ecgBuf[ecgBuf.length - 1];
          if (last) {
            ctx.beginPath();
            ctx.arc(last.x, last.y, 2.5, 0, Math.PI * 2);
            ctx.fillStyle = CLT + '0.9)';
            ctx.fill();
          }
        }

        /* ── Grid ─────────────────────────────────────── */
        function drawGrid() {
          ctx.strokeStyle = gridColor();
          ctx.lineWidth = 0.5;
          var s = 48;
          for (var x = 0; x < W; x += s) {
            ctx.beginPath(); ctx.moveTo(x, 0); ctx.lineTo(x, H); ctx.stroke();
          }
          for (var y = 0; y < H; y += s) {
            ctx.beginPath(); ctx.moveTo(0, y); ctx.lineTo(W, y); ctx.stroke();
          }
        }

        /* ── Scanline ─────────────────────────────────── */
        var scanY = 0;
        function drawScanline() {
          scanY = (scanY + 0.55) % H;
          var op = 0.055;
          var g = ctx.createLinearGradient(0, scanY - 60, 0, scanY + 4);
          g.addColorStop(0,   'rgba(190,18,60,0)');
          g.addColorStop(0.7, 'rgba(190,18,60,' + (op * 0.4) + ')');
          g.addColorStop(1,   'rgba(190,18,60,' + op + ')');
          ctx.fillStyle = g;
          ctx.fillRect(0, scanY - 60, W, 64);
        }

        /* ── Main loop ────────────────────────────────── */
        function loop() {
          ctx.clearRect(0, 0, W, H);
          ctx.fillStyle = bgColor();
          ctx.fillRect(0, 0, W, H);
          drawGrid();
          drawScanline();
          drawNodes();
          drawECG();
          drawRipples();
          requestAnimationFrame(loop);
        }
        requestAnimationFrame(loop);
      })();
    ")),
    
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
# Login screen UI
# ------------------------------------------------------------

login_ui <- fluidPage(
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Rajdhani:wght@400;500;600;700&family=Inter:wght@300;400;500;600&family=Share+Tech+Mono&display=swap"
    ),
    tags$style(HTML("
      html, body { margin: 0; padding: 0; height: 100%; background: #080a0f; overflow: hidden; }

      #login-bg {
        position: fixed; top: 0; left: 0;
        width: 100%; height: 100%; z-index: 0; pointer-events: none;
      }

      .login-wrap {
        position: relative; z-index: 1;
        min-height: 100vh;
        display: flex; align-items: center; justify-content: center;
        font-family: 'Inter', sans-serif;
      }

      .login-card {
        width: 380px;
        background: rgba(14,17,23,0.92);
        border: 1px solid #1e2735;
        border-radius: 14px;
        padding: 38px 34px 32px;
        position: relative;
        backdrop-filter: blur(2px);
      }

      .login-card::before {
        content: '';
        position: absolute; top: 0; left: 0; right: 0;
        height: 2px; background: #be123c;
        box-shadow: 0 0 12px rgba(190,18,60,0.7);
        border-radius: 14px 14px 0 0;
      }
      .login-card::after {
        content: ''; position: absolute; top: 12px; right: 12px;
        width: 16px; height: 16px;
        border-top: 2px solid rgba(190,18,60,0.5);
        border-right: 2px solid rgba(190,18,60,0.5);
      }

      .login-icon {
        text-align: center; color: #f43f5e; font-size: 34px; margin-bottom: 10px;
      }

      .login-title {
        font-family: 'Rajdhani', sans-serif;
        font-size: 24px; font-weight: 700; letter-spacing: 1.5px;
        text-transform: uppercase; text-align: center;
        color: #e8eaf0; margin-bottom: 2px;
      }

      .login-sub {
        font-family: 'Share Tech Mono', monospace;
        font-size: 11px; letter-spacing: 1px; text-transform: uppercase;
        text-align: center; color: #4a5568; margin-bottom: 26px;
      }

      .login-card .form-group { margin-bottom: 16px; }

      .login-card label {
        font-family: 'Share Tech Mono', monospace;
        font-size: 10.5px; letter-spacing: 1px; text-transform: uppercase;
        color: #8892a4; margin-bottom: 5px;
      }

      .login-card .form-control {
        background: #080a0f !important;
        border: 1px solid #1e2735 !important;
        border-radius: 6px !important;
        color: #e8eaf0 !important;
        font-family: 'Inter', sans-serif !important;
        font-size: 14px !important;
        height: 42px;
        transition: border-color 0.18s, box-shadow 0.18s;
      }
      .login-card .form-control:focus {
        border-color: #be123c !important;
        box-shadow: 0 0 0 2px rgba(190,18,60,0.15) !important;
      }

      .login-btn {
        width: 100%;
        background: #be123c !important;
        color: #fff !important;
        border: none !important;
        border-radius: 6px !important;
        font-family: 'Rajdhani', sans-serif !important;
        font-size: 16px !important;
        font-weight: 600 !important;
        letter-spacing: 1.5px !important;
        text-transform: uppercase !important;
        height: 44px;
        margin-top: 8px;
        transition: background 0.18s, box-shadow 0.18s;
      }
      .login-btn:hover {
        background: #f43f5e !important;
        box-shadow: 0 0 16px rgba(190,18,60,0.45) !important;
      }

      .login-hint {
        font-family: 'Share Tech Mono', monospace;
        font-size: 10.5px; letter-spacing: 0.5px;
        text-align: center; color: #4a5568; margin-top: 18px;
      }

      .login-error {
        font-family: 'Inter', sans-serif;
        font-size: 12.5px; color: #f43f5e;
        text-align: center; margin-top: 14px; min-height: 16px;
      }
    "))
  ),
  
  tags$canvas(id = "login-bg"),
  tags$script(HTML("
    (function() {
      var canvas = document.getElementById('login-bg');
      if (!canvas) return;
      var ctx = canvas.getContext('2d');
      var W, H;
      var C = 'rgba(190,18,60,', CLT = 'rgba(244,63,94,';
      function resize() { W = canvas.width = window.innerWidth; H = canvas.height = window.innerHeight; }
      window.addEventListener('resize', resize); resize();

      var mouse = { x: -999, y: -999, active: false };
      document.addEventListener('mousemove', function(e){ mouse.x=e.clientX; mouse.y=e.clientY; mouse.active=true; });

      var nodes = [];
      for (var i = 0; i < 90; i++) {
        nodes.push({
          x: Math.random()*window.innerWidth, y: Math.random()*window.innerHeight,
          vx:(Math.random()-0.5)*0.3, vy:(Math.random()-0.5)*0.3,
          r: Math.random()*1.5+0.7, pulse: Math.random()*Math.PI*2
        });
      }

      var ecgX=0, ecgBuf=[], ecgMax=340;
      function ecgPoint(x){ var p=x%200; if(p>80&&p<84)return 11; if(p>84&&p<88)return -62; if(p>88&&p<95)return 28; if(p>95&&p<100)return 8; return 0; }

      function loop() {
        ctx.clearRect(0,0,W,H);
        ctx.fillStyle = '#080a0f'; ctx.fillRect(0,0,W,H);

        ctx.strokeStyle = 'rgba(30,39,53,0.8)'; ctx.lineWidth = 0.5;
        for (var x=0;x<W;x+=48){ ctx.beginPath(); ctx.moveTo(x,0); ctx.lineTo(x,H); ctx.stroke(); }
        for (var y=0;y<H;y+=48){ ctx.beginPath(); ctx.moveTo(0,y); ctx.lineTo(W,y); ctx.stroke(); }

        for (var i=0;i<nodes.length;i++){
          for (var j=i+1;j<nodes.length;j++){
            var dx=nodes[i].x-nodes[j].x, dy=nodes[i].y-nodes[j].y, d=Math.sqrt(dx*dx+dy*dy);
            if (d<160){ ctx.strokeStyle=C+((1-d/160)*0.16)+')'; ctx.lineWidth=0.6;
              ctx.beginPath(); ctx.moveTo(nodes[i].x,nodes[i].y); ctx.lineTo(nodes[j].x,nodes[j].y); ctx.stroke(); }
          }
        }
        if (mouse.active){
          for (var i=0;i<nodes.length;i++){
            var dx=mouse.x-nodes[i].x, dy=mouse.y-nodes[i].y, d=Math.sqrt(dx*dx+dy*dy);
            if (d<210){ ctx.strokeStyle=CLT+((1-d/210)*0.5)+')'; ctx.lineWidth=0.9;
              ctx.beginPath(); ctx.moveTo(mouse.x,mouse.y); ctx.lineTo(nodes[i].x,nodes[i].y); ctx.stroke(); }
          }
        }
        for (var i=0;i<nodes.length;i++){
          var n=nodes[i]; n.pulse+=0.018;
          var b=0.4+0.6*(Math.sin(n.pulse)*0.5+0.5);
          ctx.beginPath(); ctx.arc(n.x,n.y,n.r,0,Math.PI*2); ctx.fillStyle=C+(b*0.6)+')'; ctx.fill();
          n.x+=n.vx; n.y+=n.vy;
          if(n.x<0||n.x>W)n.vx*=-1; if(n.y<0||n.y>H)n.vy*=-1;
        }

        ecgX=(ecgX+2.2)%W;
        ecgBuf.push({x:ecgX, y:H*0.85+ecgPoint(ecgX)});
        if(ecgBuf.length>ecgMax)ecgBuf.shift();
        for(var i=1;i<ecgBuf.length;i++){
          ctx.strokeStyle=C+((i/ecgBuf.length)*0.6)+')'; ctx.lineWidth=1.4;
          ctx.beginPath(); ctx.moveTo(ecgBuf[i-1].x,ecgBuf[i-1].y); ctx.lineTo(ecgBuf[i].x,ecgBuf[i].y); ctx.stroke();
        }
        var last=ecgBuf[ecgBuf.length-1];
        if(last){ ctx.beginPath(); ctx.arc(last.x,last.y,2.5,0,Math.PI*2); ctx.fillStyle=CLT+'0.9)'; ctx.fill(); }

        requestAnimationFrame(loop);
      }
      requestAnimationFrame(loop);
    })();
  ")),
  
  div(
    class = "login-wrap",
    div(
      class = "login-card",
      div(class = "login-icon", icon("heartbeat")),
      div(class = "login-title", "FluTrack PH"),
      div(class = "login-sub", "Surveillance Access Terminal"),
      textInput("login_user", "Username", placeholder = "admin"),
      passwordInput("login_pass", "Password", placeholder = "\u2022\u2022\u2022\u2022\u2022\u2022\u2022\u2022"),
      actionButton("login_submit", "Authenticate", class = "login-btn"),
      div(class = "login-error", textOutput("login_msg", inline = TRUE)),
      div(class = "login-hint", "Demo access \u2014 admin / admin123")
    )
  )
)

# ------------------------------------------------------------
# Top-level gated UI
# ------------------------------------------------------------

ui <- fluidPage(
  style = "padding: 0; margin: 0;",
  uiOutput("app_ui")
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ── Authentication state ──────────────────────────────────
  authenticated <- reactiveVal(FALSE)
  
  observeEvent(input$login_submit, {
    if (identical(input$login_user, "admin") &&
        identical(input$login_pass, "admin123")) {
      authenticated(TRUE)
    } else {
      output$login_msg <- renderText("Invalid credentials. Access denied.")
    }
  })
  
  observeEvent(input$logout, {
    authenticated(FALSE)
  })
  
  # ── Render login or dashboard ─────────────────────────────
  output$app_ui <- renderUI({
    if (authenticated()) dashboard_ui else login_ui
  })
  
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
      fillcolor = "rgba(190,18,60,0.15)",
      line      = list(color = "#f43f5e", width = 2),
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
          font = list(color = "#f5f5f5", size = 15)
        ),
        xaxis = list(
          title       = "Surveillance Date",
          rangeslider = list(visible = TRUE),
          showgrid    = TRUE,
          gridcolor   = "#2a2a2a",
          color       = "#a3a3a3"
        ),
        yaxis     = list(title = "Total Cases", showgrid = TRUE, gridcolor = "#2a2a2a", color = "#a3a3a3"),
        hovermode = "x unified",
        plot_bgcolor  = "#161616",
        paper_bgcolor = "#161616",
        font          = list(color = "#a3a3a3")
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
      geom_col(fill = "#be123c", width = 0.55) +
      labs(
        title = paste("Model Comparison \u2013", selected_metric),
        x     = "Model",
        y     = selected_metric
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(face = "bold", color = "#f5f5f5"),
        plot.background  = element_rect(fill = "#161616", color = NA),
        panel.background = element_rect(fill = "#161616", color = NA),
        panel.grid.major = element_line(color = "#2a2a2a"),
        panel.grid.minor = element_blank(),
        axis.text        = element_text(color = "#a3a3a3"),
        axis.title       = element_text(color = "#a3a3a3")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        plot_bgcolor  = "#161616",
        paper_bgcolor = "#161616",
        font          = list(color = "#a3a3a3")
      )
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
      "actual_cases" = "#f5f5f5",
      "ARIMA"        = "#f59e0b",
      "XGBoost"      = "#f43f5e"
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
        line      = list(color = "#f5f5f5", width = 2),
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
        line      = list(color = "#f59e0b", width = 2, dash = "dash"),
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
        line      = list(color = "#f43f5e", width = 2, dash = "dot"),
        text      = ~paste0("Date: ", date, "<br>Series: XGBoost<br>Cases: ", round(XGBoost, 2)),
        hoverinfo = "text"
      ) %>%
      layout(
        title = list(
          text = "<b>Actual vs Predicted Weekly Influenza Cases \u2013 2024 Testing Period</b>",
          font = list(color = "#f5f5f5", size = 15)
        ),
        xaxis     = list(title = "Date",        showgrid = TRUE, gridcolor = "#2a2a2a", color = "#a3a3a3"),
        yaxis     = list(title = "Weekly Cases", showgrid = TRUE, gridcolor = "#2a2a2a", color = "#a3a3a3"),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.15, font = list(color = "#a3a3a3")),
        plot_bgcolor  = "#161616",
        paper_bgcolor = "#161616",
        font          = list(color = "#a3a3a3")
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
      geom_col(fill = "#be123c", width = 0.70) +
      coord_flip() +
      labs(
        title = "XGBoost Feature Importance Based on Gain",
        x     = "Feature",
        y     = "Gain"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(face = "bold", color = "#f5f5f5"),
        plot.background  = element_rect(fill = "#161616", color = NA),
        panel.background = element_rect(fill = "#161616", color = NA),
        panel.grid.major = element_line(color = "#2a2a2a"),
        panel.grid.minor = element_blank(),
        axis.text        = element_text(color = "#a3a3a3"),
        axis.title       = element_text(color = "#a3a3a3")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        plot_bgcolor  = "#161616",
        paper_bgcolor = "#161616",
        font          = list(color = "#a3a3a3")
      )
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
      plot_ly() %>%
        layout(
          title = list(
            text = "<b>Four-Week Ahead Influenza Forecast</b>",
            font = list(color = "#f5f5f5", size = 15)
          ),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(list(
            text      = "Forecast values are all zero.\nPlease re-run the Phase 4 XGBoost forecasting script\nto generate valid predicted_cases.",
            x         = 0.5, y = 0.5,
            xref      = "paper", yref = "paper",
            showarrow = FALSE,
            font      = list(size = 14, color = "#6b7280")
          )),
          plot_bgcolor  = "#161616",
          paper_bgcolor = "#161616"
        )
    } else {
      upper_limit <- max(forecast_data$predicted_cases, na.rm = TRUE) * 1.25
      
      plot_ly(
        forecast_data,
        x         = ~date,
        y         = ~predicted_cases,
        type      = "scatter",
        mode      = "lines+markers",
        line      = list(color = "#f43f5e", width = 2),
        marker    = list(color = "#f43f5e", size = 9),
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
            font = list(color = "#f5f5f5", size = 15)
          ),
          xaxis     = list(title = "Forecast Date",   showgrid = TRUE, gridcolor = "#2a2a2a", color = "#a3a3a3"),
          yaxis     = list(title = "Predicted Cases",  range = c(0, upper_limit),
                           showgrid = TRUE, gridcolor = "#2a2a2a", color = "#a3a3a3"),
          hovermode = "x unified",
          plot_bgcolor  = "#161616",
          paper_bgcolor = "#161616",
          font          = list(color = "#a3a3a3")
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
