#####
##### Install and load packages
#####

# The lines below are run if the packages were not yet installed.
# Otherwise,they are commented out so the code is not executed.
# install.packages(c(
#   "shiny", "leaflet", "tigris", "sf", "scales", "lubridate", "tidyverse", "DT"
# ))

# The shiny package is used to build the interactive web application.
library(shiny)
# The leaflet package is used to render the interactive choropleth map.
library(leaflet)
# The tigris package is used to download US Census shapefiles for state
# and county boundaries.
library(tigris)
# The sf package is used to handle spatial data formats returned by tigris.
library(sf)
# The scales package contains the function comma, which formats large
# numbers with comma separators for readability (e.g., 1234 becomes 1,234).
library(scales)
# The lubridate package contains date functions including floor_date, year,
# and month, which are used to round dates and extract date components.
library(lubridate)
# The tidyverse package contains functions for data wrangling and 
# generating figures, such as `ggplot`, among others.
library(tidyverse)
# The DT package provides the dataTableOutput and renderDataTable functions
# used to display the interactive summary tables.
library(DT)

# The tigris option below instructs the package to save downloaded shapefiles
# to a local cache folder. This prevents re-downloading on each app run.
options(tigris_use_cache = TRUE)

#####
##### Data loading and preparation
#####

# File paths are handled differently in developer mode vs when app is deployed.
# If users are testing changes to the app locally, developer mode is set to TRUE.
# Otherwise, if the app is deployed, developer is set to FALSE.
developer_mode <- FALSE

if (developer_mode) {
  wd_path <- getwd()
  data_path <- paste0(wd_path, "/Assignment-4/measles_county_all_updates_detailed.csv")
} else {
  data_path <- "measles_county_all_updates_detailed.csv"
}

# The data file is read into the R environment using read_csv from the readr
# package. An object called raw is created which contains the imported data.
raw <- read.csv(data_path, header=TRUE) %>%
  mutate(
    # The date column is converted from a character string to an R Date object
    # so that date-based filtering and arithmetic can be performed.
    date = as.Date(date),
    
    # The location_id column is zero-padded to 5 digits to match the format
    # of US Census FIPS codes used by the tigris shapefiles.
    # For example, 8001 becomes "08001" for Adams County, Colorado.
    fips = sprintf("%05d", as.integer(location_id)),
    
    # The state name is extracted from the location_name column, which is
    # formatted as "County, State". The sub function removes everything up to
    # and including the comma and space. trimws removes any remaining whitespace.
    state = trimws(sub(".*,\\s*", "", location_name)),
    
    # The county name is extracted from the location_name column by removing
    # everything from the comma onward.
    county = trimws(sub(",.*", "", location_name)),
    
    # The value column is explicitly converted to numeric to ensure case counts
    # are stored as numbers and not character strings.
    value = as.numeric(value),
    
    # Each date is rounded down to the start of its week using floor_date.
    # For example, a Wednesday date becomes the preceding Sunday.
    # This column is used for grouping in the weekly case count plot.
    week = floor_date(date, "week")
  )

# A named character vector of outcome types is created for use in the
# case type dropdown input. The left side of each pair is the label shown
# to the user, and the right side is the value stored in the data column.
outcome_choices <- c(
  "All cases (lab-confirmed)" = "case_lab-confirmed",
  "Imported cases"            = "case_imported",
  "Local transmission"        = "case_local",
  "Unvaccinated"              = "case_unvaccinated",
  "Vaccinated"                = "case_vaccinated"
)

# The earliest and latest dates in the dataset are stored as a two-element
# vector. These values are used to set the minimum, maximum, and default
# values of the date range input in the user interface.
date_range <- range(raw$date, na.rm = TRUE)

# The unique calendar years present in the dataset are extracted and sorted.
# This vector is used to build a color palette for the cumulative plot
# that scales automatically if the data spans more than two years.
data_years <- sort(unique(year(raw$date)))

#####
##### Plain (non-reactive) helper functions
#####

### Color palette

# A color palette function for the map is defined. It accepts a vector of
# case count values and returns a leaflet-compatible color scale ranging
# from light gray at zero cases to dark red at the maximum case count.
# The na.color argument specifies the color for counties or states with
# no matching data after the join.
make_pal <- function(values) {
  colorNumeric(
    palette  = colorRampPalette(c("#F5F5F5", "#FADADD", "#F4A0A0",
                                  "#E05555", "#A61C1C"))(256),
    domain   = c(0, max(values, 1)),
    na.color = "#cccccc"
  )
}

# A named color vector for the cumulative plot is created using
# colorRampPalette, which interpolates between two red shades to produce
# one color per year. The setNames function pairs each color with its
# corresponding year string so that scale_color_manual in ggplot2 can
# match colors to the correct year lines.
year_colors <- setNames(
  colorRampPalette(c("#E05555", "#4A0A0A"))(length(data_years)),
  as.character(data_years)
)

### Data filter

# This function applies the date range and optional state filter to any 
# data frame that has "date" and "state" columns.  It is called inside the 
# reactives below.
# Inputs include:
#   df         — the data frame to filter (usually raw or a subset of it)
#   date_start — earliest date to keep  (from input$date_range[1])
#   date_end   — latest date to keep    (from input$date_range[2])
#   states     — character vector of selected state names, or an empty
#                vector (length 0) meaning "keep all states"
filter_by_date_state <- function(df, date_start, date_end, states) {
  # Step 1: always apply the date range filter
  out <- df %>%
    filter(date >= date_start, date <= date_end)
  # Step 2: only apply the state filter when specific states were chosen.
  # If states is empty (length 0), every state is retained.
  if (length(states) > 0) {
    out <- out %>% filter(state %in% states)
  }
  out
}

### Table setup

# This function guarantees a set of required column names exist in a data 
# frame. It is called after pivot_wider in any block that needs case_imported, 
# case_local, etc., because pivot_wider only creates columns that are present 
# in the data. If a filtered subset happens to have no rows for a given outcome 
# type, that outcome column would be missing entirely — adding it as a zero
# column prevents downstream division errors.
# Inputs include:
#   df   — the data frame to check and modify
#   cols — a character vector of column names that must exist
# It returns the data frame with any missing columns added as zero.
ensure_outcome_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- 0
  }
  df
}

# This function adds Prop Imported, Prop Local, and IRR columns to any 
# data frame that already has case_imported, case_local, case_unvaccinated, 
# and case_vaccinated columns (i.e., after pivot_wider). NaN and Inf are
# replaced with NA so the table displays a dash instead of a number.
add_proportion_cols <- function(df) {
  df %>%
    mutate(
      `Prop Imported` = round(case_imported / (case_imported + case_local), 3),
      `Prop Imported` = ifelse(is.nan(`Prop Imported`), NA, `Prop Imported`),
      `Prop Local`    = round(case_local    / (case_imported + case_local), 3),
      `Prop Local`    = ifelse(is.nan(`Prop Local`),    NA, `Prop Local`),
      `IRR`           = round(case_unvaccinated / case_vaccinated, 3),
      `IRR`           = ifelse(is.nan(`IRR`) | is.infinite(`IRR`), NA, `IRR`)
    )
}

# This function builds the Part B proportion/rate data frame used by both 
# summary reactives. It filters raw to the selected date range and states, 
# then pivots outcome types wide and computes proportion and IRR columns.
# group_cols — character vector of grouping columns before outcome_type,
#              e.g. "state" for state-level or c("state", "county") for
#              county-level. The same vector is used in the final select
#              so the returned data frame has exactly those ID columns
#              plus Prop Imported, Prop Local, and IRR.
build_outcome_wide <- function(raw, date_start, date_end, states, group_cols) {
  filter_by_date_state(raw, date_start, date_end, states) %>%
    group_by(across(all_of(c(group_cols, "outcome_type")))) %>%
    summarize(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from  = outcome_type, values_from = total,
                values_fill = 0) %>%
    ensure_outcome_cols(c("case_imported", "case_local",
                          "case_unvaccinated", "case_vaccinated")) %>%
    add_proportion_cols() %>%
    select(all_of(group_cols), `Prop Imported`, `Prop Local`, `IRR`)
}

### Correlation plot and table

# This function is for generating the scatter plots. 
# Inputs include:
#   df    — the county-level data frame from corr_data()
#   x_col — name of the column to plot on the x-axis (as a string)
#   y_col — name of the column to plot on the y-axis (as a string)
#   x_lab — human-readable x-axis label shown on the plot
#   y_lab — human-readable y-axis label shown on the plot
# It returns a ggplot object that can be rendered directly by renderPlot.
build_corr_plot <- function(df, x_col, y_col, x_lab, y_lab) {
  ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    # Each point represents one county. alpha = 0.5 adds transparency
    # so overlapping points (many counties with zero cases) are visible.
    geom_point(color = "#A61C1C", alpha = 0.5, size = 1.8) +
    # geom_smooth adds a linear trend line with a shaded 95% confidence
    # interval. method = "lm" fits a straight line. se = TRUE draws the
    # confidence band. The line color matches the app's red palette.
    geom_smooth(method = "lm", se = TRUE,
                color = "#4A0A0A", fill = "#FADADD", linewidth = 0.9) +
    # comma from the scales package formats axis tick labels with commas
    # (e.g., 1000 becomes "1,000"), matching the style of other plots.
    scale_x_continuous(labels = comma,
                        expand = expansion(mult = c(0.02, 0.05))) +
    scale_y_continuous(labels = comma,
                        expand = expansion(mult = c(0.02, 0.1))) +
    labs(x = x_lab, y = y_lab) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major   = element_line(color = "#eeeeee"),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(size = 11),
      axis.text.y        = element_text(size = 11),
      axis.title.x       = element_text(size = 11, margin = margin(t = 8)),
      axis.title.y       = element_text(size = 11, margin = margin(r = 8)),
      plot.margin        = margin(10, 15, 10, 50)
    )
}

# This function runs cor.test with method = "spearman" on two numeric 
# vectors and returns a one-row data frame with three columns:
#   Correlation (rho) — the Spearman rank correlation coefficient
#   P-value           — probability of observing this rho by chance
#   Sample Size (n)   — number of counties used in the test
# exact = FALSE suppresses a warning about ties that occurs when many
# counties share the same count (e.g., zero), which is common here.
build_corr_table <- function(x, y) {
  ct <- cor.test(x, y, method = "spearman", exact = FALSE)
  data.frame(
    `Correlation (rho)` = round(ct$estimate, 3),
    `P-value`           = ifelse(ct$p.value < 0.001,
                                  "< 0.001",
                                  as.character(round(ct$p.value, 3))),
    `Sample Size (n)`   = length(x),
    # This preserves spaces and parentheses in col names
    check.names = FALSE   
  )
}

#####
##### User interface
#####

# The user interface is defined using fluidPage, which creates a responsive
# page layout. The interface is divided into a sidebar panel containing all
# input controls and a main panel containing all output components.
ui <- fluidPage(
  titlePanel("United States Measles Surveillance Dashboard"),
  
  sidebarLayout(
    
    # The sidebar panel contains all input controls. Values selected here
    # are passed to the server using the inputId of each control.
    sidebarPanel(
      p("Although measles was declared eliminated in the United States in 
      2000 by the Centers for Disease Control and Prevention, recent years 
      have shown a concerning resurgence in reported cases. Tracking measles 
      cases improves awareness of outbreak trends and potential reemergence 
      of disease, which can aid in deploying public health initiatives to 
      prevent further spread.",
      br(),
      br()),

      ### Inputs

      # The radioButtons input allows the user to select one map level.
      # The inputId "map_level" is used in the server to read this value.
      # The choices argument defines the display labels and their
      # corresponding values returned to the server.
      radioButtons(
        inputId  = "map_level",
        label    = "Map level",
        choices  = c("State (aggregated)" = "state", "County" = "county"),
        selected = "state"
      ),

      # The selectInput input creates a single-select dropdown for the
      # case type. The outcome_choices vector defined above provides the
      # display labels and corresponding data values.
      selectInput(
        inputId  = "outcome",
        label    = "Case type",
        choices  = outcome_choices,
        selected = "case_lab-confirmed"
      ),

      # The dateRangeInput input creates a start and end date selector.
      # The min and max arguments set the earliest and latest selectable
      # dates based on the range of dates present in the data.
      dateRangeInput(
        inputId = "date_range",
        label   = "Date range",
        min     = date_range[1],
        max     = date_range[2],
        start   = date_range[1],
        end     = date_range[2]
      ),

      # The selectizeInput input creates a multi-select dropdown for states.
      # The option "All States" is assigned the sentinel value "ALL" and is
      # selected by default. A JavaScript callback defined in the options
      # argument enforces mutual exclusivity: selecting "All States" clears
      # all other selections, and selecting any real state removes "All States".
      # The remove_button plugin adds an X tag to each selected item.
      selectizeInput(
        inputId  = "state_filter",
        label    = "Filter to state(s)",
        choices  = c("All States" = "ALL", sort(unique(raw$state))),
        selected = "ALL",
        multiple = TRUE,
        options  = list(
          plugins = list("remove_button"),
          onItemAdd = I("function(value, item) {
            if (value === 'ALL') {
              var current = this.getValue();
              var filtered = current.filter(function(v) { return v === 'ALL'; });
              this.setValue(filtered, true);
            } else {
              var current = this.getValue();
              var filtered = current.filter(function(v) { return v !== 'ALL'; });
              this.setValue(filtered, true);
            }
          }")
        )
      ),

      # The sliderInput below allows the user choose how many states appear
      # in the top locations table on the Overview tab and the State Summary
      # and County Summary tables. The value is read as input$top_n_states
      # in the server. The range is 1 to 10, defaulting to 5.
      sliderInput(
        inputId = "top_n_states",
        label   = "Top number of states",
        min     = 1,
        max     = 10,
        value   = 5,
        step    = 1
      ),

      # The sliderInput below allows the user choose how many counties per
      # state appear in the County Summary table. The value is read as
      # input$top_n_counties in the server. The range is 1 to 5,
      # defaulting to 3.
      sliderInput(
        inputId = "top_n_counties",
        label   = "Top number of counties (per state)",
        min     = 1,
        max     = 5,
        value   = 3,
        step    = 1
      ),

      br(),

      # The actionButton input creates a button labeled Update. All filtered
      # data reactives in the server are gated on this button using bindEvent,
      # so outputs only update when the button is clicked.
      actionButton(
        inputId = "update",
        label   = "Update",
        width   = "100%",
        class   = "btn-danger"
      ),

      p(br(),
        br(),
        "Data source:",
        a(href="https://github.com/CSSEGISandData/measles_data", "CSSEGISandData"),
        " from Johns Hopkins University Measles Tracking Team - International Vaccine 
        Access Center (IVAC), Bloomberg School of Public Health; Center for 
        Systems Science and Engineering (CSSE), Whiting School of Engineering; 
        Bloomberg Center for Government Excellence, Johns Hopkins University"), 

    ),
    
    # The main panel contains all output components. Each output function
    # below is paired with a corresponding render function in the server.
    mainPanel(

      # Four summary boxes are arranged in a single row using fluidRow and
      # column. Each box displays a label and a text output value.
      # These boxes sit above the tabs so they are always visible.
      fluidRow(
        column(3, div(class = "summary-box",
                      div(class = "summary-label", "Total Cases"),
                      div(class = "summary-value", textOutput("box_total"))
        )),
        column(3, div(class = "summary-box",
                      div(class = "summary-label", "Date Range"),
                      div(class = "summary-value summary-value-sm", textOutput("box_dates"))
        )),
        column(3, div(class = "summary-box",
                      div(class = "summary-label", "State Count"),
                      div(class = "summary-value", textOutput("box_states"))
        )),
        column(3, div(class = "summary-box",
                      div(class = "summary-label", "County Count"),
                      div(class = "summary-value", textOutput("box_counties"))
        ))
      ),

      # The tags$style function injects CSS into the page to style the
      # summary boxes defined above. Styles are applied by class name.
      tags$style("
        .summary-box {
          background: #f8f8f8;
          border: 1px solid #e0e0e0;
          border-radius: 8px;
          padding: 14px 16px;
          margin-bottom: 16px;
          text-align: center;
        }
        .summary-label {
          font-size: 12px;
          color: #888;
          text-transform: uppercase;
          letter-spacing: 0.05em;
          margin-bottom: 4px;
        }
        .summary-value {
          font-size: 26px;
          font-weight: 600;
          color: #A61C1C;
        }
        .summary-value-sm {
          font-size: 15px;
          padding-top: 5px;
          color: #333;
        }
      "),

      ### Outputs

      # The tabsetPanel function creates a tab layout. Each tabPanel
      # defines one tab. Tabs share the sidebar filter controls above.
      tabsetPanel(

        # The Overview tab contains the map, case count figures, and top locations table.
        tabPanel(
          "Overview",
          br(),

          # The leafletOutput function declares the map output component.
          # The outputId "map" is used in the server to render and update
          # the map. Width is set to 100% and height to 480 pixels.
          leafletOutput("map", width = "100%", height = "480px"),
          br(),

          # The plotOutput function declares the weekly case count plot.
          h4("Weekly case counts"),
          plotOutput("line_plot", height = "260px"),
          br(),

        # The plotOutput function declares the cumulative cases by year chart.
          h4("Cumulative cases by year"),
          plotOutput("cumulative_plot", height = "260px"),
          br(),

          # The tableOutput function declares the top locations table.
          h4("Top locations"),
          tableOutput("top_table")
          ),

        # The State Summary tab contains the per-state statistics table.
        tabPanel(
          "State Summary",
          br(),

          # A brief description for the table.
          p(" Below is a table of summary statistics for the top states 
            by total case count, based on the filter settings for case type, 
            date range, and selected states. The number of states shown is 
            determined by the top number of states slider in the sidebar 
            filter settings.", 
            br(),
            br(),

            "The descriptive statistics columns (total count, range [minimum, 
            maximum], mean, standard deviation, median, interquartile range, 
            number of affected counties) are based on the case type selected. 
            The statistics are calculated over counties within a given state.",
            br(),
            br(),

            "The proportion and rate columns (proportion of imported cases, 
            proportion of local cases, incidence rate ratio) are based on all 
            case types. The proportion of imported cases is calculated as the 
            number of imported cases divided by the sum of imported and local 
            cases. The proportion of local cases is calculated as the number of 
            imported cases divided by the sum of imported and local cases. The 
            incidence rate ratio (IRR) is calculated as the ratio of unvaccinated 
            cases to vaccinated cases."),

          # The dataTableOutput function declares the interactive summary
          # table. The outputId "state_summary_table" is matched to a
          # renderDataTable call in the server.
          dataTableOutput("state_summary_table")
        ),

        # The County Summary tab contains the per-county statistics table.
        tabPanel(
          "County Summary",
          br(),

          # A brief description for the table.
          p("Below is a table of summary statistics for the top counties 
            by total case count within each of the top states, based on the 
            filter settings for case type, date range, and selected states. 
            The number of states shown is based on the top number of states 
            slider, and the number of counties per state shown is based on 
            the top number of counties (per state) slider, both of which are in 
            the sidebar filter settings.",
            br(),
            br(),
          
            "The descriptive statistics columns (total count, range [minimum, 
            maximum], mean, standard deviation, median, interquartile range, 
            number of affected counties) are based on the case type selected. 
            The statistics are calculated over reporting days for a given county.",
            br(),
            br(),
          
            "The proportion and rate columns (proportion of imported cases, 
            proportion of local cases, incidence rate ratio) are based on all 
            case types. The proportion of imported cases is calculated as the 
            number of imported cases divided by the sum of imported and local 
            cases. The proportion of local cases is calculated as the number of 
            imported cases divided by the sum of imported and local cases. The 
            incidence rate ratio (IRR) is calculated as the ratio of unvaccinated 
            cases to vaccinated cases."),

          # The dataTableOutput function declares the interactive county
          # summary table. The outputId "county_summary_table" is matched
          # to a renderDataTable call in the server.
          dataTableOutput("county_summary_table")
        ),

        # The Correlation Analysis tab has Spearman correlation figures and tables.
        tabPanel(
          "Correlation Analysis",
          br(),

          # A brief description explaining what the tab shows and how the
          # unit of observation (county-level totals) is derived.
          # Both correlations use the same date and state filters as the
          # other tabs. The Case Type filter not used, as specific case types 
          # are specified per figure instead (imported, local, unvaccinated).
          p("Spearman rank correlations between the specified case types, 
            computed at the county level using total counts over the selected 
            date range and state filter. Note that case types are fixed by the 
            correlation being examined. A scatter plot with a linear trend line 
            is shown in each of the figures below with shading as the 95% 
            confidence interval."),

          # The plotOutput function declares the scatter plot for the
          # imported vs. local correlation.
          h4("Figure 1: Imported cases vs. local transmission cases"),
          plotOutput("corr_plot_imp_local", height = "340px"),
          br(),

          # The tableOutput function declares the summary statistics table
          # (rho, p-value, n) for the imported vs. local correlation.
          tableOutput("corr_table_imp_local"),
          br(),

          # The plotOutput function declares the scatter plot for the
          # unvaccinated vs. local correlation.
          h4("Figure 2: Unvaccinated cases vs. local transmission cases"),
          plotOutput("corr_plot_unvacc_local", height = "340px"),
          br(),

          # The tableOutput function declares the summary statistics table
          # (rho, p-value, n) for the unvaccinated vs. local correlation.
          tableOutput("corr_table_unvacc_local")
        )
      )
    )
  )
)

#####
##### Server
#####

# The server function contains all reactive logic. It takes input and output
# as arguments. Input values are read using input$inputId and output values
# are assigned using output$outputId.
server <- function(input, output){

  #####
  ##### Reactives
  #####
  
  ### Helper reactives
  
  # A helper reactive called sel_states is defined to return the vector of
  # selected state names with the sentinel value "ALL" removed. This reactive
  # is called by multiple other reactives below, so defining it here avoids
  # repeating it.
  # The reactive returns a character vector of state names, or an empty
  # character vector if "All States" is selected.
  sel_states <- reactive({
    input$state_filter[input$state_filter != "ALL"]
  })

  # A helper reactive called plot_subtitle builds the subtitle string
  # used on both the weekly case count and cumulative cases plots. It is
  # defined once here so the same logic does not need to be repeated inside
  # each renderPlot call below.
  # Logic:
  #   - If no specific states are selected, return "All states".
  #   - If 1–3 states are selected, list their names after a colon.
  #   - If 4 or more states are selected, show only the count to save space.
  plot_subtitle <- reactive({
    sel <- sel_states()
    if (length(sel) == 0) {
      "All states"
    } else if (length(sel) <= 3) {
      paste("Filtered to:", paste(sel, collapse = ", "))
    } else {
      paste0("Filtered to ", length(sel), " states")
    }
  })

  ### Filtered data reactives
  
  # The filtered_raw reactive applies the outcome type, date range, and state
  # filters to the raw dataset. It returns a filtered data frame and serves
  # as the input to all downstream summary reactives.
  # The bindEvent function gates execution on input$update, so this reactive
  # only re-runs when the Update button is clicked. The ignoreNULL = FALSE
  # argument allows the reactive to run once on startup before any click.
  filtered_raw <- reactive({
    # Apply the date and state filters using the helper, then
    # keep only the outcome type the user selected in the dropdown.
    filter_by_date_state(raw,
                         date_start = input$date_range[1],
                         date_end   = input$date_range[2],
                         states     = sel_states()) %>%
      filter(outcome_type == input$outcome)
  }) %>% bindEvent(input$update, ignoreNULL = FALSE)

  # The filtered_county reactive groups the filtered data by county and
  # computes the total case count per county. The output is used for
  # the county-level map, the county top locations table, and the
  # county count summary box.
  filtered_county <- reactive({
    filtered_raw() %>%
      group_by(fips, state, county) %>%
      summarize(cases = sum(value, na.rm = TRUE), .groups = "drop")
  }) %>% bindEvent(input$update, ignoreNULL = FALSE)

  # The filtered_state reactive groups the filtered data by state and
  # computes the total case count per state. The output is used for
  # the state-level map, the state top locations table, and the
  # state count summary box.
  filtered_state <- reactive({
    filtered_raw() %>%
      group_by(state) %>%
      summarize(cases = sum(value, na.rm = TRUE), .groups = "drop")
  }) %>% bindEvent(input$update, ignoreNULL = FALSE)

  # The weekly_cases reactive groups the filtered data by week and computes
  # the total case count per week. The output is used for the weekly line
  # plot. The data is sorted chronologically so the line draws from left
  # to right.
  weekly_cases <- reactive({
    filtered_raw() %>%
      group_by(week) %>%
      summarize(cases = sum(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(week)
  }) %>% bindEvent(input$update, ignoreNULL = FALSE)

  # The cumulative_cases reactive groups the filtered data by year and month
  # and computes the total case count per year-month combination. Within
  # each year, the function cumsum computes a running total of cases so that
  # each month's value reflects all cases from January of that year through
  # the current month. The year column is converted to a factor so that
  # ggplot2 assigns a distinct color to each year rather than treating
  # year as a continuous numeric variable.
  cumulative_cases <- reactive({
    filtered_raw() %>%
      mutate(
        year  = year(date),
        month = month(date)
      ) %>%
      group_by(year, month) %>%
      summarize(cases = sum(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(year, month) %>%
      group_by(year) %>%
      mutate(cumulative = cumsum(cases)) %>%
      ungroup() %>%
      mutate(year = as.factor(year))
  }) %>% bindEvent(input$update, ignoreNULL = FALSE)

  ### Geographical data reactives
  
  # The state_geo reactive downloads state boundary shapefiles using the
  # tigris function states and joins the filtered state case counts onto
  # the shapefile by matching the Census NAME column to the state column
  # in the case data. States with no matching case data are assigned zero.
  # If specific states are selected, unselected states are assigned NA so
  # they render in the na.color gray defined in make_pal.
  state_geo <- reactive({
    sel <- sel_states()
    geo <- states(cb = TRUE, resolution = "20m", year = 2023) %>%
      filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "VI", "MP", "AS")) %>%
      left_join(filtered_state(), by = c("NAME" = "state")) %>%
      mutate(cases = replace_na(cases, 0))
    if (length(sel) > 0) {
      geo <- geo %>%
        mutate(cases = ifelse(NAME %in% sel, cases, NA_real_))
    }
    geo
  })
  
  # The county_geo reactive downloads county boundary shapefiles using the
  # tigris function counties. If specific states are selected, only counties
  # belonging to those states are downloaded by passing the corresponding
  # two-digit FIPS state codes to the state argument of counties. The
  # filtered county case counts are then joined onto the shapefile using
  # the five-digit FIPS code. Counties with no matching case data are
  # assigned zero.
  county_geo <- reactive({
    sel <- sel_states()
    if (length(sel) == 0) {
      geo <- counties(cb = TRUE, resolution = "20m", year = 2023) %>%
        filter(!STATEFP %in% c("02", "15", "72", "66", "78", "60", "69"))
    } else {
      fp  <- fips_codes %>%
        filter(state_name %in% sel) %>%
        pull(state_code) %>%
        unique()
      geo <- counties(state = fp, cb = TRUE, year = 2023)
    }
    geo %>%
      left_join(filtered_county(), by = c("GEOID" = "fips")) %>%
      mutate(
        cases = replace_na(cases, 0),
        # STATE_NAME from tigris is always populated; state from the join
        # is NA for any county with zero cases. coalesce picks the first non-NA.
        state = coalesce(state, STATE_NAME)
      )
  })

  ### State summary table reactive

  state_summary_data <- reactive({

    # Part A: Descriptive statistics for the selected outcome type.
    # Summarize at county level first (one row per county), then roll up
    # to state level. "Mean" addresses the question: on average, how many 
    # cases did an affected county in this state report?
    county_level <- filtered_raw() %>%
      group_by(state, fips, county) %>%
      summarize(cases = sum(value, na.rm = TRUE), .groups = "drop")

    stats_a <- county_level %>%
      group_by(state) %>%
      summarize(
        `Total Count`       = sum(cases,    na.rm = TRUE),
        .min                = min(cases,    na.rm = TRUE),
        .max                = max(cases,    na.rm = TRUE),
        Mean                = round(mean(cases,   na.rm = TRUE), 2),
        `Std Dev`           = round(sd(cases,     na.rm = TRUE), 2),
        Median              = round(median(cases, na.rm = TRUE), 2),
        IQR                 = round(IQR(cases,    na.rm = TRUE), 2),
        `Affected Counties` = sum(cases > 0, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Range = paste0("[", .min, ", ", .max, "]")) %>%
      select(-c(.min, .max))

    # Part B: Proportion and rate columns (all outcome types).
    # build_outcome_wide applies only date and state filters, pivots outcome
    # types wide, and computes Prop Imported, Prop Local, and IRR.
    stats_b <- build_outcome_wide(raw,
                                  date_start = input$date_range[1],
                                  date_end   = input$date_range[2],
                                  states     = sel_states(),
                                  group_cols = "state")

    # Join and keep top N states by total count
    stats_a %>%
      left_join(stats_b, by = "state") %>%
      arrange(desc(`Total Count`)) %>%
      slice_head(n = input$top_n_states) %>%
      select(State = state, `Total Count`, Range, Mean, `Std Dev`,
              Median, IQR, `Prop Imported`, `Prop Local`, `IRR`,
              `Affected Counties`)

  }) %>% bindEvent(input$update, ignoreNULL = FALSE)

  ### County summary table reactive

  county_summary_data <- reactive({

    # Part A: Descriptive statistics for the selected outcome type.
    # Each row in filtered_raw() is one county on one date. Statistics are
    # computed across reporting periods within each county. "Mean" addresses
    # the question: on an average reporting day, how many cases did this 
    # county report?
    stats_a <- filtered_raw() %>%
      group_by(state, county) %>%
      summarize(
        `Total Count`       = sum(value,    na.rm = TRUE),
        .min                = min(value,    na.rm = TRUE),
        .max                = max(value,    na.rm = TRUE),
        Mean                = round(mean(value,   na.rm = TRUE), 2),
        `Std Dev`           = round(sd(value,     na.rm = TRUE), 2),
        Median              = round(median(value, na.rm = TRUE), 2),
        IQR                 = round(IQR(value,    na.rm = TRUE), 2),
        `Affected Counties` = as.integer(sum(value, na.rm = TRUE) > 0),
        .groups = "drop"
      ) %>%
      mutate(Range = paste0("[", .min, ", ", .max, "]")) %>%
      select(-c(.min, .max))

    # Identify top N states and top N counties per state
    top_states <- stats_a %>%
      group_by(state) %>%
      summarize(state_total = sum(`Total Count`, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(desc(state_total)) %>%
      slice_head(n = input$top_n_states) %>%
      pull(state)

    stats_a <- stats_a %>%
      filter(state %in% top_states) %>%
      group_by(state) %>%
      slice_max(`Total Count`, n = input$top_n_counties, with_ties = FALSE) %>%
      ungroup()

    # Part B: Proportion and rate columns (all outcome types).
    # Same helper as the state reactive, but grouped by state and county.
    stats_b <- build_outcome_wide(raw,
                                  date_start = input$date_range[1],
                                  date_end   = input$date_range[2],
                                  states     = sel_states(),
                                  group_cols = c("state", "county"))

    # Join and enforce top-state ordering
    stats_a %>%
      left_join(stats_b, by = c("state", "county")) %>%
      mutate(state = factor(state, levels = top_states)) %>%
      arrange(state, desc(`Total Count`)) %>%
      mutate(state = as.character(state)) %>%
      select(State = state, County = county, `Total Count`, Range,
              Mean, `Std Dev`, Median, IQR, `Prop Imported`, `Prop Local`,
              `IRR`, `Affected Counties`)

  }) %>% bindEvent(input$update, ignoreNULL = FALSE)

  ### Correlation data reactive

  # The corr_data reactive prepares the county-level data used by both
  # Spearman correlation figures on the Correlation Analysis tab.
  # It is gated on input$update like all other filtered reactives so
  # all tabs update at the same time when the user clicks Update.
  
  # The unit of observation is one county. For each county we compute
  # the total case count for three specific outcome types over the
  # selected date range:
  #   case_imported    — imported cases
  #   case_local       — local transmission cases
  #   case_unvaccinated — cases in unvaccinated individuals
  
  # We do not filter by input$outcome here because the correlations
  # always compare fixed pairs of outcome types, regardless of what the
  # user has selected in the Case Type dropdown.
  corr_data <- reactive({

    # Start from raw data and apply only the date and state filters using the
    # helper function. We do not filter by outcome type here because the
    # correlations always compare fixed pairs of outcome types, regardless
    # of what the user selected in the Case Type dropdown.
    d <- filter_by_date_state(raw,
                               date_start = input$date_range[1],
                               date_end   = input$date_range[2],
                               states     = sel_states()) %>%
      filter(
        # Keep only the three outcome types needed for both correlations.
        # This reduces the data size before the group-by step below.
        outcome_type %in% c("case_imported", "case_local",
                            "case_unvaccinated")
      )

    # Aggregate to county-level totals, then pivot so each outcome type
    # becomes its own column. This gives one row per county with columns
    # case_imported, case_local, and case_unvaccinated.
    d %>%
      group_by(state, county, outcome_type) %>%
      summarize(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = outcome_type, values_from = total,
                  values_fill = 0) %>%
      # Use ensure_outcome_cols() to guarantee all three required
      # columns are present even if a narrow filter produces no rows for
      # one of the outcome types.
      ensure_outcome_cols(c("case_imported", "case_local",
                            "case_unvaccinated"))

  }) %>% bindEvent(input$update, ignoreNULL = FALSE)

  #####
  ##### Upper panel outputs: summary boxes
  #####
  
  # The total cases output sums all values in the value column of the
  # filtered raw data. The comma function from the scales package formats
  # the result with comma separators.
  output$box_total <- renderText({
    comma(sum(filtered_raw()$value, na.rm = TRUE))
  })
  
  # The date range output formats the selected start and end dates as
  # month, day, and year strings separated by an em dash.
  output$box_dates <- renderText({
    paste0(format(input$date_range[1], "%b %d, %Y"),
           " - ",
           format(input$date_range[2], "%b %d, %Y"))
  }) %>% bindEvent(input$update, ignoreNULL = FALSE)
  
  # The state count output returns the number of distinct states present
  # in the filtered state summary data.
  output$box_states <- renderText({
    n_distinct(filtered_state()$state)
  })
  
  # The county count output returns the number of counties in the filtered
  # county summary data that have at least one reported case.
  output$box_counties <- renderText({
    filtered_county() %>% filter(cases > 0) %>% nrow()
  })

  #####
  ##### Overview tab outputs
  #####

  ### Interactive map
  
  # The renderLeaflet function creates the base map once on startup with a
  # light CartoDB tile layer centered on the contiguous United States.
  # Subsequent updates to the map polygons are handled by leafletProxy,
  # which modifies the existing map in place without redrawing it.
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = -96, lat = 38, zoom = 4)
  })
  
  # The observe block updates the map polygons and legend whenever the
  # Update button is clicked or the map level radio button is changed.
  # The bindEvent function attends to both input$update and input$map_level,
  # so that switching between state and county view takes effect immediately
  # without requiring a button click.
  observe({
    if (input$map_level == "state") {
      
      # For the state map, the color palette is built from non-NA case values
      # only. Tooltip labels are constructed as HTML strings and converted
      # using htmltools::HTML so that the bold tags render correctly.
      geo  <- state_geo()
      pal  <- make_pal(geo$cases[!is.na(geo$cases)])
      labs <- sprintf("<strong>%s</strong><br/>%s cases",
                      geo$NAME,
                      ifelse(is.na(geo$cases), "Not selected",
                             comma(geo$cases))) %>%
        lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        clearShapes()   %>%
        clearControls() %>%
        addPolygons(
          data         = geo,
          fillColor    = ~pal(cases),
          fillOpacity  = 0.8,
          color        = "#AAAAAA",
          weight       = 1.5,
          highlight    = highlightOptions(weight = 2.5, color = "#ff6b35",
                                          bringToFront = TRUE),
          label        = labs,
          labelOptions = labelOptions(style = list("font-size" = "13px"))
        ) %>%
        addLegend(
          pal       = pal,
          values    = geo$cases[!is.na(geo$cases)],
          title     = "Total cases",
          position  = "bottomright",
          labFormat = labelFormat(big.mark = ",")
        )
      
    } else {
      
      # For the county map, the border weight is set to 0.4, which is thinner
      # than the state border weight of 1.5, to prevent the county grid from
      # appearing visually dense if zoomed out to view entire country.
      geo  <- county_geo()
      pal  <- make_pal(geo$cases)
      labs <- sprintf("<strong>%s, %s</strong><br/>%s cases",
                      geo$NAME, geo$state, comma(geo$cases)) %>%
        lapply(htmltools::HTML)
      
      leafletProxy("map") %>%
        clearShapes()   %>%
        clearControls() %>%
        addPolygons(
          data         = geo,
          fillColor    = ~pal(cases),
          fillOpacity  = 0.8,
          color        = "#AAAAAA",
          weight       = 0.4,
          highlight    = highlightOptions(weight = 1.5, color = "#ff6b35",
                                          bringToFront = TRUE),
          label        = labs,
          labelOptions = labelOptions(style = list("font-size" = "12px"))
        ) %>%
        addLegend(
          pal       = pal,
          values    = geo$cases,
          title     = "Total cases",
          position  = "bottomright",
          labFormat = labelFormat(big.mark = ",")
        )
    }
  }) %>% bindEvent(input$update, input$map_level, ignoreNULL = FALSE)

  ### Weekly case count plot
  
  # The renderPlot function creates the weekly case count line plot using
  # ggplot2. The x-axis displays weeks with tick marks placed at the first
  # week of each calendar month, labeled in the format "Jan '25". The
  # y-axis displays the case count formatted with comma separators.
  # A shaded area is drawn beneath the line using geom_area.
  output$line_plot <- renderPlot({
    wk <- weekly_cases()
    
    # The x-axis tick positions are set to the earliest week date falling
    # within each calendar month. The slice_min function retains only one
    # row per month group, corresponding to the earliest week in that month.
    month_breaks <- wk %>%
      mutate(month = floor_date(week, "month")) %>%
      group_by(month) %>%
      slice_min(week, n = 1) %>%
      pull(week)
    
    ggplot(wk, aes(x = week, y = cases)) +
      geom_area(fill = "#FADADD", alpha = 0.5) +
      geom_line(color = "#A61C1C", linewidth = 0.9) +
      geom_point(color = "#A61C1C", size = 1.8) +
      scale_x_date(
        breaks       = month_breaks,
        labels       = function(x) format(x, "%b '%y"),
        minor_breaks = NULL
      ) +
      scale_y_continuous(
        labels = comma,
        expand = expansion(mult = c(0, 0.1))
      ) +
      labs(x = NULL, y = "Cases", subtitle = plot_subtitle()) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(color = "#eeeeee"),
        plot.subtitle      = element_text(color = "#888888", size = 11),
        axis.text.x        = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y        = element_text(size = 11),
        plot.margin        = margin(8, 16, 8, 8)
      )
  }, res = 110)
  
  ### Cumulative cases by year chart
  
  # The renderPlot function creates the cumulative cases by year chart using
  # ggplot2. Each calendar year is drawn as a separate line. The x-axis
  # displays month abbreviations from January through December. The y-axis
  # displays the running cumulative case count from the start of each year.
  # The year_colors vector defined at startup maps each year to a red shade.
  output$cumulative_plot <- renderPlot({
    cum <- cumulative_cases()
    
    # The x-axis tick marks are set to integers 1 through 12 corresponding
    # to each calendar month. The built-in R constant month.abb provides
    # the three-letter abbreviations Jan through Dec as tick labels.
    ggplot(cum, aes(x = month, y = cumulative, color = year, group = year)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.8) +
      scale_x_continuous(
        breaks = 1:12,
        labels = month.abb,
        limits = c(1, 12)
      ) +
      scale_y_continuous(
        labels = comma,
        expand = expansion(mult = c(0, 0.1))
      ) +
      scale_color_manual(values = year_colors) +
      labs(
        x        = NULL,
        y        = "Cumulative cases",
        color    = "Year",
        subtitle = plot_subtitle()
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(color = "#eeeeee"),
        plot.subtitle      = element_text(color = "#888888", size = 11),
        axis.text.x        = element_text(size = 11),
        axis.text.y        = element_text(size = 11),
        legend.position    = "right",
        plot.margin        = margin(8, 16, 8, 8)
      )
  }, res = 110)

  ### Top locations table
  
  # The renderTable function creates the top locations table. The table
  # displays the top N locations with the highest case counts under the
  # current filter settings, where N is controlled by the Top States slider.
  # When the map level is set to state, one row per state is returned.
  # When set to county, one row per county is returned with a combined
  # location label in the format "County, State".
  # The digits argument suppresses decimal places and the format.args
  # argument adds comma separators to case count values.
  output$top_table <- renderTable({
    if (input$map_level == "state") {
      filtered_state() %>%
        arrange(desc(cases)) %>%
        # Use input$top_n_states so this table respects the slider.
        slice_head(n = input$top_n_states) %>%
        rename(State = state, `Total cases` = cases)
    } else {
      filtered_county() %>%
        arrange(desc(cases)) %>%
        # Use input$top_n_states so this table respects the slider.
        slice_head(n = input$top_n_states) %>%
        mutate(Location = paste0(county, " County, ", state)) %>%
        select(Location, `Total cases` = cases)
    }
  }, digits = 0, format.args = list(big.mark = ","))

  #####
  ##### State summary tab table output
  #####

  # The renderDataTable function creates the interactive per-state statistics
  # table on the State Summary tab. DT::renderDataTable is called explicitly
  # to use the DT package version (which supports column formatting options)
  # rather than the base Shiny version.
  output$state_summary_table <- DT::renderDataTable({

    df <- state_summary_data()

    # The datatable function from DT creates an interactive HTML table with
    # built-in search, sort, and pagination controls. The options argument
    # suppresses the row count selector (pageLength = 10 shows all rows
    # since we cap at 10 states) and centers the table on the page.
    DT::datatable(
      df,
      rownames = FALSE,
      options  = list(
        pageLength = 10,     # Show all rows without pagination controls
        dom        = "t",    # "t" means show only the table, no search bar
        scrollX    = TRUE    # Allow horizontal scroll for wide tables
      )
    ) %>%
      # Format Total Count and Affected Counties as integers with commas.
      DT::formatCurrency(
        columns  = c("Total Count", "Affected Counties"),
        currency = "",
        digits   = 0,
        mark     = ","
      ) %>%
      # Format Mean, Std Dev, Median, and IQR to two decimal places.
      DT::formatRound(
        columns = c("Mean", "Std Dev", "Median", "IQR"),
        digits  = 2
      ) %>%
      # Format proportion and IRR columns to three decimal places.
      # NA values (e.g., division by zero) will display as a dash.
      DT::formatRound(
        columns = c("Prop Imported", "Prop Local", "IRR"),
        digits  = 3
      )
  })

  #####
  ##### County summary tab table output
  #####

  # The renderDataTable function creates the interactive per-county statistics
  # table on the County Summary tab. DT::renderDataTable is called explicitly
  # to use the DT package version (which supports column formatting options)
  # rather than the base Shiny version.
  output$county_summary_table <- DT::renderDataTable({

    df <- county_summary_data()

    # The datatable function creates an interactive HTML table with built-in
    # sort and scroll controls. pageLength is set to 50 (the maximum possible
    # rows: 10 states × 5 counties) so all rows are always visible without
    # pagination, regardless of the current slider settings.
    # dom = "t" hides the search bar so the table stays compact.
    # scrollX allows horizontal scrolling on narrow screens.
    DT::datatable(
      df,
      rownames = FALSE,
      options  = list(
        pageLength = 50,     # Show all rows (up to 10 states × 5 counties)
        dom        = "t",    # "t" means show only the table, no search bar
        scrollX    = TRUE    # Allow horizontal scroll for wide tables
      )
    ) %>%
      # Format Total Count and Affected Counties as integers with commas.
      DT::formatCurrency(
        columns  = c("Total Count", "Affected Counties"),
        currency = "",
        digits   = 0,
        mark     = ","
      ) %>%
      # Format Mean, Std Dev, Median, and IQR to two decimal places.
      DT::formatRound(
        columns = c("Mean", "Std Dev", "Median", "IQR"),
        digits  = 2
      ) %>%
      # Format proportion and IRR columns to three decimal places.
      # NA values (e.g., division by zero) will display as a dash.
      DT::formatRound(
        columns = c("Prop Imported", "Prop Local", "IRR"),
        digits  = 3
      )
  })
 
  #####
  ##### Correlation analysis tab outputs: plots and tables
  #####

  # Figure 1: Imported cases vs. Local transmission.
  # The renderPlot function builds the scatter plot for Figure 1.
  # The entire body is wrapped in tryCatch so that if the filtered data
  # lacks sufficient variance (e.g., all-zero columns after a narrow filter),
  # the plot area shows a readable message instead of a red error screen.
  output$corr_plot_imp_local <- renderPlot({
    tryCatch({
      df <- corr_data()
      build_corr_plot(
        df    = df,
        x_col = "case_imported",
        y_col = "case_local",
        x_lab = "Imported cases \n(county total)",
        y_lab = "Local transmission cases \n(county total)"
      )
    }, error = function(e) {
      # Display a plain-text message if the plot cannot be generated.
      # This happens when there is not enough data variation after filtering.
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("Insufficient data to generate this figure.",
                               "\nTry expanding the date range or state filter."),
                 size = 5, color = "#888888", hjust = 0.5, vjust = 0.5) +
        theme_void()
    })
  }, res = 110)

  # The renderTable function builds the summary statistics table for
  # Figure 1. digits = 3 limits decimal places for numeric columns.
  output$corr_table_imp_local <- renderTable({
    df <- corr_data()
    build_corr_table(df$case_imported, df$case_local)
  }, digits = 3)

  # Figure 2: Unvaccinated cases vs. Local transmission.
  # The renderPlot function builds the scatter plot for Figure 2.
  # The entire body is wrapped in tryCatch for the same reason as Figure 1.
  output$corr_plot_unvacc_local <- renderPlot({
    tryCatch({
      df <- corr_data()
      build_corr_plot(
        df    = df,
        x_col = "case_unvaccinated",
        y_col = "case_local",
        x_lab = "Unvaccinated cases \n(county total)",
        y_lab = "Local transmission cases \n(county total)"
      )
    }, error = function(e) {
      # Display a plain-text message if the plot cannot be generated.
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("Insufficient data to generate this figure.",
                               "\nTry expanding the date range or state filter."),
                 size = 5, color = "#888888", hjust = 0.5, vjust = 0.5) +
        theme_void()
    })
  }, res = 110)

  # The renderTable function builds the summary statistics table for
  # Figure 2.
  output$corr_table_unvacc_local <- renderTable({
    df <- corr_data()
    build_corr_table(df$case_unvaccinated, df$case_local)
  }, digits = 3)
}

#####
##### Launch the shiny app
#####

# The shinyApp function connects the ui and server objects defined
# above and launches the application.
shinyApp(ui = ui, server = server)
