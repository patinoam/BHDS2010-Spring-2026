#####
##### Install and load packages
#####

# The lines below are run if the packages were not yet installed.
# Otherwise,they are commented out so the code is not executed.
# install.packages(c(
#   "shiny", "leaflet", "tigris", "sf", "scales", "lubridate", "tidyverse"
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

#####
##### Data loading and preparation
#####

# File paths are handled differently in development mode vs when deployed.
development_mode <- TRUE

if (development_mode) {
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
    # This column is used for grouping in the weekly case count chart.
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
# This vector is used to build a color palette for the cumulative chart
# that scales automatically if the data spans more than two years.
data_years <- sort(unique(year(raw$date)))

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

# A named color vector for the cumulative chart is created using
# colorRampPalette, which interpolates between two red shades to produce
# one color per year. The setNames function pairs each color with its
# corresponding year string so that scale_color_manual in ggplot2 can
# match colors to the correct year lines.
year_colors <- setNames(
  colorRampPalette(c("#E05555", "#4A0A0A"))(length(data_years)),
  as.character(data_years)
)

#####
##### User interface
#####

# The user interface is defined using fluidPage, which creates a responsive
# page layout. The interface is divided into a sidebar panel containing all
# input controls and a main panel containing all output components.
ui <- fluidPage(
  titlePanel("United States Measles Caseload Dashboard"),
  
  sidebarLayout(
    
    # The sidebar panel contains all input controls. Values selected here
    # are passed to the server using the inputId of each control.
    sidebarPanel(

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

      br(),

      # The actionButton input creates a button labeled Update. All filtered
      # data reactives in the server are gated on this button using bindEvent,
      # so outputs only update when the button is clicked.
      actionButton(
        inputId = "update",
        label   = "Update",
        width   = "100%",
        class   = "btn-danger"
      )
    ),
    
    # The main panel contains all output components. Each output function
    # below is paired with a corresponding render function in the server.
    mainPanel(

      # Four summary boxes are arranged in a single row using fluidRow and
      # column. Each box displays a label and a text output value.
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

      # The tableOutput function declares the top locations table.
      h4("Top locations"),
      tableOutput("top_table")

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
  ##### Helper reactive
  #####
  
  # A helper reactive called sel_states is defined to return the vector of
  # selected state names with the sentinel value "ALL" removed. This reactive
  # is called by multiple other reactives below, so defining it here avoids
  # repeating it.
  # The reactive returns a character vector of state names, or an empty
  # character vector if "All States" is selected.
  sel_states <- reactive({
    input$state_filter[input$state_filter != "ALL"]
  })

  #####
  ##### Filtered data reactives
  #####
  
  # The filtered_raw reactive applies the outcome type, date range, and state
  # filters to the raw dataset. It returns a filtered data frame and serves
  # as the input to all downstream summary reactives.
  # The bindEvent function gates execution on input$update, so this reactive
  # only re-runs when the Update button is clicked. The ignoreNULL = FALSE
  # argument allows the reactive to run once on startup before any click.
  filtered_raw <- reactive({
    d <- raw %>%
      filter(
        outcome_type == input$outcome,
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
    # If one or more specific states are selected, the data is further
    # filtered to include only rows matching those states.
    if (length(sel_states()) > 0) {
      d <- d %>% filter(state %in% sel_states())
    }
    d
  }) %>% bindEvent(input$update, ignoreNULL = FALSE)

  # The filtered_county reactive groups the filtered data by county and
  # computes the total case count per county. The output is used for
  # the county-level map, the county top locations table, and the
  # county count summary box.
  filtered_county <- reactive({
    filtered_raw() %>%
      group_by(fips, state, county) %>%
      summarise(cases = sum(value, na.rm = TRUE), .groups = "drop")
  }) %>% bindEvent(input$update, ignoreNULL = FALSE)

  # The filtered_state reactive groups the filtered data by state and
  # computes the total case count per state. The output is used for
  # the state-level map, the state top locations table, and the
  # state count summary box.
  filtered_state <- reactive({
    filtered_raw() %>%
      group_by(state) %>%
      summarise(cases = sum(value, na.rm = TRUE), .groups = "drop")
  }) %>% bindEvent(input$update, ignoreNULL = FALSE)

  #####
  ##### Top locations table
  #####
  
  # The renderTable function creates the top locations table. The table
  # displays the ten locations with the highest case counts under the
  # current filter settings. When the map level is set to state, one row
  # per state is returned. When set to county, one row per county is
  # returned with a combined location label in the format
  # "County, State". The digits argument suppresses decimal places and the
  # format.args argument adds comma separators to case count values.
  output$top_table <- renderTable({
    if (input$map_level == "state") {
      filtered_state() %>%
        arrange(desc(cases)) %>%
        slice_head(n = 10) %>%
        rename(State = state, `Total cases` = cases)
    } else {
      filtered_county() %>%
        arrange(desc(cases)) %>%
        slice_head(n = 10) %>%
        mutate(Location = paste0(county, " County, ", state)) %>%
        select(Location, `Total cases` = cases)
    }
  }, digits = 0, format.args = list(big.mark = ","))
 
}

#####
##### Launch the shiny app
#####

# The shinyApp function connects the ui and server objects defined
# above and launches the application.
# shinyApp(ui = ui, server = server)