if (!require("shiny")) {install.packages("shiny")}; (library("shiny")) # For the user interface
if (!require("shinyWidgets")) {install.packages("shinyWidgets")}; library("shinyWidgets")
if (!require("shinydashboard")) {install.packages("shinydashboard")}; library("shinydashboard")
if (!require("tidyverse")) {install.packages("tidyverse")}; library("tidyverse") # Data processing
if (!require("magrittr")) {install.packages("magrittr")}; library("magrittr")
if (!require("accelerometry")) {install.packages("accelerometry")}; library("accelerometry")
if (!require("lfstat")) {install.packages("lfstat")}; library("lfstat")
if (!require("xts")) {install.packages("xts")}; library("xts") # For time-series data processing
if (!require("lubridate")) {install.packages("lubridate")}; library("lubridate")
if (!require("Hmisc")) {install.packages("Hmisc")}; library("Hmisc") 
if (!require("DT")) {install.packages("DT")}; library("DT") # For table display
if (!require("plotly")) {install.packages("plotly")}; library("plotly") # For plotting
options("shiny.sanitize.errors" = FALSE, "scipen" = 999)

ui <- dashboardPage( 
  skin = "green",
  dashboardHeader(title = "GST Parameters"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload data", tabName = "upload", icon = icon("paperclip")),
      menuItem("Crop to on-site measurements", tabName = "crop", icon = icon("scissors")),
      menuItem("Visualise", tabName = "charts", icon = icon("chart-line"),
               menuSubItem("Period means", tabName = "means"),
               menuSubItem("Positive/Negative degree days", tabName = "degdays"),
               menuSubItem("Other parameters", tabName = "other_params")),
      menuItem("Summarise and download", tabName = "summary", icon = icon("clipboard")),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    tags$style(HTML(
      ".dygraph-legend {
      left: 75px !important;
    }"
    )),
    tabItems(
      tabItem(tabName = "upload", # ============================================
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Upload a GST dataset")),
                  h4(strong("Caution: "), "This app only accepts a single file at a time in order to avoid confusion between datasets.")
                )
              ),
              fluidRow(
                box(
                  title = "Upload settings",
                  status = "success",
                  solidHeader = FALSE,
                  width = 5,
                  fileInput("fname", "Data", buttonLabel = "Browse..."),
                  radioButtons("separator", "Separator:", choices = c("Tab"   = "\t",
                                                                      ","     = ",",
                                                                      ";"     = ";",
                                                                      "|"     = "|",
                                                                      "Space" = " ")),
                  numericInput("skip", "Rows to skip", 0, min = 0),
                  checkboxInput("header", "Column headers", TRUE),
                  textInput("decimal", "Decimal marker", "."),
                  textInput("format", "Date-time format (POSIXct)", "%d.%m.%y %H:%M"),
                  uiOutput("posix_link"),br(),
                  radioButtons("hemisphere", "Location of your study area",
                               choices = c("Northern hemisphere" = "north",
                                           "Southern hemisphere" = "south")),
                  actionButton("uploadbtn", "Upload", style = "position: absolute; right: 10px; bottom: 5px")
                ),
                uiOutput("raw_box")
              )
      ),
      tabItem(tabName = "crop", # ==============================================
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Crop data to on-site measurements")),
                  h4("This step is not necessary if you have already prepared your dataset and restricted it to on-site measurements."),
                  h4(strong("Caution: "), "This tool only allows to crop all columns to the same date-times, which may result in some loss of data.")
                )
              ),
              fluidRow(
                box(
                  width = 4,
                  status = "success",
                  h4(strong("Select the time range to examine.")),
                  sliderInput("first", "Number of days after dataset start", min = 1, max = 40, value = 1),
                  sliderInput("last", "Number of days before dataset end", min = 1, max = 40, value = 1)
                ),
                uiOutput("crop_box"),
                box(
                  width = 4,
                  status = "success",
                  h4(strong("Do you wish to crop your data to the selected times?")),
                  br(),
                  actionButton("cropbtn", "Crop", style = "position: absolute; right: 10px; bottom: 5px")
                )
              ),
              fluidRow(
                box(
                  id = "prelim_box",
                  width = 12,
                  plotOutput("preliminary")
                ),
                tags$head(tags$style('#prelim_box .box-header{ display: none}'))
              ),
              fluidRow(
                box(
                  title = "Cropped dataset",
                  width = 12,
                  status = "primary",
                  DTOutput("cropped_table")
                )
              )
      ),
      tabItem(tabName = "means", # =============================================
              
              # Select variables -----------------------------------------------
              fluidRow(
                uiOutput("means_varselect")
              ),
              
              # Daily means ----------------------------------------------------
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Daily means")),
                  h4(strong("Caution: "), "Daily means are only calculated for days during which the number of observations is equal to or greater than the mean daily number of observations throughout the dataset.",
                     "Incomplete days are not displayed.")
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  status = "primary",
                  plotlyOutput("dmeans_plot")
                ),
                box(
                  width = 6,
                  status = "success",
                  DTOutput("dmeans_table")
                )
              ),
              
              # Monthly means --------------------------------------------------  
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Monthly means")),
                  h4(strong("Caution: "), "Monthly means are only calculated for months during which data is available every day. ",
                     "Incomplete months (usually at the beginning and end of your dataset) are not displayed.")
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  status = "primary",
                  plotlyOutput("mmeans_plot")
                ),
                box(
                  width = 6,
                  status = "success",
                  DTOutput("mmeans_table")
                )
              )
      ),
      
      tabItem(tabName = "degdays", # ===========================================
              
              # NDD ------------------------------------------------------------
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Negative degree days"))
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  status = "success",
                  column(
                    width = 6, 
                    h4(strong("Final NDD summary")),
                    DTOutput("ndd_totals")
                  ),
                  column(
                    width = 6,
                    h4(strong("Detailed NDDs")),
                    DTOutput("ndd_table")
                  )
                )
              ),
              
              # PDD ------------------------------------------------------------
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Positive degree days"))
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  status = "success",
                  column(
                    width = 6, 
                    h4(strong("Final PDD summary")),
                    DTOutput("pdd_totals")
                  ),
                  column(
                    width = 6,
                    h4(strong("Detailed PDDs")),
                    DTOutput("pdd_table")
                  )
                )
              )
      ),
      tabItem(tabName = "other_params", # ======================================
              
              # Select variable to plot # --------------------------------------
              fluidRow(
                uiOutput("otherparams_varselect")
              ),
              
              # Zero curtain ---------------------------------------------------
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Zero curtain"))
                )
              ),
              fluidRow(
                uiOutput("zc_plot_ui"),
                uiOutput("zc_table_ui")
              ),
              
              # WEqT -----------------------------------------------------------
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Winter equilibrium temperature")),
                  #h4(strong("Caution:"), "Not displayed in variable summary as the computation period must be selected manually.")¨
                  h3(em("UNDER CONSTRUCTION"))
                )
              ),
              # fluidRow(
              #   box(
              #     width = 4,
              #     numericInput("weqt_before", "Start (days before zero curtain)",
              #                  value = 1,
              #                  min = 1)
              #   ),
              #   box(
              #     width = 4,
              #     numericInput("weqt_duration", "Duration (days from start)",
              #                  value = 1, 
              #                  min = 1)
              #   )
              # ),
              # fluidRow(
              #   box(
              #     title = "Period selected for WEqT computation",
              #     width = 6,
              #     status = "primary",
              #     plotOutput("weqt_plot")
              #   ),
              #   box(
              #     width = 6,
              #     status = "success",
              #     DTOutput("weqt_table")
              #   )
              # ),
      ),
      tabItem(tabName = "summary", # ===========================================
              fluidRow(
                column(
                  width = 3,
                  h3(strong("Daily means"))
                )
              ), 
              fluidRow(
                column(
                  width = 3,
                  downloadButton("daily_dl", "Download as CSV")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Variable summary"))
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  status = "success",
                  DTOutput("summary_table"),
                  br(),
                  br(),
                  downloadButton("summary_dl", "Download as CSV", style = "position: absolute; right: 10px; bottom: 5px")
                )
              )
      ),
      tabItem(tabName = "about", # =============================================
              fluidRow(
                column(
                  width = 12,
                  h3(strong("About this application")),
                  p("This application was developed using 2021-2022 ground surface temperature data from Les Attelas VS and Les Cliosses VS. \n
                    It is intended for use with GST data, but is essentially applicable to any temperature time series.")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Source code availability")),
                  p("The source code of this application is made publicly available on GitHub at ", a("https://github.com/ocallaghanm/GST_parameters"), ".", br(),
                    "If you prefer, the individual functions this application uses are made available in an R script you may use directly in RStudio.")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Queries and feedback")),
                  p("Any issues or feature requests may be reported to GitHub at ", a("https://github.com/ocallaghanm/GST_parameters/issues"),
                    "or by mail at ", a("marc.ocallaghan@bluewin.ch"), ".")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Unlicense")),
                  p("This application is public domain, and as such, is distributed under the terms of the Unlicense:"),
                  div('This is free and unencumbered software released into the public domain.', br(),
                      'Anyone is free to copy, modify, publish, use, compile, sell, or
                  distribute this software, either in source code form or as a compiled
                  binary, for any purpose, commercial or non-commercial, and by any
                  means.', br(),
                  'In jurisdictions that recognize copyright laws, the author or authors
                  of this software dedicate any and all copyright interest in the
                  software to the public domain. We make this dedication for the benefit
                  of the public at large and to the detriment of our heirs and
                  successors. We intend this dedication to be an overt act of
                  relinquishment in perpetuity of all present and future rights to this
                  software under copyright law.', br(),
                  'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
                  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
                  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
                  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
                  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
                  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
                  OTHER DEALINGS IN THE SOFTWARE.', br(),
                  'For more information, please refer to ', a('http://unlicense.org/'), '.')
                )
              )
      )
    )
  )
)

# SERVER #######################################################################
server <- function(input, output, session, environment) { 
  
  output$posix_link <- renderUI({
    tagList(a("More about POSIX date-time formats", 
              href = "https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime",
              target = "_blank"))
  })
  
  # Upload raw data
  raw <- bindEvent(reactive({ 
    req(input$fname, input$separator, input$skip, input$decimal, input$format)
    
    # Read in data as zoo object then convert to xts
    tryCatch(
      expr = read.table.zoo(file    = input$fname$datapath,
                             sep    = input$separator,
                             skip   = input$skip,
                             header = input$header,
                             dec    = input$decimal,
                             index.column = 1,
                             drop = FALSE,
                             blank.lines.skip = TRUE,
                             strip.white = TRUE,
                             check.names = FALSE,
                             na.strings = c("NA", "N/A", "NAN", "", "#N/A", "na", "nan", "NaN"),
                             FUN = as.POSIXct,
                             tz = "UTC", # Workaround for as.yearmon(), which converts all datetimes to UTC. Should not have any incidence on data as there is only one site processed at a time.
                             format = input$format) %>% as.xts(check.names = FALSE),
      error = function(err) {
        showNotification(paste0("Error: ", err$message), duration = 20, type = "error")
        return(data.frame())
      },
      warning = function(warn) {
        showNotification(paste0("Warning: ", warn$message), duration = 20, type = "warning")
        return(data.frame())
      })
  }), 
  input$uploadbtn
  ) 
  
  output$warn_raw_na <- renderText({
    if(anyNA(raw())) {
      nacount <- raw() %>% is.na %>% sum
      paste(strong("Warning: "), "This dataset contains", nacount, 
            "missing values.",
            br(),
            "They will be ignored in following steps.", br(),
            "If you wish to fill them in, consider na.approx() to interpolate them or na.locf() to carry the previous observation forward. ")
    } else if(nrow(raw()) == 0) {
      paste(strong("Warning: "), "It seems there was a problem with your upload.",
            br(),
            "This may be due to:", br(),
            strong("1."), "Wrong date-time format - please specify your date-time in ", strong("POSIXct format;"), br(),
            strong("2."), "Missing ", strong("header "), "- don't forget to tick the corresponding box;", br(),
            strong("3."), "Metadata or other text not fitting your data format - if they are located at the head of the document, you can ", strong("skip"), "them with the ", 
            em("Rows to skip"), " input.", br(),
            strong("4."), "Wrong ", strong("separator"), "- please check the selected separator character;", br(),
            strong("5."), "Your data may contain ", strong("non-Unicode white spaces or a typo in the index"), "- check the error notification to find the rows causing the error.")
    } 
  })
  
  output$raw_table <- renderDT({
    req(raw())
    datatable(as.data.frame(raw()), options = list(scrollX = TRUE), class = "display nowrap")
  })
  
  raw_status <- reactive({
    if_else({anyNA(raw()) | nrow(raw()) == 0}, "warning", "success")
  })
  
  output$raw_box <- renderUI({
    req(raw_status())
    status <- raw_status()
    
    box(
      title = "Raw data inspection",
      status = status,
      solidHeader = FALSE,
      width = 7,
      span(htmlOutput("warn_raw_na"), style = "color:orange"),
      DTOutput("raw_table")
    )
  })
  
  # Select beginning and end of dataset
  first_data <- reactive({
    req(raw(), input$first, input$last)
    raw() %>% first(paste(input$first, "days")) %>% zoo
  })
  
  last_data  <- reactive({
    req(raw())
    raw() %>% last(paste(input$last, "days"))  %>% zoo
  })
  
  # UI to crop to onsite measurements
  output$crop_box <- renderUI({
    req(raw())
    step_end <- index(raw()[length(index(raw()))])
    step_start <- index(raw()[1])
    step <- difftime(step_end, step_start, units = "hours") %>% 
      `/` (length(index(raw()))) %>% 
      round
    step %<>% as.double(units = "secs") %>% round(-2)
    
    box(
      width = 4,
      status = "success",
      h4(strong("Select the start and end time of on-site measurements.")),
      sliderInput("onsite_start", "Start of on-site measurements",
                  min = as.POSIXct(min(index(first_data()))), 
                  max = as.POSIXct(max(index(first_data()))),
                  value = min(index(first_data())),
                  timeFormat = "%F %R",
                  step = step),
      sliderInput("onsite_end", "End of on-site measurements", 
                  min = as.POSIXct(min(index(last_data()))), 
                  max = as.POSIXct(max(index(last_data()))),
                  value = max(index(last_data())),
                  timeFormat = "%F %R",
                  step = step)
    )
  })
  
  # Plot first and last section to find where to crop
  output$preliminary <- renderPlot({
    
    req(first_data(), last_data(), input$onsite_start, input$onsite_end)
    
    layout(matrix(c(1,2), 2, 2, byrow = TRUE), widths = c(5, 5))
    par(mar = c(2,4,2,4)) # Plot margins
    
    # Left plot: raw data, first entries
    plot(first_data(),
         col  = rainbow(ncol(first_data())),
         plot.type = "single",
         xlab = NA,
         ylab = "GST [°C]",
         xaxt = "n",
         main = "First portion",
         cex.main = 1.8,
         cex.lab = 1.5)
    raw_start_ticks <- axis.POSIXct(side  = 1,
                                    x     = index(first_data()),
                                    at    = pretty(index(first_data())),
                                    labels = format(pretty(index(first_data())), format = "%Y-%m-%d"),
                                    cex = 1.2)
    raw_start_ticks
    abline(v = raw_start_ticks, col = "lightgray")
    abline(v = input$onsite_start, col = "darkgreen")
    
    # Right plot: raw data, last entries
    plot(last_data(),
         col  = rainbow(ncol(last_data())),
         plot.type = "single",
         xlab = NA,
         ylab = NA,
         xaxt = "n",
         main = "Last portion",
         cex.main = 1.8,
         cex.lab = 1.5)
    raw_end_ticks <- axis.POSIXct(side  = 1,
                                  x      = index(last_data()),
                                  at     = pretty(index(last_data())),
                                  labels = format(pretty(index(last_data())), format = "%Y-%m-%d"),
                                  cex = 1.2)
    raw_end_ticks
    abline(v = raw_end_ticks, col = "lightgray")
    abline(v = input$onsite_end, col = "navy")
  })
  
  cropped_data <- reactiveValues(data = NULL)
  
  cropped <- bindEvent(reactive({
    req(raw())
    
    start_indx <- index(raw()) >= as.POSIXct(input$onsite_start) # Get start
    end_indx   <- index(raw()) <= as.POSIXct(input$onsite_end) # Get end
    crp <- raw()[which(start_indx & end_indx), , drop = FALSE] %>%
      set_colnames(colnames(raw())) # Keep same column names.
  }), 
  
  input$cropbtn
  )
  
  # Populate cropped_data if applicable
  observe({
    cropped_data$data <- cropped()
  })
  
  output$cropped_table <- renderDT({
    req(cropped())
    cropped_data$data %>%
      as.data.frame %>%
      datatable(options = list(scrollX = TRUE), class = "display nowrap")
  })
  
  site <- reactiveValues(data = NULL) # To store cropped or uncropped data
  zc_msgs <- reactiveValues(error = NULL, warning = NULL) # To store warnings if no zero curtain is found
  
  # Reset data and sliders if a new dataset is uploaded
  observeEvent(input$uploadbtn, {
    cropped_data$data <- NULL
    site$data <- raw()
    zc_msgs$error <- NULL
    zc_msgs$warning <- NULL
    updateSliderInput(session = session, "first", value = 1)
    updateSliderInput(session = session, "last", value = 1)
    updateSliderInput(session = session, "onsite_start", value = as.POSIXct(min(index(first_data()))))
    updateSliderInput(session = session, "onsite_end", value = as.POSIXct(max(index(last_data()))))
  })
  
  # Check if dataset was cropped, else use raw
  observe({
    if (!is.null(cropped_data$data)){
      site$data <- cropped_data$data
    }
  })
  
  # UI to select which variables to plot the period means for
  output$means_varselect <- renderUI({
    req(site$data)
    col_choice <- colnames(site$data)
    box(
      width = 12,
      title = strong("Select variable(s) to plot:"),
      pickerInput("mean_variables", NULL, 
                  choices = col_choice, 
                  multiple = TRUE,
                  inline = TRUE,
                  options = list(`actions-box` = TRUE))
    )
  })
  
  # Find which days have data from start to end 
  full_days <- reactive({
    req(site$data)
    daily_nona <- site$data 
    
    daily_lengths <- daily_nona %>%
      apply.daily(apply, 2, function(x) {sum(!is.na(x))}) # Number of valid observations per day
    tclass(daily_lengths) <- "Date"
    mean_daily_entries <- daily_lengths %>% mean %>% ceiling # Mean number of observations per day
    for (var in names(daily_lengths)) {
      var_missing <- index(daily_lengths[which(daily_lengths[, var] < mean_daily_entries)])
      daily_nona[which(as.Date(index(daily_nona)) %in% var_missing), var] <- NA
    }
    daily_nona
  })
  
  # Daily means
  daily <- reactive({
    req(full_days()) 
    dly <- full_days() %>%
      apply.daily(apply, 2, mean) %>%
      round(digits = 2) %>%
      set_colnames(colnames(site$data))
    tclass(dly) <- "Date"
    dly
  })  
  
  output$dmeans_table <- renderDT({
    req(daily())
    daily() %>%
      as.data.frame(check.names = FALSE) %>%
      datatable(options = list(scrollX = TRUE), class = "display nowrap")
  })
  
  output$dmeans_plot <- renderPlotly({
    req(daily())
    site_mean <- daily()[,input$mean_variables] %>% coredata %>% data.frame(check.names = FALSE) # Plot only selected
    site_mean$Date <- index(daily())
    cols <- names(site_mean)
    cols <- cols[-which(cols == "Date")]
    
    dmplot <- plot_ly(data = site_mean, type = "scatter", mode = "lines")
    for (var in cols) {
      dmplot <- dmplot %>% add_trace(x = ~Date, y = site_mean[[var]], name = var, 
                                     type = "scatter", mode = "lines", 
                                     line = list(width = 1.2))
    }
    dmplot %<>% 
      layout(xaxis = list(title = ""),
             yaxis = list(title = "GST [°C]"))
    config(dmplot, toImageButtonOptions = list(format = "svg",
                                               filename = paste(tools::file_path_sans_ext(basename(input$fname$name)), "_daily_plot", sep = "")))
  })
  
  # Find which months have data every day 
  full_months <- reactive({
    req(full_days())
    monthly_nona <- full_days() 
    monthly_lengths <- full_days() %>%
      apply.daily(apply, 2, function(x) {sum(!is.na(x))}) %>% # Number of valid observations per day
      apply.monthly(apply, 2, function(x) {sum(!is.na(x))}) # Number of full days per month
    tclass(monthly_lengths) <- "yearmon"
    
    for (var in names(monthly_lengths)) {
      var_missing <- index(monthly_lengths[which(monthly_lengths[, var] < Hmisc::monthDays(monthly_lengths[, var]))])
      monthly_nona[which(as.yearmon(index(monthly_nona)) %in% var_missing), var] <- NA
    }
    monthly_nona
  })
  
  # Monthly mean only for full months
  monthly <- reactive({
    req(full_months())
    mly <- full_months() %>%
      apply.monthly(apply, 2, mean) %>%
      round(digits = 2) %>%
      set_colnames(colnames(site$data))
    tclass(mly) <- "yearmon"
    mly
  })  
  
  output$mmeans_table <- renderDT({
    req(monthly())
    monthly() %>%
      as.data.frame(check.names = FALSE) %>%
      datatable(options = list(scrollX = TRUE), class = "display nowrap")
  })
  
  output$mmeans_plot <- renderPlotly({
    req(monthly())
    site_mean <- monthly()[,input$mean_variables] %>% coredata %>% data.frame(check.names = FALSE) # Plot only selected
    site_mean$Date <- index(monthly()) %>% as.Date
    cols <- names(site_mean)
    cols <- cols[-which(cols == "Date")]
    
    mmplot <- plot_ly(data = site_mean, type = "scatter", mode = "lines")
    for (var in cols) {
      mmplot <- mmplot %>% add_trace(x = ~Date, y = site_mean[[var]], name = var, 
                                     type = "scatter", mode = "lines", 
                                     line = list(width = 1.2))
    }
    mmplot %<>% 
      layout(xaxis = list(title = ""),
             yaxis = list(title = "GST [°C]"))
    config(mmplot, toImageButtonOptions = list(format = "svg",
                                               filename = paste(tools::file_path_sans_ext(basename(input$fname$name)), "_monthly_plot", sep = "")))
  })
  
  # Determine origin of hydrological year depending on hemisphere
  wyear_origin <- reactive({
    req(input$hemisphere)
    ifelse(input$hemisphere == "north", "September", "March")
  })
  
  # Negative degree days
  ndd <- reactive({
    req(daily())
    NDD <- daily()
    NDD[NDD > 0] <- 0 # Set all positive temperatures to 0 to sum the negative values
    NDD %<>% na.trim(sides = "both", is.na = "any", maxgap = 1) # Trim first and last NA as they may only be due to measurement start time
    wyear <- water_year(index(NDD), # Compute hydrological year 
                            origin = wyear_origin(), 
                            as.POSIX = TRUE) %>% 
      as.yearmon %>% trunc("year") %>% as.numeric 
    NDD <- merge.xts(NDD, wyear, check.names = FALSE)
    NDD_df <- data.frame("Date" = index(NDD), coredata(NDD), check.names = FALSE) %>%
      group_by(wyear) %>% 
      filter(n() >= 100) %>% # Min number of observations per hydro year
      mutate(across(-Date, cumsum)) %>%
      mutate(across(-Date, ~ round(.x, digits = 1)))
    NDD_df
  })
  
  output$ndd_table <- renderDT({
    req(ndd())
    ndd() %>%
      ungroup %>%
      select(-wyear) %>%
      as.xts(check.names = FALSE) %>%
      as.data.frame(check.names = FALSE) %>%
      datatable(options = list(scrollX = TRUE), class = "display nowrap")
  })
  
  # Find final NDD values
  ndd_total <- reactive({
    req(ndd()) 
    ndd() %>% 
      select(-Date) %>% 
      summarise(across(everything(), min)) %>%
      relocate(wyear) %>%
      rename("Hydrological year" = wyear) %>%
      ungroup
  })
  
  output$ndd_totals <- renderDT({
    req(ndd_total())
    ndd_total() %>%
      datatable(rownames = FALSE, options = list(scrollX = TRUE), class = "display nowrap")
  })
  
  # Positive degree days
  pdd <- reactive({
    req(daily())
    PDD <- daily()
    PDD[PDD < 0] <- 0 # Set all negative temperatures to 0
    PDD %<>% na.trim(sides = "both", is.na = "any", maxgap = 1) # Trim first and last NA as they may only be due to measurement start time
    wyear <- water_year(index(PDD), # Compute hydrological year 
                            origin = wyear_origin(), 
                            as.POSIX = TRUE) %>% 
      as.yearmon %>% trunc("year") %>% as.numeric 
    PDD <- merge.xts(PDD, wyear, check.names = FALSE)
    PDD_df <- data.frame("Date" = index(PDD), coredata(PDD), check.names = FALSE) %>%
      group_by(wyear) %>% 
      filter(n() >= 100) %>%
      mutate(across(-Date, cumsum)) %>%
      mutate(across(-Date, ~ round(.x, digits = 1)))
  })
  
  output$pdd_table <- renderDT({
    req(pdd())
    pdd() %>%
      ungroup %>%
      select(-wyear) %>%
      as.xts(check.names = FALSE) %>%
      as.data.frame(check.names = FALSE) %>%
      datatable(options = list(scrollX = TRUE), class = "display nowrap")
  })
  
  # Find final PDD values
  pdd_total <- reactive({
    req(pdd())
    pdd() %>% 
      select(-Date) %>% 
      summarise(across(everything(), max)) %>%
      relocate(wyear) %>%
      rename("Hydrological year" = wyear) %>%
      ungroup
  })
  
  output$pdd_totals <- renderDT({
    req(pdd_total())
    pdd_total() %>%
      datatable(rownames = FALSE, options = list(scrollX = TRUE), class = "display nowrap")
  })
  
  # Select one variable for ZC plotting and WEqT computation
  output$otherparams_varselect <- renderUI({
    req(site$data)
    col_choice <- colnames(daily())
    box(
      width = 12,
      title = strong("Select variable to plot and compute WEqT for:"),
      pickerInput("param_variable", NULL, 
                  choices = col_choice, 
                  multiple = FALSE)
    )
  })
  
  # Reset pickers if a new dataset is uploaded
  observeEvent(input$uploadbtn, {
    updatePickerInput(session = session, "mean_variables", selected = NULL)
    updatePickerInput(session = session, "param_variable", selected = NULL)
  })
  
  zero_curtain <- reactive({
    req(daily())
    for_zc <- daily()
    wyear <- water_year(index(for_zc), # Compute hydrological year 
                               origin = wyear_origin(), 
                               as.POSIX = TRUE) %>% 
      as.yearmon %>% trunc("year") %>% as.numeric 
    for_zc <- merge.xts(for_zc, wyear, check.names = FALSE)
    for_zc_df <- data.frame(Date = index(for_zc), coredata(for_zc), check.names = FALSE)
    for_zc_df[for_zc_df > -0.1 & for_zc_df < 0.1] <- 0 # Set to 0 to account for sensor error
    forzc_df_filter <- for_zc_df %>% 
      group_by(wyear) %>%
      filter(n() > 100) # Remove hydrological years with too little entries to have a zero curtain
    forzc_df_list <- split(forzc_df_filter, 
                           f = forzc_df_filter$wyear) # Split by hydrological year
    zc_list <- tryCatch(expr = {
      lapply(forzc_df_list, FUN = function(x) {
        apply(x[!names(x) %in% c("wyear", "Date")], 2, FUN = function(y, wname = unique(x$wyear), dates = x$Date) {
          zcs <- rle2(y == 0, indices = TRUE) %>% 
            unclass %>% 
            data.frame(check.names = FALSE) %>%
            filter(value == 1) %>% # Where T.C is 0
            filter(length == max(length)) %>%
            filter(length > 10) %>%
            mutate(start = dates[start], # Get dates
                   stop  = dates[stop]) %>%
            select(-value) %>%
            dplyr::rename("{wname} ZC start"    := start,
                          "{wname} ZC end"      := stop,
                          "{wname} ZC duration" := length)
          if (nrow(zcs) == 0) { # Add a row of NA if there is no other
            zcs[1,] <- NA_real_
          }
          return(zcs)
        }
        )
      })},
      error = function(err) {
        zc_msgs$error <- err$message
        return(NULL)
      },
      warning = function(warn) {
        zc_msgs$warning <- warn$message
        return(NULL)
      })
    zc_by_year <- lapply(zc_list, function(x) {
      do.call(rbind, x) %>% t %>% data.frame(check.names = FALSE)
      }) # List of start-stop-durations per year
    zc_all <- do.call(rbind, unname(zc_by_year))
    zc_all
  })

  output$zc_table <- renderDT({
    req(zero_curtain())
    validate(
      need(is.null(zc_msgs$error), "Unable to compute zero curtains."),
      need(is.null(zc_msgs$warning), "No zero curtains could be identified."),
      need(!all(is.na(zero_curtain())), "No zero curtains could be identified.")
    )
    zero_curtain() %>%
      datatable(options = list(scrollX = TRUE), class = "display nowrap")
  })
  
  output$zc_plot <- renderPlot({
    req(daily(), zero_curtain(), input$param_variable) # Plot selected
    validate(
      need(!is.null(input$param_variable), "Select a variable to display.")
    )
    pltvar <- as.character(input$param_variable)
    validate(
      need(!is.na(zero_curtain()[, pltvar]), "No zero curtain to plot."),
      need(is.null(zc_msgs$warning), "No zero curtain to plot."),
      need(is.null(zc_msgs$error), "No zero curtain to plot.")
      )
    
    site_zoo <- daily()[, pltvar] %>% zoo
    startdates <- zero_curtain() %>% 
      select({pltvar}) %>%
      mutate(across(everything(), as.Date)) %>%
      rownames_to_column(var = "rname") %>% 
      filter(str_detect(rname, "start")) %>% 
      column_to_rownames(var = "rname") %>%
      as.vector %>% unname %>% unlist %>% as.Date
    enddates <- zero_curtain() %>% 
      select({pltvar}) %>% 
      mutate(across(everything(), as.Date)) %>%
      rownames_to_column(var = "rname") %>% 
      filter(str_detect(rname, "end")) %>% 
      column_to_rownames(var = "rname") %>%
      as.vector %>% unname  %>% unlist %>% as.Date
    stopifnot("Number of start and end dates must be equal." = all.equal(length(startdates), length(enddates)))
    
    par(mfrow = c(1,1), mar = c(2,4,2,4))
    plot(site_zoo, 
         col      = "blue", 
         xlab     = "Date",
         xaxt     = "n",
         ylab     = "GST [°C]")
    
    # Rectangles to show zero curtains
    rect(xleft   = startdates,
         ybottom = par("usr")[3],
         xright  = enddates,
         ytop    = par("usr")[4],
         col     = rgb(0.1, 0.1, 0.9, alpha = 0.2),
         border  = NA)
    
    # Major ticks
    major <- pretty(index(site_zoo))
    axis.Date(side   = 1, 
              x      = index(site_zoo), 
              at     = major, 
              labels = format(major, format = "%Y-%m"))
    # Minor ticks
    minor <- index(site_zoo[!site_zoo %in% major]) %>% trunc("month")
    axis.Date(side   = 1, 
              x      = index(site_zoo),
              at     = minor, 
              labels = FALSE,
              tcl    = -0.3)
    
    grid(nx = NA, ny = NULL)
  })
  
  zc_status <- reactive({
    if (is.null(zc_msgs$warning) & !is.null(zc_msgs$error)) {
      zcwarn_clr <- "color:red"
    } else if (!is.null(zc_msgs$warning) & is.null(zc_msgs$error)) {
      zcwarn_clr <- "color:orange"
    } else if (all(is.na(zero_curtain()))) {
      zcwarn_clr <- "color:orange"
    } 
  })
  
  output$zc_table_ui <- renderUI({
    box(
      width = 6,
      status = ifelse(is.null(zc_msgs$warning) & is.null(zc_msgs$error) & !all(is.na(zero_curtain())), "success", "warning"),
      DTOutput("zc_table")
    )
  })
  
  output$zc_plot_ui <- renderUI({
    box(
      width = 6,
      status = ifelse(is.null(zc_msgs$warning) & is.null(zc_msgs$error) & !all(is.na(zero_curtain())), "primary", "warning"),
      plotOutput("zc_plot")
    )
  })
  
  # weqt <- reactive({
  #   req(daily(), zero_curtain(), input$param_variable, input$weqt_before, input$weqt_duration)
  #   
  #   zc_start <- zero_curtain()[input$param_variable, "Start"] %>% as.Date # Find ZC start
  #   weqt_start <- zc_start - input$weqt_before # Find WEqT start from input and ZC start
  #   weqt_end <- weqt_start + input$weqt_duration
  #   WEqT <- daily()[, input$param_variable] %>% 
  #     window(start = weqt_start, end = weqt_end) %>% 
  #     mean
  #   data.frame(Start = weqt_start,
  #              End   = weqt_end,
  #              WEqT  = WEqT)
  # })
  # 
  # output$weqt_table <- renderDT({
  #   req(weqt())
  #   weqt() %>%
  #     datatable(class = "display nowrap")
  # })
  # 
  # output$weqt_plot <- renderPlot({
  #   req(daily(), weqt(), input$param_variable) # Plot selected
  #   site_zoo <- daily()[, input$param_variable] %>% zoo
  #   
  #   par(mfrow = c(1,1), mar = c(2,4,2,4))
  #   plot(site_zoo, 
  #        col      = "blue", 
  #        xlab     = "Date",
  #        xaxt     = "n",
  #        ylab     = "GST [°C]")
  #   
  #   # Rectangle to show selected dates
  #   rect(xleft   = weqt()$Start,
  #        ybottom = par("usr")[3],
  #        xright  = weqt()$End,
  #        ytop    = par("usr")[4],
  #        col     = rgb(0.9, 0.2, 0.2, alpha = 0.2),
  #        border  = NA)
  #   
  #   # Major ticks
  #   major <- pretty(index(site_zoo))
  #   axis.Date(side   = 1, 
  #             x      = index(site_zoo), 
  #             at     = major, 
  #             labels = format(major, format = "%Y-%m"))
  #   # Minor ticks
  #   minor <- index(site_zoo[site_zoo %nin% major]) %>% trunc("month")
  #   axis.Date(side   = 1, 
  #             x      = index(site_zoo),
  #             at     = minor, 
  #             labels = FALSE,
  #             tcl    = -0.3)
  #   
  #   grid(nx = NA, ny = NULL)
  # })

  # Mean over entire timespan
  yearly <- reactive({
    yly <- site$data 
    wyear <- water_year(index(yly), # Compute hydrological year 
                         origin = wyear_origin(), 
                         as.POSIX = TRUE) %>% 
      as.yearmon %>% trunc("year") %>% as.numeric 
    yly <- merge.xts(yly, wyear, check.names = FALSE)
    yly_df <- data.frame(DateTime = index(yly), coredata(yly), check.names = FALSE)
    yly_df <- yly_df %>%
      group_by(wyear) %>%
      filter(length(unique(as.Date(DateTime))) >= 364) %>%
      summarise(across(-DateTime, mean)) %>%
      round(digits = 2) %>%
      ungroup %>%
      as.data.frame(check.names = FALSE)
    if (nrow(yly_df) == 0) {
      for (i in length(unique(wyear))) {
        yly_df[i,] <- NA_real_
      }
      yly_df$wyear <- unique(wyear)
    }
    yly_df
  })
  
  # downloadHandler for daily means
  output$daily_dl <- downloadHandler(
    filename = function() {
      paste(tools::file_path_sans_ext(basename(input$fname$name)), "_daily.csv", sep = "")
    },
    content = function(file) {
      write.csv(as.data.frame(daily(), check.names = FALSE), file)
    },
    contentType = "text/csv"
  )
  
  # Summary of monthly means, full period mean, ZC, NDD and PDD. WEqT must be calculated individually.
  var_summary <- reactive({
    req(monthly(), yearly())
    mly_rnames <- paste(as.character(index(monthly())), "mean GST", sep = " ")
    yly_rnames <- paste(as.character(yearly()$wyear), "mean GST", sep = " ")
    ndd_rnames <- paste(as.character(ndd_total()$`Hydrological year`), "NDD", sep = " ")
    pdd_rnames <- paste(as.character(pdd_total()$`Hydrological year`), "PDD", sep = " ")
    df_yly <- yearly() %>%
      select(-wyear) %>%
      as.data.frame(check.names = FALSE) %>%
      set_rownames(yly_rnames)
    df_mly <- data.frame(coredata(monthly()), check.names = FALSE) %>%
      set_rownames(mly_rnames)
    
    # Set parameters to NULL if they do not exist
    df_ndd <- if (isTruthy(ndd_total())) { 
                     ndd_total() %>%
                       select(-`Hydrological year`) %>%
                       as.data.frame(check.names = FALSE) %>%
                       set_rownames(ndd_rnames)
      } else {NULL}
    df_pdd <- if (isTruthy(pdd_total())) { 
                     pdd_total() %>%
                       select(-`Hydrological year`) %>%
                       as.data.frame(check.names = FALSE) %>%
                       set_rownames(pdd_rnames)
      } else {NULL}
    df_zc <- if (isTruthy(zero_curtain()) & !all(is.na(zero_curtain()))) {
      zero_curtain()
      } else {NULL}
    
    df_list <- list(df_yly, df_mly) # Monthly and yearly means must be displayed even if NA
    
    # Append parameters to list if they exist
    if (!is.null(df_ndd)) {df_list[[length(df_list) + 1]] <- df_ndd}
    if (!is.null(df_pdd)) {df_list[[length(df_list) + 1]] <- df_pdd}
    if (!is.null(df_zc)) {df_list[[length(df_list) + 1]] <- df_zc}
    summary_df <- do.call(rbind, df_list)
    summary_df
  })
  
  output$summary_table <- renderDT({
    req(var_summary())
    
    var_summary() %>%
      datatable(options = list(scrollX = TRUE, pageLength = 20), class = "display nowrap")
  })
  
  # downloadHandler for summary
  output$summary_dl <- downloadHandler(
    filename = function() {
      paste(tools::file_path_sans_ext(basename(input$fname$name)), "_summary.csv", sep = "")
    },
    content = function(file) {
      write.csv(var_summary(), file)
    },
    contentType = "text/csv"
  )
  
  # Force stop on session end
  session$onSessionEnded(function() {
    stopApp()
  })
} 

# BUILD APP ####################################################################
shinyApp(ui, server)
