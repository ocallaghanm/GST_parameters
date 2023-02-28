library("shiny") # For the user interface
library("shinyWidgets")
library("shinydashboard")
library("tidyverse") 
library("magrittr") # Core packages for data processing
library("accelerometry")
library("xts") # For time-series data processing
library("lubridate")
library("DT")
library("plotly")


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
                                                                      "Other" = "")),
                  numericInput("skip", "Rows to skip", 0, min = 0),
                  checkboxInput("header", "Column headers", TRUE),
                  textInput("decimal", "Decimal marker", "."),
                  textInput("format", "Date-time format (POSIXct)", "%d.%m.%y %H:%M"),
                  uiOutput("posix_link"),
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
                uiOutput("means_varselect"),
              ),
              
              # Daily means ----------------------------------------------------
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Daily means"))
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
                box(
                  width = 6,
                  status = "primary",
                  plotOutput("zc_plot")
                ),
                box(
                  width = 6,
                  status = "success",
                  DTOutput("zc_table")
                )
              ),
              
              # WEqT -----------------------------------------------------------
              fluidRow(
                column(
                  width = 12,
                  h3(strong("Winter equilibrium temperature")),
                  h4(strong("Caution:"), "Not displayed in variable summary as the computation period must be selected manually.")
                )
              ),
              fluidRow(
                box(
                  width = 4,
                  numericInput("weqt_before", "Start (days before zero curtain)",
                               value = 1,
                               min = 1)
                ),
                box(
                  width = 4,
                  numericInput("weqt_duration", "Duration (days from start)",
                               value = 1, 
                               min = 1)
                )
              ),
              fluidRow(
                box(
                  title = "Period selected for WEqT computation",
                  width = 6,
                  status = "primary",
                  plotOutput("weqt_plot")
                ),
                box(
                  width = 6,
                  status = "success",
                  DTOutput("weqt_table")
                )
              ),
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
server <- function(input, output, session) { 
  
  output$posix_link <- renderUI({
    tagList(a("More about POSIX date-time formats", 
              href = "https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime",
              target = "_blank"))
  })
  
  # Upload raw data
  raw <- bindEvent(reactive({ 
    req(input$fname, input$separator, input$skip, input$decimal, input$format)
    
    # Read in data as zoo object then convert to xts
    read.delim.zoo(file = input$fname$datapath,
                   sep = input$separator,
                   skip = input$skip,
                   header = input$header,
                   dec = input$decimal,
                   index.column = 1,
                   drop = FALSE,
                   FUN = as.POSIXct,
                   tz = "GMT", # Workaround for as.yearmon(), which converts all datetimes to GMT. Should not have any incidence on data as there is only one site processed at a time.
                   format = input$format) %>% as.xts
  }), 
  input$uploadbtn
  )
  
  output$warn_raw_na <- renderText({
    if(anyNA(raw())) {
      naidx <- index(raw()[which(is.na(raw()))])
      nacount <- length(naidx)
      paste(strong("Warning: "), "This dataset contains", nacount, 
            "missing values.",
            br(),
            "Consider na.approx() to interpolate them or na.locf() to carry the previous observation forward. Else they will be ignored in following steps.")
    }
  })
  
  output$raw_table <- renderDT({
    req(raw())
    datatable(as.data.frame(raw()), options = list(scrollX = TRUE))
  })
  
  raw_status <- reactive({
    if_else(anyNA(raw()), "warning", "success")
  })
  
  output$raw_box <- renderUI({
    
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
    raw() %>% first(paste(input$first, "days")) %>% zoo
  })
  
  last_data  <- reactive({
    raw() %>% last(paste(input$last, "days"))  %>% zoo
  })
  
  # UI to crop to onsite measurements
  output$crop_box <- renderUI({
    
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
      datatable(options = list(scrollX = TRUE))
  })
  
  site <- reactiveValues(data = NULL)
  
  # Reset data and sliders if a new dataset is uploaded
  observeEvent(input$uploadbtn, {
    cropped_data$data <- NULL
    site$data <- raw()
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
    idxhr <- as.xts(site$data) %>% index
    fulldays <- vector()
    mean_daily_entries <- floor(length(idxhr) / ndays(idxhr))
    
    for (day in unique(as.Date(idxhr))) {
      lgth <- idxhr[which(as.Date(idxhr) == day)] %>% length
      if (lgth >= mean_daily_entries) {
        fulldays <- append(fulldays, day)
      }
    }
    idx_for_fulldays <- which(as.Date(index(site$data)) %in% fulldays)
    site$data[idx_for_fulldays,]
  })
  
  # Daily means
  daily <- reactive({
    dly <- full_days() %>%
      apply.daily(apply, 2, mean, na.rm = TRUE) %>%
      round(digits = 2) %>%
      set_colnames(colnames(full_days()))
    index(dly) %<>% format("%Y-%m-%d") %>% as.Date
    dly
  })  
  
  output$dmeans_table <- renderDT({
    req(daily())
    daily() %>%
      as.data.frame %>%
      datatable(options = list(scrollX = TRUE))
  })
  
  output$dmeans_plot <- renderPlotly({
    site_mean <- daily()[,input$mean_variables] %>% coredata %>% data.frame # Plot only selected
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
    req(site$data)
    idxday <- as.xts(site$data, dateFormat = "Date") %>% index
    fullmonths <- vector()
    for (mth in unique(as.yearmon(idxday))) {
      lgth <- idxday[which(as.yearmon(idxday) == mth)] %>% as.Date %>% unique %>% ndays
      days <- mth %>% as.yearmon %>% days_in_month %>% as.numeric
      if (lgth == days) {
        fullmonths <- append(fullmonths, mth)
      }
    }
    idx_for_fullmonths <- which(as.yearmon(index(site$data)) %in% fullmonths)
    site$data[idx_for_fullmonths,]
  })
  
  # Monthly mean only for full months
  monthly <- reactive({
    mly <- full_months() %>%
      apply.monthly(apply, 2, mean, na.rm = TRUE) %>%
      round(digits = 2) %>%
      set_colnames(colnames(site$data))
    index(mly) %<>% as.yearmon
    mly
  })  
  
  output$mmeans_table <- renderDT({
    req(monthly())
    monthly() %>%
      as.data.frame %>%
      datatable(options = list(scrollX = TRUE))
  })
  
  output$mmeans_plot <- renderPlotly({
    site_mean <- monthly()[,input$mean_variables] %>% coredata %>% data.frame # Plot only selected
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
  
  # Negative degree days
  ndd <- reactive({
    req(daily())
    NDD <- daily()
    
    NDD[NDD > 0] <- 0 # Set all positive temperatures to 0
    NDD %<>% apply(2, cumsum) # Cumulative sum of all values
    format(NDD, scientific = FALSE) # Ditch scientific notation
    NDD %<>% round(digits = 1)
  })
  
  output$ndd_table <- renderDT({
    req(ndd())
    ndd() %>%
      as.data.frame %>%
      datatable(options = list(scrollX = TRUE))
  })
  
  # Find final NDD values
  ndd_total <- reactive({
    req(ndd())
    ndd() %>% apply(2, min)
  })
  
  output$ndd_totals <- renderDT({
    req(ndd_total())
    ndd_total() %>%
      as.data.frame %>%
      datatable(options = list(scrollX = TRUE))
  })
  
  # Positive degree days
  pdd <- reactive({
    req(daily())
    PDD <- daily()
    
    PDD[PDD < 0] <- 0 # Set all positive temperatures to 0
    PDD %<>% apply(2, cumsum) # Cumulative sum of all values
    format(PDD, scientific = FALSE) # Ditch scientific notation
    PDD %<>% round(digits = 1)
  })
  
  output$pdd_table <- renderDT({
    req(pdd())
    pdd() %>%
      as.data.frame %>%
      datatable(options = list(scrollX = TRUE))
  })
  
  # Find final PDD values
  pdd_total <- reactive({
    req(pdd())
    pdd() %>% apply(2, max)
  })
  
  output$pdd_totals <- renderDT({
    req(pdd_total())
    pdd_total() %>%
      as.data.frame %>%
      datatable(options = list(scrollX = TRUE))
  })
  
  # Select one variable for ZC plotting and WEqT computation
  output$otherparams_varselect <- renderUI({
    col_choice <- colnames(site$data)
    box(
      width = 12,
      title = strong("Select variable to plot and compute WEqT for:"),
      pickerInput("param_variable", NULL, 
                  choices = col_choice, 
                  multiple = FALSE)
    )
  })
  
  zero_curtain <- reactive({
    req(daily())
    
    max_list <- vector()
    startdate_list <- vector()
    enddate_list <- vector()
    
    for (i in seq(ncol(daily()))) {
      vec_i <- daily()[, i]
      vec <- coredata(vec_i) %>% as.vector
      vec[vec > -0.1 & vec < 0.1] <- 0 # Set all values in (-0.1, 0.1) to 0, accounting for sensor error
      
      rle_zc <- rle2(vec == 0, indices = TRUE) %>% unclass %>% data.frame # Consecutive 0s yield value = 1, store indices of run start and end
      
      max_zero_rle <- max(rle_zc$length[rle_zc$value == 1]) # Find longest
      
      # Errors
      if (length(max_zero_rle) < 1) { # No max value
        warning("No zero curtain could be identified.")
      } else if (length(max_zero_rle) > 1) { # Several equal max values
        stop("More than one potential zero curtain. Examine data manually.")
      }
      
      max_list %<>% append(max_zero_rle)
      
      startpt <- rle_zc$start[rle_zc$value == 1 & rle_zc$length == max_zero_rle]
      
      endpt <- rle_zc$stop[rle_zc$value == 1 & rle_zc$length == max_zero_rle]
      
      startdate <- index(vec_i[startpt]) %>% as.Date # Get start
      enddate <- index(vec_i[endpt]) %>% as.Date # Get end
      
      startdate_list <- append(startdate_list, startdate)
      enddate_list <- append(enddate_list, enddate)
    }
    
    data.frame(Start = startdate_list,
               End = enddate_list,
               Duration = max_list,
               row.names = colnames(daily()))
  })
  
  output$zc_table <- renderDT({
    req(zero_curtain())
    zero_curtain() %>%
      datatable(options = list(scrollX = TRUE))
  })
  
  output$zc_plot <- renderPlot({
    req(daily(), zero_curtain(), input$param_variable) # Plot selected
    var <- input$param_variable
    
    site_zoo <- daily()[, var] %>% zoo
    startdate <- zero_curtain()[var, "Start"]
    enddate <- zero_curtain()[var, "End"]
    
    par(mfrow = c(1,1), mar = c(2,4,2,4))
    plot(site_zoo, 
         col      = "blue", 
         xlab     = "Date",
         xaxt     = "n",
         ylab     = "GST [°C]")
    
    # Rectangle to show zero curtain
    rect(xleft   = as.Date(startdate),
         ybottom = par("usr")[3],
         xright  = as.Date(enddate),
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
  
  weqt <- reactive({
    req(daily(), zero_curtain(), input$param_variable, input$weqt_before, input$weqt_duration)
    
    zc_start <- zero_curtain()[input$param_variable, "Start"] %>% as.Date # Find ZC start
    weqt_start <- zc_start - input$weqt_before # Find WEqT start from input and ZC start
    weqt_end <- weqt_start + input$weqt_duration
    WEqT <- daily()[, input$param_variable] %>% 
      window(start = weqt_start, end = weqt_end) %>% 
      mean
    data.frame(Start = weqt_start,
               End   = weqt_end,
               WEqT  = WEqT)
  })
  
  output$weqt_table <- renderDT({
    req(weqt())
    weqt() %>%
      datatable
  })
  
  output$weqt_plot <- renderPlot({
    req(daily(), weqt(), input$param_variable) # Plot selected
    site_zoo <- daily()[, input$param_variable] %>% zoo
    
    par(mfrow = c(1,1), mar = c(2,4,2,4))
    plot(site_zoo, 
         col      = "blue", 
         xlab     = "Date",
         xaxt     = "n",
         ylab     = "GST [°C]")
    
    # Rectangle to show selected dates
    rect(xleft   = weqt()$Start,
         ybottom = par("usr")[3],
         xright  = weqt()$End,
         ytop    = par("usr")[4],
         col     = rgb(0.9, 0.2, 0.2, alpha = 0.2),
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
  
  # Mean over entire timespan
  fullperiod <- reactive({
    fpd <- site$data %>%
      colMeans(na.rm = TRUE) %>%
      round(digits = 2)
    fpd
  })
  
  # downloadHandler for daily means
  output$daily_dl <- downloadHandler(
    filename = function() {
      paste(tools::file_path_sans_ext(basename(input$fname$name)), "_daily.csv", sep = "")
    },
    content = function(file) {
      write.csv(as.data.frame(daily()), file)
    },
    contentType = "text/csv"
  )
  
  # Summary of monthly means, full period mean, ZC, NDD and PDD. WEqT must be calculated individually.
  var_summary <- reactive({
    req(monthly(), fullperiod(), zero_curtain(), ndd(), pdd())
    
    mly_rnames <- paste(as.character(index(monthly())), "mean GST", sep = " ")
    df_fpd <- fullperiod() %>%
      data.frame %>%
      t %>%
      set_rownames("Full period mean GST")
    df_mly <- monthly() %>%
      as.data.frame(row.names = mly_rnames)
    df_ndd <- ndd_total() %>%
      data.frame %>%
      t %>%
      set_rownames("Negative degree days")
    df_pdd <- pdd_total() %>%
      data.frame %>%
      t %>%
      set_rownames("Positive degree days")
    df_zc <- zero_curtain() %>% 
      t %>%
      set_rownames(c("Zero curtain start", "Zero curtain end", "Zero curtain duration"))
    
    df_list <- list(df_fpd, df_mly, df_ndd, df_pdd, df_zc)
    summary_df <- do.call(rbind, df_list)
    
  })
  
  output$summary_table <- renderDT({
    req(var_summary())
    
    var_summary() %>%
      datatable(options = list(scrollX = TRUE, pageLength = 20))
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
