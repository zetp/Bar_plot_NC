#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyWidgets)

library(shinyjqui) # resizable plot
#library(ggedit) # editing of plot
library(rhandsontable) # editing of data
library(shinysky) # busy spinner with delay timer
library(shinyjs) # to reset fileInput

library(dplyr)
library(tidyr)

library(ggplot2)
library(svglite) # required for svg output

library(colourpicker)
library(RColorBrewer)

#options(shiny.port=7777)
#options(shiny.host="192.168.136.3")

source("Nature_Comm_bar.R")

### here is code to make color scale picker (palette picker) UI input
### it is taken from shinyWidgets website: https://dreamrs.github.io/shinyWidgets/articles/palette_picker.html

# List of palettes
colors_pal <- lapply(
  X = split(
    x = brewer.pal.info,
    f = factor(brewer.pal.info$category, labels = c("Diverging", "Qualitative", "Sequential"))
  ),
  FUN = rownames
)

# Get all colors given a palette name(s)
get_brewer_name <- function(name) {
  pals <- brewer.pal.info[rownames(brewer.pal.info) %in% name, ]
  res <- lapply(
    X = seq_len(nrow(pals)),
    FUN = function(i) {
      brewer.pal(n = pals$maxcolors[i], name = rownames(pals)[i])
    }
  )
  unlist(res)
}

background_pals <- sapply(unlist(colors_pal, use.names = FALSE), get_brewer_name)

# Calc linear gradient for CSS
linear_gradient <- function(cols) {
  x <- round(seq(from = 0, to = 100, length.out = length(cols)+1))
  ind <- c(1, rep(seq_along(x)[-c(1, length(x))], each = 2), length(x))
  m <- matrix(data = paste0(x[ind], "%"), ncol = 2, byrow = TRUE)
  res <- lapply(
    X = seq_len(nrow(m)),
    FUN = function(i) {
      paste(paste(cols[i], m[i, 1]), paste(cols[i], m[i, 2]), sep = ", ")
    }
  )
  res <- unlist(res)
  res <- paste(res, collapse = ", ")
  paste0("linear-gradient(to right, ", res, ");")
}

background_pals <- unlist(lapply(X = background_pals, FUN = linear_gradient))
head(background_pals, 3)

colortext_pals <- rep(c("white", "black", "black"), times = sapply(colors_pal, length))

### end of pallete picker code

#' Below is code for detection of client mobile devices
#' it is slighly modified approach from https://g3rv4.com/2017/08/shiny-detect-mobile-browsers
mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

# Define UI for application
ui <- fluidPage(
  #tags$head(includeHTML(("google_analytics.html"))), # for google analytics
  
  mobileDetect('isMobile'), # for mobile devices detection
  
  useShinyjs(), # Shinyjs
  
  # text formating for validate() messages
  tags$head(
    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: #990000;
                    font-weight: 400;
                    line-height: 3;
                    text-align:center
                    }
                    "))
    ),
  
   # Application title
  titlePanel(title = HTML(paste("<center><h4>PointBar <small> – bar plots with indicated measurement points</small></h4></center>")), windowTitle = "Point bar"),
   # Sidebar with inputs 
   sidebarLayout(
      sidebarPanel(
        
        tabsetPanel(type="pills",
          tabPanel("Main", value=1), 
          tabPanel("Points", value=2),
          tabPanel("Colour", value=3),
          tabPanel("Axis", value=4)
          , id = "conditionedPanels"),
        
          br(),
            fluidRow(column(9,
            fileInput("data_g", "Choose File",
              multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain",".csv"),
              placeholder = "Sample data"
            )), column(3, tags$small("Decimal separator:"),
            switchInput("dot_comma", onLabel = "<b>.</b>", offLabel = "<b>,</b>", value = TRUE, size = "mini",
                        onStatus = "primary", offStatus = "primary"))),
        div(id="1",
            conditionalPanel(condition="input.conditionedPanels==1", # MAIN
                             
            fluidRow(
              column(6, tags$b(
            radioGroupButtons(inputId = "style_select", label = "Choose style",
                  choices = c("basic","bicolour","greyscale","divergent","custom"),selected="basic",
                  direction="vertical", size = "sm", justified =T)
            )),
            column(6,
            selectInput(inputId = "out_format", width = '100%',
                        label = "Output format",
                        choices = c("pdf", "svg", "eps", "ps"), selected = "pdf"),
            br(),
            downloadButton("DL_plot", label = HTML("Download<br>plot"))
            )),
            
            radioGroupButtons(inputId = "err_select", label = "Choose error",
                              choices = c("SD","SEM"),selected="SD",
                              direction="horizontal", size = "sm", justified =T),
            
            sliderInput("width_slider", "Bar width",
                        min = 0, max = 1, value = 0.8, step = 0.01
            ),
            
            sliderInput("size_slider", "Points size",
                        min = 0, max = 6, value = 2.5, step = 0.1
            ),

            sliderInput("alpha_slider", "Points opacity",
              min = 0, max = 1, value = 1, step = 0.01 
            )
            ) # cinditional panel
            ), # div
        div(id="2",
            conditionalPanel(condition="input.conditionedPanels==2", # POINTS
                             wellPanel(
                               tags$small("set up random seed value for reproducible data pionts arrangement"),
                               splitLayout(
                                 shiny::actionButton(inputId = "get_seed", label = HTML(paste("re-arrange<br>data points"))),
                                 numericInput("seed_", label = "random seed", min = 0, max = 100, value = 42)
                               )),
            selectInput(inputId = "point_shape", label = "Point shape", choices = 0:25, selected = 16),
            plotOutput("ShapePlot")
            ) # conditional panel
            ), # div
            
        div(id="3",
            conditionalPanel(condition="input.conditionedPanels==3", #COLOUR
                             wellPanel(
                             h4("Select colour:"),
                             colourpicker::colourInput("col", "Primary points colour", value = "blue", palette = "limited"),
                             colourpicker::colourInput("col2", "Secondary points colour", value = "red", palette = "limited"),
                             
                             hr(), # color palette input
                             pickerInput( inputId = "col_palette", label = HTML("Colour scale<br><span style='font-weight:normal'><small>works with divergent and custom styles</small></span>"),
                               choices = colors_pal, selected = "Paired", width = "100%",
                               choicesOpt = list(
                                 content = sprintf(
                                   "<div style='width:100%%;padding:5px;border-radius:4px;background:%s;color:%s'>%s</div>",
                                   unname(background_pals), colortext_pals, names(background_pals)
                                 )
                               )
                             ),
                             uiOutput("Colorblind"),
                             checkboxInput("reverse", "reverse color scale", value = FALSE))
            ) # conditional panel
            ), # div
            
        div(id="4",
            conditionalPanel(condition="input.conditionedPanels==4", # AXIS
                             HTML(paste("<h4>Axes labels</h4>")),
                             numericInput(inputId = "axis_text_size", label = "Axis text size", min = 0, max = 1000, value = 12),
                             textInput(inputId = "x_label", label = "X axis label", value = "", placeholder = "Enter text to be used as axis title" ),
                             textInput(inputId = "y_label", label = "Y axis label", value = "", placeholder = "Enter text to be used as axis title" ),
                             numericInput(inputId = "axis_label_size", label = "Axis label size", min = 0, max = 1000, value = 14),
                             HTML(paste("<h4>Y axis values</h4>")),
                             splitLayout(
                             uiOutput("inp_lim_max"),
                             uiOutput("inp_lim_min")),
                             uiOutput("inp_y_div"),
                             checkboxInput("h_lines", "show horizontal lines", value = FALSE)
                             
            ) # conditional panel
            ) # div
            ), # sidebarPanel
      
      # Show a plot of the generated distribution
      mainPanel(tags$style("body {background-color: #EBEBEB; }"), # set bg color helps with plot resizing
        tabsetPanel(id="main_tabs",
          tabPanel("Plot",
                 busyIndicator("rendering plot...",wait = 3000), # prograss idicator to use when it rendering takes than 3 sec
                 jqui_resizable(
                   plotOutput('distPlot'),
                        options = list(minHeight = 200, minWidth = 200,
                                       distance = 30,
                                       delay = 100,
                                       autoHide = T
                                       )), #jqui_resizabled( options = list(aspectRatio = TRUE))
        uiOutput("jqui")
        ), # tab
        tabPanel("Read Me", 
                  fillPage(padding = 0, title = NULL, bootstrap = F, theme = NULL,
                  wellPanel(style = "background-color: #ffffff; overflow-y:scroll; max-height: 750px;",
                  includeHTML("Read_me_app.html")))
                 ),
        tabPanel("Data Editing",br(),
                 p(shiny::actionButton("applyBtn", "Apply changes"),
                 downloadButton("saveBtn", "Download data"),
                 shiny::actionButton("resetBtn", "Reset")),
                 fillPage(padding = 0, title = NULL, bootstrap = F, theme = NULL,
                          wellPanel(style = "background-color: #ffffff; overflow-y:scroll; max-height: 750px;",
                                    rHandsontableOutput("RH")))
                 
                 )
        # ## ======== remove comment from here to enble ggEdit panel
        # ,
        # tabPanel("Edit plot with ggEdit",
        #          br(),
        #          p("This plot editing module uses", tags$a(href="https://github.com/metrumresearchgroup/ggedit", "ggEdit"),
        #           "package and is experimental. To start editing load plot with", tags$b("Load plot"), "button. 
        #           Editing of error bars will most likely result in incorrect position.
        #           Editing of points will most likely result in module crash.
        #           However you ca still do couple of things here like:",br(),
        #           "- add outline to bars -> Update Bar plot layers -> colour",br(),
        #           "- add legend Update Plot Theme -> legend -> position",br(),
        #           "although most likely you will have to change text size: Update Plot Theme -> text -> size = 2 
        #           and also change size of legend text Plot Theme -> text -> size = 10 etc."),
        #          br(),
        #          fluidRow(
        #            column(6,
        #                   shiny::actionButton("load_p", "Load plot")),
        #            column(2,
        #                   selectInput(inputId = "out_format2", label = NULL, # this removes the label
        #                               width = '100%', choices = c("pdf", "svg", "eps", "ps"), selected = "pdf")),
        #            column(4,
        #                   downloadButton("DL_plot2", label = "Download plot"))
        #          ),
        #          br(),
        #          ggEditUI("ggEdit")
        #          ) # tab panel - ggEdit
        # ## ========= remove comment to here
                ) # tabsetPanel
      ))
   )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ### error message
  e_message <- HTML("<small align=left><br>Please make sure that:<ul>
                                                    <li>fields are separated by white space characters
                    (that is one or more spaces, tabs, newlines or carriage returns)</li>
                    <li>first row consists of column names</li>
                    <li>first column consists of sample names (repeated names indicate multiple measurements for given sample)</li>
                    <li>row or column names do not include whitesapce charatcers (space, tab etc.)</li>
                    <li>decimal separator is setup correctly</li></small>")
  
  ### TO DO: consider for some values session$userData$flag <- value instead of reactive
  # initiate reactive values
  r_values <- reactiveValues(plot_out = NULL)
  r_values$data_g1 <-NULL
  r_values$y_limits <- NULL
  r_values$y_div <- NULL
  r_values$DS_ <- NULL
  
  ### functions
  load_sample_data <- function(){
    r_values$data_g1 <- read.table("Sample_data.txt", check.names=T, sep="", fill = T,
                                   header = T, stringsAsFactors = F,row.names = NULL)
      #fread("Sample_data.txt",data.table=F, check.names=T) 
  }
  
  load_user_data <- function() {
    sep_ <- ifelse(input$dot_comma==T,".",",")
    data_file <- input$data_g
    tryCatch({
      r_values$data_g1 <- read.table(data_file$datapath, 
                                     check.names=T, sep="", fill = T, 
                                     dec= sep_, header = T, stringsAsFactors = F, row.names = NULL)
        #fread(data_file$datapath, data.table=F, dec = sep_, check.names=T)
    },
    warning=function(w){
      #alert
      sendSweetAlert(session = session,
                     title = "Error",
                     text = tags$span("Error occured while opening file.", e_message),
                     type = "error",
                     closeOnClickOutside = F)
      #action
      load_sample_data()
      reset('data_g') # Shinyjs
    },
    error=function(e){
      #alert
      sendSweetAlert(session = session,
                     title = "Error",
                     text = tags$span("Error occured while opening file.", e_message),
                     type = "error",
                     closeOnClickOutside = F)
      #action
      load_sample_data()
      reset('data_g') # Shinyjs
      }
    )
  }
  
  error_handle <- function(){
    # show alert
    sendSweetAlert(session = session,
                   title = "Error",
                   text = tags$span("Error occured while loading data",
                                    tags$br(),"Re-loading dataset", e_message), #re-loading dataset
                   type = "error",
                   html=T,
                   closeOnClickOutside = F)
    # take action
    if (!is.null(input$data_g)) {
      load_user_data()
    } else {
      load_sample_data() # loading of default dataset
    }}

  data_processed <- function(){
    req(r_values$data_g1, input$err_select) # needs r_values$data_g1 for calculation
    
    tryCatch({
    cess_ <- preprocess(r_values$data_g1, input$err_select) # process file using helper function
    limits_base <- axis_limits(cess_$df) # limits - needed for y axis limits

    out_ <- list(y_limits=limits_base$limits, # get data series No - needed for custom color scale for bars
                 y_div=limits_base$div,
                 DS_=cess_$series_No)},
    
    error=function(e){error_handle()},
    warning=function(w){error_handle()
      }
  )
    } # data_processed
  
  data_operations <- function(){
    #calculate values for data
    temp_list <- data_processed()
    # transfer objects to the reactiveValues list
    for (i in names(temp_list)){r_values[[i]] <- temp_list[[i]]}
    # load vlaues to the input fields
    updateNumericInput(session, "Y_min_lim", value = r_values$y_limits[1])
    updateNumericInput(session, "Y_max_lim", value = r_values$y_limits[2])
    updateNumericInput(session, "Y_div", value = r_values$y_div)
  }
  
  ### /functions
  
  # show warning when mobile device is detected
  observeEvent(input$isMobile, {
    if (input$isMobile){
      sendSweetAlert(session = session,
                     title = "Warning",
                     text = "This app is developed for desktop computers and 
                                 will most likely not work properly on mobile device.",
                     type = "warning",
                     btn_labels = "I have been warned",
                     closeOnClickOutside = F)
    }
  })

  # main tab panel observer hide show UI parts
  observeEvent({
    #input$conditionedPanels # no longer necessary
    input$main_tabs},
    {
    if (input$main_tabs %in% c("Data Editing", "Read Me", "Edit plot with ggEdit")){
      jqui_hide(paste0("#",input$conditionedPanels), effect = NULL) # hide current panel
      jqui_hide("#conditionedPanels", effect = NULL) # hide panels tabs
    }
      
    if (input$main_tabs == "Plot"){
      jqui_show(paste0("#",input$conditionedPanels), effect = NULL)
      jqui_show("#conditionedPanels", effect = NULL) # show panels tabs
    }
  })
  
  # react to data changes by recalculating axis limits etc
  observeEvent(r_values$data_g1, {
    data_operations()
  })
    
  # read file
  observeEvent(input$data_g, {
    
    if (is.null(input$data_g)){
      load_sample_data() # if no file provided then load Sample data
    } else {
      load_user_data() # if file provided load user data
    }
  }, ignoreNULL = F) # ,ignoreNULL = T - do not attempt to process if there is no data
  
  # observe error change
  observeEvent(input$err_select, {
    
    data_operations() #re-calculate values for new error type

  }, ignoreNULL = T)
  
  ### renderUI output that is based on the data - AXIS
  output$inp_lim_max <- renderUI({
    req(r_values$y_limits)
    # Y axis max
    numericInput(inputId = "Y_max_lim", label = "Maximum",
                 min = extendrange(r_values$y_limits, f=100)[1],
                 max = extendrange(r_values$y_limits, f=100)[2], 
                 value = prettyNum(max(r_values$y_limits)),
                 step = prettyNum(abs(diff(extendrange(r_values$y_limits, f=100)))/10000))
  })
  
  output$inp_lim_min <- renderUI({
    req(r_values$y_limits)
    # Y axis min
    numericInput(inputId = "Y_min_lim", label = "Minimum",
                 min = extendrange(r_values$y_limits, f=100)[1],
                 max = extendrange(r_values$y_limits, f=100)[2], 
                 value = prettyNum(min(r_values$y_limits)),
                 step = prettyNum(abs(diff(extendrange(r_values$y_limits, f=100)))/10000))
  })
  output$inp_y_div <- renderUI({
    req(r_values$y_div)
    # ticks spacing
    numericInput(inputId = "Y_div", label = "Ticks spacing",
                 min = r_values$y_div/100,
                 max = r_values$y_div*1000,
                 value = r_values$y_div,
                 step = r_values$y_div/100)
  })
  ### / renderUI output that is based on the data - AXIS

### <Draw main plot>
output$distPlot <- renderPlot({
  
  #if else for Y axis limits
  validate(need(input$Y_min_lim, label= 'Y axis minimum'))
  validate(need(input$Y_max_lim, label= 'Y axis maximum'))
  
  if (length(c(input$Y_min_lim, input$Y_max_lim))!=2 ){
    limity_ <- r_values$y_limits # if there is not user input take estimation
  } else {
    limity_ <- c(input$Y_min_lim, input$Y_max_lim) # else take user input
  }
  
  validate(need(input$Y_div, label= 'Ticks spacing'))
  # if else for ticks spacing (length(NULL) = 0)
  if (length(input$Y_div)!= 1){
    spacing_ <- r_values$y_div
  } else {
    validate(need(input$Y_div>(r_values$y_div/50), 'Tick spacing value is too small'), errorClass = "warning")
    spacing_ <- input$Y_div
  }
  
  #if else for reverse color scale
  col_scale <- if(input$reverse) {
    rev(brewer.pal(r_values$DS_, input$col_palette))
  } else {
    brewer.pal(r_values$DS_, input$col_palette)
  }
  
  r_values$plot_out <- 0
  ### setting seed
  validate(need(input$seed_, label= 'random seed'))
  set.seed(input$seed_) # we have to set up seed here otherwise it will change when changing slider or other input
  # export plot to variable (for saving it in the downloadHandler below)
  tryCatch({
    
    temp_plot <- bar_plt_points(r_values$data_g1, # the data frame
                                style = input$style_select,
                                width = input$width_slider,
                                point_size = input$size_slider,
                                a = input$alpha_slider,
                                col = input$col,
                                col2 = input$col2,
                                seed = input$seed_,
                                y_limits = limity_,
                                y_div = spacing_,
                                col_scale = col_scale,
                                Error = input$err_select,
                                ps = as.numeric(input$point_shape))
    
    temp_plot <- add_error_bars(temp_plot,
                                width = input$width_slider)
    
    temp_plot <- manipulate_axis(temp_plot,
                                 y_l = input$y_label,
                                 x_l = input$x_label,
                                 xy_ts = input$axis_text_size,
                                 xy_ls = input$axis_label_size)
    
    r_values$plot_out <- add_h_lines(temp_plot,
                                     h_lines = input$h_lines)
    
  }, error=function(e){
    # show alert
    sendSweetAlert(session = session,
                   title = "Error",
                   text = tags$span("Error occured while rendering plot",tags$br(),"Re-loading sample dataset",
                                    tags$br(), e_message), #re-loading dataset
                   type = "error",
                   html=T,
                   closeOnClickOutside = F)
    # take action
    load_sample_data()
    reset("data_g") # Shinyjs
    }) # tryCatch

  ### Execute plot
  r_values$plot_out
})
### </Draw main plot>
  
  # print size of plot
  output$jqui <- renderUI({
    ppp <- input$distPlot_size
    tags$small(paste0("drag to resize plot (current size: ",as.integer(ppp$width), " x ", as.integer(ppp$height), ")"))
  })
  
  # check and render text if Colour blind friendly
  output$Colorblind <- renderUI({
    if(brewer.pal.info %>% subset(rownames(.) == input$col_palette) %>% .$colorblind) {
      txt_ <- "Yes"
    } else {
      txt_ <- "No"
    }
    HTML(paste("<p>Is colour scale colour blind frienndly?<b>", txt_, "</b></p>"))
  })
  
  # Set random seed code
  observeEvent(input$get_seed, {
    # set new seed
    set.seed(Sys.time()) # unset seed
    seed_new <- sample(1:100, 1)
    # update numeric input field
    updateNumericInput(session, "seed_", value = seed_new)
  })

  # change possible choices based on selected style
  observeEvent(input$style_select, {
     if (input$style_select %in% c("basic", "custom")) {
       updateSelectInput(session, inputId = "point_shape",
                         choices = c(0:25),
                         selected = input$point_shape)
       r_values$pch_ <- c(0:25)
       
     } else if (input$style_select %in% c("bicolour", "divergent")) {
       updateSelectInput(session, inputId = "point_shape",
                         choices = c(0:20),
                         selected = ifelse(input$point_shape<=20, input$point_shape, 16))
       r_values$pch_ <- c(0:20)
       
     } else if (input$style_select %in% c("greyscale")) {
       updateSelectInput(session, inputId = "point_shape",
                         choices = c(21:25),
                         selected = ifelse(input$point_shape>=21, input$point_shape, 21))
       r_values$pch_ <- c(21:25)
     }})

  # plot for POINTS tab with helper function
  output$ShapePlot<- renderPlot({
    point_plt(ipch=r_values$pch_, col = input$col, bg = input$col2, coltext = "black", cex = 2, cextext = 1.2, lwd = 1.75)
  })

  ### ggEdit 
  observeEvent(input$load_p, {
    ppp <- input$distPlot_size
    set.seed(input$seed_) # we have to set up seed here otherwise it will change when changing slider or other input
    # but it is not perfect sometimes point get rearranged when editing them with ggEdit
    # export variable for saving purposes
    r_values$out_ggE <- callModule(ggEdit,"ggEdit", obj=reactive(list(r_values$plot_out)), width=ppp$width,  height=ppp$height)
})
  
  ### Data Edit
  output$RH <- renderRHandsontable({
    DF <- r_values$data_g1 # tranfer the data to the internal variable - we want to work on copy and then apply changes with the button
    
    rhandsontable(DF, height = "75%", width = "90%",  rowHeaders = NULL, useTypes = F) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>% 
      hot_cols(manualColumnMove = F, manualColumnResize = T, fixedColumnsLeft=1) %>% # disable column rearangement as it does not result in data change 
      hot_context_menu(allowRowEdit = T, allowColEdit = F) # disable adding columns as it results in crash in hot_to_r function
  })
  ### Data Edit - data saving
  output$saveBtn = downloadHandler(
    filename = function(){
      paste0("data.csv")
    },
    content = function(file) {
      write.table(hot_to_r(input$RH), file = file, quote=F, sep = "\t")
      }
  )
  ### Data Edit - attempt to apply changes
  observeEvent(input$applyBtn, {
    r_values$data_g1 <- hot_to_r(input$RH)
  })
  
  ### Data Edit - attempt to apply changes
  observeEvent(input$resetBtn, {
    if (is.null(input$data_g)){
      load_sample_data() # if no file provided then load Sample data
    } else {
      load_user_data() # if file provided load user data
    }
  })

  ### Attempt to save plot
  output$DL_plot = downloadHandler(
    filename = function(){
      paste0("plot.", input$out_format)
    },
    content = function(file) {
                        ppp <- input$distPlot_size # get the plot dimentions from shinyjqui
                        ggsave(r_values$plot_out, filename = file, 
                        device = input$out_format, 
                        width = ppp$width/100, 
                        height = ppp$height/100, 
                        units = "in", dpi=100)}
  )
  
  ### Attempt to save plot ggEdit
  output$DL_plot2 = downloadHandler(
    filename = function(){
      paste0("plot.", input$out_format2)
    },
    content = function(file) {
      
      # do mumbo-jumbo to get the edited plot here:
      # replace ggplot layers with updated layers and the same for the theme
      if (all(c("UpdatedThemes", "UpdatedLayers") %in% names(r_values$out_ggE()))) {
        r_values$plot_out %>% rgg("", newLayer=r_values$out_ggE()$UpdatedLayers) +
          r_values$out_ggE()$UpdatedThemes -> updated_plot
      } else if ("UpdatedLayers" %in% names(r_values$out_ggE())){
        r_values$plot_out %>% rgg("", newLayer=r_values$out_ggE()$UpdatedLayers) -> updated_plot
      } else if ("UpdatedThemes" %in% names(r_values$out_ggE())){
        r_values$plot_out + r_values$out_ggE()$UpdatedThemes -> updated_plot
      } else {
        r_values$plot_out -> updated_plot
      }
      
      ppp <- input$distPlot_size # get the plot dimentions from shinyjqui
      ggsave(updated_plot, filename = file, 
             device = input$out_format2,
             width = ppp$width/100, 
             height = ppp$height/100, 
             units = "in", dpi=100)}
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


