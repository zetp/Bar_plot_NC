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
library(ggedit) # editing of plot
library(rhandsontable) # editing of data
library(shinysky) # busy spinner with delay timer

library(dplyr)
library(tidyr)

library(ggplot2)
library(svglite) # required for svg output

library(colourpicker)
library(RColorBrewer)

#options(shiny.port=7777)
#options(shiny.host="192.168.136.219")

source("Nature_Comm_bar.R")

### here is code to make color scale picker (palette picker) UI input
### it is taken from shinyWidgets website: https://dreamrs.github.io/shinyWidgets/articles/palette_picker.html

# List of palettes
colors_pal <- lapply(
  X = split(
    x = brewer.pal.info,
    f = factor(brewer.pal.info$category, labels = c("ging", "Qualitative", "Sequential"))
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

#'  TO DO:
#'  
#'  ADD error on drawing plot or processing data - as with Data Editign it can be easily broken
#'  
#'  on lauch plot executes 4 times - find why and fix it, maybe it is about ignoreInit?
#'  
#'  add some loading animation or spinner
#'  
#'  make READ ME txt
#'  Disable the point tab if mode is greyscale and other stuff like that?
#'  Hide side panel when not in Main tab
#'  hide show tab - UI tabsetPanel(id="xxx", tabPanel("Main")), Server if else showTab(inputId = "xxx", target = "Main", ) hideTab
#'#'
#'  Keep in mind: flowLayout (Lays out elements in a left-to-right, top-to-bottom arrangement )
#'  verticalLayout - vertical placement
#'  splitLayout - horizontal placemnt
#'  
#'  Shiny Cavas for resize - maybe better than shinyjqui??? https://github.com/metrumresearchgroup/shinyCanvas
#'  
#'  Use error handling for data processing pathway:
#'  https://www.rforge.net/doc/packages/testit/assert.html
#'  https://cran.r-project.org/web/packages/assertthat/README.html
#'  https://www.r-bloggers.com/error-handling-in-r/
#'  https://stat.ethz.ch/R-manual/R-devel/library/tools/html/assertCondition.html
#'  

# Define UI for application
ui <- fluidPage(
  
   # Application title
  titlePanel("Bar plots with indicated measurements points", windowTitle = "Bar plots"),
   
   # Sidebar with inputs 
   sidebarLayout(
      sidebarPanel(
        
        tabsetPanel(type="pills",
          tabPanel("Main", value=1), 
          tabPanel("Points", value=2),
          tabPanel("Colors", value=3),
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
            
            conditionalPanel(condition="input.conditionedPanels==1", # MAIN
                             
            fluidRow(
              column(6, tags$b(
            radioGroupButtons(inputId = "style_select", label = "Choose style",
                  choices = c("basic","bicolor","greyscale","divergent","custom"),selected="basic",
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
                        min = 0, max = 1, value = 0.8, step = 0.05
            ),
            
            sliderInput("size_slider", "Points size",
                        min = 0, max = 6, value = 2.5, step = 0.1
            ),

            sliderInput("alpha_slider", "Points opacity",
              min = 0, max = 1, value = 1, step = 0.1 
            )
            ),
            
            conditionalPanel(condition="input.conditionedPanels==2", # POINTS
                             wellPanel(
                               tags$small("set up random seed value for reproducible data pionts arrangement"),
                               splitLayout(
                                 actionButton(inputId = "get_seed", label = HTML(paste("re-arrange<br>data points"))),
                                 numericInput("seed_", label = "random seed", min = 0, max = 100, value = 42)
                               )),
            selectInput(inputId = "point_shape", label = "Point shape", choices = 0:25, selected = 16),
            plotOutput("ShapePlot")
            ),
            
            conditionalPanel(condition="input.conditionedPanels==3", #COLOUR
                             wellPanel(
                             h4("Select colour:"),
                             colourInput("col", "Primary points colour", value = "blue", palette = "limited"),
                             colourInput("col2", "Secondary points colour", value = "red", palette = "limited"),
                             
                             hr(), # color palette input
                             pickerInput( inputId = "col_palette", label = "Colour palette",
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
            ),
            
            conditionalPanel(condition="input.conditionedPanels==4", # AXIS
                             HTML(paste("<h4>Axes labels</h4>")),
                             textInput(inputId = "x_label", label = "X axis label", value = "", placeholder = "Enter text to be used as axis title" ),
                             textInput(inputId = "y_label", label = "Y axis label", value = "", placeholder = "Enter text to be used as axis title" ),
                             numericInput(inputId = "axis_label_size", label = "axis label size", min = 0, max = 1000, value = 14),
                             numericInput(inputId = "axis_text_size", label = "axis text size", min = 0, max = 1000, value = 12),
                             HTML(paste("<h4>Y Axis values</h4>")),
                             uiOutput("inp_lim_max"),
                             uiOutput("inp_lim_min"),
                             uiOutput("inp_y_div"),
                             checkboxInput("h_lines", "show horizontal lines", value = FALSE)
                             
            )
            ), # sidebarPanel
      
      # Show a plot of the generated distribution
      mainPanel(tags$style("body {background-color: #EBEBEB; }"), # set bg color helps with plot resizing
        tabsetPanel(
          tabPanel("Plot",
                 busyIndicator("rendering plot...",wait = 3000), # prograss idicator to use when it rendering takes than 3 sec
                 jqui_resizable(
                   plotOutput('distPlot'), # add spinner progress indicator
                        options = list(minHeight = 200, minWidth = 200,
                                       distance = 30,
                                       delay = 100,
                                       autoHide = T
                                       )), #jqui_resizabled( options = list(aspectRatio = TRUE))
        uiOutput("jqui")
        ), # tab
        tabPanel("Read Me", br(),
                 p("what is it?, 
                   about presets (options tailored for each preset)
                   options explamation,
                   data editign,
                   ggedit
                   aknowlegements: people and packages")),
        tabPanel("Data Editing",br(),
                 p(actionButton("applyBtn", "Apply changes"),
                 downloadButton("saveBtn", "Download data")),
                 rHandsontableOutput("RH")
                 ),
        tabPanel("Edit plot with ggEdit",
                 br(),
                 fluidRow(
                   column(6,
                 actionButton("load_p", "Load plot")),
                   column(2,
                          selectInput(inputId = "out_format2", label = NULL, # this removes the label
                                      width = '100%', choices = c("pdf", "svg", "eps", "ps"), selected = "pdf")),
                   column(4,
                          downloadButton("DL_plot2", label = "Download plot"))
                 ),
                 br(),
                 ggEditUI("ggEdit")
                 
                 ## TO DO:
                 ## add this is editing based on ggEdit module that is made by ...
                 ## package not done by me
                 ## see manual: https://metrumresearchgroup.github.io/ggedit/
                 ## add change text siez and make legend, + add to make outline of bars
                 ## to add legend go and position from none to one of the described then click apply
                 ## add may break error bars
                 ## add point editing might not work
                 )
                ) # tabsetPanel
      ))
   )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ### TO DO: consider for some values session$userData$flag <- value instead of reactive
  # initiate reactive values
  r_values <- reactiveValues(plot_out = NULL)
  r_values$data_g1 <-NULL
  r_values$y_limits <- NULL
  r_values$y_div <- NULL
  r_values$DS_ <- NULL
  
  data_processed <- reactive({
    req(r_values$data_g1, input$err_select) # needs r_values$data_g1 for calculation
    cess_ <- preprocess(r_values$data_g1, input$err_select) # process file using helper function
    limits_base <- cess_$df %>% axis_limits() # limits - needed for y axis limits
    out_ <- list(y_limits=limits_base$limits, # get data series No - needed for custom color scale for bars
                 y_div=limits_base$div,
                 DS_=cess_$series_No)
    return(out_)
  })
  
  # read file
  observeEvent(input$data_g, {
    
    # get decimal separator form switch
    sep_ <- ifelse(input$dot_comma==T,".",",")
    
    data_file <- input$data_g
    
    # read file
    # TO DO: here add error handler as with wrong separator the app chrashes
    # also identify where it fails
    
    r_values$data_g1 <- fread(data_file$datapath,data.table=F, dec = sep_, check.names=T)
    
    #calculate values for data
    temp_list <- data_processed()
    # transfer objects to the reactiveValues list
    for (i in names(temp_list)){r_values[[i]] <- temp_list[[i]]}
    # load vlaues to the input fields
    updateNumericInput(session, "Y_min_lim", value = r_values$y_limits[1])
    updateNumericInput(session, "Y_max_lim", value = r_values$y_limits[2])
    updateNumericInput(session, "Y_div", value = r_values$y_div)

  }, ignoreNULL = T) # ,ignoreNULL = T - do not attempt to draw if there is no data
  
  # observe error change
  observeEvent(input$err_select, {
    
    #re-calculate values for data
    temp_list <- data_processed()
    # transfer objects to the reactiveValues list
    for (i in names(temp_list)){r_values[[i]] <- temp_list[[i]]}
    # load vlaues to the input fields
    updateNumericInput(session, "Y_min_lim", value = r_values$y_limits[1])
    updateNumericInput(session, "Y_max_lim", value = r_values$y_limits[2])
    updateNumericInput(session, "Y_div", value = r_values$y_div)
    
  }, ignoreNULL = T)
  
  ### renderUI output that is based on the data - AXIS
  output$inp_lim_max <- renderUI({
    req(r_values$y_limits)
    # Y axis max
    numericInput(inputId = "Y_max_lim", label = "Maximum:",
                 min = extendrange(r_values$y_limits, f=100)[1],
                 max = extendrange(r_values$y_limits, f=100)[2], 
                 value = max(r_values$y_limits),
                 step = abs(diff(extendrange(r_values$y_limits, f=100)))/10000)
  })
  
  output$inp_lim_min <- renderUI({
    req(r_values$y_limits)
    # Y axis min
    numericInput(inputId = "Y_min_lim", label = "Minimum:",
                 min = extendrange(r_values$y_limits, f=100)[1],
                 max = extendrange(r_values$y_limits, f=100)[2], 
                 value = prettyNum(min(r_values$y_limits)),
                 step = prettyNum(abs(diff(extendrange(r_values$y_limits, f=100)))/10000))
  })
  output$inp_y_div <- renderUI({
    req(r_values$y_div)
    # ticks spacing
    numericInput(inputId = "Y_div", label = "Ticks spacing:",
                 min = 0,
                 max = r_values$y_div*1000,
                 value = r_values$y_div,
                 step = r_values$y_div/100)
  })
  ### / renderUI output that is based on the data - AXIS


### <Draw main plot>
output$distPlot <- renderPlot({
  
  # if no file provided then load Sample data
  if (is.null(r_values$data_g1)){
    r_values$data_g1 <- fread("Sample_data.txt",data.table=F, check.names=T) # no need to set searator
    #calculate values for sample data
    temp_list <- data_processed()
    # transfer objects to the reactiveValues list
    for (i in names(temp_list)){r_values[[i]] <- temp_list[[i]]}
  }
  
  #if else for Y axis limits
  if (length(c(input$Y_min_lim, input$Y_max_lim))!=2 ){
    limity_ <- r_values$y_limits # if there is not user input take estimation
  } else {
    limity_ <- c(input$Y_min_lim, input$Y_max_lim) # else take user input
  }
  
  # if else for ticks spacing (length(NULL) = 0)
  if (length(input$Y_div)!= 1){
    spacing_ <- r_values$y_div
  } else {
    spacing_ <- input$Y_div
  }
  
  #if else for reverse color scale
  col_scale <- if(input$reverse) {
    rev(brewer.pal(r_values$DS_, input$col_palette))
  } else {
    brewer.pal(r_values$DS_, input$col_palette)
  }
  
  ### setting seed
  set.seed(input$seed_) # we have to set up seed here otherwise it will change when changing slider or other input
  # export plot to variable (for saving it in the downloadHandler below)
  r_values$plot_out <- bar_plt_points(
    r_values$data_g1,
    style = input$style_select,
    width = input$width_slider,
    a = input$alpha_slider,
    point_size = input$size_slider,
    seed = input$seed_,
    y_l = input$y_label,
    x_l = input$x_label,
    xy_ts = input$axis_text_size,
    xy_ls = input$axis_label_size,
    y_limits = limity_,
    y_div = spacing_,
    col = input$col,
    col2 = input$col2,
    Error = input$err_select,
    h_lines = input$h_lines,
    # commented line uses rep to make input of ifelse equal to the desired output.
    col_scale = col_scale, #ifelse(rep(input$reverse, DS_), rev(brewer.pal(DS_, input$col_palette)), brewer.pal(DS_, input$col_palette)),
    ps = as.numeric(input$point_shape)
  )
  
  ### Execute plot
  r_values$plot_out
})
### </Draw main plot>

  
  # print size of plot
  output$jqui <- renderUI({
    ppp <- input$distPlot_size
    tags$small(paste0("drag to resize plot (current size: ",as.integer(ppp$width), " x ", as.integer(ppp$height), ")"))
  })
  # check render text if Color blind friendly
  output$Colorblind <- renderUI({
    HTML(paste("<p>Color blind friendly:<b>", 
             (brewer.pal.info %>% subset(rownames(.) == input$col_palette) %>% .$colorblind), "</b></p>"))
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
  r_values$points_C <- observe(
     if (input$style_select %in% c("basic", "custom")) {
       updateSelectInput(session, inputId = "point_shape",
                         choices = c(0:25),
                         selected = input$point_shape)
       r_values$pch_ <- c(0:25)
       
     } else if (input$style_select %in% c("bicolor", "divergent")) {
       updateSelectInput(session, inputId = "point_shape",
                         choices = c(0:20),
                         selected = ifelse(input$point_shape<=20, input$point_shape, 16))
       r_values$pch_ <- c(0:20)
       
     } else if (input$style_select %in% c("greyscale")) {
       updateSelectInput(session, inputId = "point_shape",
                         choices = c(21:25),
                         selected = ifelse(input$point_shape>=21, input$point_shape, 21))
       r_values$pch_ <- c(21:25)
     })

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
      hot_cols(manualColumnMove = T, manualColumnResize = T, fixedColumnsLeft=1) %>% 
      hot_context_menu(allowRowEdit = T, allowColEdit = F) # disable adding columns as it results in crash
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
    
    ### now we have to recalculate some stuff based on new data
    #calculate values for sample data
    temp_list <- data_processed()
    # transfer objects to the reactiveValues list
    for (i in names(temp_list)){r_values[[i]] <- temp_list[[i]]}
    # load values to input fields
    updateNumericInput(session, "Y_min_lim", value = r_values$y_limits[1])
    updateNumericInput(session, "Y_max_lim", value = r_values$y_limits[2])
    updateNumericInput(session, "Y_div", value = r_values$y_div)
    
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
      # replace ggplot layers with updated layers and so for the theme
      # refer to r_values$out_ggE() with () as it is reactive value
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


