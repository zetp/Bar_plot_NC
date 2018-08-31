library(dplyr)
library(tidyr)
library(data.table) # for fread
library(ggplot2)
library(RColorBrewer)
library(scales) # for oob to work

### Things to consider: 
### is numeric -change albo numeric jest typeof i class
### DS_ as not No of column but No of columns which contain numeric data - sum sapply is numeric

### helper functions:

#' preprocessing data function - this is helper function for bar_plt_points
#' not intended to be used separately
#' @param x - data
#' @param E - type of error to calculate (SD or SEM)
#' this function will make sure that data is formatted proprely for further pipeline
#' 
#' @return return list with two objects:
#' "df" - data frame with formatted data
#' "series_No" - number of data series detected in the data provided
#' @export
#'
#' @examples iris %>% dplyr::select(Species, everything()) %>% preprocess()
#' 

preprocess <- function(x, E="SD"){

  #na<- x[1,] # use forst row as names
  na <- names(x)
  names(x) <- c("samples", na[2:length(na)]) # make sure that 1st column in named samples as we will use this name in the code
  #x <- x[-1,] # delete first row first row
  
  #x[2:length(x)] <- x[2:length(x)] %>% lapply(as.numeric) # change values to numeric
  
  x$samples <- as.character(x$samples) # make sure that sample names are txt not numbers
  
  DS_ <- length(na) - 1 # how many data series do we have?
  
  sem <- function(x){sd(x)/sqrt(length(x))}
  
  x <- x %>% melt() %>% filter(complete.cases(.)) # complete.cases - removes NA
  
  data_g1 <- x %>% 
    group_by(variable, samples) %>%
    mutate_at("value", funs(mean, sd, sem)) %>% rename_("err_"=tolower(E), "mean_" = "mean") # tolower - SD to sd cause col names as function names

  # now change NA to zero in err_ column 0 the NA would be a result of single repetition
  data_g1$err_[is.na(data_g1$err_)] <- 0
  
  data_g1$samples <- factor(x$samples, levels = unique(x$samples)) # maintain order of data series as in input file on the plot
  
  out_ <- list(df=data_g1, series_No=DS_)
  return(out_)
}

#' this function determines Y axis limits - this is helper function for bar_plt_points
#' this function only works on pre defined names in provided data frame
#' and is intended to be use in pipline with output of preprocess() function
#' NOT for general use
#'
#' @param x - properly formatted data frame (output of preprocess funtion)
#' @param p - percent of margin to extend over extreme values
#'
#' @return return list with three objects:
#' "limits" - limits of Y axis with given % margin (list of two values)
#' "min" - minimum value in provided data (does not include the margin)
#' "max" - minimum value in provided data (does not include the margin)
#' @export
#'
#' @examples iris %>% dplyr::select(Species, everything()) %>% preprocess() %>% .$df %>% axis_limits()
#' 

axis_limits <- function(x, p = 5){ 
  
  max_ <- max(max(x$value), max(x$mean_+x$err_)) # max is the point value or mean+SD whichever greater
  min_ <- min(min(x$value), min(x$mean_-x$err_)) # set min, for negative values
  
  if (min_>0){min_ <- 0} # for positive values set minimum to zero
  
  mult_ <- (p/100)*abs(diff(c(min_, max_)))
  
  if (min_==0){  # set limins to have n% magrgin around extreme values
    y_limits <- c(min_, max_+mult_)
  } else {
    y_limits <- c(min_-mult_, max_+mult_)
  }

  ticks_ <- pretty(y_limits, n = 10) # calculate ticks positions
  div_y <- diff(ticks_)[1] # calculate ticks spacing
  
  lim_ <- list(
    limits=y_limits,
    min=min_,
    max=max_,
    ticks=ticks_,
    div=div_y
    )
  
  return(lim_)
}

#' Create bar charts plot with indicated measuremnts points
#' As per requirement of Nature Communications journal:
#' "Please ensure that the corresponding dot plots are overlaid in the bar charts"
#' 
#' @param x this is the data.frame containg data to be plotted in following format:
#'         conc1 conc2 conc3 conc4
#' sample1 15735 15043 13755 10373
#' sample1 15795 15249 13537 10484
#' sample1 16222 15001 13678 11035
#' sample2 19695 17940 16781 14820
#' sample2 21084 20716 17762 14149
#' sample2 21398 20663 18586 15427
#' sample3 39442 28614 22010 17504
#' sample3 41434 29442 22688 17990
#' sample3 40706 28816 21479 17273
#' 
#' above is an exmaple of measurements of 3 samples in 3 repetitions for 4 concentrations
#' 
#' @param style - style of dots (default "basic"):
#' "basic" - single color,
#' "divergent" - divergent colors differnet for each condition,
#' "greyscale" - grayscale version - to aviod color usage and preserve figure in grayscale,
#' "bicolour" - two colors for dots: above the mean (col, see below) and below or equal to the mean (col2, see below),
#' "custom" - lets you customize the color of bars and points,
#' @param width width of bars (as fraction, default 0.8)
#' @param point_size size of point (default 2.5)
#' @param a alpha or transparency (1-0, where 0 = totally transparent, default1)
#' @param col color of the dots (default blue)
#' @param col2 color of the dots below or equal to the mean for "ab" style (default red)
#' @param seed random seed -  it is set up for reproducible position of dots for each
#'  rendering of the image (default 42), try diffecnet numbers for different dots positions
#' @param y_limits limits of Y axis should be two numbers, example c(0,100), if
#'   not defined this function will attempt to estimate best limits automatically
#' @param y_div spacing between Y axis ticks, should be single number, if not
#'   defined this function will attempt to estimate spacing to achieve ~10 ticks automatically
#' @param y_l text for Y axis label
#' @param x_l text for X axis label
#' @param ps points shape (number 1-25). N.B. it is not applicable to greyscale style ("gs")
#' @param xy_ts axis text size
#' @param xy_ls axis label size
#' @param col_scale - color scale for bars,
#' @param Error - type of error to be calculated ("SD" or "SEM")
#' @param h_lines - draw horizontal lines in plot backgroud #'   
#' @return ggplot2 object
#' @export 
#'
#' @examples iris %>% dplyr::select(Species, everything()) %>% bar_plt_points(style = "div", width = 0.7, col = "red", seed = 15,  ps =18)

bar_plt_points <- function(x, # the data frame
           style = "basic",
           width = 0.8,
           point_size = 2.5,
           a = 1,
           col = rgb(0, 0, 1),
           col2 = rgb(1, 0, 0),
           seed = 42,
           y_limits,
           y_div,
           y_l,
           x_l,
           xy_ts,
           xy_ls,
           col_scale = NULL,
           Error = "SD",
           h_lines = F,
           ps = 19) {
    # preprocessing data
    
    out_ <- preprocess(x, E=Error)
  
    data_g1 <-  out_$df
    
    DS_ <- out_$series_No
    
    ### calculate min_ even if y_limits are provided - as it is needed below
    lims_ <- axis_limits(data_g1)
    min_ <- lims_$min
    
    # if limints of y axis are not provided then estimate them
    if(missing(y_limits)){
      
      y_limits <- lims_$limits
      
    } else if (any((length(y_limits) != 2),(class(y_limits) != "numeric"))){ # check if limits have two items that are numbers
      warning("y_limints should consists of two numeric values, for exapmple c(0,100)")
    }

    # if division missing estimate it esle use provided y_div
    if(missing(y_div)){
      pre_ <- pretty(y_limits, n = 10)
    } else {
      pre_ <- seq(y_limits[1], y_limits[2], by = y_div) # else use suer provided values 
    }
    
    ### if no color scale use default
    if(is.null(col_scale)){
      col_scale <- rev(brewer.pal(DS_, "Dark2"))
    }
    
    #### Set Divergent color values if color scale is provided
    # default color scale
    col_scale3 <- list(scale_colour_manual(values = rev(adjustcolor(brewer.pal(DS_, "Dark2"), alpha.f = a))))
    # if scale provided change the variable
    if (!missing(col_scale)&style == "divergent") {
        col_scale3 <- list(scale_colour_manual(values= adjustcolor(col_scale,alpha.f = a)))
        }
    if (length(col_scale) < DS_) {
        col_scale3 <- list(scale_colour_manual(values= adjustcolor((colorRampPalette(col_scale)(DS_)),alpha.f = a))) # expand color scale if necessary
        } 

    ### ==== POINTS Styles definitions====
    # modes: basic, divergent, greyscale and bicolour
    
      ## basic version single color - DEFAULT mode
      S_basic <- list(geom_jitter(
       aes(y = value, color = variable),
       stroke = point_size / 2.5,
       position = position_jitterdodge(dodge.width = width, jitter.width = width/2),
       shape = ps,
       size = point_size,
       fill = adjustcolor(col2, alpha.f = a)),
       scale_colour_manual(values = rep(adjustcolor(col, alpha.f = a), DS_))
     )
      ## divergent dot colors
      S_div <- list(geom_jitter(
        aes(y = value, color = variable),
        stroke = point_size / 2.5,
        position = position_jitterdodge(dodge.width = width, jitter.width = width/2),
        shape = ps,
        size = point_size,
        fill = adjustcolor(col2, alpha.f = a)
      ), col_scale3 #scale_colour_manual(values = rev(adjustcolor(brewer.pal(DS_, "Dark2"), alpha.f = a)))
      )

      ## grayscale version
      S_gs <- list(geom_jitter(
        aes(y = value,color = variable),
        stroke = point_size / 2.5, # storke controls outline thickness
        position = position_jitterdodge(dodge.width = width, jitter.width = width/2),
        shape = ps,
        size = point_size,
        fill = grey(1, alpha = a)
      ), scale_colour_manual(values = rep(grey(0, alpha = a), DS_)) # add as many as No of data series
      )

      ## bicolour different colors below above average value for each series
      S_ab <- list(geom_jitter(
        aes(y = value,
            group =  variable,
          color = ifelse((value <= 0), (value > mean_), (value < mean_)) # above mean - col, below or equal - col2
          # the ifelse is for the negative values cases
        ),
        position = position_jitterdodge(dodge.width = width, jitter.width = width/2),
        shape = ps,
        size = point_size
      ), scale_colour_manual(values = c(adjustcolor(col, alpha.f = a), adjustcolor(col2, alpha.f = a)))
      )
      
      ## Custom change color of bars and points
      S_cust <- list(geom_jitter(
        aes(y = value, color = variable),
        position = position_jitterdodge(dodge.width = width, jitter.width = width/2),
        shape = ps, # ps,
        size = point_size,
        stroke = point_size / 2.5,
        fill = adjustcolor(col2, alpha.f = a)),
        scale_colour_manual(values = rep(adjustcolor(col, alpha.f = a), DS_))
        )
      
    ### ====/ POINTS Styles definitions====
    ### === POINTS switch ===
    Style_ <- switch(
      style,
      "divergent" = S_div,
      "greyscale" = S_gs,
      "bicolour" = S_ab,
      "custom" = S_cust,
      S_basic # default mode
      )  
    
    ### START PLOT
      # color scale for bars with exception for custom mode
      # if there is no color sclae provided or Style is not custom provide default color scale
      if(!missing(col_scale)&style == "custom") {
        col_scale2 <- scale_fill_manual(values = col_scale); #example brewer.pal(4, "Blues")) 
        if (length(col_scale) < DS_) {
          col_scale2 <- scale_fill_manual(values=colorRampPalette(col_scale)(DS_))} # expand color scale if necessary
      } else {
        col_scale2 <- scale_fill_grey(start = 1 - 1 / DS_, end = 0) # kolory
      }
      
      # main plot
      p_ <-
        ggplot(data_g1, aes(
          x = samples,
          y = mean_,
          fill = variable
          )) + geom_bar(position = position_dodge(),  # this adds border arond bars colour="black", size = 1, # size 
                      width = width,
                      stat = "identity") + col_scale2
      
      # error bars
      p_ <- p_ + geom_errorbar(
        aes(
          ymin = mean_ - err_,
          ymax = mean_ + err_
        ),
        position = position_dodge(width = width),
        width = width / 2.5
      )
      
      # add line on zero y if values are negative
      if (min_ < 0) {
        Style_ <- append(Style_, geom_hline(yintercept = 0, linetype = "dashed", size = 1/2)) 
      }
      
      # applying points to the plot
      p_ <- p_ + Style_

    ### formatting the plot
    p_ <-
      p_ + theme_classic() + theme(
        axis.line = element_line(colour = "black", size = 1),
        axis.ticks = element_line(colour = "black", size = 1)
      )
    # transparent backgroud
    p_ <- p_ + theme(legend.position="none", # removing legend as they are done when combining the image
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )
    
    # change axis text size if value is set
    if(!missing(xy_ts)){
    p_ <- p_ + theme(axis.text=element_text(size=xy_ts))
    }
    
    # change axis label size if value is set
    if(!missing(xy_ls)){
      p_ <- p_ + theme(axis.title=element_text(size=xy_ls))
    }
    
    # adjustment so that bars were not hovering above axis
    p_ <- p_ + scale_y_continuous(expand = c(0, 0),
                                  oob=rescale_none,
                                  limits = c(y_limits),
                                  breaks = pre_)
    #' if we set up the min value to positive number then the bars on plot disappears
    #' this is why we need oob=rescale_none added above
    
    # add horizontal lines
    if (h_lines){
      p_ <- p_ + theme(panel.grid.minor = element_line(colour = "grey", linetype = 3, size = 1))
    }
    
    ## add labels if they exist
    # for y
    if(!missing(y_l)){
      p_ <- p_ + ylab(y_l)
    }
    
    # for x
    if(!missing(x_l)){
      p_ <- p_ + xlab(x_l)
    }
    
    # execution
    if (class(seed) == "numeric") {
      # set seed for  and plot
      set.seed(seed)
      return(p_)
    } else # if seed is not good just render plot
      return(p_)
    
}


# this is modiifed pchShow() function mainly to draw pch point in two rows - more siutable for sidebar of Shiny app
point_plt <- function(ipch=c(0:25), cex = 3, col = "red3", bg = "gold", coltext = "brown", cextext = 1.2, lwd = 1)
{
  np <- length(ipch)
  k <- ifelse(length(ipch)<15, 10, np/2)
  dd <- c(-1,1)/2
  rx <- dd + range(ix <- ipch %/% k)
  ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
  pch <- as.list(ipch) # list with integers & strings
  plot(rx, ry, type = "n", axes  =  FALSE, xlab = "", ylab = "")
  for(i in 1:np) {
    pc <- pch[[i]]
    ## 'col' symbols with a 'bg'-colored interior (where available) :
    points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex, lwd = lwd)
    if(cextext > 0)
      text(ix[i] - 0.3, iy[i], pc, col = coltext, cex = cextext)
  }
}
