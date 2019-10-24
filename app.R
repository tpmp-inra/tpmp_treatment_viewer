list.of.packages <- c("shiny", 
                      "ggplot2", 
                      "ggExtra",
                      "ggcorrplot",
                      "tidyverse",
                      "gridExtra",
                      "reshape2",
                      "data.table",
                      "RColorBrewer",
                      "shinyWidgets",
                      "ggrepel")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(shiny)
library(ggplot2)
library(ggExtra)
library(ggcorrplot)
library(tidyverse)
library(gridExtra)
library(reshape2)
library(data.table)
library(RColorBrewer)
library(shinyWidgets)
library(ggrepel)
library(gtools)

source('../shinyCommon/R/shiny_common_all.R')

ui <- pageWithSidebar(
  
  headerPanel("Treatment scatter"),
  
  sidebarPanel(
    fileInput('datafile', 
              'Choose CSV file',
              accept=c("text/csv", 
                       "text/comma-separated-values,text/plain",
                       ".csv")),
    fluidRow(
      column(6,
             uiOutput("cbTreatmentSelection"),
             uiOutput("cbPlantSelection"),
             uiOutput("xAxisCombo"),
             uiOutput("yAxisCombo"),
             uiOutput("smoothingModel"),
             uiOutput("cbShowLabels")),
      column(6,
             uiOutput("chkShowOutliers"),
             uiOutput("cbPaletteSelector"),
             uiOutput("cbSplitScatter"),
             uiOutput("colorBy"),
             uiOutput("dotSize"),
             uiOutput("chkDrawGenotype"),
             uiOutput("chkShowCorrelationMatrix"),
             uiOutput("cbCorrelationMatrix"))),
    uiOutput("timeSliceSelector"),
    uiOutput("chkUseTimePointSelector"),
    uiOutput("timePointSelector"),
    
    tags$head(tags$style("#scatter_plot{height:80vh !important;}"))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Scatter", 
               plotOutput(outputId = "scatter_plot", 
                          inline = FALSE, 
                          height = "800",
                          hover = "plot_hover"),
               tableOutput("observationInfo")),
      tabPanel("CSV File", tableOutput("filetable"))
    )
  )
)

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2) # Not sure it's a good idea
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    load_experience_csv(input)
  })
  
  filteredData <- reactive({
    req(input$xAxisCombo, input$yAxisCombo, input$dotSize)
    
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    xv <- input$xAxisCombo
    if (is.null(xv)) return(NULL)
    yv <- input$yAxisCombo
    if (is.null(yv)) return(NULL)
    dotSize <- input$dotSize
    if (is.null(dotSize)) return(NULL)
    if (dotSize == 'none') {
      dotSize <- 4
    }
    dotColor <- input$colorBy
    if (is.null(dotColor)) return(NULL)
    selPlant <- input$cbPlantSelection
    if (is.null(selPlant)) return(NULL)
    
    if (sum(xv %in% names(df))>0){ # supress error when changing files
      
      treatments_to_plot <- 
        df %>%
        filter(treatment %in% input$cbTreatmentSelection) %>%
        filter(plant %in% selPlant)
      
      if (!input$chkShowOutliers & ("outlier" %in% colnames(df))) {
        treatments_to_plot <- treatments_to_plot %>% filter(outlier == 0)
      }
      
      # Create color column
      ccn = color_column_name()
      if (dotColor != 'none') {
        ccn.vector <- as.factor(treatments_to_plot[,dotColor][[1]])
        if (length(unique(ccn.vector)) > 20) {
          ccn.vector <- treatments_to_plot[,dotColor][[1]]
        }
        treatments_to_plot <- 
          treatments_to_plot %>%
          mutate(!!ccn := ccn.vector)
      } else {
        treatments_to_plot <- 
          treatments_to_plot %>%
          mutate(!!ccn := as.factor(1))
      }
      
      if (input$chkUseTimePointSelector) {
        timesVector <- as.vector(treatments_to_plot["day_after_start"])
        
        min_dist <- min(abs(timesVector - input$timePointSelector))
        treatments_to_plot <- filter(treatments_to_plot, trunc(day_after_start) == input$timePointSelector)
      } else {
        if (input$xAxisCombo == "day_after_start") {
          treatments_to_plot <- filter(treatments_to_plot, 
                                       day_after_start >= input$timeSliceSelector[1] & day_after_start <= input$timeSliceSelector[2])
        }
      }
      return(list(df = treatments_to_plot,
                  dotSize = dotSize, 
                  xv = xv,
                  yv = yv))
    }
  })
  
  color_column_name <- reactive({
    dotColor <- input$colorBy
    if (dotColor == 'none') {
      'no_color'
    } else {
      paste('color', dotColor, 'xyz', sep = '_')
    }
  })
  
  # Print observation details
  output$observationInfo <- renderTable({
    df <-filteredData()
    if (is.null(df)) return(NULL)
    
    nearPoints(df$df, input$plot_hover)
  })
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    filedata()
  })
  
  # Populate treatment selector
  output$cbTreatmentSelection <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fill_treatment_selection(df,
                             "cbTreatmentSelection",
                             "Select treatments to be displayed",
                             "count > 3")
  })
  
  # Populate plants selector
  output$cbPlantSelection <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fill_plant_selection(df)
  })
  
  output$cbPaletteSelector <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    fill_palette_selector("cbPaletteSelector")
  })
  
  output$chkDrawGenotype <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if("genotype" %in% colnames(df)) {
      checkboxInput("chkDrawGenotype", 
                    "Draw each genotype with a different shape (performance hit)", 
                    FALSE)
    } else {
      checkboxInput("chkDrawGenotype", 
                    "No genotype option available", 
                    FALSE)
    }
  })
  
  output$cbSplitScatter <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    build_string_selectImput(df, "cbSplitScatter",  "Separate graphs using:", "treament")
  })
  
  output$chkShowCorrelationMatrix <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    checkboxInput("chkShowCorrelationMatrix", "Show correlation matrix", FALSE)
  })
  
  output$cbCorrelationMatrix <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    new_df <- df[sapply(df,is.numeric)]
    dsnames <- names(new_df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    
    pickerInput(
      inputId = "cbCorrelationMatrix", 
      label = "Select correlation matrix attributes", 
      choices = cb_options,
      options = list(
        `selected-text-format` = "count > 5",
        `count-selected-text` = "{0} attributes selelcted",
        `actions-box` = TRUE,
        `deselect-all-text` = "Select none",
        `select-all-text` = "Select all"
      ), 
      selected = cb_options,
      multiple = TRUE
    )
  })
  
  #The following set of functions populate the x axis selectors
  output$xAxisCombo <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    new_df <- df[sapply(df,is.numeric)]
    dsnames <- names(new_df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    selectInput("xAxisCombo", "X Axis:", choices = cb_options, selected = "day_after_start")
  })
  
  #The following set of functions populate the y axis selectors
  output$yAxisCombo <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    new_df <- df[sapply(df,is.numeric)]
    dsnames <- names(new_df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    selectInput("yAxisCombo", "Y Axis:", choices = cb_options, selected = "area")
  })
  
  output$colorBy <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    dsnames <- names(df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    cb_options <- cb_options[mixedorder(unlist(cb_options),decreasing=F)]
    if ('treatment' %in% cb_options) {
      selected_choice <- 'treatment'
    } else {
      selected_choice <- 'none'
    }
    selectInput("colorBy", "Color dots using:", choices = c('none', cb_options), selected = selected_choice)
  })
  
  #The following set of functions populate the dot size selectors
  output$dotSize <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    build_numeric_selectImput(df, "dotSize", "Dot Size:", "none")
  })
  
  output$smoothingModel <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    selectInput("smoothingModel", 
                "Smoothing model (regression):", 
                c("None" = "none",
                  "Linear Regression" = "lm", 
                  "Generalized Linear Regression" = "glm", 
                  "Generalized Additive Model" = "gam", 
                  "Local Regression" = "loess", 
                  "Robust Linear Models" = "rlm",
                  "Auto" = "auto"
                ))
    
  })
  
  output$cbShowLabels <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    build_string_selectImput(df, 
                             "cbShowLabels",  
                             "Show plant name (if all dots are displayed graph will become cluttered:", 
                             "none")
  })
  
  output$chkShowOutliers <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if ("outlier" %in% colnames(df)) {
      checkboxInput("chkShowOutliers", paste('Show outliers (', length(which(df$outlier==1)), ')', sep=''), TRUE)
    } else {
      checkboxInput("chkShowOutliers", 'No outliers detected, option ignored', FALSE)
    }
  })
  
  output$timeSliceSelector <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    tags$hr()
    min_time <- trunc(min(df$day_after_start))
    max_time <- trunc(max(df$day_after_start)) + 1
    sliderInput(inputId =  "timeSliceSelector", 
                label = "Time slice: ", 
                min = min_time, 
                max = max_time, 
                value = c(min_time, max_time),
                step = 0.1)
  })
  
  output$chkUseTimePointSelector <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    checkboxInput("chkUseTimePointSelector", "Show only selected time", FALSE)
  })
  
  output$timePointSelector <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    tags$hr()
    max_time <- trunc(max(df$day_after_start))
    sliderInput(inputId =  "timePointSelector", 
                label = NULL, 
                min = 0, 
                max = max_time, 
                value = max_time / 2,
                step = 1)
  })
  
  # Here it renders
  output$scatter_plot = renderPlot({
    dt <- filteredData()
    if (is.null(dt)) return(NULL)
    
    if (input$chkShowCorrelationMatrix) {
      
      corrDf <- dt$df[, input$cbCorrelationMatrix]
      corr <- round(cor(corrDf), 1)
      
      # Plot
      ggcorrplot(corr, 
                 type = "lower", 
                 lab = TRUE, 
                 lab_size = 3, 
                 method="circle", 
                 colors = c("tomato2", "white", "springgreen3"), 
                 title="Correlogram of mtcars", 
                 ggtheme=theme_bw)
    } else {
      # Plot the dots
      if(input$chkDrawGenotype & "genotype" %in% colnames(dt$df)) {
        gg <- ggplot(dt$df, aes_string(x = dt$xv , y = dt$yv, color = color_column_name(), size = dt$dotSize, shape = "genotype"))
      } else {
        gg <- ggplot(dt$df, aes_string(x = dt$xv , y = dt$yv, color = color_column_name(), size = dt$dotSize))
      }
      
      # Set palette
      cl.vector <- dt$df[,color_column_name()][[1]]
      if (length(cl.vector) > 20) {
        gg <- gg + scale_fill_gradient(low = brewer.pal(8, input$cbPaletteSelector)[1],
                                       high = brewer.pal(8, input$cbPaletteSelector)[length(brewer.pal(8, input$cbPaletteSelector))],
                                       space = "Lab",
                                       na.value = "grey50",
                                       guide = "colourbar")
      } else {
        gg <- gg + scale_colour_manual(values = colorRampPalette(brewer.pal(8, input$cbPaletteSelector))(n_distinct(cl.vector)))
      }
      
      # Select display mode
      gg <- gg + geom_point(alpha = 0.7)
      
      if (input$cbShowLabels != "none") {
        gg <- gg + geom_text_repel(aes_string(color = color_column_name(), label = input$cbShowLabels), vjust = -1)
      }
      
      # Scatter the scatter
      if (input$cbSplitScatter != "none"){
        gg <- gg +  facet_wrap(input$cbSplitScatter)
      }

      # Smoothy
      if (!input$chkUseTimePointSelector & (input$smoothingModel != "none")) {
        gg <- gg + geom_smooth(method = input$smoothingModel, size = 1)
      }
      
      # gg <- gg + theme(legend.title = element_text(size=32, face = "bold"),
      #                  legend.text=element_text(size=30),
      #                  axis.text=element_text(size=20),
      #                  axis.title=element_text(size=22,face="bold"),
      #                  title = element_text(size=20))
      
      gg
    }
  })
  
}

shinyApp(ui, server)