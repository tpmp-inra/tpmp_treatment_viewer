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
library(shinyCommon)

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
             uiOutput("cbNormalizationMethod"),
             uiOutput("cbShowLabels")),
      column(6,
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
      dotSize <- 2
    }
    colorBy <- input$colorBy
    if (is.null(colorBy)) return(NULL)
    selPlant <- input$cbPlantSelection
    if (is.null(selPlant)) return(NULL)
    
    if (sum(xv %in% names(df))>0){ # supress error when changing files
      
      treatments_to_plot <- 
        df %>%
        filter(treatment %in% input$cbTreatmentSelection) %>%
        filter(plant %in% selPlant)
      
      # Normalize
      if (input$cbNormalizationMethod == "normalization") {
        normalize <- function(x) {
          return((x-min(x)) / (max(x)-min(x)))
        }
        treatments_to_plot <- treatments_to_plot %>% mutate_at(vars(yv), funs(normalize(.) %>% as.vector))
      } else {
        if (input$cbNormalizationMethod == "scale") {
          treatments_to_plot <- treatments_to_plot %>% mutate_at(vars(yv), funs(scale(.) %>% as.vector))
        }
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
                  colorBy = colorBy, 
                  xv = xv,
                  yv = yv))
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
    selectInput("colorBy", "Color dots using:", choices = cb_options, selected = "treatment")
  })
  
  #The following set of functions populate the dot size selectors
  output$dotSize <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    new_df <- df[sapply(df,is.numeric)]
    dsnames <- names(new_df)
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    selectInput("dotSize", "Dot Size:", choices = c('none', cb_options), selected = "shape_solidity")
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
  
  output$cbNormalizationMethod <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    fill_normalization_cb()
  })
  
  output$cbShowLabels <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    build_string_selectImput(df, 
                             "cbShowLabels",  
                             "Show plant name (if all dots are displayed graph will become cluttered:", 
                             "none")
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
        gg <- ggplot(dt$df, aes_string(x = dt$xv , y = dt$yv, color = dt$colorBy, size = dt$dotSize, shape = "genotype"))
      } else {
        gg <- ggplot(dt$df, aes_string(x = dt$xv , y = dt$yv, color = dt$colorBy, size = dt$dotSize))
      }
      
      # Set palette
      numericDf <- dt$df[sapply(dt$df,is.numeric)]
      if (dt$colorBy %in% colnames(numericDf)) {
        gg <- gg + scale_fill_gradient(low = brewer.pal(8, input$cbPaletteSelector)[1],
                                       high = brewer.pal(8, input$cbPaletteSelector)[length(brewer.pal(8, input$cbPaletteSelector))],
                                       space = "Lab",
                                       na.value = "grey50",
                                       guide = "colourbar")
      } else {
        treatmentsVector <- as.vector(dt$df[dt$colorBy])
        gg <- gg + scale_colour_manual(values = colorRampPalette(brewer.pal(8, input$cbPaletteSelector))(n_distinct(treatmentsVector)))
      }
      
      # Select display mode
      gg <- gg + geom_point(alpha = 0.7)
      
      if (input$cbShowLabels != "none") {
        gg <- gg + geom_text_repel(aes_string(color = dt$colorBy, label = input$cbShowLabels), vjust = -1)
      }
      
      # Scatter the scatter
      if (input$cbSplitScatter != "none"){
        gg <- gg +  facet_wrap(input$cbSplitScatter)
      }

      # Smoothy
      if (!input$chkUseTimePointSelector & (input$smoothingModel != "none")) {
        gg <- gg + geom_smooth(method = input$smoothingModel)
      }
      
      gg
    }
  })
  
}

shinyApp(ui, server)