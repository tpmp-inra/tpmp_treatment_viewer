library(shiny)
library(ggplot2)
library(ggExtra)
library(ggcorrplot)
library(dplyr)
library(gridExtra)
library(reshape2)
library(data.table)
library(RColorBrewer)

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
             uiOutput("treatment_selector"),
             uiOutput("genotype_selector"),
             uiOutput("treatment_value_selector"),
             uiOutput("xAxisCombo"),
             uiOutput("yAxisCombo"),
             uiOutput("smoothingModel")),
      column(6,
             uiOutput("cbPaletteSelector"),
             uiOutput("chkSplitScatter"),
             uiOutput("colorBy"),
             uiOutput("dotSize"),
             uiOutput("chkDrawGenotype"),
             uiOutput("chkShowCorrelationMatrix"),
             uiOutput("correlationHelpText"))),
    uiOutput("timeSliceSelector"),
    uiOutput("chkUseTimePointSelector"),
    uiOutput("timePointSelector")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Scatter", 
               plotOutput(outputId = "scatter_plot", 
                          inline = FALSE, 
                          height = "800")),
      tabPanel("CSV File", tableOutput("filetable"))
    )
  )
)

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2) # Not sure it's a good idea
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    filedata()
  })
  
  output$treatment_selector <- renderUI({
    # Exit if not ready
    df <-filedata()
    if (is.null(df)) return(NULL)
    # Exit if not for current display mode
    if ("genotype" %in% colnames(df) & "treatment_value" %in% colnames(df)) return(NULL)
    
    distinct_df = distinct(df,treatment)
    distinct_vector = distinct_df$treatment
    cb_options = as.list(levels(distinct_vector))
    checkboxGroupInput(inputId =  "treatment_selector",
                       label = "Display treatments:",
                       choices = cb_options,
                       selected = cb_options)
  })
  
  output$genotype_selector <- renderUI({
    # Exit if not ready
    df <-filedata()
    if (is.null(df)) return(NULL)
    # Exit if not for current display mode
    if (!("genotype" %in% colnames(df)) | !("treatment_value" %in% colnames(df))) return(NULL)
    
    
    cb_options = as.list(levels(distinct(df,genotype)$genotype))
    checkboxGroupInput(inputId =  "genotype_selector",
                       label = "Genotypes:",
                       choices = cb_options,
                       selected = cb_options)
  })
  
  output$treatment_value_selector <- renderUI({
    # Exit if not ready
    df <-filedata()
    if (is.null(df)) return(NULL)
    # Exit if not for current display mode
    if (!("genotype" %in% colnames(df)) | !("treatment_value" %in% colnames(df))) return(NULL)
    
    cb_options = as.list(distinct(df,treatment_value)$treatment_value)
    checkboxGroupInput(inputId =  "treatment_value_selector",
                       label = "Treatments values:",
                       choices = cb_options,
                       selected = cb_options)
  })
  
  output$cbPaletteSelector <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    selectInput("cbPaletteSelector",
                "Color Palette:",
                c("Accent" = "Accent",
                  "Spectral" = "Spectral",
                  "RdYlGn" = "RdYlGn",
                  "RdYlBu" = "RdYlBu",
                  "Set1" = "Set1",
                  "Set2" = "Set2",
                  "Set3" = "Set3",
                  "Pastel1" = "Pastel1",
                  "Pastel2" = "Pastel2",
                  "Paired" = "Paired",
                  "Dark2" = "Dark2",
                  "Blues" = "Blues"
                ))
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
  
  output$chkSplitScatter <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    stringDf <- df[sapply(df,is.factor)]
    stringDf <- stringDf[, !(colnames(stringDf) %in% c("date_time"))]
    dsnames <- names(stringDf)
    # cb_options <- list()
    # cb_options[ dsnames] <- dsnames
    selectInput("chkSplitScatter", 
                "Separate graphs using:", 
                choices = c("None", as.list(dsnames)), 
                selected = "treament")
  })
  
  output$chkShowCorrelationMatrix <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    checkboxInput("chkShowCorrelationMatrix", "Show correlation matrix", FALSE)
  })
  
  output$correlationHelpText <- renderText({
    df <-filedata()
    if (is.null(df)) return(NULL)
    "Correlation matrix will ignore all other options except treatments displayed"
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
    selectInput("dotSize", "Dot Size:", choices = cb_options, selected = "shape_solidity")
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
    req(input$xAxisCombo, input$yAxisCombo, input$dotSize)
    
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    xv <- input$xAxisCombo
    if (is.null(xv)) return(NULL)
    yv <- input$yAxisCombo
    if (is.null(yv)) return(NULL)
    dotSize <- input$dotSize
    if (is.null(dotSize)) return(NULL)
    colorBy <- input$colorBy
    if (is.null(colorBy)) return(NULL)
    
    if (sum(xv %in% names(df))>0){ # supress error when changing files
      
      if ("genotype" %in% colnames(df) & "treatment_value" %in% colnames(df)) {
        treatments_to_plot <- filter(df, genotype %in% input$genotype_selector)
        treatments_to_plot <- filter(treatments_to_plot, treatment_value %in% input$treatment_value_selector)
      } else {
        treatments_to_plot <- filter(df, treatment %in% input$treatment_selector)
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
      
      numericDf <- treatments_to_plot[sapply(treatments_to_plot,is.numeric)]
      if (input$chkShowCorrelationMatrix) {
        
        corr <- round(cor(numericDf), 1)
        
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
        if(input$chkDrawGenotype & "genotype" %in% colnames(treatments_to_plot)) {
          gg <- ggplot(treatments_to_plot, aes_string(x = xv , y = yv, color = colorBy, size = dotSize, shape = "genotype"))
        } else {
          gg <- ggplot(treatments_to_plot, aes_string(x = xv , y = yv, color = colorBy, size = dotSize))
        }
        
        # Set palette
        if (colorBy %in% colnames(numericDf)) {
          gg <- gg + scale_fill_gradient(low = brewer.pal(8, input$cbPaletteSelector)[1],
                                         high = brewer.pal(8, input$cbPaletteSelector)[length(brewer.pal(8, input$cbPaletteSelector))],
                                         space = "Lab",
                                         na.value = "grey50", 
                                         guide = "colourbar")
        } else {
          treatmentsVector <- as.vector(treatments_to_plot[colorBy])
          gg <- gg + scale_colour_manual(values = colorRampPalette(brewer.pal(8, input$cbPaletteSelector))(n_distinct(treatmentsVector)))
        }
        
        # Select display mode
        gg <- gg + geom_point(alpha = 0.7)
        
        # Scatter the scatter
        if (input$chkSplitScatter != "None"){
          gg <- gg +  facet_wrap(input$chkSplitScatter)
        }

        # Smoothy
        if (!input$chkUseTimePointSelector & (input$smoothingModel != "none")) {
          gg <- gg + geom_smooth(method = input$smoothingModel)
        }

        gg
      }
    }  
  })
  
}

shinyApp(ui, server)