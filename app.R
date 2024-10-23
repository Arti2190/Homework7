# Install required packages
# install.packages("shinyalert")
library(shiny)
library(shinyalert)
library(tidyverse)
#library(conflicted)
library(dplyr)

#shiny::runExample("01_hello")

# Load helper functions
source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  #Title Panel
  titlePanel("Correlation Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Find Correlation:"),
      
      # Selectize inputs for x and y variables
      selectizeInput("corr_x", "X Variable", choices = numeric_vars),
      selectizeInput("corr_y", "Y Variable", choices = numeric_vars),
      
      # Radio buttons for household language, Snap recipient, and educational attainment
      radioButtons("hhl_corr", "Household Language",
                   choices = list("All" = "all", "English" = "english", "Spanish" = "spanish", "Other" = "other")),
      radioButtons("fs_corr", "SNAP Recipient",
                   choices = list("All" = "all", "Yes" = "yes", "No" = "no")),
      radioButtons("schl_corr", "Educational attainment",
                   choices = list("All" = "all", "High School no Completed" = "no_hs", "High School or GED" = "hs", "College Degree" = "college")),
      
      h2("Select a Sample Size"),
      # Slider for select sample size 
      sliderInput("corr_n", "Sample Size", min = 20, max = 500, value = 20),
      
      # Button to get sample
      actionButton("corr_sample", "Get a Sample!")
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      conditionalPanel(
        condition = "input.corr_sample",
        h2("Guess the correlation!"),
        column(6, 
               numericInput("corr_guess",
                            "Your Guess ",
                            value = 0,
                            min = -1, 
                            max = 1
               )
        ),
        column(6, 
               actionButton("corr_submit", "Check Your Guess!"))
      )
    )
  )
)

# Load sample data
my_sample <- readRDS("my_sample_temp.rds")

 # Define server logic required to draw a histogram
server <- function(input, output, session) {
  #################################################3
  ##Correlation tab  
  # Create a reactiveValues object for storing data(corr_data) and correlation truth(corr_truth)
  #both should be set to null to start with!
  
  sample_corr <- reactiveValues(corr_data = NULL, corr_truth = NULL)
  
  # Update input boxes so they can't choose the same variable
  observeEvent(c(input$corr_x, input$corr_y), {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x == corr_y){
      choices <- choices[-which(choices == corr_x)]
      updateSelectizeInput(session, "corr_y", choices = choices)
    }
  })
  
  # Observe when the "Get a Sample!" button is clicked
  #Use an observeEvent() to look for the action button (corr_sample)
  observeEvent(input$corr_sample, {
    # Subsetting the data based on radio button choices
    if(input$hhl_corr == "all"){
      hhl_sub <- HHLvals
    } else if(input$hhl_corr == "english"){
      hhl_sub <- HHLvals["1"]
    } else if(input$hhl_corr == "spanish"){
      hhl_sub <- HHLvals["2"]
    } else {
      hhl_sub <- HHLvals[c("0", "3", "4", "5")]
    }
    
    if(input$fs_corr == "all"){
      fs_sub <- FSvals
    } else if(input$fs_corr == "yes"){
      fs_sub <- FSvals["1"]
    } else {
      fs_sub <- FSvals["2"]
    }
    
    if(input$schl_corr == "all"){
      schl_sub <- SCHLvals
    } else if(input$schl_corr == "no_hs"){
      schl_sub <- SCHLvals[as.character(0:15)]
    } else if(input$schl_corr == "hs"){
      schl_sub <- SCHLvals[as.character(16:19)]
    } else {
      schl_sub <- SCHLvals[as.character(20:24)]
    }
    
    corr_vars <- c(input$corr_x, input$corr_y)
    
    subsetted_data <- my_sample %>%
      filter(HHLfac %in% hhl_sub, FSfac %in% fs_sub, SCHLfac %in% schl_sub) %>%
      {if ("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
      {if ("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
      {if ("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
      {if ("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
      {if ("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
      {if ("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
      {if ("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
      {if ("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
      {if ("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .} 
    
    index <- sample(1:nrow(subsetted_data), 
                    size = input$corr_n, 
                    replace = TRUE, 
                    prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
    
    #Update the sample_corr reactive value object
    #the corr_data argument should be updated to be the subsetted_data[index,]
    #the corr_truth argument should be updated to be the correlation between 
    #the two variables selected: 
    #cor(sample_corr$corr_data |> select(corr_vars))[1,2]
    
    sample_corr$corr_data <- subsetted_data[index, ]
    sample_corr$corr_truth <- cor(sample_corr$corr_data |> select(corr_vars))[1, 2]
  })
  
  # #Create a renderPlot() object to output a scatter plot
  output$scatterPlot <- renderPlot({
    validate(
      need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
    )
    ggplot(sample_corr$corr_data, aes_string(x = input$corr_x, y = input$corr_y)) +
      geom_point() +
      theme_minimal()
  })
  
  #Use this code for the correlation guessing game!
        observeEvent(input$corr_submit, {
          close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
          if(close){
            shinyalert(title = "Nicely done!",
                       paste0("The sample correlation is ", 
                              round(sample_corr$corr_truth, 4), 
                              "."),
                       type = "success"
            )
          } else {
            if(input$corr_guess > sample_corr$corr_truth){
              shinyalert(title = "Try again!",
                         "Try guessing a lower value.")
            } else {
              shinyalert(title = "Try again!",
                         "Try guessing a higher value.")
      }
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
