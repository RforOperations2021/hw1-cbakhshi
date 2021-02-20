library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
library(tools)

#importing the csv file -----------------------------------------------
universities = read_csv('University.csv')

#reordering factor levels in the Age Column ------------------------------
universities$Age_ = factor(universities$Age_, levels=c( 'Less than 10 years old', 
                                                        '25 to 50 years old', '50 to 100 years old', 'More than 100 years old'))

#reordering factor levels in the Size Column ------------------------------
universities$Size_ = factor(universities$Size_, levels=c( 'Fewer than 5,000', 
                                                          '>= 5,000 students', '>= 12,000 students', 'More than 30,000 students'))

#Turning Rank column to categorical from Numeric ------------------------------
universities$Rank = as.factor(universities$Rank)

#Reversing the levels to have 1 at top and last at the bottom-------------------
universities$Rank = with(universities, factor(Rank, levels = rev(levels(Rank))))

#Turning Rank column to categorical from Numeric ------------------------------
universities$University = as.factor(universities$University)
universities$University = factor(universities$University, levels= universities$University[order(universities$Rank)], ordered=TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Top 100 World University Rankings"),
    
    # Sidebar layout with a input and output definitions --------------
    sidebarLayout(
        
        # Inputs: Select variables to plot ------------------------------
        sidebarPanel(
            
            # Set top level ---------------------------------------------
            sliderInput(inputId = "topn", 
                        label = "Select the Number of Universities:", 
                        min = 10, max = 100, step = 5,
                        value = 20)
        ),
        
        mainPanel(
            # Output: Show barplot --------------------------------------
            plotOutput(outputId = "barplot")
        )
    )
)
# Define server function required to create the scatterplot ---------
server <- function(input, output) {
    
    # Create a subset of data filtering for top n universities ------
    top_subset <- reactive({
        req(input$topn) # ensure availablity of value before proceeding
        universities[1 :input$topn,] %>%
            group_by(Country) %>%
            summarise(count = n())
    })
    
    # Create barplot object the plotOutput function is expecting --
    output$barplot <- renderPlot({
        ggplot(data = top_subset(), aes_string(x = 'Country', y = 'count', fill = 'Country')) +
            geom_bar(stat = "identity", position = 'dodge')+
            labs(title = "Number of top Universities by Country", subtitle = "Choose the number of Universities to look at on the left. Data from QS World University Rankings 2020",
                 y = "Number of Universities", x = 'Country')
    }
    )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)