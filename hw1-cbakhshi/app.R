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
            
            # Set top level for universities---------------------------------------------
            sliderInput(inputId = "topn", 
                        label = "Select the Number of Universities:", 
                        min = 10, max = 100, step = 5,
                        value = 20),
            
            # Horizontal line for visual separation -----------------------
            hr(),
            
            # Select variable for different score types ----------------------------------
            selectInput(inputId = "scores", 
                        label = "Select Score Type:",
                        choices = c("Academic Reputation Score" = "Academic_Reputation_Score",
                                    "Employer Reputation Score" = "Employer_Reputation_Score",
                                    "Faculty Citations Score" = "Faculty_Citations_Score",
                                    "International Student Score" = "International_Student_Score"), 
                        selected = "Academic Reputation Score"),
            
            # Horizontal line for visual separation -----------------------
            hr(),
            
            # Select variable for x-axis of the third plot----------------------------------
            selectInput(inputId = "x", 
                        label = "X-axis for plot 3:",
                        choices = c("Size" = "Size_", 
                                    "Age"= "Age_",
                                    "Academic Reputation Score" = "Academic_Reputation_Score",
                                    "Employer Reputation Score" = "Employer_Reputation_Score",
                                    "Faculty Citations Score" = "Faculty_Citations_Score",
                                    "International Student Score" = "International_Student_Score"), 
                        selected = "Size"),
            
            # Set alpha level for plot 3---------------------------------------------
            sliderInput(inputId = "alpha", 
                        label = "Alpha:", 
                        min = 0, max = 1, 
                        value = 0.5),
            
            # Set point size level for plot 3---------------------------------------------
            sliderInput(inputId = "size", 
                        label = "Point Size:", 
                        min = 0, max = 4, 
                        value = 2),
            
            # Horizontal line for visual separation -----------------------
            hr(),
            
            
            # Show data table ---------------------------------------------
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE),
            
            # Horizontal line for visual separation -----------------------
            hr(),
            
            # Select which countries to plot for plot 3------------------------
            checkboxGroupInput(inputId = "selected_type",
                               label = "Select Country:",
                               choices = unique(universities$Country),
                               selected = "United States")
        ),
        
        mainPanel(
            # Output: Show barplot --------------------------------------
            plotOutput(outputId = "barplot"),
            
            br(),   br(),     # a little bit of visual separation
            
            # Output: Show barplot 2 --------------------------------------
            plotOutput(outputId = "barplot2", height = 400, width = 850),
            
            br(),   br(),     # a little bit of visual separation
            
            # Output: Show scatterplot --------------------------------------
            plotOutput(outputId = "scatterplot", height = 900, width = 650),
            
            br(),   br(),     # a little bit of visual separation
            
            #Download button 
            downloadButton(
                outputId = "downloadbutton",
                label = "Download",
                class = NULL,
                icon = shiny::icon("download")), 
            
            br(),  br(),     # a little bit of visual separation
            
            # Show data table ---------------------------------------------
            DT::dataTableOutput(outputId = "universitiestable")
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
    # Create a subset of data grouped by Country------
    avg_subset <- reactive({
        req(input$scores) # ensure availablity of value before proceeding
        universities %>%
            group_by(Country) 
    })
    
    # Create barplot 2 object the plotOutput function is expecting --
    output$barplot2 <- renderPlot({
        ggplot(data = avg_subset(), aes_string(x = 'Country' , y = input$scores, fill = 'Country'))+
            geom_bar(stat = "identity", position = 'dodge')+
            labs(title = "Average Scores by Country", subtitle = "Choose the scores you want to at on the left.",
                 x = 'Countries',
                 y = toTitleCase(str_replace_all(input$scores, "_", " "))) +
            theme(axis.text = element_text(angle = 45))
        
    })
    
    # Create a subset of data filtering for selected countries for Plot 3------
    university_subset <- reactive({
        req(input$selected_type) # ensure availablity of value before proceeding
        filter(universities, Country %in% input$selected_type)
    })
    
    
    # Create scatterplot object the plotOutput function is expecting --
    output$scatterplot <- renderPlot({
        ggplot(data = university_subset(), aes_string(x = input$x , y = 'University')) +
            geom_point(alpha = input$alpha, size = input$size, colour = "steelblue") +
            labs(title = "Top Universities", subtitle = "Data from QS World University Rankings 2020")+
            labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
                 y = "University Rankings")
    })
    
    # Print data table if checked -------------------------------------
    output$universitiestable <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = university_subset(), 
                          options = list(pageLength = 5), 
                          rownames = FALSE)
        }
    )
    
    #Download data if checked 
    output$downloadbutton <- downloadHandler(
        filename = function(){
            paste('file1.csv')
        },
        content = function(file) {
            write.csv(university_subset(), file)
        }
    )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)