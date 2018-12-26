
# importing train file in R:
data<-read.csv(file.choose(),header = T)
# to view the file:
data
attach(data)
head(data)

# CODE TO CREATE DASHBOARD:
## app.R ##

# adding needed library:
library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(shinythemes)
library(rsconnect)

# Define UI for application that plots features of movies
ui <- fluidPage(theme = shinytheme("journal"),
                
                # App title
                titlePanel("APNA GHAR", windowTitle = "GHAR"),
                
                # Sidebar layout with a input and output definitions
                sidebarLayout(
                  
                  # Inputs
                  sidebarPanel(
                    
                    h3("Plotting"),      # Third level header: Plotting
                    
                    # Select variable for y-axis 
                    selectInput(inputId = "y", 
                                label = "first independent variable:",
                                choices = c( "CentralAir",
                                             "Fireplaces","GarageType","GarageFinish"
                                ) ,
                                selected = "Fireplaces"),
                    
                    # Select variable for x-axis 
                    selectInput(inputId = "x", 
                                label = "second independent variable:",
                                choices = c(
                                  "YearBuilt"  =  "YearBuilt" ,
                                  "YrSold"     =  "YrSold" ,"CentralAir" = "CentralAir","GarageCars","FullBath","HalfBath",
                                  "Fireplaces" = "Fireplaces","HeatingQC","GrLivArea","OverallQual","OverallCond","LotShape","SaleCondition"
                                ),
                                selected = "YrSold"),
                    
                    hr(),
                    # Show data table
                    checkboxInput(inputId = "show_data",
                                  label = "Price Prediction",
                                  value = TRUE),
                    hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),hr(),
                    sliderInput(inputId = "bins",
                                label = "Number of bins:",
                                min = min(train1$YearBuilt),
                                max = max(train1$YearBuilt),
                                value = c(1885,1990))
                    
                    
                  ),
                  
                  # Output:
                  mainPanel( 
                    
                    tabsetPanel(type = "tabs",
                                id = "tabsetpanel",
                                tabPanel(title = "Plot", 
                                         plotOutput(outputId = "histogram"),
                                         br(),
                                         h5(textOutput("description")),
                                         plotOutput(outputId = "hist"),
                                         plotOutput(outputId = "polygon")),
                                tabPanel(title = "Predicted_Values", 
                                         br(),
                                         DT::dataTableOutput(outputId = "data_stable"),
                                         plotOutput(outputId = "line"),
                                         br(),
                                         plotOutput(outputId = "scatterplot")),
                                # New tab panel for Codebook
                                tabPanel("Fitted model", 
                                         br(),
                                         verbatimTextOutput(outputId = "Fitted_model"),
                                         verbatimTextOutput(outputId = "fitted_mod")
                                         
                                ),
                                tabPanel(
                                  "prediction" ,
                                  numericInput("X", "Enter the number of FIREPLACES", 1,min = 0, max = 4),
                                  numericInput("Y", "Enter the CENTARL AIR CONDITION number:", 1,min = 0, max = 2),
                                  numericInput("Z",  "Enter the number of CARS in garage :", 1,min = 0, max = 10),
                                  numericInput("W", "the OVERALL QUALITY of the house should be:", 1,min = 0, max = 10),
                                  h5("SALEPRICE",color="blue"),
                                  textOutput("text_calc")
                                  
                                ))
                  )
                )
)




# Define server function required to create the scatterplot
server <- function(input, output, session) {

  # Create a subset of data filtering for selected title types
  movies_selected <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(movies, title_type %in% input$selected_type)
  })
  
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  # Create scatterplot object the plotOutput function is expecting 
  output$histogram <- renderPlot({
    ggplot(data = train, 
           aes(train$YrSold, train$SalePrice,col=train$GarageCars)) +
      geom_bar(position = "dodge", stat = "identity") + ylab("Saleprice (in ruppee)")+
      xlab("Year Sold") + theme(legend.position="bottom" 
                                ,plot.title = element_text(size=15, face="bold")) + theme(panel.background = element_rect(fill = "lemonchiffon"))+
      ggtitle(" Yearly Sale Price of the houses sold w.r.t Garage Cars ")
  })
  output$hist<-renderPlot({
    ggplot(data = train,aes(x=train$SalePrice))+geom_histogram(fill="yellow",col="red")+theme(panel.background = element_rect(fill = "lemonchiffon"))+xlab("Saleprice")
    })
  
  # Create description of plot
  output$description <- renderText({
    paste("The plot above shows the relationship between",
          input$x,
          "and",
          input$y,
          "for",
          nrow(data()),
          "movies.")
  })
  
  
  output$line<-renderPlot({
    ggplot(data = p,aes_string(x=p$predict))+geom_freqpoly(bins=100)+xlab("Predicted Values")+theme(panel.background = element_rect(fill = "lightblue"))
  })
  output$scatterplot<-renderPlot({
    plot(p$predict)
  })
  
  output$polygon<-renderPlot({
    binsView <- seq(input$bins[1], input$bins[2])
    f1_new <- data1[which(train_4$YearBuilt %in% binsView),] 
    ggplot(data = f1_new, 
           aes(x=YearBuilt, y=SalePrice, fill=factor(CentralAir))) + 
      geom_histogram(position = "dodge", stat = "identity") + ylab("Saleprice (in ruppee)")+
      xlab("Yearbuilt") + theme(legend.position="bottom" 
                                ,plot.title = element_text(size=15, face="bold")) + theme(panel.background = element_rect(fill = "lemonchiffon"))+
      ggtitle("salepricebyyearbuilt") + labs(fill = "centralAir")
  
  })
  
  
  
  
  
  
  #creating data table.
  data_var<-test[,c("GarageCars","CentralAir","YearBuilt","OverallQual","LotShape","SaleCondition")]
  data_var1<-cbind(data_var,p) 
  # Print data table if checked
  output$data_stable <- DT::renderDataTable(
    DT::datatable(data = data_var1[, 1:7], 
                  options = list(pageLength = 10),
                  rownames = FALSE)
  )
  
  
  
  # create regression output:
output$Fitted_model <-renderPrint({
    X<-data1 %>% pull(input$x)
    
    Y<-data1 %>% pull(input$y)
    sum<- summary(lm(SalePrice~X+Y,data=data1))
    print(sum, digits = 3, signif.stars = FALSE)
    
})

output$fitted_mod<-renderPrint({
  summ<-summary(lm(train6 ~ . , data=train_4))
  print(summ, digits = 3, signif.stars = FALSE)
})


# Display data table tab only if show_data is checked
observeEvent(input$show_data, {
  if(input$show_data){
    showTab(inputId = "tabsetpanel", target = "Predicted_Values", select = TRUE)
  } else {
    hideTab(inputId = "tabsetpanel", target = "Predicted_Values")
  }
})

output$text_calc <- renderText({
  x <- input$X
  y <- input$Y
  z <- input$Z
  w <- input$W
  paste("The SALE_PRICE of your dream HOUSE will be : =", exp(10.389199+(0.103083*x)+(0.170178*y)+(0.143818*z)+(0.162024*w)))
})

  
}


# Create Shiny app object
shinyApp(ui = ui, server = server
         )
