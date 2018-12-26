

data<-read.csv(file.choose(),header = T)

data
attach(data)
head(data)
summary(data1$CentralAir)





data1<-subset(data,YearBuilt>=2000)
## app.R ##
library(shiny)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)
server <- function(input, output) { }
shinyApp(ui, server)




#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "DASHBOARD for house price prediction")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("About project",tabName = "dashboard",icon = icon("dashboard")),
    menuItem("Raw Data",tabName = "dashboard",icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.salesforce.com")
  )
)




library(ggplot2)

# In my opinion we are taking "value1","value2","value3" because we are going to create ggplot in which 
## three values are required.

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow2 <- fluidRow( 
  box(
    title = " SalePricepercentral air"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("Salepricebyyearbuild", height = "300px")
  )
  ,box(
    title = "Salepriceperfireplaces"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("Salepriceforyearbuilt", height = "300px")
  ) 
)
dashboardBody(tabItems(tabItem(tabName = "Raw Data",fluidRow(box(width = "150",height = "1500",
                                                                 fluidRow(column(1,selectInput("neigh","neighbour",c("All",unique(as.character(data1$Neighborhood))),selected = "All"))
                                                                          ),
                                                                 column(2,actionButton("button1","get detail")),
                                                                 column(2,actionButton("reset","reset detail")))))))

fluidRow( column(1,selectInput("neigh",
                                      "neighbour",
                                      c("All",unique(as.character(data1$neighbour))),selected="All")
))




# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2,frow3)



#creating main title of Your page.
#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'HOUSE PRICE', header, sidebar, body, skin='red')

summary(data$YearBuilt)


# server part.
data$YearBuilt <- as.character(data$YearBuilt)
data$YearBuilt_category <- ifelse(data$YearBuilt<=1900,"upto 1900 year",ifelse(data$YearBuilt<1915
& data$YearBuilt>1900,"1900 to 1915 years",ifelse(data$YearBuilt>1900 & data$YearBuilt<1945,"1900-45 years",
ifelse(data$YearBuilt>1945&data$YearBuilt<1980,"1945-80",ifelse(data$YearBuilt>1980&data$YearBuilt<1990,"1980-90",ifelse(data$YearBuilt>1990&data$YearBuilt<2000,"1990-2000","more than 2000"))))))


library(ggplot2)
#creating the plotOutput content
server <- function(input, output) {
output$Salepricebyyearbuild<-renderPlot({
  ggplot(data = data, 
         aes(x=YearBuilt, y=SalePrice, fill=factor(CentralAir))) + 
    geom_histogram(position = "dodge", stat = "identity") + ylab("Saleprice (in ruppee)")+
    xlab("Yearbuilt") + theme(legend.position="bottom" 
                            ,plot.title = element_text(size=15, face="bold")) + theme(panel.background = element_rect(fill = "lemonchiffon"))+
    ggtitle("salepricebyyearbuilt") + labs(fill = "centralAir")
  data$YearBuilt_category
})
data$YearBuilt_category
output$Salepriceforyearbuilt <- renderPlot({
  ggplot(data = data1, 
         aes(x=YearBuilt, y=SalePrice, fill=as.factor(Fireplaces))) + 
    geom_bar(position = "dodge", stat = "identity") + ylab("SalePrice (in rupee)") + 
    xlab("YearBuilt") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))+
    ggtitle("salePriceByFireplaces") + labs(fill = "Fireplaces")+ facet_grid(.~as.factor(Fireplaces))+theme(axis.text.x = element_text(angle=90))
})
}




library(png)
dddddimg <- readPNG("C:\\Users\\ankita\\Desktop\\New folder\\house image.png")
g <- rasterGrob(img, interpolate=TRUE) 
packs <- c("png","grid")
lapply(packs, require, character.only = TRUE) 

attach(data1)
#run/call the shiny app
shinyApp(ui, server)


getwd()

