library(shinydashboard)
library(DT)
library(shiny)
library(jsonlite)
library(data.table)
library(ggplot2)

header_1 <- dashboardHeader(title =  "Yelp Data Analytics App")

sidebar_1<-  dashboardSidebar(
  sidebarMenu(
    
    menuItem("Basic DashBoard",tabName = "bdb",icon = icon("dashboard")),
    menuItem("Analytics Dashboard",tabName = "ad",icon = icon("dashboard"))
  )
)

body_1 <- dashboardBody(
  tabItems(
  tabItem(tabName = "bdb",h2("basic dashboard"),actionButton("data","Populate Dashboard"),
          HTML('<script type="text/javascript">
        $(document).ready(function() {
          $("#data").click(function() {
            $("#dt").html("<h5>Fetching Aggregated Data from MongoDB.....</h5>");
            $("#dt2").text("Fetching Aggregated Data from MongoDB.....");
          });
        });
      </script>
'),        
          
  fluidRow(
    box(title = "No of restaurants in Cities ",background = "maroon", plotOutput("dt")),
    box(title = "Cities and average ranking",background = "olive",plotOutput("dt2")),
    box(title = "Slice and aggregate example",background = "fuchsia",
        sliderInput("numbers",label = h5("No of entries"),min=1,max = 50,5),
        textInput("text", "query:")),
    box(title="Slice Output",dataTableOutput("dt3"))
    
  )
),

tabItem(tabName = "ad",
        h2("analytics tab item"))))


server <- function(input,output){
  
  observeEvent(input$data,{
    cat("upon pressing button")
    #sampled_yelp_data <- data_yelp[sample(nrow(data_yelp),100),cols]
    #withProgress("Fetching aggregated data from MongoDB",value = 0)
    
    count_per_city <- setDT(connection$aggregate('[{"$group":{
                                      "_id":"$state",
                                      "count":{"$sum":1}
                                            }}]
                                     '))
    aggregated_rating_data <- setDT(collection2$aggregate('[{
                                            "$group":
                                         {
                                          "_id":"$business_id",
                                          "avg_rating":{"$avg":"$stars"}
                                         }

                                              }]
                                         '))
    setkey(aggregated_rating_data,`_id`)
    
    business_names <- setDT(connection$find('{}',fields = '{"business_id":1,"name":1,"city":1,"state":1,"categories":1}'))
    setkey(business_names,business_id)
    rating_data_city <- business_names[aggregated_rating_data]
    avg_rating_city <- rating_data_city[,.("mean_avg_rating"=mean(avg_rating)),state][order(-mean_avg_rating)]
    
    city_data <- count_per_city[order(-count)][,city:=ifelse(.I <=10,`_id`,"others")]
    
    output$dt <- renderPlot(ggplot(city_data,aes(x=city,y=count))+geom_bar(stat = "identity",fill = "green",color="blue")+coord_flip())
    output$dt2 <-  renderPlot(ggplot(head(avg_rating_city,6),aes(x=state,y=mean_avg_rating))+geom_bar(stat="identity",color="red",fill="yellow"))
    output$dt3 <- renderDataTable(head(rating_data_city[grepl(input$text,categories),.(name,city,state,avg_rating)][order(-avg_rating)],input$numbers))
     
  }) 
  
}

ui <- dashboardPage(header_1,sidebar_1,body_1)
shinyApp(ui, server)