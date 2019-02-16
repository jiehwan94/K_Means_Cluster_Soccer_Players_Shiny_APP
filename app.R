library(shiny)
library(shinysky)
library(dplyr)
library(radarchart)
library(tidyr)
library(shinydashboard)
library(flexdashboard)
library(shinyjs)
library(plotly)

ui2<- dashboardPage(
  dashboardHeader(title="BIG Player Hunter"),
  dashboardSidebar(
    fluidRow(
      column(12, align="center",
             uiOutput(outputId = "image",height='48px')
      )
             ),
    selectizeInput('player_name',"Player Name:",
                   choices=fifa_data$Name,
                   selected=NULL,
                   multiple=TRUE),
    sliderInput("player_count",
                "Number of players:",
                min=1,
                max=50,
                value=5),
    sliderInput("proximity",
                "How close:",
                min=0.01,
                max=0.99,
                value=0.05),
    sliderInput("valuerange", "Price Range", min = 0, max = max(fifa_data$ValueNumeric_pounds), 
                value = c(25, 100000000)),
    
    actionButton("search", "Search"),
    # actionButton("create", "Create"),
    
    
    uiOutput("checkbox")
  ),
  dashboardBody(

              verbatimTextOutput('text1'),
              # dataTableOutput("table3"),
              dataTableOutput("table2"),
              # dataTableOutput("table1"),
              DT::DTOutput("table1"),
              fluidRow(class="myRow1", align="center", plotlyOutput("plot1")),
              fluidRow(align="center", chartJSRadarOutput("radarchart1")),
              tags$head(tags$style("
                .myRow1{height:600px;}"
                                   ))
  )
)

server <- shinyServer(function(input, output, session) {
  observe({
    # radarDF<- reactiveValues(aaa = NULL)
    
    observeEvent(input$search,{
        
      # radarDF$aaa<- data.frame()
      
      output$image<- renderUI({
        tags$img(src= fifa_data[fifa_data$Name==input$player_name,]$Photo)
      })
      
      
      df<- return_similar_players(input$player_name,
                                  input$player_count,
                                  input$proximity)
      
      output$table1<- DT::renderDT({DT::datatable(as.data.frame(df[c(-9,-10)]),
                                                  escape = FALSE,
                                                options=list(orderClasses = TRUE,
                                                lengthMenu = c(5, 10, 30), pageLength = 8))})
      
      # output$text1<- renderPrint({
      #   s = input$table1_rows_selected
      #   if (length(s)) {
      #     cat('These rows were selected:\n\n')
      #     cat(s, sep = ', ')
      #     cat(class(s))
      #   }
      # })
      
      
      f<- merge(x= df, y= fifa_data, by="ID")

      ff<-f[,c("Name.y","Overall.x",
               "Crossing","Finishing","HeadingAccuracy","ShortPassing","Volleys","Dribbling",
               "Curve","FKAccuracy","LongPassing","BallControl","Acceleration","SprintSpeed","Agility",
               "Reactions","Balance","ShotPower","Jumping","Strength","LongShots","Aggression","Interceptions",
               "Positioning","Vision","Penalties","Composure","Marking","StandingTackle","SlidingTackle")]

      colnames(ff)<- c("Name","Overall",
                       "Crossing","Finishing","HeadingAccuracy","ShortPassing","Volleys","Dribbling",
                       "Curve","FKAccuracy","LongPassing","BallControl","Acceleration","SprintSpeed","Agility",
                       "Reactions","Balance","ShotPower","Jumping","Strength","LongShots","Aggression","Interceptions",
                       "Positioning","Vision","Penalties","Composure","Marking","StandingTackle","SlidingTackle")


      radarDF <- gather(ff[1:input$player_count,], key=Label, value=Overall, -Name) %>%
        spread(key=Name, value=Overall)

      output$radarchart1 <- renderChartJSRadar({chartJSRadar(scores = radarDF,
                                                             maxScale = 100,
                                                             showToolTipLabel = TRUE)})
      
      aa<-  df%>%
        mutate(Cluster = paste("Cluster: ", Cluster, sep = "")) %>%
        arrange(desc(Overall)) %>% 
        group_by(Cluster) %>%
        mutate(Under27 = ifelse(Age < 27, "Yes", "No")) %>%
        ggplot(aes(x= Overall, y= ValueNumeric_pounds)) +
        geom_point(position = "jitter", shape = 21, color = "black", size = 2, aes(text = paste(Name, PositionGroup), fill = Under27)) +
        scale_fill_manual(values = c("red", "green")) +
        scale_y_continuous(labels = dollar_format(prefix = "€")) +
        ggtitle("Player's overall rating plotted against their value within the cluster")
      
      output$plot1<- renderPlotly({ggplotly(aa, height = 600, width = 800)}) 
      
    })
    
    # observeEvent(input$create, {
    #   dd= input$table1_rows_selected
    #   if (length(dd)){

    #     output$table2<- renderDataTable({as.data.frame(df[c(dd),])})
    #     
    #     f<- merge(x= as.data.frame(df(dd),]), y= fifa_data, by="ID")
    #     
    #     ff<-f[,c("Name.y","Overall.x",
    #              "Crossing","Finishing","HeadingAccuracy","ShortPassing","Volleys","Dribbling",
    #              "Curve","FKAccuracy","LongPassing","BallControl","Acceleration","SprintSpeed","Agility",
    #              "Reactions","Balance","ShotPower","Jumping","Strength","LongShots","Aggression","Interceptions",
    #              "Positioning","Vision","Penalties","Composure","Marking","StandingTackle","SlidingTackle")]
    #     
    #     colnames(ff)<- c("Name","Overall",
    #                      "Crossing","Finishing","HeadingAccuracy","ShortPassing","Volleys","Dribbling",
    #                      "Curve","FKAccuracy","LongPassing","BallControl","Acceleration","SprintSpeed","Agility",
    #                      "Reactions","Balance","ShotPower","Jumping","Strength","LongShots","Aggression","Interceptions",
    #                      "Positioning","Vision","Penalties","Composure","Marking","StandingTackle","SlidingTackle")
    #     
    #     
    #     radarDF2 <- gather(ff[1:input$player_count,], key=Label, value=Overall, -Name) %>%
    #       spread(key=Name, value=Overall)
    #     
    #     output$radarchart1 <- renderChartJSRadar({chartJSRadar(scores = radarDF2,
    #                                                            maxScale = 100,
    #                                                            showToolTipLabel = TRUE)})
    #   }
    # })
    
    # observeEvent(input$create,{
    #       # output$radarchart1<- renderChartJSRadar({
    #       dd = input$table1_rows_selected
    #       if (length(dd)) {
    #         # output$table3<- renderDataTable({as.data.frame(df[c(dd),])}) 
    # 
    # 
    #         temp_df<- as.data.frame(df[c(dd),])
    # 
    #         f<- merge(x= temp_df, y= fifa_data, by="ID")
    # 
    #         ff<-f[,c("Name.y","Overall.x",
    #                  "Crossing","Finishing","HeadingAccuracy","ShortPassing","Volleys","Dribbling",
    #                  "Curve","FKAccuracy","LongPassing","BallControl","Acceleration","SprintSpeed","Agility",
    #                  "Reactions","Balance","ShotPower","Jumping","Strength","LongShots","Aggression","Interceptions",
    #                  "Positioning","Vision","Penalties","Composure","Marking","StandingTackle","SlidingTackle",
    #                  "GKDiving","GKHandling","GKKicking","GKPositioning","GKReflexes","Skill Moves","Weak Foot")]
    # 
    #         colnames(ff)<- c("Name","Overall",
    #                          "Crossing","Finishing","HeadingAccuracy","ShortPassing","Volleys","Dribbling",
    #                          "Curve","FKAccuracy","LongPassing","BallControl","Acceleration","SprintSpeed","Agility",
    #                          "Reactions","Balance","ShotPower","Jumping","Strength","LongShots","Aggression","Interceptions",
    #                          "Positioning","Vision","Penalties","Composure","Marking","StandingTackle","SlidingTackle",
    #                          "GKDiving","GKHandling","GKKicking","GKPositioning","GKReflexes","Skill Moves","Weak Foot")
    # 
    # 
    #         radarDF <- gather(ff[1:input$player_count,], key=Label, value=Overall, -Name) %>%
    #           spread(key=Name, value=Overall)
    # 
    #         # return(chartJSRadar(scores = radarDF,
    #         #              maxScale = 100,
    #         #              showToolTipLabel = TRUE))
    # 
    #         output$radarchart1 <- renderChartJSRadar({chartJSRadar(scores = radarDF,
    #                                                                maxScale = 100,
    #                                                                showToolTipLabel = TRUE)})
    #       
    #         
    #       }
    #       
    #       
    #     # })
    # 
    # })

    
  })
})

shinyApp(ui = ui2, server = server)
  
  
  
