library(shiny)
library(tidyverse)
library(ggmap)
library(googleway)
library(ruler)


source("utils.R", encoding = "UTF-8")

data <- read.csv('final7.csv', fileEncoding="cp949")
register_google(key="AIzaSyBvzYxXWp7cd9ahv3TYJb0xJo8W_6tG0SE")
icn <- get_map("Incheon", zoom=7, maptype="roadmap")

ui <- fluidPage(
  titlePanel(strong('인천광역시 맞춤형 전기차 충전소 입지선정'),
             windowTitle = '인천광역시 맞춤형 전기차 충전소 입지선정'),
  sidebarLayout(
    sidebarPanel(
      
      h3(strong('기본 설정')),
      
      br(),
    

      selectInput('candidate',
                  label = strong('입지 선정 개수 입력'),
                  choices = c(1:10),
                  selected = '선택안함'
      ),

      
      sliderInput('lot_size', 
                  label = strong('주차장 면수 제한'),
                  min=0, max=1157, value=c(50, 100)),
      
      radioButtons("algorithm", 
                   label = "분석 알고리즘",
                   selected = "P-median",
                   inline=TRUE,
                   choices =  c("MCLP", "P-median")),
      br(),
      
      h3(strong('입력변수 가중치 설정')),
      
      br(),
      
      selectInput('charger_location',
                  label = strong('반경 500m 전기차 충전소 개수(역수)'),
                  choices = c(0:3),
                  selected = '선택안함'
      ),

      selectInput('pop',
                  label = strong('세대수'),
                  choices = c(0:3),
                  selected = '선택안함'
      ),
      
      selectInput('car_reg',
                  label = strong('전기차 등록수'),
                  choices = c(0:3),
                  selected = '선택안함'
      ),
      
      selectInput('building',
                  label = strong('반경 500m 주차장 주변 건물수(제 1 근린시설)'),
                  choices = c(0:3),
                  selected = '선택안함'
      ),
      
      
      br(),
      
      actionButton("analyze", "분석하기")
      
      
    ),
    mainPanel(
      DT::dataTableOutput("table"),
      google_mapOutput(outputId="map", height="1000px")
    )),
  hr(),
  div(
    class = "footer",
    includeHTML("footer.html")
  )
)

server <- function(input, output) {
  observeEvent(input$analyze, {
    data = data %>% filter(면수 >= input$lot_size[1] & 면수 <= input$lot_size[2])
    
    if(input$algorithm == "P-median") {
      result <- pmedian(num = as.integer(input$candidate),
               pop = as.integer(input$pop),
               building = as.integer(input$building),
               ev.location = as.integer(input$charger_location),
               ev.num = as.integer(input$car_reg),
               dtframe = data)
      rownames(result) <- NULL
    } else {
      result <- simpleMCLP2(candidate = as.integer(input$candidate),
                        pop = as.integer(input$pop),
                        building = as.integer(input$building),
                        charger_location = as.integer(input$charger_location),
                        car_reg = as.integer(input$car_reg),
                        dtframe = data)
      rownames(result) <- NULL
    }

    output$map <- renderGoogle_map({
      google_map(data=result, key="AIzaSyBFU-yod8bo6R2ZFp-QGpOIVVvQvySRkE0") %>%
        add_markers(lat="lat", lon="lng", mouse_over="name")
    })
    output$table <- DT::renderDataTable({result})
  })
}

shinyApp(ui = ui, server = server)