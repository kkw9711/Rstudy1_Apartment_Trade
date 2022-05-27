install.packages("devtools")
install_github("dkahle/ggmap")
install.packages("ggplot2")
install.packages("shinydashboard")
install.packages("geosphere")
install.packages("validColors")
install.packages("DT")
install.packages("ggrepel")
install.packages("tidyr")
install.packages("shinycssloaders")
install.packages("shinythemes")
install.packages("SwimmeR")
install.packages("googleway")
install.packages("rsconnect")

library(shiny)
library(ggplot2)
library(readxl)
library(devtools)
library(ggmap)
library(dplyr)
library(geosphere)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(googleway)
library(rsconnect)


# 배포

rsconnect::setAccountInfo(rsconnect::setAccountInfo(name='stationproject', token='B4661B8D805E51606A20EFE4AED5F674', secret='d1g0yaaLBYYjE/GK3h1Y3xJr2E5k6AxJXz3yXS5Z'))

#APIKey등록

googleAPIkey <- ""
register_google(googleAPIkey)

#geocode 함수로 station_code 값을 위도와 경도로 변환한다.

station_code <- station_data$역주소 %>% enc2utf8 %>% geocode()

head(station_code)

# station_data하고 station_code를 결합하고 station_code_final에 할당한다. 이후 # head로 앞 부분 데이터 확인

station_code_final <- cbind(station_data, station_code)
head(station_code_final)

####

apartment_data <- read.csv("C:/Users/ruddn/Documents/Rstudy/apart_Trading_Price.csv")
head(apartment_data)


# 반올림
apartment_data$전용면적 = round(apartment_data$전용면적)
head(apartment_data)


# 전용면적 기준으로 빈도가 가장 높은것을 토대로 / 내림차순 정렬

count(apartment_data, 전용면적) %>% arrange(desc(n))
head(apartment_data)


# 아파트  전용면적 빈도가 가장 높은 것
apartment_data_85 <- subset(apartment_data, 전용면적 == "85")
head(apartment_data_85)

# 쉼표 -> 공백
apartment_data_85$거래금액 <- gsub(",","",apartment_data_85$거래금액)
head(apartment_data_85)
str(apartment_data_85$거래금액)

#거래금액을 문자형에서 정수형으로 변환 후 단지명 별 평균 계산 후 할당 / 데이터를 그룹별로 묶어서 연산하기

apartment_data_85_cost <- aggregate(as.integer(거래금액) ~ 단지명, apartment_data_85 , mean)
apartment_data_85_cost <- rename(apartment_data_85_cost,"거래금액" = "as.integer(거래금액)")
head(apartment_data_85_cost)


# 중복된 행 제거 
apartment_data_85 <- apartment_data_85[!duplicated(apartment_data_85$단지명),]
head(apartment_data_85)


#단지명 기준으로 데이터를 합치기 

apartment_data_85 <- left_join(apartment_data_85, apartment_data_85_cost, by = "단지명")

# 거래금액을 하나로 통일
apartment_data_85 <- apartment_data_85 %>% select("단지명", "시군구", "번지", "전용면적", "거래금액.y")
apartment_data_85 <- rename(apartment_data_85, "거래금액" = "거래금액.y")
View(apartment_data_85)

#시군구 번지를 하나의 주소로 통합

apartment_Address <- paste(apartment_data_85$"시군구", apartment_data_85$"번지") %>% data.frame()
apartment_Address <- rename(apartment_Address, "주소" = ".")
View(apartment_Address)


# 아파트 주소를 위도경도로 변환 해 새 변수에 할당 (구글 지도에 표시하기 위함)
apartment_Address_code <- as.character(apartment_Address$주소) %>% enc2utf8() %>% geocode()


# 아파트 실거래가 데이터 최종 종합 단지명, 전용면적, 거래금액, 주소, 위도, 경도 (cbind를 통한 열 결합)
apartment_code_final <- cbind(apartment_data_85, apartment_Address, apartment_Address_code) %>% 
  select("단지명","전용면적","거래금액","주소", "lon","lat")

head(apartment_code_final)

# 마포구 지도를 가져와 저장

mapo_map <- get_googlemap("Mapo station", maptype = "roadmap", zoom = 12)

ggmap(mapo_map)
# 산점도로 지하철역 위치 및 역명 표시하기

ggmap(mapo_map) +
  geom_point(data = station_code_final, aes(x = lon, y = lat),
             colour = "red", size =3) + 
  geom_text(data = station_code_final, aes(label = 역명, vjust = -1))

# 홍대역 지도의 정보를 가져와 변수에 저장

test_map <- get_googlemap("hongdae station", maptype = "roadmap", zoom = 15)

# 홍대입구역 지도에 지하철 정보 및 아파트 정보 일괄 표시

ggmap(test_map) +
  geom_point(data = station_code_final, aes(x = lon, y = lat), colour = "red", size = 2) +
  geom_text(data = station_code_final, aes(label = 역명, vjust = -1)) +
  geom_point(data = apartment_code_final, aes(x = lon, y = lat)) + 
  geom_text(data = apartment_code_final, aes(label = 단지명, vjust = -1)) + 
  geom_text(data = apartment_code_final, aes(label = 거래금액, vjust = 1))
  
  ###########
  
hongdae_dist_value <- geocode("hongdae station", source = "google") # 홍대입구역의 경도 ,위도
geocode("hongdae station", source = "google")
apartment_distance_location <- apartment_code_final %>% select("lon", "lat") # 단지별 거리 계산을 위한 데이터 세트

# 홍대입구역 좌표와 단지별 좌표의 거리를 각각 계산 후 "거리" 열을 만들어 데이터를 추가
apartment_code_final[, "거리"] =  distGeo(c(126.9245, 37.55753), apartment_distance_location)

# 거리를 가장 가까운 곳을 찾기 위해 오름차순으로 정렬
apartment_Address_close <- arrange(apartment_code_final,(거리))



View(apartment_Address_close)

#####################################Text Input....#################### shiny


# WebApp

ui <- fluidPage(
  
  navbarPage("2호선 지하철역 근처 아파트 실거래가", 

            tabPanel("Data",
            h2("지하철역 및 아파트 데이터"),
            DT::dataTableOutput("stationTable"),
  
          hr(),
        
          fluidRow(
            h2("금액 범위 지정"),
            
            column(4,
            sliderInput("siPriceRange", min = 0,  max = 200000, label = "Slider", value = c(80000, 125000))),
          
            
            column(4, textInput("text", label = h3("부터"), value = "Text input....")),
                                
            column(4, textInput("text", label = h3("까지"), value = "Text input....")),
            actionButton(inputId = "aasdfe", label = "조회"),
          ),
          
          fluidRow(
            column(4, verbatimTextOutput("resultPrice"))
            )
            ),
  

      
        tabPanel("Map",
                 sidebarLayout( 
                   sidebarPanel(
                     textInput(inputId = "txt_StationName", label = h3("2호선 지하철역 입력"), value = "Text Input...."),
                     actionButton(inputId = "btn_StationName", label = "조회"),
                     verbatimTextOutput("searchStation"),
                     hr(),
                     
                     selectInput("select", label = h3("2호선 지하철역 목록"), 
                                 choices = list("홍대입구역" = "hongdae station", 
                                                "합정역" = "hapjeong station", 
                                                "신촌역" = "sinchon station"), 
                                 selected = 1),
                     actionButton(inputId = "selectBtn_StationName", label = "조회"),
                     
                     hr(),
                     fluidRow(column(6, verbatimTextOutput("value")))
                   ),
                   mainPanel(
                     wellPanel(
                        plotOutput(outputId = "pjPlot", width = "100%", height = "800px")
             )
           )
        )
     )
  )
)


server <- function(input, output){

  ## data set tab
  
  output$stationTable = DT::renderDataTable({
    
    apartment_Address_close
    
  })
  
  output$resultPrice <- renderPrint({ 
    
    input$siPriceRange 
    
    })

  
  output$value <- renderPrint({ input$select })
  
  ## Default Map tab
  
  output$pjPlot <- renderPlot({
    
    searchStationValue <- reactiveValues(data = NULL)
    searchStationValue$input_value <- input$txt_StationName # serachStationValue에 text에 들어온 값을 할당
    
    googleAPIkey <- "AIzaSyBAfz6zAe3YlcbpoW5uK_ic9JaSbYPQCM8"
    register_google(googleAPIkey)
    
    projectMap <- get_googlemap("sinchon",
                                maptype = "roadmap", 
                                zoom = 13) 
    
    ggmap(projectMap) +
      geom_point(data = station_code_final, aes(x = lon, y = lat),
                 colour = "red", size =3) + 
      geom_text(data = station_code_final, aes(label = 역명, vjust = -1))
    
    ggmap(test_map) +
      geom_point(data = station_code_final, aes(x = lon, y = lat), colour = "red", size = 3) +
      geom_text(data = station_code_final, aes(label = 역명, vjust = -1)) +
      geom_point(data = apartment_code_final, aes(x = lon, y = lat), colour = "blue", size = 3) + 
      geom_text(data = apartment_code_final, aes(label = 단지명, vjust = -1)) + 
      geom_text(data = apartment_code_final, aes(label = 거래금액, vjust = 1))
    #str(searchStationValue$input_value)
  })

  ## search station
  
  searchStationValue <- reactiveValues(data = NULL) 
  
  observeEvent(input$btn_StationName, { # btn_StationName 버튼 이벤트 / 버튼 클릭시 
    searchStationValue$input_value <- input$txt_StationName # serachStationValue에 text에 들어온 값을 할당
    
    output$searchStation <- renderPrint({ # 출력 될 ouput변수 serachStation에 글자 render
      searchStationValue$input_value # serachStation의 값을 
   
      })
  })
}

shinyApp(ui, server)
