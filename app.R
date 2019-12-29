library(shiny)
library(plyr)
library(dplyr)
library(kableExtra)
library(knitr)
library(ggplot2)
library(plotly)
library(DT)

get_season_statistics <- function(all_data) {
    all_statistics <- all_data %>% mutate(home_team_point = case_when(FTR == 'H' ~ 3, FTR == 'D' ~ 1, FTR == 'A' ~ 0),
                                          away_team_point = case_when(FTR == 'A' ~ 3, FTR == 'D' ~ 1, FTR == 'H' ~ 0),
                                          home_team_point_half = case_when(weekIndex < 18 & FTR == 'H' ~ 3, 
                                                                           weekIndex < 18 & FTR == 'D' ~ 1, 
                                                                           weekIndex < 18 & FTR == 'A' ~ 0, 
                                                                           weekIndex >= 18 ~ 0),
                                          away_team_point_half = case_when(weekIndex < 18 & FTR == 'A' ~ 3, 
                                                                           weekIndex < 18 & FTR == 'D' ~ 1, 
                                                                           weekIndex < 18 & FTR == 'H' ~ 0, 
                                                                           weekIndex >= 18 ~ 0))
    home_season_statistics <- all_statistics %>% group_by(season, HomeTeam) %>% 
        summarise(point = sum(home_team_point), HG = sum(FTHG), HC = sum(FTAG), difference = HG - HC) %>% 
        mutate(team=HomeTeam) %>% 
        select(season, team, point, HG, HC) %>% 
        arrange(desc(point))
    
    away_season_statistics <- all_statistics %>% group_by(season, AwayTeam) %>% 
        summarise(point = sum(away_team_point), AG = sum(FTAG), AC = sum(FTHG)) %>% 
        mutate(team=AwayTeam) %>% 
        select(season, team, point, AG, AC) %>% 
        arrange(desc(point))
    
    season_statistics<- full_join(home_season_statistics,away_season_statistics,by=c("season","team"))%>%
        mutate(total_point=point.x+point.y, goal_scored = HG + AG, goal_conceded = HC + AC,
               goal_difference = goal_scored - goal_conceded) %>% arrange(desc(season,total_point, goal_difference)) %>% 
        select(season, team, total_point, goal_scored, goal_conceded, goal_difference) %>% arrange(desc(total_point))
}

get_data_url <- function(start_year) {
    data_url <- paste("https://github.com/pjournal/mef03g-mujde-r/blob/master/Group%20Project/Raw%20Data/", start_year, "-",
                      start_year + 1, ".csv?raw=true", sep='')
    return (data_url)
}

get_data_between_years <- function (start_year, end_year){
    all_raw_data <- data.frame()
    for (year in seq(start_year, end_year)) {
        data_url <- get_data_url(year)
        raw_data <- read.csv(data_url,skip=1,sep=',',header=F)
        raw_data <- raw_data %>% mutate(V1 = year)
        if(year < 1999){
            all_raw_data <- rbind.fill(all_raw_data, raw_data)
        } else if(year < 2017){
            ## First 10 variables
            all_raw_data <- rbind.fill(all_raw_data, raw_data[c(1:10)])
        } else{
            ## First 22 variables
            all_raw_data <- rbind.fill(all_raw_data, raw_data[c(1:22)])
        }
    }
    return (all_raw_data)
}
raw_data <- get_data_between_years(1994, 2018)
totalWeekCount = 34
## As 18 teams are in league, there are 9 matches every week
totalMatchPerWeek = 9
## In 2009 there are 17 teams in league therefore there are 8 matches every week
totalMatchPerweek_2009 = 8
totalMatchCountUntil2009 = totalWeekCount * totalMatchPerWeek * 15
totalMatchCountIn2009 = totalWeekCount * totalMatchPerweek_2009

all_data <- raw_data %>% mutate(rowIndex = seq.int(nrow(raw_data)))
all_data <- all_data %>% mutate(weekIndex = case_when(V1 < 2009 ~ (((rowIndex - 1) %/% totalMatchPerWeek) %% 34) + 1,
                                                      V1 == 2009 ~(((rowIndex - totalMatchCountUntil2009 - 1) %/% totalMatchPerweek_2009) %% 34 + 1),
                                                      V1 > 2009 ~(((rowIndex - totalMatchCountUntil2009 - totalMatchCountIn2009 - 1) %/%
                                                                       totalMatchPerWeek) %% 34 + 1)))

colnames(all_data)<-c("season","date","HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR","HS","AS","HST","AST","HF","AF","HC","AC","HY","AY","HR","AR", "rowIndex", "weekIndex")

all_data[all_data == "Antalya"] <- "Antalyaspor"
all_data[all_data == "Kayseri"] <- "Kayserispor"
all_data[all_data == "Istanbulspor"] <- "Buyuksehyr"
all_data[all_data == "Yeni Malatyaspor"] <- "Malatyaspor"
all_data[all_data == "Hacettepespor"] <- "Ankaraspor"
all_data[all_data == "Osmanlispor"] <- "Ankaraspor"
all_data[all_data == "Erzurum BB"] <- "Erzurumspor"
all_data[all_data == "Erzurum"] <- "Erzurumspor"

ui <- fluidPage(

    titlePanel("Turkish Football League"),

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId ="season",
                        label = "Season",
                        choices = seq(1994,2018,1)),
            sliderInput(inputId ="week", 
                        label = "Match Week",
                        min = 1,
                        max = 34,
                        value = 25),
            radioButtons(inputId = "choice",
                         label = "Report Type",
                         c("Table", "Plot"))
        ),

        mainPanel(
           plotlyOutput("leaguePlot"),
           dataTableOutput("leagueTable")
        )
    )
)

server <- function(input, output) {
    output$leaguePlot <- renderPlotly ({
        if(input$choice == "Plot"){
            tableData <- all_data %>% filter(season == input$season, weekIndex <= input$week)
            season_statistics <- get_season_statistics(tableData)
            p <- ggplot(season_statistics, aes(x = reorder(team, total_point), y = total_point, fill=team)) +
            geom_bar(stat = "identity")  + coord_flip() + labs(title="Puan Sıralaması", x="Takım", y="Puan")
            ggplotly(p)
        }
     })
    output$leagueTable <- renderDataTable({
        if(input$choice == "Table"){
            tableData <- all_data %>% filter(season == input$season, weekIndex <= input$week)
            season_statistics <- get_season_statistics(tableData)
            season_statistics           
        }
    })
}

shinyApp(ui = ui, server = server)
