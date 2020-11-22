rm(list = ls())

library(RPostgreSQL)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(datasets)
library(plotly)

con <- dbConnect(PostgreSQL(), dbname = "opioids", user = "postgres")

s_year <- 2018

e_year <- 2018

drug_name <- dbGetQuery(con, "SELECT DISTINCT gennme FROM sdud_time;")

map_details <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
)


label_list <- list()

label_list[['nrx']] <- 'Number of Prescriptions'
label_list[['nur']] <- 'Number of Units'
label_list[['nur_adj']] <- 'MME Units'


ui <- fluidPage(
    headerPanel('Example'),
    sidebarPanel(
        selectInput('measure','Measure', c('nrx', 'nur', 'nur_adj')),
        selectInput('drug','Drug', drug_name$gennme),
        selected = 'nrx'),
    mainPanel(
        plotlyOutput('plot')
    )
)

server <- function(input, output) {

    temp_df <- dbGetQuery(con, "SELECT gennme, state_code, nrx, nur, nur_adj FROM sdud_time;")

    map_df <- reactive({temp_df %>% rename(meas = input$measure) %>%
                                    group_by(state_code, gennme) %>%
                                    summarise(meas = sum(meas)) %>%
                                    filter(gennme %in% input$drug)})

    output$plot <- renderPlotly(

        fig <- plot_geo(map_df(), locationmode = 'USA-states') %>%
                                 add_trace(z = ~meas,
                                 locations = ~state_code,
                                 color = ~meas,
                                 colors = 'Purples') %>%
            colorbar(title = input$measure, x = -0.2, y = 0.8) %>%
            layout(geo = map_details)

        )

}

shinyApp(ui,server)
