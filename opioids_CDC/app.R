rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(datasets)
library(plotly)
library(zoo)
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)
library(RColorBrewer)
library(ggpmisc)
library(gridExtra)

load('output/opi_deaths.Rda')

# height = 600, width = 800, unit = 'px',


meas_opts_list <- list()
stat_opts_list <- list()

meas_opts_list[['Prescriptions']] <- 'nrx'
meas_opts_list[['Units']] <- 'nur'
meas_opts_list[['Morphine Miligram Equivalent']] <- 'nur_adj'

stat_opts_list[['Share']] <- 'mkt_shr'
stat_opts_list[['Total']] <- 'raw'

map <- map_data("state")

ui <- fluidPage(

  fluidRow(
    column(3,
           selectInput('state','State:', state.abb)),
    column(3,
           sliderInput("year", "Years:",
                       min = 2006, max = 2018, value = c(2006,2018),  step = 1, sep = '')
    )

  ),

  hr(),

  mainPanel(

    div(tabsetPanel(type = "tabs",
                        tabPanel("Time Series", plotOutput("plot")),
                        tabPanel("Map", plotOutput("map")),
                        tabPanel("Correlation", plotOutput("correlation"))
    ), class = "span7")

    )
  )

server <- function(input, output) {

  plot_df <- reactive({

    opi_df %>% filter(state_abbr == input$state)

  })

  min_year <- min(opi_df$year)
  max_year <- max(opi_df$year)

  title_vals <- reactive({state.name[state.abb %in% input$state]})

  theme_opts <- list(theme(panel.background = element_blank(),
                             axis.line = element_line(),
                             legend.key = element_blank(),
                             legend.title = element_blank(),
                             legend.position = "top",
                             legend.text = element_text(size = 12),
                             axis.title = element_text(size = 12),
                             axis.text = element_text(size = 12),
                             plot.title = element_text(size = 14, hjust = 0.5)))

  rx_plot <- reactive({ggplot(plot_df()) +
                        geom_line(aes(x = year, y = rx_p100, lty = 'Prescriptions per 100'),
                                  color  = 'blue',
                                  alpha = 0.5,
                                  size = 1.5) +
                        scale_y_continuous(name = 'Prescriptions per 100') +
                        scale_x_continuous(name = 'Year',
                                           breaks = seq(min_year, max_year, 2)) +
                        scale_color_discrete(labels = c("Prescriptions per 100")) +
                        theme_opts +
                        labs(title = paste0('Prescriptions in ', title_vals()))})

  death_plot <- reactive({ggplot(plot_df()) +
                          geom_line(aes(x = year, y = deaths_p100k, lty = 'Prescriptions per 100'),
                                    color = 'red', alpha = 0.5, size = 1.5) +
                          scale_y_continuous(name = 'Deaths per 100,000') +
                          scale_x_continuous(name = 'Year',
                                             breaks = seq(min_year, max_year, 2)) +
                          scale_color_discrete(labels = c("Deaths per 100,000")) +
                          theme_opts +
                          labs(title = paste0('Deaths in ', title_vals()))})


  map_plot <- reactive({temp_df <- opi_df %>% filter(year >= input$year[1] & year <= input$year[2]) %>%
                                                group_by(state) %>%
                                                summarise(m_rat = mean(ratio, na.rm = TRUE)) %>%
                                                mutate(state = tolower(state))

                              temp_df <- merge(temp_df, map, by.x = 'state', by.y = 'region')

                              return(temp_df)

                              })

  corr_plot <- reactive({opi_df %>% filter(year >= input$year[1] & year <= input$year[2])})

  output$plot <- renderPlot(

    height = 500, width = 700, unit = 'px',

    grid.arrange(rx_plot(), death_plot(), ncol=2)

  )

  output$map <- renderPlot(

    height = 500, width = 700, unit = 'px',

    ggplot() + geom_polygon(data = map_plot(),
                            aes(x = long, y = lat, group = group, fill = m_rat),
                            color="white", size = 0.2) +
                theme(axis.line = element_blank()) +
                theme(panel.background = element_blank()) +
                theme(axis.title = element_blank()) +
                theme(axis.text = element_blank()) +
                theme(axis.ticks = element_blank()) +
                theme(legend.title = element_text(size = 14)) +
                theme(legend.key.size = unit(3.0, "lines")) +
                labs(fill = 'Deaths per 100,000 to Prescriptions per 100 Ratio') +
                theme(legend.position = 'top', legend.justification = 'center') +
                scale_fill_continuous(guide = guide_colourbar(direction = "horizontal",
                                                              title.position = 'top'),
                                      low = '#FCEAE9', high = '#961010')

  )

  output$correlation <- renderPlot(

    height = 450, width = 750, unit = 'px',

    ggplot(data = corr_plot(), aes(x = rx_p100, y = deaths_p100k)) +
      geom_point(aes(x = rx_p100, y = deaths_p100k), color = 'blue', alpha = 0.5) +
      geom_smooth(method = "lm", formula  = y~x, se = FALSE, color = 'black', alpha = 0.5) +
      theme_opts +
      theme(plot.title = element_text(size = 16, hjust = 0.5)) +
      labs(y = 'Deaths per 100,000', x = 'Prescriptions per 100', title = 'Deaths vs Prescriptions Over Time') +
      stat_poly_eq(formula = y~x,
                   eq.with.lhs = "Deaths~`=`~",
                   eq.x.rhs = "~x~Prescriptions",
                   aes(label = ..eq.label..),
                   parse = TRUE,
                   size = 5.0)
  )

}

shinyApp(ui,server)
