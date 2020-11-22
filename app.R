rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(datasets)
library(plotly)
library(zoo)

load('output/base_data.Rda')

meas_opts_list <- list()
stat_opts_list <- list()

meas_opts_list[['Prescriptions']] <- 'nrx'
meas_opts_list[['Units']] <- 'nur'
meas_opts_list[['Morphine Miligram Equivalent']] <- 'nur_adj'

stat_opts_list[['Share']] <- 'mkt_shr'
stat_opts_list[['Total']] <- 'raw'

ui <- fluidPage(

  fluidRow(
    column(3,
           selectInput('measure', 'Measure:', c('Prescriptions', 'Units', 'Morphine Miligram Equivalent'))
    ),
    column(2,
           selectInput('stat','Statistic:', c('Share', 'Total'))),
    column(2,
           selectInput('state','State:', state.abb)),
    column(3,
           sliderInput("year", "Years:",
                       min = 1991, max = 2019, value = c(1991,2019),  step = 1, sep = '')
    )

  ),

  hr(),

  plotOutput('plot'),

)

server <- function(input, output) {

  plot_df <- reactive({meas_inp <- meas_opts_list[[input$measure]]
                       stic_inp <- stat_opts_list[[input$stat]]

                       temp_df <- base_df[base_df$year >= input$year[1] & base_df$year <= input$year[2],
                                          c('year', 'quarter', 'gennme', 'state_code', meas_inp)]

                       temp_df  <- temp_df %>% filter(state_code %in% input$state) %>%
                        group_by(year, quarter) %>%
                        mutate(total = sum(!!sym(meas_inp))) %>%
                        mutate(mkt_shr = !!sym(meas_inp) / total)

                      temp_df$date <- as.yearqtr(paste0(temp_df$year, ' Q', temp_df$quarter))

                      if (stic_inp == 'raw'){

                        temp_df <- temp_df %>% rename(stic = meas_inp) %>%
                          arrange(year, quarter) %>%
                          ungroup(year, quarter) %>%
                          select(date, gennme, stic)

                      } else {

                        temp_df <- temp_df %>% rename(stic = stic_inp) %>%
                          arrange(year, quarter) %>%
                          ungroup(year, quarter) %>%
                          select(date, gennme, stic)

                      }})

  title_vals <- reactive({c(input$stat, state.name[state.abb %in% input$state], input$measure)})
  y_axis_opt <- reactive({input$stat})

  output$plot <- renderPlot(

      height =  600, unit = 'px',

      if (y_axis_opt() == 'Share'){

        ggplot(plot_df(), aes(x = date, y = stic, group = gennme, color = gennme)) +
          scale_color_brewer(palette = 'Paired') +
          scale_x_yearqtr(format = "%YQ%q", n  = 5) +
          geom_line(size = 1) +
          theme(panel.background = element_blank()) +
          theme(legend.position = "bottom") +
          theme(legend.key = element_blank()) +
          theme(legend.title = element_blank()) +
          guides(color = guide_legend(ncol = 2)) +
          theme(legend.text = element_text(size=12)) +
          theme(plot.title = element_text(size = 14)) +
          theme(axis.title = element_text(size = 12)) +
          theme(axis.text = element_text(size = 12)) +
          scale_y_continuous(labels = scales::percent) +
          labs(y = 'Market Share', x  = '',
               title = paste0(title_vals()[1], ' of Different Opioids in ', title_vals()[2], ' Medicaid by ', title_vals()[3])) +
          theme(axis.line = element_line())

        } else{

          ggplot(plot_df(), aes(x = date, y = stic, group = gennme, color = gennme)) +
            scale_color_brewer(palette = 'Paired') +
            scale_x_yearqtr(format = "%YQ%q", n  = 10) +
            geom_line(size = 1) +
            theme(panel.background = element_blank()) +
            theme(legend.position = "bottom") +
            theme(legend.key = element_blank()) +
            theme(legend.title = element_blank()) +
            guides(color = guide_legend(ncol = 2)) +
            theme(legend.text = element_text(size=12)) +
            theme(plot.title = element_text(size = 14)) +
            theme(axis.title = element_text(size = 12)) +
            theme(axis.text = element_text(size = 12)) +
            scale_y_continuous(labels = scales::comma) +
            theme(axis.line = element_line()) +
            labs(y = 'Market Share', x  = '',
                 title = paste0(title_vals()[1], ' of Different Opioids in ', title_vals()[2], ' Medicaid by ', title_vals()[3]))

        }

  )

}

shinyApp(ui,server)
