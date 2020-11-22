rm(list = ls())

library(RPostgreSQL)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(datasets)
library(plotly)

con <- dbConnect(PostgreSQL(), dbname = "opioids", user = "postgres")

measure <- 'nrx'

drug <- 'Hydrocodone-Acetaminophen'

s_year <- 2018

e_year <- 2018

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


# map_func <- function(s_year, e_year, drug, measure){

  temp_df <- dbGetQuery(con, paste0("SELECT gennme, state_code, ",
                                 measure,
                                 " FROM sdud_time",
                                 " WHERE year >= ", s_year,
                                 " AND year <= ", e_year, ";"))

  # temp_df <- temp_df %>% rename(meas = measure) %>%
  #                         group_by(state_code, gennme) %>%
  #                         summarise(meas = sum(!!sym('nrx')))
  #                         # group_by(state_code) %>%
  #                         # mutate(rank = dense_rank(desc(meas))) %>%
  #                         # filter(gennme %in% drug)

  temp_df <- temp_df %>% rename(meas = 'nrx') %>%
                          group_by(state_code, gennme) %>%
                          summarise(meas = sum(meas))

  fig <- plot_geo(temp_df, locationmode = 'USA-states')

  fig <- fig %>% add_trace(z = ~meas,
                           locations = ~state_code,
                           color = ~meas,
                           colors = 'Purples')

  fig <- fig %>% colorbar(title = label_list[[measure]], x = -0.2, y = 0.8)

  fig <- fig %>% layout(title = paste0(label_list[[measure]], ' of ', drug, ' by States Medicaid'),
                        geo = map_details)


# }

all_cons <- dbListConnections(PostgreSQL())

for (conx in  all_cons){
  tryCatch({dbDisconnect(conx)})
}

dbDisconnect(con)

fig
