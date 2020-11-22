rm(list = ls())

library(RPostgreSQL)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(datasets)
library(plotly)
library(zoo)

con <- dbConnect(PostgreSQL(), dbname = "opioids", user = "postgres")

top_10_alltime <- dbGetQuery(con, "SELECT gennme, nrx, state_code FROM sdud_time;")

top_10_alltime <- top_10_alltime %>%
  group_by(state_code, gennme) %>%
  summarise(nrx = sum(nrx)) %>%
  mutate(total = sum(nrx))

top_10_alltime$mkt_share <- top_10_alltime$nrx  / top_10_alltime$total

top_10_alltime <- top_10_alltime %>% filter(mkt_share >= 0.1)

top_10_alltime <- unique(top_10_alltime$gennme)

base_df <- dbGetQuery(con, "SELECT gennme, state_code, year, quarter, nrx, nur, nur_adj
                                  FROM sdud_time;")

base_df$gennme[!base_df$gennme %in% top_10_alltime] <- 'Other'

base_df <- base_df %>% group_by(year, quarter, state_code, gennme) %>%
  summarise(nrx = sum(nrx),
            nur =  sum(nur),
            nur_adj =  sum(nur_adj))

base_df$gennme[base_df$gennme %in% 'Hydrocodone-Acetaminophen'] <- 'Hydrocodone-Acetaminophen (Vicodin)'
base_df$gennme[base_df$gennme %in% 'Oxycodone-Acetaminophen'] <- 'Oxycodone-Acetaminophen (Oxycontin)'
base_df$gennme[base_df$gennme %in% 'Codeine-Acetaminophen'] <- 'Codeine-Acetaminophen (Tylenol-Codeine)'
base_df$gennme[base_df$gennme %in% 'Oxycodone'] <- 'Oxycodone (Percocet)'
base_df$gennme[base_df$gennme %in% 'Propoxyphene-Acetaminophen'] <- 'Propoxyphene-Acetaminophen (Darvocet)'

save(base_df, file = 'output/base_data.Rda')
