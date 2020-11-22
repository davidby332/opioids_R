rm(list = ls())

library(RPostgreSQL)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(datasets)
library(ggplot2)
library(zoo)
library(wesanderson)

con <- dbConnect(PostgreSQL(), dbname = "opioids", user = "postgres")

measure <- 'nrx'

drug <- 'Hydrocodone-Acetaminophen'

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

e_year <- 2019
s_year <- 1991

measure  <- 'nrx'

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

temp_df <- base_df[base_df$year >= s_year & base_df$year <=e_year,
                   c('year', 'quarter', 'gennme', 'state_code', 'nrx')]

temp_df  <- temp_df %>% filter(state_code %in% 'CA') %>%
                        group_by(year, quarter) %>%
                        mutate(total = sum(!!sym('nrx')))

temp_df$mkt_share <- temp_df$nrx / temp_df$total

temp_df$date <- as.yearqtr(paste0(temp_df$year, ' Q', temp_df$quarter))

temp_df <- temp_df %>% arrange(year, quarter) %>%
                       ungroup(year, quarter) %>%
                       select(date, gennme, mkt_share)

temp_df %>%
  ggplot(aes(x = date, y = mkt_share, group = gennme, color = gennme)) +
  geom_line() +
  guides(color=guide_legend(ncol=2)) +
  scale_color_brewer(palette = 'Paired') +
  scale_x_yearqtr(format = "%YQ%q", n  = 5) +
  geom_line() + guides(fill=guide_legend(ncol=3)) +
  theme(panel.background = element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.key=element_blank()) +
  theme(legend.title = element_blank()) +
  labs(y = 'Market Share', x  = '', title = 'Market Share of Different Opioids in California Medicaid') +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.line = element_line())

all_cons <- dbListConnections(PostgreSQL())

for (conx in  all_cons){
  tryCatch({dbDisconnect(conx)})
}

dbDisconnect(con)



