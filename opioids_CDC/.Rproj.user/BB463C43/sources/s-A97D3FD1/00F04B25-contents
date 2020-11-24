rm(list = ls())

library(rvest)
library(ggplot2)
library(broom)
library(data.table)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)

base <- getwd()

input <- 'data'

# Original data

web <- read_html('https://www.drugabuse.gov/drug-topics/opioids/opioid-summaries-by-state')

table <- web %>%
  html_nodes("#block-nida-vic-adaptive-system-main > article > div > div:nth-child(1) > table") %>%
  html_table(fill=TRUE)

opi_df <- table[[1]]

colnames(opi_df)[2] <- 'deaths_p100k'
colnames(opi_df)[3] <- 'rx_p100'

colnames(opi_df) <- tolower(colnames(opi_df))

opi_df <- opi_df  %>% filter(!deaths_p100k %like% 'Not' |
                               !rx_p100 %like% 'Not') %>%
                      filter(!is.na(deaths_p100k)) %>%
                      filter(!deaths_p100k %like% '\\*')

opi_df <- opi_df %>% mutate(deaths_p100k = as.numeric(deaths_p100k)) %>%
                      mutate(rx_p100 = as.numeric(rx_p100)) %>%
                      mutate(ratio = deaths_p100k / rx_p100)

for (i in 2006:2018){

  state_rx <- read_html(paste0('https://www.cdc.gov/drugoverdose/maps/rxstate', i, '.html'))

  loc <- "#acc-panel-1 > div > table"

  # if (i == 2018){
  #
  #   loc <- "#acc-panel-1 > div > div > table"
  #
  # }

  table_rx <- state_rx %>%
    html_nodes(loc) %>%
    html_table(fill=TRUE)

  temp_df <- table_rx[[1]]

  temp_df <- temp_df[temp_df[,3] != '–',]

  temp_df$year <- i

  colnames(temp_df) <- tolower(colnames(temp_df))

  assign(paste0('state_', i), temp_df)

}

# 2017 and 2018 read in oddly

for (i in 2017:2018){

  temp_df <- get(paste0('state_', i))

  colnames(temp_df) <- colnames(state_2016)

  colnames(temp_df)[3] <- paste0(i, ' prescribing rate')

  temp_df <- temp_df[3:nrow(temp_df), ]

  assign(paste0('state_', i), temp_df)

}

all_data <- state_2006

colnames(all_data)[colnames(all_data) %like% 'rate'] <- 'rx_p100'

for (i in 2007:2018){

  temp_df <- get(paste0('state_', i))

  colnames(temp_df)[colnames(temp_df) %like% 'rate'] <- 'rx_p100'

  all_data <- rbind(all_data, temp_df)

}

deaths <- fread(paste0(input, '/deaths_98_18.txt'))

colnames(deaths) <- tolower(colnames(deaths))

deaths <- deaths[, c('state', 'year', 'age adjusted rate')]

colnames(deaths)[3] <- 'deaths_p100k'

opi_df <- merge(all_data, deaths, by = c('state', 'year'))

opi_df <- opi_df %>% filter(!state %like% 'United')

opi_df <- opi_df %>% mutate(rx_p100 = as.numeric(rx_p100)) %>%
                      mutate(deaths_p100k = as.numeric(deaths_p100k)) %>%
                      filter(!is.na(rx_p100) & !is.na(deaths_p100k))
# Ratio

opi_df <- opi_df %>% mutate(ratio =  deaths_p100k / rx_p100)

colnames(opi_df) <- gsub(' ', '_', colnames(opi_df))

save(opi_df, file = 'output/opi_deaths.Rda')

# opi_df  <- opi_df %>% filter(state_abbr == 'AL')
#
# opi_df$group <- 1
#
# coef <- mean(opi_df$rx_p100) / mean(opi_df$deaths_p100k)
#
# min_year <- min(opi_df$year)
# max_year <- max(opi_df$year)
#
# rx_plot <- ggplot(opi_df) +
#             geom_line(aes(x = year, y = rx_p100, lty = 'Prescriptions per 100'), color  = 'blue') +
#             scale_colour_manual(values=c("blue")) +
#             scale_y_continuous(name = 'Prescriptions per 100') +
#             scale_x_continuous(name = 'Year',
#                                breaks = seq(min_year, max_year, 1)) +
#             scale_color_discrete(labels = c("Prescriptions per 100")) +
#             theme(panel.background = element_blank()) +
#             theme(axis.line = element_line()) +
#             theme(legend.key = element_blank()) +
#             theme(legend.title = element_blank()) +
#             theme(legend.position = "top") +
#             labs(title = paste0('Prescriptions in ', 'California')) +
#             theme(plot.title = element_text(size=14, hjust = 0.5))
#
# death_plot <- ggplot(opi_df) +
#             geom_line(aes(x = year, y = deaths_p100k, lty = 'Prescriptions per 100'), color = 'red') +
#             scale_y_continuous(name = 'Deaths per 100,000') +
#             scale_x_continuous(name = 'Year',
#                                breaks = seq(min_year, max_year, 1)) +
#             scale_color_discrete(labels = c("Deaths per 100,000")) +
#             theme(panel.background = element_blank()) +
#             theme(axis.line = element_line()) +
#             theme(legend.key = element_blank()) +
#             theme(legend.title = element_blank()) +
#             theme(legend.position = "top") +
#             labs(title = paste0('Deaths in ', 'California')) +
#             theme(plot.title = element_text(size=14, hjust = 0.5))

# grid.arrange(rx_plot, death_plot, ncol=2)

map_plot <- opi_df %>% filter(year >= 2006 & year <= 2018) %>%
                        group_by(state) %>%
                        summarise(m_rat = mean(ratio, na.rm = TRUE)) %>%
                        mutate(state = tolower(state))

map <- map_data("state")

map_plot <- merge(map_plot, map, by.x = 'state', by.y = 'region')

ggplot() + geom_polygon(data = map_plot,
                        aes(x=long, y=lat, group=group, fill = m_rat),
                        color="white", size = 0.2)


# ggplot(opi_df) + geom_line(aes(x = year, y = rx_p100, color = 'red')) +
#                  geom_line(aes(x = year, y = deaths_p100k * coef, color = 'blue')) +
#                   scale_y_continuous(name = 'Prescriptions per 100',
#                                      sec.axis = sec_axis( ~./coef, name = "Deaths per 100,000")) +
#                   scale_x_continuous(name = 'Year',
#                                      breaks = seq(min_year, max_year, 1)) +
#                   scale_color_discrete(labels = c("Deaths per 100,000", "Prescriptions per 100")) +
#                   theme(panel.background = element_blank()) +
#                   theme(axis.line = element_line()) +
#                   theme(legend.key = element_blank()) +
#                   theme(legend.title = element_blank()) +
#                   theme(legend.position = "top") +
#                   labs(title = paste0('Prescriptions and Deaths in ', 'California')) +
#                   theme(plot.title = element_text(size=14, hjust = 0.5))

# Exported results
#
# opi_df_06 <- opi_df_rep[opi_df_rep$year == 2006, c('state', 'rx_p100', 'deaths_p100k')]
#
# opi_df_18 <- opi_df[, c('state', 'rx_p100', 'deaths_p100k')]
#
# opi_df_all <- opi_df_rep[, c('state', 'year', 'rx_p100', 'deaths_p100k')]
#
# reg_06 <- lm(deaths_p100k ~ rx_p100, data = opi_df_06)
#
# reg_18 <- lm(deaths_p100k ~ rx_p100, data = opi_df_18)
#
# reg_all <- lm(deaths_p100k ~ rx_p100, data = opi_df_all)
#
# reg_06 <- tidy(reg_06)
#
# reg_18 <- tidy(reg_18)
#
# reg_all <- tidy(reg_all)
