library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)

load('output/base_data.Rda')

state_xwalk <- data.frame(cbind(state.abb, state.name))

colnames(state_xwalk) <- c('state_code', 'region')

state_xwalk$region <-tolower(state_xwalk$region)

base_df <- merge(base_df, state_xwalk, by =  'state_code')

rank_df <- base_df %>% filter(!gennme %in% 'other') %>%
                    group_by(region, gennme) %>%
                    summarise(nrx = sum(nrx)) %>%
                    mutate(rank = dense_rank(desc(nrx)))

rank_df <- rank_df %>% select(region, rank, gennme) %>%
                       filter(rank == 1)

rank_df$gennme[!rank_df$gennme %like% 'Hydrocodone-Acetaminophen' & !rank_df$gennme %like% 'Oxycodone-Acetaminophen'] <- 'Other'

rank_df$gennme <- as.factor(rank_df$gennme)

rank_df$gennme <- factor(rank_df$gennme, levels = c("Hydrocodone-Acetaminophen (Vicodin)",
                                                    "Oxycodone-Acetaminophen (Oxycontin)",
                                                    "Other"))

map <- map_data("state")

theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_blank(),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         legend.position="bottom",
                         plot.title = element_text(size=14, hjust = 0.5)))

rank_map_plot <-  ggplot(data = rank_df) +
  geom_map(data = rank_df, map = map, aes(map_id = region, fill = gennme), alpha = 0.5) +
  coord_equal() +
  theme_opts +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c('#3386FF', '#DC4A4A', '#D6D6D6')) +
  geom_polygon(data = map, aes(x=long, y = lat, group = group), fill = NA, colour="black", size=0.25) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(title = 'Top Ranked Opioid in Each State')

