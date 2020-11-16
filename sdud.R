rm(list = ls())

library(RSocrata)
library(RPostgreSQL)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(datasets)

con <- dbConnect(PostgreSQL(), dbname = "opioids", user = "postgres")

token <- "5lSu1fL0xcZHjXcMJyWifYP5n"

ndc_list <- dbGetQuery(con, "SELECT DISTINCT ndc
                              FROM data_2018;")

sdud_opi <- data.table()

sdud_vars <- 'state_code,quarter,units_reimbursed,number_of_prescriptions,ndc'

for (state in state.abb){

    temp_sdud <- read.socrata(paste0("https://data.medicaid.gov/resource/e5ds-i36p.json?$select=", sdud_vars,
                                     "&state_code=", state),
                         app_token = token,
                         email = "davidby332@gmail.com",
                         password = "Buf01r08!")

    temp_sdud <- temp_sdud %>% filter(ndc %in% ndc_list$ndc)

    sdud_opi <- rbind(sdud_opi, temp_sdud, fill = TRUE)

    print(state)

}

ndc_names <- dbGetQuery(con, "SELECT DISTINCT ndc, gennme, mme_conversion_factor
                              FROM data_2018;")

for (filler in c('sulfate', 'hydrochloride', 'bitartrate', 'phosphate', 'pentazocine', 'tartrate', 'acetate', 'napsylate')){

  ndc_names$gennme <- gsub(filler, '', tolower(ndc_names$gennme))

}

ndc_names$gennme <- trimws(ndc_names$gennme)

ace_cond <- ndc_names$gennme %like% 'acetaminophen'

ndc_names$gennme[ace_cond] <- paste0(substr(ndc_names$gennme[ace_cond], 15, 1000L),
                                    '-',
                                    substr(ndc_names$gennme[ace_cond], 1, 13))

ndc_names$gennme <- gsub('\\/', '\\-', ndc_names$gennme)

ndc_names$gennme <- str_to_title(ndc_names$gennme)

sdud_opi <- merge(sdud_opi, ndc_names, by = 'ndc')

sdud_opi$number_of_prescriptions <- as.numeric(sdud_opi$number_of_prescriptions)

sdud_opi$units_reimbursed <- as.numeric(sdud_opi$units_reimbursed)

sdud_opi$mme_conversion_factor <- as.numeric(sdud_opi$mme_conversion_factor)

sdud_opi$nur_adj <- sdud_opi$mme_conversion_factor * sdud_opi$units_reimbursed

sdud_opi$year <- 2018

sdud_opi_time <- sdud_opi %>% group_by(year, quarter, gennme, state_code) %>%
                              summarise(nrx = sum(number_of_prescriptions, na.rm = TRUE),
                                        nur = sum(units_reimbursed, na.rm = TRUE),
                                        nur_adj = sum(nur_adj, na.rm = TRUE))

dbGetQuery(con, "DROP TABLE IF EXISTS sdud_time;")

dbWriteTable(con, name = "sdud_time", value = sdud_opi_time, row.names = FALSE)

all_cons <- dbListConnections(PostgreSQL())

for (conx in  all_cons){
  tryCatch({dbDisconnect(conx)})
}

dbDisconnect(con)

