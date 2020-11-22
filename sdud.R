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

# api_end_points <- list(c('https://data.medicaid.gov/resource/va5y-jhsv.json', 2020),
#                     c('https://data.medicaid.gov/resource/qnsz-yp89.json', 2019),
#                     c('https://data.medicaid.gov/resource/e5ds-i36p.json', 2018),
#                     c('https://data.medicaid.gov/resource/3v5r-x5x9.json', 2017),
#                     c('https://data.medicaid.gov/resource/3v6v-qk5s.json', 2016),
#                     c('https://data.medicaid.gov/resource/ju2h-vcgs.json', 2015),
#                     c('https://data.medicaid.gov/resource/955u-9h9g.json', 2014),
#                     c('https://data.medicaid.gov/resource/rkct-3tm8.json', 2013),
#                     c('https://data.medicaid.gov/resource/yi2j-kk5z.json', 2012),
#                     c('https://data.medicaid.gov/resource/ra84-ffhc.json', 2011),
#                     c('https://data.medicaid.gov/resource/mmgn-kvy5.json', 2010),
#                     c('https://data.medicaid.gov/resource/fhmx-iqs3.json', 2009),
#                     c('https://data.medicaid.gov/resource/ny8j-2ymd.json', 2008),
#                     c('https://data.medicaid.gov/resource/q947-frj2.json', 2007),
#                     c('https://data.medicaid.gov/resource/e7is-4a3j.json', 2006),
#                     c('https://data.medicaid.gov/resource/ezjn-vqh8.json', 2005),
#                     c('https://data.medicaid.gov/resource/rn2y-fgjb.json', 2004),
#                     c('https://data.medicaid.gov/resource/66gr-qxnr.json', 2003),
#                     c('https://data.medicaid.gov/resource/5jcx-2xey.json', 2002),
#                     c('https://data.medicaid.gov/resource/t5ct-xf3k.json', 2001),
#                     c('https://data.medicaid.gov/resource/78qv-c4cn.json', 2000),
#                     c('https://data.medicaid.gov/resource/vhg8-v7wa.json', 1999),
#                     c('https://data.medicaid.gov/resource/ykva-ug36.json', 1998),
#                     c('https://data.medicaid.gov/resource/c7wf-ku3w.json', 1997),
#                     c('https://data.medicaid.gov/resource/jqjw-uby8.json', 1996),
#                     c('https://data.medicaid.gov/resource/v83u-wwk3.json', 1995),
#                     c('https://data.medicaid.gov/resource/8uti-96dw.json', 1994),
#                     c('https://data.medicaid.gov/resource/iu8s-z84j.json', 1993),
#                     c('https://data.medicaid.gov/resource/agzs-hwsn.json', 1992),
#                     c('https://data.medicaid.gov/resource/q7kf-kjqz.json', 1991))

api_end_points <- list(c('https://data.medicaid.gov/resource/agzs-hwsn.json', 1992),
                       c('https://data.medicaid.gov/resource/q7kf-kjqz.json', 1991))

for (apiep in api_end_points){

  for (state in state.abb){

    temp_sdud <- read.socrata(paste0(apiep[1],  "?$select=", sdud_vars,
                                     "&state_code=", state),
                              app_token = token,
                              email = "davidby332@gmail.com",
                              password = "Buf01r08!")

    if (nrow(temp_sdud) > 0){

      temp_sdud <- temp_sdud %>% filter(ndc %in% ndc_list$ndc)

      temp_sdud$year <- as.numeric(apiep[2])

      sdud_opi <- rbind(sdud_opi, temp_sdud, fill = TRUE)

    }

  }

  print(apiep[2])

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

