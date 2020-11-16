rm(list = ls())

library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "opioids", user = "postgres")

dbGetQuery(con, "DROP TABLE IF EXISTS data_2018;")

dbGetQuery(con, "CREATE TABLE data_2018
                 (NDC character varying(50),
                 NDC_numeric character varying(50),
                 PRODNME character varying(50),
                 GENNME character varying(50),
                 Master_Form character varying(50),
                 Class character varying(50),
                 Drug character varying(50),
                 LongShortActing character varying(50),
                 DEAClassCode character varying(50),
                 Streng_Per_Unit float,
                 UOM_character character varying(50),
                 MME_Conversion_Factor character varying(50));
                 ")

dbGetQuery(con, "COPY data_2018(NDC,
                 NDC_numeric,
                 PRODNME,
                 GENNME,
                 Master_Form,
                 Class,
                 Drug,
                 LongShortActing,
                 DEAClassCode,
                 Streng_Per_Unit,
                 UOM_character,
                 MME_Conversion_Factor)
                FROM '/Users/david/Library/Mobile\ Documents/com~apple~CloudDocs/Data\ science/opioids_R/opioids_R/data/CDC_Oral_Morphine_Milligram_Equivalents_Sept_2018 - Opioids.csv'
                WITH CSV HEADER;")

dbDisconnect(con)

all_cons <- dbListConnections(drv)

for (conx in  all_cons){
  tryCatch({dbDisconnect(conx)})
}



