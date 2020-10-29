
rm(list=ls())
library(RODBC)
library(tidyverse)
library(readr)

save.file <- T

SWFSC <- T
# load a couple databases through ODBC
# this works only at SWFSC - or VPN also?

sp2filter <- "Dermochelys"

if (SWFSC){
  Turtle <- odbcConnect(dsn = 'Turtle', uid = '', pwd = '')
  #Turtle.tbls <- sqlTables(Turtle)
  Turtle.Stranding <- sqlQuery(Turtle,
                               'select * from tbl_Stranding')
  
  Turtle.Stranding.Details <- sqlQuery(Turtle,
                                       'select * from tbl_Stranding_Detail')
  Sp.table <- sqlQuery(Turtle,
                       'select * from tblSpecies')
  odbcClose(Turtle)

  SWFSCCommon <- odbcConnect(dsn = 'Common', uid = '', pwd = '')
  Sp.table <- sqlQuery(SWFSCCommon,
                       'select * from tblSpecies')
  State.table <- sqlQuery(SWFSCCommon,
                          'select * from tblState')
  City.table <-  sqlQuery(SWFSCCommon,
                          'select * from tblCity')
  odbcClose(SWFSCCommon)

  # remove some fields. ts is a checkbox results
  Turtle.Stranding %>% select(-c(ts, starts_with("Observed_By"),
                                 starts_with("Edit_"),
                                 Field_ID,
                                 Other_ID)) -> Turtle.Stranding
  
  # need to remove some fields - they are paragraphs and not useful
  Turtle.Stranding.Details %>% select(-c(contains("_By"),
                                         ends_with("Description"),
                                         starts_with("Edit_"),
                                         contains("Necrops"),
                                         ts,
                                         Location,
                                         Locality_Details,
                                         Other_Finding,
                                         Other_Finding_Description,
                                         Necropsy_Remarks,
                                         starts_with("Evidence_"),
                                         starts_with("Other_Findings"),
                                         starts_with("Euthanized"),
                                         contains("Samples"),
                                         contains("Determined_By"),
                                         contains("Estimated"),
                                         contains("Relocated"),
                                         Carcass_Disposition,
                                         contains("How"),
                                         contains("Transfer"),
                                         contains("Where"),
                                         contains("Why"),
                                         Seaworld_Paperwork_Notes)) -> Turtle.Stranding.Details
  
  Sp.table %>% select(ID, Genus, Species) -> Sp.table
  
  write.csv(Turtle.Stranding,
            file = paste0("data/Turtle_Stranding_", Sys.Date(), ".csv"),
            quote = F, row.names = F)

  write.csv(Turtle.Stranding.Details,
            file = paste0("data/Turtle_Stranding_Details_", 
                          Sys.Date(), ".csv"),
            quote = F, row.names = F)

  write.csv(Sp.table,
            file = paste0("data/Species_Table_.csv"),
            quote = F, row.names = F)

  write.csv(State.table,
            file = 'data/State_Table.csv',
            quote = F, row.names = F)

  write.csv(City.table,
            file = 'data/City_Table.csv',
            quote = F, row.names = F)

} else {
  # if not at SWFSC, use the local files.
  Turtle.Stranding <- read.table(file = 'data/Turtle_Stranding.csv',
                                 header = T,
                                 sep = ",")

  Turtle.Stranding.Details <- read.table(file = 'data/Turtle_Stranding_Details.csv',
                                         header = T,
                                         sep = ",")

  Sp.table <- read.table(file = 'data/Species_Table.csv',
                         header = T, sep = ",")

  State.table <- read.table(file = 'data/State_Table.csv',
                         header = T, sep = ",")

  City.table <- read.table(file = 'data/City_Table.csv',
                            header = T, sep = ",")
}


# merge together by ID and Stranding_ID, which are shared
# then merge with Sp ID table (Sp.table)
all.data <- inner_join(Turtle.Stranding,
                       Turtle.Stranding.Details,
                       by = c("ID" = "Stranding_ID")) %>%
  inner_join(Sp.table, by = c("Species_ID" = "ID")) %>%
  inner_join(State.table, by = c("State_ID" = "ID")) %>%
  rename(State = Name)

  #%>%
  # inner_join(., City.table, by = c("City_ID" = "ID")) %>%
  # rename(., City = Name)

all.data %>% filter(Genus == sp2filter) %>%
  select(Genus, 
         NMFS_ID, 
         Year_Initially_Observed,
         Month_Initially_Observed,
         Day_Initially_Observed,
         Stranded,
         Latitude, Longitude,
         State,
         Curved_Carapace_Length, 
         Fishery_Interaction,
         Human_Interaction,
         Alive_Released) -> sp.data


fishery.interactions <- filter(sp.data, Fishery_Interaction == 1)
alive.released <- filter(sp.data, Alive_Released == 1)
CA.stranding <- filter(sp.data, State == "CALIFORNIA")


if (save.file)
  write.csv(sp.data, 
            file = paste0("data/", sp2filter, "_Strandings_", Sys.Date(), ".csv"),
            row.names = FALSE, quote = FALSE)

