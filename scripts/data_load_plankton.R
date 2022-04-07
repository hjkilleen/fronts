#Script to load plankton data

# Tue Dec 14 09:17:03 2021 ------------------------------

#LIBRARIES & SOURCES
#====
library(tidyverse)
library(readxl)
#====

#LOAD DATA
#====
sheet <- excel_sheets("data/biology/counts/07182019.xlsx")
cruise1 <- lapply(setNames(sheet, sheet), 
                  function(x) read_excel("data/biology/counts/07182019.xlsx", sheet=x))
cruise1 <- bind_rows(cruise1, .id="Sheet")
save(cruise1, file = "data/biology/counts/cruise1.rda")

sheet <- excel_sheets("data/biology/counts/08152019.xlsx")
cruise3 <- lapply(setNames(sheet, sheet), 
                    function(x) read_excel("data/biology/counts/08152019.xlsx", sheet=x))
cruise3 <- bind_rows(cruise3, .id="Sheet")
save(cruise3, file = "data/biology/counts/cruise3.rda")

sheet <- excel_sheets("data/biology/counts/09202019.xlsx")
cruise4 <- lapply(setNames(sheet, sheet), 
                  function(x) read_excel("data/biology/counts/09202019.xlsx", sheet=x))
cruise4 <- bind_rows(cruise4, .id="Sheet")
save(cruise4, file = "data/biology/counts/cruise4.rda")

sheet <- excel_sheets("data/biology/counts/06232020.xlsx")
cruise5 <- lapply(setNames(sheet, sheet), 
                  function(x) read_excel("data/biology/counts/06232020.xlsx", sheet=x))
cruise5 <- bind_rows(cruise5, .id="Sheet")
save(cruise5, file = "data/biology/counts/cruise5.rda")

sheet <- excel_sheets("data/biology/counts/06302020.xlsx")
cruise6 <- lapply(setNames(sheet, sheet), 
                  function(x) read_excel("data/biology/counts/06302020.xlsx", sheet=x))
cruise6 <- bind_rows(cruise6, .id="Sheet")
save(cruise6, file = "data/biology/counts/cruise6.rda")

sheet <- excel_sheets("data/biology/counts/09302020.xlsx")
cruise9 <- lapply(setNames(sheet, sheet), 
                  function(x) read_excel("data/biology/counts/09302020.xlsx", sheet=x))
cruise9 <- bind_rows(cruise9, .id="Sheet")
save(cruise9, file = "data/biology/counts/cruise9.rda")

sheet <- excel_sheets("data/biology/counts/10062020.xlsx")
cruise10 <- lapply(setNames(sheet, sheet), 
                  function(x) read_excel("data/biology/counts/10062020.xlsx", sheet=x))
cruise10 <- bind_rows(cruise10, .id="Sheet")
save(cruise10, file = "data/biology/counts/cruise10.rda")

#Bind all cruises
plankton <- bind_rows(cruise1, cruise3, cruise4, cruise5, cruise6, cruise9, cruise10)
save(plankton, file = "data/biology/counts/plankton.rda")
#====


