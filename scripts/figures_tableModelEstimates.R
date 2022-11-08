#Table 1
#Common species estimates
# Thu Jul  7 22:17:22 2022 ------------------------------

#LIBRARIES
#====
library(readxl)
library(knitr)
library(kableExtra)
library(tikzDevice)
#====

#SET UP
#====
df <- read_xlsx("data/table_modelEstimates.xlsx")#load table
names(df) <- c("Type", "Test statistic1", "p-value1", "Test statistic2", "p-value2")
df$`p-value1` <- p.adjust(df$`p-value1`, method = "hochberg")
df$`p-value2` <- p.adjust(df$`p-value2`, method = "hochberg")
names(df) <- c("Type", "Test statistic", "p-value", "Test statistic", "p-value")

df2 <- read_xlsx("data/table_modelContrasts.xlsx")#load table
names(df2) <- c("Type", "Estimate", "p-value", "Estimate", "p-value", "Estimate", "p-value", "Summary", "Hypothesis")
#====

#Model Estimates
#====
df %>% #knit table
  kbl() %>% 
  kable_classic() %>% 
  add_header_above(c(" " = 1, "Location" = 2, "Location:Depth" = 2), bold = TRUE) %>% 
  pack_rows(index = c("Coastal Meroplankton" = 7, "Pelagic Holoplankton" = 7, "Non-Swimming" = 3)) %>% 
  row_spec(0, bold = TRUE)
#====

#Model Contrasts
#====
df2 %>% #knit table
  kbl() %>% 
  kable_classic() %>% 
  add_header_above(c(" " = 1, "Onshore-Front" = 2, "Onshore-Offshore" = 2, "Front-Offshore" = 2, " " = 2), bold = TRUE) %>% 
  pack_rows(index = c("Coastal Meroplankton" = 2, "Pelagic Holoplankton" = 3, "Non-Swimming" = 1)) %>% 
  row_spec(0, bold = TRUE)
#====