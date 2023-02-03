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
df <- read_csv("output/mainEfx.csv")#load table
df$location_p <- round(p.adjust(df$location_p, method = "hochberg"), 4)#adjust p value for multiple comparisons
df$location.depth_p <- round(p.adjust(df$location.depth_p, method = "hochberg"), 4)#adjust p value for multiple comparisons
names(df) <- c("Type", "Test statistic", "p-value", "Test statistic", "p-value")

df2 <- read_csv("output/pairwise.csv")#load table
names(df2) <- c("Type", "Estimate", "p-value", "Estimate", "p-value", "Estimate", "p-value")
df2 <- cbind(df2, data.frame(Summary = c("on<f=off", "on=f>off", "on=f<off", "on=f<off"),#add in summary columns based on estimates and p values
                             Hypothesis = c("Barrier onshore", "Barrier offshore", "Barrier onshore", "Barrier onshore")))
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
  pack_rows(index = c("Coastal Meroplankton" = 2, "Pelagic Holoplankton" = 2)) %>% 
  row_spec(0, bold = TRUE)
#====