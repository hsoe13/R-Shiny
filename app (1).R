#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(shinydashboard)
library(C50)
library(tree)
library(randomForest)
library(rpart)
library(gbm)
library(tidyverse)
library(fastDummies)
library(reshape2)
library(usmap)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

meteorite_data <- read.csv("Meteorite_Landings.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")
classification_map <- list(
  "Acapulcoite" = "Achondrite Primitive",
  "Acapulcoite/lodranite" = "Achondrite Primitive",
  "Acapulcoite/Lodranite" = "Achondrite Primitive",
  "Achondrite-prim" = "Achondrite Primitive",
  "Achondrite-ung" = "Achondrite Primitive",
  "Angrite" = "Achondrite Asteroidal",
  "Aubrite" = "Achondrite Asteroidal",
  "Aubrite-an" = "Achondrite Asteroidal",
  "Brachinite" = "Achondrite Primitive",
  "C" = "Chondrite Carbonaceous",
  "C1/2-ung" = "Chondrite Carbonaceous",
  "C2" = "Chondrite Carbonaceous",
  "C2-ung" = "Chondrite Carbonaceous",
  "C3-ung" = "Chondrite Carbonaceous",
  "C3.0-ung" = "Chondrite Carbonaceous",
  "C4" = "Chondrite Carbonaceous",
  "C4-ung" = "Chondrite Carbonaceous",
  "C4/5" = "Chondrite Carbonaceous",
  "C5/6-ung" = "Chondrite Carbonaceous",
  "C6" = "Chondrite Carbonaceous",
  "CB" = "Chondrite Carbonaceous",
  "CBa" = "Chondrite Carbonaceous",
  "CBb" = "Chondrite Carbonaceous",
  "CH/CBb" = "Chondrite Carbonaceous",
  "CH3" = "Chondrite Carbonaceous",
  "Chondrite-fusion crust" = "Chondrite Carbonaceous",
  "Chondrite-ung" = "Chondrite Carbonaceous",
  "CI1" = "Chondrite Carbonaceous",
  "CK" = "Chondrite Carbonaceous",
  "CK3" = "Chondrite Carbonaceous",
  "CK3-an" = "Chondrite Carbonaceous",
  "CK4" = "Chondrite Carbonaceous",
  "CK4-an" = "Chondrite Carbonaceous",
  "CK4/5" = "Chondrite Carbonaceous",
  "CK5" = "Chondrite Carbonaceous",
  "CK5/6" = "Chondrite Carbonaceous",
  "CK6" = "Chondrite Carbonaceous",
  "CM" = "Chondrite Carbonaceous",
  "CM-an" = "Chondrite Carbonaceous",
  "CM1" = "Chondrite Carbonaceous",
  "CM1/2" = "Chondrite Carbonaceous",
  "CM2" = "Chondrite Carbonaceous",
  "CO3" = "Chondrite Carbonaceous",
  "CO3.0" = "Chondrite Carbonaceous",
  "CO3.1" = "Chondrite Carbonaceous",
  "CO3.2" = "Chondrite Carbonaceous",
  "CO3.3" = "Chondrite Carbonaceous",
  "CO3.4" = "Chondrite Carbonaceous",
  "CO3.5" = "Chondrite Carbonaceous",
  "CO3.6" = "Chondrite Carbonaceous",
  "CO3.7" = "Chondrite Carbonaceous",
  "CO3.8" = "Chondrite Carbonaceous",
  "CR" = "Chondrite Carbonaceous",
  "CR-an" = "Chondrite Carbonaceous",
  "CR1" = "Chondrite Carbonaceous",
  "CR2" = "Chondrite Carbonaceous",
  "CR2-an" = "Chondrite Carbonaceous",
  "CR7" = "Chondrite Carbonaceous",
  "CV2" = "Chondrite Carbonaceous",
  "CV3" = "Chondrite Carbonaceous",
  "CV3-an" = "Chondrite Carbonaceous",
  "Diogenite" = "Achondrite Asteroidal",
  "Diogenite-an" = "Achondrite Asteroidal",
  "Diogenite-olivine" = "Achondrite Asteroidal",
  "Diogenite-pm" = "Achondrite Asteroidal",
  "E" = "Chondrite Enstatite",
  "E-an" = "Chondrite Enstatite",
  "E3" = "Chondrite Enstatite",
  "E3-an" = "Chondrite Enstatite",
  "E4" = "Chondrite Enstatite",
  "E5" = "Chondrite Enstatite",
  "E5-an" = "Chondrite Enstatite",
  "E6" = "Chondrite Enstatite",
  "EH" = "Chondrite Enstatite",
  "EH-imp melt" = "Chondrite Enstatite",
  "EH3" = "Chondrite Enstatite",
  "EH3/4-an" = "Chondrite Enstatite",
  "EH4" = "Chondrite Enstatite",
  "EH4/5" = "Chondrite Enstatite",
  "EH5" = "Chondrite Enstatite",
  "EH6" = "Chondrite Enstatite",
  "EH6-an" = "Chondrite Enstatite",
  "EH7" = "Chondrite Enstatite",
  "EH7-an" = "Chondrite Enstatite",
  "EL-melt rock" = "Chondrite Enstatite",
  "EL3" = "Chondrite Enstatite",
  "EL4" = "Chondrite Enstatite",
  "EL4/5" = "Chondrite Enstatite",
  "EL5" = "Chondrite Enstatite",
  "EL6" = "Chondrite Enstatite",
  "EL6/7" = "Chondrite Enstatite",
  "EL7" = "Chondrite Enstatite",
  "Enst achon-ung" = "Chondrite Enstatite",
  "Eucrite" = "Achondrite Asteroidal",
  "Eucrite-an" = "Achondrite Asteroidal",
  "Eucrite-br" = "Achondrite Asteroidal",
  "Eucrite-cm" = "Achondrite Asteroidal",
  "Eucrite-Mg rich" = "Achondrite Asteroidal",
  "Eucrite-mmict" = "Achondrite Asteroidal",
  "Eucrite-pmict" = "Achondrite Asteroidal",
  "Eucrite-unbr" = "Achondrite Asteroidal",
  "Fusion crust" = "Chondrite Carbonaceous",
  "H" = "Chondrite Ordinary",
  "H-an" = "Chondrite Ordinary",
  "H-imp melt" = "Chondrite Ordinary",
  "H-melt breccia" = "Chondrite Ordinary",
  "H-melt rock" = "Chondrite Ordinary",
  "H-metal" = "Chondrite Ordinary",
  "H?" = "Chondrite Ordinary",
  "H(?)4" = "Chondrite Ordinary",
  "H(5?)" = "Chondrite Ordinary",
  "H(L)3" = "Chondrite Ordinary",
  "H(L)3-an" = "Chondrite Ordinary",
  "H/L~4" = "Chondrite Ordinary",
  "H/L3" = "Chondrite Ordinary",
  "H/L3.5" = "Chondrite Ordinary",
  "H/L3.6" = "Chondrite Ordinary",
  "H/L3.9" = "Chondrite Ordinary",
  "H/L4" = "Chondrite Ordinary",
  "H/L4-5" = "Chondrite Ordinary",
  "H/L5" = "Chondrite Ordinary",
  "H/L6" = "Chondrite Ordinary",
  "H~4" = "Chondrite Ordinary",
  "H~4/5" = "Chondrite Ordinary",
  "H~5" = "Chondrite Ordinary",
  "H~6" = "Chondrite Ordinary",
  "H3" = "Chondrite Ordinary",
  "H3 " = "Chondrite Ordinary",
  "H3-4" = "Chondrite Ordinary",
  "H3-5" = "Chondrite Ordinary",
  "H3-6" = "Chondrite Ordinary",
  "H3-an" = "Chondrite Ordinary",
  "H3.0" = "Chondrite Ordinary",
  "H3.0-3.4" = "Chondrite Ordinary",
  "H3.05" = "Chondrite Ordinary",
  "H3.1" = "Chondrite Ordinary",
  "H3.10" = "Chondrite Ordinary",
  "H3.2" = "Chondrite Ordinary",
  "H3.2-3.7" = "Chondrite Ordinary",
  "H3.2-6" = "Chondrite Ordinary",
  "H3.2-an" = "Chondrite Ordinary",
  "H3.3" = "Chondrite Ordinary",
  "H3.4" = "Chondrite Ordinary",
  "H3.4-5" = "Chondrite Ordinary",
  "H3.4/3.5" = "Chondrite Ordinary",
  "H3.5" = "Chondrite Ordinary",
  "H3.5-4" = "Chondrite Ordinary",
  "H3.6" = "Chondrite Ordinary",
  "H3.6-6" = "Chondrite Ordinary",
  "H3.7" = "Chondrite Ordinary",
  "H3.7-5" = "Chondrite Ordinary",
  "H3.7-6" = "Chondrite Ordinary",
  "H3.7/3.8" = "Chondrite Ordinary",
  "H3.8" = "Chondrite Ordinary",
  "H3.8-4" = "Chondrite Ordinary",
  "H3.8-5" = "Chondrite Ordinary",
  "H3.8-6" = "Chondrite Ordinary",
  "H3.8-an" = "Chondrite Ordinary",
  "H3.8/3.9" = "Chondrite Ordinary",
  "H3.8/4" = "Chondrite Ordinary",
  "H3.9" = "Chondrite Ordinary",
  "H3.9-5" = "Chondrite Ordinary",
  "H3.9-6" = "Chondrite Ordinary",
  "H3.9/4" = "Chondrite Ordinary",
  "H3/4" = "Chondrite Ordinary",
  "H4" = "Chondrite Ordinary",
  "H4 " = "Chondrite Ordinary",
  "H4-5" = "Chondrite Ordinary",
  "H4-6" = "Chondrite Ordinary",
  "H4-an" = "Chondrite Ordinary",
  "H4-melt breccia" = "Chondrite Ordinary",
  "H4(?)" = "Chondrite Ordinary",
  "H4/5" = "Chondrite Ordinary",
  "H4/6" = "Chondrite Ordinary",
  "H5" = "Chondrite Ordinary",
  "H5 " = "Chondrite Ordinary",
  "H5-6" = "Chondrite Ordinary",
  "H5-7" = "Chondrite Ordinary",
  "H5-an" = "Chondrite Ordinary",
  "H5-melt breccia" = "Chondrite Ordinary",
  "H5/6" = "Chondrite Ordinary",
  "H6" = "Chondrite Ordinary",
  "H6 " = "Chondrite Ordinary",
  "H6-melt breccia" = "Chondrite Ordinary",
  "H7" = "Chondrite Ordinary",
  "Howardite" = "Achondrite Asteroidal",
  "Howardite-an" = "Achondrite Asteroidal",
  "Iron" = "Iron",
  "Iron, IAB comple\"" = "Iron",
  "Iron, IAB-an" = "Iron",
  "Iron, IAB-MG" = "Iron",
  "Iron, IAB-sHH" = "Iron",
  "Iron, IAB-sHL" = "Iron",
  "Iron, IAB-sHL-an" = "Iron",
  "Iron, IAB-sLH" = "Iron",
  "Iron, IAB-sLL" = "Iron",
  "Iron, IAB-sLM" = "Iron",
  "Iron, IAB-ung" = "Iron",
  "Iron, IAB?" = "Iron",
  "Iron, IC" = "Iron",
  "Iron, IC-an" = "Iron",
  "Iron, IIAB" = "Iron",
  "Iron, IIAB-an" = "Iron",
  "Iron, IIC" = "Iron",
  "Iron, IID" = "Iron",
  "Iron, IID-an" = "Iron",
  "Iron, IIE" = "Iron",
  "Iron, IIE-an" = "Iron",
  "Iron, IIF" = "Iron",
  "Iron, IIG" = "Iron",
  "Iron, IIIAB" = "Iron",
  "Iron, IIIAB-an" = "Iron",
  "Iron, IIIAB?" = "Iron",
  "Iron, IIIE" = "Iron",
  "Iron, IIIE-an" = "Iron",
  "Iron, IIIF" = "Iron",
  "Iron, IVA" = "Iron",
  "Iron, IVA-an" = "Iron",
  "Iron, IVB" = "Iron",
  "Iron, ungrouped" = "Iron",
  "K" = "Chondrite Kakangari",
  "K3" = "Chondrite Kakangari",
  "L" = "Chondrite Ordinary",
  "L-imp melt" = "Chondrite Ordinary",
  "L-melt breccia" = "Chondrite Ordinary",
  "L-melt rock" = "Chondrite Ordinary",
  "L-metal" = "Chondrite Ordinary",
  "L(?)3" = "Chondrite Ordinary",
  "L(LL)3" = "Chondrite Ordinary",
  "L(LL)3.05" = "Chondrite Ordinary",
  "L(LL)3.5-3.7" = "Chondrite Ordinary",
  "L(LL)5" = "Chondrite Ordinary",
  "L(LL)6" = "Chondrite Ordinary",
  "L/LL" = "Chondrite Ordinary",
  "L/LL(?)3" = "Chondrite Ordinary",
  "L/LL~4" = "Chondrite Ordinary",
  "L/LL~5" = "Chondrite Ordinary",
  "L/LL~6" = "Chondrite Ordinary",
  "L/LL3" = "Chondrite Ordinary",
  "L/LL3-5" = "Chondrite Ordinary",
  "L/LL3-6" = "Chondrite Ordinary",
  "L/LL3.10" = "Chondrite Ordinary",
  "L/LL3.2" = "Chondrite Ordinary",
  "L/LL3.4" = "Chondrite Ordinary",
  "L/LL3.6/3.7" = "Chondrite Ordinary",
  "L/LL4" = "Chondrite Ordinary",
  "L/LL4-6" = "Chondrite Ordinary",
  "L/LL4/5" = "Chondrite Ordinary",
  "L/LL5" = "Chondrite Ordinary",
  "L/LL5-6" = "Chondrite Ordinary",
  "L/LL5/6" = "Chondrite Ordinary",
  "L/LL6" = "Chondrite Ordinary",
  "L/LL6-an" = "Chondrite Ordinary",
  "L~3" = "Chondrite Ordinary",
  "L~4" = "Chondrite Ordinary",
  "L~4-6" = "Chondrite Ordinary",
  "L~5" = "Chondrite Ordinary",
  "L~6" = "Chondrite Ordinary",
  "L3" = "Chondrite Ordinary",
  "L3-4" = "Chondrite Ordinary",
  "L3-5" = "Chondrite Ordinary",
  "L3-6" = "Chondrite Ordinary",
  "L3-melt breccia" = "Chondrite Ordinary",
  "L3.0" = "Chondrite Ordinary",
  "L3.0-3.7" = "Chondrite Ordinary",
  "L3.0-3.9" = "Chondrite Ordinary",
  "L3.00" = "Chondrite Ordinary",
  "L3.05" = "Chondrite Ordinary",
  "L3.1" = "Chondrite Ordinary",
  "L3.10" = "Chondrite Ordinary",
  "L3.2" = "Chondrite Ordinary",
  "L3.2-3.5" = "Chondrite Ordinary",
  "L3.2-3.6" = "Chondrite Ordinary",
  "L3.3" = "Chondrite Ordinary",
  "L3.3-3.5" = "Chondrite Ordinary",
  "L3.3-3.6" = "Chondrite Ordinary",
  "L3.3-3.7" = "Chondrite Ordinary",
  "L3.4" = "Chondrite Ordinary",
  "L3.4-3.7" = "Chondrite Ordinary",
  "L3.5" = "Chondrite Ordinary",
  "L3.5-3.7" = "Chondrite Ordinary",
  "L3.5-3.8" = "Chondrite Ordinary",
  "L3.5-3.9" = "Chondrite Ordinary",
  "L3.5-5" = "Chondrite Ordinary",
  "L3.6" = "Chondrite Ordinary",
  "L3.6-4" = "Chondrite Ordinary",
  "L3.7" = "Chondrite Ordinary",
  "L3.7-3.9" = "Chondrite Ordinary",
  "L3.7-4" = "Chondrite Ordinary",
  "L3.7-6" = "Chondrite Ordinary",
  "L3.7/3.8" = "Chondrite Ordinary",
  "L3.8" = "Chondrite Ordinary",
  "L3.8-6" = "Chondrite Ordinary",
  "L3.8-an" = "Chondrite Ordinary",
  "L3.9" = "Chondrite Ordinary",
  "L3.9-5" = "Chondrite Ordinary",
  "L3.9-6" = "Chondrite Ordinary",
  "L3.9/4" = "Chondrite Ordinary",
  "L3/4" = "Chondrite Ordinary",
  "L4" = "Chondrite Ordinary",
  "L4 " = "Chondrite Ordinary",
  "L4-5" = "Chondrite Ordinary",
  "L4-6" = "Chondrite Ordinary",
  "L4-an" = "Chondrite Ordinary",
  "L4-melt breccia" = "Chondrite Ordinary",
  "L4/5" = "Chondrite Ordinary",
  "L5" = "Chondrite Ordinary",
  "L5 " = "Chondrite Ordinary",
  "L5-6" = "Chondrite Ordinary",
  "L5-7" = "Chondrite Ordinary",
  "L5-melt breccia" = "Chondrite Ordinary",
  "L5/6" = "Chondrite Ordinary",
  "L6" = "Chondrite Ordinary",
  "L6 " = "Chondrite Ordinary",
  "L6-melt breccia" = "Chondrite Ordinary",
  "L6/7" = "Chondrite Ordinary",
  "L7" = "Chondrite Ordinary",
  "LL" = "Chondrite Ordinary",
  "LL-imp melt" = "Chondrite Ordinary",
  "LL-melt breccia" = "Chondrite Ordinary",
  "LL-melt rock" = "Chondrite Ordinary",
  "LL(L)3" = "Chondrite Ordinary",
  "LL~3" = "Chondrite Ordinary",
  "LL~5" = "Chondrite Ordinary",
  "LL~6" = "Chondrite Ordinary",
  "LL3" = "Chondrite Ordinary",
  "LL3-5" = "Chondrite Ordinary",
  "LL3-6" = "Chondrite Ordinary",
  "LL3.0" = "Chondrite Ordinary",
  "LL3.00" = "Chondrite Ordinary",
  "LL3.05" = "Chondrite Ordinary",
  "LL3.1" = "Chondrite Ordinary",
  "LL3.1-3.5" = "Chondrite Ordinary",
  "LL3.15" = "Chondrite Ordinary",
  "LL3.2" = "Chondrite Ordinary",
  "LL3.3" = "Chondrite Ordinary",
  "LL3.4" = "Chondrite Ordinary",
  "LL3.5" = "Chondrite Ordinary",
  "LL3.6" = "Chondrite Ordinary",
  "LL3.7" = "Chondrite Ordinary",
  "LL3.7-6" = "Chondrite Ordinary",
  "LL3.8" = "Chondrite Ordinary",
  "LL3.8-4" = "Chondrite Ordinary",
  "LL3.8-6" = "Chondrite Ordinary",
  "LL3.9" = "Chondrite Ordinary",
  "LL3.9/4" = "Chondrite Ordinary",
  "LL3/4" = "Chondrite Ordinary",
  "LL4" = "Chondrite Ordinary",
  "LL4-5" = "Chondrite Ordinary",
  "LL4-6" = "Chondrite Ordinary",
  "LL4/5" = "Chondrite Ordinary",
  "LL5" = "Chondrite Ordinary",
  "LL5-6" = "Chondrite Ordinary",
  "LL5-7" = "Chondrite Ordinary",
  "LL5/6" = "Chondrite Ordinary",
  "LL6" = "Chondrite Ordinary",
  "LL6 " = "Chondrite Ordinary",
  "LL6-an" = "Chondrite Ordinary",
  "LL6-melt breccia" = "Chondrite Ordinary",
  "LL6(?)" = "Chondrite Ordinary",
  "LL7" = "Chondrite Ordinary",
  "LL7(?)" = "Chondrite Ordinary",
  "Lodranite" = "Achondrite Primitive",
  "Lodranite-an" = "Achondrite Primitive",
  "Lunar" = "Achondrite Lunar",
  "Lunar (anorth)" = "Achondrite Lunar",
  "Lunar (bas. breccia)" = "Achondrite Lunar",
  "Lunar (bas/anor)" = "Achondrite Lunar",
  "Lunar (basalt)" = "Achondrite Lunar",
  "Lunar (feldsp. breccia)" = "Achondrite Lunar",
  "Lunar (gabbro)" = "Achondrite Lunar",
  "Lunar (norite)" = "Achondrite Lunar",
  "Martian (basaltic breccia)" = "Achondrite Martian",
  "Martian (chassignite)" = "Achondrite Martian",
  "Martian (nakhlite)" = "Achondrite Martian",
  "Martian (OP\")" = "Achondrite Martian",
  "Martian (shergottite)" = "Achondrite Martian",
  "Mesosiderite" = "Stony-iron",
  "Mesosiderite-A" = "Stony-iron",
  "Mesosiderite-A1" = "Stony-iron",
  "Mesosiderite-A2" = "Stony-iron",
  "Mesosiderite-A3" = "Stony-iron",
  "Mesosiderite-A3/4" = "Stony-iron",
  "Mesosiderite-A4" = "Stony-iron",
  "Mesosiderite-an" = "Stony-iron",
  "Mesosiderite-B" = "Stony-iron",
  "Mesosiderite-B1" = "Stony-iron",
  "Mesosiderite-B2" = "Stony-iron",
  "Mesosiderite-B4" = "Stony-iron",
  "Mesosiderite-C" = "Stony-iron",
  "Mesosiderite-C2" = "Stony-iron",
  "Mesosiderite?" = "Stony-iron",
  "OC" = "Chondrite Ordinary",
  "Pallasite" = "Stony-iron",
  "Pallasite, PES" = "Stony-iron",
  "Pallasite, PMG" = "Stony-iron",
  "Pallasite, PMG-an" = "Stony-iron",
  "Pallasite, ungrouped" = "Stony-iron",
  "Pallasite?" = "Stony-iron",
  "R" = "Chondrite Rumurutti",
  "R3" = "Chondrite Rumurutti",
  "R3-4" = "Chondrite Rumurutti",
  "R3-5" = "Chondrite Rumurutti",
  "R3-6" = "Chondrite Rumurutti",
  "R3.5-4" = "Chondrite Rumurutti",
  "R3.5-6" = "Chondrite Rumurutti",
  "R3.6" = "Chondrite Rumurutti",
  "R3.8" = "Chondrite Rumurutti",
  "R3.8-5" = "Chondrite Rumurutti",
  "R3.8-6" = "Chondrite Rumurutti",
  "R3.9" = "Chondrite Rumurutti",
  "R4" = "Chondrite Rumurutti",
  "R5" = "Chondrite Rumurutti",
  "R6" = "Chondrite Rumurutti",
  "Relict iron" = "Iron",
  "Relict OC" = "Chondrite Ordinary",
  "Ureilite" = "Achondrite Primitive",
  "Ureilite-an" = "Achondrite Primitive",
  "Ureilite-pmict" = "Achondrite Primitive",
  "Winonaite" = "Achondrite Primitive"
)

md <- meteorite_data %>%
  na.omit() %>%
  rename(mass = mass..g.) %>%
  mutate(classification_type = classification_map[recclass]) %>%
  unnest(cols = c(classification_type)) %>%
  mutate(classification = str_split(classification_type, " ", simplify = TRUE)[, 1]) %>%
  filter(mass > 1) %>%
  mutate(classification_type = ifelse(classification_type == "Chondrite Ordinary" & 
                                        str_detect(recclass, "^(LL|L|H)"), 
                                      paste0(classification_type, " ", str_extract(recclass, "^(LL|H|L)")), 
                                      classification_type))

data <- read.csv("my_data.csv")
data <- data[data$year<2021,]
data1 <- data %>% group_by(year, classification) %>% summarise(mass=sum(mass))
data1 <- data1[data1$year >= 1399,]
data2 <- data %>% group_by(fall, classification, country) %>% summarise(mass=sum(mass))
data4 <- data %>% group_by(reclat, reclong, fall, year, country) %>% summarise(mass=sum(mass))
data$number <- 1
data <- data %>% group_by(country, year, fall) %>% summarise(Numbers=sum(number))
data <- na.omit(data)


train <- read.csv("train.csv")
train$creationDate <- (as.Date(train$creationDate, format="%Y-%m-%d"))
train$creationDate <- year(train$creationDate)
train <- train[,-c(1,2,3)]
train <- na.omit(train)
train <- train[,c("salutation","state", "color", "size", "return")]
train$return <- ifelse(train$return==1, "return", "not_return")
train$return <- factor(train$return)
f2 <- rpart(return~., data=train, control=rpart.control(minsplit=6, cp=0.02446921))


crimeDat <- readRDS("usaCrimeDat.rds")
myCrime1 <- as.character(unique(crimeDat$Crime))
# load("omaps.RData")
# crimes <- read.csv("omaha-crimes.csv") 
# myCrime <- as.character(unique(crimes$Crime))

library(shiny)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title="Meteorite Impact"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Discovery and Mass by Class", tabName="map3"),
      menuItem("Map", tabName="map4"),
      menuItem("Impact Location by Year", tabName="map7"),
      menuItem("Mass Density by Classification", tabName="map8"),
      menuItem("Number of Meteorites by Discovery", tabName="map9"),
      menuItem("Discoveries by Year", tabName="map10"),
      menuItem("Map of Meteorites by Discovery", tabName="map11"),
      menuItem("Mass Distribution by Classification", tabName="map13")
    )
  ),
  
  dashboardBody(
    tabItems(
      #third one
      tabItem(tabName="map3",
              h2("Discovery Type and Mass by Classification"),
              fluidRow(
                column(5,
                       plotlyOutput("mapping3", width=800, height=450))
              ),
              fluidRow(
                column(5,
                       selectInput("countries",
                                   label="Select a Country",
                                   choices=unique(data2$country),
                                   selected="United States of America")
                       )
              )
              ),
      #fourth one
      tabItem(tabName="map4",
              h2("Map"),
              fluidRow(
                column(5,
                       leafletOutput("mapping4", width=800, height=450)
                       )
              ),
              fluidRow(
                column(5,
                       selectInput("yearmap",
                                   label="Select a Year",
                                   choices=unique(data4[order(data4$year),"year"]),
                                   selected=1999)
                       )
              )
              ),
      #seventh one
      tabItem(tabName="map7",
              h2("Impact Location by Year"),
              fluidRow(
                column(5,
                       plotOutput("mapping7", width=800, height=400)
                       )
              )
              ),
      #eighth one
      tabItem(tabName="map8",
              h2("Mass Density by Classification"),
              fluidRow(
                column(5, 
                       plotOutput("mapping8", width=800, height=400)
                       )
              )
              ),
      #ninth one
      tabItem(tabName="map9",
              h2("Number of Meteorites by Discovery Type"),
              fluidRow(
                column(5,
                       plotOutput("mapping9", width=800, height=400)
                       )
              )
              ),
      #tenth one
      tabItem(tabName="map10",
              h2("Discoveries by Year"),
              fluidRow(
                column(5,
                       plotOutput("mapping10", width=800, height=400)
                       )
              )
              ),
      #eleventh one
      tabItem(tabName="map11",
              h2("Map of Meteorites by Discovery Type"),
              fluidRow(
                column(5,
                       plotOutput("mapping11", width=800, height=400)
                       )
              )
              ),
      #thirteen one
      tabItem(tabName="map13",
              h2("Mass Distribution by Classification Type"),
              fluidRow(
                column(5,
                       plotOutput("mapping13", width=800, height=400)
                       )
              )
              )
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #third one
    output$mapping3 <- renderPlotly({
      data2 <- subset(data2, data2$country %in% input$countries)
      plot_ly(data2, x=~fall, y=~mass, color=~classification) %>% add_bars() %>% layout(yaxis=list(title="mass in grams"))
    })
    
    #fourth one
    output$mapping4 <- renderLeaflet({
      data4 <- subset(data4, data4$year %in% input$yearmap)
      leaflet(data4) %>% addTiles() %>% addCircleMarkers(lat=~reclat, lng=~reclong, color=c("blue"), opacity=0.1, fillColor=c("blue"), fillOpacity=0.6, popup=~paste(country, fall, year), radius=~(sqrt(log(mass))))
    })
    
    output$mapping7 <- renderPlot({
      ggplot(md, aes(x = mass, fill = classification)) +
        geom_density(alpha = 0.5) +
        scale_x_log10(breaks = 10^(1:7), labels = scales::comma) +
        labs(x = expression(paste("Mass in Grams (log scale)")),
             y = "Density",
             fill = "Classification") +
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              panel.grid.major = element_line(color = "gray80", size = 0.2),
              legend.position = "bottom",
              legend.box = "horizontal")
    })
    
    output$mapping8 <- renderPlot({
      ggplot(md, aes(x = fall)) +
        geom_bar(fill = "dodgerblue3") +
        geom_text(stat = 'count', aes(label=..count..), vjust = -0.5, size = 3.5) +
        labs(x = "Fell vs Found",
             y = "Number of Meteorites") +
        theme(axis.ticks.x = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent", colour = NA),
              plot.background = element_rect(fill = "transparent", colour = NA))
    })
    
    output$mapping9 <- renderPlot({
      md_after_1900 <- md %>%
        filter(year > 1970 & year < 2012)
      
      ggplot(md_after_1900, aes(x = year, group = 1)) +
        geom_line(stat = "count", color = "forestgreen") +
        labs(x = "Year",
             y = "Number of Discoveries") +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "gray80", size = 0.2),
              panel.border = element_blank(),
              panel.background = element_blank())
    })
    
    output$mapping10 <- renderPlot({
      top_7_classification_type <- md %>%
        count(classification_type, sort = TRUE) %>%
        top_n(7)
      
      ggplot(top_7_classification_type, aes(x = reorder(classification_type, -n), y = n, fill = classification_type)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = n), vjust = -0.5, size = 3.5) + # Add the text labels
        labs(x = NULL, # Remove the x-axis label
             y = "Count") +
        theme(legend.position = "right",
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.ticks.x = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent", colour = NA),
              plot.background = element_rect(fill = "transparent", colour = NA)) +
        guides(fill = guide_legend(title = "Classification Type"))
    })
    
    output$mapping11 <- renderPlot({
      Ct <- md %>% group_by(reclat, reclong, fall) %>%
        summarize(count = n())
      
      ggplot() +
        geom_sf(data = world) +
        geom_point(data = Ct, aes(x = reclong, y = reclat, color = fall, size = count), alpha = 0.5, shape = 21) +
        coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
        labs(x = "Longitude",
             y = "Latitude") +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "gray80", size = 0.2),
              panel.border = element_blank(),
              panel.background = element_blank())
    })
    
    output$mapping13 <- renderPlot({
      ggplot(md, aes(x=as.factor(classification), y=log(mass))) + 
        geom_boxplot(fill="dodgerblue3") + 
        xlab("Classification Type") +
        ylab("Mass in Grams") +
        theme(panel.background = element_blank(),
              panel.grid.major = element_line(color = "gray70", size = 0.2),
              panel.border = element_blank(),
              axis.line = element_blank(),
              axis.line.x = element_blank(),
              axis.line.y = element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
