# Heatmap (Figure 5) created by Daniel Chrisendo

install.packages("ggplot")
install.packages("ggpattern")

# load required code libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpattern)
library(purrr)
library(scico)

# set working directory (i.e. directory that includes the files)
setwd("/Volumes/chrised1/Papers/Food Loss/Data/") 

#1 HEAT MAP FOR COMMODITIES (Figure 5a)
# read in excel file with regression results and add the commodities
data2 <- read.csv("Resultswheat_2023-04-17.csv")
data2 <- data2 %>% mutate(commodities="Wheat")
data3 <- read.csv("Resultsmaize_2023-04-17.csv")
data3 <- data3 %>% mutate(commodities="Maize")
data4 <- read.csv("Resultsrice_2023-04-17.csv")
data4 <- data4 %>% mutate(commodities="Rice")
data5 <- read.csv("Resultscereals_2023-04-17.csv")
data5 <- data5 %>% mutate(commodities="Other cereals & pulses")
data6 <- read.csv("Resultsfruits_2023-04-17.csv")
data6 <- data6 %>% mutate(commodities="Fruits & vegetables")
data7 <- read.csv("Resultsroots_2023-04-17.csv")
data7 <- data7 %>% mutate(commodities="Roots, tubers, & oil crops")

data_merged1<-merge(data2,data3,all.x = TRUE, all.y = TRUE)
data_merged2<-merge(data_merged1,data4,all.x = TRUE, all.y = TRUE)
data_merged3<-merge(data_merged2,data5,all.x = TRUE, all.y = TRUE)
data_merged4<-merge(data_merged3,data6,all.x = TRUE, all.y = TRUE)
data_merged5<-merge(data_merged4,data7,all.x = TRUE, all.y = TRUE)

data_merged5 <- data_merged5[, c(1,2,3,4,6,12,16)] %>% # select only those columns that are of interest
  arrange(commodities) %>% #to sort the data based on commodities
  dplyr::filter(as.numeric(X) <= 9 & as.numeric(X) >= 2) %>% #filter unnecessary row from the data
  mutate(Estimate = as.double(estimate), p.value = as.double(p.value)) %>%
  mutate(p_value_sign = case_when(
    p.value < 0.1 & p.value >= 0.05 ~ 0.01,
    p.value < 0.05 & p.value >= 0.01 ~ 0.05,
    p.value < 0.01 ~ 0.1))

# create heatmap
data_merged5 %>%
  mutate(commodities = fct_relevel(commodities, "All", 
                                   "Wheat", "Maize", "Rice", 
                                   "Other cereals & pulses", "Fruits & vegetables", "Roots, tubers, & oil crops")) %>% #to reorder the x-axis
  mutate(term = fct_relevel(term, 
                            "property", "export", "polStab", "phoneSub", "electricityRural",
                            "msch", "agriEmp", "agriShr", "ln_gni")) %>% #to reorder the y-axis
  ggplot(aes(y=term, x=commodities, fill=Estimate)) + # initialize a ggplot
  labs(fill=expression(beta*~estimate)) +
  geom_tile(color = "grey", lwd =0.1, linetype =1) + #to customize the border
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle =45, hjust =0, size=15, color="black", family ="Times"),
        axis.text.y = element_text(size=15, color="black", family ="Times")) + # minor formatting of the figure
  scale_x_discrete(labels=c("All"="All commodities", "Wheat"="Wheat", "Maize"="Maize", "Rice"="Rice", 
                            "Other cereals & pulses"="Other cereals & pulses", "Fruits & vegetables"="Fruits & vegetables", "Roots, tubers, & oil bearing crops"="Roots, tubers, & oil bearing crops"),
                   position ="top") + #to rename and move the labels to the top
  scale_fill_scico(palette= 'vik', limits = c(-1.25,1.25), oob = scales::squish, breaks = seq(-1,1, by=0.25), super=ScaleBinned) +  # define a colorscale for the figure
  geom_point(data = data_merged5 %>% filter(p_value_sign <= 0.1), aes( x = commodities, y = term, size = p_value_sign )) +
  labs(size = "p-value <") +
  scale_size_continuous(breaks=c(0.1, 0.05, 0.01), labels=c('0.01' , '0.05', '0.1')) +
  scale_y_discrete(labels=c("ln_gni"="GNI per capita", "agriShr"="Agriculture share of GDP",
                            "agriEmp"="Employment in agriculture", "msch"="Years of education",
                            "electricityRural"="Access to electricity in rural area", "phoneSub"="Mobile cellular subscription",
                            "polStab"="Political stability", "export"="Export volume index")) + #to rename X-axis value
  theme(text=element_text(size=14, color = "black", family ="Times"))

ggsave("revHeatmapCommodities1.pdf")

#2. HEAT MAP FOR SUPPLY STAGE (Figure 5b)
# read in excel file with regression results and add the stage
data8 <- read.csv("resultsFarm_2023-04-17.csv")
data8 <- data8 %>% mutate(stage="Farm")
data9 <- read.csv("resultsHarvest_2023-04-17.csv")
data9 <- data9 %>% mutate(stage="Harvest")
data10 <- read.csv("resultsStorage_2023-04-17.csv")
data10 <- data10 %>% mutate(stage="Storage")
data11 <- read.csv("resultsTransport_2023-04-17.csv")
data11 <- data11 %>% mutate(stage="Transport")

data_merged8<-merge(data8,data9,all.x = TRUE, all.y = TRUE)
data_merged9<-merge(data_merged8,data10,all.x = TRUE, all.y = TRUE)
data_merged10<-merge(data_merged9,data11,all.x = TRUE, all.y = TRUE)

data_merged10 <- data_merged10[, c(1,2,3,4,6,12,16)] %>% # select only those columns that are of interest
  arrange(stage) %>% #to sort the data based on commodities
  dplyr::filter(as.numeric(X) <= 9 & as.numeric(X) >= 2) %>% #filter unnecessary row from the data
  mutate(Estimate = as.double(estimate), p.value = as.double(p.value)) %>%
  mutate(p_value_sign = case_when(
    p.value < 0.1 & p.value >= 0.05 ~ 0.01,
    p.value < 0.05 & p.value >= 0.01 ~ 0.05,
    p.value < 0.01 ~ 0.1))

# create heatmap
data_merged10 %>%
  mutate(stage = fct_relevel(stage, "Farm", "Harvest", "Storage", "Transport")) %>% #to reorder the x-axis
  mutate(term = fct_relevel(term, 
                            "property", "export", "polStab", "phoneSub", "electricityRural",
                            "msch", "agriEmp", "agriShr", "ln_gni")) %>% #to reorder the y-axis
  ggplot(aes(y=term, x=stage, fill=Estimate)) + # initialize a ggplot
  labs(fill=expression(beta*~estimate)) +
  geom_tile(color = "grey", lwd =0.1, linetype =1) + #to customize the border
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size=15, color="black", family ="Times"),
        axis.text.y = element_text(size=15, color="black", family ="Times")) + # minor formatting of the figure
  scale_x_discrete(position = "top") + # move x-axis ticks to the top
  scale_fill_scico(palette= 'vik', limits = c(-1.25,1.25), oob = scales::squish, breaks = seq(-1,1, by=0.25), super=ScaleBinned) +  # define a colorscale for the figure
  geom_point(data = data_merged10 %>% filter(p_value_sign <= 0.1), aes( x = stage, y = term, size = p_value_sign)) +
  labs(size = "p-value <") +
  scale_size_continuous(breaks=c(0.1, 0.05,0.01), labels=c('0.01', '0.05', '0.1')) +
  scale_y_discrete(labels=c("ln_gni"="GNI per capita", "agriShr"="Agriculture share of GDP",
                            "agriEmp"="Employment in agriculture", "msch"="Years of education",
                            "electricityRural"="Access to electricity in rural area", "phoneSub"="Mobile cellular subscription",
                            "polStab"="Political stability", "export"="Export volume index")) + #to rename X-axis value
  theme(text=element_text(size=14, color = "black", family ="Times"))

ggsave("revHeatmapStage1.pdf")

#3. HEAT MAP FOR DIFFERENT INCOME LEVEL (Figure 5c)
# read in excel file with regression results and add the stage
data12 <- read.csv("revResults_pool.csv")
data12 <- data12 %>% mutate(income="All")
data13 <- read.csv("ResultsL_2023-04-17.csv")
data13 <- data13 %>% mutate(income="Low-income")
data14 <- read.csv("ResultsM_2023-04-17.csv")
data14 <- data14 %>% mutate(income="Middle- & high-income")

data_merged12<-merge(data12,data13,all.x = TRUE, all.y = TRUE)
data_merged13<-merge(data_merged12,data14,all.x = TRUE, all.y = TRUE)

data_merged13 <- data_merged13[, c(1,2,3,4,6,12,16)] %>% # select only those columns that are of interest
  arrange(income) %>% #to sort the data based on commodities
  dplyr::filter(as.numeric(X) <= 9 & as.numeric(X) >= 2) %>% #filter unnecessary row from the data
  mutate(Estimate = as.double(estimate), p.value = as.double(p.value)) %>%
  mutate(p_value_sign = case_when(
    p.value < 0.1 & p.value >= 0.05 ~ 0.01,
    p.value < 0.05 & p.value >= 0.01 ~ 0.05,
    p.value < 0.01 ~ 0.1))

# create heatmap
data_merged13 %>%
  mutate(income = fct_relevel(income, "All", "Low-income", "Middle- & high-income")) %>% #to reorder the x-axis
  mutate(term = fct_relevel(term, 
                            "property", "export", "polStab", "phoneSub", "electricityRural",
                            "msch", "agriEmp", "agriShr", "ln_gni")) %>% #to reorder the y-axis
  ggplot(aes(y=term, x=income, fill=Estimate)) + # initialize a ggplot
  labs(fill=expression(beta*~estimate)) +
  geom_tile(color = "grey", lwd =0.1, linetype =1) + #to customize the border
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size=10, color="black", family ="Times"),
        axis.text.y = element_text(size=10, color="black", family ="Times")) + # minor formatting of the figure
  scale_x_discrete(position = "top") + # move x-axis ticks to the top
  scale_fill_scico(palette= 'vik', limits = c(-1.25,1.25), oob = scales::squish, breaks = seq(-1,1, by=0.25), super=ScaleBinned) +  # define a colorscale for the figure
  geom_point(data = data_merged13 %>% filter(p_value_sign <= 0.1), aes( x = income, y = term, size = p_value_sign)) +
  labs(size = "p-value <") +
  scale_size_continuous(breaks=c(0.1, 0.05,0.01), labels=c('0.01', '0.05', '0.1')) +
  scale_y_discrete(labels=c("ln_gni"="GNI per capita", "agriShr"="Agriculture share of GDP",
                            "agriEmp"="Employment in agriculture", "msch"="Years of education",
                            "electricityRural"="Access to electricity in rural area", "phoneSub"="Mobile cellular subscription",
                            "polStab"="Political stability", "export"="Export volume index")) + #to rename X-axis value
  theme(text=element_text(size=10, color = "black", family ="Times"))

ggsave("revHeatmapIncome1.pdf")

