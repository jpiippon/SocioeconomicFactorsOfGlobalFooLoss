#Data reading, cleaning, and regression, Table 2-7, and Figure 4 made by Daniel Chrisendo

install.packages("sf")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("naniar")
install.packages("scico")
install.packages("xlsx")
install.packages("writexl")
install.packages("fixest")
install.packages("modelsummary")
install.packages("flextable")
install.packages("epiDisplay")
install.packages("plotrix")

library(sf)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)
library(fixest)
library(readr)
library(broom)
library("naniar")
library(ggplot2)
library(forcats)
library(scico)
library(openxlsx)
library(xlsx)
library("writexl")
library(zoo)
library(Hmisc)
library(modelsummary)
library(flextable)
library(plotrix)
library(epiDisplay)

#set workind directory. For example:
#setwd("/Volumes/chrised1/Papers/Food Loss/Data/")

#1 Read data
#1.a country ids
cntryID <- read_csv("countries_codes_and_coordinates.csv") %>% 
  select(-GADM_code) %>% 
  select(cntry_code,iso2,iso3,Country,RegionID) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>%
  mutate(RegionID = as.factor(RegionID))

#1.b regional name
regName <- cntryID %>% 
  rename(Country.Name=`Country`) %>%
  mutate(regionalName= ifelse(RegionID==1, "Australia and Oceania",
                              ifelse(RegionID==2|RegionID==6, "Latin America & the Caribbean",
                                     ifelse(RegionID==3|RegionID==11, "East & Southeast Asia",
                                            ifelse(RegionID==4|RegionID==10|RegionID==12, "Europe & North America",
                                                   ifelse(RegionID==5, "Central & South Asia",
                                                          ifelse(RegionID==7, "Sub-Saharan Africa",
                                                                 ifelse(RegionID==8|RegionID==9, "Middle East & North Africa", NA)))))))) %>%
  #correct regionalName of some countries
  mutate(regionalName=ifelse(cntry_code==4|cntry_code==398|cntry_code==417|cntry_code==762|cntry_code==795|
                               cntry_code==860|cntry_code==876,"Central & South Asia",regionalName)) %>%
  mutate(regionalName=ifelse(cntry_code==516, "Sub-Saharan Africa", regionalName)) %>%
  pivot_longer(!c(Country.Name,iso3,iso2,cntry_code,RegionID), values_to = "regionalName") %>% 
  select(-c(iso2,iso3,Country.Name,name,RegionID))

#1.c World Bank data
f_readWBdata <- function(dataIn,varName) {
  # read urb population share data (world bank)
  gni <- read_excel(dataIn,sheet = "Data",skip = 2) %>%
    select(`Country Name`, `Country Code`, as.character(c(1990:2020))) %>%
    rename(iso3=`Country Code`) %>% 
    rename(Country.Name=`Country Name`) %>% 
    mutate(iso3 = ifelse(iso3=="XKX", "XKO", iso3)) %>% # for kosovo correct code
    as_tibble() %>% 
    left_join(cntryID[,c(1,3)]) %>% 
    filter(!is.na(cntry_code)) %>% 
    select(Country.Name,iso3,cntry_code,everything())
  #drop_na()
  
  # put it to long format so that easy to join with food loss data
  gni_long <- gni %>% 
    pivot_longer(!c(Country.Name,iso3,cntry_code),names_to = "year", values_to = varName) %>% 
    select(-c(iso3,Country.Name)) %>% 
    # we need to make year as numeric to be compatible with food loss data
    mutate(year = as.numeric(year))
  
  return(gni_long)
}

# read all the data in you need to
# just download the xls data from world bank and put it to a function
gni_long <- f_readWBdata("API_NY.GNP.PCAP.PP.CD_DS2_en_excel_v2_3472325.xls","gni")
agriShr_long <- f_readWBdata("API_NV.AGR.TOTL.ZS_DS2_en_excel_v2_3469285.xls","agriShr")
agriEmp_long <- f_readWBdata("API_SL.AGR.EMPL.ZS_DS2_en_excel_v2_3469532.xls","agriEmp")
electricityRural_long <- f_readWBdata("API_EG.ELC.ACCS.RU.ZS_DS2_en_excel_v2_3471132.xls","electricityRural")
phoneSub_long <- f_readWBdata("API_IT.CEL.SETS.P2_DS2_en_excel_v2_3469999.xls", "phoneSub")
export_long <- f_readWBdata("API_TX.QTY.MRCH.XD.WD_DS2_en_excel_v2_3480226.xls", "export")
property_long <- f_readWBdata("API_IQ.CPA.PROP.XQ_DS2_en_excel_v2_3693990.xls", "property")

#1.d World bank data (income level)
f_readWBdata1 <- function(dataIn,varName) {
  # read the income level data (World Bank)
  incLevel <- read_excel(dataIn, sheet="Country Analytical History", skip = 5)
  incLevel[-c(1:5),] %>%
    select(c(1:2,6:36)) %>%
    rename_at(1, ~'iso3') %>%
    rename_at(2, ~'Country.Name') %>%
    mutate(iso3 = ifelse(iso3=="XKX", "XKO", iso3)) %>% # for kosovo correct code
    as_tibble() %>%
    left_join(cntryID[,c(1,3)]) %>%
    filter(!is.na(cntry_code)) %>%
    select(Country.Name,iso3,cntry_code,everything())
}

incLevel <- f_readWBdata1("OGHIST.xlsx","incLevel")

#put it into long format so that easy to join with food loss data
incLevel_long <- incLevel %>%
  pivot_longer(!c(Country.Name,iso3,cntry_code),names_to = "year") %>% 
  select(-c(iso3,Country.Name)) %>% 
  # we need to make year as numeric to be compatible with food loss data
  mutate(year = as.numeric(year)) %>%
  rename_at(3, ~'incLevel')

#1.e UN Data
f_readUNdata1 <- function(dataIn,varName) {
  # read education data
  edu <- read_csv(dataIn,skip = 6) %>%
    # remove columns ...4, ...6, ... ...62
    select(-paste0('...',seq(4,62,2))) %>%
    # replace '..' values with NA
    naniar::replace_with_na_all(condition = ~.x == '..') %>%
    #replace_with_na(replace = list('1990' = '..'))
    select(-'HDI Rank') %>%
    # rename countries to match the cntry_id file
    mutate(Country = ifelse(Country == 'Bolivia (Plurinational State of)', 'Bolivia',
                            ifelse(Country == 'Brunei Darussalam', 'Brunei',
                                   ifelse(Country == 'Cabo Verde','Cape Verde',
                                          ifelse(Country == 'Congo (Democratic Republic of the)','Congo',Country))))) %>%
    mutate(Country = ifelse(Country == 'Czechia', 'Czech Republic',
                            ifelse(Country == "Côte d'Ivoire", 'Ivory Coast',
                                   ifelse(Country == 'Eswatini (Kingdom of)','Swaziland',
                                          ifelse(Country == 'Hong Kong, China (SAR)','Hong Kong',Country))))) %>%
    mutate(Country = ifelse(Country == 'Iran (Islamic Republic of)', 'Iran, Islamic Republic of',
                            ifelse(Country == "Korea (Republic of)", 'South Korea',
                                   ifelse(Country == 'Micronesia (Federated States of)','Micronesia, Federated States of',
                                          ifelse(Country == 'Moldova (Republic of)','Moldova, Republic of',Country))))) %>%
    mutate(Country = ifelse(Country == 'North Macedonia', 'Macedonia, the former Yugoslav Republic of',
                            ifelse(Country == "Palestine, State of", 'Palestinian Territory, Occupied',
                                   ifelse(Country == 'Russian Federation','Russia',
                                          ifelse(Country == 'Tanzania (United Republic of)','Tanzania, United Republic of',Country))))) %>%
    mutate(Country = ifelse(Country == 'Venezuela (Bolivarian Republic of)', 'Venezuela',
                            ifelse(Country == "Viet Nam", 'Vietnam',Country))) %>%
    # join cntry_id file
    left_join(cntryID[,c(1,4)]) %>%
    select(Country,cntry_code,everything()) %>%
    # remove entries without cntry_code
    filter(!is.na(cntry_code))
  
  # put it to long format so that easy to join with food loss data
  edu_long <- edu %>%
    pivot_longer(!c(Country,cntry_code),names_to = "year", values_to = varName) %>%
    select(-c(Country)) %>%
    # we need to make year as numeric to be compatible with food loss data
    mutate(year = as.numeric(year))
  
  return(edu_long)
}

# read all the data in you need to
# just download the xls data from world bank and put it to a function
school_long <- f_readUNdata1("Mean years of schooling (years).csv","schoolYear") %>%
  mutate(schoolYear = as.numeric(schoolYear))

#1.f FAO data
fao_id <- read.csv('cntry_fao_id.csv')

# join fao id with other ids
cntryID_fao <- cntryID %>% 
  left_join(fao_id[,c(4,11)], by = c('cntry_code'='iso_n3'))

f_readFAOSTATdata <- function(dataIn,varName) {
  # read data
  # read faostat data 
  polStab <- read_excel(dataIn,sheet = "Sheet1",skip = 0) %>%
    # select the wanted columns
    select(`Area Code (FAO)`, `Area`,Year,Value) %>%
    rename(fao_id = 'Area Code (FAO)') %>% 
    rename(Country=Area) %>% 
    rename(year = Year) %>% 
    mutate(fao_id = as.integer(fao_id)) %>% 
    # year and Value to numeric
    mutate(year = as.numeric(year)) %>% 
    mutate(Value = as.numeric(Value)) %>% 
    # add the cntry id data
    left_join(cntryID_fao[,c(1,6)]) %>% 
    # remove all countries without cntry code
    filter(!is.na(cntry_code)) %>% 
    # rename Value with the given VarName
    rename(!!varName :=  Value) %>% 
    
    # select the wanted columns %>% 
    select(cntry_code,year,!!varName)
  
  return(polStab)
}

polStab <- f_readFAOSTATdata("FAOSTAT_data_1-31-2022-2.xls",'polStab')

#1.g Food loss data
foodLoss <- read_csv("Data.csv") %>%
  rename(cntry_code = m49_code) %>%
  # you can choose the wanted columns
  select(cntry_code,country,year,cpc_code,commodity,activity,loss_percentage,food_supply_stage) %>%
  # create new group for food supply chain
  mutate(supChainGr= ifelse(food_supply_stage=="Farm" | food_supply_stage=="Harvest" | food_supply_stage=="Pre-harvest", "On-farm",
                            ifelse(food_supply_stage=="Post-harvest" | food_supply_stage=="Grading" | food_supply_stage=="Transport" | food_supply_stage=="Storage"| food_supply_stage=="Trader" | food_supply_stage=="Processing", "Post-harvest", NA))) %>%
  group_by(country, year, commodity, activity, food_supply_stage) %>%
  mutate(loss_percentage=mean(loss_percentage)) %>%
  ungroup() %>%
  distinct() %>% #to remove duplicates
  group_by(country, year, commodity, food_supply_stage) %>% #to add all loss_percentage in the same food supply stage
  mutate(loss_percentage1 = sum(loss_percentage)) %>%
  ungroup() %>%
  distinct(country, year, commodity, food_supply_stage, .keep_all= TRUE) %>%
  drop_na(supChainGr)

#1.f FAO basket items

cpc_cereals <- read_csv("Data_cerealsPulses.csv") %>%
  select(cpc_code) %>%
  distinct() %>%
  mutate(basketItem = "cereals")

cpc_fruits <- read_csv("Data_fruitsVegs.csv") %>%
  select(cpc_code) %>%
  distinct( ) %>%
  mutate(basketItem = "fruits")

cpc_meat <- read_csv("Data_meatAnimal.csv") %>%
  select(cpc_code) %>%
  distinct() %>%
  mutate(basketItem = "meat")

cpc_other <- read_csv("Data_other.csv") %>%
  select(cpc_code) %>%
  distinct() %>%
  mutate(basketItem = "other")

cpc_roots <- read_csv("Data_rootsTubers.csv") %>%
  select(cpc_code) %>%
  distinct() %>%
  mutate(basketItem = "roots")

cpc_basketItem <- rbind(cpc_cereals,cpc_fruits,cpc_meat,cpc_other,cpc_roots)

#1.g Combine sosec data
sosecData <- gni_long %>%
  left_join(agriShr_long) %>%
  left_join(agriEmp_long) %>%
  left_join(school_long) %>%
  left_join(export_long) %>%
  left_join(phoneSub_long) %>%
  left_join(electricityRural_long) %>%
  left_join(polStab) %>%
  left_join(property_long) %>%
  left_join(cntryID_fao) %>%
  select(1,14, 2:11) #to rearrange the column order

write.csv(sosecData, "/Volumes/chrised1/Papers/Food Loss/Data/sosecData1.csv")

#The missing SE data is then interpolated and extrapolated by Matti Kummu (please see the interpolation and extrapolation code)

#2 combine food loss data with SE data that has been interpolated and extrapolated. The new SE data is derived from the interpolateAdm0

intraExtra <- read_csv("matti_adm0_seData_interpExtrap_GnicMsch_2000_2020.csv")

foodLoss_comb <- foodLoss %>%
  left_join(cpc_basketItem) %>%
  mutate(basketItem2= ifelse(commodity=="Wheat", "wheat",
                             ifelse(commodity=="Maize (corn)", "maize",
                                    ifelse(commodity=="Rice", "rice", basketItem)))) %>%
  select(c(1:3,5,8,10,12)) %>%
  filter(basketItem2=="wheat"|basketItem2=="rice"|basketItem2=="maize"|basketItem2=="cereals"|basketItem2=="fruits"|
           basketItem2=="fruits"|basketItem2=="roots") %>%
  left_join(intraExtra) %>%
  left_join(regName) %>%
  left_join(incLevel_long) %>% #combine with income level
  mutate(incLevel= str_replace(incLevel, "LM", "M")) %>%
  mutate(incLevel= str_replace(incLevel, "UM", "M")) %>%
  mutate(incLevel= str_replace(incLevel, "H", "M")) %>%
  filter(country!="Europe" & country!="Southern Asia" & country!="South-Eastern Asia" & country!="Central Asia" &
           country!="Western Asia" & country!="Northern Africa" & country!="Western Africa" & country!="Sub-Saharan Africa" &
           country!="Latin America and the Caribbean" & country!="Northern America") %>%
  #select(-property) %>% #uncomment this if I want to exclude property
  #polstab has range from - to + values so need to be normalized
  mutate(polStab = (polStab - min(polStab, na.rm=T)) / (max(polStab, na.rm=T) - min(polStab, na.rm=T))) %>%
  mutate(ln_gni = log(gni)) %>%
  mutate(ln_gnic = log(gnic)) %>%
  mutate(gnic = coalesce(gnic, gni)) %>%
  mutate(ln_gnic = coalesce(ln_gnic, ln_gni)) %>%
  mutate(msch = coalesce(msch, schoolYear)) %>%
  drop_na()

#3 Final data for analysis (only including the relevant food supply stages and commodities)
foodLoss_comb_sel4 <- foodLoss_comb %>%
  filter(food_supply_stage =="Farm" | food_supply_stage =="Harvest" | food_supply_stage =="Transport" | food_supply_stage =="Storage") %>% 
  filter(basketItem2 == "wheat" | basketItem2 == "rice" | basketItem2 == "maize" | basketItem2 == "cereals" | basketItem2 == "fruits" | basketItem2 == "roots")
write.csv(foodLoss_comb_sel4,"/Volumes/chrised1/Papers/Food Loss/Data/revFinal_data.csv") #To save the final data

#4 To generate content of Table 2
mean_se_descriptive <-
  foodLoss_comb_sel4 %>%
  group_by(incLevel) %>%
  summarise(loss_farm_wheat=mean(loss_percentage1[food_supply_stage=="Farm" & basketItem2=="wheat"]),
            se_farm_wheat=std.error(loss_percentage1[food_supply_stage=="Farm" & basketItem2=="wheat"]),
            ne_farm_wheat=sum(food_supply_stage=="Farm" & basketItem2=="wheat"),
            loss_harvest_wheat=mean(loss_percentage1[food_supply_stage=="Harvest" & basketItem2=="wheat"]),
            se_harvest_wheat=std.error(loss_percentage1[food_supply_stage=="Harvest" & basketItem2=="wheat"]),
            ne_harvest_wheat=sum(food_supply_stage=="Harvest" & basketItem2=="wheat"),
            loss_storage_wheat=mean(loss_percentage1[food_supply_stage=="Storage" & basketItem2=="wheat"]),
            se_storage_wheat=std.error(loss_percentage1[food_supply_stage=="Storage" & basketItem2=="wheat"]),
            ne_storage_wheat=sum(food_supply_stage=="Storage" & basketItem2=="wheat"),
            loss_transport_wheat=mean(loss_percentage1[food_supply_stage=="Transport" & basketItem2=="wheat"]),
            se_transport_wheat=std.error(loss_percentage1[food_supply_stage=="Transport" & basketItem2=="wheat"]),
            ne_transport_wheat=sum(food_supply_stage=="Transport" & basketItem2=="wheat"),
            loss_farm_maize=mean(loss_percentage1[food_supply_stage=="Farm" & basketItem2=="maize"]),
            se_farm_maize=std.error(loss_percentage1[food_supply_stage=="Farm" & basketItem2=="maize"]),
            ne_farm_maize=sum(food_supply_stage=="Farm" & basketItem2=="maize"),
            loss_harvest_maize=mean(loss_percentage1[food_supply_stage=="Harvest" & basketItem2=="maize"]),
            se_harvest_maize=std.error(loss_percentage1[food_supply_stage=="Harvest" & basketItem2=="maize"]),
            ne_harvest_maize=sum(food_supply_stage=="Harvest" & basketItem2=="maize"),
            loss_storage_maize=mean(loss_percentage1[food_supply_stage=="Storage" & basketItem2=="maize"]),
            se_storage_maize=std.error(loss_percentage1[food_supply_stage=="Storage" & basketItem2=="maize"]),
            ne_storage_maize=sum(food_supply_stage=="Storage" & basketItem2=="maize"),
            loss_transport_maize=mean(loss_percentage1[food_supply_stage=="Transport" & basketItem2=="maize"]),
            se_transport_maize=std.error(loss_percentage1[food_supply_stage=="Transport" & basketItem2=="maize"]),
            ne_transport_maize=sum(food_supply_stage=="Transport" & basketItem2=="maize"),
            loss_farm_rice=mean(loss_percentage1[food_supply_stage=="Farm" & basketItem2=="rice"]),
            se_farm_rice=std.error(loss_percentage1[food_supply_stage=="Farm" & basketItem2=="rice"]),
            ne_farm_rice=sum(food_supply_stage=="Farm" & basketItem2=="rice"),
            loss_harvest_rice=mean(loss_percentage1[food_supply_stage=="Harvest" & basketItem2=="rice"]),
            se_harvest_rice=std.error(loss_percentage1[food_supply_stage=="Harvest" & basketItem2=="rice"]),
            ne_harvest_rice=sum(food_supply_stage=="Harvest" & basketItem2=="rice"),
            loss_storage_rice=mean(loss_percentage1[food_supply_stage=="Storage" & basketItem2=="rice"]),
            se_storage_rice=std.error(loss_percentage1[food_supply_stage=="Storage" & basketItem2=="rice"]),
            ne_storage_rice=sum(food_supply_stage=="Storage" & basketItem2=="rice"),
            loss_transport_rice=mean(loss_percentage1[food_supply_stage=="Transport" & basketItem2=="rice"]),
            se_transport_rice=std.error(loss_percentage1[food_supply_stage=="Transport" & basketItem2=="rice"]),
            ne_transport_rice=sum(food_supply_stage=="Transport" & basketItem2=="rice"),
            loss_farm_cereals=mean(loss_percentage1[food_supply_stage=="Farm" & basketItem2=="cereals"]),
            se_farm_cereals=std.error(loss_percentage1[food_supply_stage=="Farm" & basketItem2=="cereals"]),
            ne_farm_cereals=sum(food_supply_stage=="Farm" & basketItem2=="cereals"),
            loss_harvest_cereals=mean(loss_percentage1[food_supply_stage=="Harvest" & basketItem2=="cereals"]),
            se_harvest_cereals=std.error(loss_percentage1[food_supply_stage=="Harvest" & basketItem2=="cereals"]),
            ne_harvest_cereals=sum(food_supply_stage=="Harvest" & basketItem2=="cereals"),
            loss_storage_cereals=mean(loss_percentage1[food_supply_stage=="Storage" & basketItem2=="cereals"]),
            se_storage_cereals=std.error(loss_percentage1[food_supply_stage=="Storage" & basketItem2=="cereals"]),
            ne_storage_cereals=sum(food_supply_stage=="Storage" & basketItem2=="cereals"),
            loss_transport_cereals=mean(loss_percentage1[food_supply_stage=="Transport" & basketItem2=="cereals"]),
            se_transport_cereals=std.error(loss_percentage1[food_supply_stage=="Transport" & basketItem2=="cereals"]),
            ne_transport_cereals=sum(food_supply_stage=="Transport" & basketItem2=="cereals"),
            loss_farm_fruits=mean(loss_percentage1[food_supply_stage=="Farm" & basketItem2=="fruits"]),
            se_farm_fruits=std.error(loss_percentage1[food_supply_stage=="Farm" & basketItem2=="fruits"]),
            ne_farm_fruits=sum(food_supply_stage=="Farm" & basketItem2=="fruits"),
            loss_harvest_fruits=mean(loss_percentage1[food_supply_stage=="Harvest" & basketItem2=="fruits"]),
            se_harvest_fruits=std.error(loss_percentage1[food_supply_stage=="Harvest" & basketItem2=="fruits"]),
            ne_harvest_fruits=sum(food_supply_stage=="Harvest" & basketItem2=="fruits"),
            loss_storage_fruits=mean(loss_percentage1[food_supply_stage=="Storage" & basketItem2=="fruits"]),
            se_storage_fruits=std.error(loss_percentage1[food_supply_stage=="Storage" & basketItem2=="fruits"]),
            ne_storage_fruits=sum(food_supply_stage=="Storage" & basketItem2=="fruits"),
            loss_transport_fruits=mean(loss_percentage1[food_supply_stage=="Transport" & basketItem2=="fruits"]),
            se_transport_fruits=std.error(loss_percentage1[food_supply_stage=="Transport" & basketItem2=="fruits"]),
            ne_transport_fruits=sum(food_supply_stage=="Transport" & basketItem2=="fruits"),
            loss_farm_roots=mean(loss_percentage1[food_supply_stage=="Farm" & basketItem2=="fruits"]),
            se_farm_roots=std.error(loss_percentage1[food_supply_stage=="Farm" & basketItem2=="roots"]),
            ne_farm_roots=sum(food_supply_stage=="Farm" & basketItem2=="roots"),
            loss_harvest_roots=mean(loss_percentage1[food_supply_stage=="Harvest" & basketItem2=="fruits"]),
            se_harvest_roots=std.error(loss_percentage1[food_supply_stage=="Harvest" & basketItem2=="roots"]),
            ne_harvest_roots=sum(food_supply_stage=="Harvest" & basketItem2=="roots"),
            loss_storage_roots=mean(loss_percentage1[food_supply_stage=="Storage" & basketItem2=="fruits"]),
            se_storage_roots=std.error(loss_percentage1[food_supply_stage=="Storage" & basketItem2=="roots"]),
            ne_storage_roots=sum(food_supply_stage=="Storage" & basketItem2=="roots"),
            loss_transport_roots=mean(loss_percentage1[food_supply_stage=="Transport" & basketItem2=="fruits"]),
            se_transport_roots=std.error(loss_percentage1[food_supply_stage=="Transport" & basketItem2=="roots"]),
            ne_transport_roots=sum(food_supply_stage=="Transport" & basketItem2=="roots"),
            gni_mean = mean(gni),
            se_gni=std.error(gni),
            ne_gni=n(),
            agriShr_mean = mean(agriShr),
            se_agriShr=std.error(agriShr),
            ne_agriShr=n(),
            agriEmp_mean = mean(agriEmp),
            se_agriEmp=std.error(agriEmp),
            ne_agriEmp=n(),
            years_of_schooling_mean = mean(msch),
            se_msch=std.error(msch),
            ne_msch=n(),
            electricityRural_mean = mean(electricityRural),
            se_electricityRural=std.error(electricityRural),
            ne_electricityRural=n(),
            phoneSub_mean = mean(phoneSub),
            se_phoneSub=std.error(phoneSub),
            ne_phoneSub=n(),
            polStab_mean = mean(polStab),
            se_polStab=std.error(polStab),
            ne_polStab=n(),
            export_mean = mean(export),
            se_export=std.error(export),
            ne_export=n())
            #median_incLevel_num = median(as.numeric(factor(incLevel, levels = c("L", "M"))))) #%>% 
  #st_drop_geometry()

write.csv(mean_se_descriptive,"/Volumes/chrised1/Papers/Food Loss/Data/mean_se_descriptive.csv")

y <- feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export + regionalName + as.factor(year), cluster =  ~ regionalName, data = foodLoss_comb_sel4)
summary(y)

#5 To create Figure 4
#Stacked BAR PLOT
foodLoss_comb_sel4 %>%
  mutate(basketItem2 = fct_relevel(basketItem2, 
                                   "wheat", "maize", "rice", 
                                   "cereals", "fruits", "roots")) %>% #to reorder the x-axis
  mutate(food_supply_stage = fct_relevel(food_supply_stage, 
                                         "Transport", "Storage", "Harvest", "Farm")) %>% #to reorder the y-axis
  mutate(loss_percentage2=ave(loss_percentage1, by=list(food_supply_stage, basketItem2))) %>% #create a new variable which is the mean of percentage loss in specific basket item and supply chain
  distinct(loss_percentage2, .keep_all = TRUE) %>% #drop duplicates
  ggplot(aes(x=basketItem2, y=loss_percentage2, fill=food_supply_stage)) + #the fill is to create stack
  geom_bar(position="stack", stat = "identity") +
  scale_fill_viridis_d() + #to add colorblind friendly color
  labs(y="Loss percentage", x = "Commodities") +
  scale_x_discrete(labels=c("wheat"="Wheat", "maize"="Maize", "rice"="Rice", "cereals"="Other cereals & pulses",
                            "fruits"="Fruits & vegetables", "roots"="Roots, tubers, & oil-bearing crops")) + #to rename X-axis value
  labs(fill="Value chain stages") + #to change the legend label
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle =45, hjust =1)) #to remove the background but not the x and y line and to tilt the x label
ggsave("revBarPlot.pdf")

#coefficient etc from the model
modelCoef <- broom::tidy(y)
modelGlance <- broom::glance(y)
#combine
modelCoef <- modelCoef %>% 
  bind_cols(modelGlance)
#to save them
pool <- paste0("/Volumes/T213/T20702/work/chrised1/Papers/Food Loss/Data/Results","_",Sys.Date(),".csv")
write.csv(modelCoef, "revResults_pool.csv")

#6. regression all commodities and stages based on different income level (TABLE 3)

model_income <- list(
  "All" = model_all <- foodLoss_comb_sel4 %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Low-income" = model_low <- foodLoss_comb_sel4 %>%
    filter(incLevel=="L") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Middle- and high-income" = model_high <- foodLoss_comb_sel4 %>%
    filter(incLevel!="L") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export + regionalName + as.factor(year), cluster =  ~ regionalName)
)

model_income2 <- modelsummary(model_income, stars=T, coef_map = c("ln_gni" = "GNI per capita (log)", "agriShr" = "Agriculture share of GDP", "agriEmp" = "Employment in agriculture",
                                                           "msch" = "Years of education", "electricityRural" = "Access to electricity in rural area", "phoneSub" = "Mobile cellular subscription",
                                                           "polStab" = "Political stability", "export" = "Export volume index"),
                             output = "flextable")

model_income2
# Create the table for the feols model
save_as_docx(model_income2, path = ("model_income.docx"))

#7 regression of different commodities based on different income level (Table 6)
model_crop <- list(
  "Wheat" = model_low_wheat <- foodLoss_comb_sel4 %>%
    filter(incLevel=="L") %>% filter(basketItem2=="wheat") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Maize" = model_low_maize <- foodLoss_comb_sel4 %>%
    filter(incLevel=="L") %>% filter(basketItem2=="maize") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Rice" = model_low_rice <- foodLoss_comb_sel4 %>%
    filter(incLevel=="L") %>% filter(basketItem2=="rice") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Other cereals & pulses" = model_low_cereals <- foodLoss_comb_sel4 %>%
    filter(incLevel=="L") %>% filter(basketItem2=="cereals") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Fruits & vegetables" = model_low_fruits <- foodLoss_comb_sel4 %>%
    filter(incLevel=="L") %>% filter(basketItem2=="fruits") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Roots, tubers, and oil crops" = model_low_roots <- foodLoss_comb_sel4 %>%
    filter(incLevel=="L") %>% filter(basketItem2=="roots") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Wheat" = model_low_wheat <- foodLoss_comb_sel4 %>%
    filter(incLevel!="L") %>% filter(basketItem2=="wheat") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Maize" = model_low_maize <- foodLoss_comb_sel4 %>%
    filter(incLevel!="L") %>% filter(basketItem2=="maize") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Rice" = model_low_rice <- foodLoss_comb_sel4 %>%
    filter(incLevel!="L") %>% filter(basketItem2=="rice") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Other cereals & pulses" = model_low_cereals <- foodLoss_comb_sel4 %>%
    filter(incLevel!="L") %>% filter(basketItem2=="cereals") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Fruits & vegetables" = model_low_fruits <- foodLoss_comb_sel4 %>%
    filter(incLevel!="L") %>% filter(basketItem2=="fruits") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Roots, tubers, and oil crops" = model_low_roots <- foodLoss_comb_sel4 %>%
    filter(incLevel!="L") %>% filter(basketItem2=="roots") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName)
)

model_crop2 <- modelsummary(model_crop, stars=T, coef_map = c("ln_gni" = "GNI per capita (log)", "agriShr" = "Agriculture share of GDP", "agriEmp" = "Employment in agriculture",
                                                                  "msch" = "Years of education", "electricityRural" = "Access to electricity in rural area", "phoneSub" = "Mobile cellular subscription",
                                                                  "polStab" = "Political stability", "export" = "Export volume index"),
                              output = "flextable")

model_crop2
# Create the table for the feols model
save_as_docx(model_crop2, path = ("model_crop2.docx"))

#8. regression of different stages based on different income level (Table 7)

model_stage <- list(
  "Farm" = model_low_farm <- foodLoss_comb_sel4 %>%
    filter(incLevel=="L") %>% filter(food_supply_stage=="Farm") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Harvest" = model_low_harvest <- foodLoss_comb_sel4 %>%
    filter(incLevel=="L") %>% filter(food_supply_stage=="Harvest") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Storage" = model_low_storage <- foodLoss_comb_sel4 %>%
    filter(incLevel=="L") %>% filter(food_supply_stage=="Storage") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Transport" = model_low_transport <- foodLoss_comb_sel4 %>%
    filter(incLevel=="L") %>% filter(food_supply_stage=="Transport") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Farm" = model_low_farm <- foodLoss_comb_sel4 %>%
    filter(incLevel!="L") %>% filter(food_supply_stage=="Farm") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Harvest" = model_low_harvest <- foodLoss_comb_sel4 %>%
    filter(incLevel!="L") %>% filter(food_supply_stage=="Harvest") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Storage" = model_low_storage <- foodLoss_comb_sel4 %>%
    filter(incLevel!="L") %>% filter(food_supply_stage=="Storage") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Transport" = model_low_transport <- foodLoss_comb_sel4 %>%
    filter(incLevel!="L") %>% filter(food_supply_stage=="Transport") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName)
 )

model_stage2 <- modelsummary(model_stage, stars=T, coef_map = c("ln_gni" = "GNI per capita (log)", "agriShr" = "Agriculture share of GDP", "agriEmp" = "Employment in agriculture",
                                                              "msch" = "Years of education", "electricityRural" = "Access to electricity in rural area", "phoneSub" = "Mobile cellular subscription",
                                                              "polStab" = "Political stability", "export" = "Export volume index"),
                            output = "flextable")

model_stage2
# Create the table for the feols model
save_as_docx(model_stage2, path = ("model_stage2.docx"))

#9. regrssion of different commodities (table 4)
model_crop3 <- list(
  "Wheat" = model_low_wheat <- foodLoss_comb_sel4 %>%
    filter(basketItem2=="wheat") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Maize" = model_low_maize <- foodLoss_comb_sel4 %>%
     filter(basketItem2=="maize") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Rice" = model_low_rice <- foodLoss_comb_sel4 %>%
    filter(basketItem2=="rice") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Other cereals & pulses" = model_low_cereals <- foodLoss_comb_sel4 %>%
    filter(basketItem2=="cereals") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Fruits & vegetables" = model_low_fruits <- foodLoss_comb_sel4 %>%
    filter(basketItem2=="fruits") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Roots, tubers, and oil crops" = model_low_fruits <- foodLoss_comb_sel4 %>%
    filter(basketItem2=="roots") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName)
  )

model_crop4 <- modelsummary(model_crop3, stars=T, coef_map = c("ln_gni" = "GNI per capita (log)", "agriShr" = "Agriculture share of GDP", "agriEmp" = "Employment in agriculture",
                                                              "msch" = "Years of education", "electricityRural" = "Access to electricity in rural area", "phoneSub" = "Mobile cellular subscription",
                                                              "polStab" = "Political stability", "export" = "Export volume index"),
                            output = "flextable")

model_crop4
# Create the table for the feols model
save_as_docx(model_crop4, path = ("model_crop4.docx"))

#10 regression of different stages (Table 5)

model_stage3 <- list(
  "Farm" = model_low_farm <- foodLoss_comb_sel4 %>%
    filter(food_supply_stage=="Farm") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Harvest" = model_low_harvest <- foodLoss_comb_sel4 %>%
    filter(food_supply_stage=="Harvest") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Storage" = model_low_storage <- foodLoss_comb_sel4 %>%
    filter(food_supply_stage=="Storage") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName),
  "Transport" = model_low_transport <- foodLoss_comb_sel4 %>%
    filter(food_supply_stage=="Transport") %>% feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export  + regionalName + as.factor(year), cluster =  ~ regionalName)
)

model_stage4 <- modelsummary(model_stage3, stars=T, coef_map = c("ln_gni" = "GNI per capita (log)", "agriShr" = "Agriculture share of GDP", "agriEmp" = "Employment in agriculture",
                                                                "msch" = "Years of education", "electricityRural" = "Access to electricity in rural area", "phoneSub" = "Mobile cellular subscription",
                                                                "polStab" = "Political stability", "export" = "Export volume index"),
                             output = "flextable")

model_stage4
# Create the table for the feols model
save_as_docx(model_stage4, path = ("model_stage4.docx"))

#Regression for heatmap materials
#11
#### simple correlation example with a loop based on crops----

# basketItems to be included
unique(foodLoss_comb_sel4$basketItem2)
incBasketItems <- c('wheat', 'rice', 'maize', 'cereals', 'fruits', 'roots')

# go through each combination
for (iBasket in 1:length(incBasketItems)) {
  
  # if you want to test for certain step, uncomment these
  #incBasket = 1
  #incChain = 1
  
  # apply filters to select wanted data
  foodLoss_comb_sel <- foodLoss_comb_sel4 %>% 
    filter(basketItem2 == incBasketItems[iBasket])
  
  # multiple linear regression between WL diffs
  model <- feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export + regionalName + as.factor(year), cluster =  ~ regionalName, data = foodLoss_comb_sel)
  # coefficients etc from the model
  revModelCoef <- broom::tidy(model)
  revModelGlance <- broom::glance(model)
  
  # combine
  revModelCoef <- revModelCoef %>% 
    bind_cols(revModelGlance)
  
  fileName <- paste0("/Volumes/chrised1/Papers/Food Loss/Data/Results",incBasketItems[iBasket],"_",Sys.Date(),".csv")
  # write to file
  write.csv( revModelCoef , fileName)
  
}

#12 #### simple correlation example with a loop based on stages----

# chain items to be included
unique(foodLoss_comb_sel4$food_supply_stage)
incChainSteps <- c('Farm', 'Storage', 'Harvest', 'Transport')

# go through each combination
for (iChain in 1:length(incChainSteps)) {
  
  # if you want to test for certain step, uncomment these
  #incBasket = 1
  #incChain = 1
  
  # apply filters to select wanted data
  foodLoss_comb_sel2 <- foodLoss_comb_sel4 %>% 
    filter(food_supply_stage == incChainSteps[iChain])
  
  # multiple linear regression between WL diffs
  model <- feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export + regionalName + as.factor(year), cluster =  ~ regionalName, data = foodLoss_comb_sel2)
  
  # coefficients etc from the model
  revModelCoef <- broom::tidy(model)
  revModelGlance <- broom::glance(model)
  
  # combine
  revModelCoef <- revModelCoef %>% 
    bind_cols(revModelGlance)
  

  fileName <- paste0("/Volumes/chrised1/Papers/Food Loss/Data/results",incChainSteps[iChain],"_",Sys.Date(),".csv")
  # write to file
  write.csv( revModelCoef , fileName)
  
}

#13 #### simple correlation example with a loop based on incLevel----

# chain items to be included
unique(foodLoss_comb_sel4$incLevel)
incIncomeSteps <- c('L', 'M')

# go through each combination
for (iIncome in 1:length(incIncomeSteps)) {
  
  # if you want to test for certain step, uncomment these
  #incBasket = 1
  #incChain = 1
  
  # apply filters to select wanted data
  foodLoss_comb_sel5 <- foodLoss_comb_sel4 %>% 
    filter(incLevel == incIncomeSteps[iIncome])
  
  # multiple linear regression between WL diffs
  model <- feols(loss_percentage1 ~ ln_gni + agriShr + agriEmp + msch + electricityRural + phoneSub + polStab + export + regionalName + as.factor(year), cluster =  ~ regionalName, data = foodLoss_comb_sel5)
  
  # coefficients etc from the model
  revModelCoef <- broom::tidy(model)
  revModelGlance <- broom::glance(model)
  
  # combine
  revModelCoef <- revModelCoef %>% 
    bind_cols(revModelGlance)
  
  fileName <- paste0("/Volumes/chrised1/Papers/Food Loss/Data/Results",incIncomeSteps[iIncome],"_",Sys.Date(),".csv")
  # write to file
  write.csv( revModelCoef , fileName)
  
}
