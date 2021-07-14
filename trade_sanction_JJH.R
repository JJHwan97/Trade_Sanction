library(dplyr)
library(tidyverse)
library(countrycode)
library(readr)
library(readxl)

data1_clean <- read_csv("E:/aa/data1_clean.csv")

import.full <- data1_clean[,1:9]

remove(data1_clean)

country <- read_excel("E:/Economic Sanctions/UN Comtrade Country List.xls")[,c(1:2)]

country[country[,1] == 384,2] <- "Cote d'Ivoire"
country[country[,1] == 531,2] <- "Curacao"

import.full[import.full[,8] == "C척te d'Ivoire",8] <- "Cote d'Ivoire"
import.full[import.full[,9] == "C척te d'Ivoire",9] <- "Cote d'Ivoire"

import.full[import.full[,8] == "Cura챌ao",8] <- "Curacao"
import.full[import.full[,9] == "Cura챌ao",9] <- "Curacao"

colnames(country) <- c("PartnerISO",import.full %>% colnames() %>% .[8])

import.full <- left_join(import.full, country)

colnames(country) <- c("ReporterISO",import.full %>% colnames() %>% .[9])

import.full <- left_join(import.full, country)

import.full <- import.full %>% filter(Trade.Value..US.. != 0)

import.full$weight <- 100/log(import.full$Trade.Value..US..+1)  

import.full <- import.full[,c(1,10:12)]

import.full <- import.full %>% drop_na()

import.full[import.full[,3] == 841,3] <- 840
import.full[import.full[,2] == 841,2] <- 840

import.full[import.full[,3] == 842,3] <- 840
import.full[import.full[,2] == 842,2] <- 840

import.full[import.full[,3] == 699,3] <- 356
import.full[import.full[,2] == 699,2] <- 356

import.full[import.full[,3] == 568,3] <- 492
import.full[import.full[,2] == 568,2] <- 492

import.full[import.full[,3] == 810,3] <- 643
import.full[import.full[,2] == 810,2] <- 643


library(igraph)

for (i in import.full$Year %>% unique()){
  temp <- import.full %>% filter(Year == i)
  temp <- drop_na(temp)
  temp <- temp[,2:4]
  # assign(paste0("import",i), temp)
  temp <- graph_from_data_frame(temp,directed = TRUE)
  constraint.temp <- constraint(temp)
  # assign(paste0("import",i,"graph"), temp)
  constraint.temp <- constraint.temp %>% as.data.frame()
  colnames(constraint.temp) <- i
  constraint.temp <- rownames_to_column(constraint.temp, var = "ISOcountry")
  assign(paste0("constraint","import",i), constraint.temp)
}

for (i in import.full$Year %>% unique()){
  temp <- get(paste0("constraint","import",i))
  if (i == 1962){
    final.constraint <- temp
  }else{
    final.constraint <- full_join(final.constraint , temp)
  }
}

final.constraint <- final.constraint %>% as.data.frame()

final.constraint <- final.constraint %>% 
  pivot_longer(!ISOcountry, names_to = "year", values_to = "constraint")

final.constraint <- final.constraint %>% as.data.frame()
final.constraint[,1] <- final.constraint[,1] %>% as.numeric()
final.constraint[,2] <- final.constraint[,2] %>% as.numeric()

rm(temp)

url <- "https://sanctions.web.unc.edu/wp-content/uploads/sites/18834/2021/04/TIESv4-1.xls"
ties <- rio::import(file = url,which = 1) %>% 
  glimpse()

url <- "https://mgmt.wharton.upenn.edu/wp-content/uploads/2017/02/POLCON_2017.xlsx"
veto <- rio::import(file = url,which = 1) %>% 
  glimpse()

url <- "http://www.systemicpeace.org/inscr/p5v2018.xls"
dem5 <- rio::import(file = url,which = 1) %>% 
  glimpse()

dem5 <- dem5 %>% dplyr::select(ccode, year, polity)
dem5$ISOcountry <- dem5$ccode %>% countrycode(., origin = 'p4n', destination = 'iso3n')
dem5 <- dem5 %>% select(!c(ccode))

library(wbstats)
GDP<- wb_data(indicator = "NY.GDP.MKTP.CD")

cinc <- read_csv("E:/Economic Sanctions/NMC_5_0/NMC_5_0.csv")
cinc <- cinc[,c(2,3,10)]

veto <- veto[,c(2,7,8)]

GDP$ISOcountry <- GDP$iso3c %>% countrycode(., origin = 'iso3c', destination = 'iso3n')
GDP <- GDP[,c(4,5,10)]
colnames(GDP)[1] <- 'year'
colnames(GDP)[2] <- 'GDP'

GDP <- drop_na(GDP)
cinc <- drop_na(cinc)
veto <- drop_na(veto)
dem5 <- drop_na(dem5)

ties <- ties %>% filter(startyear > 1962)

ties <- ties %>% dplyr::select(caseid, startyear, startmonth, startday, endyear, sender1, institution, targetstate, 
                        targetinstitution, threat, sanctiontypethreat, othersanctiontypethreatened, 
                        sanctiontype, imposition, sancimpositionstartyear, sancimpositionstartmonth, sancimpositionstartday,
                        sanctiontype, othersanctiontype, finaloutcome)
temp1 <- ties$sender1
temp2 <- ties$sender1 %>% countrycode(., origin = 'cown', destination = 'iso3n')
temp2[temp1 == 260] <- 280
temp2[temp1 == 265] <- 278
temp2[temp1 == 315] <- 200
temp2[temp1 == 345] <- 890
temp2[temp1 == 1000] <- 492

ties$sender1ISO <- temp2

temp1 <- ties$targetstate
temp2 <- ties$targetstate %>% countrycode(., origin = 'cown', destination = 'iso3n')
temp2[temp1 == 260] <- 280
temp2[temp1 == 265] <- 278
temp2[temp1 == 315] <- 200
temp2[temp1 == 345] <- 890
temp2[temp1 == 1000] <- 492
temp2[temp1 == 680] <- 720
temp2[temp1 == 817] <- 868

ties$targetstateISO <- temp2

ties$sancimpositionstartyear <- ties$sancimpositionstartyear %>% as.numeric()

ties$year <- ties$startyear -1
colnames(ties)[colnames(ties)=="sender1ISO"] <- "ISOcountry"
colnames(ties)[colnames(ties)=="sender1"] <- "ccode"

ties <- left_join(ties, final.constraint)
ties <- left_join(ties, veto)
ties <- left_join(ties, cinc)
ties <- left_join(ties, GDP)
ties <- left_join(ties, dem5)

colnames(ties)[colnames(ties)=="ISOcountry"] <- "sender1ISO"
colnames(ties)[colnames(ties)=="ccode"] <- "sender1"
colnames(ties)[colnames(ties)=="constraint"] <- "senderconstraint"
colnames(ties)[colnames(ties)=="polconiii"] <- "senderveto"
colnames(ties)[colnames(ties)=="cinc"] <- "sendercinc"
colnames(ties)[colnames(ties)=="GDP"] <- "sendergdp"
colnames(ties)[colnames(ties)=="polity"] <- "senderpolity"


colnames(ties)[colnames(ties)=="targetstateISO"] <- "ISOcountry"
colnames(ties)[colnames(ties)=="targetstate"] <- "ccode"

ties <- left_join(ties, final.constraint)
ties <- left_join(ties, veto)
ties <- left_join(ties, cinc)
ties <- left_join(ties, GDP)
ties <- left_join(ties, dem5)

colnames(ties)[colnames(ties)=="ISOcountry"] <- "targetstateISO"
colnames(ties)[colnames(ties)=="ccode"] <- "targetstate"
colnames(ties)[colnames(ties)=="constraint"] <- "targetconstraint"
colnames(ties)[colnames(ties)=="polconiii"] <- "targetveto"
colnames(ties)[colnames(ties)=="cinc"] <- "targetcinc"
colnames(ties)[colnames(ties)=="GDP"] <- "targetgdp"
colnames(ties)[colnames(ties)=="polity"] <- "targetpolity"

ties <- ties %>% dplyr::select(!year)

ties$year <- ties$sancimpositionstartyear -1
colnames(ties)[colnames(ties)=="sender1ISO"] <- "ISOcountry"
colnames(ties)[colnames(ties)=="sender1"] <- "ccode"

ties <- left_join(ties, final.constraint)
ties <- left_join(ties, veto)
ties <- left_join(ties, cinc)
ties <- left_join(ties, GDP)
ties <- left_join(ties, dem5)

colnames(ties)[colnames(ties)=="ISOcountry"] <- "sender1ISO"
colnames(ties)[colnames(ties)=="ccode"] <- "sender1"
colnames(ties)[colnames(ties)=="constraint"] <- "senderconstraint_imposition"
colnames(ties)[colnames(ties)=="polconiii"] <- "senderveto_imposition"
colnames(ties)[colnames(ties)=="cinc"] <- "sendercinc_imposition"
colnames(ties)[colnames(ties)=="GDP"] <- "sendergdp_imposition"
colnames(ties)[colnames(ties)=="polity"] <- "senderpolity_imposition"

colnames(ties)[colnames(ties)=="targetstateISO"] <- "ISOcountry"
colnames(ties)[colnames(ties)=="targetstate"] <- "ccode"

ties <- left_join(ties, final.constraint)
ties <- left_join(ties, veto)
ties <- left_join(ties, cinc)
ties <- left_join(ties, GDP)
ties <- left_join(ties, dem5)

colnames(ties)[colnames(ties)=="ISOcountry"] <- "targetstateISO"
colnames(ties)[colnames(ties)=="ccode"] <- "targetstate"
colnames(ties)[colnames(ties)=="constraint"] <- "targetconstraint_imposition"
colnames(ties)[colnames(ties)=="polconiii"] <- "targetveto_imposition"
colnames(ties)[colnames(ties)=="cinc"] <- "targetcinc_imposition"
colnames(ties)[colnames(ties)=="GDP"] <- "targetgdp_imposition"
colnames(ties)[colnames(ties)=="polity"] <- "targetpolity_imposition"

ties <- ties %>% dplyr::select(!year)

alliance <- read_csv("E:/Economic Sanctions/alliance/alliance_v4.1_by_directed_yearly.csv")
alliance <- alliance[,c(2,4,14,18)]
alliance_sum <- alliance %>% 
  group_by(ccode1, ccode2, year) %>%
  summarize(defense_sum = sum(defense))%>%
  filter(defense_sum != 0)
alliance_sum$alliance <- 1
alliance_sum<- alliance_sum %>% dplyr::select(!defense_sum)

colnames(ties)[colnames(ties)=="sender1"] <- "ccode1"
colnames(ties)[colnames(ties)=="targetstate"] <- "ccode2"
ties$year <- ties$startyear

ties <- left_join(ties, alliance_sum)

ties[is.na(ties$alliance),"alliance"] <- 0

ties <- ties %>% dplyr::select(!year)

ties$year <- ties$sancimpositionstartyear -1

ties <- left_join(ties, alliance_sum)

ties[is.na(ties$alliance),"alliance"] <- 0

colnames(ties)[colnames(ties)=="aliance"] <- "aliance_imposition"

colnames(ties)[colnames(ties)=="ccode1"] <- "sender1"
colnames(ties)[colnames(ties)=="ccode2"] <- "targetstate"
ties[is.na(ties$aliance_imposition),"aliance_imposition"] <- 0

ties <- ties %>% dplyr::select(!year)

data1_clean <- read_csv("E:/aa/data1_clean.csv")

import.full <- data1_clean[,1:9]

remove(data1_clean)

import.full[import.full[,8] == "C척te d'Ivoire",8] <- "Cote d'Ivoire"
import.full[import.full[,9] == "C척te d'Ivoire",9] <- "Cote d'Ivoire"

import.full[import.full[,8] == "Cura챌ao",8] <- "Curacao"
import.full[import.full[,9] == "Cura챌ao",9] <- "Curacao"

colnames(country) <- c("PartnerISO",import.full %>% colnames() %>% .[8])

import.full <- left_join(import.full, country)

colnames(country) <- c("ReporterISO",import.full %>% colnames() %>% .[9])

import.full <- left_join(import.full, country)

import.full <- import.full %>% filter(Trade.Value..US.. != 0)

import.full$weight <- 100/log(import.full$Trade.Value..US..+1)  

import.full <- import.full[,c(1,10,11,6)]

import.full <- import.full %>% drop_na()

import.full[import.full[,3] == 841,3] <- 840
import.full[import.full[,2] == 841,2] <- 840

import.full[import.full[,3] == 842,3] <- 840
import.full[import.full[,2] == 842,2] <- 840

import.full[import.full[,3] == 699,3] <- 356
import.full[import.full[,2] == 699,2] <- 356

import.full[import.full[,3] == 568,3] <- 492
import.full[import.full[,2] == 568,2] <- 492

import.full[import.full[,3] == 810,3] <- 643
import.full[import.full[,2] == 810,2] <- 643

colnames(import.full)[4]<-"trade"

import.full.summarize <- import.full %>% 
  group_by(PartnerISO, ReporterISO, Year) %>%
  summarize(trade_sum = sum(trade))

import.full.summarize.country <- import.full %>% 
  group_by(ReporterISO, Year) %>%
  summarize(trade_sum = sum(trade))

colnames(import.full.summarize.country)[3] <- "trade_sum_full"

trade.portion <- left_join(import.full.summarize, import.full.summarize.country)
trade.portion$portion <- trade.portion$trade_sum/trade.portion$trade_sum_full
colnames(trade.portion)[3]<-"year"
trade.portion <- trade.portion[,c(1,2,3,6)]

#Target Data
colnames(ties)[colnames(ties)=="sender1ISO"] <- "PartnerISO"
colnames(ties)[colnames(ties)=="targetstateISO"] <- "ReporterISO"
ties$year <- ties$startyear -1

ties<-left_join(ties, trade.portion)

colnames(ties)[colnames(ties)=="PartnerISO"] <- "sender1ISO"
colnames(ties)[colnames(ties)=="ReporterISO"] <- "targetstateISO"
ties[is.na(ties$portion),"portion"] <- 0
colnames(ties)[colnames(ties)=="portion"] <- "sender/target"

ties <- ties %>% dplyr::select(!year)

#Sender Data
colnames(ties)[colnames(ties)=="sender1ISO"] <- "ReporterISO"
colnames(ties)[colnames(ties)=="targetstateISO"] <- "PartnerISO"

ties$year <- ties$startyear -1
ties<-left_join(ties, trade.portion)

colnames(ties)[colnames(ties)=="ReporterISO"] <- "sender1ISO"
colnames(ties)[colnames(ties)=="PartnerISO"] <- "targetstateISO"
ties[is.na(ties$portion),"portion"] <- 0
colnames(ties)[colnames(ties)=="portion"] <- "target/sender"
ties <- ties %>% dplyr::select(!year)

#Target Data
colnames(ties)[colnames(ties)=="sender1ISO"] <- "PartnerISO"
colnames(ties)[colnames(ties)=="targetstateISO"] <- "ReporterISO"
ties$year <- ties$sancimpositionstartyear -1

ties<-left_join(ties, trade.portion)

colnames(ties)[colnames(ties)=="PartnerISO"] <- "sender1ISO"
colnames(ties)[colnames(ties)=="ReporterISO"] <- "targetstateISO"
ties[is.na(ties$portion),"portion"] <- 0
colnames(ties)[colnames(ties)=="portion"] <- "sender/target_imposition"
ties <- ties %>% dplyr::select(!year)

#Sender Data
colnames(ties)[colnames(ties)=="sender1ISO"] <- "ReporterISO"
colnames(ties)[colnames(ties)=="targetstateISO"] <- "PartnerISO"

ties$year <- ties$sancimpositionstartyear -1

ties<-left_join(ties, trade.portion)

colnames(ties)[colnames(ties)=="ReporterISO"] <- "sender1ISO"
colnames(ties)[colnames(ties)=="PartnerISO"] <- "targetstateISO"
ties[is.na(ties$portion),"portion"] <- 0
colnames(ties)[colnames(ties)=="portion"] <- "target/sender_imposition"
ties <- ties %>% dplyr::select(!year)

##war
war <- read_csv("E:/Economic Sanctions/dyadic_mid_4.01/directed_dyadic_war.csv")
war <- war[,c(4,5,12)]
war$mid <- 1

colnames(ties)[colnames(ties)=="sender1ISO"] <- "statea"
colnames(ties)[colnames(ties)=="targetstateISO"] <- "stateb"
ties$year <- ties$startyear

ties <- left_join(ties, war)

colnames(ties)[colnames(ties)=="statea"] <- "sender1ISO"
colnames(ties)[colnames(ties)=="stateb"] <- "targetstateISO"
ties[is.na(ties$mid),"mid"] <- 0
colnames(ties)[colnames(ties)=="mid"] <- "mid_start"
ties <- ties %>% dplyr::select(!year)

colnames(ties)[colnames(ties)=="sender1"] <- "statea"
colnames(ties)[colnames(ties)=="targetstate"] <- "stateb"
ties$year <- ties$sancimpositionstartyear

ties <- left_join(ties, war)

colnames(ties)[colnames(ties)=="statea"] <- "sender1"
colnames(ties)[colnames(ties)=="stateb"] <- "targetstate"
ties[is.na(ties$mid),"mid"] <- 0
colnames(ties)[colnames(ties)=="mid"] <- "mid_imposition"
ties <- ties %>% dplyr::select(!year)

sanctiontype <- read_csv("E:/Economic Sanctions/sanctiontype.csv")
sanctiontype <- sanctiontype$trade %>% as.data.frame
colnames(sanctiontype) <- "impositiontype"
sanctiontype_threat <- read_csv("E:/Economic Sanctions/sanctiontype_threat.csv")
sanctiontype_threat <- sanctiontype_threat$threattype %>% as.data.frame
colnames(sanctiontype_threat) <- "threattype"

ties<- cbind(ties, sanctiontype)

ties<-cbind(ties, sanctiontype_threat)

ties$datestart <- paste0(ties$startyear, "-", ties$startmonth , "-" ,ties$startday) %>% as.Date()
# 
# ties$yearsince <- format(as.Date(ties$datestart, format="%Y/-%m/-%d"),"%Y") %>% as.numeric
# 
# ties$sancimpositionstartyear <- ties$sancimpositionstartyear %>% as.numeric()
# ties$sancimpositionstartday <- ties$sancimpositionstartday %>% as.numeric()

ties$date_imposition <- paste0(ties$sancimpositionstartyear,"-",ties$sancimpositionstartmonth,"-",ties$sancimpositionstartday)
# ties$date_imposition <- format(as.Date(ties$date_imposition, format="%Y/-%m/-%d"),"%Y")

ties <- arrange(ties, sancimpositionstartyear)

ties$date_imposition  <- ties$date_imposition  %>% as.Date()

ties <- ties %>% drop_na(sender1)

for (i in 1 : length(unique(ties$sender1))){
  j <- unique(ties$sender1)[i]
  temp2 <- ties %>% filter(sender1 == j)
  allyear <- temp2$date_imposition
  temp2$lastimposition <- NA
  for (k in 1:nrow(temp2)){
    sample <- (allyear - temp2$datestart[k]) %>% as.numeric() %>% as.data.frame() 
    sample_days <- sample[sample<0]
    if(which.min(sample_days) %>% length() != 0){
      temp2$lastimposition[k] <- (-sample_days[which.max(sample_days)]) %>% min()
    }else{
      next
    }}
  if (i ==1){
    a <- temp2
  }else{
    a <- rbind(a,temp2)
  }}

a$sancimpositionstartyear <- a$sancimpositionstartyear %>% as.numeric()

ties <- left_join(ties, a)  
 
# ties$timesince <- ties$yearsince - ties$lastimposition

write.csv(ties, "E:/Economic Sanctions/ties.csv")

top <- ties[order(ties$senderconstraint),] %>%
  .[,c("startyear","sender1","senderconstraint","targetstate","targetconstraint")]

top$sendername <- top$sender1 %>% countrycode(., origin = 'cown', destination = 'country.name')
top$targetname <- top$targetstate %>% countrycode(., origin = 'cown', destination = 'country.name')

# write.csv(top, "E:/Economic Sanctions/top.csv")

i = 1989
temp <- import.full %>% filter(Year == i)
temp <- drop_na(temp)
temp <- temp[,2:4]

temp_graph <- graph_from_data_frame(temp,directed = TRUE)


temp_forbind <- temp[,c(2,1,3)]

temp_dyad <- rbind(temp, temp_forbind)

colnames(temp_dyad)[1:2] <- c("V1","V2")

temp_1989 <- make_ego_graph(temp_graph, order = 1, nodes = "840", mode = c("all"), mindist = 0)
temp_1989 <- temp_1989[[1]] %>% as_edgelist() %>% as.data.frame()
temp_1989[,1] <- temp_1989[,1] %>% as.numeric()
temp_1989[,2] <- temp_1989[,2] %>% as.numeric()

temp_1989_840 <- left_join(temp_1989, temp_dyad)

plot_ego_1989 <- graph_from_data_frame(temp_1989_840)

plot(plot_ego_1989, layout= layout_with_fr, 
     edge.width=rank(-E(plot_ego_1989)$weight)*0.00005, edge.arrow.size= E(plot_ego_1989)$weight * 0.005)

i = 1990
temp <- import.full %>% filter(Year == i)
temp <- drop_na(temp)
temp <- temp[,2:4]

temp_graph <- graph_from_data_frame(temp,directed = TRUE)

temp_forbind <- temp[,c(2,1,3)]

temp_dyad <- rbind(temp, temp_forbind)

colnames(temp_dyad)[1:2] <- c("V1","V2")

temp_1990 <- make_ego_graph(temp_graph, order = 2, nodes = "840", mode = c("all"), mindist = 0)
temp_1990 <- temp_1990[[1]] %>% as_edgelist() %>% as.data.frame()
temp_1990[,1] <- temp_1990[,1] %>% as.numeric()
temp_1990[,2] <- temp_1990[,2] %>% as.numeric()

temp_1990_840 <- left_join(temp_1990, temp_dyad)

##

temp_1990_840 <- temp_1990_840[rank(temp_1990_840$weight) < 500,]

temp_1989_840 <- temp_1989_840[rank(temp_1989_840$weight) < 500,]

plot_ego_1989 <- graph_from_data_frame(temp_1989_840)

plot_ego_1990 <- graph_from_data_frame(temp_1990_840)

plot(plot_ego_1989, vertex.size=VComponent_Size1989, layout= layout_with_kk, vertex.color = VComponent1989,
     edge.width=rank(-E(plot_ego_1989)$weight)*0.008, edge.arrow.size= E(plot_ego_1989)$weight * 0.005, edge.curved=.1)

plot(plot_ego_1990, vertex.size=VComponent_Size1990, layout= layout_with_kk, vertex.color = VComponent1990,
     edge.width= rank(E(plot_ego_1990)$weight)*0.008, edge.arrow.size= E(plot_ego_1990)$weight * 0.005, edge.curved=.1)

colrs <- c("tomato")

VComponent1989 = rep("White", length(V(plot_ego_1989)))
VComponent1989[c(8,5)] <- "cyan2" 
VComponent1989[21] <- "tomato" 
# Set node size based on audience size:
VComponent_Size1989 = rep(2, length(V(plot_ego_1989)))
VComponent_Size1989[21] <- 15
VComponent_Size1989[c(5,8)] <- 10

VComponent1990 = rep("White", length(V(plot_ego_1990)))
VComponent1990[c(1,2)] <- "cyan2" 
VComponent1990[5] <- "tomato" 
# Set node size based on audience size:
VComponent_Size1990 = rep(2, length(V(plot_ego_1990)))
VComponent_Size1990[5] <- 15
VComponent_Size1990[c(1,2)] <- 10

Vcomponent_Lable1990 = V(plot_ego_1990) %>% as.matrix()
Vcomponent_Lable1990[5] <- "USA"

