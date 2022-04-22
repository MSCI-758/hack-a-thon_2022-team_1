## Amadi##

### Loading needed libraries

#Libraries
library(tidyverse)
library(raster)
library(mapdata)  
library(marmap)
library(sf)
library(readr)
library(readxl)


###  Reading in csv for 2018 and 2016 


#using read_csv removes dots from column names
imp_2018 = read_csv("data/2018303d_final.csv") # SC Impaired Waters List 303d for 2018
#imp_2016 = read_csv("data/SC_303d_lists_2006to2016/PN_2016303d_final.csv")
############
imp_2016 = read_excel("data/SC_303d_lists_2006to2016/PN_2016303d_final.xls")
##    Could read the excel file instead
###########

#summary(imp_2018) 
#glimpse(imp_2018)
#summary(imp_2016)
#glimpse(imp_2016)


### Getting rid of empty rows in 2018 priority rank


##2018
dim(imp_2018) #1979 rows
imp_2018 = imp_2018 %>%
  filter(`PRIORITY RANK` != "")#%>%
#filter(`PRIORITY RANK` == "1")#
dim(imp_2018) # 1979-861 = 1118 rows
#glimpse(imp_2018)
##2016
#Getting rid of empty rows in 2016 priority rank
dim(imp2016) #1503 rows
imp_2016 = imp2016 %>%
  filter(`PRIORITY RANK` != "") 
dim(imp_2016) # 1979-535 = 1038 rows 
#glimpse(imp_2016)


### 2018


#Selecting needed columns
imp_2018_use = imp_2018 %>%
  dplyr::select(`PRIORITY RANK`, USE, HUC_12, `CAUSE(S)`)
#glimpse(imp_2018_use)
#summary(imp_2018_use)


### Bar plots for priority rank, use and cause - 2018


#excluding the 4 rows with multiple ranks
imp_2018_use_no_multipleranks = imp_2018_use %>%   dplyr::select(`PRIORITY RANK`, USE, HUC_12, `CAUSE(S)`)


### Bar plot for use and priority rank - 2018


bar_2018_rankuse = ggplot(imp_2018_use_no_multipleranks, aes(x=USE, fill= `PRIORITY RANK`)) +
  geom_bar() + #stat = "identity")
  ggtitle("Plot of Priority Rank and Use for 2018") + 
  theme_bw() +
  theme(text=element_text(size=20))
bar_2018_rankuse
ggsave("figures/bar_2018_rankuse.png", bar_2018_rankuse, device="png", 
       scale=1, width = 30, height=25, units=c("cm"), dpi=300, limitsize = FALSE)


### Bar plot for priority rank and cause - 2018


bar_2018_rankcause = ggplot(imp_2018_use_no_multipleranks, aes(x=`CAUSE(S)`, fill= `PRIORITY RANK`)) +
  geom_bar() +#stat = "identity") +
  ggtitle("Plot of Priority Rank and Cause for 2018")+
  #facet_wrap(~USE)+
  coord_flip() +
  theme_bw() +
  theme(text=element_text(size=20))
bar_2018_rankcause
ggsave("figures/bar_2018_rankcause.png", bar_2018_rankcause, device="png", 
       scale=1, width = 50, height=40, units=c("cm"), dpi=300, limitsize = FALSE)


### Bar plot for use and cause - 2018


bar_2018_usecause = ggplot(imp_2018_use_no_multipleranks, aes(x=`CAUSE(S)`, fill= USE)) +
  geom_bar() +#stat = "identity") +
  ggtitle("Plot of Use and Cause for 2018")+
  #facet_wrap(~USE)+
  coord_flip()  +
  theme_bw() +
  theme(text=element_text(size=20))
bar_2018_usecause
ggsave("figures/bar_2018_usecause.png", bar_2018_usecause, device="png", 
       scale=1, width = 50, height=40, units=c("cm"), dpi=300, limitsize = FALSE)


## 2016 


#Selecting needed columns
imp_2016_use = imp_2016 %>%
  dplyr::select(`PRIORITY RANK`, USE, HUC_12, `CAUSE(S)`) 
#glimpse(imp_2018_use)
#summary(imp_2018_use)


### Bar plots for priority rank, use and cause - 2016


#excluding the 4 rows with multiple ranks
imp_2016_use_no_multipleranks = imp_2016_use %>%
  slice(-c(115,129,137,138,287,288,563,952))


### Bar plot for use and priority rank - 2016


bar_2016_rankuse = ggplot(imp_2016_use_no_multipleranks, aes(x=USE, fill= `PRIORITY RANK`)) +
  geom_bar() + #stat = "identity")
  ggtitle("Plot of Priority Rank and Use for 2016") +
  theme_bw() +
  theme(text=element_text(size=20))
bar_2016_rankuse
ggsave("figures/bar_2016_rankuse.png", bar_2016_rankuse, device="png", 
       scale=1, width = 30, height=25, units=c("cm"), dpi=300, limitsize = FALSE)


### Bar plot for priority rank and cause - 2016


bar_2016_rankcause = ggplot(imp_2016_use_no_multipleranks, aes(x=`CAUSE(S)`, fill= `PRIORITY RANK`)) +
  geom_bar() +
  ggtitle("Plot of Priority Rank and Cause for 2016")+
  #facet_wrap(~USE)+
  coord_flip() +
  theme_bw() +
  theme(text=element_text(size=20))
bar_2016_rankcause
ggsave("figures/bar_2016_rankcause.png", bar_2016_rankcause, device="png", 
       scale=1, width = 50, height=40, units=c("cm"), dpi=300, limitsize = FALSE)


### Bar plot for use and cause - 2016


bar_2016_usecause = ggplot(imp_2016_use_no_multipleranks, aes(x=`CAUSE(S)`, fill= USE)) +
  geom_bar() +
  ggtitle("Plot of Use and Cause for 2016")+
  #facet_wrap(~USE)+
  coord_flip() +
  theme_bw() +
  theme(text=element_text(size=20))
bar_2016_usecause
ggsave("figures/bar_2018_usecause.png", bar_2018_usecause, device="png", 
       scale=1, width = 50, height=40, units=c("cm"), dpi=300, limitsize = FALSE)

