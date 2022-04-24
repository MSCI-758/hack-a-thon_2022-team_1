##2018
imp_2018 = read.csv("data/2018303d_final.csv") 
summary(imp_2018) 

dim(imp_2018) 
imp_2018 = imp_2018 %>%
  filter(PRIORITY.RANK != "") %>%
  filter(PRIORITY.RANK == c("1", "2", "3")) 
dim(imp_2018) 

imp_2018_use = imp_2018 %>%
  dplyr::select(`PRIORITY.RANK`, USE, HUC_12, `CAUSE.S.`)
imp_2018_use_no_multipleranks = imp_2018_use %>% 
  slice(-c(128:131))
str(imp_2018_use_no_multipleranks)
summary(imp_2018_use_no_multipleranks)

imp_2018_use_no_multipleranks1 <- tidyr::separate(data = imp_2018_use_no_multipleranks, col =  `CAUSE.S.`,
                         into = c("CAUSES" , "C1", "C2", "C3"),
                         sep = ", ", remove = TRUE, convert = FALSE, extra = "warn", fill = "warn")
imp_2018_all_cause <- pivot_longer(data = imp_2018_use_no_multipleranks1, 
                                   cols = 5:7, 
                                   values_to = imp_2018_use_no_multipleranks1$CAUSES[142])


bar_2018_rankcauseall = ggplot(imp_2018_all_cause, aes(x=CAUSES, fill= PRIORITY.RANK)) +
  geom_bar() +#stat = "identity") +
  ggtitle("Priority Rank and All Causes for 2018")+
  #facet_wrap(~USE)+
  coord_flip() +
  theme_bw() +
  theme(text=element_text(size=20))

bar_2018_rankcauseall

ggsave("figures/bar_2018_rankcauseall.png", bar_2018_rankcauseall, device="png", 
       scale=1, width = 50, height=40, units=c("cm"), dpi=300, limitsize = FALSE)


imp_2018_use_no_multipleranks$PRIORITY.RANK <- as.numeric(imp_2018_use_no_multipleranks$PRIORITY.RANK)

kruskal.test(imp_2018_use_no_multipleranks$CAUSE.S. ~ imp_2018_use_no_multipleranks$PRIORITY.RANK)
# p-value = 0.0005702

bar_2018_usecauseall = ggplot(imp_2018_all_cause, aes(x=CAUSES, fill= USE)) +
  geom_bar() +#stat = "identity") +
  ggtitle("Use and All Causes for 2018")+
  #facet_wrap(~USE)+
  coord_flip()  +
  theme_bw() +
  theme(text=element_text(size=20))

bar_2018_usecauseall

ggsave("figures/bar_2018_usecauseall.png", bar_2018_usecauseall, device="png", 
       scale=1, width = 50, height=40, units=c("cm"), dpi=300, limitsize = FALSE)

kruskal.test(imp_2018_use_no_multipleranks$CAUSE.S. ~ imp_2018_use_no_multipleranks$USE)
# p-value < 2.2e-16

##2016
library(readxl)

imp2016 <- readxl::read_excel("data/SC_303d_lists_2006to2016/PN_2016303d_final.xls")
imp2016

dim(imp2016) 
imp_2016 = imp2016 %>%
  filter(`PRIORITY RANK` != "") %>%
  filter(`PRIORITY RANK` == c("1", "2", "3")) 
dim(imp_2016) 

imp_2016_use = imp_2016 %>%
  dplyr::select(`PRIORITY RANK`, USE, HUC_12, `CAUSE(S)`)
imp_2016_use_no_multipleranks = imp_2016_use %>% 
  slice(-c(128:131))
str(imp_2016_use_no_multipleranks)
summary(imp_2016_use_no_multipleranks)

imp_2016_use_no_multipleranks1 <- tidyr::separate(data = imp_2016_use_no_multipleranks, col =  `CAUSE(S)`,
                                                  into = c("CAUSES" , "C1", "C2", "C3"),
                                                  sep = ", ", remove = TRUE, convert = FALSE, extra = "warn", fill = "warn")
imp_2016_all_cause <- pivot_longer(data = imp_2016_use_no_multipleranks1, 
                                   cols = 5:7, 
                                   values_to = imp_2016_use_no_multipleranks1$CAUSES[143])

bar_2016_rankcauseall = ggplot(imp_2016_all_cause, aes(x=CAUSES, fill= `PRIORITY RANK`)) +
  geom_bar() +#stat = "identity") +
  ggtitle("Priority Rank and All Causes for 2016")+
  #facet_wrap(~USE)+
  coord_flip() +
  theme_bw() +
  theme(text=element_text(size=20))

bar_2016_rankcauseall

ggsave("figures/bar_2016_rankcauseall.png", bar_2016_rankcauseall, device="png", 
       scale=1, width = 50, height=40, units=c("cm"), dpi=300, limitsize = FALSE)


imp_2016_use_no_multipleranks$`PRIORITY RANK` <- as.numeric(imp_2016_use_no_multipleranks$`PRIORITY RANK`)

kruskal.test(imp_2016_use_no_multipleranks$`CAUSE(S)` ~ imp_2016_use_no_multipleranks$`PRIORITY RANK`)
# p-value = 0.003951

bar_2016_usecauseall = ggplot(imp_2016_all_cause, aes(x=CAUSES, fill= USE)) +
  geom_bar() +#stat = "identity") +
  ggtitle("Use and All Causes for 2016")+
  #facet_wrap(~USE)+
  coord_flip()  +
  theme_bw() +
  theme(text=element_text(size=20))

bar_2016_usecauseall

ggsave("figures/bar_2016_usecauseall.png", bar_2016_usecauseall, device="png", 
       scale=1, width = 50, height=40, units=c("cm"), dpi=300, limitsize = FALSE)

kruskal.test(imp_2016_use_no_multipleranks$`CAUSE(S)` ~ imp_2016_use_no_multipleranks$USE)
# p-value = 9.846e-13

