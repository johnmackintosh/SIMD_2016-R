library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(viridis)
library(hrbrthemes)
library(extrafont)



#read shapefile
scot <- st_read("SG_SIMD_2016.shp")
colnames(scot) <- colnames(scot) %>% str_to_lower()


 # filter out smaller dataframe for parochial purposes
highland <- filter(scot,laname == "Highland")


# regular plot by quintile
  ggplot(highland) +
  geom_sf(aes(fill = quintile)) +
  scale_fill_viridis("quintile",option = "C",
                     guide = guide_legend(title = "Quintile")) +
  ggtitle("SIMD 2016 - Highland Council Area by Quintile",
          subtitle = "1 = most deprived, 5 = least deprived") +
  theme_ipsum(base_size = 10) +
  theme(plot.title = element_text(hjust = 0))
  ggsave("SIMD2016_quintile.png", width = 8.41, height = 5.94)
  
  
  #### overall ranking ###
  ggplot(highland)+
    geom_sf(aes(fill = rank ))+
    scale_fill_viridis_c("rank",option = "C",
                         guide = guide_legend(title = "SIMD 2016 Rank")) +
    ggtitle(label = " Overall Datazone Ranking",
            subtitle = "Highland Council Area - SIMD 2016") +
    labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
    theme_ipsum(base_size = 10)
  ggsave("highland-rank.png",width = 8.41, height = 5.94)
  
  
#number who are employment deprived
ggplot(highland) +
  geom_sf(aes(fill = empnumdep)) +
  scale_fill_viridis("empnumdep",option = "C",
                     guide = guide_legend(title = "# Employment Deprived People")) +
  ggtitle("Highland SIMD 2016 - Employment Deprived") +
  theme_ipsum(base_size = 10) +
  theme(plot.title = element_text(hjust = 0))

ggsave("SIMD2016_employment-deprived-people.png",width = 8.41, height = 5.94)
  #theme(legend.position = "bottom")

#employment deprivation percent
ggplot(highland) +
  geom_sf(aes(fill = emprate * 100)) +
  scale_fill_viridis("emprate",option = "C",
                     guide = guide_legend(title = " % Employment Deprived")) +
  ggtitle("Highland SIMD 2016 - % people employment deprived") +
  theme_ipsum(base_size = 10) +
  theme(plot.title = element_text(hjust = 0))
ggsave("SIMD2016_employment-deprived-percent.png",width = 8.41, height = 5.94)

# prescribed drugs for depression
ggplot(highland) +
  geom_sf(aes(fill = hlthdprspc *100)) +
  scale_fill_viridis("hlthdprspc",option = "A",direction = -1,
          guide = guide_legend(title = " % population"))+
  ggtitle(label = "Proportion prescribed drugs for depression",
          subtitle = "Highland Council Area - SIMD 2016 \n Includes prescribed drugs for anxiety, depression or psychosis") +
  labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
  theme_ipsum(base_size = 10)
ggsave("SIMD2016_depression-percent.png",width = 8.41, height = 5.94)


top_two <- scot %>% filter(laname %in% c("Glasgow City","City of Edinburgh"))

# top2 cities depression
ggplot(top_two) +
  geom_sf(aes(fill = hlthdprspc *100)) +
  scale_fill_viridis("hlthdprspc",option = "A", direction =-1,
                     guide = guide_legend(title = "% population"))+
  ggtitle(label = "Proportion prescribed drugs for depression : Edinburgh & Glasgow",
          subtitle = "Scotland  - SIMD 2016") +
          #labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
  labs(caption = "Proportion of population being prescribed drugs for anxiety, depression or psychosis \n Source http://www.gov.scot/Topics/Statistics/SIMD")+
  theme_ipsum(base_size = 10) +
  facet_wrap(~laname, scales="free")

 ggsave("edin-glas-depression.png",width = 8.41, height = 5.94)

 
 glasgow <- filter(top_two, laname == "Glasgow City")
 edinburgh <- filter(top_two, laname == "City of Edinburgh")
 
 
 ggplot(glasgow) +
   geom_sf(aes(fill = hlthdprspc *100)) +
   scale_fill_viridis("hlthdprspc",option = "A", direction =-1,
                      guide = guide_legend(title = "% population"))+
   ggtitle(label = "Proportion prescribed drugs for depression : Glasgow",
           subtitle = "Scotland  - SIMD 2016") +
   #labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
   labs(caption = "Proportion of population being prescribed drugs for anxiety, depression or psychosis \n Source http://www.gov.scot/Topics/Statistics/SIMD")+
   theme_ipsum(base_size = 10) 
 ggsave("glasgow_depression.png",width = 8.41, height = 5.94)
 
 ggplot(edinburgh) +
   geom_sf(aes(fill = hlthdprspc *100)) +
   scale_fill_viridis("hlthdprspc",option = "A", direction =-1,
                      guide = guide_legend(title = "% population"))+
   ggtitle(label = "Proportion prescribed drugs for depression : Edinburgh",
           subtitle = "Scotland  - SIMD 2016") +
   #labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
   labs(caption = "Proportion of population being prescribed drugs for anxiety, depression or psychosis \n Source http://www.gov.scot/Topics/Statistics/SIMD")+
   theme_ipsum(base_size = 10) 
 ggsave("edinburgh_depression.png",width = 8.41, height = 5.94)
 
 
aberdeen <- scot %>% filter(laname %in% c("Aberdeen City","Aberdeenshire"))
ggplot(aberdeen) +
  geom_sf(aes(fill = hlthdprspc *100)) +
  scale_fill_viridis("hlthdprspc",option = "A", direction =-1,
                     guide = guide_legend(title = "% population"))+
  ggtitle(label = "Proportion prescribed drugs for depression : Aberdeen",
          subtitle = "Scotland  - SIMD 2016") +
  #labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
  labs(caption = "Proportion of population being prescribed drugs for anxiety, depression or psychosis \n Source http://www.gov.scot/Topics/Statistics/SIMD")+
  theme_ipsum(base_size = 10) 
ggsave("aber-deen-pression.png",width = 8.41, height = 5.94)

#dundee
dundee <- scot %>% filter(laname %in% c("Dundee City"))
ggplot(dundee) +
  geom_sf(aes(fill = hlthdprspc *100)) +
  scale_fill_viridis("hlthdprspc",option = "A", direction =-1,
                     guide = guide_legend(title = "% population"))+
  ggtitle(label = "Proportion prescribed drugs for depression : Dundee",
          subtitle = "Scotland  - SIMD 2016") +
  #labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
  labs(caption = "Proportion of population being prescribed drugs for anxiety, depression or psychosis \n Source http://www.gov.scot/Topics/Statistics/SIMD")+
  theme_ipsum(base_size = 10) 
ggsave("dun-depression.png",width = 8.41, height = 5.94)




# percent no central heating
ggplot(highland) +
  geom_sf(aes(fill = housencrat *100)) +
  scale_fill_viridis("housencrat",option = "C",
  guide = guide_legend(title = "Proportion of population")) +
  ggtitle(label = "Percentage of people in households without central heating",
          subtitle = "Highland Council Area - SIMD 2016") +
  labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
  theme_ipsum(base_size = 10)
ggsave("highland-central-heating.png")


drive <- select(highland,1:9,starts_with("gac"))
#petrol station
ggplot(drive)+
  geom_sf(aes(fill = gaccpetrol)) +
  scale_fill_viridis("gaccpetrol",option = "C",
                     guide = guide_legend(title = "Avg Drive Time (Mins)")) +
  ggtitle(label = "Average Drive Time to Petrol Station (Mins)",
          subtitle = "Highland Council Area - SIMD 2016") +
  labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
  theme_ipsum(base_size = 10)
ggsave("highland-drive-time-petrol.png",width = 8.41, height = 5.94)

# gp 
ggplot(drive)+
  geom_sf(aes(fill = gaccdtgp)) +
  scale_fill_viridis("gaccdtgp",option = "C",
                     guide = guide_legend(title = "Avg Drive Time (Mins)")) +
  ggtitle(label = "Average Drive Time to GP (Mins)",
          subtitle = "Highland Council Area - SIMD 2016") +
  labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
  theme_ipsum(base_size = 10)
ggsave("highland-drive-time-gp.png",width = 8.41, height = 5.94)

##retail park
ggplot(drive)+
  geom_sf(aes(fill = gaccdtret)) +
  scale_fill_viridis("gaccdtret",option = "C",
                     guide = guide_legend(title = "Avg Drive Time (Mins)")) +
  ggtitle(label = "Average Drive Time to Retail Centre (Mins)",
          subtitle = "Highland Council Area - SIMD 2016") +
  labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
  theme_ipsum(base_size = 10)
ggsave("highland-drive-time-retail.png",width = 8.41, height = 5.94)

##secondary school
ggplot(drive)+
  geom_sf(aes(fill = gaccdtssch)) +
  scale_fill_viridis("gaccdtssch",option = "C",
                     guide = guide_legend(title = "Avg Drive Time (Mins)")) +
  ggtitle(label = "Average Drive Time to Secondary School (Mins)",
          subtitle = "Highland Council Area - SIMD 2016") +
  labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
  theme_ipsum(base_size = 10)
ggsave("highland-drive-time-secschool.png",width = 8.41, height = 5.94)

##spublic transport to gp
ggplot(drive)+
  geom_sf(aes(fill = gaccptgp)) +
  scale_fill_viridis("gaccptgp",option = "C",
                     guide = guide_legend(title = "Pubic Tansport Travel Time(Mins)")) +
  ggtitle(label = "Public transport travel time to a GP surgery (Mins)",
          subtitle = "Highland Council Area - SIMD 2016") +
  labs(caption = " data from http://www.gov.scot/Topics/Statistics/SIMD") +
  theme_ipsum(base_size = 10)
ggsave("highland-pubtrans-time-gp.png",width = 8.41, height = 5.94)





