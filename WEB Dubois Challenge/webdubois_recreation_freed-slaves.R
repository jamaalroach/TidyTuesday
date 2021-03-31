library(tidyverse)
library(extrafont)



###### Get Data #########

freed_slaves <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv'
  )

###### convert  data to long format ######

freed_slaves <-
  freed_slaves %>% pivot_longer(-Year, "Status", values_to =  "Percentage")


###### amend data for year 1800 to total to 100% ########

freed_slaves[3,3]<-89 #change percentage so that the total 100%.


###### create plot #####

plot<-ggplot(freed_slaves, aes(x=Year,y=Percentage, group=Status, fill=Status))+
  geom_area()+
  geom_text(data=freed_slaves%>%filter(Status=="Free" & Year!=1870), aes(x=Year, y=100-Percentage, label=scales::percent(Percentage/100)), vjust=-.5, size=4.5)+
  geom_text(data=freed_slaves%>%filter(Status=="Free" & Year==1870), aes(x=Year, y=Percentage-10, label=scales::percent(Percentage/100)), vjust=-.5, size=4.5)+
  geom_text(aes(x=1830, y=50, label="SLAVES \n ESCLAVES"), colour="#EDE0D0", size=10)+
  geom_text(aes(x=1830, y=96.5, label="FREE - LIBRE"), size=6)+
  geom_linerange(data = freed_slaves%>%filter(Status=="Slave" & Year!=1870 & Year!=1790), aes(ymax=100, ymin=Percentage+1.75), colour="#101010", size=0.1)+
  labs(title="PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES \n\n 
       PROPORTION DES NEGRES LIBRES ET DES ESCLAVES EN AMERIQUE\n\n", 
       subtitle = "DONE BY ATLANTA UNIVERSITY \n\n",
       caption =" Source: Starks, Hillery & Tyler - W.E.B Du Bois Challenge | Created by @jamaalroach " )+
  scale_x_continuous(position = "top", breaks = freed_slaves$Year)+
  scale_fill_manual(values = c("#4e996f", "black"))+
  scale_y_continuous(limits =c(0,100), expand = c(0,0))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face="bold", size=14, colour="black"),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=.5, face = "bold", family = "Segoe UI", size=16),
        plot.subtitle = element_text(hjust=.5, face = "bold", family = "Segoe UI"),
        plot.caption = element_text(hjust=.5, family = "Segoe UI",size=12),
        legend.position = "none",
        panel.background = element_rect(fill="#EDE0D0"),
        plot.background = element_rect(fill="#EDE0D0"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(family = "Segoe UI"))
        


ggsave(plot, filename = "plot.png", units = "in", width = 8.5, height = 11)
  
  
