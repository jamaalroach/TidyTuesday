

####### LIBRARIES  ###########

library(tidyverse)
library(unvotes)
library(cluster)
library(ggthemes)
library(rnaturalearth)
library(sf)
library(countrycode)


########### DATA MANIPULATION #########

votes<-un_votes
roll<-un_roll_calls
issue<-un_roll_call_issues
issue<-issue[!duplicated(issue[,1]),]  #

vote_data<-left_join(votes, roll, "rcid")
vote_data<-left_join(vote_data, issue, "rcid")


vote_data<-vote_data%>%mutate(vote_n=match(vote, c("no", "abstain","yes")))
vote_data<-vote_data%>%filter(issue!="NA")

vote_data<-vote_data%>%filter(date>"1999-12-31")%>%select(country,rcid,  vote_n)


######## K MEANS   #########


# data needs to be ina  wide format
cluster_data<-vote_data%>%pivot_wider(names_from = rcid, values_from = vote_n, values_fill = 0, names_prefix="rcid_") 


#data needs to be all numerical , country variable changed to a rowname
cluster_data<-column_to_rownames(cluster_data, var="country") 


#scaling data to have a standard nor mal distribution i.e mean = 0, var = 1. required for clustering algorithm
cluster_data<-scale(cluster_data) 


#run K means algorithm 
set.seed(0)
vote_cluster<-kmeans(cluster_data, 3, nstart = 20)




####### PLOTTING DATA PREPARATION ###########

cluster_data<-as.data.frame(cluster_data)
cluster_data$cluster<-vote_cluster$cluster
plot_data<-cluster_data%>%select(cluster)
plot_data$country<-rownames(plot_data)
plot_data$cluster<-as.factor(plot_data$cluster)
plot_data$region<-plot_data$country



## get natural earth mapping data in dataframe format

world_ne<-ne_countries(scale="medium", returnclass = "sf")   


#uses country code package to get country 1so2 code , required to merge with natural earth mapping data

plot_data$iso_a2<-countrycode(plot_data$country, "country.name", "iso2c") 

#merges mapping data with plotting data
plot_data<-merge(world_ne, plot_data, "iso_a2") 


####### PLOTTING ##########

bloc_plot<-
  ggplot(plot_data)+
  geom_sf(aes(fill=cluster),colour="#2a3135", alpha=.6, size=.4)+
  coord_sf()+
  theme_map()+
  labs(title = "VOTING BLOCS",
       subtitle = "Cluster analysis of UN General Assembly votes 2000 - 2019",
       caption="#TidyTuesday | Source: UN Votes, Harvard Dataverse \n @jamaalroach")+
  scale_fill_discrete(name="BLOCS Include:", 
                      labels=c("\n Most of the world\n ", 
                                "Canada, Europe, \nAustrailia, Japan, New Zealand...",
                               "\nUS, Central Africa, \n Israel, Macedonia..."),
                      type =c("#6da7de","#dee000", "#63193b" ) 
                      )+
  theme(legend.background = element_rect(fill="#2a3135", colour = "#2a3135"),
        legend.text = element_text(colour="#dddddd", size=10),
        legend.key = element_rect(fill="#2a3135"),
        legend.title = element_text(colour = "#dddddd", size=10
        ),
        plot.title.position = "plot",
        plot.subtitle = element_text(colour="#dddddd", size=14),
        plot.caption = element_text(colour="#dddddd", size=8),
        plot.caption.position = "plot",
        plot.title = element_text(face="bold", colour="#dddddd", size = 16),
        panel.background = element_rect(fill="#2a3135",colour="#2a3135"),
        plot.background = element_rect(fill="#2a3135", colour="#2a3135"),
        plot.margin = margin(25,25,10,25))


ggsave(bloc_plot, filename = "bloc_plot.png", units = "in", width = 11, height = 8.5)








