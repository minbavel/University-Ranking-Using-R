#Mohanavel_Inbavel
#0889477
#Data Science Project
library(readr)
library(dplyr)
library(ggplot2)

times <- read.csv("timesData.csv")

#1)Top 10 Universities in 2011
new_time<-head(times,10)
View(new_time)

#2)Countrywise distribution of top 100 Universities
#Top 100 universities list
df <- data.frame(times[1:100,])
df <- droplevels(df)

new_df<- as.data.frame(table(df$country))
View(new_df)

colnames(new_df) <- c("Country", "Frequency")

univ <- ggplot(new_df, aes(x = Country, y = Frequency))+xlab("Country") + 
  ylab("Number of universities")+geom_bar(stat = "identity")+ 
  ggtitle("No of Times a Country is in top 100")+
  geom_text(aes(label = new_df$Frequency)) 
univ

#3)Country Ranking by Research
res<-df %>% filter(country %in% df$country) %>%
  ggplot(aes(x=country, y=research)) +
  geom_point() + coord_flip() +
  labs(x="Country", y="Research score", 
       title="Research score", subtitle="Grouped by country")
res

#4)Top 10 University Depending on no of International students in 2011
top10_time<-head(times,10)

plot1<-times %>% filter(university_name %in% top10_time$university_name) %>%
  ggplot(aes(x=university_name, y=as.numeric(as.character(gsub("%","",international_students)))),fill=year) +
  geom_bar(stat="identity", position=position_dodge(0.2))+ coord_flip() + 
  labs(x="University", y="International Students percentage", 
       title="International Students percentage", subtitle="Top 10 universities")
plot1

#5)Teaching Score Grouped by Country
teach<-times %>% group_by(country) %>% summarise(n = length(teaching)) %>% 
  top_n(10,n) %>% ungroup()
teach
plot_2<-times %>% filter(country %in% teach$country) %>%
  ggplot(aes(x=country, y=teaching)) +
  geom_point()  + coord_flip()+
  labs(x="Country", y="Teaching", 
       title="Teaching score", subtitle="Grouped by country")
plot_2

#6)Number of Universities in Canada and US
plot_3<-times %>%group_by(year, country) %>%summarise(count = n()) %>%
  filter(country == "United States of America" | country == "Canada") %>%
  ggplot(aes(country, count)) + geom_bar(stat = "identity") +
  facet_wrap(~year) + geom_text(aes(label = count)) + 
  labs(title = "Number of Universities in Canada and US", y = "Number of Universities")
plot_3



data<-read.csv(file="cwurData.csv", head=TRUE, sep=",")


#7)Top 10 university of world in 2012
data_2012<-data %>% filter(world_rank <11,year=="2012") 
View(data_2012)

#Top 10 university of world in 2013
data_2013<-data %>% filter(world_rank <11,year=="2013") 
View(data_2013)

#8)Top 5 University from 2012-2015
data_top5<-data %>% group_by(year) %>% 
  select(year,institution,world_rank) %>% top_n(-5, wt = world_rank)
View(data_top5)

plot_4<-data_top5 %>% group_by(year %in% data_top5$institution) %>%
  ggplot(aes(x=year, y=world_rank , fill=institution , col=institution)) +
  geom_point(stat = "identity") 
plot_4




