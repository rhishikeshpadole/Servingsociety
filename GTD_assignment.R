library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(gridExtra)
GTD_12_15 = read.csv('C:/Users/Administrator/Downloads/gtd_12to15_52134.csv')
GTD_92_11 = read.csv('C:/Users/Administrator/Downloads/gtd_92to11_no 93_55072.csv')
GTD_70_91 = read.csv('C:/Users/Administrator/Downloads/gtd_70to91_49566 (1).csv')
GTD_93 = read.csv('C:/Users/Administrator/Downloads/gtd1993_748.csv')

########################################################################################
#1]

terror_12_15 = GTD_12_15 %>% group_by(iyear) %>%
  summarise(yearwise_attacks = n_distinct(eventid)) %>%
  select(iyear, yearwise_attacks)



terror_92_11 = GTD_92_11 %>% group_by(iyear) %>%
  summarise(yearwise_attacks = n_distinct(eventid)) %>%
  select(iyear, yearwise_attacks)


terror_70_91 = GTD_70_91 %>% group_by(iyear) %>%
  summarise(yearwise_attacks = n_distinct(eventid)) %>%
  select(iyear, yearwise_attacks)


terror_93 = GTD_93 %>% group_by(iyear) %>%
  summarise(yearwise_attacks = n_distinct(eventid)) %>%
  select(iyear, yearwise_attacks)



# merging dataframes

q1 = data.frame(rbind(terror_70_91, terror_92_11, terror_12_15, terror_93))


# Plotting graph

q1_plot = ggplot(q1, aes(x=iyear, y=yearwise_attacks)) +
  geom_area(stat = "Identity")+geom_point(aes(fill=iyear))
  
q1_plot

install.packages("plotly")
library(plotly)
q1_plot = ggplot(q1, aes(x=iyear, y=yearwise_attacks)) +
  geom_area(stat = "Identity",color = "Red",size=1,fill="green")+
  geom_point(color="blue",size=2.5)
q1_plot
ggplotly(q1_plot)
########################################################################################
#2]

terror1_12_15 = GTD_12_15 %>% group_by(iyear) %>% 
  filter(attacktype1_txt == "Bombing/Explosion" | attacktype2_txt == "Bombing/Explosion" | attacktype3_txt == "Bombing/Explosion") %>%
  summarise(yearwise_bombing = n()) %>%
  select(iyear, yearwise_bombing)


terror1_92_11 = GTD_92_11 %>% group_by(iyear) %>% 
  filter(attacktype1_txt == "Bombing/Explosion" | attacktype2_txt == "Bombing/Explosion" | attacktype3_txt == "Bombing/Explosion") %>%
  summarise(yearwise_bombing = n()) %>%
  select(iyear, yearwise_bombing)


terror1_70_91 = GTD_70_91 %>% group_by(iyear) %>% 
  filter(attacktype1_txt == "Bombing/Explosion" | attacktype2_txt == "Bombing/Explosion" | attacktype3_txt == "Bombing/Explosion") %>%
  summarise(yearwise_bombing = n()) %>%
  select(iyear, yearwise_bombing)


terror1_93 = GTD_93 %>% group_by(iyear) %>% 
  filter(attacktype1_txt == "Bombing/Explosion" | attacktype2_txt == "Bombing/Explosion" | attacktype3_txt == "Bombing/Explosion") %>%
  summarise(yearwise_bombing = n()) %>%
  select(iyear, yearwise_bombing)




q2 = data.frame(rbind(terror1_70_91, terror1_92_11, terror1_93, terror1_12_15))
View(q2)

## Plotting graph for q2

q2_plot = ggplot(q2, aes(x=iyear, y=yearwise_bombing)) +
  geom_area(stat = "Identity",color = "darkorange3",size=0.5,fill="blue4")+
  geom_point(color="grey50",size=1.5)
ggplotly(q2_plot)
###########################################################################################



#########################################################################################

#3]

terror2_12_15 = GTD_12_15 %>% group_by(iyear, region_txt) %>% filter(doubtterr == 0) %>%
  summarise(total_terr_att = n()) %>% select(iyear, region_txt, total_terr_att)



terror2_92_11 = GTD_92_11 %>% group_by(iyear, region_txt) %>% filter(doubtterr == 0) %>%
  summarise(total_terr_att = n()) %>% select(iyear, region_txt, total_terr_att)



terror2_70_91 = GTD_70_91 %>% group_by(iyear, region_txt) %>% filter(doubtterr == 0) %>%
  summarise(total_terr_att = n()) %>% select(iyear, region_txt, total_terr_att)



terror2_93 = GTD_93 %>% group_by(iyear, region_txt) %>% filter(doubtterr == 0) %>%
  summarise(total_terr_att = n()) %>% select(iyear, region_txt, total_terr_att)



qa3 = data.frame(rbind(terror2_70_91, terror2_92_11, terror2_12_15, terror2_93))

qa3$region_txt[qa3$region_txt %in% c("Russia & the Newly Independent States (NIS)","Eastern Europe")]="Eastern Europe"

q3 = qa3 %>% group_by(iyear,region_txt) %>%
  summarise(Tot_Terr_Att = sum(total_terr_att))
View(q3)

## Plotting graph for q3


q3_plot = ggplot(q3,aes(x=iyear,y=Tot_Terr_Att))+
  geom_area(aes(fill = region_txt),col='Black')+
  theme_bw() + facet_wrap(~region_txt, scales="free",ncol=3)+
  geom_point(col="black",size=0.8)+
  theme(legend.position ='')+
  theme(legend.title = element_blank())+
  labs(subtitle='From 1970 to 2015',y='No Of Attacks', x='Year', title='Terrorist attacks region wise per year',caption ='www.GTD.com')
ggplotly(q3_plot)
  



############################################################################################

#4]


unraw_1 = GTD_70_91 %>%filter(doubtterr==0)%>% 
  group_by(region_txt, attacktype1_txt) %>%
  summarise(Total_Attacks = n())

unraw_2 = GTD_92_11 %>%filter(doubtterr==0)%>% 
  group_by(region_txt, attacktype1_txt) %>%
  summarise(Total_Attacks = n())

unraw_3 = GTD_93 %>%filter(doubtterr==0)%>% 
  group_by(region_txt, attacktype1_txt) %>%
  summarise(Total_Attacks = n())

unraw_4 = GTD_12_15 %>%filter(doubtterr==0)%>% 
  group_by(region_txt, attacktype1_txt) %>%
  summarise(Total_Attacks = n())



raw4 = data.frame(rbind(unraw_1,unraw_2,unraw_3,unraw_4))

raw4$region_txt[raw4$region_txt %in% c("Russia & the Newly Independent States (NIS)","Eastern Europe")]="Eastern Europe"
View(raw4)
qa4 = raw4 %>% group_by(region_txt,attacktype1_txt) %>% summarise(Tot_att = sum(Total_Attacks))


View(raw4)

q4 = raw4 %>% filter(!attacktype1_txt == "Unknown") %>%
  group_by(region_txt,attacktype1_txt) %>%
  summarise(Totall_Attacks = sum(Total_Attacks))%>%
  top_n(5)


View(q4)
#########################################################################################

# Plotting graph for q4
q4_plot = ggplot(q4, aes(x=attacktype1_txt,y=Totall_Attacks))+
  geom_bar(stat = "Identity", aes(fill=attacktype1_txt))+theme_bw()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank())+
  labs(subtitle="Global Terrorism Data", 
       y="Total no. of attacks", x="Types of Attacks", title="4]:Regionwise Top 5 types of Attacks")+facet_wrap(~region_txt,scale="free")

ggplotly(q4_plot)
  
##########################################################################################

#5]

terror5_12_15 = GTD_12_15 %>% group_by(region_txt,targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))

terror5_12_15$Total_wound = terror5_12_15$total_kill + terror5_12_15$total_wound


terror5_92_11 = GTD_92_11 %>% group_by(region_txt,targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))

terror5_92_11$Total_wound = terror5_92_11$total_kill + terror5_92_11$total_wound


terror5_70_91 = GTD_70_91 %>% group_by(region_txt,targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))

terror5_70_91$Total_wound = terror5_70_91$total_kill + terror5_70_91$total_wound


terror5_93 = GTD_93 %>% group_by(region_txt,targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))

terror5_93$Total_wound = terror5_93$total_kill + terror5_93$total_wound



raw_17 = data.frame(rbind(terror5_70_91, terror5_92_11, terror5_12_15, terror5_93))

View(raw_17)

q5 = raw_17 %>%filter(!targtype1_txt=="Other") %>%group_by(region_txt,targtype1_txt)%>% summarise(Total_cas = sum(Total_wound))%>%
  top_n(5)
View(q5)

##########################################################################################

## Plotting graph for q5

q5_plot = ggplot(q5, aes(x=reorder(targtype1_txt,-Total_cas), y=Total_cas))+
  geom_bar(stat = "Identity", aes(fill=targtype1_txt))+theme_bw()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank())+
  labs(subtitle="Global Terrorism Data", 
       y="Total no. of casualties", x="Types of Targets", title="5]:Heaviest heat target types")



q5_plot


###########################################################################################

# 6]

terror6_12_15 = GTD_12_15 %>% filter(country == 92 | country == 153) %>%
  group_by(iyear,country_txt) %>% summarise(Total_terr_att = n())



terror6_92_11 = GTD_92_11 %>% filter(country == 92 | country == 153) %>%
  group_by(iyear,country_txt) %>% summarise(Total_terr_att = n())



terror6_70_91 = GTD_70_91 %>% filter(country == 92 | country == 153) %>%
  group_by(iyear,country_txt) %>% summarise(Total_terr_att = n())



terror6_93 = GTD_93 %>% filter(country == 92 | country == 153) %>%
  group_by(iyear,country_txt) %>% summarise(Total_terr_att = n())



raw_1 = data.frame(rbind(terror6_93, terror6_70_91, terror6_92_11, terror6_12_15))

q6 = raw_1 %>% arrange(-iyear)
View(q6)

###########################################################################################

## Plotting graph for q6


q6_plot = q5_plot = ggplot(q6, aes(x=as.factor(iyear), y=Total_terr_att))+
  geom_line(stat = "Identity")+
  labs(subtitle="Global Terrorism Data", 
       y="Total no. of Terrorist Attacks", x="Year", title="6]:Comparison bet India and Pakistan")+
  facet_wrap(~country_txt,scale="free")


q6_plot
##########################################################################################

#7]

terror7_93 = GTD_93 %>% filter((country == 217|country == 167 | country == 359) & doubtterr == 0) %>%
  group_by(iyear,country_txt) %>% summarise(Total_terr_att = n())
#View(terror7_93)


terror7_70_91 = GTD_70_91 %>% filter((country == 217|country == 167 | country == 359) & doubtterr == 0) %>%
  group_by(iyear,country_txt) %>% summarise(Total_terr_att = n())
#View(terror7_70_91)


terror7_92_11 = GTD_92_11 %>% filter((country == 217|country == 167 | country == 359) & doubtterr == 0) %>%
  group_by(iyear,country_txt) %>% summarise(Total_terr_att = n())
#View(terror7_92_11)


terror7_12_15 = GTD_12_15 %>% filter((country == 217|country == 167 | country == 359) & doubtterr == 0) %>%
  group_by(iyear,country_txt) %>% summarise(Total_terr_att = n())
#View(terror7_12_15)


rr7 = data.frame(rbind(terror7_93,terror7_92_11,terror7_70_91,terror7_12_15))
View(rr7)


rr7$country_txt[q7$country_txt %in% c("Soviet Union","Russia")] = "Russia"

q7 = rr7 %>% group_by(iyear,country_txt) %>% summarise(Tot_att = sum(Total_terr_att))
View(q7)

########################################################################################

## Plotting graph for q7

q7_plot = ggplot(q7, aes(x=as.factor(iyear), y=Tot_att))+
  geom_bar(stat = "Identity", aes(fill=iyear))+theme_bw()+
  theme(axis.text.x = element_blank(),axis.ticks = element_blank())+
  labs(subtitle="Global Terrorism Data", 
       y="Total no. of Terrorist Attacks", x="Year", title="7]:Comparison bet USA and Russia")+
  geom_text(aes(label = Tot_att), position = position_dodge(width = .9), vjust = 0.25,hjust=-0.05)+
  facet_wrap(~country_txt) + coord_flip()

q7_plot
########################################################################################

#8]

# Grouping by countrywise


unraw_12_15 = GTD_12_15 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


unraw_70_91 = GTD_70_91 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


unraw_92_11 = GTD_92_11 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


unraw_93 = GTD_93 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))



raw = data.frame(rbind(unraw_70_91, unraw_12_15,unraw_92_11,unraw_93))

dd = gather(raw,Casualty_type,Totall_casualties,"total_kill":"total_wound")
q8 = dd %>% group_by(country_txt) %>%
  summarise(Total_casualties = sum(Totall_casualties)) %>% arrange(-Total_casualties)%>%
  head(10)


View(q8)


##################################################################################

## Plotting graph for q8

q8a_plot = ggplot(q8, aes(x=reorder(country_txt,-Total_casualties), y=Total_casualties))+
  geom_bar(stat = "Identity", aes(fill=country_txt))+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
    labs(subtitle="Global Terrorism Data", 
       y="Total Number of Casualties", x="Countries", title="8]:Most Attacked Countries")+
    geom_text(aes(label = Total_casualties), position = position_dodge(width = .9),vjust=-0.25)

q8a_plot

# Targetwise



unraw1_12_15 = GTD_12_15 %>% group_by(targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))%>%
  filter(!targtype1_txt == "Unknown")
unraw1_70_91 = GTD_70_91 %>% group_by(targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))%>%
  filter(!targtype1_txt == "Unknown")
unraw1_92_11 = GTD_92_11 %>% group_by(targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))%>%
  filter(!targtype1_txt == "Unknown")
unraw1_93 = GTD_93 %>% group_by(targtype1_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))%>%
  filter(!targtype1_txt == "Unknown")
unrawr = data.frame(rbind(unraw1_12_15,unraw1_70_91,unraw1_92_11, unraw1_93))
View(unrawr)
ghp = gather(unrawr, Casualty_type, Totally_casu, "total_kill":"total_wound")
View(ghp)


q8b = ghp %>%filter(!targtype1_txt == "Other")%>% group_by(targtype1_txt)%>%
  summarise(Total_casualtis = sum(Totally_casu))%>%
  arrange(-Total_casualtis)%>%head(15)
View(q8b)

########################################################################################

q8b_plot = ggplot(q8b, aes(x=reorder(targtype1_txt,-Total_casualtis), y=Total_casualtis))+
  geom_bar(stat = "Identity", aes(fill=targtype1_txt))+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(subtitle="Global Terrorism Data", 
       y="Total Number of Casualties", x="Countries", title="8]:Most Attacked Target Types")+
  geom_text(aes(label = Total_casualtis), position = position_dodge(width = .9),vjust=-0.25)

q8b_plot

#########################################################################################

#9]


d1 = GTD_12_15 %>% group_by(iyear) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


d2 = GTD_70_91 %>% group_by(iyear) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


d3 = GTD_92_11 %>% group_by(iyear) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


d4 = GTD_93 %>% group_by(iyear) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))

df = data.frame(rbind(d1,d2,d3,d4))
View(df)

ght = gather(df, Casu_type, total_casu, "total_kill":"total_wound")
View(ght)

q9 = ght %>% group_by(iyear) %>%
  summarise(Tota_casualties = sum(total_casu)) %>%
  arrange(iyear)
View(q9)
#########################################################################################

## Plotting graph for q9

q9_plot = ggplot(q9, aes(x=as.factor(iyear), y=Tota_casualties))+
  geom_bar(stat = "Identity", aes(fill=iyear))+theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(subtitle="Global Terrorism Data", 
       y="Total Number of Casualties", x="Years", title="9]:Evolution of casualties among years")+
  geom_text(aes(label = Tota_casualties), position = position_dodge(width = .9),hjust=-0.25)+
  coord_flip()


q9_plot

#########################################################################################

#10]

g1 = GTD_12_15 %>% filter(!weaptype1_txt == "Unknown") %>%filter(!weaptype1_txt == "Other")%>%
  group_by(weaptype1_txt) %>%
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


g2 = GTD_70_91 %>% filter(!weaptype1_txt == "Unknown") %>%filter(!weaptype1_txt == "Other")%>%
  group_by(weaptype1_txt) %>%
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


g3 = GTD_92_11 %>% filter(!weaptype1_txt == "Unknown") %>%filter(!weaptype1_txt == "Other")%>%
  group_by(weaptype1_txt) %>%
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


g4 = GTD_93 %>% filter(!weaptype1_txt == "Unknown") %>%filter(!weaptype1_txt == "Other")%>%
  group_by(weaptype1_txt) %>%
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


gh = data.frame(rbind(g1,g2,g3,g4))
View(gh)


sdf = gather(gh, Casu_type, total_casua, "total_kill":"total_wound")
View(sdf)


q10 = sdf %>% group_by(weaptype1_txt) %>%
  summarise(Total_casualtes = sum(total_casua)) %>%
  arrange(-Total_casualtes)

q10$weaptype1_txt = as.character(q10$weaptype1_txt)
q10$weaptype1_txt[q10$weaptype1_txt %in% c("Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)")]="Vehicles"
print(q10$weaptype1_txt)
View(q10)
#########################################################################################
# Plotting graph for q10

q10_plot = ggplot(q10, aes(x=reorder(weaptype1_txt,-Total_casualtes), y=Total_casualtes))+
  geom_bar(stat = "Identity", aes(fill=weaptype1_txt))+theme_bw()+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  labs(subtitle="Global Terrorism Data", 
       y="Total Number of Casualties", x="Types of Weapons", title="10]:Total casualties by Weapon Type")+
  geom_text(aes(label = Total_casualtes), position = position_dodge(width = .9),vjust=-0.25)


q10_plot


############################################################################################


#11]

n1 = GTD_12_15 %>% group_by(country_txt)%>%
  summarise(Total_attaks = n())


n2 = GTD_70_91 %>% group_by(country_txt)%>%
  summarise(Total_attaks = n())


n3 = GTD_92_11 %>% group_by(country_txt)%>%
  summarise(Total_attaks = n())


n4 = GTD_93 %>% group_by(country_txt)%>%
  summarise(Total_attaks = n())


nh = data.frame(rbind(n1,n2,n3,n4))

q11 = nh %>% group_by(country_txt) %>%
  summarise(Total_attacks = sum(Total_attaks)) %>% 
  arrange(-Total_attacks)%>%head(15)
View(q11)


# if there were one column"Nationality of terrrorist group", we were able to give clear estimation


########################################################################################

# Plotting graph for q11

q11_plot = ggplot(q11, aes(x=reorder(country_txt,-Total_attacks), y=Total_attacks))+
  geom_bar(stat = "Identity", aes(fill=country_txt))+theme_bw()+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  labs(subtitle="Global Terrorism Data", 
       y="Total Number of Attacks", x="Countries", title="11]:Targeted Nationalities")+
  geom_text(aes(label = Total_attacks), position = position_dodge(width = .9),vjust=-0.25)


q11_plot

#######################################################################################3

#12]

# Less casualtywise
aw_12_15 = GTD_12_15 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


aw_70_91 = GTD_70_91 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


aw_92_11 = GTD_92_11 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))


aw_93 = GTD_93 %>% group_by(country_txt) %>% 
  summarise(total_kill = sum(round(nkill),na.rm = T), total_wound = sum(round(nwound),na.rm = T))



aw = data.frame(rbind(aw_70_91, aw_12_15,aw_92_11,aw_93))

aw_gather = gather(aw,Casualty_type,Totall_casualties,"total_kill":"total_wound")
q12 = aw_gather %>% group_by(country_txt) %>%
  summarise(Total_casualt = sum(Totall_casualties)) %>% arrange(Total_casualt)%>%
  head(20)
View(q12)
#########################################################################################

# Plotting graph for q12

q12_plot = ggplot(q12, aes(x=reorder(country_txt,Total_casualt), y=Total_casualt))+
  geom_bar(stat = "Identity", aes(fill=country_txt))+theme_bw()+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  labs(subtitle="Global Terrorism Data", 
       y="Total Number of Attacks", x="Countries", title="12]:Safest Countries")+
  geom_text(aes(label = Total_casualt), position = position_dodge(width = .9),vjust=-0.25)

q12_plot
#########################################################################################
# Success ratewise


ax1 = GTD_12_15 %>% group_by(country_txt) %>%
  filter(success == 0&(!success == -9|!success == -99))%>% summarise(total_defends = n())
ay1 = GTD_12_15 %>% group_by(country_txt) %>%
  filter(!success == -9|!success == -99)%>% summarise(total_attacks = n())
a1 = merge(ax1,ay1, by = "country_txt", all.x = T)


ax2 = GTD_70_91 %>% group_by(country_txt) %>%
  filter(success == 0&(!success == -9|!success == -99))%>% summarise(total_defends = n())
ay2 = GTD_70_91 %>% group_by(country_txt) %>%
  filter(!success == -9|!success == -99)%>% summarise(total_attacks = n())
a2 = merge(ax2,ay2, by = "country_txt", all.x = T)


ax3 = GTD_92_11 %>% group_by(country_txt) %>%
  filter(success == 0&(!success == -9|!success == -99))%>% summarise(total_defends = n())
ay3 = GTD_92_11 %>% group_by(country_txt) %>%
  filter(!success == -9|!success == -99)%>% summarise(total_attacks = n())
a3 = merge(ax3,ay3, by = "country_txt", all.x = T)



ax4 = GTD_93 %>% group_by(country_txt) %>%
  filter(success == 0&(!success == -9|!success == -99))%>% summarise(total_defends = n())
ay4 = GTD_93 %>% group_by(country_txt) %>%
  filter(!success == -9|!success == -99)%>% summarise(total_attacks = n())
a4 = merge(ax4,ay4, by = "country_txt", all.x = T)


an = data.frame(rbind(a1,a2,a3,a4))

View(an)

ag = an %>% group_by(country_txt) %>%
  summarise(tot_def = sum(total_defends), tot_att = sum(total_attacks))





ag$ratio = round(ag$tot_def/ag$tot_att,2)


q12b = ag %>% arrange(-ratio) %>% head(10)
View(q12b)


# Limitation of this categorywise content is success = 0 (means failure) may be there 
# because of the attcker's own mistake
# If we know the nationality of terrorist org, then we can directly derrive which countries
#   are most safe
############################################################################################


# Plotting graph for q12b

q12b_plot = ggplot(q12b, aes(x=reorder(country_txt,-ratio), y=ratio))+
  geom_bar(stat = "Identity", aes(fill=country_txt))+theme_bw()+
  theme(axis.text.x = element_text(angle = 45),axis.ticks.x = element_blank())+
  labs(subtitle="Global Terrorism Data", 
       y="Ratio of defends per attack", x="Countries", title="12]:Safest Countries")+
  geom_text(aes(label = ratio), position = position_dodge(width = .9),vjust=-0.25)

q12b_plot


