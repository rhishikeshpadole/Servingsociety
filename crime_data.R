library(dplyr)
library(psych)
library(ggplot2)
library(readxl)
library(gridExtra)
library(purrr)
library(tidyr)



crime = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/39_Specific_purpose_of_kidnapping_and_abduction.csv")
View(crime)
###############################################################################3
#1] what are the 5 major reasons people being kidnapped in each state

crime$K_A_Cases_Reported = as.character(crime$K_A_Cases_Reported)
crime$K_A_Cases_Reported = as.numeric(crime$K_A_Cases_Reported)
# do not convert factor column directly into numeric , because factors start from 0 and integers starts from 1
#there may be mutation during convertion


crime$kidnap_status = ifelse(crime$Group_Name == "Kidnap - Total" | crime$Group_Name == "Kidnap - For Other Purposes", T, F) 

raw_1 = crime %>%filter(kidnap_status == "FALSE")%>% group_by(ï..Area_Name,Group_Name)%>%
  summarise(reasons = sum(K_A_Cases_Reported, na.rm = T))

raw_2 = raw_1 %>% top_n(5)

View(raw_2)

crime_graph = ggplot(raw_2, aes(x = Group_Name, y = reasons)) + geom_bar(stat = "Identity", aes(fill = Group_Name)) + theme(axis.text = element_blank(), axis.ticks = element_blank()) + 
  facet_wrap(~ï..Area_Name)
crime_graph


# It is another method to get rid of Kidnap-Total

#raw_1 = crime %>%filter(!Group_Name == "Kidnap - Total")%>% group_by(ï..Area_Name,Group_Name)%>%
# summarise(reasons = sum(K_A_Cases_Reported, na.rm = T))
#View(raw_1)

####################################################################################################

#2] Offenders relationship with rape victim

off_rel_vic = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/21_Offenders_known_to_the_victim.csv")
View(off_rel_vic)

off_rel_vic$No_of_Cases_in_which_offenders_were_known_to_the_Victims = as.character(off_rel_vic$No_of_Cases_in_which_offenders_were_known_to_the_Victims)
off_rel_vic$No_of_Cases_in_which_offenders_were_known_to_the_Victims = as.numeric(off_rel_vic$No_of_Cases_in_which_offenders_were_known_to_the_Victims)
off_rel_vic$No_of_Cases_in_which_offenders_were_Neighbours = as.character(off_rel_vic$No_of_Cases_in_which_offenders_were_Neighbours)
off_rel_vic$No_of_Cases_in_which_offenders_were_Neighbours = as.numeric(off_rel_vic$No_of_Cases_in_which_offenders_were_Neighbours)
off_rel_vic$No_of_Cases_in_which_offenders_were_Other_Known_persons = as.character(off_rel_vic$No_of_Cases_in_which_offenders_were_Other_Known_persons)
off_rel_vic$No_of_Cases_in_which_offenders_were_Other_Known_persons = as.numeric(off_rel_vic$No_of_Cases_in_which_offenders_were_Other_Known_persons)
off_rel_vic$No_of_Cases_in_which_offenders_were_Parentsclose_family_members = as.character(off_rel_vic$No_of_Cases_in_which_offenders_were_Parentsclose_family_members)
off_rel_vic$No_of_Cases_in_which_offenders_were_Parentsclose_family_members = as.numeric(off_rel_vic$No_of_Cases_in_which_offenders_were_Parentsclose_family_members)
off_rel_vic$No_of_Cases_in_which_offenders_were_Relatives = as.character(off_rel_vic$No_of_Cases_in_which_offenders_were_Relatives)
off_rel_vic$No_of_Cases_in_which_offenders_were_Relatives = as.numeric(off_rel_vic$No_of_Cases_in_which_offenders_were_Relatives)


col = c("Neigh", "other_Know", "Family_member", "Relative")
colnames(off_rel_vic)[4:7] = col


raw2_1 = gather(off_rel_vic, Relationship, Cases, Neigh:other_Know:Family_member:Relative)
View(raw2_1)

raw2_2 = raw2_1 %>% group_by(Area_Name,Relationship) %>% summarise(Total_cases = sum(Cases))
View(raw2_2)


## plotting graph

offender_graph = ggplot(raw2_2, aes(x = Relationship, y = Total_cases)) + geom_bar(stat = "Identity", aes(fill = Total_cases)) +
  theme(title = element_text("Rape victim relationship with offender")) +
  facet_wrap(~Area_Name)
offender_graph  

#############################################################################################


#3] Juveniles family background, education and economical setup



## for education

Juv_edu = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/18_01_Juveniles_arrested_Education.csv")
View(Juv_edu)

cols = c("5-12std", "Illeterate", "12pass", "Total_educated","1-4std")

colnames(Juv_edu)[4:8] = cols
str(Juv_edu)


raw3_1 = gather(Juv_edu, Education, No_of_Juveniles, "5-12std" : "Illeterate" : "12pass": "1-4std")
View(raw3_1)

raw3_2a = raw3_1 %>%filter(!Education == "Total_educated")%>% group_by(Education) %>%
  summarise(Total_juvs = sum(No_of_Juveniles))
View(raw3_2a)

# Plotting the graph

Juv_graph = ggplot(raw3_2a, aes(x = Education, y = Total_juvs)) + geom_bar(stat = "Identity", aes(fill = Education))
Juv_graph


########################################3


## for economic background

Juv_economy = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/18_02_Juveniles_arrested_Economic_setup.csv")
View(Juv_economy)

cols = c("25k-50k","less_than_25k","100k-200k","50k-100k","Total","more_than_300k","200k-300k")

colnames(Juv_economy)[4:10] = cols

raw_data1 = gather(Juv_economy, Income_group, No_of_juvs, "25k-50k":"less_than_25k":"100k-200k":"50k-100k":"more_than_300k":"200k-300k")
View(raw_data1)

raw_data2 = raw_data1 %>%filter(!Income_group == "Total")%>% group_by(Income_group) %>% summarise(Total_juvs = sum(No_of_juvs))
View(raw_data2)



# plotting graph for economy background

Juv_graph_eco = ggplot(raw_data2, aes(x = Income_group, y = Total_juvs)) + geom_bar(stat = "Identity", aes(fill = Income_group))
Juv_graph_eco



## for family background

Juv_family = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/18_03_Juveniles_arrested_Family_background.csv")
View(Juv_family)


cols = c("Homeless", "Liv_guardian","Liv_parents")

colnames(Juv_family)[4:6] = cols


raw_dataa1 = gather(Juv_family, Living_type, No_of_juvs, "Homeless" : "Liv_guardian":"Liv_parents")
View(raw_dataa1)


raw_dataa2 = raw_dataa1 %>% group_by(Living_type) %>% summarise(Total_juvs = sum(No_of_juvs))
View(raw_dataa2)

# Plotting graph for family background

Juv_fam_graph = ggplot(raw_dataa2, aes(x = Living_type, y = Total_juvs)) +
  geom_bar(stat = "Identity", aes(fill=Living_type))
Juv_fam_graph


grid.arrange(Juv_graph, Juv_fam_graph, Juv_graph_eco)
##########################################################################################

#5]

Comp_agai_police = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/25_Complaints_against_police.csv")
View(Comp_agai_police)
str(Comp_agai_police)


cols = c("Total_Reg_cases")
colnames(Comp_agai_police)[4] = cols

raw5_1 = Comp_agai_police %>% group_by(ï..Area_Name) %>% 
  summarise(Total_cases = sum(Total_Reg_cases)) %>%
  arrange(-Total_cases) %>% head(10)
  
View(raw5_1)


# Plotiing for comp_agai_police

comp_police_graph = ggplot(raw5_1, aes(x = reorder(ï..Area_Name, -Total_cases), y = Total_cases)) +
  geom_bar(stat = "Identity", aes(fill = ï..Area_Name)) + theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
comp_police_graph
######################################################################################

##6]

# for women

women_2012 = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/42_District_wise_crimes_committed_against_women_2001_2012.csv")
women_2013 = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/42_District_wise_crimes_committed_against_women_2013.csv")

View(women_2013)
cols1 = c("Kidnap","Dowry","Assault","Insult","Home_voilence","Girl_import")
colnames(women_2012)[5:10] = cols1

# Also changing names of dataframe"women_2013"

cols2 = c("Kidnap","Dowry","Assault","Insult","Home_voilence","Girl_import")
colnames(women_2013)[5:10] = cols2



women_comp = data.frame(rbind(women_2012, women_2013))
View(women_comp)

str(women_comp)



raw6_1 = gather(women_comp, crime_type, No_of_cases, Rape:Girl_import)
View(raw6_1)

raw6_2 = raw6_1 %>% group_by(STATE.UT) %>% summarise(Total_cases = sum(No_of_cases)) %>%
  arrange(-Total_cases)
View(raw6_2)

# Plotting graph for total women violence

women_graph = ggplot(raw6_2, aes(x = reorder(STATE.UT, -Total_cases), y = Total_cases)) + 
  geom_bar(stat = "Identity", aes(fill=STATE.UT)) + theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),legend.position = "bottom")+
  geom_text(aes(label = STATE.UT), position = position_dodge(width = .9), vjust = -0.25)

women_graph


# for child

chil_2012 = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/03_District_wise_crimes_committed_against_children_2001_2012.csv")

chil_2013 = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/03_District_wise_crimes_committed_against_children_2013.csv")



child_2012 = chil_2012 %>% select(-Other.Crimes,-Total)

child_2013 = chil_2013 %>% select(-Infanticid,-Other.Crimes,-Total)

View(child_2012)

cols = c("Murder","Rape", "Kidnap", "Foeticide", "Suicide_Abetment", "Abandonment", "Procuration_girls", "Buy_Girls_prostitute", "Sell_Girls_prostitute","Child_marriage")

colnames(child_2012)[4:13] = cols
colnames(child_2013)[4:13] = cols




child_comp = data.frame(rbind(child_2012, child_2013))
View(child_comp)




rawq = gather(child_comp, Crime_Type, Total_No_cases, Murder:Child_marriage) 
View(rawq)


raww = rawq %>% group_by(STATE.UT) %>% summarise(Total_cases = sum(Total_No_cases))  %>%
  arrange(-Total_cases) %>% head(10)
View(raww)


## Plotting graph for child_crime

child_graph = ggplot(raww, aes(x = reorder(STATE.UT,-Total_cases), y = Total_cases)) + 
  geom_bar(stat = "Identity", aes(fill = STATE.UT)) + theme_bw()+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "bottom")+
  geom_text(aes(label = STATE.UT), position = position_dodge(width = .9), vjust = -0.25)

child_graph


grid.arrange(women_graph, child_graph)


##############################################################################################








foreigner_2012 = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/01_District_wise_crimes_committed_IPC_2001_2012.csv")
foreigner_2013 = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/01_District_wise_crimes_committed_IPC_2013.csv")
foreigner_2014 = read.csv("C:/Users/Administrator/Downloads/rajanand-crime-in-india-20180827T070855Z-001/rajanand-crime-in-india/01_District_wise_crimes_committed_IPC_2014.csv")



str(foreigner_2014)
View(foreigner_2014)

raw_2012 = foreigner_2012 %>% select(-OTHER.IPC.CRIMES,-TOTAL.IPC.CRIMES,-CAUSING.DEATH.BY.NEGLIGENCE,-CRUELTY.BY.HUSBAND.OR.HIS.RELATIVES)
raw_2013 = foreigner_2013 %>% select(-OTHER.IPC.CRIMES,-TOTAL.IPC.CRIMES,-CAUSING.DEATH.BY.NEGLIGENCE,-CRUELTY.BY.HUSBAND.OR.HIS.RELATIVES)



raww2 = data.frame(rbind(raw_2012, raw_2013))
str(raww2)

raww3 = gather(raww2, Crime_Type, Total_No_cases ,MURDER:IMPORTATION.OF.GIRLS.FROM.FOREIGN.COUNTRIES)
View(raww3)


raww4 = raww3 %>% group_by(STATE.UT) %>% summarise(Total_cases = sum(Total_No_cases))
View(raww4)

##########################################################################################################
