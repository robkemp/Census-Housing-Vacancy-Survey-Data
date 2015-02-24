library(ggplot2)
library(grid)
library("robR")
library(codemog)
library(tidyr)
library(dplyr)


rent_msa=read.csv("msa_rental_2014.csv")
howner_msa=read.csv("msa_homeownership_2014.csv")
rent_state=read.csv("state_rental_2014.csv")
howner_state=read.csv("state_homeownership_2014.csv")

### Puts Ranks into Data
rent_msa=rent_msa%>%
  group_by(year)%>%
  mutate(rank_q1=dense_rank(rent_q1),
         rank_q2=dense_rank(rent_q2),
         rank_q3=dense_rank(rent_q3),
         rank_q4=dense_rank(rent_q4))
rent_state=rent_state%>%
  group_by(year)%>%
  mutate(rank_q1=dense_rank(rent_q1),
         rank_q2=dense_rank(rent_q2),
         rank_q3=dense_rank(rent_q3),
         rank_q4=dense_rank(rent_q4))
howner_msa=howner_msa%>%
  group_by(year)%>%
  mutate(rank_q1=dense_rank(howner_q1),
         rank_q2=dense_rank(howner_q2),
         rank_q3=dense_rank(howner_q3),
         rank_q4=dense_rank(howner_q4))
howner_state=howner_state%>%
  group_by(year)%>%
  mutate(rank_q1=dense_rank(howner_q1),
         rank_q2=dense_rank(howner_q2),
         rank_q3=dense_rank(howner_q3),
         rank_q4=dense_rank(howner_q4))
### Graphs ###
#temp=howner_msa%>%filter(grepl("Denver", msa))
p1=rent_state%>%
  filter(grepl("Colorado", state)|grepl("California", state) )%>%
  ggplot(aes(x=as.character(year), y=rent_q4, color=state))+
  geom_line(aes(group=state),size=1.5)+
  scale_color_manual(values=c(rgb(31,74,126, max=255), rgb(192,80,77,max=255)),
                    name="State")+
  theme_codemog()+
  labs(x="Year", y="Rental Vacancy Rate (%)", title="State Rental Vacancy Rate, Fourth Quarter, 2005 to 2014")
p1
p2=rent_state%>%
  filter(grepl("Colorado", state)|grepl("California", state) )%>%
  ggplot(aes(x=as.character(year), y=rank_q4, color=state))+
  geom_line(aes(group=state),size=1.5)+
  scale_color_manual(values=c(rgb(31,74,126, max=255), rgb(192,80,77,max=255)),
                     name="State")+
   theme_codemog()+
  labs(x="Year", y="Rank", title="State Rental Vacancy Rank (Lowest Rate Ranks First)\nFourth Quarter, 2005 to 2014")
p2

p3=rent_msa%>%
  filter(grepl("Denver", msa)| grepl("San Francisco", msa) )%>%
  ggplot(aes(x=as.character(year), y=rent_q4, color=msa))+
  geom_line(aes(group=msa), size=1.5)+
  scale_color_manual(values=c(rgb(31,74,126, max=255), rgb(192,80,77,max=255)),
                     name="MSA")+
  theme_codemog()+
  labs(x="Year", y="Rental Vacancy Rate (%)", title="MSA Rental Vacancy Rate, Fourth Quarter, 2005 to 2014")
p3
p4=rent_msa%>%
  filter(grepl("Denver", msa)| grepl("San Francisco", msa) )%>%
  ggplot(aes(x=as.character(year), y=rank_q4, color=msa))+
  geom_line(aes(group=msa), size=1.5)+
  scale_color_manual(values=c(rgb(31,74,126, max=255), rgb(192,80,77,max=255)),
                     name="MSA")+
  theme_codemog()+
  labs(x="Year", y="Rank", title="MSA Rental Vacancy Rank (Lowest Rate Ranks First)\nFourth Quarter, 2005 to 2014\n(Among 75 Largest MSAs)")
p4




