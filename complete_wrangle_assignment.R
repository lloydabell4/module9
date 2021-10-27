#Data Wrangling
library(tidyverse)


combine <- read_csv("data/combine.csv")
position_guide <- read_csv("data/position_guide.csv")
team_picking_categories <- read_csv("data/team-picking-categories.csv")
#from https://www.kaggle.com/kbanta11/nfl-combine



#add LS to the position guide
position_guide <- position_guide %>%
  rbind(c("LS","Special Teams"))
view(combine$Pos)

#join player data with position data
##note that PK was not in data
player_position <- left_join(combine, position_guide, by = "Pos")

player_position <- player_position %>%
  separate('Drafted (tm/rnd/yr)', into = c("TEAM","Rnd","Pick","Year"), sep = " / ") 

player_position <- player_position%>%
  left_join(team_picking_categories, by = "TEAM")

player_position <- replace_na(player_position,list(Rnd = "Not Drafted"))
player_position$Rnd <- as.factor(player_position$Rnd)

player_position$Pick <- parse_number(player_position$Pick)

combine_clean <- player_position %>%
  rename(forty ='40YD')%>%
  rename(threecone = '3Cone')%>%
  mutate(TEAM = str_replace(TEAM, "St. Louis Rams","Los Angeles Rams"))%>%
  mutate(TEAM = str_replace(TEAM, "Los Angeles Chargers", "San Diego Chargers"))%>%
  select(TEAM,Player,Height,Wt,forty,Vertical,BenchReps,'Broad Jump',threecone,Shuttle,Pos,Rnd,Pick,Offense_Defense_SpecialTeams,Year)

#look at # of top picks for all teams
top_picks <- c("1st","2nd","3rd")

#data transformation for top picks
by_round_picks <- combine_clean %>%
  filter(Rnd %in% top_picks)%>%
  group_by(TEAM)%>%
  summarize(num_of_picks = n())%>%
  arrange()
#how would you get the count for total picks 

combine_clean%>%
  count(Pos, TEAM = 'Jacksonville Jaguars')

#most picks by Jags
count_by_position <- combine_clean %>%
  group_by(TEAM,Pos)%>%
  summarise(pos_count = n())%>%
  filter(TEAM == "Jacksonville Jaguars")%>%
  arrange(desc(pos_count))%>%
  ggplot(aes(x = reorder(Pos,-pos_count), y = pos_count))+
  geom_bar(stat = 'identity')

count_by_position

#most pics jags vs NE
count_by_position <- combine_clean %>%
  group_by(TEAM,Pos)%>%
  summarise(pos_count = n())%>%
  filter(TEAM == "Jacksonville Jaguars" | TEAM == "New England Patriots")%>%
  arrange(desc(pos_count))%>%
  ggplot(aes(x = reorder(Pos,-pos_count), y = pos_count, fill = TEAM))+
  geom_bar(stat = 'identity', position = 'dodge')

count_by_position

#Create a visualization that shows the range of 40 yard dash times (40YD) for each position. 
ggplot(combine_clean, aes(reorder(Pos, forty, na.rm=TRUE), forty))+
  geom_boxplot()+
  labs(x = "Position", y = "40 Yard Dash time (s)")+
  coord_flip()


#Determine whether this range has changed over time. 
#look specifically at defense players
combine_clean%>%
  filter(Offense_Defense_SpecialTeams == "Defense")%>%
  ggplot(aes(Year, forty))+
  geom_boxplot()+
  labs(x = "Year", y = "40 Yard Dash time (s)")+
  coord_flip()+
  facet_wrap(~Pos)

team_combine_position$Year <- as.numeric(team_combine_position$Year)
#trends in categories
#correlation between Coaching - Strength of on-field leadership and Ownership Loyalty

ggplot(team_picking_categories, aes(CCH,OWN))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(x = "Coaching - Strength of on-field leadership", 
       y = "Ownership - Honesty; loyalty to core players and the community")

team_picks <- combine_clean%>%
  filter(Rnd == "1st")%>%
  group_by(TEAM)%>%
  summarise(mean_PICK = mean(Pick))%>%
  arrange(mean_PICK)

#join combine data with team category data
TEAM_combine_categories <- left_join(team_picks,team_picking_categories, by = "TEAM")

ggplot(TEAM_combine_categories, aes(CCH,mean_PICK))+
  geom_point()+
  geom_smooth(method = 'lm')

#pivot_longer to facet across categories
pivot_team_combine_cat <- TEAM_combine_categories %>%
  pivot_longer(BMK:BEH, names_to = "Team_Category", values_to = "Values")


ggplot(pivot_team_combine_cat, aes(Values,mean_PICK))+
  geom_point(aes(color = TEAM))+
  geom_smooth(method = 'lm')+
  facet_wrap(~Team_Category)+
  labs(x = "Team Value", 
       y = "Average 1st Round Pick Over the Last 10 Years")+
  guides(color = FALSE)

pivot_team_combine_cat%>%
  filter(Team_Category %in% c('BWG','CCH'))%>%
  ggplot(aes(Values,mean_PICK, color = Team_Category))+
  geom_point()+
  geom_smooth(method = 'lm')

#https://github.com/fivethirtyeight/data/tree/master/nfl-favorite-team

for_stats <- combine_clean %>% 
  filter(Pos %in% c("QB","WR"))%>%
  mutate(Pos_factor = factor(Pos, levels = c("QB", "WR")))

ttest_40xpos <- t.test(forty ~ Pos_factor, data = for_stats)

ttest_40xpos
