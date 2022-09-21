# OKC Thunder Technical Assessment

shots_data <- read.csv('shots_data.csv')

shots_data$pythag <- sqrt(shots_data$x**2 + shots_data$y**2)

shots_data$shot_type <- ifelse(abs(shots_data$x) > 22 & shots_data$y <= 7.8, 'C3', 
                               ifelse(shots_data$pythag > 23.75, 'NC3', '2PT'))

shots_data$three_made <- ifelse(shots_data$shot_type != '2PT' & 
                                  shots_data$fgmade == 1, 0.5, 0)

# distribution of shots by team and shot category
shot_distribution <- shots_data %>% group_by(team, shot_type) %>% summarise(n = n()) %>% 
  mutate(shot_distribution = n/sum(n))

# eFG = (FGM + (.5*3PM))/FGA
eFG <- shots_data %>% group_by(team, shot_type) %>% 
  summarize(eFG = (sum(fgmade) + sum(three_made))/n())
