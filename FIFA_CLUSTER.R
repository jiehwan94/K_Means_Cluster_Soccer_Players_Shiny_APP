library(readr)
library(tidyverse)
library(scales)
library(ggthemes)
library(kableExtra)
library(plotly)
library(ggplot2)
library(vioplot)

options(stringsAsFactors = FALSE)
fifa_data<- read_csv('C:\\Users\\user\\Desktop\\Project\\FIFA19\\data.csv',
                     local=locale(encoding="UTF-8"))
head(fifa_data)



options(scipen = 999)

# Feature Engineering 1

fifa_data <- fifa_data %>%
  mutate(ValueMultiplier = ifelse(str_detect(Value, "K"), 1000, ifelse(str_detect(Value, "M"), 1000000, 1))) %>%
  mutate(ValueNumeric_pounds = as.numeric(str_extract(Value, "[0-9]+")) * ValueMultiplier) %>%
  mutate(Position = ifelse(is.na(Position), "Unknown", Position))


fifa_data <- fifa_data %>%
  mutate(WageMultiplier = ifelse(str_detect(Wage, "K"), 1000, ifelse(str_detect(Wage, "M"), 1000000, 1))) %>%
  mutate(WageNumeric_pounds = as.numeric(str_extract(Wage, "[0-9]+")) * WageMultiplier)


positions <- unique(fifa_data$Position)
positions

gk <- "GK"
defs <- positions[str_detect(positions, "B$")]
mids <- positions[str_detect(positions, "M$")]
f1 <- positions[str_detect(positions, "F$")]
f2 <- positions[str_detect(positions, "S$")]
f3 <- positions[str_detect(positions, "T$")]
f4 <- positions[str_detect(positions, "W$")]
fwds <- c(f1, f2, f3, f4)

fifa_data <- fifa_data %>% 
  mutate(PositionGroup = ifelse(Position %in% gk, "GK", ifelse(Position %in% defs, "DEF", ifelse(Position %in% mids, "MID", ifelse(Position %in% fwds, "FWD", "Unknown")))))

# BOX PLOT
a <- fifa_data %>%
  filter(PositionGroup != "Unknown") %>%
  ggplot(aes(x= PositionGroup, y= ValueNumeric_pounds, fill= PositionGroup)) +
  geom_violin() +
  geom_boxplot(width= 0.2) +
  scale_y_log10(labels = dollar_format(prefix = "€")) +
  ggtitle("Position vs Player_Value",subtitle = "Forwards and Mid-fields are generally valued higher") +
  theme_fivethirtyeight()
a

b <- fifa_data %>%
  filter(PositionGroup != "Unknown") %>%
  ggplot(aes(x= Position, y= ValueNumeric_pounds)) +
  geom_boxplot(fill = "orange") +
  scale_y_log10(labels = dollar_format(prefix = "€")) +
  coord_flip() +
  ggtitle("Position vs Player_Value",subtitle = "RF, LF, RAM are valued the most") +
  theme_fivethirtyeight() +
  facet_wrap(~ PositionGroup, scales = "free") +
  theme(strip.background = element_rect(fill = "black"), strip.text = element_text(colour = "orange", face = "bold"))

b

gridExtra::grid.arrange(a, b)


# Correlations with Overall rating
gk_vars <- fifa_data %>% select(contains("GK")) %>% names()


spearman_cor_with_overall <- fifa_data %>%
  filter(Position != "GK") %>%
  select_if(is.numeric) %>%
  select(-GKDiving, -GKHandling, -GKKicking, -GKPositioning, -GKReflexes, 
         -ID, -X1, - `Jersey Number`, -ValueMultiplier, -WageMultiplier) %>% 
  as.matrix() %>%
  na.omit() %>%
  cor(method = "spearman") 

pearson_cor_with_overall <- fifa_data %>%
  filter(Position != "GK") %>%
  select_if(is.numeric) %>%
  select(-GKDiving, -GKHandling, -GKKicking, -GKPositioning, -GKReflexes,
        -ID, -X1, - `Jersey Number`, -ValueMultiplier, -WageMultiplier) %>% 
  as.matrix() %>%
  na.omit() %>%
  cor()

cor_colnames <- colnames(spearman_cor_with_overall)

spearman_cor_with_overall <- spearman_cor_with_overall[,2] %>% data.frame()
spearman_cor_with_overall <- cbind(cor_colnames, spearman_cor_with_overall) %>% arrange(desc(`.`))

pearson_cor_with_overall <- pearson_cor_with_overall[,2] %>% data.frame()
pearson_cor_with_overall <- cbind(cor_colnames, pearson_cor_with_overall) %>% arrange(desc(`.`))

head(spearman_cor_with_overall[-1,],n = 10)
head(pearson_cor_with_overall[-1,], n= 10)

xx<- spearman_cor_with_overall %>% 
  left_join(pearson_cor_with_overall, by = "cor_colnames") %>% 
    #rename(Feature = "cor_colnames", Spearman = "..x", Pearson = "..y") %>% 
      #filter(Feature != "Overall") %>% 
        head(11)
colnames(xx)<- c("Features", "Spearman", "Pearson")
xx<- xx[-1,]





# Feature Engineering 2

fifa_data <- fifa_data  %>%
  mutate(AttackingRating = (Finishing + LongShots + Penalties + ShotPower + Positioning) /5)

data_cluster <- fifa_data %>%
  filter(PositionGroup != "Unknown") %>%
  filter(PositionGroup != "GK") %>%
  mutate(RoomToGrow = Potential - Overall) %>%
  select_if(is.numeric) %>%
  select(-X1, -ID, -`Jersey Number`, -AttackingRating, -starts_with("Value"), - starts_with("Wage"), -starts_with("GK"), -Overall)
data_cluster

# Scale the data

scaled_data <- scale(data_cluster)



set.seed(422)
# Initialize total within sum of squares error: wss
wss <- 0
# For 1 to 30 cluster centers
# This for loop take a while to run @@@@@@@@@@@@
for (j in 1:30) {
  km.out <- kmeans(scaled_data, centers = j, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[j] <- km.out$tot.withinss
}

wss_df <- data.frame(num_cluster = 1:30, wgss = wss)
wss_df

ggplot(data = wss_df, aes(x=num_cluster, y= wgss)) + 
  geom_line(color = "lightgrey", size = 2) + 
  geom_point(color = "orange", size = 4) +
  geom_curve(x=14, xend=8, y=300000, yend= 290500, arrow = arrow(length = unit(0.2,"cm")), size =1, colour = "red") +
  geom_text(label = "k = 8", x=14, y= 290000, colour = "red") +
  labs(title = "At which point does it bend?")


# Set k equal to the number of clusters corresponding to the elbow location
k <- 8

# Create a k-means model on wisc.data: wisc.km
wisc.km <- kmeans(scale(data_cluster), centers = k, nstart = 20)
wisc.km

# add the cluster group back to the original DF for all players other than GK and Unknown
cluster_data <- fifa_data %>%
  filter(PositionGroup != "Unknown") %>%
  filter(PositionGroup != "GK") %>%
  mutate(Cluster = wisc.km$cluster)
cluster_data$Cluster

# create a new DF with just some select variables for analysis
cluster_analysis <- cluster_data %>%
  select(Photo, Name,Club, Age, PositionGroup, Overall, Cluster, ValueNumeric_pounds, `Club Logo`,ID)

# how have the clusters been split between position groups
table(cluster_analysis$Cluster, cluster_analysis$PositionGroup)


# build the plot for each of the 20 highest rated players in each cluster
aa<-  pp%>%
  mutate(Cluster = paste("Cluster: ", Cluster, sep = "")) %>%
  arrange(desc(Overall)) %>% 
  group_by(Cluster) %>%
  slice(1:20) %>%
  mutate(Under27 = ifelse(Age < 27, "Yes", "No")) %>%
  ggplot(aes(x= Overall, y= ValueNumeric_pounds)) +
  geom_point(position = "jitter", shape = 21, color = "black", size = 2, aes(text = paste(Name, PositionGroup), fill = Under27)) +
  scale_fill_manual(values = c("red", "green")) +
  scale_y_continuous(labels = dollar_format(prefix = "€")) +
  ggtitle("Player's overall rating plotted against their value within the cluster")

ggplotly(aa, height = 600, width = 800)


# This enables populating images of players and their club logos
# Also, clicking the player image will direct you to the player's profile on sofifa.com
cluster_analysis$Photo<- paste(paste("<a href='https://sofifa.com/player/",
                                     cluster_analysis$ID,
                                     "'><img src=",cluster_analysis$Photo,"></img></a>",sep=''))
cluster_analysis$Club<- paste(paste("<img src=",cluster_analysis$`Club Logo`,"></img>  ",sep=''),
                              cluster_analysis$Club)


# create a function for k-means
# I will be using this for shiny app
return_similar_players <- function(player, num_results, return_within_fraction) {
  
  cluster_filter <- cluster_analysis$Cluster[cluster_analysis$Name == player]
  player_value <- cluster_analysis$ValueNumeric_pounds[cluster_analysis$Name == player]
  
  cluster_analysis %>%
    filter(Cluster == cluster_filter,
           ValueNumeric_pounds >= (player_value * (1- return_within_fraction)) & ValueNumeric_pounds <= (player_value * (1 + return_within_fraction))) %>%
    head(num_results)
  
}

# Test data
head(return_similar_players("H. Son", 15, .05))
return_similar_players("Anderson Talisca", 5, .01)