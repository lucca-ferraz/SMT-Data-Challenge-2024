setwd('2024_SMT_Data_Challenge')

library(dplyr)
data_directory <- '2024_SMT_Data_Challenge'

library(arrow)

game_info <- arrow::open_csv_dataset(paste0(data_directory,"/game_info"), 
                                     partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                     hive_style = F, 
                                     unify_schemas = T, 
                                     na = c("", "NA", "NULL", NA, "\\N"))

ball_pos <- arrow::open_csv_dataset(paste0(data_directory,"/ball_pos"), 
                                    partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                    hive_style = F, 
                                    unify_schemas = T, 
                                    na = c("", "NA", "NULL", NA, "\\N"))

game_events <- arrow::open_csv_dataset(paste0(data_directory,"/game_events"), 
                                       partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                       hive_style = F, 
                                       unify_schemas = T, 
                                       na = c("", "NA", "NULL", NA, "\\N"))

player_pos <- arrow::open_csv_dataset(paste0(data_directory,"/player_pos"), 
                                      partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                      hive_style = F, 
                                      unify_schemas = T, 
                                      na = c("", "NA", "NULL", NA, "\\N"))

team_info <- arrow::open_csv_dataset(paste0(data_directory,"/team_info.csv"), 
                                     hive_style = F, 
                                     unify_schemas = T, 
                                     na = c("", "NA", "NULL", NA, "\\N"))

all_game_events <- game_events %>%
  collect() %>%
  mutate(game_str_play_id = paste0(game_str, "_", play_per_game))

all_game_info <- game_info %>%
  collect() %>%
  mutate(game_inning_id = paste0(game_str, top_bottom, inning),
         batting_team = ifelse(top_bottom=="top", away_team, ifelse(top_bottom=="bottom", home_team, NA)),
         batting_team_game_id = paste0(game_str, batting_team),
         at_bat_game_str = paste0(at_bat, "_", game_str),
         game_str_play_id = paste0(game_str, "_", play_per_game))

all_game_info_cleaned <- all_game_info %>%
  filter(!is.na(pitcher)) %>%
  filter(!is.na(batter))

# player_pos_83_1 <- player_pos %>%
#   filter(Season=="Season_1883", HomeTeam=="Home1A") %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id)) %>%
#   collect()
# 
# player_pos_83_2 <- player_pos %>%
#   filter(Season=="Season_1883", HomeTeam=="Home2A") %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id)) %>%
#   collect()
# 
# player_pos_83_3 <- player_pos %>%
#   filter(Season=="Season_1883", HomeTeam=="Home3A") %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id)) %>%
#   collect()
# 
# player_pos_83_4 <- player_pos %>%
#   filter(Season=="Season_1883", HomeTeam=="Home4A") %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id)) %>%
#   collect()
# 
# player_pos_84_1 <- player_pos %>%
#   filter(Season=="Season_1884", HomeTeam=="Home1A") %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id)) %>%
#   collect()
# 
# player_pos_84_2 <- player_pos %>%
#   filter(Season=="Season_1884", HomeTeam=="Home2A") %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id)) %>%
#   collect()
# 
# player_pos_84_3 <- player_pos %>%
#   filter(Season=="Season_1884", HomeTeam=="Home3A") %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id)) %>%
#   collect()
# 
# player_pos_84_4 <- player_pos %>%
#   filter(Season=="Season_1884", HomeTeam=="Home4A") %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id)) %>%
#   collect()

all_ball_pos <- ball_pos %>%
  collect() %>%
  mutate(game_str_play_id = paste0(game_str, "_", play_id))

all_team_info <- team_info %>%
  collect()


##########################################

library(dplyr)
library(tidyr)

#ONLY HAVE TO DO ONCE. LOADED EVERYTHING INTO CSVs FOR MEMORY PURPOSE
# position_lookup <- data.frame(
#   player_position = 1:13,
#   position_name = c(
#     "Pitcher", "Catcher", "First Baseman", "Second Baseman", "Third Baseman", "Shortstop",
#     "Left Field", "Center Field", "Right Field", "Batter",
#     "First Baserunner", "Second Baserunner", "Third Baserunner"
#   )
# )
# 
# position_columns <- c("pitcher", "catcher", "first_base", "second_base", "third_base", 
#                       "shortstop", "left_field", "center_field", "right_field", 
#                       "batter", "first_baserunner", "second_baserunner", "third_baserunner")
# 
# player_positions_long <- all_game_info_cleaned %>%
#   pivot_longer(cols = all_of(position_columns), 
#                names_to = "position_name", 
#                values_to = "player_id") %>%
#   mutate(position_name = recode(position_name,
#                                 pitcher = "Pitcher",
#                                 catcher = "Catcher",
#                                 first_base = "First Baseman",
#                                 second_base = "Second Baseman",
#                                 third_base = "Third Baseman",
#                                 shortstop = "Shortstop",
#                                 left_field = "Left Field",
#                                 center_field = "Center Field",
#                                 right_field = "Right Field",
#                                 batter = "Batter",
#                                 first_baserunner = "First Baserunner",
#                                 second_baserunner = "Second Baserunner",
#                                 third_baserunner = "Third Baserunner"))
# 
# player_positions_long_selected <- player_positions_long %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_per_game)) %>%
#   select(game_str_play_id, position_name, player_id) %>%
#   filter(!is.na(player_id))
# 
# calculate_speed <- function(df) {
#   df %>%
#     arrange(timestamp) %>%
#     mutate(
#       delta_x = field_x - lag(field_x),
#       delta_y = field_y - lag(field_y),
#       delta_t = (timestamp - lag(timestamp)) / 1000,  # Convert milliseconds to seconds
#       speed = sqrt(delta_x^2 + delta_y^2) / delta_t   # Speed = distance / time
#     ) %>%
#     filter(!is.na(speed))  # Remove rows with NA values
# }
# 
# player_pos_83_1 <- player_pos_83_1 %>%
#   left_join(position_lookup, by = "player_position")
# 
# all_player_pos_expanded_83_1 <- player_pos_83_1 %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id))
# 
# all_player_pos_expanded_83_1 <- merge(all_player_pos_expanded_83_1, player_positions_long_selected, by = c("game_str_play_id", "position_name"))
# 
# write.csv(all_player_pos_expanded_83_1, "all_player_pos_expanded_83_1.csv", row.names=F)
# 
# 
# 
# ###
# 
# speed_data_83_1 <- all_player_pos_expanded_83_1 %>%
#   group_by(game_str_play_id, player_id) %>%
#   group_modify(~ calculate_speed(.x)) %>%
#   ungroup()
# 
# percentile_975_speed_per_player_83_1 <- speed_data_83_1 %>%
#   group_by(player_id) %>%
#   summarize(percentile_975_speed = quantile(speed, 0.975, na.rm = TRUE)) %>%
#   mutate(percentile_975_speed = percentile_975_speed * 0.681818)
# 
# write.csv(percentile_975_speed_per_player_83_1, "percentile_975_speed_per_player_83_1.csv", row.names=F)
# 
# ######
# 
# player_pos_83_2 <- player_pos_83_2 %>%
#   left_join(position_lookup, by = "player_position")
# 
# all_player_pos_expanded_83_2 <- player_pos_83_2 %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id))
# 
# all_player_pos_expanded_83_2 <- merge(all_player_pos_expanded_83_2, player_positions_long_selected, by = c("game_str_play_id", "position_name"))
# 
# write.csv(all_player_pos_expanded_83_2, "all_player_pos_expanded_83_2.csv", row.names=F)
# 
# speed_data_83_2 <- all_player_pos_expanded_83_2 %>%
#   group_by(game_str_play_id, player_id) %>%
#   group_modify(~ calculate_speed(.x)) %>%
#   ungroup()
# 
# percentile_975_speed_per_player_83_2 <- speed_data_83_2 %>%
#   group_by(player_id) %>%
#   summarize(percentile_975_speed = quantile(speed, 0.975, na.rm = TRUE)) %>%
#   mutate(percentile_975_speed = percentile_975_speed * 0.681818)
# 
# write.csv(percentile_975_speed_per_player_83_2, "percentile_975_speed_per_player_83_2.csv", row.names=F)
# 
# ######
# 
# player_pos_83_3 <- player_pos_83_3 %>%
#   left_join(position_lookup, by = "player_position")
# 
# all_player_pos_expanded_83_3 <- player_pos_83_3 %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id))
# 
# all_player_pos_expanded_83_3 <- merge(all_player_pos_expanded_83_3, player_positions_long_selected, by = c("game_str_play_id", "position_name"))
# 
# write.csv(all_player_pos_expanded_83_3, "all_player_pos_expanded_83_3.csv", row.names=F)
# 
# speed_data_83_3 <- all_player_pos_expanded_83_3 %>%
#   group_by(game_str_play_id, player_id) %>%
#   group_modify(~ calculate_speed(.x)) %>%
#   ungroup()
# 
# percentile_975_speed_per_player_83_3 <- speed_data_83_3 %>%
#   group_by(player_id) %>%
#   summarize(percentile_975_speed = quantile(speed, 0.975, na.rm = TRUE)) %>%
#   mutate(percentile_975_speed = percentile_975_speed * 0.681818)
# 
# write.csv(percentile_975_speed_per_player_83_3, "percentile_975_speed_per_player_83_3.csv", row.names=F)
# 
# ######
# 
# player_pos_83_4 <- player_pos_83_4 %>%
#   left_join(position_lookup, by = "player_position")
# 
# all_player_pos_expanded_83_4 <- player_pos_83_4 %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id))
# 
# all_player_pos_expanded_83_4 <- merge(all_player_pos_expanded_83_4, player_positions_long_selected, by = c("game_str_play_id", "position_name"))
# 
# write.csv(all_player_pos_expanded_83_4, "all_player_pos_expanded_83_4.csv", row.names=F)
# 
# speed_data_83_4 <- all_player_pos_expanded_83_4 %>%
#   group_by(game_str_play_id, player_id) %>%
#   group_modify(~ calculate_speed(.x)) %>%
#   ungroup()
# 
# percentile_975_speed_per_player_83_4 <- speed_data_83_4 %>%
#   group_by(player_id) %>%
#   summarize(percentile_975_speed = quantile(speed, 0.975, na.rm = TRUE)) %>%
#   mutate(percentile_975_speed = percentile_975_speed * 0.681818)
# 
# write.csv(percentile_975_speed_per_player_83_4, "percentile_975_speed_per_player_83_4.csv", row.names=F)
# 
# ######
# 
# player_pos_84_1 <- player_pos_84_1 %>%
#   left_join(position_lookup, by = "player_position")
# 
# all_player_pos_expanded_84_1 <- player_pos_84_1 %>%
#   mutate(game_str_play_id = paste0(game_str, "_", play_id))
# 
# all_player_pos_expanded_84_1 <- merge(all_player_pos_expanded_84_1, player_positions_long_selected, by = c("game_str_play_id", "position_name"))
# 
# write.csv(all_player_pos_expanded_84_1, "all_player_pos_expanded_84_1.csv", row.names=F)
# 
# speed_data_84_1 <- all_player_pos_expanded_84_1 %>%
#   group_by(game_str_play_id, player_id) %>%
#   group_modify(~ calculate_speed(.x)) %>%
#   ungroup()
# 
# percentile_975_speed_per_player_84_1 <- speed_data_84_1 %>%
#   group_by(player_id) %>%
#   summarize(percentile_975_speed = quantile(speed, 0.975, na.rm = TRUE)) %>%
#   mutate(percentile_975_speed = percentile_975_speed * 0.681818)
# 
# write.csv(percentile_975_speed_per_player_84_1, "percentile_975_speed_per_player_84_1.csv", row.names=F)
# 
# 
# 
# 
# ######
# 
# player_pos_84_2 <- player_pos_84_2 %>%
#   left_join(position_lookup, by = "player_position")
# 
# all_player_pos_expanded_84_2 <- merge(player_pos_84_2, player_positions_long_selected, by = c("game_str_play_id", "position_name"))
# 
# write.csv(all_player_pos_expanded_84_2, "all_player_pos_expanded_84_2.csv", row.names=F)
# 
# speed_data_84_2 <- all_player_pos_expanded_84_2 %>%
#   group_by(game_str_play_id, player_id) %>%
#   group_modify(~ calculate_speed(.x)) %>%
#   ungroup()
# 
# percentile_975_speed_per_player_84_2 <- speed_data_84_2 %>%
#   group_by(player_id) %>%
#   summarize(percentile_975_speed = quantile(speed, 0.975, na.rm = TRUE)) %>%
#   mutate(percentile_975_speed = percentile_975_speed * 0.681818)
# 
# write.csv(percentile_975_speed_per_player_84_2, "percentile_975_speed_per_player_84_2.csv", row.names=F)
# 
# 
# ######
# 
# player_pos_84_3 <- player_pos_84_3 %>%
#   left_join(position_lookup, by = "player_position")
# 
# all_player_pos_expanded_84_3 <- merge(player_pos_84_3, player_positions_long_selected, by = c("game_str_play_id", "position_name"))
# 
# write.csv(all_player_pos_expanded_84_3, "all_player_pos_expanded_84_3.csv", row.names=F)
# 
# speed_data_84_3 <- all_player_pos_expanded_84_3 %>%
#   group_by(game_str_play_id, player_id) %>%
#   group_modify(~ calculate_speed(.x)) %>%
#   ungroup()
# 
# percentile_975_speed_per_player_84_3 <- speed_data_84_3 %>%
#   group_by(player_id) %>%
#   summarize(percentile_975_speed = quantile(speed, 0.975, na.rm = TRUE)) %>%
#   mutate(percentile_975_speed = percentile_975_speed * 0.681818)
# 
# write.csv(percentile_975_speed_per_player_84_3, "percentile_975_speed_per_player_84_3.csv", row.names=F)
# 
# 
# #######
# 
# player_pos_84_4 <- player_pos_84_4 %>%
#   left_join(position_lookup, by = "player_position")
# 
# all_player_pos_expanded_84_4 <- merge(player_pos_84_4, player_positions_long_selected, by = c("game_str_play_id", "position_name"))
# 
# write.csv(all_player_pos_expanded_84_4, "all_player_pos_expanded_84_4.csv", row.names=F)
# 
# speed_data_84_4 <- all_player_pos_expanded_84_4 %>%
#   group_by(game_str_play_id, player_id) %>%
#   group_modify(~ calculate_speed(.x)) %>%
#   ungroup()
# 
# percentile_975_speed_per_player_84_4 <- speed_data_84_4 %>%
#   group_by(player_id) %>%
#   summarize(percentile_975_speed = quantile(speed, 0.975, na.rm = TRUE)) %>%
#   mutate(percentile_975_speed = percentile_975_speed * 0.681818)
# 
# write.csv(percentile_975_speed_per_player_84_4, "percentile_975_speed_per_player_84_4.csv", row.names=F)


##############

percentile_975_speed_per_player_83_1 <- read.csv("percentile_975_speed_per_player_83_1.csv")
percentile_975_speed_per_player_83_2 <- read.csv("percentile_975_speed_per_player_83_2.csv")
percentile_975_speed_per_player_83_3 <- read.csv("percentile_975_speed_per_player_83_3.csv")
percentile_975_speed_per_player_83_4 <- read.csv("percentile_975_speed_per_player_83_4.csv")

percentile_975_speed_per_player_84_1 <- read.csv("percentile_975_speed_per_player_84_1.csv")
percentile_975_speed_per_player_84_2 <- read.csv("percentile_975_speed_per_player_84_2.csv")
percentile_975_speed_per_player_84_3 <- read.csv("percentile_975_speed_per_player_84_3.csv")
percentile_975_speed_per_player_84_4 <- read.csv("percentile_975_speed_per_player_84_4.csv")

percentile_975_speed_per_player_all <- rbind(percentile_975_speed_per_player_83_1, 
                                             percentile_975_speed_per_player_83_2,
                                             percentile_975_speed_per_player_83_3,
                                             percentile_975_speed_per_player_83_4,
                                             percentile_975_speed_per_player_84_1,
                                             percentile_975_speed_per_player_84_2,
                                             percentile_975_speed_per_player_84_3,
                                             percentile_975_speed_per_player_84_4)

percentile_975_speed_per_player_all <- percentile_975_speed_per_player_all %>%
  group_by(player_id) %>%
  summarise(miles_per_hour = max(percentile_975_speed)) %>%
  mutate(feet_per_second = miles_per_hour * 5280 / 3600) %>%
  filter(miles_per_hour<50)

hist(percentile_975_speed_per_player_all$miles_per_hour)

write.csv(percentile_975_speed_per_player_all, "percentile_975_speed_per_player_all.csv", row.names = F)


##########################################

library(dplyr)

# Filter for plays where the ball was hit
ball_hit_plays <- all_game_events %>%
  filter(event_code == 4) %>%
  group_by(game_str_play_id) %>%
  summarise(hit_count = n()) %>%
  select(game_str_play_id)

plays_ball_hit <- ball_hit_plays$game_str_play_id

# Create an empty data frame to store the results
advance_from_first_to_second_or_third <- data.frame(
  game_str_play_id = character(),
  advanced_to_third = integer(),
  stringsAsFactors = FALSE
)

# Loop through the plays
for (i in 1:(nrow(all_game_info_cleaned) - 1)) {
  
  # Extract relevant values for readability and debugging
  first_baserunner <- all_game_info_cleaned$first_baserunner[i]
  third_baserunner_next <- all_game_info_cleaned$third_baserunner[i + 1]
  second_baserunner_next <- all_game_info_cleaned$second_baserunner[i + 1]
  first_baserunner_next <- all_game_info_cleaned$first_baserunner[i + 1]
  batter_current <- all_game_info_cleaned$batter[i]
  batter_next <- all_game_info_cleaned$batter[i + 1]
  current_play_id <- all_game_info_cleaned$game_str_play_id[i]
  
  # Check conditions for advancing from first to second or third base
  if (!is.na(first_baserunner) &&  # First baserunner is not NA
      current_play_id %in% plays_ball_hit &&  # Play is a ball hit play
      (isTRUE(third_baserunner_next == first_baserunner) ||  # Advanced to third
       isTRUE(second_baserunner_next == first_baserunner)) &&  # Advanced to second
      (isTRUE(batter_next != batter_current) || is.na(batter_next))) {  # Batters are different or next batter is NA
    
    # Check if the previous play's batter is now on first base
    if (isTRUE(first_baserunner_next == batter_current)) {
      # Determine if the runner advanced to third
      advanced_to_third <- ifelse(isTRUE(third_baserunner_next == first_baserunner), 1, 0)
      
      # Append to the result data frame
      advance_from_first_to_second_or_third <- rbind(
        advance_from_first_to_second_or_third,
        data.frame(
          game_str_play_id = current_play_id,
          advanced_to_third = advanced_to_third,
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

ball_pos_needed <- all_ball_pos %>%
  filter(game_str_play_id %in% advance_from_first_to_second_or_third$game_str_play_id)

ball_pos_needed <- merge(ball_pos_needed, advance_from_first_to_second_or_third, by = "game_str_play_id") %>%
  group_by(game_str_play_id) %>%
  slice(-(1:10)) %>%
  ungroup()

ball_location_first_to_third_or_second_advance <- ball_pos_needed %>%
  group_by(game_str_play_id) %>%
  slice(which.max(ball_position_y))



library(ggplot2)
library(ggpubr)
library(jpeg)
img <- readJPEG("dodgers_field.jpeg")
ball_location_first_to_third_or_second_advance_adjusted <- ball_location_first_to_third_or_second_advance %>%
  mutate(new_x = ball_position_x + 20,
         new_y = ball_position_y + 20)
advance_to_third_plot <- ggplot(ball_location_first_to_third_or_second_advance_adjusted, aes(x = new_x, y = new_y, color = factor(advanced_to_third))) +
  background_image(img) +
  geom_point() +
  scale_color_manual(values = c("gray", "yellow"), labels = c("Not Advanced", "Advanced")) +  # Adjust colors and labels as needed
  labs(
    x = "X Coordinate",
    y = "Y Coordinate",
  ) +
  scale_y_continuous(limits = c(20, max(ball_location_first_to_third_or_second_advance$ball_position_y))) +
  scale_x_continuous(limits = c(min(ball_location_first_to_third_or_second_advance$ball_position_x), max(ball_location_first_to_third_or_second_advance$ball_position_x))) +
  theme_void() +
  guides(color="none", title = "none")

ggsave("advance_to_third_plot.png", advance_to_third_plot)


# ball_location_first_to_third_or_second_advance_2 <- ball_location_first_to_third_or_second_advance %>%
#   mutate(rounded_x = round(ball_position_x),
#          rounded_y = round(ball_position_y),
#          advance_rate = mean(advanced_to_third)) %>%
#   ungroup() %>%
#   select(rounded_x, rounded_y, advance_rate)
# 
# all_coordinates <- data.frame(
#   rounded_x = integer(),
#   rounded_y = integer()
#   )
# 
# all_coordinates<- data.frame(
#   rounded_x = rep(-250:250, each = 400)
# )
# 
# all_coordinates$rounded_y <- rep(0:399, times = 501)
# all_coordinates$advance_rate <- 0
# 
# ball_location_first_to_third_or_second_advance_2_with_all_coordinates <- rbind(ball_location_first_to_third_or_second_advance_2, all_coordinates)
# 
# ball_location_first_to_third_or_second_advance_2_with_all_coordinates <- ball_location_first_to_third_or_second_advance_2_with_all_coordinates %>%
#   group_by(rounded_x, rounded_y) %>%
#   summarise(advance_rate = max(advance_rate))
# 
# library("lattice")
# levelplot(advance_rate ~ rounded_x*rounded_y, data=ball_location_first_to_third_or_second_advance_2_with_all_coordinates  ,xlab="X",
#           main="")
# 


#I want a model that predicts likelihood of advancing from 3rd from 1st on a single depending on ball location and speed

first_base_runners <- all_game_info %>%
  select(first_baserunner, game_str_play_id)

advance_2 <- merge(advance_from_first_to_second_or_third, first_base_runners, by = "game_str_play_id")

advance_3 <- merge(advance_2, percentile_975_speed_per_player_all, by.x = "first_baserunner", by.y = "player_id") %>%
  select(-miles_per_hour)

ball_pos_needed <- all_ball_pos %>%
  filter(game_str_play_id %in% advance_from_first_to_second_or_third$game_str_play_id)

ball_pos_needed_2 <- merge(ball_pos_needed, advance_3, by = "game_str_play_id") %>%
  group_by(game_str_play_id) %>%
  slice(-(1:10)) %>%
  ungroup()

ball_location_first_to_third_or_second_advance_2 <- ball_pos_needed_2 %>%
  group_by(game_str_play_id) %>%
  slice(which.max(ball_position_y))


ball_location_first_to_third_or_second_advance_adjusted_2 <- ball_location_first_to_third_or_second_advance_2 %>%
  mutate(new_x = ball_position_x + 20,
         new_y = ball_position_y + 20) %>%
  arrange((advanced_to_third)) %>%
  filter(!ball_position_x%in%c(-1,0,1) & !ball_position_y<65)


advance_to_third_plot_2 <- ggplot(ball_location_first_to_third_or_second_advance_adjusted_2, aes(x = new_x, y = new_y)) +
  background_image(img) +
  geom_point(aes(fill=factor(advanced_to_third), size = ((feet_per_second)^2) / 5), 
             colour="black",pch=21) +
  scale_fill_manual(values = c(alpha("gray69", 1), "dodgerblue"), labels = c("Not Advanced", "Advanced")) +
  labs(
    x = "X Coordinate",
    y = "Y Coordinate",
  ) +
  scale_y_continuous(limits = c(20, max(ball_location_first_to_third_or_second_advance$ball_position_y))) +
  scale_x_continuous(limits = c(min(ball_location_first_to_third_or_second_advance$ball_position_x), max(ball_location_first_to_third_or_second_advance$ball_position_x))) +
  scale_size_continuous(range = c(1, 4), breaks = c(1, 2.25, 4)) +
  theme_void() +
  guides(fill = "none", size = "none")

ggsave("advance_to_third_plot_2.png", advance_to_third_plot_2)

model_data <- ball_location_first_to_third_or_second_advance_adjusted_2 %>%
  ungroup()%>%
  select(advanced_to_third, ball_position_x, ball_position_y, ball_position_z, feet_per_second)

library(caret)
set.seed(117)

index <- createDataPartition(model_data$advanced_to_third, p = 0.7, list = FALSE)
train_data <- model_data[index, ]
test_data <- model_data[-index, ]

lm_model1 <- lm(advanced_to_third ~ ., data = model_data)
summary(lm_model1)

library(MASS)
step_model <- stepAIC(lm_model1, direction = "both",
                      trace = FALSE)
summary(step_model)

library(mgcv)
gam_model <- gam(advanced_to_third ~ s(ball_position_x) + s(ball_position_y) + 
                   s(ball_position_z) + s(feet_per_second), data = model_data)
summary(gam_model)

gam_predictions <- predict(gam_model, test_data)
test_data$gam_predictions <- gam_predictions

cor(test_data$gam_predictions, test_data$advanced_to_third)
#
gam_model_2 <- gam(advanced_to_third ~ s(ball_position_x) + s(ball_position_y)+ 
                     s(feet_per_second), data = model_data)
summary(gam_model_2)

gam_predictions_2 <- predict(gam_model_2, test_data)
test_data$gam_predictions_2 <- gam_predictions_2


best_model <- gam_model


summary(ball_location_first_to_third_or_second_advance_2$advanced_to_third)

right_field <- ball_location_first_to_third_or_second_advance_2 %>%
  filter(ball_position_x >31.5, ball_position_y>128)

left_field <- ball_location_first_to_third_or_second_advance_2 %>%
  filter(ball_position_x < -31.5, ball_position_y>128)

center_field <- ball_location_first_to_third_or_second_advance_2 %>%
  filter(ball_position_x <=31.5 | ball_position_x>= -31.5, ball_position_y>128)

summary(right_field$advanced_to_third)

summary(left_field$advanced_to_third)

summary(center_field$advanced_to_third)


right_field_advanced <- right_field %>%
  filter(advanced_to_third==1)

right_field_no_advance <- right_field %>%
  filter(advanced_to_third==0)

summary(right_field_advanced$feet_per_second)

summary(right_field_no_advance$feet_per_second)


center_field_advanced <- center_field %>%
  filter(advanced_to_third==1)

center_field_no_advance <- center_field %>%
  filter(advanced_to_third==0)

summary(center_field_advanced$feet_per_second)

summary(center_field_no_advance$feet_per_second)



left_field_advanced <- left_field %>%
  filter(advanced_to_third==1)

left_field_no_advance <- left_field %>%
  filter(advanced_to_third==0)

summary(left_field_advanced$feet_per_second)

summary(left_field_no_advance$feet_per_second)




all_singles <- function(){
  singles_info <- all_game_info |> 
    filter(lead(first_baserunner) == batter)
  
  singles_game_play_id <- singles_info$game_str_play_id
  
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% singles_game_play_id) %>%
    filter(event_code == 4)
  
  # Ensuring the `batter` column is properly referenced
  batter_ids <- singles_info$batter[singles_info$game_str_play_id %in% game_events_wanted$game_str_play_id]
  
  data.frame(game_str_play_id = game_events_wanted$game_str_play_id, batter_id = batter_ids)
}

# Call the function to get all singles
singles_data <- all_singles()




single_ball_pos <- all_ball_pos %>%
  filter(game_str_play_id %in% singles_data$game_str_play_id) %>%
  group_by(game_str_play_id) %>%
  slice(-(1:10)) %>%
  ungroup() %>%
  group_by(game_str_play_id) %>%
  slice(which.max(ball_position_y)) %>%
  mutate(location = ifelse(ball_position_x>31.5 & ball_position_y>128, "Right Field", ifelse(ball_position_x< -31.5 & ball_position_y>128, "Left Field", ifelse((ball_position_x < 31.5 | ball_position_x > -31.5) & ball_position_y>128, "Center Field", NA))))

single_ball_pos_select <- subset(single_ball_pos, select = c(game_str_play_id, location))

singles_data <- merge(singles_data, single_ball_pos_select, by = "game_str_play_id")  

location_percentage_data <- singles_data %>%
  group_by(batter_id) %>%
  summarise(
    left_field = sum(location == 'Left Field', na.rm = T) / n(),
    right_field = sum(location == 'Right Field', na.rm = T) / n(),
    center_field = sum(location == 'Center Field', na.rm = T) / n(),
    NA_field = sum(is.na(location)) / n()
  )




na_data <- ball_location_first_to_third_or_second_advance_2 %>%
  filter(ball_position_y<128)

summary(na_data$advanced_to_third)



stats_per_player <- merge(stats_per_player, location_percentage_data, by.x = "player", by.y = "batter_id")



#######################################################

homeruns <- all_game_events %>%
  filter(event_code==11)

homeruns <- merge(homeruns, all_game_info_cleaned, by = "game_str_play_id")

homeruns <- subset(homeruns, select = c("batter", "inning"))

homerun_batters <- homeruns %>%
  group_by(batter) %>%
  summarise(homeruns = n()) %>%
  arrange(desc(homeruns))

homerun_batters_by_inning <- homeruns %>%
  group_by(batter, inning) %>%
  summarise(homeruns=n()) %>%
  arrange(desc(homeruns))


########################################

batting_orders <- all_game_info %>%
  filter(!is.na(batter) & !is.na(at_bat)) %>%
  group_by(batting_team_game_id, at_bat) %>%
  summarise(batter = first(batter),
            inning = first(inning),
            game_str = first(game_str),
            batting_team = first(batting_team)) %>%
  ungroup() %>%
  group_by(batting_team_game_id) %>%
  filter(n() >= 9) %>%
  arrange(batting_team_game_id, at_bat) %>%
  mutate(batting_appearance = row_number(),
         batting_order = ifelse((batting_appearance %% 9)==0, 9, (batting_appearance %% 9)),
         game_inning = paste0(batting_team_game_id, inning))

simple_batting_order_frequency_table <- batting_orders %>%
  group_by(batting_order) %>%
  summarise(at_bats = n())

write.csv(simple_batting_order_frequency_table, "simple_batting_order_frequency_table.csv", row.names = FALSE)

number_of_inning_appearances <- batting_orders %>%
  group_by(game_inning) %>%
  summarise(inning = first(inning))

inning_counts <- number_of_inning_appearances %>%
  count(inning, name = "inning_appearances")

inning_batting_order <- batting_orders %>%
  ungroup%>%
  select(inning, batting_order)

inning_batting_order <- merge(inning_batting_order, inning_counts, by = "inning")

likelihood_table <- inning_batting_order %>%
  group_by(inning, batting_order) %>%
  summarise(at_bats = n(),
            inning_appearances = first(inning_appearances)) %>%
  mutate(likelihood_of_appearing = at_bats / inning_appearances)

#There seems to be some data errors, as batting orders 1 through 3 should have a 100% likelihood of appearing in the first inning
#Because of this error, I am manually changing the data so it reads out correctly. In terms of if there could be data errors with the other rows,
#it is possible, but I believe it is extremely minuscule if so. The first 3 batters info is minuscule as well, but it is known that there is a 100% likelihood,
#so I feel confident in changing the data myself.

likelihood_table <- likelihood_table %>%
  mutate(at_bats = ifelse(inning == 1 & batting_order %in% c(1,2,3), 576, at_bats),
         likelihood_of_appearing = ifelse(inning == 1 & batting_order %in% c(1,2,3), 1, likelihood_of_appearing)) %>%
  select(inning, batting_order, likelihood_of_appearing)

write.csv(likelihood_table, "batting_order_appearance_likelihood.csv", row.names = FALSE)


#####################################################

at_bat_by_player <- function(player){
  player_info <- all_game_info %>%
    filter(!is.na(batter)) %>%
    group_by(at_bat_game_str) %>%
    summarise(batter = first(batter)) %>%
    ungroup() %>%
    group_by(batter) %>%
    summarise(at_bats = n()) %>%
    filter(batter == player)
  if(nrow(player_info) == 0) return(0)
  return(player_info$at_bats)
}

walk_by_player <- function(player){
  player_info <- all_game_info |> 
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  first_bases_gotten <- nrow(player_info |> 
                               filter(batter == player & lead(first_baserunner)==player))
  first_based <- (player_info |> 
                    filter(batter == player & lead(first_baserunner)==player))
  first_based_game_play_id <- first_based$game_str_play_id
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% first_based_game_play_id) %>%
    filter(event_code==4)
  singles <- nrow(game_events_wanted)
  walks <- first_bases_gotten - singles
  walks
}

single_by_player <- function(player){
  player_info <- all_game_info |> 
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  first_based <- (player_info |> 
                    filter(batter == player & lead(first_baserunner)==player))
  first_based_game_play_id <- first_based$game_str_play_id
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% first_based_game_play_id) %>%
    filter(event_code==4)
  nrow(game_events_wanted)
}

double_by_player <- function(player){
  player_info <- all_game_info |> 
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  nrow(player_info |> 
         filter(batter == player & lead(second_baserunner)==player))
}

triple_by_player <- function(player){
  player_info <- all_game_info |> 
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  nrow(player_info |> 
         filter(batter == player & lead(third_baserunner)==player))
}

homeruns <- all_game_events %>%
  filter(event_code==11)

homeruns <- merge(homeruns, all_game_info_cleaned, by = "game_str_play_id")

homerun_batters <- homeruns %>%
  select(batter) %>%
  group_by(batter) %>%
  summarise(homeruns = n()) %>%
  arrange(desc(homeruns))

homerun_by_player <- function(player){
  homerun <- homerun_batters %>%
    filter(batter == player)
  if(nrow(homerun) == 0) return(0)
  return(homerun$homeruns)
}


single_by_player(572)
double_by_player(76788)
triple_by_player(76788)
homerun_by_player(76788)
at_bat_by_player(572)
walk_by_player(572)

all_batters <- unique(na.omit(all_game_info$batter))

stats_per_player <- tibble(player = all_batters)

library(purrr)
stats_per_player <- stats_per_player %>%
  mutate(
    at_bats = map_dbl(player, ~at_bat_by_player(.x)),
    walks = map_dbl(player, ~walk_by_player(.x)),
    singles = map_dbl(player, ~single_by_player(.x)),
    doubles = map_dbl(player, ~double_by_player(.x)),
    triples = map_dbl(player, ~triple_by_player(.x)),
    home_runs = map_dbl(player, ~homerun_by_player(.x)),
    on_base_percentage = (walks + singles + doubles + triples + home_runs) / at_bats,
    wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples) + (2.101 * home_runs)) / at_bats
  )


stats_per_player <- stats_per_player %>%
  mutate(at_bats = ifelse(at_bats<(walks+singles+doubles+triples+home_runs),walks+singles+doubles+triples+home_runs, at_bats),
         on_base_percentage = (walks + singles + doubles + triples + home_runs) / at_bats,
         wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples) + (2.101 * home_runs)) / at_bats)

stats_per_player$homerun_percentage <- stats_per_player$home_runs / stats_per_player$at_bats
stats_per_player$walk_percentage <- stats_per_player$walks / stats_per_player$at_bats
stats_per_player$single_percentage <- stats_per_player$singles / stats_per_player$at_bats
stats_per_player$double_percentage <- stats_per_player$doubles / stats_per_player$at_bats
stats_per_player$triple_percentage <- stats_per_player$triples / stats_per_player$at_bats

write.csv(stats_per_player, "stats_per_player.csv", row.names = F)

stats_per_player <- read.csv("stats_per_player.csv")

percentile_975_speed_per_player_all <- read.csv("percentile_975_speed_per_player_all.csv")

stats_per_player <- merge(stats_per_player, percentile_975_speed_per_player_all, by.x = "player", by.y = "player_id")

ecdf_speed <- ecdf(stats_per_player$feet_per_second)

stats_per_player$speed_percentile <- ecdf_speed(stats_per_player$feet_per_second) * 100

write.csv(stats_per_player, "stats_per_player.csv", row.names = F)

stats_per_player <- read.csv("stats_per_player.csv")
stats_per_player_handedness <- merge(stats_per_player, batter_handedness, by.x = "player", by.y = "batter")
stats_per_player_handedness_against_righty <- stats_per_player_handedness %>%
  mutate(
    single_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "left_right"],
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    double_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "left_right"],
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    triple_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "left_right"],
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    homerun_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "left_right"],
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "right_right"]
    ))



stats_per_player_handedness_against_lefty <- stats_per_player_handedness %>%
  mutate(
    single_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "right_left"],
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    double_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "right_left"],
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    triple_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "right_left"],
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    homerun_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "right_left"],
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "left_left"]
    ))

write.csv(stats_per_player_handedness_against_righty, "stats_per_player_handedness_against_righty.csv", row.names = F)
write.csv(stats_per_player_handedness_against_lefty, "stats_per_player_handedness_against_lefty.csv", row.names = F)


#########################################


total_directional <- all_ball_pos %>%
  filter(game_str_play_id %in% ball_hit_plays$game_str_play_id) %>%
  group_by(game_str_play_id) %>%
  slice(which.max(ball_position_y)) %>%
  filter(ball_position_y>60)

all_game_info_batter <- subset(all_game_info_cleaned, select = c("batter", "game_str_play_id"))

total_directional <- merge(total_directional, all_game_info_batter, by = "game_str_play_id")

total_directional$location = ifelse(total_directional$ball_position_x>31.5, "Right", ifelse(total_directional$ball_position_x< -31.5, "Left", ifelse((total_directional$ball_position_x < 31.5 | total_directional$ball_position_x > -31.5), "Center", NA)))                                                 

location_percentage_data_all_hits <- total_directional %>%
  group_by(batter) %>%
  summarise(
    left = sum(location == 'Left', na.rm = T) / n(),
    right = sum(location == 'Right', na.rm = T) / n(),
    center = sum(location == 'Center', na.rm = T) / n()
  )

write.csv(location_percentage_data_all_hits, "location_percentage_data_all_hits.csv")


batter_out_pct <- read.csv("batter_out_pct.csv")

batter_out_pct_subset <- subset(batter_out_pct, select = c("batter", "strikeouts", "fly_out_pct"))

right_only <- subset(location_percentage_data_all_hits, select = c("batter", "right"))

stats_per_player <- merge(stats_per_player, right_only, by.x = "player", by.y = "batter")

stats_per_player <- merge(stats_per_player, batter_out_pct_subset, by.x = "player", by.y = "batter")

write.csv(stats_per_player, "stats_per_player.csv")


#####################################################

batting_stats_by_inning_at_bat <- read.csv("batting_stats_by_inning_at_bat.csv")

plot(batting_stats_by_inning_at_bat$ops)

### Not significant enough to include

#####################################################

first_base_no_second_base <- all_game_info %>%
  filter(!is.na(first_baserunner), is.na(second_baserunner))

first_base_no_second_base_id <- first_base_no_second_base$game_str_play_id

stolen_base_data <- all_game_info_cleaned %>%
  filter(inning>0)%>%
  arrange(at_bat_game_str, play_per_game) %>%  # Ensure data is sorted
  group_by(at_bat_game_str) %>%  # Group by game and at bat
  mutate(starting_first_baserunner = first(first_baserunner),
         ending_second_baserunner = last(second_baserunner),
         ending_first_baserunner = last(first_baserunner),
         steal = ifelse(starting_first_baserunner == ending_second_baserunner, 1, 0),
         caught_stealing = ifelse(is.na(ending_first_baserunner) & is.na(ending_second_baserunner) & !is.na(starting_first_baserunner), 1, 0)
  ) %>%
  ungroup()

sum(stolen_base_data$steal, na.rm=T)
sum(stolen_base_data$caught_stealing)
###This is not the amount of stolen bases and caught stolen bases, 
#but the amount of rows of a certain at bat in which a stolen base or caught stolen base has occurred.
#These numbers are only for me to see if it seems about right, and it does

stolen_base_data_grouped <- stolen_base_data %>%
  group_by(at_bat_game_str) %>%
  summarise(stolen_base = max(steal),
            stolen_base_caught = max(caught_stealing),
            runner = first(starting_first_baserunner))

stolen_base_by_player <- stolen_base_data_grouped %>%
  group_by(runner) %>%
  summarise(stolen_bases = sum(stolen_base, na.rm = T),
            caught_stolen_bases = sum(stolen_base_caught, na.rm = T))

running_appearances_first_base <- all_game_info %>%
  filter(!is.na(first_baserunner)) %>%
  group_by(at_bat_game_str) %>%
  summarise(runner = first(first_baserunner)) %>%
  group_by(runner) %>%
  summarise(running_appearances = n())

stolen_base_by_player <- merge(stolen_base_by_player, running_appearances_first_base, by = "runner")

stolen_base_by_player <- stolen_base_by_player %>%
  mutate(stolen_base_percentage = stolen_bases / running_appearances,
         caught_stolen_base_percentage = caught_stolen_bases / running_appearances)

stolen_base_selected <- subset(stolen_base_by_player, select = c("runner", "stolen_base_percentage", "caught_stolen_base_percentage"))

stats_per_player <- merge(stats_per_player, stolen_base_selected, by.x = "player", by.y = "runner")

write.csv(stats_per_player, "stats_per_player.csv", row.names = F)




#####

second_base_no_third_base <- all_game_info %>%
  filter(!is.na(second_baserunner), is.na(third_baserunner))

second_base_no_third_base_id <- second_base_no_third_base$game_str_play_id

stolen_base_data_2_to_3 <- all_game_info_cleaned %>%
  filter(inning>0)%>%
  arrange(at_bat_game_str, play_per_game) %>%  # Ensure data is sorted
  group_by(at_bat_game_str) %>%  # Group by game and at bat
  mutate(starting_second_baserunner = first(second_baserunner),
         ending_third_baserunner = last(third_baserunner),
         ending_second_baserunner = last(second_baserunner),
         steal = ifelse(starting_second_baserunner == ending_third_baserunner, 1, 0),
         caught_stealing = ifelse(is.na(ending_second_baserunner) & is.na(ending_third_baserunner) & !is.na(starting_second_baserunner), 1, 0)
  ) %>%
  ungroup()

sum(stolen_base_data_2_to_3$steal, na.rm=T)
sum(stolen_base_data_2_to_3$caught_stealing)
###This is not the amount of stolen bases and caught stolen bases, 
#but the amount of rows of a certain at bat in which a stolen base or caught stolen base has occurred.
#These numbers are only for me to see if it seems about right, and it does

stolen_base_data_grouped_2_to_3 <- stolen_base_data_2_to_3 %>%
  group_by(at_bat_game_str) %>%
  summarise(stolen_base = max(steal),
            stolen_base_caught = max(caught_stealing),
            runner = first(starting_second_baserunner))

stolen_base_by_player_2_to_3 <- stolen_base_data_grouped_2_to_3 %>%
  group_by(runner) %>%
  summarise(stolen_bases = sum(stolen_base, na.rm = T),
            caught_stolen_bases = sum(stolen_base_caught, na.rm = T))

running_appearances_second_base <- all_game_info %>%
  filter(!is.na(second_baserunner)) %>%
  group_by(at_bat_game_str) %>%
  summarise(runner = first(second_baserunner)) %>%
  group_by(runner) %>%
  summarise(running_appearances = n())

stolen_base_by_player_2_to_3 <- merge(stolen_base_by_player_2_to_3, running_appearances_second_base, by = "runner")

stolen_base_by_player_2_to_3 <- stolen_base_by_player_2_to_3 %>%
  mutate(stolen_base_percentage_2_to_3 = stolen_bases / running_appearances,
         caught_stolen_base_percentage_2_to_3 = caught_stolen_bases / running_appearances)

stolen_base_selected_2_to_3 <- subset(stolen_base_by_player_2_to_3, select = c("runner", "stolen_base_percentage_2_to_3", "caught_stolen_base_percentage_2_to_3"))

stats_per_player <- merge(stats_per_player, stolen_base_selected_2_to_3, by.x = "player", by.y = "runner")

write.csv(stats_per_player, "stats_per_player.csv", row.names = F)


#############


first_base_no_second_base_1A <- all_game_info %>%
  filter(!is.na(first_baserunner), is.na(second_baserunner)) %>%
  filter(batting_team == "Home1A")

first_base_no_second_base_id_1A <- first_base_no_second_base_1A$game_str_play_id

stolen_base_data_1A <- all_game_info_cleaned %>%
  filter(batting_team=="Home1A")%>%
  filter(inning>0)%>%
  arrange(at_bat_game_str, play_per_game) %>%  # Ensure data is sorted
  group_by(at_bat_game_str) %>%  # Group by game and at bat
  mutate(starting_first_baserunner = first(first_baserunner),
         ending_second_baserunner = last(second_baserunner),
         ending_first_baserunner = last(first_baserunner),
         steal = ifelse(starting_first_baserunner == ending_second_baserunner, 1, 0),
         caught_stealing = ifelse(is.na(ending_first_baserunner) & is.na(ending_second_baserunner) & !is.na(starting_first_baserunner), 1, 0)
  ) %>%
  ungroup()

sum(stolen_base_data_1A$steal, na.rm=T)
sum(stolen_base_data_1A$caught_stealing)
###This is not the amount of stolen bases and caught stolen bases, 
#but the amount of rows of a certain at bat in which a stolen base or caught stolen base has occurred.
#These numbers are only for me to see if it seems about right, and it does

stolen_base_data_grouped_1A<- stolen_base_data_1A %>%
  group_by(at_bat_game_str) %>%
  summarise(stolen_base = max(steal),
            stolen_base_caught = max(caught_stealing),
            runner = first(starting_first_baserunner))

stolen_base_by_player_1A <- stolen_base_data_grouped_1A %>%
  group_by(runner) %>%
  summarise(stolen_bases = sum(stolen_base, na.rm = T),
            caught_stolen_bases = sum(stolen_base_caught, na.rm = T))

running_appearances_first_base_1A <- all_game_info %>%
  filter(batting_team=="Home1A")%>%
  filter(!is.na(first_baserunner)) %>%
  group_by(at_bat_game_str) %>%
  summarise(runner = first(first_baserunner)) %>%
  group_by(runner) %>%
  summarise(running_appearances = n())

stolen_base_by_player_1A <- merge(stolen_base_by_player_1A, running_appearances_first_base_1A, by = "runner")

stolen_base_by_player_1A <- stolen_base_by_player_1A %>%
  mutate(stolen_base_percentage = stolen_bases / running_appearances,
         caught_stolen_base_percentage = caught_stolen_bases / running_appearances)

stolen_base_selected_1A <- subset(stolen_base_by_player_1A, select = c("runner", "stolen_bases", "caught_stolen_bases", "running_appearances", "stolen_base_percentage", "caught_stolen_base_percentage"))

stats_per_player_1A <- merge(stats_per_player_1A, stolen_base_selected_1A, by.x = "player", by.y = "runner")

write.csv(stats_per_player_1A, "stats_per_player_1A.csv", row.names = F)




#####

second_base_no_third_base_1A <- all_game_info %>%
  filter(!is.na(second_baserunner), is.na(third_baserunner)) %>%
  filter(batting_team == "Home1A")

second_base_no_third_base_id_1A <- second_base_no_third_base_1A$game_str_play_id

stolen_base_data_1A_2_to_3 <- all_game_info_cleaned %>%
  filter(batting_team=="Home1A")%>%
  filter(inning>0)%>%
  arrange(at_bat_game_str, play_per_game) %>%  # Ensure data is sorted
  group_by(at_bat_game_str) %>%  # Group by game and at bat
  mutate(starting_second_baserunner = first(second_baserunner),
         ending_third_baserunner = last(third_baserunner),
         ending_second_baserunner = last(second_baserunner),
         steal = ifelse(starting_second_baserunner == ending_third_baserunner, 1, 0),
         caught_stealing = ifelse(is.na(ending_second_baserunner) & is.na(ending_third_baserunner) & !is.na(starting_second_baserunner), 1, 0)
  ) %>%
  ungroup()

sum(stolen_base_data_1A_2_to_3$steal, na.rm=T)
sum(stolen_base_data_1A_2_to_3$caught_stealing)
###This is not the amount of stolen bases and caught stolen bases, 
#but the amount of rows of a certain at bat in which a stolen base or caught stolen base has occurred.
#These numbers are only for me to see if it seems about right, and it does

stolen_base_data_grouped_1A_2_to_3<- stolen_base_data_1A_2_to_3 %>%
  group_by(at_bat_game_str) %>%
  summarise(stolen_base = max(steal),
            stolen_base_caught = max(caught_stealing),
            runner = first(starting_second_baserunner))

stolen_base_by_player_1A_2_to_3 <- stolen_base_data_grouped_1A_2_to_3 %>%
  group_by(runner) %>%
  summarise(stolen_bases_2_to_3 = sum(stolen_base, na.rm = T),
            caught_stolen_bases_2_to_3 = sum(stolen_base_caught, na.rm = T))

running_appearances_second_base_1A <- all_game_info %>%
  filter(batting_team=="Home1A")%>%
  filter(!is.na(second_baserunner)) %>%
  group_by(at_bat_game_str) %>%
  summarise(runner = first(second_baserunner)) %>%
  group_by(runner) %>%
  summarise(running_appearances_2_to_3 = n())

stolen_base_by_player_1A_2_to_3 <- merge(stolen_base_by_player_1A_2_to_3, running_appearances_second_base_1A, by = "runner")

stolen_base_by_player_1A_2_to_3 <- stolen_base_by_player_1A_2_to_3 %>%
  mutate(stolen_base_percentage_2_to_3 = stolen_bases_2_to_3 / running_appearances_2_to_3,
         caught_stolen_base_percentage_2_to_3 = caught_stolen_bases_2_to_3 / running_appearances_2_to_3)

stolen_base_selected_1A_2_to_3 <- subset(stolen_base_by_player_1A_2_to_3, select = c("runner", "stolen_bases_2_to_3", "caught_stolen_bases_2_to_3", "running_appearances_2_to_3", "stolen_base_percentage_2_to_3", "caught_stolen_base_percentage_2_to_3"))

stats_per_player_1A <- merge(stats_per_player_1A, stolen_base_selected_1A_2_to_3, by.x = "player", by.y = "runner")

write.csv(stats_per_player_1A, "stats_per_player_1A.csv", row.names = F)



#############


first_base_no_second_base_2A <- all_game_info %>%
  filter(!is.na(first_baserunner), is.na(second_baserunner)) %>%
  filter(batting_team == "Home2A")

first_base_no_second_base_id_2A <- first_base_no_second_base_2A$game_str_play_id

stolen_base_data_2A <- all_game_info_cleaned %>%
  filter(batting_team=="Home2A")%>%
  filter(inning>0)%>%
  arrange(at_bat_game_str, play_per_game) %>%  # Ensure data is sorted
  group_by(at_bat_game_str) %>%  # Group by game and at bat
  mutate(starting_first_baserunner = first(first_baserunner),
         ending_second_baserunner = last(second_baserunner),
         ending_first_baserunner = last(first_baserunner),
         steal = ifelse(starting_first_baserunner == ending_second_baserunner, 1, 0),
         caught_stealing = ifelse(is.na(ending_first_baserunner) & is.na(ending_second_baserunner) & !is.na(starting_first_baserunner), 1, 0)
  ) %>%
  ungroup()

sum(stolen_base_data_2A$steal, na.rm=T)
sum(stolen_base_data_2A$caught_stealing)
###This is not the amount of stolen bases and caught stolen bases, 
#but the amount of rows of a certain at bat in which a stolen base or caught stolen base has occurred.
#These numbers are only for me to see if it seems about right, and it does

stolen_base_data_grouped_2A<- stolen_base_data_2A %>%
  group_by(at_bat_game_str) %>%
  summarise(stolen_base = max(steal),
            stolen_base_caught = max(caught_stealing),
            runner = first(starting_first_baserunner))

stolen_base_by_player_2A <- stolen_base_data_grouped_2A %>%
  group_by(runner) %>%
  summarise(stolen_bases = sum(stolen_base, na.rm = T),
            caught_stolen_bases = sum(stolen_base_caught, na.rm = T))

running_appearances_first_base_2A <- all_game_info %>%
  filter(batting_team=="Home2A")%>%
  filter(!is.na(first_baserunner)) %>%
  group_by(at_bat_game_str) %>%
  summarise(runner = first(first_baserunner)) %>%
  group_by(runner) %>%
  summarise(running_appearances = n())

stolen_base_by_player_2A <- merge(stolen_base_by_player_2A, running_appearances_first_base_2A, by = "runner")

stolen_base_by_player_2A <- stolen_base_by_player_2A %>%
  mutate(stolen_base_percentage = stolen_bases / running_appearances,
         caught_stolen_base_percentage = caught_stolen_bases / running_appearances)

stolen_base_selected_2A <- subset(stolen_base_by_player_2A, select = c("runner", "stolen_bases", "caught_stolen_bases", "running_appearances", "stolen_base_percentage", "caught_stolen_base_percentage"))

stats_per_player_2A <- merge(stats_per_player_2A, stolen_base_selected_2A, by.x = "player", by.y = "runner")

write.csv(stats_per_player_2A, "stats_per_player_2A.csv", row.names = F)




#####

second_base_no_third_base_2A <- all_game_info %>%
  filter(!is.na(second_baserunner), is.na(third_baserunner)) %>%
  filter(batting_team == "Home2A")

second_base_no_third_base_id_2A <- second_base_no_third_base_2A$game_str_play_id

stolen_base_data_2A_2_to_3 <- all_game_info_cleaned %>%
  filter(batting_team=="Home2A")%>%
  filter(inning>0)%>%
  arrange(at_bat_game_str, play_per_game) %>%  # Ensure data is sorted
  group_by(at_bat_game_str) %>%  # Group by game and at bat
  mutate(starting_second_baserunner = first(second_baserunner),
         ending_third_baserunner = last(third_baserunner),
         ending_second_baserunner = last(second_baserunner),
         steal = ifelse(starting_second_baserunner == ending_third_baserunner, 1, 0),
         caught_stealing = ifelse(is.na(ending_second_baserunner) & is.na(ending_third_baserunner) & !is.na(starting_second_baserunner), 1, 0)
  ) %>%
  ungroup()

sum(stolen_base_data_2A_2_to_3$steal, na.rm=T)
sum(stolen_base_data_2A_2_to_3$caught_stealing)
###This is not the amount of stolen bases and caught stolen bases, 
#but the amount of rows of a certain at bat in which a stolen base or caught stolen base has occurred.
#These numbers are only for me to see if it seems about right, and it does

stolen_base_data_grouped_2A_2_to_3<- stolen_base_data_2A_2_to_3 %>%
  group_by(at_bat_game_str) %>%
  summarise(stolen_base = max(steal),
            stolen_base_caught = max(caught_stealing),
            runner = first(starting_second_baserunner))

stolen_base_by_player_2A_2_to_3 <- stolen_base_data_grouped_2A_2_to_3 %>%
  group_by(runner) %>%
  summarise(stolen_bases_2_to_3 = sum(stolen_base, na.rm = T),
            caught_stolen_bases_2_to_3 = sum(stolen_base_caught, na.rm = T))

running_appearances_second_base_2A <- all_game_info %>%
  filter(batting_team=="Home2A")%>%
  filter(!is.na(second_baserunner)) %>%
  group_by(at_bat_game_str) %>%
  summarise(runner = first(second_baserunner)) %>%
  group_by(runner) %>%
  summarise(running_appearances_2_to_3 = n())

stolen_base_by_player_2A_2_to_3 <- merge(stolen_base_by_player_2A_2_to_3, running_appearances_second_base_2A, by = "runner")

stolen_base_by_player_2A_2_to_3 <- stolen_base_by_player_2A_2_to_3 %>%
  mutate(stolen_base_percentage_2_to_3 = stolen_bases_2_to_3 / running_appearances_2_to_3,
         caught_stolen_base_percentage_2_to_3 = caught_stolen_bases_2_to_3 / running_appearances_2_to_3)

stolen_base_selected_2A_2_to_3 <- subset(stolen_base_by_player_2A_2_to_3, select = c("runner", "stolen_bases_2_to_3", "caught_stolen_bases_2_to_3", "running_appearances_2_to_3", "stolen_base_percentage_2_to_3", "caught_stolen_base_percentage_2_to_3"))

stats_per_player_2A <- merge(stats_per_player_2A, stolen_base_selected_2A_2_to_3, by.x = "player", by.y = "runner")

write.csv(stats_per_player_2A, "stats_per_player_2A.csv", row.names = F)



###########

#############


first_base_no_second_base_3A <- all_game_info %>%
  filter(!is.na(first_baserunner), is.na(second_baserunner)) %>%
  filter(batting_team == "Home3A")

first_base_no_second_base_id_3A <- first_base_no_second_base_3A$game_str_play_id

stolen_base_data_3A <- all_game_info_cleaned %>%
  filter(batting_team=="Home3A")%>%
  filter(inning>0)%>%
  arrange(at_bat_game_str, play_per_game) %>%  # Ensure data is sorted
  group_by(at_bat_game_str) %>%  # Group by game and at bat
  mutate(starting_first_baserunner = first(first_baserunner),
         ending_second_baserunner = last(second_baserunner),
         ending_first_baserunner = last(first_baserunner),
         steal = ifelse(starting_first_baserunner == ending_second_baserunner, 1, 0),
         caught_stealing = ifelse(is.na(ending_first_baserunner) & is.na(ending_second_baserunner) & !is.na(starting_first_baserunner), 1, 0)
  ) %>%
  ungroup()

sum(stolen_base_data_3A$steal, na.rm=T)
sum(stolen_base_data_3A$caught_stealing)
###This is not the amount of stolen bases and caught stolen bases, 
#but the amount of rows of a certain at bat in which a stolen base or caught stolen base has occurred.
#These numbers are only for me to see if it seems about right, and it does

stolen_base_data_grouped_3A<- stolen_base_data_3A %>%
  group_by(at_bat_game_str) %>%
  summarise(stolen_base = max(steal),
            stolen_base_caught = max(caught_stealing),
            runner = first(starting_first_baserunner))

stolen_base_by_player_3A <- stolen_base_data_grouped_3A %>%
  group_by(runner) %>%
  summarise(stolen_bases = sum(stolen_base, na.rm = T),
            caught_stolen_bases = sum(stolen_base_caught, na.rm = T))

running_appearances_first_base_3A <- all_game_info %>%
  filter(batting_team=="Home3A")%>%
  filter(!is.na(first_baserunner)) %>%
  group_by(at_bat_game_str) %>%
  summarise(runner = first(first_baserunner)) %>%
  group_by(runner) %>%
  summarise(running_appearances = n())

stolen_base_by_player_3A <- merge(stolen_base_by_player_3A, running_appearances_first_base_3A, by = "runner")

stolen_base_by_player_3A <- stolen_base_by_player_3A %>%
  mutate(stolen_base_percentage = stolen_bases / running_appearances,
         caught_stolen_base_percentage = caught_stolen_bases / running_appearances)

stolen_base_selected_3A <- subset(stolen_base_by_player_3A, select = c("runner", "stolen_bases", "caught_stolen_bases", "running_appearances", "stolen_base_percentage", "caught_stolen_base_percentage"))

stats_per_player_3A <- merge(stats_per_player_3A, stolen_base_selected_3A, by.x = "player", by.y = "runner")

write.csv(stats_per_player_3A, "stats_per_player_3A.csv", row.names = F)




#####

second_base_no_third_base_3A <- all_game_info %>%
  filter(!is.na(second_baserunner), is.na(third_baserunner)) %>%
  filter(batting_team == "Home3A")

second_base_no_third_base_id_3A <- second_base_no_third_base_3A$game_str_play_id

stolen_base_data_3A_2_to_3 <- all_game_info_cleaned %>%
  filter(batting_team=="Home3A")%>%
  filter(inning>0)%>%
  arrange(at_bat_game_str, play_per_game) %>%  # Ensure data is sorted
  group_by(at_bat_game_str) %>%  # Group by game and at bat
  mutate(starting_second_baserunner = first(second_baserunner),
         ending_third_baserunner = last(third_baserunner),
         ending_second_baserunner = last(second_baserunner),
         steal = ifelse(starting_second_baserunner == ending_third_baserunner, 1, 0),
         caught_stealing = ifelse(is.na(ending_second_baserunner) & is.na(ending_third_baserunner) & !is.na(starting_second_baserunner), 1, 0)
  ) %>%
  ungroup()

sum(stolen_base_data_3A_2_to_3$steal, na.rm=T)
sum(stolen_base_data_3A_2_to_3$caught_stealing)
###This is not the amount of stolen bases and caught stolen bases, 
#but the amount of rows of a certain at bat in which a stolen base or caught stolen base has occurred.
#These numbers are only for me to see if it seems about right, and it does

stolen_base_data_grouped_3A_2_to_3<- stolen_base_data_3A_2_to_3 %>%
  group_by(at_bat_game_str) %>%
  summarise(stolen_base = max(steal),
            stolen_base_caught = max(caught_stealing),
            runner = first(starting_second_baserunner))

stolen_base_by_player_3A_2_to_3 <- stolen_base_data_grouped_3A_2_to_3 %>%
  group_by(runner) %>%
  summarise(stolen_bases_2_to_3 = sum(stolen_base, na.rm = T),
            caught_stolen_bases_2_to_3 = sum(stolen_base_caught, na.rm = T))

running_appearances_second_base_3A <- all_game_info %>%
  filter(batting_team=="Home3A")%>%
  filter(!is.na(second_baserunner)) %>%
  group_by(at_bat_game_str) %>%
  summarise(runner = first(second_baserunner)) %>%
  group_by(runner) %>%
  summarise(running_appearances_2_to_3 = n())

stolen_base_by_player_3A_2_to_3 <- merge(stolen_base_by_player_3A_2_to_3, running_appearances_second_base_3A, by = "runner")

stolen_base_by_player_3A_2_to_3 <- stolen_base_by_player_3A_2_to_3 %>%
  mutate(stolen_base_percentage_2_to_3 = stolen_bases_2_to_3 / running_appearances_2_to_3,
         caught_stolen_base_percentage_2_to_3 = caught_stolen_bases_2_to_3 / running_appearances_2_to_3)

stolen_base_selected_3A_2_to_3 <- subset(stolen_base_by_player_3A_2_to_3, select = c("runner", "stolen_bases_2_to_3", "caught_stolen_bases_2_to_3", "running_appearances_2_to_3", "stolen_base_percentage_2_to_3", "caught_stolen_base_percentage_2_to_3"))

stats_per_player_3A <- merge(stats_per_player_3A, stolen_base_selected_3A_2_to_3, by.x = "player", by.y = "runner")

write.csv(stats_per_player_3A, "stats_per_player_3A.csv", row.names = F)



###########

#############


first_base_no_second_base_4A <- all_game_info %>%
  filter(!is.na(first_baserunner), is.na(second_baserunner)) %>%
  filter(batting_team == "Home4A")

first_base_no_second_base_id_4A <- first_base_no_second_base_4A$game_str_play_id

stolen_base_data_4A <- all_game_info_cleaned %>%
  filter(batting_team=="Home4A")%>%
  filter(inning>0)%>%
  arrange(at_bat_game_str, play_per_game) %>%  # Ensure data is sorted
  group_by(at_bat_game_str) %>%  # Group by game and at bat
  mutate(starting_first_baserunner = first(first_baserunner),
         ending_second_baserunner = last(second_baserunner),
         ending_first_baserunner = last(first_baserunner),
         steal = ifelse(starting_first_baserunner == ending_second_baserunner, 1, 0),
         caught_stealing = ifelse(is.na(ending_first_baserunner) & is.na(ending_second_baserunner) & !is.na(starting_first_baserunner), 1, 0)
  ) %>%
  ungroup()

sum(stolen_base_data_4A$steal, na.rm=T)
sum(stolen_base_data_4A$caught_stealing)
###This is not the amount of stolen bases and caught stolen bases, 
#but the amount of rows of a certain at bat in which a stolen base or caught stolen base has occurred.
#These numbers are only for me to see if it seems about right, and it does

stolen_base_data_grouped_4A<- stolen_base_data_4A %>%
  group_by(at_bat_game_str) %>%
  summarise(stolen_base = max(steal),
            stolen_base_caught = max(caught_stealing),
            runner = first(starting_first_baserunner))

stolen_base_by_player_4A <- stolen_base_data_grouped_4A %>%
  group_by(runner) %>%
  summarise(stolen_bases = sum(stolen_base, na.rm = T),
            caught_stolen_bases = sum(stolen_base_caught, na.rm = T))

running_appearances_first_base_4A <- all_game_info %>%
  filter(batting_team=="Home4A")%>%
  filter(!is.na(first_baserunner)) %>%
  group_by(at_bat_game_str) %>%
  summarise(runner = first(first_baserunner)) %>%
  group_by(runner) %>%
  summarise(running_appearances = n())

stolen_base_by_player_4A <- merge(stolen_base_by_player_4A, running_appearances_first_base_4A, by = "runner")

stolen_base_by_player_4A <- stolen_base_by_player_4A %>%
  mutate(stolen_base_percentage = stolen_bases / running_appearances,
         caught_stolen_base_percentage = caught_stolen_bases / running_appearances)

stolen_base_selected_4A <- subset(stolen_base_by_player_4A, select = c("runner", "stolen_bases", "caught_stolen_bases", "running_appearances", "stolen_base_percentage", "caught_stolen_base_percentage"))

stats_per_player_4A <- merge(stats_per_player_4A, stolen_base_selected_4A, by.x = "player", by.y = "runner")

write.csv(stats_per_player_4A, "stats_per_player_4A.csv", row.names = F)




#####

second_base_no_third_base_4A <- all_game_info %>%
  filter(!is.na(second_baserunner), is.na(third_baserunner)) %>%
  filter(batting_team == "Home4A")

second_base_no_third_base_id_4A <- second_base_no_third_base_4A$game_str_play_id

stolen_base_data_4A_2_to_3 <- all_game_info_cleaned %>%
  filter(batting_team=="Home4A")%>%
  filter(inning>0)%>%
  arrange(at_bat_game_str, play_per_game) %>%  # Ensure data is sorted
  group_by(at_bat_game_str) %>%  # Group by game and at bat
  mutate(starting_second_baserunner = first(second_baserunner),
         ending_third_baserunner = last(third_baserunner),
         ending_second_baserunner = last(second_baserunner),
         steal = ifelse(starting_second_baserunner == ending_third_baserunner, 1, 0),
         caught_stealing = ifelse(is.na(ending_second_baserunner) & is.na(ending_third_baserunner) & !is.na(starting_second_baserunner), 1, 0)
  ) %>%
  ungroup()

sum(stolen_base_data_4A_2_to_3$steal, na.rm=T)
sum(stolen_base_data_4A_2_to_3$caught_stealing)
###This is not the amount of stolen bases and caught stolen bases, 
#but the amount of rows of a certain at bat in which a stolen base or caught stolen base has occurred.
#These numbers are only for me to see if it seems about right, and it does

stolen_base_data_grouped_4A_2_to_3<- stolen_base_data_4A_2_to_3 %>%
  group_by(at_bat_game_str) %>%
  summarise(stolen_base = max(steal),
            stolen_base_caught = max(caught_stealing),
            runner = first(starting_second_baserunner))

stolen_base_by_player_4A_2_to_3 <- stolen_base_data_grouped_4A_2_to_3 %>%
  group_by(runner) %>%
  summarise(stolen_bases_2_to_3 = sum(stolen_base, na.rm = T),
            caught_stolen_bases_2_to_3 = sum(stolen_base_caught, na.rm = T))

running_appearances_second_base_4A <- all_game_info %>%
  filter(batting_team=="Home4A")%>%
  filter(!is.na(second_baserunner)) %>%
  group_by(at_bat_game_str) %>%
  summarise(runner = first(second_baserunner)) %>%
  group_by(runner) %>%
  summarise(running_appearances_2_to_3 = n())

stolen_base_by_player_4A_2_to_3 <- merge(stolen_base_by_player_4A_2_to_3, running_appearances_second_base_4A, by = "runner")

stolen_base_by_player_4A_2_to_3 <- stolen_base_by_player_4A_2_to_3 %>%
  mutate(stolen_base_percentage_2_to_3 = stolen_bases_2_to_3 / running_appearances_2_to_3,
         caught_stolen_base_percentage_2_to_3 = caught_stolen_bases_2_to_3 / running_appearances_2_to_3)

stolen_base_selected_4A_2_to_3 <- subset(stolen_base_by_player_4A_2_to_3, select = c("runner", "stolen_bases_2_to_3", "caught_stolen_bases_2_to_3", "running_appearances_2_to_3", "stolen_base_percentage_2_to_3", "caught_stolen_base_percentage_2_to_3"))

stats_per_player_4A <- merge(stats_per_player_4A, stolen_base_selected_4A_2_to_3, by.x = "player", by.y = "runner")

write.csv(stats_per_player_4A, "stats_per_player_4A.csv", row.names = F)

#############################################

batter_by_team <- all_game_info %>%
  filter(grepl("Home", batting_team)) %>%
  filter(!is.na(batter), !is.na(batting_team)) %>%
  group_by(batting_team, batter) %>%
  summarise(plays = n())

number_of_teams_per_batter <- batter_by_team %>%
  group_by(batter) %>%
  summarise(number_of_teams = n(),
            number_of_total_plays = sum(plays))

library(tidyr)
batter_by_team_2 <- batter_by_team %>%
  pivot_wider(
    names_from = batting_team,
    values_from = plays,
    values_fill = list(plays = 0)
  )

write.csv(batter_by_team_2, "batting_team_data.csv")

players_by_level <- all_team_info %>% 
  mutate(level = ifelse(grepl("4", home_team), 4, 
                        ifelse(grepl("3", home_team), 3, 
                               ifelse(grepl("2", home_team), 2, 
                                      ifelse(grepl("1", home_team), 1, NA)))))

batter_by_team_3 <- all_game_info %>%
  filter(!is.na(batter), !is.na(batting_team)) %>%
  group_by(batting_team, batter) %>%
  summarise(plays = n())


#############################################################

library(tidyr)
library(dplyr)

player_positions_modified_long <- all_game_info %>%
  pivot_longer(cols = c(batter, first_base, second_base, third_base, shortstop, left_field, center_field, right_field, catcher, pitcher), names_to = "position", values_to = "player")

count_data <- player_positions_modified_long %>%
  distinct(game_str, player, position) %>%
  group_by(player, position) %>%
  summarize(games_played = n(), .groups = 'drop')

player_positions <- count_data %>%
  pivot_wider(names_from = position, values_from = games_played, values_fill = list(games_played = 0))

player_positions_modified <- player_positions %>%
  rowwise() %>%
  mutate(across(-c(player, batter), ~ ifelse(. == max(c_across(-c(player, batter))), ., 0))) %>%
  ungroup()


colnames(player_positions_modified)[3] <- "designated_hitter"

write.csv(player_positions_modified, "player_positions_modified.csv", row.names = F)

batter_handedness <- read.csv("batter_handedness.csv")



#####
#1A
stats_per_player_1A <- read.csv("stats_per_player_1A.csv")

stats_per_player_sub_1A <- subset(stats_per_player_1A, select = c("player", "on_base_percentage", "homerun_percentage", "speed_percentile"))

player_positions_modified_plus_1A <- merge(player_positions_modified, stats_per_player_sub_1A, by = "player")

player_positions_modified_all_1A <- merge(player_positions_modified_plus_1A, batter_handedness, by.x = "player", by.y = "batter") %>%
  filter(designated_hitter>5)

write.csv(player_positions_modified_all_1A, "player_positions_modified_all_1A.csv", row.names = F)

##
stats_per_player_sub_righty_1A <- subset(stats_per_player_handedness_against_righty_1A, select = c("player", "on_base_percentage", "homerun_percentage", "speed_percentile"))

player_positions_modified_plus_righty_1A <- merge(player_positions_modified, stats_per_player_sub_righty_1A, by = "player")

player_positions_modified_righty_1A <- merge(player_positions_modified_plus_righty_1A, batter_handedness, by.x = "player", by.y = "batter")

player_positions_modified_righty_1A <- player_positions_modified_righty_1A %>%
  filter(designated_hitter>5)

write.csv(player_positions_modified_righty_1A, "player_positions_modified_righty_1A.csv", row.names = F)

##
stats_per_player_sub_lefty_1A <- subset(stats_per_player_handedness_against_lefty_1A, select = c("player", "on_base_percentage", "homerun_percentage", "speed_percentile"))

player_positions_modified_plus_lefty_1A <- merge(player_positions_modified, stats_per_player_sub_lefty_1A, by = "player")

player_positions_modified_lefty_1A <- merge(player_positions_modified_plus_lefty_1A, batter_handedness, by.x = "player", by.y = "batter")

player_positions_modified_lefty_1A <- player_positions_modified_lefty_1A%>%
  filter(designated_hitter>5)

write.csv(player_positions_modified_lefty_1A, "player_positions_modified_lefty_1A.csv", row.names = F)

#####
#2A
stats_per_player_2A <- read.csv("stats_per_player_2A.csv")

stats_per_player_sub_2A <- subset(stats_per_player_2A, select = c("player", "on_base_percentage", "homerun_percentage", "speed_percentile"))

player_positions_modified_plus_2A <- merge(player_positions_modified, stats_per_player_sub_2A, by = "player")

player_positions_modified_all_2A <- merge(player_positions_modified_plus_2A, batter_handedness, by.x = "player", by.y = "batter") %>%
  filter(designated_hitter>5)

write.csv(player_positions_modified_all_2A, "player_positions_modified_all_2A.csv", row.names = F)

##
stats_per_player_sub_righty_2A <- subset(stats_per_player_handedness_against_righty_2A, select = c("player", "on_base_percentage", "homerun_percentage", "speed_percentile"))

player_positions_modified_plus_righty_2A <- merge(player_positions_modified, stats_per_player_sub_righty_2A, by = "player")

player_positions_modified_righty_2A <- merge(player_positions_modified_plus_righty_2A, batter_handedness, by.x = "player", by.y = "batter")

player_positions_modified_righty_2A <- player_positions_modified_righty_2A %>%
  filter(designated_hitter>5)

write.csv(player_positions_modified_righty_2A, "player_positions_modified_righty_2A.csv", row.names = F)

##
stats_per_player_sub_lefty_2A <- subset(stats_per_player_handedness_against_lefty_2A, select = c("player", "on_base_percentage", "homerun_percentage", "speed_percentile"))

player_positions_modified_plus_lefty_2A <- merge(player_positions_modified, stats_per_player_sub_lefty_2A, by = "player")

player_positions_modified_lefty_2A <- merge(player_positions_modified_plus_lefty_2A, batter_handedness, by.x = "player", by.y = "batter")

player_positions_modified_lefty_2A <- player_positions_modified_lefty_2A%>%
  filter(designated_hitter>5)

write.csv(player_positions_modified_lefty_2A, "player_positions_modified_lefty_2A.csv", row.names = F)

#####
#3A
stats_per_player_3A <- read.csv("stats_per_player_3A.csv")

stats_per_player_sub_3A <- subset(stats_per_player_3A, select = c("player", "on_base_percentage", "homerun_percentage", "speed_percentile"))

player_positions_modified_plus_3A <- merge(player_positions_modified, stats_per_player_sub_3A, by = "player")

player_positions_modified_all_3A <- merge(player_positions_modified_plus_3A, batter_handedness, by.x = "player", by.y = "batter") %>%
  filter(designated_hitter>5)

write.csv(player_positions_modified_all_3A, "player_positions_modified_all_3A.csv", row.names = F)

##
stats_per_player_sub_righty_3A <- subset(stats_per_player_handedness_against_righty_3A, select = c("player", "on_base_percentage", "homerun_percentage", "speed_percentile"))

player_positions_modified_plus_righty_3A <- merge(player_positions_modified, stats_per_player_sub_righty_3A, by = "player")

player_positions_modified_righty_3A <- merge(player_positions_modified_plus_righty_3A, batter_handedness, by.x = "player", by.y = "batter")

player_positions_modified_righty_3A <- player_positions_modified_righty_3A %>%
  filter(designated_hitter>5)

write.csv(player_positions_modified_righty_3A, "player_positions_modified_righty_3A.csv", row.names = F)

##
stats_per_player_sub_lefty_3A <- subset(stats_per_player_handedness_against_lefty_3A, select = c("player", "on_base_percentage", "homerun_percentage", "speed_percentile"))

player_positions_modified_plus_lefty_3A <- merge(player_positions_modified, stats_per_player_sub_lefty_3A, by = "player")

player_positions_modified_lefty_3A <- merge(player_positions_modified_plus_lefty_3A, batter_handedness, by.x = "player", by.y = "batter")

player_positions_modified_lefty_3A <- player_positions_modified_lefty_3A%>%
  filter(designated_hitter>5)

write.csv(player_positions_modified_lefty_3A, "player_positions_modified_lefty_3A.csv", row.names = F)

#####
#4A
stats_per_player_4A <- read.csv("stats_per_player_4A.csv")

stats_per_player_sub_4A <- subset(stats_per_player_4A, select = c("player", "on_base_percentage", "homerun_percentage", "speed_percentile"))

player_positions_modified_plus_4A <- merge(player_positions_modified, stats_per_player_sub_4A, by = "player")

player_positions_modified_all_4A <- merge(player_positions_modified_plus_4A, batter_handedness, by.x = "player", by.y = "batter") %>%
  filter(designated_hitter>5)

write.csv(player_positions_modified_all_4A, "player_positions_modified_all_4A.csv", row.names = F)

##
stats_per_player_sub_righty_4A <- subset(stats_per_player_handedness_against_righty_4A, select = c("player", "on_base_percentage", "homerun_percentage", "speed_percentile"))

player_positions_modified_plus_righty_4A <- merge(player_positions_modified, stats_per_player_sub_righty_4A, by = "player")

player_positions_modified_righty_4A <- merge(player_positions_modified_plus_righty_4A, batter_handedness, by.x = "player", by.y = "batter")

player_positions_modified_righty_4A <- player_positions_modified_righty_4A %>%
  filter(designated_hitter>5)

write.csv(player_positions_modified_righty_4A, "player_positions_modified_righty_4A.csv", row.names = F)

##
stats_per_player_sub_lefty_4A <- subset(stats_per_player_handedness_against_lefty_4A, select = c("player", "on_base_percentage", "homerun_percentage", "speed_percentile"))

player_positions_modified_plus_lefty_4A <- merge(player_positions_modified, stats_per_player_sub_lefty_4A, by = "player")

player_positions_modified_lefty_4A <- merge(player_positions_modified_plus_lefty_4A, batter_handedness, by.x = "player", by.y = "batter")

player_positions_modified_lefty_4A <- player_positions_modified_lefty_4A%>%
  filter(designated_hitter>5)

write.csv(player_positions_modified_lefty_4A, "player_positions_modified_lefty_4A.csv", row.names = F)



#######################################################


#1A
at_bat_by_player_1A <- function(player){
  player_info <- all_game_info %>%
    filter(grepl("1", home_team))%>%
    filter(!is.na(batter)) %>%
    group_by(at_bat_game_str) %>%
    summarise(batter = first(batter)) %>%
    ungroup() %>%
    group_by(batter) %>%
    summarise(at_bats = n()) %>%
    filter(batter == player)
  if(nrow(player_info) == 0) return(0)
  return(player_info$at_bats)
}

walk_by_player_1A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("1", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  first_bases_gotten <- nrow(player_info |> 
                               filter(batter == player & lead(first_baserunner)==player))
  first_based <- (player_info |> 
                    filter(batter == player & lead(first_baserunner)==player))
  first_based_game_play_id <- first_based$game_str_play_id
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% first_based_game_play_id) %>%
    filter(event_code==4)
  singles <- nrow(game_events_wanted)
  walks <- first_bases_gotten - singles
  walks
}

single_by_player_1A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("1", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  first_based <- (player_info |> 
                    filter(batter == player & lead(first_baserunner)==player))
  first_based_game_play_id <- first_based$game_str_play_id
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% first_based_game_play_id) %>%
    filter(event_code==4)
  nrow(game_events_wanted)
}

double_by_player_1A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("1", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  nrow(player_info |> 
         filter(batter == player & lead(second_baserunner)==player))
}

triple_by_player_1A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("1", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  nrow(player_info |> 
         filter(batter == player & lead(third_baserunner)==player))
}

homeruns <- all_game_events %>%
  filter(event_code==11)

homeruns <- merge(homeruns, all_game_info_cleaned, by = "game_str_play_id")

homerun_batters_1A <- homeruns %>%
  filter(grepl("1", home_team))%>%
  select(batter) %>%
  group_by(batter) %>%
  summarise(homeruns = n()) %>%
  arrange(desc(homeruns))

homerun_by_player_1A <- function(player){
  homerun <- homerun_batters_1A %>%
    filter(batter == player)
  if(nrow(homerun) == 0) return(0)
  return(homerun$homeruns)
}

game_info_1A <- all_game_info_cleaned %>%
  filter(grepl("1", home_team))
all_batters_1A <- unique(na.omit(game_info_1A$batter)) 

stats_per_player_1A <- tibble(player = all_batters_1A)

library(purrr)
stats_per_player_1A <- stats_per_player_1A %>%
  mutate(
    at_bats = map_dbl(player, ~at_bat_by_player_1A(.x)),
    walks = map_dbl(player, ~walk_by_player_1A(.x)),
    singles = map_dbl(player, ~single_by_player_1A(.x)),
    doubles = map_dbl(player, ~double_by_player_1A(.x)),
    triples = map_dbl(player, ~triple_by_player_1A(.x)),
    home_runs = map_dbl(player, ~homerun_by_player_1A(.x)),
    on_base_percentage = (walks + singles + doubles + triples + home_runs) / at_bats,
    wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples) + (2.101 * home_runs)) / at_bats
  )


stats_per_player_1A <- stats_per_player_1A %>%
  mutate(at_bats = ifelse(at_bats<(walks+singles+doubles+triples+home_runs),walks+singles+doubles+triples+home_runs, at_bats),
         on_base_percentage = (walks + singles + doubles + triples + home_runs) / at_bats,
         wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples) + (2.101 * home_runs)) / at_bats)

stats_per_player_1A$homerun_percentage <- stats_per_player_1A$home_runs / stats_per_player_1A$at_bats
stats_per_player_1A$walk_percentage <- stats_per_player_1A$walks / stats_per_player_1A$at_bats
stats_per_player_1A$single_percentage <- stats_per_player_1A$singles / stats_per_player_1A$at_bats
stats_per_player_1A$double_percentage <- stats_per_player_1A$doubles / stats_per_player_1A$at_bats
stats_per_player_1A$triple_percentage <- stats_per_player_1A$triples / stats_per_player_1A$at_bats

stats_per_player_1A <- stats_per_player_1A %>%
  filter(at_bats>=10)

write.csv(stats_per_player_1A, "stats_per_player_1A.csv", row.names = F)

stats_per_player_1A <- read.csv("stats_per_player_1A.csv")

stats_per_player <- read.csv("stats_per_player.csv")

extra_data <- subset(stats_per_player, select = c("player", "speed_percentile", "left_field", "right_field", "center_field",
                                                  "NA_field", "right", "strikeouts", "fly_out_pct",
                                                  "stolen_base_percentage", "caught_stolen_base_percentage", 
                                                  "stolen_base_percentage_2_to_3", "caught_stolen_base_percentage_2_to_3"
))

stats_per_player_1A <- merge(stats_per_player_1A, extra_data, by = "player")

write.csv(stats_per_player_1A, "stats_per_player_1A.csv", row.names = F)

handedness_stats <- read.csv("handedness_stats.csv")

stats_per_player_1A <- read.csv("stats_per_player_1A.csv")

stats_per_player_handedness_1A <- merge(stats_per_player_1A, batter_handedness, by.x = "player", by.y = "batter")

stats_per_player_handedness_against_righty_1A <- stats_per_player_handedness_1A %>%
  mutate(
    single_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "left_right"],
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    double_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "left_right"],
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    triple_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "left_right"],
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    homerun_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "left_right"],
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    on_base_percentage = (walk_percentage + single_percentage + double_percentage + triple_percentage + homerun_percentage)
  )



stats_per_player_handedness_against_lefty_1A <- stats_per_player_handedness_1A %>%
  mutate(
    single_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "right_left"],
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    double_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "right_left"],
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    triple_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "right_left"],
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    homerun_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "right_left"],
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    on_base_percentage = (walk_percentage + single_percentage + double_percentage + triple_percentage + homerun_percentage))

write.csv(stats_per_player_handedness_against_righty_1A, "stats_per_player_handedness_against_righty_1A.csv", row.names = F)
write.csv(stats_per_player_handedness_against_lefty_1A, "stats_per_player_handedness_against_lefty_1A.csv", row.names = F)



#2A
at_bat_by_player_2A <- function(player){
  player_info <- all_game_info %>%
    filter(grepl("2", home_team))%>%
    filter(!is.na(batter)) %>%
    group_by(at_bat_game_str) %>%
    summarise(batter = first(batter)) %>%
    ungroup() %>%
    group_by(batter) %>%
    summarise(at_bats = n()) %>%
    filter(batter == player)
  if(nrow(player_info) == 0) return(0)
  return(player_info$at_bats)
}

walk_by_player_2A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("2", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  first_bases_gotten <- nrow(player_info |> 
                               filter(batter == player & lead(first_baserunner)==player))
  first_based <- (player_info |> 
                    filter(batter == player & lead(first_baserunner)==player))
  first_based_game_play_id <- first_based$game_str_play_id
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% first_based_game_play_id) %>%
    filter(event_code==4)
  singles <- nrow(game_events_wanted)
  walks <- first_bases_gotten - singles
  walks
}

single_by_player_2A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("2", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  first_based <- (player_info |> 
                    filter(batter == player & lead(first_baserunner)==player))
  first_based_game_play_id <- first_based$game_str_play_id
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% first_based_game_play_id) %>%
    filter(event_code==4)
  nrow(game_events_wanted)
}

double_by_player_2A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("2", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  nrow(player_info |> 
         filter(batter == player & lead(second_baserunner)==player))
}

triple_by_player_2A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("2", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  nrow(player_info |> 
         filter(batter == player & lead(third_baserunner)==player))
}

homeruns <- all_game_events %>%
  filter(event_code==11)

homeruns <- merge(homeruns, all_game_info_cleaned, by = "game_str_play_id")

homerun_batters_2A <- homeruns %>%
  filter(grepl("2", home_team))%>%
  select(batter) %>%
  group_by(batter) %>%
  summarise(homeruns = n()) %>%
  arrange(desc(homeruns))

homerun_by_player_2A <- function(player){
  homerun <- homerun_batters_2A %>%
    filter(batter == player)
  if(nrow(homerun) == 0) return(0)
  return(homerun$homeruns)
}

game_info_2A <- all_game_info_cleaned %>%
  filter(grepl("2", home_team))
all_batters_2A <- unique(na.omit(game_info_2A$batter)) 

stats_per_player_2A <- tibble(player = all_batters_2A)

library(purrr)
stats_per_player_2A <- stats_per_player_2A %>%
  mutate(
    at_bats = map_dbl(player, ~at_bat_by_player_2A(.x)),
    walks = map_dbl(player, ~walk_by_player_2A(.x)),
    singles = map_dbl(player, ~single_by_player_2A(.x)),
    doubles = map_dbl(player, ~double_by_player_2A(.x)),
    triples = map_dbl(player, ~triple_by_player_2A(.x)),
    home_runs = map_dbl(player, ~homerun_by_player_2A(.x)),
    on_base_percentage = (walks + singles + doubles + triples + home_runs) / at_bats,
    wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples) + (2.101 * home_runs)) / at_bats
  )


stats_per_player_2A <- stats_per_player_2A %>%
  mutate(at_bats = ifelse(at_bats<(walks+singles+doubles+triples+home_runs),walks+singles+doubles+triples+home_runs, at_bats),
         on_base_percentage = (walks + singles + doubles + triples + home_runs) / at_bats,
         wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples) + (2.101 * home_runs)) / at_bats)

stats_per_player_2A$homerun_percentage <- stats_per_player_2A$home_runs / stats_per_player_2A$at_bats
stats_per_player_2A$walk_percentage <- stats_per_player_2A$walks / stats_per_player_2A$at_bats
stats_per_player_2A$single_percentage <- stats_per_player_2A$singles / stats_per_player_2A$at_bats
stats_per_player_2A$double_percentage <- stats_per_player_2A$doubles / stats_per_player_2A$at_bats
stats_per_player_2A$triple_percentage <- stats_per_player_2A$triples / stats_per_player_2A$at_bats

stats_per_player_2A <- stats_per_player_2A %>%
  filter(at_bats>=10)

write.csv(stats_per_player_2A, "stats_per_player_2A.csv", row.names = F)

stats_per_player_2A <- read.csv("stats_per_player_2A.csv")

stats_per_player <- read.csv("stats_per_player.csv")

extra_data <- subset(stats_per_player, select = c("player", "speed_percentile", "left_field", "right_field", "center_field",
                                                  "NA_field", "right", "strikeouts", "fly_out_pct",
                                                  "stolen_base_percentage", "caught_stolen_base_percentage", 
                                                  "stolen_base_percentage_2_to_3", "caught_stolen_base_percentage_2_to_3"
))

stats_per_player_2A <- merge(stats_per_player_2A, extra_data, by = "player")

write.csv(stats_per_player_2A, "stats_per_player_2A.csv", row.names = F)

handedness_stats <- read.csv("handedness_stats.csv")

stats_per_player_2A <- read.csv("stats_per_player_2A.csv")

stats_per_player_handedness_2A <- merge(stats_per_player_2A, batter_handedness, by.x = "player", by.y = "batter")

stats_per_player_handedness_against_righty_2A <- stats_per_player_handedness_2A %>%
  mutate(
    single_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "left_right"],
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    double_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "left_right"],
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    triple_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "left_right"],
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    homerun_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "left_right"],
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    on_base_percentage = (walk_percentage + single_percentage + double_percentage + triple_percentage + homerun_percentage))



stats_per_player_handedness_against_lefty_2A <- stats_per_player_handedness_2A %>%
  mutate(
    single_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "right_left"],
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    double_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "right_left"],
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    triple_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "right_left"],
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    homerun_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "right_left"],
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    on_base_percentage = (walk_percentage + single_percentage + double_percentage + triple_percentage + homerun_percentage))

write.csv(stats_per_player_handedness_against_righty_2A, "stats_per_player_handedness_against_righty_2A.csv", row.names = F)
write.csv(stats_per_player_handedness_against_lefty_2A, "stats_per_player_handedness_against_lefty_2A.csv", row.names = F)



#3A
at_bat_by_player_3A <- function(player){
  player_info <- all_game_info %>%
    filter(grepl("3", home_team))%>%
    filter(!is.na(batter)) %>%
    group_by(at_bat_game_str) %>%
    summarise(batter = first(batter)) %>%
    ungroup() %>%
    group_by(batter) %>%
    summarise(at_bats = n()) %>%
    filter(batter == player)
  if(nrow(player_info) == 0) return(0)
  return(player_info$at_bats)
}

walk_by_player_3A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("3", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  first_bases_gotten <- nrow(player_info |> 
                               filter(batter == player & lead(first_baserunner)==player))
  first_based <- (player_info |> 
                    filter(batter == player & lead(first_baserunner)==player))
  first_based_game_play_id <- first_based$game_str_play_id
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% first_based_game_play_id) %>%
    filter(event_code==4)
  singles <- nrow(game_events_wanted)
  walks <- first_bases_gotten - singles
  walks
}

single_by_player_3A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("3", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  first_based <- (player_info |> 
                    filter(batter == player & lead(first_baserunner)==player))
  first_based_game_play_id <- first_based$game_str_play_id
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% first_based_game_play_id) %>%
    filter(event_code==4)
  nrow(game_events_wanted)
}

double_by_player_3A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("3", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  nrow(player_info |> 
         filter(batter == player & lead(second_baserunner)==player))
}

triple_by_player_3A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("3", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  nrow(player_info |> 
         filter(batter == player & lead(third_baserunner)==player))
}

homeruns <- all_game_events %>%
  filter(event_code==11)

homeruns <- merge(homeruns, all_game_info_cleaned, by = "game_str_play_id")

homerun_batters_3A <- homeruns %>%
  filter(grepl("3", home_team))%>%
  select(batter) %>%
  group_by(batter) %>%
  summarise(homeruns = n()) %>%
  arrange(desc(homeruns))

homerun_by_player_3A <- function(player){
  homerun <- homerun_batters_3A %>%
    filter(batter == player)
  if(nrow(homerun) == 0) return(0)
  return(homerun$homeruns)
}

game_info_3A <- all_game_info_cleaned %>%
  filter(grepl("3", home_team))
all_batters_3A <- unique(na.omit(game_info_3A$batter)) 

stats_per_player_3A <- tibble(player = all_batters_3A)

library(purrr)
stats_per_player_3A <- stats_per_player_3A %>%
  mutate(
    at_bats = map_dbl(player, ~at_bat_by_player_3A(.x)),
    walks = map_dbl(player, ~walk_by_player_3A(.x)),
    singles = map_dbl(player, ~single_by_player_3A(.x)),
    doubles = map_dbl(player, ~double_by_player_3A(.x)),
    triples = map_dbl(player, ~triple_by_player_3A(.x)),
    home_runs = map_dbl(player, ~homerun_by_player_3A(.x)),
    on_base_percentage = (walks + singles + doubles + triples + home_runs) / at_bats,
    wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples) + (2.101 * home_runs)) / at_bats
  )


stats_per_player_3A <- stats_per_player_3A %>%
  mutate(at_bats = ifelse(at_bats<(walks+singles+doubles+triples+home_runs),walks+singles+doubles+triples+home_runs, at_bats),
         on_base_percentage = (walks + singles + doubles + triples + home_runs) / at_bats,
         wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples) + (2.101 * home_runs)) / at_bats)

stats_per_player_3A$homerun_percentage <- stats_per_player_3A$home_runs / stats_per_player_3A$at_bats
stats_per_player_3A$walk_percentage <- stats_per_player_3A$walks / stats_per_player_3A$at_bats
stats_per_player_3A$single_percentage <- stats_per_player_3A$singles / stats_per_player_3A$at_bats
stats_per_player_3A$double_percentage <- stats_per_player_3A$doubles / stats_per_player_3A$at_bats
stats_per_player_3A$triple_percentage <- stats_per_player_3A$triples / stats_per_player_3A$at_bats

stats_per_player_3A <- stats_per_player_3A %>%
  filter(at_bats>=10)

write.csv(stats_per_player_3A, "stats_per_player_3A.csv", row.names = F)

stats_per_player_3A <- read.csv("stats_per_player_3A.csv")

stats_per_player <- read.csv("stats_per_player.csv")

extra_data <- subset(stats_per_player, select = c("player", "speed_percentile", "left_field", "right_field", "center_field",
                                                  "NA_field", "right", "strikeouts", "fly_out_pct",
                                                  "stolen_base_percentage", "caught_stolen_base_percentage", 
                                                  "stolen_base_percentage_2_to_3", "caught_stolen_base_percentage_2_to_3"
))

stats_per_player_3A <- merge(stats_per_player_3A, extra_data, by = "player")

write.csv(stats_per_player_3A, "stats_per_player_3A.csv", row.names = F)

handedness_stats <- read.csv("handedness_stats.csv")

stats_per_player_3A <- read.csv("stats_per_player_3A.csv")

stats_per_player_handedness_3A <- merge(stats_per_player_3A, batter_handedness, by.x = "player", by.y = "batter")

stats_per_player_handedness_against_righty_3A <- stats_per_player_handedness_3A %>%
  mutate(
    single_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "left_right"],
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    double_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "left_right"],
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    triple_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "left_right"],
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    homerun_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "left_right"],
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    on_base_percentage = (walk_percentage + single_percentage + double_percentage + triple_percentage + homerun_percentage))



stats_per_player_handedness_against_lefty_3A <- stats_per_player_handedness_3A %>%
  mutate(
    single_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "right_left"],
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    double_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "right_left"],
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    triple_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "right_left"],
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    homerun_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "right_left"],
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    on_base_percentage = (walk_percentage + single_percentage + double_percentage + triple_percentage + homerun_percentage))

write.csv(stats_per_player_handedness_against_righty_3A, "stats_per_player_handedness_against_righty_3A.csv", row.names = F)
write.csv(stats_per_player_handedness_against_lefty_3A, "stats_per_player_handedness_against_lefty_3A.csv", row.names = F)



#4A
at_bat_by_player_4A <- function(player){
  player_info <- all_game_info %>%
    filter(grepl("4", home_team))%>%
    filter(!is.na(batter)) %>%
    group_by(at_bat_game_str) %>%
    summarise(batter = first(batter)) %>%
    ungroup() %>%
    group_by(batter) %>%
    summarise(at_bats = n()) %>%
    filter(batter == player)
  if(nrow(player_info) == 0) return(0)
  return(player_info$at_bats)
}

walk_by_player_4A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("4", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  first_bases_gotten <- nrow(player_info |> 
                               filter(batter == player & lead(first_baserunner)==player))
  first_based <- (player_info |> 
                    filter(batter == player & lead(first_baserunner)==player))
  first_based_game_play_id <- first_based$game_str_play_id
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% first_based_game_play_id) %>%
    filter(event_code==4)
  singles <- nrow(game_events_wanted)
  walks <- first_bases_gotten - singles
  walks
}

single_by_player_4A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("4", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  first_based <- (player_info |> 
                    filter(batter == player & lead(first_baserunner)==player))
  first_based_game_play_id <- first_based$game_str_play_id
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% first_based_game_play_id) %>%
    filter(event_code==4)
  nrow(game_events_wanted)
}

double_by_player_4A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("4", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  nrow(player_info |> 
         filter(batter == player & lead(second_baserunner)==player))
}

triple_by_player_4A <- function(player){
  player_info <- all_game_info |> 
    filter(grepl("4", home_team))%>%
    filter(batter == player | first_baserunner == player | second_baserunner==player | third_baserunner == player)
  nrow(player_info |> 
         filter(batter == player & lead(third_baserunner)==player))
}

homeruns <- all_game_events %>%
  filter(event_code==11)

homeruns <- merge(homeruns, all_game_info_cleaned, by = "game_str_play_id")

homerun_batters_4A <- homeruns %>%
  filter(grepl("4", home_team))%>%
  select(batter) %>%
  group_by(batter) %>%
  summarise(homeruns = n()) %>%
  arrange(desc(homeruns))

homerun_by_player_4A <- function(player){
  homerun <- homerun_batters_4A %>%
    filter(batter == player)
  if(nrow(homerun) == 0) return(0)
  return(homerun$homeruns)
}

game_info_4A <- all_game_info_cleaned %>%
  filter(grepl("4", home_team))
all_batters_4A <- unique(na.omit(game_info_4A$batter)) 

stats_per_player_4A <- tibble(player = all_batters_4A)

library(purrr)
stats_per_player_4A <- stats_per_player_4A %>%
  mutate(
    at_bats = map_dbl(player, ~at_bat_by_player_4A(.x)),
    walks = map_dbl(player, ~walk_by_player_4A(.x)),
    singles = map_dbl(player, ~single_by_player_4A(.x)),
    doubles = map_dbl(player, ~double_by_player_4A(.x)),
    triples = map_dbl(player, ~triple_by_player_4A(.x)),
    home_runs = map_dbl(player, ~homerun_by_player_4A(.x)),
    on_base_percentage = (walks + singles + doubles + triples + home_runs) / at_bats,
    wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples) + (2.101 * home_runs)) / at_bats
  )


stats_per_player_4A <- stats_per_player_4A %>%
  mutate(at_bats = ifelse(at_bats<(walks+singles+doubles+triples+home_runs),walks+singles+doubles+triples+home_runs, at_bats),
         on_base_percentage = (walks + singles + doubles + triples + home_runs) / at_bats,
         wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples) + (2.101 * home_runs)) / at_bats)

stats_per_player_4A$homerun_percentage <- stats_per_player_4A$home_runs / stats_per_player_4A$at_bats
stats_per_player_4A$walk_percentage <- stats_per_player_4A$walks / stats_per_player_4A$at_bats
stats_per_player_4A$single_percentage <- stats_per_player_4A$singles / stats_per_player_4A$at_bats
stats_per_player_4A$double_percentage <- stats_per_player_4A$doubles / stats_per_player_4A$at_bats
stats_per_player_4A$triple_percentage <- stats_per_player_4A$triples / stats_per_player_4A$at_bats

stats_per_player_4A <- stats_per_player_4A %>%
  filter(at_bats>=10)

write.csv(stats_per_player_4A, "stats_per_player_4A.csv", row.names = F)

stats_per_player_4A <- read.csv("stats_per_player_4A.csv")

stats_per_player <- read.csv("stats_per_player.csv")

extra_data <- subset(stats_per_player, select = c("player", "speed_percentile", "left_field", "right_field", "center_field",
                                                  "NA_field", "right", "strikeouts", "fly_out_pct",
                                                  "stolen_base_percentage", "caught_stolen_base_percentage", 
                                                  "stolen_base_percentage_2_to_3", "caught_stolen_base_percentage_2_to_3"
))

stats_per_player_4A <- merge(stats_per_player_4A, extra_data, by = "player")

write.csv(stats_per_player_4A, "stats_per_player_4A.csv", row.names = F)

handedness_stats <- read.csv("handedness_stats.csv")

stats_per_player_4A <- read.csv("stats_per_player_4A.csv")

stats_per_player_handedness_4A <- merge(stats_per_player_4A, batter_handedness, by.x = "player", by.y = "batter")

stats_per_player_handedness_against_righty_4A <- stats_per_player_handedness_4A %>%
  mutate(
    single_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "left_right"],
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    double_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "left_right"],
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    triple_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "left_right"],
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    homerun_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "left_right"],
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    on_base_percentage = (walk_percentage + single_percentage + double_percentage + triple_percentage + homerun_percentage))



stats_per_player_handedness_against_lefty_4A <- stats_per_player_handedness_4A %>%
  mutate(
    single_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "right_left"],
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    double_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "right_left"],
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    triple_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "right_left"],
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    homerun_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "right_left"],
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    on_base_percentage = (walk_percentage + single_percentage + double_percentage + triple_percentage + homerun_percentage))

write.csv(stats_per_player_handedness_against_righty_4A, "stats_per_player_handedness_against_righty_4A.csv", row.names = F)
write.csv(stats_per_player_handedness_against_lefty_4A, "stats_per_player_handedness_against_lefty_4A.csv", row.names = F)



########################################################################

# Function to get the number of at-bats by inning
at_bat_by_inning <- function(Inning){
  player_info <- all_game_info %>%
    filter(!is.na(batter) & inning == Inning) %>%
    group_by(at_bat_game_str) %>%
    summarise(inning = first(inning)) %>%
    ungroup() %>%
    group_by(inning) %>%
    summarise(at_bats = n())
  if(nrow(player_info) == 0) return(0)
  return(player_info$at_bats)
}

# Function to get walks by inning
walk_by_inning <- function(Inning){
  player_info <- all_game_info |> 
    filter(inning == Inning)
  first_bases_gotten <- nrow(player_info |> 
                               filter(lead(first_baserunner) == batter))
  first_based <- player_info |> 
    filter(lead(first_baserunner) == batter)
  first_based_game_play_id <- first_based$game_str_play_id
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% first_based_game_play_id) %>%
    filter(event_code == 4)
  singles <- nrow(game_events_wanted)
  walks <- first_bases_gotten - singles
  walks
}

# Function to get singles by inning
single_by_inning <- function(Inning){
  player_info <- all_game_info |> 
    filter(inning == Inning)
  first_based <- player_info |> 
    filter(lead(first_baserunner) == batter)
  first_based_game_play_id <- first_based$game_str_play_id
  game_events_wanted <- all_game_events %>%
    filter(game_str_play_id %in% first_based_game_play_id) %>%
    filter(event_code == 4)
  nrow(game_events_wanted)
}

# Function to get doubles by inning
double_by_inning <- function(Inning){
  player_info <- all_game_info |> 
    filter(inning == Inning)
  nrow(player_info |> 
         filter(lead(second_baserunner) == batter))
}

# Function to get triples by inning
triple_by_inning <- function(Inning){
  player_info <- all_game_info |> 
    filter(inning == Inning)
  nrow(player_info |> 
         filter(lead(third_baserunner) == batter))
}

# Function to get home runs by inning
homerun_by_inning <- function(Inning){
  homerun <- homerun_batters_by_inning %>%
    filter(inning == Inning)
  if(nrow(homerun) == 0) return(0)
  return(sum(homerun$homeruns))
}

# Create a table for all innings
innings <- rep(1:9)

inning_summary <- data.frame(
  inning = innings,
  at_bats = sapply(innings, at_bat_by_inning),
  walks = sapply(innings, walk_by_inning),
  singles = sapply(innings, single_by_inning),
  doubles = sapply(innings, double_by_inning),
  triples = sapply(innings, triple_by_inning),
  home_runs = sapply(innings, homerun_by_inning)
)

inning_summary <- inning_summary %>%
  mutate(single_percentage = singles/at_bats,
         walk_percentage = walks/at_bats,
         double_percentage = doubles/at_bats,
         triple_percentage = triples/at_bats,
         homerun_percentage = home_runs/at_bats,
         on_base_percentage = single_percentage + walk_percentage + 
           double_percentage + triple_percentage + homerun_percentage)


totals <- data.frame(
  inning = "Total",
  at_bats = sum(inning_summary$at_bats, na.rm = TRUE),
  walks = sum(inning_summary$walks, na.rm = TRUE),
  singles = sum(inning_summary$singles, na.rm = TRUE),
  doubles = sum(inning_summary$doubles, na.rm = TRUE),
  triples = sum(inning_summary$triples, na.rm = TRUE),
  home_runs = sum(inning_summary$home_runs, na.rm = TRUE)
)

totals_summary <- totals %>%
  mutate(single_percentage = singles/at_bats,
         walk_percentage = walks/at_bats,
         double_percentage = doubles/at_bats,
         triple_percentage = triples/at_bats,
         homerun_percentage = home_runs/at_bats,
         on_base_percentage = single_percentage + walk_percentage + 
           double_percentage + triple_percentage + homerun_percentage)

write.csv(totals_summary, "totals_summary.csv")
write.csv(inning_summary, "inning_summary.csv")

###
inning_summary_selected <- subset(inning_summary, select = c("inning", "single_percentage", "walk_percentage", "double_percentage", "triple_percentage", "homerun_percentage"))
inning_summary_long <- reshape2::melt(inning_summary_selected, id.vars = "inning", variable.name = "type", value.name = "percentage")

library(ggplot2)
ggplot(inning_summary_long, aes(x = inning, y = percentage, color = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Percentages over 9 Innings",
       x = "Inning",
       y = "Percentage",
       color = "Type") +
  scale_color_manual(values = c("single_percentage" = "blue", 
                                "walk_percentage" = "red", 
                                "double_percentage" = "green", 
                                "triple_percentage" = "purple", 
                                "homerun_percentage" = "orange")) +
  theme_minimal()

ggplot(inning_summary_long, aes(x = inning, y = percentage, color = type, fill = type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = 0, ymax = percentage), alpha = 0.1) +
  labs(title = "Percentages over 9 Innings",
       x = "Inning",
       y = "Percentage",
       color = "Type",
       fill = "Type") +
  scale_color_manual(values = c("single_percentage" = "blue", 
                                "walk_percentage" = "red", 
                                "double_percentage" = "green", 
                                "triple_percentage" = "purple", 
                                "homerun_percentage" = "orange")) +
  scale_fill_manual(values = c("single_percentage" = "blue", 
                               "walk_percentage" = "red", 
                               "double_percentage" = "green", 
                               "triple_percentage" = "purple", 
                               "homerun_percentage" = "orange")) +
  facet_wrap(~ type, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = 1:9, labels = as.character(1:9)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "none")

library(dplyr)
high_points <- inning_summary_long %>% group_by(type) %>% top_n(1, wt = percentage)
low_points <- inning_summary_long %>% group_by(type) %>% top_n(-1, wt = percentage)

name_mapping <- c(
  "single_percentage" = "Single Percentage",
  "walk_percentage" = "Walk Percentage",
  "double_percentage" = "Double Percentage",
  "triple_percentage" = "Triple Percentage",
  "homerun_percentage" = "Home Run Percentage"
)

batting_stats_by_inning <- ggplot(inning_summary_long, aes(x = inning, y = percentage, color = type, fill = type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = 0, ymax = percentage), alpha = 0.1) +
  geom_point(data = high_points, aes(x = inning, y = percentage), shape = 24, size = 4, fill = "yellow") +
  geom_point(data = low_points, aes(x = inning, y = percentage), shape = 25, size = 4, fill = "black") +
  labs(title = "Batting Statistics Over All 9 Innings",
       x = "Inning",
       y = "Percentage",
       color = "Type",
       fill = "Type") +
  scale_color_manual(values = c("single_percentage" = "blue", 
                                "walk_percentage" = "red", 
                                "double_percentage" = "green", 
                                "triple_percentage" = "purple", 
                                "homerun_percentage" = "orange"),
                     labels = name_mapping) +
  scale_fill_manual(values = c("single_percentage" = "blue", 
                               "walk_percentage" = "red", 
                               "double_percentage" = "green", 
                               "triple_percentage" = "purple", 
                               "homerun_percentage" = "orange"),
                    labels = name_mapping) +
  facet_wrap(~ type, scales = "free_y", ncol = 1, labeller = as_labeller(name_mapping)) +
  scale_x_continuous(breaks = 1:9, labels = as.character(1:9)) +
  theme_bw()+
  theme(strip.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "none")

ggsave("batting_stats_by_inning.png", batting_stats_by_inning, width = 6, height = 8)



##################################################

batter_handedness <- read.csv("batter_handedness.csv")
pitcher_handedness <- read.csv("pitcher_handedness.csv")
handedness_stats <- read.csv("handedness_stats.csv")

right_vs_all_single_percentage <- ((handedness_stats[1,]$single_pct * handedness_stats[1,]$at_bats) + 
                                     (handedness_stats[3,]$single_pct * handedness_stats[3,]$at_bats))/(handedness_stats[3,]$at_bats + handedness_stats[1,]$at_bats)

left_vs_all_single_percentage <- ((handedness_stats[2,]$single_pct * handedness_stats[2,]$at_bats) + 
                                    (handedness_stats[4,]$single_pct * handedness_stats[4,]$at_bats))/(handedness_stats[4,]$at_bats + handedness_stats[2,]$at_bats)

right_vs_all_double_percentage <- ((handedness_stats[1,]$double_pct * handedness_stats[1,]$at_bats) + 
                                     (handedness_stats[3,]$double_pct * handedness_stats[3,]$at_bats))/(handedness_stats[3,]$at_bats + handedness_stats[1,]$at_bats)

left_vs_all_double_percentage <- ((handedness_stats[2,]$double_pct * handedness_stats[2,]$at_bats) + 
                                    (handedness_stats[4,]$double_pct * handedness_stats[4,]$at_bats))/(handedness_stats[4,]$at_bats + handedness_stats[2,]$at_bats)

right_vs_all_triple_percentage <- ((handedness_stats[1,]$triple_pct * handedness_stats[1,]$at_bats) + 
                                     (handedness_stats[3,]$triple_pct * handedness_stats[3,]$at_bats))/(handedness_stats[3,]$at_bats + handedness_stats[1,]$at_bats)

left_vs_all_triple_percentage <- ((handedness_stats[2,]$triple_pct * handedness_stats[2,]$at_bats) + 
                                    (handedness_stats[4,]$triple_pct * handedness_stats[4,]$at_bats))/(handedness_stats[4,]$at_bats + handedness_stats[2,]$at_bats)

right_vs_all_hr_percentage <- ((handedness_stats[1,]$hr_pct * handedness_stats[1,]$at_bats) + 
                                 (handedness_stats[3,]$hr_pct * handedness_stats[3,]$at_bats))/(handedness_stats[3,]$at_bats + handedness_stats[1,]$at_bats)

left_vs_all_hr_percentage <- ((handedness_stats[2,]$hr_pct * handedness_stats[2,]$at_bats) + 
                                (handedness_stats[4,]$hr_pct * handedness_stats[4,]$at_bats))/(handedness_stats[4,]$at_bats + handedness_stats[2,]$at_bats)

right_vs_all_on_base_percentage <- ((handedness_stats[1,]$on_base_pct * handedness_stats[1,]$at_bats) + 
                                      (handedness_stats[3,]$on_base_pct * handedness_stats[3,]$at_bats))/(handedness_stats[3,]$at_bats + handedness_stats[1,]$at_bats)

left_vs_all_on_base_percentage <- ((handedness_stats[2,]$on_base_pct * handedness_stats[2,]$at_bats) + 
                                     (handedness_stats[4,]$on_base_pct * handedness_stats[4,]$at_bats))/(handedness_stats[4,]$at_bats + handedness_stats[2,]$at_bats)

right_vs_all_slug_percentage <- ((handedness_stats[1,]$slug_pct * handedness_stats[1,]$at_bats) + 
                                   (handedness_stats[3,]$slug_pct * handedness_stats[3,]$at_bats))/(handedness_stats[3,]$at_bats + handedness_stats[1,]$at_bats)

left_vs_all_slug_percentage <- ((handedness_stats[2,]$slug_pct * handedness_stats[2,]$at_bats) + 
                                  (handedness_stats[4,]$slug_pct * handedness_stats[4,]$at_bats))/(handedness_stats[4,]$at_bats + handedness_stats[2,]$at_bats)

right_vs_all_ops <- ((handedness_stats[1,]$ops * handedness_stats[1,]$at_bats) + 
                       (handedness_stats[3,]$ops * handedness_stats[3,]$at_bats))/(handedness_stats[3,]$at_bats + handedness_stats[1,]$at_bats)

left_vs_all_ops <- ((handedness_stats[2,]$ops * handedness_stats[2,]$at_bats) + 
                      (handedness_stats[4,]$ops * handedness_stats[4,]$at_bats))/(handedness_stats[4,]$at_bats + handedness_stats[2,]$at_bats)

handedness_stats$single_pct_oa <- NA
handedness_stats$double_pct_oa <- NA
handedness_stats$triple_pct_oa <- NA
handedness_stats$hr_pct_oa <- NA
handedness_stats$on_base_pct_oa <- NA
handedness_stats$slug_pct_oa <- NA
handedness_stats$ops_oa <- NA

handedness_stats[1,]$single_pct_oa <- handedness_stats[1,]$single_pct/right_vs_all_single_percentage
handedness_stats[1,]$double_pct_oa <- handedness_stats[1,]$double_pct/right_vs_all_double_percentage
handedness_stats[1,]$triple_pct_oa <- handedness_stats[1,]$triple_pct/right_vs_all_triple_percentage
handedness_stats[1,]$hr_pct_oa <- handedness_stats[1,]$hr_pct/right_vs_all_hr_percentage
handedness_stats[1,]$on_base_pct_oa <- handedness_stats[1,]$on_base_pct/right_vs_all_on_base_percentage
handedness_stats[1,]$slug_pct_oa <- handedness_stats[1,]$slug_pct/right_vs_all_slug_percentage
handedness_stats[1,]$ops_oa <- handedness_stats[1,]$ops/right_vs_all_ops

handedness_stats[2,]$single_pct_oa <- handedness_stats[2,]$single_pct/left_vs_all_single_percentage
handedness_stats[2,]$double_pct_oa <- handedness_stats[2,]$double_pct/left_vs_all_double_percentage
handedness_stats[2,]$triple_pct_oa <- handedness_stats[2,]$triple_pct/left_vs_all_triple_percentage
handedness_stats[2,]$hr_pct_oa <- handedness_stats[2,]$hr_pct/left_vs_all_hr_percentage
handedness_stats[2,]$on_base_pct_oa <- handedness_stats[2,]$on_base_pct/left_vs_all_on_base_percentage
handedness_stats[2,]$slug_pct_oa <- handedness_stats[2,]$slug_pct/left_vs_all_slug_percentage
handedness_stats[2,]$ops_oa <- handedness_stats[2,]$ops/left_vs_all_ops

handedness_stats[3,]$single_pct_oa <- handedness_stats[3,]$single_pct/right_vs_all_single_percentage
handedness_stats[3,]$double_pct_oa <- handedness_stats[3,]$double_pct/right_vs_all_double_percentage
handedness_stats[3,]$triple_pct_oa <- handedness_stats[3,]$triple_pct/right_vs_all_triple_percentage
handedness_stats[3,]$hr_pct_oa <- handedness_stats[3,]$hr_pct/right_vs_all_hr_percentage
handedness_stats[3,]$on_base_pct_oa <- handedness_stats[3,]$on_base_pct/right_vs_all_on_base_percentage
handedness_stats[3,]$slug_pct_oa <- handedness_stats[3,]$slug_pct/right_vs_all_slug_percentage
handedness_stats[3,]$ops_oa <- handedness_stats[3,]$ops/right_vs_all_ops

handedness_stats[4,]$single_pct_oa <- handedness_stats[4,]$single_pct/left_vs_all_single_percentage
handedness_stats[4,]$double_pct_oa <- handedness_stats[4,]$double_pct/left_vs_all_double_percentage
handedness_stats[4,]$triple_pct_oa <- handedness_stats[4,]$triple_pct/left_vs_all_triple_percentage
handedness_stats[4,]$hr_pct_oa <- handedness_stats[4,]$hr_pct/left_vs_all_hr_percentage
handedness_stats[4,]$on_base_pct_oa <- handedness_stats[4,]$on_base_pct/left_vs_all_on_base_percentage
handedness_stats[4,]$slug_pct_oa <- handedness_stats[4,]$slug_pct/left_vs_all_slug_percentage
handedness_stats[4,]$ops_oa <- handedness_stats[4,]$ops/left_vs_all_ops

batter_handedness <- subset(batter_handedness, select = c("batter", "handedness"))
stats_per_player_handedness <- merge(stats_per_player, batter_handedness, by.x = "player", by.y = "batter")

stats_per_player_handedness_against_righty <- stats_per_player_handedness %>%
  mutate(
    single_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "left_right"],
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    double_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "left_right"],
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    triple_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "left_right"],
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "right_right"]
    ),
    homerun_percentage = ifelse(
      handedness %in% c("Left", "Switch"),
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "left_right"],
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "right_right"]
    ))



stats_per_player_handedness_against_lefty <- stats_per_player_handedness %>%
  mutate(
    single_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "right_left"],
      single_percentage * handedness_stats$single_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    double_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "right_left"],
      double_percentage * handedness_stats$double_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    triple_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "right_left"],
      triple_percentage * handedness_stats$triple_pct_oa[handedness_stats$matchup == "left_left"]
    ),
    homerun_percentage = ifelse(
      handedness %in% c("Right", "Switch"),
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "right_left"],
      homerun_percentage * handedness_stats$hr_pct_oa[handedness_stats$matchup == "left_left"]
    ))


write.csv(handedness_stats, "handedness_stats.csv", row.names = F)

library(reshape2)
handedness_stats_selected <- subset(handedness_stats, select = c("matchup", "single_pct", "double_pct", "triple_pct", "hr_pct", "on_base_pct", "slug_pct", "ops"))
handedness_stats_long <- melt(handedness_stats_selected, id.vars = "matchup", variable.name = "metric", value.name = "value")

ggplot(handedness_stats_long, aes(x = metric, y = value, fill = matchup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Metrics by Handedness Matchup",
       x = "Metric",
       y = "Value",
       fill = "Handedness Matchup") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

handedness_stats_long$matchup <- factor(handedness_stats_long$matchup,
                                        levels = c("left_left", "right_right", "left_right", "right_left"))

legend_labels <- c(
  "left_left" = "Lefty Batter vs Lefty Pitcher",
  "right_right" = "Righty Batter vs Righty Pitcher",
  "left_right" = "Lefty Batter vs Righty Pitcher",
  "right_left" = "Righty Batter vs Lefty Pitcher"
)

x_axis_labels <- c(
  "single_pct" = "Single Percentage",
  "double_pct" = "Double Percentage",
  "triple_pct" = "Triple Percentage",
  "hr_pct" = "Home Run Percentage",
  "on_base_pct" = "On-Base Percentage",
  "slug_pct" = "Slugging Percentage",
  "ops" = "OPS"
)


handedness_plot <- ggplot(handedness_stats_long, aes(x = metric, y = value, fill = matchup)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Comparison of Metrics by Handedness Matchup",
       x = "",
       y = "",
       fill = "Handedness Matchup") +
  scale_fill_manual(values = c("left_left" = "red", 
                               "right_right" = "lightcoral", 
                               "left_right" = "springgreen4", 
                               "right_left" = "palegreen2"),
                    labels = legend_labels) +
  scale_x_discrete(labels = x_axis_labels) +
  theme_classic() +
  theme(axis.text.x = element_text(),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        panel.grid.major = element_line(size = 0.5, color = "gray"),
        panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave("handedness_plot.png", handedness_plot, width = 10, height = 5)

#######################################

batting_orders <- all_game_info %>%
  filter(!is.na(batter) & !is.na(at_bat)) %>%
  group_by(game_str, at_bat, batting_team) %>%
  summarise(batter = first(batter),
            inning = first(inning)) %>%
  ungroup() %>%
  group_by(game_str, batting_team) %>%
  filter(n() >= 9) %>%
  arrange(game_str, batting_team, at_bat) %>%
  mutate(batting_appearance = row_number(),
         batting_order = ifelse((batting_appearance %% 9)==0, 9, (batting_appearance %% 9)),
         game_inning = paste0(game_str, batting_team, inning))

lineup_data <- batting_orders %>%
  group_by(game_str) %>%
  slice(1:9)

lineup_data_2 <- lineup_data %>%
  group_by(game_str, batting_team) %>%
  arrange(batting_order) %>%
  summarise(lineup = paste(batter, collapse = "-")) %>%
  ungroup()

lineup_counts <- lineup_data_2 %>%
  group_by(batting_team, lineup) %>%
  summarise(count = n()) %>%
  arrange(batting_team, desc(count)) %>%
  filter(grepl("Home", batting_team))

combination_data <- lineup_data %>%
  group_by(game_str, batting_team) %>%
  summarise(batters = paste(sort(batter), collapse = "-")) %>%
  ungroup()

combination_counts <- combination_data %>%
  group_by(batting_team, batters) %>%
  summarise(count = n()) %>%
  arrange(batting_team, desc(count))

write.csv(lineup_data_2, "lineup_data1.csv")
write.csv(lineup_counts, "lineup_counts.csv")
write.csv(combination_data, "combination_data.csv")
write.csv(combination_counts, "combination_counts.csv")


#########################################