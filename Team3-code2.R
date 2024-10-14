library(tidyverse)
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

all_game_events <- game_events |> 
  collect()

all_game_info <- game_info |> 
  collect()

all_player_pos <- player_pos |> 
  collect()

all_ball_pos <- ball_pos |> 
  collect()

all_team_info <- team_info |> 
  collect()

all_game_info <- all_game_info |> left_join(all_game_events |> 
                                              select(game_str, play_per_game, play_id) |> 
                                              distinct(game_str, play_per_game, .keep_all = TRUE),
                                            by = c("game_str", "play_per_game")) 

# stats by inning ---------------------------------------------------------

first_inning_info <- all_game_info |> 
  filter(inning == 1)

first_inning_events <- all_game_events |> 
  semi_join(first_inning_info, by = c("game_str", "play_per_game"))

first_inning_home_runs <- as.numeric(sum(first_inning_events$event_code == 11))
first_inning_events |> 
  distinct(game_str, at_bat) |> 
  nrow()
View(first_inning_info |> 
  filter(batter == lead(first_baserunner)))

home_runs_by_inning <- function(hr_inning){
  inning_info <- all_game_info |> 
    filter(inning == hr_inning)
  inning_events <- all_game_events |> 
    semi_join(inning_info, by = c("game_str", "play_per_game"))
  return(as.numeric(sum(inning_events$event_code == 11)))
}
home_runs_by_inning(1)

at_bats_by_inning <- function(ab_inning){
  inning_info <- all_game_info |> 
    filter(inning == ab_inning)
  inning_events <- all_game_events |> 
    semi_join(inning_info, by = c("game_str", "play_per_game"))
  return(inning_events |> 
    distinct(game_str, at_bat) |> 
    nrow())
}
at_bats_by_inning(1)

non_hr_onbase_per_inning <- function(bb_inning){
  inning_info <- all_game_info |> 
    filter(inning == bb_inning)
  inning_events <- all_game_events |> 
    semi_join(inning_info, by = c("game_str", "play_per_game"))
  nrow(inning_info |> 
    filter(batter == lead(first_baserunner) | batter == lead(second_baserunner) |
             batter == lead(third_baserunner)))
}
non_hr_onbase_per_inning(1)

single_per_inning <- function(single_inning){
  inning_info <- all_game_info |> 
    filter(inning == single_inning)
  nrow(inning_info |> 
         filter(batter == lead(first_baserunner)))
}
double_per_inning <- function(double_inning){
  inning_info <- all_game_info |> 
    filter(inning == double_inning)
  nrow(inning_info |> 
         filter(batter == lead(second_baserunner)))
}
triple_per_inning <- function(triple_inning){
  inning_info <- all_game_info |> 
    filter(inning == triple_inning)
  nrow(inning_info |> 
         filter(batter == lead(third_baserunner)))
}

stats_per_inning <- tibble(inning = 1:9)
stats_per_inning <- stats_per_inning |> 
  mutate(home_runs = map_dbl(inning, home_runs_by_inning),
         at_bats = map_dbl(inning, at_bats_by_inning),
         hr_pct = home_runs / at_bats,
         on_base_pct = (map_dbl(inning, non_hr_onbase_per_inning) + home_runs) / at_bats,
         slug_pct = (map_dbl(inning, single_per_inning) 
         + 2 * map_dbl(inning, double_per_inning) + 3 * map_dbl(inning, triple_per_inning)
         + 4 * home_runs) / at_bats)

write.csv(stats_per_inning, "batting_stats_per_inning.csv", row.names = FALSE)


# stats per pitch number --------------------------------------------------

top_pitches <- all_game_info |> 
  filter(top_bottom == "top") |> 
  mutate(pitch_count = play_per_game) |> 
  mutate(pitch_count = ifelse(play_per_game == 1, 1, ifelse(lag(pitcher) == pitcher, lag(pitch_count) + 1, 1))) |> 
  select(game_str, at_bat, play_per_game, pitcher, pitch_count)

top_pitches <- all_game_info |> 
  filter(top_bottom == "top") |> 
  group_by(game_str, pitcher) |> 
  mutate(pitch_count = row_number()) |> 
  ungroup()

bottom_pitches <- all_game_info |> 
  filter(top_bottom == "bottom") |> 
  group_by(game_str, pitcher) |> 
  mutate(pitch_count = row_number()) |> 
  ungroup()

all_pitch_count <- bind_rows(top_pitches, bottom_pitches) |> 
  arrange(game_str, play_per_game)

all_pitch_count |> 
  filter(!is.na(pitcher)) |> 
  ggplot(aes(x = pitch_count)) + 
  geom_histogram(bins = 24)

stats_by_pitch_count <- function(lower, upper){
  info <- all_pitch_count |> 
    filter(lower <= pitch_count & pitch_count <= upper & !is.na(pitcher))
  events <- all_game_events |> 
    semi_join(info, by = c("game_str", "play_per_game"))
  home_runs <- as.numeric(sum(events$event_code == 11))
  at_bats <- events |> 
    distinct(game_str, at_bat) |> 
    nrow()
  singles <- nrow(info |> 
                    filter(batter == lead(first_baserunner)))
  doubles <- nrow(info |> 
                    filter(batter == lead(second_baserunner)))
  triples <- nrow(info |> 
                    filter(batter == lead(third_baserunner)))
  on_base_pct <- (singles + doubles + triples + home_runs) / at_bats
  slug_pct <- (singles + 2 * doubles + 3 * triples + 4 * home_runs) / at_bats
  ops <- on_base_pct + slug_pct
  pitch_count <- paste(lower, "-", upper)
  return(tibble(pitch_count, at_bats, home_runs, on_base_pct, slug_pct, ops))
}
stats_by_pitch <- tibble()
for (i in seq(0, 110, by = 10)){
  stats <- stats_by_pitch_count(i, i + 10)
  stats_by_pitch <- bind_rows(stats_by_pitch, stats)
}

write.csv(stats_by_pitch, "batting_stats_by_pitch_count.csv", row.names = FALSE)


# batting stats with RISP vs without --------------------------------------

stats_by_risp <- function(player_id = "all"){
  if (player_id != "all"){
    info_no_risp <- all_game_info |> 
      filter(is.na(second_baserunner) & is.na(third_baserunner) & batter == player_id)
    info_risp <- all_game_info |> 
      filter((!is.na(second_baserunner) | !is.na(third_baserunner)) & batter == player_id)
    events_no_risp <- all_game_events |> 
      semi_join(info_no_risp, by = c("game_str", "play_per_game"))
    events_risp <- all_game_events |> 
      semi_join(info_risp, by = c("game_str", "play_per_game"))
    home_runs_no_risp <- as.numeric(sum(events_no_risp$event_code == 11))
    at_bats_no_risp <- events_no_risp |> 
      distinct(game_str, at_bat) |> 
      nrow()
    singles_no_risp <- nrow(all_game_info |> 
                              filter(is.na(second_baserunner) & is.na(third_baserunner) &
                                       batter == lead(first_baserunner) & batter == player_id))
    doubles_no_risp <- nrow(all_game_info |> 
                              filter(is.na(second_baserunner) & is.na(third_baserunner) &
                                       batter == lead(second_baserunner) & batter == player_id))
    triples_no_risp <- nrow(all_game_info |> 
                              filter(is.na(second_baserunner) & is.na(third_baserunner) &
                                       batter == lead(third_baserunner) & batter == player_id))
    single_pct_no_risp <- singles_no_risp / at_bats_no_risp
    double_pct_no_risp <- doubles_no_risp / at_bats_no_risp
    triple_pct_no_risp <- triples_no_risp / at_bats_no_risp
    hr_pct_no_risp <- home_runs_no_risp / at_bats_no_risp
    on_base_pct_no_risp <- (singles_no_risp + doubles_no_risp + triples_no_risp 
                            + home_runs_no_risp) / at_bats_no_risp
    slug_pct_no_risp <- (singles_no_risp + 2 * doubles_no_risp + 3 * triples_no_risp
                         + 4 * home_runs_no_risp) / at_bats_no_risp
    ops_no_risp <- on_base_pct_no_risp + slug_pct_no_risp
    no_risp_tibble <- tibble(player_id, risp = "No RISP", at_bats = at_bats_no_risp,
                             single_pct = single_pct_no_risp, double_pct = double_pct_no_risp,
                             triple_pct = triple_pct_no_risp, hr_pct = hr_pct_no_risp,
                             on_base_pct = on_base_pct_no_risp, slug_pct = slug_pct_no_risp,
                             ops = ops_no_risp)
    
    home_runs_risp <- as.numeric(sum(events_risp$event_code == 11))
    at_bats_risp <- events_risp |> 
      distinct(game_str, at_bat) |> 
      nrow()
    singles_risp <- nrow(all_game_info |> 
                           filter((!is.na(second_baserunner) | !is.na(third_baserunner)) &
                                    batter == lead(first_baserunner) & batter == player_id))
    doubles_risp <- nrow(all_game_info |> 
                           filter((!is.na(second_baserunner) | !is.na(third_baserunner)) &
                                    batter == lead(second_baserunner) & batter == player_id))
    triples_risp <- nrow(all_game_info |> 
                           filter((!is.na(second_baserunner) | !is.na(third_baserunner)) &
                                    batter == lead(third_baserunner) & batter == player_id))
    single_pct_risp <- singles_risp / at_bats_risp
    double_pct_risp <- doubles_risp / at_bats_risp
    triple_pct_risp <- triples_risp / at_bats_risp
    hr_pct_risp <- home_runs_risp / at_bats_risp
    on_base_pct_risp <- (singles_risp + doubles_risp + triples_risp 
                         + home_runs_risp) / at_bats_risp
    slug_pct_risp <- (singles_risp + 2 * doubles_risp + 3 * triples_risp
                      + 4 * home_runs_risp) / at_bats_risp
    ops_risp <- on_base_pct_risp + slug_pct_risp
    risp_tibble <- tibble(player_id, risp = "RISP", at_bats = at_bats_risp,
                          single_pct = single_pct_risp, double_pct = double_pct_risp,
                          triple_pct = triple_pct_risp, hr_pct = hr_pct_risp,
                          on_base_pct = on_base_pct_risp, slug_pct = slug_pct_risp,
                          ops = ops_risp)
    full_tibble <- bind_rows(no_risp_tibble, risp_tibble)
    return(full_tibble)
  } else{
    info_no_risp <- all_game_info |> 
      filter(is.na(second_baserunner) & is.na(third_baserunner))
    info_risp <- all_game_info |> 
      filter(!is.na(second_baserunner) | !is.na(third_baserunner))
    events_no_risp <- all_game_events |> 
      semi_join(info_no_risp, by = c("game_str", "play_per_game"))
    events_risp <- all_game_events |> 
      semi_join(info_risp, by = c("game_str", "play_per_game"))
    home_runs_no_risp <- as.numeric(sum(events_no_risp$event_code == 11))
    at_bats_no_risp <- events_no_risp |> 
      distinct(game_str, at_bat) |> 
      nrow()
    singles_no_risp <- nrow(all_game_info |> 
                              filter(is.na(second_baserunner) & is.na(third_baserunner) &
                                       batter == lead(first_baserunner)))
    doubles_no_risp <- nrow(all_game_info |> 
                              filter(is.na(second_baserunner) & is.na(third_baserunner) &
                                       batter == lead(second_baserunner)))
    triples_no_risp <- nrow(all_game_info |> 
                              filter(is.na(second_baserunner) & is.na(third_baserunner) &
                                       batter == lead(third_baserunner)))
    single_pct_no_risp <- singles_no_risp / at_bats_no_risp
    double_pct_no_risp <- doubles_no_risp / at_bats_no_risp
    triple_pct_no_risp <- triples_no_risp / at_bats_no_risp
    hr_pct_no_risp <- home_runs_no_risp / at_bats_no_risp
    on_base_pct_no_risp <- (singles_no_risp + doubles_no_risp + triples_no_risp 
                            + home_runs_no_risp) / at_bats_no_risp
    slug_pct_no_risp <- (singles_no_risp + 2 * doubles_no_risp + 3 * triples_no_risp
                         + 4 * home_runs_no_risp) / at_bats_no_risp
    ops_no_risp <- on_base_pct_no_risp + slug_pct_no_risp
    no_risp_tibble <- tibble(player_id, risp = "No RISP", at_bats = at_bats_no_risp,
                             single_pct = single_pct_no_risp, double_pct = double_pct_no_risp,
                             triple_pct = triple_pct_no_risp, hr_pct = hr_pct_no_risp,
                             on_base_pct = on_base_pct_no_risp, slug_pct = slug_pct_no_risp,
                             ops = ops_no_risp)
    
    home_runs_risp <- as.numeric(sum(events_risp$event_code == 11))
    at_bats_risp <- events_risp |> 
      distinct(game_str, at_bat) |> 
      nrow()
    singles_risp <- nrow(all_game_info |> 
                           filter((!is.na(second_baserunner) | !is.na(third_baserunner)) &
                                    batter == lead(first_baserunner)))
    doubles_risp <- nrow(all_game_info |> 
                           filter((!is.na(second_baserunner) | !is.na(third_baserunner)) &
                                    batter == lead(second_baserunner)))
    triples_risp <- nrow(all_game_info |> 
                           filter((!is.na(second_baserunner) | !is.na(third_baserunner)) &
                                    batter == lead(third_baserunner)))
    single_pct_risp <- singles_risp / at_bats_risp
    double_pct_risp <- doubles_risp / at_bats_risp
    triple_pct_risp <- triples_risp / at_bats_risp
    hr_pct_risp <- home_runs_risp / at_bats_risp
    on_base_pct_risp <- (singles_risp + doubles_risp + triples_risp 
                         + home_runs_risp) / at_bats_risp
    slug_pct_risp <- (singles_risp + 2 * doubles_risp + 3 * triples_risp
                      + 4 * home_runs_risp) / at_bats_risp
    ops_risp <- on_base_pct_risp + slug_pct_risp
    risp_tibble <- tibble(player_id, risp = "RISP", at_bats = at_bats_risp,
                          single_pct = single_pct_risp, double_pct = double_pct_risp,
                          triple_pct = triple_pct_risp, hr_pct = hr_pct_risp,
                          on_base_pct = on_base_pct_risp, slug_pct = slug_pct_risp,
                          ops = ops_risp)
    full_tibble <- bind_rows(no_risp_tibble, risp_tibble)
    return(full_tibble)
  }
  
}
stats_by_risp(586)
stats_by_risp(492)
risp_all <- stats_by_risp()
player_ids <- unique(all_team_info$player_id)
risp_by_player <- bind_rows(map(player_ids, stats_by_risp))
risp_by_player <- risp_by_player |> 
  group_by(player_id) |> 
  filter(sum(at_bats) > 0) |> 
  ungroup()

risp_by_player |> 
  ggplot(aes(ops, fill = risp)) +
  geom_density(alpha = 0.8)

ops_data <- risp_by_player |> 
  select(player_id, risp, ops) |> 
  spread(key = risp, value = ops) |> 
  filter(!is.na(`No RISP`), !is.na(RISP))

t.test(ops_data$`No RISP`, ops_data$RISP, paired = TRUE)

risp_breakdown <- bind_rows(risp_all, risp_by_player |> 
                              mutate(player_id = as.character(player_id)))

write.csv(risp_breakdown, "batting_stats_by_risp.csv", row.names = FALSE)


# Batter and Pitcher Handedness -------------------------------------------

batter_player_pos <- player_pos |> 
  filter(player_position == 10) |> 
  select(game_str:field_y) |> 
  collect()

pitch_timestamps <- all_game_events |> 
  filter(event_code == 1 & player_position == 1) |> 
  select(game_str:event_code)

batter_ids <- all_game_info |> 
  select(game_str, at_bat, play_per_game, batter) |> 
  filter(!is.na(batter))

batter_actions <- batter_player_pos |> 
  filter(timestamp %in% pitch_timestamps$timestamp) |> 
  select(-player_position)

batter_actions <- left_join(batter_actions, pitch_timestamps) |> 
  select(-player_position, -event_code) |> 
  filter(!is.na(at_bat) & !is.na(play_per_game))

batter_actions <- batter_actions |> 
  left_join(batter_ids) |> 
  filter(!is.na(batter))

batter_positions <- batter_actions |> 
  group_by(batter) |> 
  summarise(at_bats = n(),
            mean_x = mean(field_x)) |> 
  mutate(handedness = case_when(
    mean_x > 1  ~ "Left",
    mean_x < -1 ~ "Right",
    TRUE ~ "Switch"
  ))

batter_positions |> 
  group_by(handedness) |> 
  summarise(count = n())

pitcher_player_pos <- player_pos |> 
  filter(player_position == 1) |> 
  select(game_str:field_y) |> 
  collect()

pitcher_ids <- all_game_info |> 
  select(game_str, at_bat, play_per_game, pitcher) |> 
  filter(!is.na(pitcher))

pitcher_actions <- pitcher_player_pos |> 
  filter(timestamp %in% pitch_timestamps$timestamp) |> 
  select(-player_position)

pitcher_actions <- left_join(pitcher_actions, pitch_timestamps) |> 
  select(-player_position, -event_code) |> 
  filter(!is.na(at_bat) & !is.na(play_per_game))

pitcher_actions <- pitcher_actions |> 
  left_join(pitcher_ids) |> 
  filter(!is.na(pitcher))

pitcher_positions <- pitcher_actions |> 
  group_by(pitcher) |> 
  summarise(pitches = n(),
            mean_x = mean(field_x)) |> 
  mutate(handedness = case_when(
    mean_x > 0  ~ "Left",
    mean_x < 0 ~ "Right"
  ))

pitcher_positions |> 
  group_by(handedness) |> 
  summarise(count = n())

matchups <- all_game_info |> 
  select(game_str, at_bat, pitcher, batter, first_baserunner, second_baserunner,
         third_baserunner) |> 
  distinct(game_str, at_bat, pitcher, batter)

matchups <- matchups |> 
  left_join(pitcher_positions |> select(pitcher, pitcher_handedness = handedness)) |> 
  left_join(batter_positions |> select(batter, batter_handedness = handedness)) |> 
  filter(!is.na(pitcher_handedness) & !is.na(batter_handedness))

right_right <- matchups |> 
  filter(batter_handedness == "Right" & pitcher_handedness == "Right")

left_left <- matchups |> 
  filter(batter_handedness == "Left" & pitcher_handedness == "Left")

right_left <- matchups |> 
  filter((batter_handedness == "Right" | batter_handedness == "Switch")
         & pitcher_handedness == "Left")

left_right <- matchups |> 
  filter((batter_handedness == "Left" | batter_handedness == "Switch")
         & pitcher_handedness == "Right")

stats_by_handedness <- function(data){
  simple_data <- data |> 
    select(game_str, at_bat)
  info <- all_game_info |> 
    semi_join(simple_data)
  events <- all_game_events |> 
    semi_join(simple_data)
  home_runs <- as.numeric(sum(events$event_code == 11))
  at_bats <- events |> 
    distinct(game_str, at_bat) |> 
    nrow()
  singles <- nrow(info |> 
                    filter(batter == lead(first_baserunner)))
  doubles <- nrow(info |> 
                    filter(batter == lead(second_baserunner)))
  triples <- nrow(info |> 
                    filter(batter == lead(third_baserunner)))
  single_pct <- singles / at_bats
  double_pct <- doubles / at_bats
  triple_pct <- triples / at_bats
  hr_pct <- home_runs / at_bats
  on_base_pct <- (singles + doubles + triples + home_runs) / at_bats
  slug_pct <- (singles + 2 * doubles + 3 * triples + 4 * home_runs) / at_bats
  ops <- on_base_pct + slug_pct
  matchup <- deparse(substitute(data))
  return(tibble(matchup, at_bats, single_pct, double_pct, triple_pct,
                hr_pct, on_base_pct, slug_pct, ops))
}
handedness_stats <- bind_rows(stats_by_handedness(right_right), stats_by_handedness(left_left),
                              stats_by_handedness(right_left), stats_by_handedness(left_right))

write.csv(handedness_stats, "handedness_stats.csv", row.names = FALSE)

pitcher_positions |> 
  group_by(handedness) |> 
  rename(player_id = pitcher) |> 
  filter(player_id %in% all_team_info$player_id) |> 
  summarise(count = n())

pitcher_positions |> 
  group_by(handedness) |> 
  rename(player_id = pitcher) |> 
  filter(!(player_id %in% all_team_info$player_id)) |> 
  summarise(count = n())

write.csv(batter_positions, "batter_handedness.csv", row.names = FALSE)
write.csv(pitcher_positions, "pitcher_handedness.csv", row.names = FALSE)

# stats per long inning pitches -------------------------------------------

pitch_count_by_inning <- all_game_info |> 
  filter(!is.na(pitcher) & inning != 0) |> 
  group_by(game_str, pitcher, inning) |> 
  mutate(pitch_count = row_number()) |> 
  ungroup()

pitch_count_by_inning |> 
  ggplot(aes(x = pitch_count)) + 
  geom_histogram(bins = 10)

stats_by_inning_pitch_count <- function(lower, upper){
  info <- pitch_count_by_inning |> 
    filter(lower <= pitch_count & pitch_count <= upper) |> 
    mutate(game_play_per_game = paste(game_str, play_per_game))
  events <- all_game_events |> 
    semi_join(info, by = c("game_str", "play_per_game")) |> 
    mutate(game_play_per_game = paste(game_str, play_per_game))
  home_runs <- as.numeric(sum(events$event_code == 11))
  at_bats <- events |> 
    distinct(game_str, at_bat) |> 
    nrow()
  firstbases <- nrow(info |> 
                    filter(batter == lead(first_baserunner)))
  firstbase_info <- info |> 
    filter(batter == lead(first_baserunner))
  firstbase_playpergame <- firstbase_info$game_play_per_game
  single_events <- events |> 
    filter(game_play_per_game %in% firstbase_playpergame) |> 
    filter(event_code == 4)
  singles = nrow(single_events)
  walks <- firstbases - singles
  doubles <- nrow(info |> 
                    filter(batter == lead(second_baserunner)))
  triples <- nrow(info |> 
                    filter(batter == lead(third_baserunner)))
  walk_pct <- walks / at_bats
  single_pct <- singles / at_bats
  double_pct <- doubles / at_bats
  triple_pct <- triples / at_bats
  hr_pct <- home_runs / at_bats
  on_base_pct <- (singles + walks + doubles + triples + home_runs) / at_bats
  slug_pct <- (singles + walks + 2 * doubles + 3 * triples + 4 * home_runs) / at_bats
  ops <- on_base_pct + slug_pct
  wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples)
          + (2.101 * home_runs)) / at_bats
  pitch_count <- paste(lower, "-", upper)
  return(tibble(pitch_count, at_bats, walks, singles, doubles, triples,
                home_runs, walk_pct, single_pct, double_pct, triple_pct, hr_pct,
                on_base_pct, slug_pct, ops, wOBA))
}
stats_by_inning_pitch <- tibble()
for (i in seq(0, 40, by = 5)){
  stats <- stats_by_inning_pitch_count(i, i + 5)
  stats_by_inning_pitch <- bind_rows(stats_by_inning_pitch, stats)
}

write.csv(stats_by_inning_pitch, "batting_stats_by_inning_pitch_count.csv", 
          row.names = FALSE)

at_bat_count_by_inning <- all_game_info |> 
  filter(!is.na(pitcher) & inning != 0) |> 
  group_by(game_str, pitcher, inning) |> 
  mutate(at_bat_by_inning = dense_rank(at_bat)) |> 
  ungroup()

at_bat_count_by_inning |> 
  ggplot(aes(x = at_bat_by_inning)) + 
  geom_histogram(bins = 10)

stats_by_inning_at_bat_count <- function(number){
  info <- at_bat_count_by_inning |> 
    mutate(game_play_per_game = paste(game_str, play_per_game))
  valid_plays <- at_bat_count_by_inning |> 
    filter(at_bat_by_inning == number) |> 
    mutate(game_play_per_game = paste(game_str, play_per_game))
  events <- all_game_events |> 
    semi_join(info, by = c("game_str", "play_per_game")) |> 
    mutate(game_play_per_game = paste(game_str, play_per_game))
  home_runs <- as.numeric(nrow(events |> 
                                 semi_join(valid_plays, by = c("game_str", "play_per_game")) |> 
                                 filter(event_code == 11)))
  at_bats <- valid_plays |> 
    distinct(game_str, at_bat) |> 
    nrow()
  firstbases <- info |> 
    filter(batter == lead(first_baserunner) & game_play_per_game %in% valid_plays$game_play_per_game) |> 
    nrow()
  firstbase_info <- info |> 
    filter(batter == lead(first_baserunner)) |> 
             filter(game_play_per_game %in% valid_plays$game_play_per_game)
  firstbase_playpergame <- firstbase_info$game_play_per_game
  single_events <- events |> 
    filter(game_play_per_game %in% firstbase_playpergame) |> 
    filter(event_code == 4)
  singles = nrow(single_events)
  walks <- firstbases - singles
  doubles <- nrow(info |> 
                    filter(batter == lead(second_baserunner)) |> 
                    filter(game_play_per_game %in% valid_plays$game_play_per_game))
  triples <- nrow(info |> 
                    filter(batter == lead(third_baserunner)) |> 
                    filter(game_play_per_game %in% valid_plays$game_play_per_game))
  walk_pct <- walks / at_bats
  single_pct <- singles / at_bats
  double_pct <- doubles / at_bats
  triple_pct <- triples / at_bats
  hr_pct <- home_runs / at_bats
  on_base_pct <- (singles + walks + doubles + triples + home_runs) / at_bats
  slug_pct <- (singles + walks + 2 * doubles + 3 * triples + 4 * home_runs) / at_bats
  ops <- on_base_pct + slug_pct
  wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples)
          + (2.101 * home_runs)) / at_bats
  return(tibble(number, at_bats, walks, singles, doubles, triples,
                home_runs, walk_pct, single_pct, double_pct, triple_pct, hr_pct,
                on_base_pct, slug_pct, ops, wOBA))
}
stats_by_inning_at_bat <- tibble()
for (i in seq(1, 10, by = 1)){
  stats <- stats_by_inning_at_bat_count(i)
  stats_by_inning_at_bat <- bind_rows(stats_by_inning_at_bat, stats)
}

write.csv(stats_by_inning_at_bat, "batting_stats_by_inning_at_bat.csv", 
          row.names = FALSE)

# double plays ------------------------------------------------------------
potential_double_info <- all_game_info |> 
  filter(!is.na(first_baserunner) & !is.na(batter) & lead(at_bat) != at_bat) |> 
  mutate(game_str_play_per_game = paste(game_str, play_per_game))

potential_double_info |> 
  mutate(game_str_at_bat = paste(game_str, at_bat)) |> 
  pull(game_str_at_bat) |> 
  unique() |> 
  length()

potential_double_events <- all_game_events |> 
  mutate(game_str_play_per_game = paste(game_str, play_per_game)) |> 
  semi_join(potential_double_info, by = join_by(game_str_play_per_game))

first_bounces_potential <- potential_double_events |> 
  filter(event_code == 16) |> 
  group_by(game_str_play_per_game) |> 
  summarise(game_str = first(game_str),
            timestamp = first(timestamp))

home_runs <- all_game_events |> 
  filter(event_code == 11) |> 
  mutate(game_str_play_id = paste(game_str, play_id))

actual_double_info <- all_game_info |> 
  mutate(game_str_play_per_game = paste(game_str, play_per_game)) |> 
  filter(!is.na(first_baserunner) & is.na(lead(first_baserunner)) & 
           is.na(lead(second_baserunner)) & 
           is.na(lead(third_baserunner)) & lead(at_bat) != at_bat) |> 
  filter(!(game_str_play_per_game %in% home_runs$game_str_play_per_game))

actual_double_info |> 
  mutate(game_str_at_bat = paste(game_str, at_bat)) |> 
  pull(game_str_at_bat) |> 
  unique() |> 
  length()

actual_double_events <- all_game_events |> 
  mutate(game_str_play_per_game = paste(game_str, play_per_game)) |> 
  semi_join(actual_double_info, by = join_by(game_str_play_per_game))

first_bounces_double <- actual_double_events |> 
  filter(event_code == 16) |> 
  group_by(game_str_play_per_game) |> 
  summarise(game_str = first(game_str),
            timestamp = first(timestamp))

first_bounces_double$is_double <- 1

first_bounces <- first_bounces_potential |> 
  left_join(first_bounces_double)

first_bounces$is_double <- first_bounces$is_double |> 
  replace_na(0)

sum(first_bounces$is_double)

first_bounce_locations <- all_ball_pos |> 
  semi_join(first_bounces, by = join_by(game_str, timestamp)) |> 
  mutate(game_str_play_per_game = paste(game_str, play_id)) |> 
  left_join(first_bounces |> select(game_str, timestamp, is_double))

first_bounce_locations |> 
  ggplot(aes(ball_position_x, ball_position_y, color = as.factor(is_double))) +
  geom_point()

infield_bounces <- first_bounce_locations |> 
  filter(ball_position_y < 128)

infield_bounces$is_double |> sum()

outfield_bounces <- first_bounce_locations |> 
  filter(ball_position_y > 128)

outfield_bounces$is_double |> sum()

player_speed <- read_csv("percentile_975_speed_per_player_all.csv")

good_bounce_speed <- good_bounce_info |> 
  left_join(player_speed |> select(-miles_per_hour), by = join_by(first_baserunner == player_id))

mean(good_bounce_speed$feet_per_second)
mean(player_speed$feet_per_second)

nrow(actual_double_info) / nrow(potential_double_info)


# strikeouts vs ball in play outs -----------------------------------------
out_info <- all_game_info |> 
  filter(!is.na(batter) & inning != 0) |> 
  filter(at_bat != lead(at_bat)) |> 
  replace_na(list(first_baserunner = 0, second_baserunner = 0, third_baserunner = 0)) |> 
  filter((batter != lead(first_baserunner)) & (batter != lead(second_baserunner)) &
           (batter != lead(third_baserunner))) |> 
  mutate(first_baserunner = na_if(first_baserunner, 0),
         second_baserunner = na_if(second_baserunner, 0),
         third_baserunner = na_if(third_baserunner, 0)) |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  filter(!(game_str_play_id %in% home_runs$game_str_play_id))

out_events <- all_game_events |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  semi_join(out_info, by = join_by(game_str_play_id))

length(unique(out_events$game_str_play_id)) #13492 total outs

not_strikeouts <- out_events |>
  filter(event_code == 4)
length(unique(not_strikeouts$game_str_play_id)) 
#on 8804 outs the ball was hit into play

13492 - 8804 #4688 outs are strikeouts

4688/13492 #34.75% of outs are strikeouts

strikeout_info <- out_info |> 
  anti_join(not_strikeouts, by = join_by(game_str_play_id))

strikeout_info$is_strikeout <- 1

ball_in_play_events <- out_events |> 
  anti_join(strikeout_info, by = join_by(game_str_play_id))

ground_out_bounces <- ball_in_play_events |> 
  filter(event_code == 16)

ground_out_bounce_info <- all_game_info |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  semi_join(ground_out_bounces, by = join_by(game_str_play_id))

ground_out_bounce_info$is_ground_out <- 1

out_info <- left_join(out_info, strikeout_info)
out_info <- left_join(out_info, ground_out_bounce_info)
out_info$is_fly_out <- ifelse(is.na(out_info$is_strikeout) & is.na(out_info$is_ground_out), 1, NA)

batter_out_pct <- out_info |> 
  group_by(batter) |> 
  summarise(outs = n(), ground_outs = sum(is_ground_out, na.rm = TRUE), 
            fly_outs = sum(is_fly_out, na.rm = TRUE),
            strikeouts = sum(is_strikeout, na.rm = TRUE), ground_out_pct = ground_outs / outs,
            fly_out_pct = fly_outs / outs, strikeouts = strikeouts / outs)

write.csv(batter_out_pct, "batter_out_pct.csv", row.names = FALSE)

ground_out_info_with_first <- ground_out_bounce_info |> 
  filter(!is.na(first_baserunner))

actual_double_info$is_double_play <- 1

all_pot_double_plays <- left_join(ground_out_info_with_first, actual_double_info)

all_pot_double_plays$is_double_play <- all_pot_double_plays$is_double_play |> replace_na(0)

bounces_of_interest <- ground_out_bounces |> 
  group_by(game_str_play_per_game) |> 
  summarise(game_str = first(game_str),
            timestamp = first(timestamp))

bounces_of_interest_locs <- all_ball_pos |> 
  semi_join(bounces_of_interest, by = join_by(game_str, timestamp)) |> 
  left_join(all_pot_double_plays |> select(game_str_play_per_game, is_double_play)) |> 
  filter(!is.na(is_double_play))

bounces_of_interest_locs |> 
  ggplot(aes(ball_position_x, ball_position_y, color = as.factor(is_double_play))) +
  geom_point()

length(unique(ground_out_bounces$game_str_play_per_game)) #4439 outs are ground outs
4439 / 13492 #32.9% of outs are ground outs

13492 - 4688 - 4439 #4365 outs are fly outs
4365 / 13492 #32.35% of outs are fly outs

out_info$is_out <- 1

all_game_info <- all_game_info |> 
  left_join(out_info)

all_game_info$is_out <- all_game_info$is_out |> replace_na(0)

all_game_info <- all_game_info |> 
  group_by(game_str, inning, top_bottom) |> 
  mutate(out_count = cumsum(is_out)) |> 
  select(-game_str_play_per_game) |> 
  ungroup()

all_game_info |> 
  group_by(game_str, inning, top_bottom) |> 
  summarise(outs = max(out_count)) |> 
  group_by(outs) |> 
  summarise(innings = n())
# potential sac flies -------------------------------------------------
bounces <- all_game_events |> 
  filter(event_code == 16) |> 
  mutate(game_play_id = paste(game_str, play_id))

off_the_wall <- all_game_events |> 
  filter(event_code == 10) |> 
  mutate(game_play_id = paste(game_str, play_id))

sac_fly_info <- all_game_info |> 
  mutate(game_play_id = paste(game_str, play_id)) |> 
  filter(!is.na(batter) & !is.na(third_baserunner) & batter != lead(batter) &
           !(game_play_id %in% strikeout_info$game_str_play_id) & 
           !(game_play_id %in% ground_out_bounces$game_str_play_id) &
           !(game_play_id %in% bounces$game_play_id) &
           !(game_play_id %in% home_runs$game_str_play_id) &
           !(game_play_id %in% off_the_wall$game_play_id))

stayed_on_third <- all_game_info |> 
  mutate(game_play_id = paste(game_str, play_id)) |> 
  filter(third_baserunner == lead(third_baserunner) & game_play_id %in% sac_fly_info$game_play_id)

plays_of_interest <- sac_fly_info |> 
  filter(!(game_play_id %in% stayed_on_third$game_play_id))

events_of_interest <- all_game_events |> 
  mutate(game_play_id = paste(game_str, play_id)) |> 
  semi_join(plays_of_interest, by = "game_play_id")

throws <- events_of_interest |> 
  filter(event_code == 3)

attempt_sacs <- all_game_info |> 
  mutate(game_play_id = paste(game_str, play_id)) |> 
  semi_join(throws, by = "game_play_id")

attempt_sac_events <- all_game_events |> 
  mutate(game_play_id = paste(game_str, play_id)) |> 
  semi_join(throws, by = "game_play_id")

catcher_ball <- attempt_sac_events |> 
  filter(player_position == 2 & event_code == 2)

attempt_sacs <- attempt_sacs |> 
  mutate(scored = ifelse(game_play_id %in% catcher_ball$game_play_id, 0, 1))

sac_attack <- bind_rows(stayed_on_third, attempt_sacs)

sac_attack <- sac_attack |> 
  mutate(outcome = case_when(
    is.na(scored) ~ "stayed",
    scored == 1 ~ "scored",
    scored == 0 ~ "out"
  )) |> 
  select(-scored)

speeds <- read.csv("percentile_975_speed_per_player_all.csv")
team_players <- read.csv("batting_team_data.csv")
speeds <- speeds |> 
  semi_join(team_players |> select(batter), by = c("player_id" = "batter"))
speeds <- speeds |> 
  mutate(percentile = rank(miles_per_hour) / length(miles_per_hour) * 100)


sac_attack <- sac_attack |> left_join(speeds |> select(-feet_per_second, -miles_per_hour), 
                                                              by = join_by("third_baserunner" == "player_id"))


sac_attack |> filter(outcome == "scored") |> pull(percentile) |> mean(na.rm = TRUE)
sac_attack |> filter(outcome == "out") |> pull(percentile) |> mean(na.rm = TRUE)
sac_attack |> filter(outcome == "stayed") |> pull(percentile) |> mean(na.rm = TRUE)

sac_attack |> filter(outcome == "scored") |> pull(percentile) |> median(na.rm = TRUE)
sac_attack |> filter(outcome == "out") |> pull(percentile) |> median(na.rm = TRUE)
sac_attack |> filter(outcome == "stayed") |> pull(percentile) |> median(na.rm = TRUE)

sac_catches <- all_game_events |> 
  mutate(game_play_id = paste(game_str, play_id)) |> 
  semi_join(attempt_sacs, by = "game_play_id") |> 
  filter(event_code == 2) |> 
  group_by(game_play_id) |> 
  slice_head(n = 1) |> 
  ungroup()
  
catches_ball_pos <- ball_pos |> 
  mutate(game_play_id = paste(game_str, play_id),
         timestamp = as.integer(timestamp)) |> 
  semi_join(sac_catches, by = c("game_play_id", "timestamp")) |> 
  select(game_play_id, ball_position_x, ball_position_y) |> 
  collect()

attempt_sacs <- attempt_sacs |> 
  left_join(catches_ball_pos, by = "game_play_id")

attempt_sacs <- attempt_sacs |> left_join(speeds |> select(-feet_per_second, -miles_per_hour), 
                                      by = join_by("third_baserunner" == "player_id"))

attempt_sacs |> 
  ggplot(aes(ball_position_x, ball_position_y, color = as.factor(scored))) +
  geom_point()

left_field_sac <- attempt_sacs |> 
  filter(ball_position_x < -31.5 & ball_position_y > 128)

right_field_sac <- attempt_sacs |> 
  filter(ball_position_x > 31.5 & ball_position_y > 128)

center_field_sac <- attempt_sacs |> 
  filter(31.5 > ball_position_x & ball_position_x > -31.5 & ball_position_y > 128)

sum(left_field_sac$scored, na.rm = TRUE) / length(left_field_sac$scored)
sum(right_field_sac$scored, na.rm = TRUE) / length(right_field_sac$scored)
sum(center_field_sac$scored, na.rm = TRUE) / length(center_field_sac$scored)

# batting stats by level of play ------------------------------------------
stats_by_level <- function(level){
  info <- all_game_info |> 
    filter(HomeTeam == level) |> 
    mutate(game_play_per_game = paste(game_str, play_per_game))
  events <- all_game_events |> 
    filter(HomeTeam == level) |> 
    mutate(game_play_per_game = paste(game_str, play_per_game))
  home_runs <- as.numeric(nrow(events |> 
                                 filter(event_code == 11)))
  at_bats <- info |> 
    distinct(game_str, at_bat) |> 
    nrow()
  firstbases <- info |> 
    filter(batter == lead(first_baserunner)) |> 
    nrow()
  firstbase_info <- info |> 
    filter(batter == lead(first_baserunner))
  firstbase_playpergame <- firstbase_info$game_play_per_game
  single_events <- events |> 
    filter(game_play_per_game %in% firstbase_playpergame) |> 
    filter(event_code == 4)
  singles = nrow(single_events)
  walks <- firstbases - singles
  doubles <- nrow(info |> 
                    filter(batter == lead(second_baserunner)))
  triples <- nrow(info |> 
                    filter(batter == lead(third_baserunner)))
  walk_pct <- walks / at_bats
  single_pct <- singles / at_bats
  double_pct <- doubles / at_bats
  triple_pct <- triples / at_bats
  hr_pct <- home_runs / at_bats
  on_base_pct <- (singles + walks + doubles + triples + home_runs) / at_bats
  slug_pct <- (singles + walks + 2 * doubles + 3 * triples + 4 * home_runs) / at_bats
  ops <- on_base_pct + slug_pct
  wOBA = ((.691*walks) + (.89 * singles) + (1.271 * doubles) + (1.616 * triples)
          + (2.101 * home_runs)) / at_bats
  return(tibble(level, at_bats, walks, singles, doubles, triples,
                home_runs, walk_pct, single_pct, double_pct, triple_pct, hr_pct,
                on_base_pct, slug_pct, ops, wOBA))
}
level_stats <- bind_rows(stats_by_level("Home1A"), stats_by_level("Home2A"), 
          stats_by_level("Home3A"), stats_by_level("Home4A"))
write.csv(level_stats, "stats_by_level_of_play.csv", row.names = FALSE)

# scoring from 1st base on a double ---------------------------------------
first_base_double_info <- all_game_info |> 
  filter(!is.na(batter) & !is.na(first_baserunner) & lead(second_baserunner) == batter) |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  mutate(valid_play = 1)

double_info <- all_game_info |> left_join(first_base_double_info)
double_info$valid_play <- double_info$valid_play |> replace_na(0)

plays_of_interest <- double_info |> 
  filter(is.na(lead(third_baserunner)) & valid_play == 1)

interest_player_pos <- player_pos |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  filter(game_str_play_id %in% plays_of_interest$game_str_play_id) |> 
  filter(player_position == 11) |> 
  collect()

interest_player_pos <- interest_player_pos |> 
  mutate(distance_from_home = sqrt(field_x^2 + (field_y-0.71)^2))
length(unique(plays_of_interest$game_str_play_id))

length(unique(interest_player_pos$game_str_play_id))
close_to_home <- interest_player_pos |> 
  group_by(game_str_play_id) |> 
  summarise(closest_dist = min(distance_from_home))

close_to_home$made_the_run <- ifelse(close_to_home$closest_dist < 85, 1, 0)

interest_ball_pos <- ball_pos |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  filter(game_str_play_id %in% plays_of_interest$game_str_play_id) |> 
  collect()

interest_ball_in_play_timestamps <- all_game_events |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  filter(game_str_play_id %in% interest_ball_pos$game_str_play_id) |> 
  filter(event_code == 4) |> 
  select(game_str_play_id, hit_timestamp = timestamp)

interest_ball_pos <- interest_ball_pos |> left_join(interest_ball_in_play_timestamps) |> 
  filter(timestamp > hit_timestamp + 1000)

interest_ball_pos <- interest_ball_pos |> 
  mutate(ball_from_home = sqrt(ball_position_x^2 + (ball_position_y-0.71)^2))

ball_from_home <- interest_ball_pos |> 
  group_by(game_str_play_id) |> 
  summarise(closest_ball = min(ball_from_home))

ball_and_play <- left_join(ball_from_home, close_to_home)

ball_and_play <- ball_and_play |> 
  mutate(scored = ifelse(closest_ball > 5 & made_the_run == 1, 1, 0))

sum(ball_and_play$scored)

first_base_double_info <- first_base_double_info |> 
  left_join(ball_and_play |> select(game_str_play_id, scored))

first_base_first_bounces <- all_game_events |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  filter(game_str_play_id %in% first_base_double_info$game_str_play_id) |> 
  filter(event_code == 16) |> 
  group_by(game_str_play_id) |> 
  summarise(timestamp = first(timestamp)) |> 
  mutate(timestamp = as.integer(timestamp)) |> 
  ungroup()

first_base_bounce_locs <- ball_pos |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  mutate(timestamp = as.integer(timestamp)) |> 
  inner_join(first_base_first_bounces) |> 
  select(game_str_play_id, ball_position_x, ball_position_y) |> collect()

first_base_double_info <- first_base_double_info |> 
  left_join(first_base_bounce_locs) |> 
  filter(!is.na(ball_position_x))

first_base_double_info |> 
  ggplot(aes(ball_position_x, ball_position_y, color = as.factor(scored))) +
  geom_point()

left_field_double <- first_base_double_info |> 
  filter(ball_position_x < -31.5 & ball_position_y > 128)

right_field_double <- first_base_double_info |> 
  filter(ball_position_x > 31.5 & ball_position_y > 128)

center_field_double <- first_base_double_info |> 
  filter(31.5 > ball_position_x & ball_position_x > -31.5 & ball_position_y > 128)

sum(left_field_double$scored, na.rm = TRUE) / length(left_field_double$scored)
sum(right_field_double$scored, na.rm = TRUE) / length(right_field_double$scored)
sum(center_field_double$scored, na.rm = TRUE) / length(center_field_double$scored)

speeds <- read.csv("percentile_975_speed_per_player_all.csv")
team_players <- read.csv("batting_team_data.csv")
speeds <- speeds |> 
  semi_join(team_players |> select(batter), by = c("player_id" = "batter"))
speeds <- speeds |> 
  mutate(percentile = rank(miles_per_hour) / length(miles_per_hour) * 100)


first_base_double_info <- first_base_double_info |> left_join(speeds |> select(-feet_per_second, -miles_per_hour), 
                                    by = join_by("first_baserunner" == "player_id"))

first_base_double_info |> filter(scored == 1) |> pull(percentile) |> mean(na.rm = TRUE)
first_base_double_info |> filter(scored == 0) |> pull(percentile) |> mean(na.rm = TRUE)
first_base_double_info |> filter(is.na(scored)) |> pull(percentile) |> mean(na.rm = TRUE)

first_base_double_info |> filter(scored == 1) |> pull(percentile) |> median(na.rm = TRUE)
first_base_double_info |> filter(scored == 0) |> pull(percentile) |> median(na.rm = TRUE)
first_base_double_info |> filter(is.na(scored)) |> pull(percentile) |> median(na.rm = TRUE)

left_field_double$field_pos <- "left"
right_field_double$field_pos <- "right"
center_field_double$field_pos <- "center"
first_model_df <- bind_rows(left_field_double, right_field_double, center_field_double) |> 
  left_join(first_base_double_info |> select(game_str_play_id, percentile)) |> 
  select(field_pos, percentile, scored) |> 
  filter(!is.na(percentile)) |> 
  mutate(outcome = case_when(
    is.na(scored) ~ "stay",
    scored == 0 ~ "failed",
    scored == 1 ~ "scored"
  )) |> 
  select(-scored)

# library(nnet)
# first_model <- multinom(outcome ~ percentile, data = first_model_df)
# summary(first_model)
# first_preds <- predict(first_model, type = "probs")

first_plot_data <- first_model_df |> 
  mutate(percentile_group = case_when(
    percentile < 33.33 ~ "slow",
    percentile < 66.67 ~ "average",
    percentile <= 100 ~ "fast"
  )) |> 
  group_by(percentile_group) |> 
  summarise(plays = n(), stay_pct = sum(outcome == "stay") / plays, 
            score_pct = sum(outcome == "scored") / plays,
            fail_pct = sum(outcome == "failed") / plays)

first_plot_data |> 
  pivot_longer(cols = c(stay_pct, score_pct, fail_pct), names_to = "outcome", 
               values_to = "value") |> 
  mutate(percentile_group = factor(percentile_group, levels = c("slow", "average", "fast")),
         outcome = factor(outcome, levels = c("fail_pct", "stay_pct", "score_pct"))) |> 
  ggplot(aes(x = percentile_group, y = value * 100, fill = outcome)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_label(aes(label = paste(round(value, 3) * 100, "%"), group = outcome), fill = "white",
             position = position_dodge(width = 0.9)) +
  ggthemes::theme_clean() +
  scale_fill_manual(values = c("score_pct" = "#008450", "fail_pct" = "#b81d13", 
                               "stay_pct" = "#EFB700"),
                    labels = c("score_pct" = "Scored", "fail_pct" = "Out at Home",
                               "stay_pct" = "Stayed on Third")) +
  labs(title = "First Baserunner Play Outcomes on a Double",
       x = "Baserunner Speed", y = "Percent of Plays", fill = "Play Outcome",
       caption = "Slow: 33rd percentile speed or worse | Average: 33rd-66th percentile speed | Fast: 66th percentile speed or better") +
  scale_x_discrete(labels = c("slow" = "Slow", "average" = "Average", "fast" = "Fast")) +
  theme(axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"))

first_base_double_info |> 
  filter(!is.na(percentile)) |> 
  mutate(percentile_group = case_when(
    percentile < 33.33 ~ "slow",
    percentile < 66.67 ~ "average",
    percentile <= 100 ~ "fast"
  )) |> 
  group_by(percentile_group) |> 
  summarise(plays = n(), stay_pct = sum(is.na(scored)) / plays, 
            score_pct = sum(scored == 1, na.rm = TRUE) / plays,
            fail_pct = sum(scored == 0, na.rm = TRUE) / plays)

# scoring from 2nd base on a single ---------------------------------------
firstbase_info <- all_game_info |> 
  mutate(game_play_id = paste(game_str, play_id)) |> 
  filter(batter == lead(first_baserunner))
single_events <- all_game_events |> 
  mutate(game_play_id = paste(game_str, play_id)) |> 
  filter(game_play_id %in% firstbase_info$game_play_id) |> 
  filter(event_code == 4)

second_base_single_info <- all_game_info |> 
  mutate(game_play_id = paste(game_str, play_id)) |> 
  filter(!is.na(batter) & !is.na(second_baserunner) & lead(first_baserunner) == batter &
           game_play_id %in% single_events$game_play_id) |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  mutate(valid_play = 1)

single_info <- all_game_info |> left_join(second_base_single_info)
single_info$valid_play <- single_info$valid_play |> replace_na(0)

plays_of_interest <- single_info |> 
  filter(is.na(lead(third_baserunner)) & valid_play == 1)

interest_player_pos <- player_pos |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  filter(game_str_play_id %in% plays_of_interest$game_str_play_id) |> 
  filter(player_position == 12) |> 
  collect()

interest_player_pos <- interest_player_pos |> 
  mutate(distance_from_home = sqrt(field_x^2 + (field_y-0.71)^2))
length(unique(plays_of_interest$game_str_play_id))

length(unique(interest_player_pos$game_str_play_id))
close_to_home <- interest_player_pos |> 
  group_by(game_str_play_id) |> 
  summarise(closest_dist = min(distance_from_home))

close_to_home$made_the_run <- ifelse(close_to_home$closest_dist < 70, 1, 0)

interest_ball_pos <- ball_pos |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  filter(game_str_play_id %in% plays_of_interest$game_str_play_id) |> 
  collect()

interest_ball_in_play_timestamps <- all_game_events |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  filter(game_str_play_id %in% interest_ball_pos$game_str_play_id) |> 
  filter(event_code == 4) |> 
  select(game_str_play_id, hit_timestamp = timestamp)

interest_ball_pos <- interest_ball_pos |> left_join(interest_ball_in_play_timestamps) |> 
  filter(timestamp > hit_timestamp + 1000)

interest_ball_pos <- interest_ball_pos |> 
  mutate(ball_from_home = sqrt(ball_position_x^2 + (ball_position_y-0.71)^2))

ball_from_home <- interest_ball_pos |> 
  group_by(game_str_play_id) |> 
  summarise(closest_ball = min(ball_from_home))

ball_and_play <- left_join(ball_from_home, close_to_home)

ball_and_play <- ball_and_play |> 
  mutate(scored = ifelse(closest_ball > 6 & made_the_run == 1, 1, 0))

sum(ball_and_play$scored, na.rm = TRUE)

second_base_single_info <- second_base_single_info |> 
  left_join(ball_and_play |> select(game_str_play_id, scored))

second_base_first_bounces <- all_game_events |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  filter(game_str_play_id %in% second_base_single_info$game_str_play_id) |> 
  filter(event_code == 16) |> 
  group_by(game_str_play_id) |> 
  summarise(timestamp = first(timestamp)) |> 
  mutate(timestamp = as.integer(timestamp)) |> 
  ungroup()

second_base_bounce_locs <- ball_pos |> 
  mutate(game_str_play_id = paste(game_str, play_id)) |> 
  mutate(timestamp = as.integer(timestamp)) |> 
  inner_join(second_base_first_bounces) |> 
  select(game_str_play_id, ball_position_x, ball_position_y) |> collect()

second_base_single_info <- second_base_single_info |> 
  left_join(second_base_bounce_locs) |> 
  filter(!is.na(ball_position_x))

second_base_single_info |> 
  ggplot(aes(ball_position_x, ball_position_y, color = as.factor(scored))) +
  geom_point()

left_field_single <- second_base_single_info |> 
  filter(ball_position_x < -31.5 & ball_position_y > 128)

right_field_single <- second_base_single_info |> 
  filter(ball_position_x > 31.5 & ball_position_y > 128)

center_field_single <- second_base_single_info |> 
  filter(31.5 > ball_position_x & ball_position_x > -31.5 & ball_position_y > 128)

sum(left_field_single$scored, na.rm = TRUE) / length(left_field_single$scored)
sum(right_field_single$scored, na.rm = TRUE) / length(right_field_single$scored)
sum(center_field_single$scored, na.rm = TRUE) / length(center_field_single$scored)

second_base_single_info <- second_base_single_info |> left_join(speeds |> select(-feet_per_second, -miles_per_hour), 
                                                              by = join_by("second_baserunner" == "player_id"))

second_base_single_info |> filter(scored == 1) |> pull(percentile) |> mean(na.rm = TRUE)
second_base_single_info |> filter(scored == 0) |> pull(percentile) |> mean(na.rm = TRUE)
second_base_single_info |> filter(is.na(scored)) |> pull(percentile) |> mean(na.rm = TRUE)

second_base_single_info |> filter(scored == 1) |> pull(percentile) |> median(na.rm = TRUE)
second_base_single_info |> filter(scored == 0) |> pull(percentile) |> median(na.rm = TRUE)
second_base_single_info |> filter(is.na(scored)) |> pull(percentile) |> median(na.rm = TRUE)

left_field_single$field_pos <- "left"
right_field_single$field_pos <- "right"
center_field_single$field_pos <- "center"
second_model_df <- bind_rows(left_field_single, right_field_single, center_field_single) |> 
  left_join(second_base_single_info |> select(game_str_play_id, percentile)) |> 
  select(field_pos, percentile, scored) |> 
  filter(!is.na(percentile)) |> 
  mutate(outcome = case_when(
    is.na(scored) ~ "stay",
    scored == 0 ~ "failed",
    scored == 1 ~ "scored"
  )) |> 
  select(-scored)

# library(nnet)
# second_model <- multinom(outcome ~ percentile + field_pos, data = second_model_df)
# summary(second_model)
# second_model_df$pred_both <- predict(second_model, type = "class")
# sum(second_model_df$outcome == second_model_df$pred_both) / length(second_model_df$outcome)

second_base_single_info |> 
  filter(!is.na(percentile)) |> 
  mutate(percentile_group = case_when(
    percentile < 33.33 ~ "slow",
    percentile < 66.67 ~ "average",
    percentile <= 100 ~ "fast"
  )) |> 
  group_by(percentile_group) |> 
  summarise(plays = n(), stay_pct = sum(is.na(scored)) / plays, 
            score_pct = sum(scored == 1, na.rm = TRUE) / plays,
            fail_pct = sum(scored == 0, na.rm = TRUE) / plays)

second_plot_data <- second_base_single_info |> 
  select(percentile, scored) |> 
  filter(!is.na(percentile)) |> 
  mutate(outcome = case_when(
    is.na(scored) ~ "stay",
    scored == 0 ~ "failed",
    scored == 1 ~ "scored"
  )) |> 
  select(-scored) |> 
  mutate(percentile_group = case_when(
    percentile < 33.33 ~ "slow",
    percentile < 66.67 ~ "average",
    percentile <= 100 ~ "fast"
  )) |> 
  group_by(percentile_group) |> 
  summarise(plays = n(), stay_pct = sum(outcome == "stay") / plays, 
            score_pct = sum(outcome == "scored") / plays,
            fail_pct = sum(outcome == "failed") / plays)

second_plot_data |> 
  pivot_longer(cols = c(stay_pct, score_pct, fail_pct), names_to = "outcome", 
               values_to = "value") |> 
  mutate(percentile_group = factor(percentile_group, levels = c("slow", "average", "fast")),
         outcome = factor(outcome, levels = c("fail_pct", "stay_pct", "score_pct"))) |> 
  ggplot(aes(x = percentile_group, y = value * 100, fill = outcome)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_label(aes(label = paste(round(value, 3) * 100, "%"), group = outcome), fill = "white",
             position = position_dodge(width = 0.9)) +
  ggthemes::theme_clean() +
  scale_fill_manual(values = c("score_pct" = "#008450", "fail_pct" = "#b81d13", 
                               "stay_pct" = "#EFB700"),
                    labels = c("score_pct" = "Scored", "fail_pct" = "Out at Home",
                               "stay_pct" = "Stayed on Third")) +
  labs(title = "Second Baserunner Play Outcomes on a Single",
       x = "Baserunner Speed", y = "Percent of Plays", fill = "Play Outcome",
       caption = "Slow: 33rd percentile speed or worse | Average: 33rd-66th percentile speed | Fast: 66th percentile speed or better") +
  scale_x_discrete(labels = c("slow" = "Slow", "average" = "Average", "fast" = "Fast")) +
  theme(axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"))



# lineup stuff ------------------------------------------------------------
stats_per_player_1A <- read_csv("SMT-Data-Challenge-24/stats_per_player_1A.csv")
stats1Afiltered <- stats_per_player_1A |> filter(at_bats > 100)

lineup_counts <- read_csv("SMT-Data-Challenge-24/lineup_counts.csv")
tidy_lineups <- lineup_counts |> 
  separate(lineup, into = paste0("batter", 1:9), sep = "-", convert = TRUE)
tidy_lineups |> 
  filter(batting_team == "Home1A") |> 
  group_by(batter1) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home1A") |> 
  group_by(batter2) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home1A") |> 
  group_by(batter3) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home1A") |> 
  group_by(batter4) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home1A") |> 
  group_by(batter5) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home1A") |> 
  group_by(batter1, batter2, batter3, batter4, batter5) |> 
  summarise(games = sum(count)) |> 
  arrange(desc(games))

five_man <- c(753, 626, 630, 892, 598)
tidy_lineups |> 
  filter(batting_team == "Home1A") |> 
  filter(batter1 %in% five_man & batter2 %in% five_man & batter3 %in% five_man 
         & batter4 %in% five_man & batter5 %in% five_man) |> 
  group_by(batter1, batter2, batter3, batter4, batter5) |> 
  summarise(games = sum(count)) |> 
  arrange(desc(games)) |> 
  ungroup()

stats_per_player_2A <- read_csv("SMT-Data-Challenge-24/stats_per_player_2A.csv")
stats2Afiltered <- stats_per_player_2A |> filter(at_bats > 100)

tidy_lineups |> 
  filter(batting_team == "Home2A") |> 
  group_by(batter1) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home2A") |> 
  group_by(batter2) |> 
  summarise(games = sum(count)) |> 
  arrange(desc(games))

tidy_lineups |> 
  filter(batting_team == "Home2A") |> 
  group_by(batter3) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home2A") |> 
  group_by(batter4) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home2A") |> 
  group_by(batter5) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home2A") |> 
  group_by(batter1, batter2, batter3, batter4, batter5) |> 
  summarise(games = sum(count)) |> 
  arrange(desc(games))

stats_per_player_3A <- read_csv("SMT-Data-Challenge-24/stats_per_player_3A.csv")
stats3Afiltered <- stats_per_player_3A |> filter(at_bats > 100)

tidy_lineups |> 
  filter(batting_team == "Home3A") |> 
  group_by(batter1) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home3A") |> 
  group_by(batter2) |> 
  summarise(games = sum(count)) |> 
  arrange(desc(games))

tidy_lineups |> 
  filter(batting_team == "Home3A") |> 
  group_by(batter3) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home3A") |> 
  group_by(batter4) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home3A") |> 
  group_by(batter5) |> 
  summarise(games = sum(count))

tidy_lineups |> 
  filter(batting_team == "Home3A") |> 
  group_by(batter1, batter2, batter3, batter4, batter5) |> 
  summarise(games = sum(count)) |> 
  arrange(desc(games))
