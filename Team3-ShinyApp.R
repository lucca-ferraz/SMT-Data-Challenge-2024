
library(shiny)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(shinyWidgets)
library(gtExtras)
library(RColorBrewer)
library(grDevices)
library(paletteer)
library(gtExtras)

At_Bat_Outcome_Simulator <- function(BaseRunners = rep(0, 3), BaseSpeeds = rep(0,3) ,walk_percentage, single_percentage, double_percentage, triple_percentage, homerun_percentage, left_field, center_field, right_field, right, strikeouts, fly_out_pct, stolen_base_percentage, caught_stolen_base_percentage, stolen_base_percentage_2_to_3, caught_stolen_base_percentage_2_to_3, speed_percentile, Outs = 0) {
  # This first line generates the probability used for the PA.
  Plate_Appearance <- runif(1)
  #Testing for stolen bases from 1st to 2nd
  if (BaseRunners[1] == 1 & BaseRunners[2] == 0) {
    stolen_base_runif <- runif(1)
    if (stolen_base_runif < stolen_base_percentage){
      BaseRunners[1] <- 0
      BaseRunners[2] <- 1
      BaseSpeeds[1] <- 0
      BaseSpeeds[2] <- BaseSpeeds[1]
    }
    else if (stolen_base_runif < stolen_base_percentage + caught_stolen_base_percentage){
      BaseRunners[1] <-  0
      BaseRunners[2] <-  0
      BaseSpeeds[1] <- 0
      BaseSpeeds[2] <- 0
    }
  }
  #Testing for stolen bases from 2nd to 3rd
  if (BaseRunners[2] == 1 & BaseRunners[3] == 0) {
    stolen_base_runif <- runif(1)
    if (stolen_base_runif < stolen_base_percentage_2_to_3){
      BaseRunners[2] <- 0
      BaseRunners[3] <- 1
      BaseSpeeds[2] <- 0
      BaseSpeeds[3] <- BaseSpeeds[2]
    }
    else if (stolen_base_runif < stolen_base_percentage_2_to_3 + caught_stolen_base_percentage_2_to_3){
      BaseRunners[2] <-  0 
      BaseRunners[3] <- 0
      BaseSpeeds[2] <- 0
      BaseSpeeds[3] <- 0
    }
  }
  #Getting Outcome of Every At Bat
  if (Plate_Appearance > walk_percentage + single_percentage + double_percentage + triple_percentage + homerun_percentage) {
    out_runif <- runif(1)
    #First checks the type of out
    #Testing strikeouts first
    if (out_runif <= strikeouts) {
      Outcome <- "Out"
      Outs <- Outs + 1
      Runs <- 0
      #Then testing fly outs
    } else if (out_runif <= strikeouts + fly_out_pct) {
      Outcome <- "Out"
      Outs <- Outs + 1
      if (BaseRunners[3]==1){
        directional_sac_fly_runif <- runif(1)
        sac_fly_runif <- runif(1)
        if (directional_sac_fly_runif<center_field){
          if(sac_fly_runif<=.8181){
            Runs <- 1
          }
          else{
            Runs <- 0
          }
        }
        else{
          if(sac_fly_runif<=.9){
            Runs <- 1
          }
          else{
            Runs <- 0
          }
        }
      }
      else{
        Runs <- 0
      }
    } else {
      #Finally testing ground outs and if it results in a double play if a runner is on 1st base
      #42% of groundouts are double plays according to the data (no real indicator of direction of hit)
      if (BaseRunners[1] == 1) {
        double_play_runif <- runif(1)
        if (double_play_runif <= .42) {
          BaseRunners[1] <- 0
          BaseSpeeds[1] <- 0
          if (Outs == 2){
            Outcome <- "Out"
            Outs <- Outs + 1
          }
          else{
            Outcome <- "Out - Double Play"
            Outs <- Outs + 2
          }
          Runs <- 0
        } else {
          Outcome <- "Out"
          Outs <- Outs + 1
          Runs <- 0
        }
      } else {
        Outcome <- "Out"
        Outs <- Outs + 1
        Runs <- 0
      }
    }
  } else {
    # Next check for a walk, if bases are loaded a run scores, if not the first empty base is filled
    if (Plate_Appearance <= walk_percentage) {
      if (sum(BaseRunners) == 3) {
        Outcome <- "Walk"
        Runs <- 1
        BaseSpeeds[3] <- BaseSpeeds[2]
        BaseSpeeds[2] <- BaseSpeeds[1]
        BaseSpeeds[1] <- speed_percentile
      } else {
        BaseRunners[which(BaseRunners == 0)[1]] <- 1
        Outcome <- "Walk"
        Runs <- 0
        BaseSpeeds[3] <- BaseSpeeds[2]
        BaseSpeeds[2] <- BaseSpeeds[1]
        BaseSpeeds[1] <- speed_percentile
      }
    } else {
      #Then checking for a single
      BB_SNGL <- walk_percentage + single_percentage
      if (Plate_Appearance > walk_percentage & Plate_Appearance <= BB_SNGL) {
        Outcome <- "Single"
        if (BaseRunners[2] == 1) {
          Score_from_Second <- runif(1)
          if (BaseSpeeds[2]<34){
            if (Score_from_Second <= .253) {
              #Runner scores
              Runs <- BaseRunners[3] + BaseRunners[2]
              BaseRunners[3] <- BaseRunners[1]
              BaseRunners[2] <- 0
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[1]
              BaseSpeeds[2] <- 0
              BaseSpeeds[1] <- speed_percentile
            } 
            else if (Score_from_Second<=.316){
              #Get out at home
              Runs <- BaseRunners[3]
              BaseRunners[3] <- BaseRunners[1]
              BaseRunners[2] <- 0
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[1]
              BaseSpeeds[2] <- 0
              BaseSpeeds[1] <- speed_percentile
              Outs = Outs + 1
            }
            else {
              #Stay on 3rd
              Runs <- BaseRunners[3]
              BaseRunners[3] <- BaseRunners[2]
              BaseRunners[2] <- BaseRunners[1]
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[2]
              BaseSpeeds[2] <- BaseSpeeds[1]
              BaseSpeeds[1] <- speed_percentile
            }
          }
          else if (BaseRunners[2]<67){
            if (Score_from_Second <= .31) {
              #Runner scores
              Runs <- BaseRunners[3] + BaseRunners[2]
              BaseRunners[3] <- BaseRunners[1]
              BaseRunners[2] <- 0
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[1]
              BaseSpeeds[2] <- 0
              BaseSpeeds[1] <- speed_percentile
            } 
            else if (Score_from_Second<=.452){
              #Gets out at home
              Runs <- BaseRunners[3]
              BaseRunners[3] <- BaseRunners[1]
              BaseRunners[2] <- 0
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[1]
              BaseSpeeds[2] <- 0
              BaseSpeeds[1] <- speed_percentile
              Outs = Outs + 1
            }
            else {
              #Stays on third
              Runs <- BaseRunners[3]
              BaseRunners[3] <- BaseRunners[2]
              BaseRunners[2] <- BaseRunners[1]
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[2]
              BaseSpeeds[2] <- BaseSpeeds[1]
              BaseSpeeds[1] <- speed_percentile
            }
          }
          else{
            if (Score_from_Second <= .324) {
              #Runner scores
              Runs <- BaseRunners[3] + BaseRunners[2]
              BaseRunners[3] <- BaseRunners[1]
              BaseRunners[2] <- 0
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[1]
              BaseSpeeds[2] <- 0
              BaseSpeeds[1] <- speed_percentile
            } 
            else if (Score_from_Second<=.409){
              #Gets out at home
              Runs <- BaseRunners[3]
              BaseRunners[3] <- BaseRunners[1]
              BaseRunners[2] <- 0
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[1]
              BaseSpeeds[2] <- 0
              BaseSpeeds[1] <- speed_percentile
              Outs = Outs + 1
            }
            else {
              #Stays on 3rd
              Runs <- BaseRunners[3]
              BaseRunners[3] <- BaseRunners[2]
              BaseRunners[2] <- BaseRunners[1]
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[2]
              BaseSpeeds[2] <- BaseSpeeds[1]
              BaseSpeeds[1] <- speed_percentile
            }
          }
        } else {
          Runs <- BaseRunners[3]
          location <- runif(1)
          First_to_Third <- runif(1)
          #Then determining if a runner on 1st base makes it to third based on where the ball was hit
          if (location <= left_field) {
            if (First_to_Third <= .1788) {
              BaseRunners[3] <- BaseRunners[1]
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[1]
              BaseSpeeds[1] <- speed_percentile
            } else {
              BaseRunners[2] <- BaseRunners[1]
              BaseRunners[1] <- 1
              BaseSpeeds[2] <- BaseSpeeds[1]
              BaseSpeeds[1] <- speed_percentile
            }
          } else if (location <= left_field + right_field) {
            if (First_to_Third <= .3952) {
              BaseRunners[3] <- BaseRunners[1]
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[1]
              BaseSpeeds[1] <- speed_percentile
            } else {
              BaseRunners[2] <- BaseRunners[1]
              BaseRunners[1] <- 1
              BaseSpeeds[2] <- BaseSpeeds[1]
              BaseSpeeds[1] <- speed_percentile
            }
          } else if (location <= left_field + right_field + center_field) {
            if (First_to_Third <= .2692) {
              BaseRunners[3] <- BaseRunners[1]
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[1]
              BaseSpeeds[1] <- speed_percentile
            } else {
              BaseRunners[2] <- BaseRunners[1]
              BaseRunners[1] <- 1
              BaseSpeeds[2] <- BaseSpeeds[1]
              BaseSpeeds[1] <- speed_percentile
            }
          } else {
            if (First_to_Third <= .2316) {
              BaseRunners[3] <- BaseRunners[1]
              BaseRunners[1] <- 1
              BaseSpeeds[3] <- BaseSpeeds[1]
              BaseSpeeds[1] <- speed_percentile
            } else {
              BaseRunners[2] <- BaseRunners[1]
              BaseRunners[1] <- 1
              BaseSpeeds[2] <- BaseSpeeds[1]
              BaseSpeeds[1] <- speed_percentile
            }
          }
        }
      } else {
        #Then checking for doubles
        Upto_DBL <- BB_SNGL + double_percentage
        if (Plate_Appearance > BB_SNGL & Plate_Appearance <= Upto_DBL) {
          Outcome <- "Double"
          #If a runner is on 1st base, then depending on the speed of the base runner, they will either score, stay, or get out at home
          if (BaseRunners[1]==1){
            Runner_on_1st <- runif(1)
            if (BaseSpeeds[1]<34){
              if (Runner_on_1st <= .185) {
                #scores
                Runs <- sum(BaseRunners)
                BaseRunners[2] <- 1
                BaseSpeeds[1] <- 0
                BaseSpeeds[2] <- speed_percentile
                BaseSpeeds[3] <- 0
              } 
              else if (Runner_on_1st<=.222){
                #out at home
                Runs <- BaseRunners[2]+BaseRunners[3]
                Outs = Outs + 1
                BaseRunners[2] <- 1
                BaseSpeeds[1] <- 0
                BaseSpeeds[2] <- speed_percentile
                BaseSpeeds[3] <- 0
              }
              else {
                #stay on 3rd
                Runs <- BaseRunners[2] + BaseRunners[3]
                BaseRunners[3] <- 1
                BaseRunners[2] <- 1
                BaseSpeeds[3] <- BaseSpeeds[1]
                BaseSpeeds[1] <- 0
                BaseSpeeds[2] <- speed_percentile
              }
            }
            else if (BaseSpeeds[1]<67){
              if (Runner_on_1st <= .359) {
                #scores
                Runs <- sum(BaseRunners)
                BaseRunners[2] <- 1
                BaseSpeeds[1] <- 0
                BaseSpeeds[2] <- speed_percentile
                BaseSpeeds[3] <- 0
              } 
              else if (Runner_on_1st<=.436){
                #out at home
                Runs <- BaseRunners[2]+BaseRunners[3]
                Outs = Outs + 1
                BaseRunners[2] <- 1
                BaseSpeeds[1] <- 0
                BaseSpeeds[2] <- speed_percentile
                BaseSpeeds[3] <- 0
              }
              else {
                #stay on 3rd
                Runs <- BaseRunners[2] + BaseRunners[3]
                BaseRunners[3] <- 1
                BaseRunners[2] <- 1
                BaseSpeeds[3] <- BaseSpeeds[1]
                BaseSpeeds[1] <- 0
                BaseSpeeds[2] <- speed_percentile
              }
            }
            else{
              if (Runner_on_1st <= .457) {
                #scores
                Runs <- sum(BaseRunners)
                BaseRunners[2] <- 1
                BaseSpeeds[1] <- 0
                BaseSpeeds[2] <- speed_percentile
                BaseSpeeds[3] <- 0
              } 
              else if (Runner_on_1st<=.5){
                #out at home
                Runs <- BaseRunners[2]+BaseRunners[3]
                Outs = Outs + 1
                BaseRunners[2] <- 1
                BaseSpeeds[1] <- 0
                BaseSpeeds[2] <- speed_percentile
                BaseSpeeds[3] <- 0
              }
              else {
                #stay on 3rd
                Runs <- BaseRunners[2] + BaseRunners[3]
                BaseRunners[3] <- 1
                BaseRunners[2] <- 1
                BaseSpeeds[3] <- BaseSpeeds[1]
                BaseSpeeds[1] <- 0
                BaseSpeeds[2] <- speed_percentile
              }
            }
          }
          else{
            Runs <- BaseRunners[2] + BaseRunners[3]
            BaseRunners[2] <- 1
            BaseSpeeds[2] <- speed_percentile
            BaseSpeeds[3] <- 0
          }
        } else {
          #Now checking for triples
          Upto_TPL <- BB_SNGL + double_percentage + triple_percentage
          if (Plate_Appearance > Upto_DBL & Plate_Appearance <= Upto_TPL) {
            Outcome <- "Triple"
            Runs <- sum(BaseRunners)
            BaseRunners <- c(0, 0, 1)
            BaseSpeeds[1] <- 0
            BaseSpeeds[2] <- 0
            BaseSpeeds[3] <- speed_percentile
          } else {
            #Finally checking for home runs
            Upto_HR <- Upto_TPL + homerun_percentage
            if (Plate_Appearance > Upto_TPL & Plate_Appearance <= Upto_HR) {
              Outcome <- "Home run"
              Runs <- sum(BaseRunners) + 1
              BaseRunners <- rep(0, 3)
              BaseSpeeds <- rep(0,3)
            }
          }
        }
      }
    }
  }
  #Returning the base runners, outs, outcome of the at bat, and runs scored during that at bat
  return(data.frame(First = BaseRunners[1], Second = BaseRunners[2], 
                    Third = BaseRunners[3], First_Speed = BaseSpeeds[1], Second_Speed = BaseSpeeds[2], Third_Speed = BaseSpeeds[3], Outs = Outs, Outcome = Outcome,
                    Runs_Scored = Runs))
}

Innings_Simulator <- function(Lineup, Last_Hitter, Inning_Num) {
  inning_summary <- read.csv("inning_summary.csv")
  totals_summary <- read.csv("totals_summary.csv")
  #To take into account pitcher fatigue, multipliers were created for each inning based on historical performance above average
  inning_metrics <- inning_summary[inning_summary$inning == as.character(Inning_Num), ]
  
  #Then adjusted all stats in the stats data frame to reflect the multipliers
  Lineup_Adjusted <- Lineup %>%
    mutate(
      walk_percentage = walk_percentage * (inning_metrics$walk_percentage / totals_summary$walk_percentage),
      single_percentage = single_percentage * (inning_metrics$single_percentage / totals_summary$single_percentage),
      double_percentage = double_percentage * (inning_metrics$double_percentage / totals_summary$double_percentage),
      triple_percentage = triple_percentage * (inning_metrics$triple_percentage / totals_summary$triple_percentage),
      homerun_percentage = homerun_percentage * (inning_metrics$homerun_percentage / totals_summary$homerun_percentage)
    )
  #Inning starting data frame with everything at 0, but making sure the hitter is the one after the last one
  Innings_Result <- data.frame(First = 0, Second = 0, Third = 0, First_Speed = 0, 
                               Second_Speed = 0, Third_Speed = 0, Outs = 0,
                               Outcome = "", Runs_Scored = 0, Hitter = Last_Hitter)
  Outs <- 0
  Hitter_Due_Up <- Last_Hitter %% 9 + 1
  #Running the at bat simulator above given the data required
  while (Outs < 3) {
    Current_Situation <- tail(Innings_Result, 1)
    AB_Result <- At_Bat_Outcome_Simulator(BaseRunners = c(Current_Situation$First,
                                                          Current_Situation$Second,
                                                          Current_Situation$Third),
                                          BaseSpeeds = c(Current_Situation$First_Speed,
                                                         Current_Situation$Second_Speed,
                                                         Current_Situation$Third_Speed),
                                          walk_percentage = Lineup_Adjusted[Hitter_Due_Up,]$walk_percentage, 
                                          single_percentage = Lineup_Adjusted[Hitter_Due_Up,]$single_percentage,
                                          double_percentage = Lineup_Adjusted[Hitter_Due_Up,]$double_percentage, 
                                          triple_percentage = Lineup_Adjusted[Hitter_Due_Up,]$triple_percentage, 
                                          homerun_percentage = Lineup_Adjusted[Hitter_Due_Up,]$homerun_percentage,
                                          left_field = Lineup_Adjusted[Hitter_Due_Up,]$left_field,
                                          center_field = Lineup_Adjusted[Hitter_Due_Up,]$center_field,
                                          right_field = Lineup_Adjusted[Hitter_Due_Up,]$right_field,
                                          right = Lineup_Adjusted[Hitter_Due_Up,]$right,
                                          strikeouts = Lineup_Adjusted[Hitter_Due_Up,]$strikeouts,
                                          fly_out_pct = Lineup_Adjusted[Hitter_Due_Up,]$fly_out_pct,
                                          stolen_base_percentage = Lineup_Adjusted[Hitter_Due_Up,]$stolen_base_percentage,
                                          caught_stolen_base_percentage = Lineup_Adjusted[Hitter_Due_Up,]$caught_stolen_base_percentage,
                                          stolen_base_percentage_2_to_3 = Lineup_Adjusted[Hitter_Due_Up,]$stolen_base_percentage,
                                          caught_stolen_base_percentage_2_to_3 = Lineup_Adjusted[Hitter_Due_Up,]$caught_stolen_base_percentage,
                                          speed_percentile = Lineup_Adjusted[Hitter_Due_Up,]$speed_percentile,
                                          Outs = Current_Situation$Outs)
    AB_Result$Hitter <- Hitter_Due_Up
    Hitter_Due_Up <- (Hitter_Due_Up %% 9) + 1
    Innings_Result <- rbind(Innings_Result, AB_Result)
    Outs <- AB_Result$Outs
  }
  return(Innings_Result)
  #Returning the full inning data
}


#Game simulator function that only gives the total number of runs in the game
Game_Simulator_concise <- function(Lineup) {
  total_runs <- 0
  Last_Hitter <- 0
  for (Inning_Num in 1:9) {
    #Runs the inning simulator for each inning 1-9
    Innings_Result <- Innings_Simulator(Lineup, Last_Hitter, Inning_Num)
    total_runs <- total_runs + sum(Innings_Result$Runs_Scored)
    Last_Hitter <- tail(Innings_Result, 1)$Hitter
  }
  return(total_runs)
}


# Define UI for application
ui <- fluidPage(
  titlePanel("Baseball Game Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pitcher_handedness", "Pitcher Handedness:",
                  choices = c("Right", "Left", "Either")),
      selectInput("level_of_play", "Level of Play:",
                  choices = c("1", "2", "3", "4"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Catcher", 
                 uiOutput("catcher_ui")),
        tabPanel("First Base", 
                 uiOutput("first_base_ui")),
        tabPanel("Second Base", 
                 uiOutput("second_base_ui")),
        tabPanel("Shortstop", 
                 uiOutput("shortstop_ui")),
        tabPanel("Third Base", 
                 uiOutput("third_base_ui")),
        tabPanel("Left Field", 
                 uiOutput("left_field_ui")),
        tabPanel("Center Field", 
                 uiOutput("center_field_ui")),
        tabPanel("Right Field", 
                 uiOutput("right_field_ui")),
        tabPanel("Designated Hitter", 
                 uiOutput("designated_hitter_ui")),
        tabPanel("Order", 
                 uiOutput("order_ui")),
        tabPanel("Results", 
                 textOutput("average_runs"),
                 tableOutput("game_result"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load data
  stats_per_player_1A <- read.csv("stats_per_player_1A.csv")
  stats_per_player_2A <- read.csv("stats_per_player_2A.csv")
  stats_per_player_3A <- read.csv("stats_per_player_3A.csv")
  stats_per_player_4A <- read.csv("stats_per_player_4A.csv")
  stats_per_player_handedness_against_lefty_1A <- read.csv("stats_per_player_handedness_against_lefty_1A.csv")
  stats_per_player_handedness_against_righty_1A <- read.csv("stats_per_player_handedness_against_righty_1A.csv")
  stats_per_player_handedness_against_lefty_2A <- read.csv("stats_per_player_handedness_against_lefty_2A.csv")
  stats_per_player_handedness_against_righty_2A <- read.csv("stats_per_player_handedness_against_righty_2A.csv")
  stats_per_player_handedness_against_lefty_3A <- read.csv("stats_per_player_handedness_against_lefty_3A.csv")
  stats_per_player_handedness_against_righty_3A <- read.csv("stats_per_player_handedness_against_righty_3A.csv")
  stats_per_player_handedness_against_lefty_4A <- read.csv("stats_per_player_handedness_against_lefty_4A.csv")
  stats_per_player_handedness_against_righty_4A <- read.csv("stats_per_player_handedness_against_righty_4A.csv")
  player_positions_modified_all_1A <- read.csv("player_positions_modified_all_1A.csv")
  player_positions_modified_lefty_1A <- read.csv("player_positions_modified_lefty_1A.csv")
  player_positions_modified_righty_1A <- read.csv("player_positions_modified_righty_1A.csv")
  player_positions_modified_all_2A <- read.csv("player_positions_modified_all_2A.csv")
  player_positions_modified_lefty_2A <- read.csv("player_positions_modified_lefty_2A.csv")
  player_positions_modified_righty_2A <- read.csv("player_positions_modified_righty_2A.csv")
  player_positions_modified_all_3A <- read.csv("player_positions_modified_all_3A.csv")
  player_positions_modified_lefty_3A <- read.csv("player_positions_modified_lefty_3A.csv")
  player_positions_modified_righty_3A <- read.csv("player_positions_modified_righty_3A.csv")
  player_positions_modified_all_4A <- read.csv("player_positions_modified_all_4A.csv")
  player_positions_modified_lefty_4A <- read.csv("player_positions_modified_lefty_4A.csv")
  player_positions_modified_righty_4A <- read.csv("player_positions_modified_righty_4A.csv")
  
  # Function to create player selection UI for each position
  create_position_ui <- function(position) {
    if (input$level_of_play == "1"){
      if (input$pitcher_handedness == "Right"){
        players <- player_positions_modified_righty_1A %>%
          filter(get(position) >= 5) %>%
          select(player, on_base_percentage, homerun_percentage, speed_percentile, handedness) %>%
          arrange(player)
      }
      else if(input$pitcher_handedness == "Left"){
        players <- player_positions_modified_lefty_1A %>%
          filter(get(position) >= 5) %>%
          select(player, on_base_percentage, homerun_percentage, speed_percentile, handedness) %>%
          arrange(player)
      }
      else{
        players <- player_positions_modified_all_1A %>%
          filter(get(position) >= 5) %>%
          select(player, on_base_percentage, homerun_percentage, speed_percentile, handedness) %>%
          arrange(player)
      }
    }
    else if (input$level_of_play == "2"){
      if (input$pitcher_handedness == "Right"){
        players <- player_positions_modified_righty_2A %>%
          filter(get(position) >= 5) %>%
          select(player, on_base_percentage, homerun_percentage, speed_percentile, handedness) %>%
          arrange(player)
      }
      else if(input$pitcher_handedness == "Left"){
        players <- player_positions_modified_lefty_2A %>%
          filter(get(position) >= 5) %>%
          select(player, on_base_percentage, homerun_percentage, speed_percentile, handedness) %>%
          arrange(player)
      }
      else{
        players <- player_positions_modified_all_2A %>%
          filter(get(position) >= 5) %>%
          select(player, on_base_percentage, homerun_percentage, speed_percentile, handedness) %>%
          arrange(player)
      }
    }
    else if (input$level_of_play == "3"){
      if (input$pitcher_handedness == "Right"){
        players <- player_positions_modified_righty_3A %>%
          filter(get(position) >= 5) %>%
          select(player, on_base_percentage, homerun_percentage, speed_percentile, handedness) %>%
          arrange(player)
      }
      else if(input$pitcher_handedness == "Left"){
        players <- player_positions_modified_lefty_3A %>%
          filter(get(position) >= 5) %>%
          select(player, on_base_percentage, homerun_percentage, speed_percentile, handedness) %>%
          arrange(player)
      }
      else{
        players <- player_positions_modified_all_3A %>%
          filter(get(position) >= 5) %>%
          select(player, on_base_percentage, homerun_percentage, speed_percentile, handedness) %>%
          arrange(player)
      }
    }
    else if (input$level_of_play == "4"){
      if (input$pitcher_handedness == "Right"){
        players <- player_positions_modified_righty_4A %>%
          filter(get(position) >= 5) %>%
          select(player, on_base_percentage, homerun_percentage, speed_percentile, handedness) %>%
          arrange(player)
      }
      else if(input$pitcher_handedness == "Left"){
        players <- player_positions_modified_lefty_4A %>%
          filter(get(position) >= 5) %>%
          select(player, on_base_percentage, homerun_percentage, speed_percentile, handedness) %>%
          arrange(player)
      }
      else{
        players <- player_positions_modified_all_4A %>%
          filter(get(position) >= 5) %>%
          select(player, on_base_percentage, homerun_percentage, speed_percentile, handedness) %>%
          arrange(player)
      }
    }
    fluidPage(
      fluidRow(
        lapply(1:nrow(players), function(i) {
          player <- players[i, ]
          
          gt_table <- gt(data.frame(
            Metric = c("On Base Percentage", "Home Run Percentage", "Speed Index"),
            Value = c(
              player$on_base_percentage,
              player$homerun_percentage,
              player$speed_percentile
              #player$handedness
            )
          )) %>%
            data_color(columns = Value, rows = 1, palette = "RdYlGn", domain = c(0, .61)) %>%
            data_color(columns = Value, rows = 2, palette = "RdYlGn", domain = c(0, .14)) %>%
            data_color(columns = Value, rows = 3, palette = "RdYlGn", domain = c(0, 100)) %>%
            tab_header(title = paste("Player", player$player),
                       subtitle = paste("Handedness -", player$handedness)) %>%
            fmt_number(columns = Value, decimals = 2) %>%
            cols_label(Metric ~ "", Value ~ "") %>%
            tab_options(
              heading.border.bottom.style = "hidden"
            ) %>%
            as_raw_html()
          
          column(6,
                 wellPanel(
                   radioButtons(
                     paste0("select_", position), 
                     choices = setNames(player$player, player$player), 
                     label = HTML(gt_table), 
                     selected = character(0)
                   )
                 )
          )
        })
      ),
      actionButton(paste0("save_", position), "Save Selection")
    )
    
  }
  
  # Generate UI for each position tab
  output$catcher_ui <- renderUI(create_position_ui("catcher"))
  output$first_base_ui <- renderUI(create_position_ui("first_base"))
  output$second_base_ui <- renderUI(create_position_ui("second_base"))
  output$shortstop_ui <- renderUI(create_position_ui("shortstop"))
  output$third_base_ui <- renderUI(create_position_ui("third_base"))
  output$left_field_ui <- renderUI(create_position_ui("left_field"))
  output$center_field_ui <- renderUI(create_position_ui("center_field"))
  output$right_field_ui <- renderUI(create_position_ui("right_field"))
  output$designated_hitter_ui <- renderUI(create_position_ui("designated_hitter"))
  
  # Function to save player selections
  save_selections <- function(position) {
    input[[paste0("select_", position)]]
  }
  observeEvent(input$save_catcher, {
    session$userData$catcher <- save_selections("catcher")
    print(paste("Catcher selected:", session$userData$catcher))  # Debug output
  })
  observeEvent(input$save_first_base, {
    session$userData$first_base <- save_selections("first_base")
    print(paste("First Base selected:", session$userData$first_base))  # Debug output
  })
  observeEvent(input$save_second_base, {
    session$userData$second_base <- save_selections("second_base")
    print(paste("Second Base selected:", session$userData$second_base))  # Debug output
  })
  observeEvent(input$save_shortstop, {
    session$userData$shortstop <- save_selections("shortstop")
    print(paste("Shortstop selected:", session$userData$shortstop))  # Debug output
  })
  observeEvent(input$save_third_base, {
    session$userData$third_base <- save_selections("third_base")
    print(paste("Third Base selected:", session$userData$third_base))  # Debug output
  })
  observeEvent(input$save_left_field, {
    session$userData$left_field <- save_selections("left_field")
    print(paste("Left Field selected:", session$userData$left_field))  # Debug output
  })
  observeEvent(input$save_center_field, {
    session$userData$center_field <- save_selections("center_field")
    print(paste("Center Field selected:", session$userData$center_field))  # Debug output
  })
  observeEvent(input$save_right_field, {
    session$userData$right_field <- save_selections("right_field")
    print(paste("Right Field selected:", session$userData$right_field))  # Debug output
  })
  observeEvent(input$save_designated_hitter, {
    session$userData$designated_hitter <- save_selections("designated_hitter")
    print(paste("Designated Hitter selected:", session$userData$designated_hitter))  # Debug output
  })
  
  # Generate lineup order UI
  output$order_ui <- renderUI({
    selected_players <- c(
      session$userData$catcher,
      session$userData$first_base,
      session$userData$second_base,
      session$userData$shortstop,
      session$userData$third_base,
      session$userData$left_field,
      session$userData$center_field,
      session$userData$right_field,
      session$userData$designated_hitter
    )
    
    # Debug output
    print("Selected Players for Order:")
    print(selected_players)
    
    # Check if all positions are filled
    if (any(is.null(selected_players)) || any(selected_players == "")) {
      return(tagList(
        h4("Please select players for all positions before selecting batting order.")
      ))
    }
    
    # Create selectInputs for each batting position
    batting_order_ui <- lapply(1:9, function(i) {
      selectInput(paste0("order_", i), 
                  label = paste("Select Player for Position", i),
                  choices = selected_players,
                  selected = selected_players[i])
    })
    
    fluidPage(
      h4("Set Batting Order"),
      fluidRow(
        column(12,
               do.call(tagList, batting_order_ui),
               actionButton("run_simulation", "Run Simulation")
        )
      )
    )
  })
  
  # Function to get stats based on pitcher handedness
  stats_per_player_based_on_pitcher <- function(Pitcher_handedness, Level) {
    if (Level == "1"){
      if (Pitcher_handedness == "Right") {
        lineup_testing <- stats_per_player_handedness_against_righty_1A %>% filter(player %in% c(
          session$userData$catcher,
          session$userData$first_base,
          session$userData$second_base,
          session$userData$shortstop,
          session$userData$third_base,
          session$userData$left_field,
          session$userData$center_field,
          session$userData$right_field,
          session$userData$designated_hitter
        ))
      } else if (Pitcher_handedness == "Left") {
        lineup_testing <- stats_per_player_handedness_against_lefty_1A %>% filter(player %in% c(
          session$userData$catcher,
          session$userData$first_base,
          session$userData$second_base,
          session$userData$shortstop,
          session$userData$third_base,
          session$userData$left_field,
          session$userData$center_field,
          session$userData$right_field,
          session$userData$designated_hitter
        ))
      }
      else if (Pitcher_handedness == "Either") {
        lineup_testing <- stats_per_player_1A %>% filter(player %in% c(
          session$userData$catcher,
          session$userData$first_base,
          session$userData$second_base,
          session$userData$shortstop,
          session$userData$third_base,
          session$userData$left_field,
          session$userData$center_field,
          session$userData$right_field,
          session$userData$designated_hitter
        ))
      }
    }
    if (Level == "2"){
      if (Pitcher_handedness == "Right") {
        lineup_testing <- stats_per_player_handedness_against_righty_2A %>% filter(player %in% c(
          session$userData$catcher,
          session$userData$first_base,
          session$userData$second_base,
          session$userData$shortstop,
          session$userData$third_base,
          session$userData$left_field,
          session$userData$center_field,
          session$userData$right_field,
          session$userData$designated_hitter
        ))
      } else if (Pitcher_handedness == "Left") {
        lineup_testing <- stats_per_player_handedness_against_lefty_2A %>% filter(player %in% c(
          session$userData$catcher,
          session$userData$first_base,
          session$userData$second_base,
          session$userData$shortstop,
          session$userData$third_base,
          session$userData$left_field,
          session$userData$center_field,
          session$userData$right_field,
          session$userData$designated_hitter
        ))
      }
      else if (Pitcher_handedness == "Either") {
        lineup_testing <- stats_per_player_2A %>% filter(player %in% c(
          session$userData$catcher,
          session$userData$first_base,
          session$userData$second_base,
          session$userData$shortstop,
          session$userData$third_base,
          session$userData$left_field,
          session$userData$center_field,
          session$userData$right_field,
          session$userData$designated_hitter
        ))
      }
    }
    else if (Level == "3"){
      if (Pitcher_handedness == "Right") {
        lineup_testing <- stats_per_player_handedness_against_righty_3A %>% filter(player %in% c(
          session$userData$catcher,
          session$userData$first_base,
          session$userData$second_base,
          session$userData$shortstop,
          session$userData$third_base,
          session$userData$left_field,
          session$userData$center_field,
          session$userData$right_field,
          session$userData$designated_hitter
        ))
      } else if (Pitcher_handedness == "Left") {
        lineup_testing <- stats_per_player_handedness_against_lefty_3A %>% filter(player %in% c(
          session$userData$catcher,
          session$userData$first_base,
          session$userData$second_base,
          session$userData$shortstop,
          session$userData$third_base,
          session$userData$left_field,
          session$userData$center_field,
          session$userData$right_field,
          session$userData$designated_hitter
        ))
      }
      else if (Pitcher_handedness == "Either") {
        lineup_testing <- stats_per_player_3A %>% filter(player %in% c(
          session$userData$catcher,
          session$userData$first_base,
          session$userData$second_base,
          session$userData$shortstop,
          session$userData$third_base,
          session$userData$left_field,
          session$userData$center_field,
          session$userData$right_field,
          session$userData$designated_hitter
        ))
      }
    }
    else if (Level == "4"){
      if (Pitcher_handedness == "Right") {
        lineup_testing <- stats_per_player_handedness_against_righty_4A %>% filter(player %in% c(
          session$userData$catcher,
          session$userData$first_base,
          session$userData$second_base,
          session$userData$shortstop,
          session$userData$third_base,
          session$userData$left_field,
          session$userData$center_field,
          session$userData$right_field,
          session$userData$designated_hitter
        ))
      } else if (Pitcher_handedness == "Left") {
        lineup_testing <- stats_per_player_handedness_against_lefty_4A %>% filter(player %in% c(
          session$userData$catcher,
          session$userData$first_base,
          session$userData$second_base,
          session$userData$shortstop,
          session$userData$third_base,
          session$userData$left_field,
          session$userData$center_field,
          session$userData$right_field,
          session$userData$designated_hitter
        ))
      }
      else if (Pitcher_handedness == "Either") {
        lineup_testing <- stats_per_player_4A %>% filter(player %in% c(
          session$userData$catcher,
          session$userData$first_base,
          session$userData$second_base,
          session$userData$shortstop,
          session$userData$third_base,
          session$userData$left_field,
          session$userData$center_field,
          session$userData$right_field,
          session$userData$designated_hitter
        ))
      }
    }
    return(lineup_testing)
  }
  
  
  # Run simulations when the button is clicked
  observeEvent(input$run_simulation, {
    selected_lineup <- c(
      session$userData$catcher,
      session$userData$first_base,
      session$userData$second_base,
      session$userData$shortstop,
      session$userData$third_base,
      session$userData$left_field,
      session$userData$center_field,
      session$userData$right_field,
      session$userData$designated_hitter
    )
    
    # Order selection
    batting_order <- sapply(1:9, function(i) input[[paste0("order_", i)]])
    
    # Debug output
    print("Batting Order:")
    print(batting_order)
    
    # Get the appropriate stats for the selected lineup based on pitcher handedness
    pitcher_handedness <- input$pitcher_handedness
    level <- input$level_of_play
    lineup_stats <- stats_per_player_based_on_pitcher(pitcher_handedness, level)
    print(lineup_stats)
    
    # Filter the stats for the selected lineup
    selected_stats <- lineup_stats %>% 
      filter(player %in% selected_lineup)
    
    # Debug output
    print("Selected Stats before reordering:")
    print(selected_stats)
    
    # Reorder selected_stats according to batting_order
    if (length(batting_order) == nrow(selected_stats)) {
      # Ensure batting_order is unique and all elements are present in selected_stats
      batting_order <- unique(batting_order)
      selected_stats <- selected_stats %>% 
        arrange(match(player, batting_order))
      
      # Debug output
      print("Selected Stats after reordering:")
      print(selected_stats)
      
      # Run simulations with progress bar
      withProgress(message = 'Running Simulations...', value = 0, {
        num_simulations <- 1000
        total_runs <- numeric(num_simulations)
        
        for (i in seq_len(num_simulations)) {
          total_runs[i] <- Game_Simulator_concise(selected_stats)  # Pass the reordered selected_stats
          incProgress(1 / num_simulations) # Update progress bar
        }
        
        # Compute average runs
        average_runs <- mean(total_runs)
        
        # Display the average runs scored
        output$average_runs <- renderText({
          paste("Average Runs Scored:", round(average_runs, 2))
        })
      })
      
    } else {
      output$average_runs <- renderText({
        "Error: Batting order does not match the number of players in the selected lineup."
      })
    }
  })
}
