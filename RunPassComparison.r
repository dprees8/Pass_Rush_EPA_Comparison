## First calculate the EP_MODEL based on coding from init_ep_fg_models.r
## Alternatively, you can save teh EP_MODEL and skip the some of the coding as some takes a while to run.

# File for building EP models (and their corresponding FG models)

# Access tidyverse:
install.packages("tidyverse")
library(tidyverse)

# Access nflWAR:
install.packages("devtools")
devtools::install_github("ryurko/nflWAR")

library(nflWAR)

# Load data from 2009 to 2016 from the nflscrapR-data repository using the
# get_pbp_data() function from the nflWAR package:
pbp_data <- get_pbp_data(2009:2016)

# Remove error game from 2011 that is coded incorrectly in raw JSON data:
pbp_data <- pbp_data %>% filter(GameID != "2011121101")

nrow(pbp_data)
# 362263

#' Define a function that takes in a play-by-play data set and returns
#' what the type of next score is and the drive number only within same half.
#' @param pbp_dataset Play-by-play dataset with the following columns:
#' sp - scoring play indicator, PlayType - what type of play, qtr - quarter
#' of the game, Drive - drive number for the play, ReturnResult - indicates
#' what happened on return type plays, posteam - indicates the possession team
#' for the play, and columns for ReturnResult, FieldGoalResult, ExPointResult,
#' TwoPointConv, DefTwoPoint, Touchdown.
#' @return Data frame with two columns: Next_Score_Half denoting the type of 
#' the next scoring event occurring within the half of each play and 
#' Drive_Score_Half denoting the drive number of the next scoring play.

find_game_next_score_half <- function(pbp_dataset) {
  
  # Which rows are the scoring plays:
  score_plays <- which(pbp_dataset$sp == 1 & pbp_dataset$PlayType != "No Play")
  
  # Define a helper function that takes in the current play index, 
  # a vector of the scoring play indices, play-by-play data,
  # and returns the score type and drive number for the next score:
  find_next_score <- function(play_i, score_plays_i,pbp_df) {
    
    # Find the next score index for the current play
    # based on being the first next score index:
    next_score_i <- score_plays_i[which(score_plays_i >= play_i)[1]]
    
    # If next_score_i is NA (no more scores after current play)
    # or if the next score is in another half,
    # then return No_Score and the current drive number
    if (is.na(next_score_i) | 
        (pbp_df$qtr[play_i] %in% c(1, 2) & pbp_df$qtr[next_score_i] %in% c(3, 4, 5)) | 
        (pbp_df$qtr[play_i] %in% c(3, 4) & pbp_df$qtr[next_score_i] == 5)) {
      
      score_type <- "No_Score"
      
      # Make it the current play index
      score_drive <- pbp_df$Drive[play_i]
      
      # Else return the observed next score type and drive number:
    } else {
      
      # Store the score_drive number
      score_drive <- pbp_df$Drive[next_score_i]
      
      # Then check the play types to decide what to return
      # based on several types of cases for the next score:
      
      # 1: Return TD
      if (identical(pbp_df$ReturnResult[next_score_i], "Touchdown")) {
        
        # For return touchdowns the current posteam would not have
        # possession at the time of return, so it's flipped:
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Opp_Touchdown"
          
        } else {
          
          score_type <- "Touchdown"
          
        }
      } else if (identical(pbp_df$FieldGoalResult[next_score_i], "Good")) {
        
        # 2: Field Goal
        # Current posteam made FG
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Field_Goal"
          
          # Opponent made FG
        } else {
          
          score_type <- "Opp_Field_Goal"
          
        }
        
        # 3: Touchdown (returns already counted for)
      } else if (pbp_df$Touchdown[next_score_i] == 1) {
        
        # Current posteam TD
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Touchdown"
          
          # Opponent TD
        } else {
          
          score_type <- "Opp_Touchdown"
          
        }
        # 4: Safety (similar to returns)
      } else if (pbp_df$Safety[next_score_i] == 1) {
        
        if (identical(pbp_df$posteam[play_i],pbp_df$posteam[next_score_i])) {
          
          score_type <- "Opp_Safety"
          
        } else {
          
          score_type <- "Safety" 
          
        }
        # 5: Extra Points
      } else if (identical(pbp_df$ExPointResult[next_score_i], "Made")) {
        
        # Current posteam Extra Point
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Extra_Point"
          
          # Opponent Extra Point
        } else {
          
          score_type <- "Opp_Extra_Point"
          
        }
        # 6: Two Point Conversions
      } else if (identical(pbp_df$TwoPointConv[next_score_i], "Success")) {
        
        # Current posteam Two Point Conversion
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Two_Point_Conversion"
          
          # Opponent Two Point Conversion
        } else {
          
          score_type <- "Opp_Two_Point_Conversion"
          
        }
        
        # 7: Defensive Two Point (like returns)
      } else if (identical(pbp_df$DefTwoPoint[next_score_i], "Success")) {
        
        if (identical(pbp_df$posteam[play_i], pbp_df$posteam[next_score_i])) {
          
          score_type <- "Opp_Defensive_Two_Point"
          
        } else {
          
          score_type <- "Defensive_Two_Point"
          
        }
        
        # 8: Errors of some sort so return NA (but shouldn't take place)
      } else {
        
        score_type <- NA
        
      }
    }
    
    return(data.frame(Next_Score_Half = score_type,
                      Drive_Score_Half = score_drive))
  }
  
  # Using lapply and then bind_rows is much faster than
  # using map_dfr() here:
  lapply(c(1:nrow(pbp_dataset)), find_next_score, 
         score_plays_i = score_plays, pbp_df = pbp_dataset) %>%
    bind_rows() %>%
    return
}

# Apply to each game (ignore the warning messages here):
pbp_next_score_half <- map_dfr(unique(pbp_data$GameID), 
                               function(x) {
                                 pbp_data %>%
                                   filter(GameID == x) %>%
                                   find_game_next_score_half()
                               })

# Join to the pbp_data:
pbp_data_next_score <- bind_cols(pbp_data, pbp_next_score_half)

# Create the EP model dataset that only includes plays with basic seven 
# types of next scoring events along with the following play types:
# Field Goal, No Play, Pass, Punt, Run, Sack, Spike

pbp_ep_model_data <- pbp_data_next_score %>% 
  filter(Next_Score_Half %in% c("Opp_Field_Goal", "Opp_Safety", "Opp_Touchdown",
                                "Field_Goal", "No_Score", "Safety", "Touchdown") & 
           PlayType %in% c("Field Goal", "No Play", "Pass", "Punt", "Run", "Sack",
                           "Spike") & is.na(TwoPointConv) & is.na(ExPointResult) &
           !is.na(down) & !is.na(TimeSecs))

nrow(pbp_ep_model_data)
# 304805

# Now adjust and create the model variables:
pbp_ep_model_data <- pbp_ep_model_data %>%
  
  # Reference level should be No_Score:
  mutate(Next_Score_Half = fct_relevel(factor(Next_Score_Half), "No_Score"),
         
         # Create a variable that is time remaining until end of half:
         # (only working with up to 2016 data so can ignore 2017 time change)
         TimeSecs_Remaining = as.numeric(ifelse(qtr %in% c(1,2), TimeSecs - 1800,
                                                ifelse(qtr == 5, TimeSecs + 900, 
                                                       TimeSecs))),
         
         # log transform of yards to go and indicator for two minute warning:
         log_ydstogo = log(ydstogo),
         Under_TwoMinute_Warning = ifelse(TimeSecs_Remaining < 120, 1, 0),
         
         # Changing down into a factor variable: 
         down = factor(down),
         
         # Calculate the drive difference between the next score drive and the 
         # current play drive:
         Drive_Score_Dist = Drive_Score_Half - Drive,
         
         # Create a weight column based on difference in drives between play and next score:
         Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) / 
           (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
         # Create a weight column based on score differential:
         ScoreDiff_W = (max(abs(ScoreDiff)) - abs(ScoreDiff)) / 
           (max(abs(ScoreDiff)) - min(abs(ScoreDiff))),
         # Add these weights together and scale again:
         Total_W = Drive_Score_Dist_W + ScoreDiff_W,
         Total_W_Scaled = (Total_W - min(Total_W)) / 
           (max(Total_W) - min(Total_W)))

# Save dataset in data folder as pbp_ep_model_data.csv
# (NOTE: this dataset is not pushed due to its size exceeding
# the github limit but will be referenced in other files)
write_csv(pbp_ep_model_data, "pbp_ep_model_data.csv")

# Fit the expected points model:
install.packages("nnet")
ep_model <- nnet::multinom(Next_Score_Half ~ TimeSecs_Remaining + yrdline100 + 
                             down + log_ydstogo + GoalToGo + log_ydstogo*down + 
                             yrdline100*down + GoalToGo*log_ydstogo + 
                             Under_TwoMinute_Warning, data = pbp_ep_model_data, 
                           weights = Total_W_Scaled, maxit = 300)

# Save the model (commented out due to file size limit)
save(ep_model, file="ep_model.RData")

# ----------------------------------------------------------------------------

# Set up a function that uses the EP_MODEL to calculate the EPA for various scenarios
# given a team and qb input
# If no team and/or qb is provided, this will default to all

install.packages("tidyverse")
install.packages("dplyr")
install.packages("na.tools")
install.packages("ggimage")
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)

# Looking only at 2019 data
pbp_data_2018 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))
# Assign Run Pass variables
pbp_data_2018_rp <- pbp_data_2018 %>% filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

# Remove non-plays
pbp_data_2018_rp <- pbp_data_2018_rp %>%  mutate(pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)) 

# Remove penalties
pbp_data_2018_rp <- pbp_data_2018_rp %>% filter(pass==1 | rush==1)

RunPassComparison <- function(team = "all", qb = "all", yardline_100_input = 80, down_input = 1, yards_to_go_input = 10) {

  if(and(team=="all", qb=="all")) {
## Variable determination for full season
## Breakdown rush plays into thirds and number of fumbles
    rush1<-pbp_data_2018_rp %>% filter(rush == 1) %>% summarize(rush_ypc=mean(yards_gained), rush_plays=n(), rush_fumble=sum(fumble_lost))
    x<-pbp_data_2018_rp %>% filter(rush == 1, fumble_lost==0) %>% summarize(rush_33=quantile(yards_gained, probs=1/3), rush_67=quantile(yards_gained, probs=2/3)) 
        rush_33 = as.integer(x[ ,"rush_33"])
        rush_67 = as.integer(x[ ,"rush_67"])
    rush2<-pbp_data_2018_rp %>% filter(rush == 1, fumble_lost==0, yards_gained<=rush_33) %>% summarize(rush_ypc_1=mean(yards_gained), rush_plays_1=n())  
    rush3<-pbp_data_2018_rp %>% filter(rush == 1, fumble_lost==0, yards_gained>rush_33, yards_gained<=rush_67) %>% summarize(rush_ypc_2=mean(yards_gained), rush_plays_2=n())
    rush4<-pbp_data_2018_rp %>% filter(rush == 1, fumble_lost==0, yards_gained>rush_67) %>% summarize(rush_ypc_3=mean(yards_gained), rush_plays_3=n())
## Breakdown pass plays into completions into thirds, incompletions, interceptions, sacks, qb scrambles, and penalties
    pass1<-pbp_data_2018_rp %>% filter(pass == 1) %>% summarize(pass_ypc=mean(yards_gained), pass_plays=n(), comp_pass=sum(complete_pass), inc_pass=sum(incomplete_pass), int=sum(interception), pass_penalty=sum(penalty))
    y<-pbp_data_2018_rp %>% filter(pass == 1, complete_pass==1) %>% summarize(pass_33=quantile(yards_gained, probs=1/3), pass_67=quantile(yards_gained, probs=2/3)) 
        pass_33 = as.integer(y[ ,"pass_33"])
        pass_67 = as.integer(y[ ,"pass_67"])
    pass2<-pbp_data_2018_rp %>% filter(pass == 1, complete_pass==1, yards_gained<=pass_33) %>% summarize(pass_ypc_1=mean(yards_gained), pass_plays_1=n())  
    pass3<-pbp_data_2018_rp %>% filter(pass == 1, complete_pass==1, yards_gained>pass_33, yards_gained<=pass_67) %>% summarize(pass_ypc_2=mean(yards_gained), pass_plays_2=n())
    pass4<-pbp_data_2018_rp %>% filter(pass == 1, complete_pass==1, yards_gained>pass_67) %>% summarize(pass_ypc_3=mean(yards_gained), pass_plays_3=n())
    pass5<-pbp_data_2018_rp %>% filter(pass == 1, sack==1) %>% summarize(sack_ypc=mean(yards_gained), sack_count=n())
    pass6<-pbp_data_2018_rp %>% filter(pass == 1, qb_scramble==1) %>% summarize(scramble_ypc=mean(yards_gained), scramble_count=n())
  } else if(qb=="all") {
## Variable determination for a given team
## Breakdown rush plays into thirds and number of fumbles
    rush1<-pbp_data_2018_rp %>% filter(rush == 1, posteam == team) %>% summarize(rush_ypc=mean(yards_gained), rush_plays=n(), rush_fumble=sum(fumble_lost))
    x<-pbp_data_2018_rp %>% filter(rush == 1, posteam == team, fumble_lost==0) %>% summarize(rush_33=quantile(yards_gained, probs=1/3), rush_67=quantile(yards_gained, probs=2/3)) 
        rush_33 = as.integer(x[ ,"rush_33"])
        rush_67 = as.integer(x[ ,"rush_67"])
    rush2<-pbp_data_2018_rp %>% filter(rush == 1, posteam == team, fumble_lost==0, yards_gained<=rush_33) %>% summarize(rush_ypc_1=mean(yards_gained), rush_plays_1=n())  
    rush3<-pbp_data_2018_rp %>% filter(rush == 1, posteam == team, fumble_lost==0, yards_gained>rush_33, yards_gained<=rush_67) %>% summarize(rush_ypc_2=mean(yards_gained), rush_plays_2=n())
    rush4<-pbp_data_2018_rp %>% filter(rush == 1, posteam == team, fumble_lost==0, yards_gained>rush_67) %>% summarize(rush_ypc_3=mean(yards_gained), rush_plays_3=n())
## Breakdown pass plays into completions into thirds, incompletions, interceptions, sacks, qb scrambles, and penalties
    pass1<-pbp_data_2018_rp %>% filter(pass == 1, posteam == team) %>% summarize(pass_ypc=mean(yards_gained), pass_plays=n(), comp_pass=sum(complete_pass), inc_pass=sum(incomplete_pass), int=sum(interception), pass_penalty=sum(penalty))
    y<-pbp_data_2018_rp %>% filter(pass == 1, posteam == team, complete_pass==1) %>% summarize(pass_33=quantile(yards_gained, probs=1/3), pass_67=quantile(yards_gained, probs=2/3)) 
        pass_33 = as.integer(y[ ,"pass_33"])
        pass_67 = as.integer(y[ ,"pass_67"])
    pass2<-pbp_data_2018_rp %>% filter(pass == 1, posteam == team, complete_pass==1, yards_gained<=pass_33) %>% summarize(pass_ypc_1=mean(yards_gained), pass_plays_1=n())  
    pass3<-pbp_data_2018_rp %>% filter(pass == 1, posteam == team, complete_pass==1, yards_gained>pass_33, yards_gained<=pass_67) %>% summarize(pass_ypc_2=mean(yards_gained), pass_plays_2=n())
    pass4<-pbp_data_2018_rp %>% filter(pass == 1, posteam == team, complete_pass==1, yards_gained>pass_67) %>% summarize(pass_ypc_3=mean(yards_gained), pass_plays_3=n())
    pass5<-pbp_data_2018_rp %>% filter(pass == 1, posteam == team, sack==1) %>% summarize(sack_ypc=mean(yards_gained), sack_count=n())
    pass6<-pbp_data_2018_rp %>% filter(pass == 1, posteam == team, qb_scramble==1) %>% summarize(scramble_ypc=mean(yards_gained), scramble_count=n())
  } else if(team=="all") {
## Variable determination for a given qb, but not team, rushing uses full season
## IS THERE ANY WAY TO ONLY INCLUDE GAMES WITH A GIVEN QB STARTING?
## Breakdown rush plays into thirds and number of fumbles
    rush1<-pbp_data_2018_rp %>% filter(rush == 1) %>% summarize(rush_ypc=mean(yards_gained), rush_plays=n(), rush_fumble=sum(fumble_lost))
    x<-pbp_data_2018_rp %>% filter(rush == 1, fumble_lost==0) %>% summarize(rush_33=quantile(yards_gained, probs=1/3), rush_67=quantile(yards_gained, probs=2/3)) 
        rush_33 = as.integer(x[ ,"rush_33"])
        rush_67 = as.integer(x[ ,"rush_67"])
    rush2<-pbp_data_2018_rp %>% filter(rush == 1, fumble_lost==0, yards_gained<=rush_33) %>% summarize(rush_ypc_1=mean(yards_gained), rush_plays_1=n())  
    rush3<-pbp_data_2018_rp %>% filter(rush == 1, fumble_lost==0, yards_gained>rush_33, yards_gained<=rush_67) %>% summarize(rush_ypc_2=mean(yards_gained), rush_plays_2=n())
    rush4<-pbp_data_2018_rp %>% filter(rush == 1, fumble_lost==0, yards_gained>rush_67) %>% summarize(rush_ypc_3=mean(yards_gained), rush_plays_3=n())
## Breakdown pass plays into completions into thirds, incompletions, interceptions, sacks, qb scrambles, and penalties
    pass1<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb) %>% summarize(pass_ypc=mean(yards_gained), pass_plays=n(), comp_pass=sum(complete_pass), inc_pass=sum(incomplete_pass), int=sum(interception), pass_penalty=sum(penalty))
    y<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb, complete_pass==1) %>% summarize(pass_33=quantile(yards_gained, probs=1/3), pass_67=quantile(yards_gained, probs=2/3)) 
        pass_33 = as.integer(y[ ,"pass_33"])
        pass_67 = as.integer(y[ ,"pass_67"])
    pass2<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb, complete_pass==1, yards_gained<=pass_33) %>% summarize(pass_ypc_1=mean(yards_gained), pass_plays_1=n())  
    pass3<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb, complete_pass==1, yards_gained>pass_33, yards_gained<=pass_67) %>% summarize(pass_ypc_2=mean(yards_gained), pass_plays_2=n())
    pass4<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb, complete_pass==1, yards_gained>pass_67) %>% summarize(pass_ypc_3=mean(yards_gained), pass_plays_3=n())
    pass5<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb, sack==1) %>% summarize(sack_ypc=mean(yards_gained), sack_count=n())
    pass6<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb, qb_scramble==1) %>% summarize(scramble_ypc=mean(yards_gained), scramble_count=n())
  } else {
## Variable determination for a given qb and team
## IS THERE ANY WAY TO ONLY INCLUDE GAMES WITH A GIVEN QB STARTING?
## Breakdown rush plays into thirds and number of fumbles
    rush1<-pbp_data_2018_rp %>% filter(rush == 1, posteam == team) %>% summarize(rush_ypc=mean(yards_gained), rush_plays=n(), rush_fumble=sum(fumble_lost))
    x<-pbp_data_2018_rp %>% filter(rush == 1, posteam == team, fumble_lost==0) %>% summarize(rush_33=quantile(yards_gained, probs=1/3), rush_67=quantile(yards_gained, probs=2/3)) 
        rush_33 = as.integer(x[ ,"rush_33"])
        rush_67 = as.integer(x[ ,"rush_67"])
    rush2<-pbp_data_2018_rp %>% filter(rush == 1, posteam == team, fumble_lost==0, yards_gained<=rush_33) %>% summarize(rush_ypc_1=mean(yards_gained), rush_plays_1=n())  
    rush3<-pbp_data_2018_rp %>% filter(rush == 1, posteam == team, fumble_lost==0, yards_gained>rush_33, yards_gained<=rush_67) %>% summarize(rush_ypc_2=mean(yards_gained), rush_plays_2=n())
    rush4<-pbp_data_2018_rp %>% filter(rush == 1, posteam == team, fumble_lost==0, yards_gained>rush_67) %>% summarize(rush_ypc_3=mean(yards_gained), rush_plays_3=n())
## Breakdown pass plays into completions into thirds, incompletions, interceptions, sacks, qb scrambles, and penalties
    pass1<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb) %>% summarize(pass_ypc=mean(yards_gained), pass_plays=n(), comp_pass=sum(complete_pass), inc_pass=sum(incomplete_pass), int=sum(interception), pass_penalty=sum(penalty))
    y<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb, complete_pass==1) %>% summarize(pass_33=quantile(yards_gained, probs=1/3), pass_67=quantile(yards_gained, probs=2/3)) 
        pass_33 = as.integer(y[ ,"pass_33"])
        pass_67 = as.integer(y[ ,"pass_67"])
    pass2<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb, complete_pass==1, yards_gained<=pass_33) %>% summarize(pass_ypc_1=mean(yards_gained), pass_plays_1=n())  
    pass3<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb, complete_pass==1, yards_gained>pass_33, yards_gained<=pass_67) %>% summarize(pass_ypc_2=mean(yards_gained), pass_plays_2=n())
    pass4<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb, complete_pass==1, yards_gained>pass_67) %>% summarize(pass_ypc_3=mean(yards_gained), pass_plays_3=n())
    pass5<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb, sack==1) %>% summarize(sack_ypc=mean(yards_gained), sack_count=n())
    pass6<-pbp_data_2018_rp %>% filter(pass == 1, passer_player_name == qb, qb_scramble==1) %>% summarize(scramble_ypc=mean(yards_gained), scramble_count=n())
  }

## Set up matrix with scenarios to test
  scenarios <- matrix(data =NA, ncol=7,nrow=13)
  colnames(scenarios)<- c("TimeSecs_Remaining","yrdline100","down","yards_to_go","GoalToGo","log_ydstogo","Under_TwoMinute_Warning")
  rownames(scenarios)<- c("Base", "Run1", "Run2", "Run3", "Fumble", "Pass1", "Pass2", "Pass3", "Incomplete", "Interception", "Sack", "Scramble", "Penalty")

## Scenarios will not include Under 2min, Goal To Go, or Time impacts
  scenarios[ ,"Under_TwoMinute_Warning"]<-0
  scenarios[ ,"TimeSecs_Remaining"]<-1800
  scenarios[ ,"GoalToGo"]<-0

  scenarios["Base","yrdline100"]<-yardline_100_input
  scenarios["Base","down"]<-down_input
  scenarios["Base","yards_to_go"]<-yards_to_go_input
  
## Set information for successful rush scenarios  
  
  if(scenarios["Base","yards_to_go"]>as.integer(rush2["rush_ypc_1"])) {
    scenarios["Run1","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(rush2["rush_ypc_1"])
    scenarios["Run1","down"]<-scenarios["Base","down"]+1
    scenarios["Run1","yards_to_go"]<-scenarios["Base","yards_to_go"]-as.integer(rush2["rush_ypc_1"])
  } else {
    scenarios["Run1","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(rush2["rush_ypc_1"])
    scenarios["Run1","down"]<-1
    scenarios["Run1","yards_to_go"]<-10
  }

  if(scenarios["Base","yards_to_go"]>as.integer(rush3["rush_ypc_2"])) {
    scenarios["Run2","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(rush3["rush_ypc_2"])
    scenarios["Run2","down"]<-scenarios["Base","down"]+1
    scenarios["Run2","yards_to_go"]<-scenarios["Base","yards_to_go"]-as.integer(rush3["rush_ypc_2"])
  } else {
    scenarios["Run2","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(rush3["rush_ypc_2"])
    scenarios["Run2","down"]<-1
    scenarios["Run2","yards_to_go"]<-10
  }
  
  if(scenarios["Base","yards_to_go"]>as.integer(rush4["rush_ypc_3"])) {
    scenarios["Run3","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(rush4["rush_ypc_3"])
    scenarios["Run3","down"]<-scenarios["Base","down"]+1
    scenarios["Run3","yards_to_go"]<-scenarios["Base","yards_to_go"]-as.integer(rush4["rush_ypc_3"])
  } else {
    scenarios["Run3","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(rush4["rush_ypc_3"])
    scenarios["Run3","down"]<-1
    scenarios["Run3","yards_to_go"]<-10
  }

## Set information for successful pass scenarios  
  
  if(scenarios["Base","yards_to_go"]>as.integer(pass2["pass_ypc_1"])) {
    scenarios["Pass1","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(pass2["pass_ypc_1"])
    scenarios["Pass1","down"]<-scenarios["Base","down"]+1
    scenarios["Pass1","yards_to_go"]<-scenarios["Base","yards_to_go"]-as.integer(pass2["pass_ypc_1"])
  } else {
    scenarios["Pass1","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(pass2["pass_ypc_1"])
    scenarios["Pass1","down"]<-1
    scenarios["Pass1","yards_to_go"]<-10
  }
  
  if(scenarios["Base","yards_to_go"]>as.integer(pass3["pass_ypc_2"])) {
    scenarios["Pass2","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(pass3["pass_ypc_2"])
    scenarios["Pass2","down"]<-scenarios["Base","down"]+1
    scenarios["Pass2","yards_to_go"]<-scenarios["Base","yards_to_go"]-as.integer(pass3["pass_ypc_2"])
  } else {
    scenarios["Pass2","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(pass3["pass_ypc_2"])
    scenarios["Pass2","down"]<-1
    scenarios["Pass2","yards_to_go"]<-10
  }
  
  if(scenarios["Base","yards_to_go"]>as.integer(pass4["pass_ypc_3"])) {
    scenarios["Pass3","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(pass4["pass_ypc_3"])
    scenarios["Pass3","down"]<-scenarios["Base","down"]+1
    scenarios["Pass3","yards_to_go"]<-scenarios["Base","yards_to_go"]-as.integer(pass4["pass_ypc_3"])
  } else {
    scenarios["Pass3","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(pass4["pass_ypc_3"])
    scenarios["Pass3","down"]<-1
    scenarios["Pass3","yards_to_go"]<-10
  }
  
## Set information for scramble scenario
  
  if(scenarios["Base","yards_to_go"]>as.integer(pass6["scramble_ypc"])) {
    scenarios["Scramble","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(pass6["scramble_ypc"])
    scenarios["Scramble","down"]<-scenarios["Base","down"]+1
    scenarios["Scramble","yards_to_go"]<-scenarios["Base","yards_to_go"]-as.integer(pass6["scramble_ypc"])
  } else {
    scenarios["Scramble","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(pass6["scramble_ypc"])
    scenarios["Scramble","down"]<-1
    scenarios["Scramble","yards_to_go"]<-10
  }
  
## Set information for other scenarios  

## Assume for fumble and interception that the there is not change in yardline
  scenarios["Fumble","yrdline100"]<-100-scenarios["Base","yrdline100"]
  scenarios["Fumble","down"]<-1
  scenarios["Fumble","yards_to_go"]<-10

  scenarios["Interception","yrdline100"]<-100-scenarios["Base","yrdline100"]
  scenarios["Interception","down"]<-1
  scenarios["Interception","yards_to_go"]<-10

  scenarios["Incomplete","yrdline100"]<-scenarios["Base","yrdline100"]
  scenarios["Incomplete","down"]<-scenarios["Base","down"]+1
  scenarios["Incomplete","yards_to_go"]<-scenarios["Base","yards_to_go"]
    
  scenarios["Sack","yrdline100"]<-scenarios["Base","yrdline100"]-as.integer(pass5["sack_ypc"])
  scenarios["Sack","down"]<-scenarios["Base","down"]+1
  scenarios["Sack","yards_to_go"]<-scenarios["Base","yards_to_go"]-as.integer(pass5["sack_ypc"])
  
## Assume for penalty it is a 10 yard penalty with an automatic first down
  scenarios["Penalty","yrdline100"]<-scenarios["Base","yrdline100"]-10
  scenarios["Penalty","down"]<-1
  scenarios["Penalty","yards_to_go"]<-10

## Change from matrix to dataframe
  scenarios <- as.data.frame(scenarios)
  
## Set log_ydstogo
  scenarios$log_ydstogo <- log(as.numeric(scenarios$yards_to_go))

# Convert the down variable to a factor:
  scenarios$down <- as.factor(scenarios$down)  
  
# Run the scenarios through the previously generated EP_MODEL to calculate probability of each next score option.
  base_ep_preds <- as.data.frame(matrix(predict(ep_model, newdata = scenarios, type = "probs"), ncol = 7))  
  colnames(base_ep_preds) <- c("No_Score", "Field_Goal", "Opp_Field_Goal", "Opp_Safety", "Opp_Touchdown",
                               "Safety", "Touchdown")
  
  
  # Rename the columns to be consistent with the nflscrapR play-by-play datasets:
  base_ep_preds <- base_ep_preds %>%
    dplyr::rename(no_score_prob = No_Score,
                  opp_fg_prob = Opp_Field_Goal,
                  opp_safety_prob = Opp_Safety,
                  opp_td_prob = Opp_Touchdown,
                  fg_prob = Field_Goal,
                  safety_prob = Safety,
                  td_prob = Touchdown) %>%
    # Calculate the expected points:
    dplyr::mutate(ep = (0 * no_score_prob) + (-fg_value * opp_fg_prob) + 
                    (-safety_value * opp_safety_prob) +
                    (-td_value * opp_td_prob) + (fg_value * fg_prob) +
                    (safety_value * safety_prob) + (td_value * td_prob))
  
  rownames(base_ep_preds)<- c("Base", "Run1", "Run2", "Run3", "Fumble", "Pass1", "Pass2", "Pass3", "Incomplete", "Interception", "Sack", "Scramble", "Penalty")
  
## Set EPA for each scenario
    base_ep_preds$epa<-NA
    base_ep_preds["Base","epa"]<-0
    base_ep_preds["Run1","epa"]<-base_ep_preds["Run1","ep"]-base_ep_preds["Base","ep"]
    base_ep_preds["Run2","epa"]<-base_ep_preds["Run2","ep"]-base_ep_preds["Base","ep"]
    base_ep_preds["Run3","epa"]<-base_ep_preds["Run3","ep"]-base_ep_preds["Base","ep"]
    base_ep_preds["Fumble","epa"]<- -base_ep_preds["Fumble","ep"]-base_ep_preds["Base","ep"]
    base_ep_preds["Pass1","epa"]<-base_ep_preds["Pass1","ep"]-base_ep_preds["Base","ep"]
    base_ep_preds["Pass2","epa"]<-base_ep_preds["Pass2","ep"]-base_ep_preds["Base","ep"]
    base_ep_preds["Pass3","epa"]<-base_ep_preds["Pass3","ep"]-base_ep_preds["Base","ep"]
    base_ep_preds["Incomplete","epa"]<-base_ep_preds["Incomplete","ep"]-base_ep_preds["Base","ep"]
    base_ep_preds["Interception","epa"]<- -base_ep_preds["Interception","ep"]-base_ep_preds["Base","ep"]
    base_ep_preds["Sack","epa"]<-base_ep_preds["Sack","ep"]-base_ep_preds["Base","ep"]
    base_ep_preds["Scramble","epa"]<-base_ep_preds["Scramble","ep"]-base_ep_preds["Base","ep"]
    base_ep_preds["Penalty","epa"]<-base_ep_preds["Penalty","ep"]-base_ep_preds["Base","ep"]
  
## Set probability for each scenario
    base_ep_preds$probability<-NA
    base_ep_preds["Base","probability"]<-0
    base_ep_preds["Run1","probability"]<- as.numeric(rush2["rush_plays_1"])/as.numeric(rush1["rush_plays"])
    base_ep_preds["Run2","probability"]<- as.numeric(rush3["rush_plays_2"])/as.numeric(rush1["rush_plays"])
    base_ep_preds["Run3","probability"]<- as.numeric(rush4["rush_plays_3"])/as.numeric(rush1["rush_plays"])
    base_ep_preds["Fumble","probability"]<- as.numeric(rush1["rush_fumble"])/as.numeric(rush1["rush_plays"])
    base_ep_preds["Pass1","probability"]<- as.numeric(pass2["pass_plays_1"])/as.numeric(pass1["pass_plays"])
    base_ep_preds["Pass2","probability"]<-as.numeric(pass3["pass_plays_2"])/as.numeric(pass1["pass_plays"])
    base_ep_preds["Pass3","probability"]<-as.numeric(pass4["pass_plays_3"])/as.numeric(pass1["pass_plays"])
    base_ep_preds["Incomplete","probability"]<-as.numeric(pass1["inc_pass"])/as.numeric(pass1["pass_plays"])
    base_ep_preds["Interception","probability"]<- as.numeric(pass1["int"])/as.numeric(pass1["pass_plays"])
    base_ep_preds["Sack","probability"]<-as.numeric(pass5["sack_count"])/as.numeric(pass1["pass_plays"])
    base_ep_preds["Scramble","probability"]<-as.numeric(pass6["scramble_count"])/as.numeric(pass1["pass_plays"])
    base_ep_preds["Penalty","probability"]<- 1-base_ep_preds["Pass1","probability"]-base_ep_preds["Pass2","probability"]-base_ep_preds["Pass3","probability"]-base_ep_preds["Incomplete","probability"]-base_ep_preds["Interception","probability"]-base_ep_preds["Sack","probability"]-base_ep_preds["Scramble","probability"]
    
Run_weighted_EPA <- base_ep_preds["Run1","probability"]*base_ep_preds["Run1","epa"] +
    base_ep_preds["Run2","probability"]*base_ep_preds["Run2","epa"] +
    base_ep_preds["Run3","probability"]*base_ep_preds["Run3","epa"] +
    base_ep_preds["Fumble","probability"]*base_ep_preds["Fumble","epa"]
Pass_weighted_EPA <- base_ep_preds["Pass1","probability"]*base_ep_preds["Pass1","epa"] +
  base_ep_preds["Pass2","probability"]*base_ep_preds["Pass2","epa"] +
  base_ep_preds["Pass3","probability"]*base_ep_preds["Pass3","epa"] +
  base_ep_preds["Incomplete","probability"]*base_ep_preds["Incomplete","epa"] +
  base_ep_preds["Interception","probability"]*base_ep_preds["Interception","epa"] +
  base_ep_preds["Sack","probability"]*base_ep_preds["Sack","epa"] +
  base_ep_preds["Scramble","probability"]*base_ep_preds["Scramble","epa"] +
  base_ep_preds["Penalty","probability"]*base_ep_preds["Penalty","epa"]

c(Run_weighted_EPA,Pass_weighted_EPA)
}
