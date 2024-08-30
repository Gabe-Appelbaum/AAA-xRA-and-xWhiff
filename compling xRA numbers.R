library(tidyverse)
library(dtplyr)
library(data.table)

#loading the model
stuff_plus = readRDS("data/stuff plus model.rda")

#load in rv for use later
run_values <- read_csv("Data/rv 23.csv") %>%
      rename(val = mean)


# selecting variables i need, and converting to same name as in statcast data so that the model runs properly
AAA <- read_csv("Data/AAA_data.csv") %>%
      mutate(batter_parentOrg = case_when(
            about.halfInning == "top" ~ away_parentOrg_name,
            about.halfInning == "bottom" ~ home_parentOrg_name
            )
      ) %>%
      select(hitData.trajectory, hitData.launchSpeed, details.call.description, count.strikes.start, count.balls.start, result.event, matchup.batter.fullName, batter_parentOrg,
             release_speed = pitchData.startSpeed,
             release_extension = pitchData.extension,
             release_spin_rate = pitchData.breaks.spinRate,
             run = pitchData.breaks.breakHorizontal,
             pfx_z = pitchData.breaks.breakVertical,
             release_pos_x = pitchData.coordinates.x0,
             release_pos_z = pitchData.coordinates.z0,
             matchup.pitchHand.code,
             game_pk, atBatIndex, pitchNumber # need these to calculate if a pitch is the final pitch
             ) %>%
      filter(!result.event %in% c("Sac Bunt", "Bunt Lineout", "Bunt Pop Out", "Bunt Groundout", "Hit By Pitch", "Catcher Interference")) %>% # filter out bunts and weird events
      filter(!details.call.description %in% c("Missed Bunt", "Foul Bunt")) %>%
      filter(!hitData.trajectory %in% c("bunt_grounder", "bunt_popup", "bunt_line_drive")) %>%
      mutate(
            whiffs = case_when( # caculate whiff or not
                  details.call.description %in% c("Swinging Strike", "Swinging Strike (Blocked)", "Foul Tip") ~ 1,
                  !details.call.description %in% c("Swinging Strike", "Swinging Strike (Blocked)", "Foul Tip") ~ 0
            ),
            run = case_when(
                  matchup.pitchHand.code == "R" ~ run * -1,
                  matchup.pitchHand.code == "L" ~ run,
            ),
            release_pos_x = case_when(
                  matchup.pitchHand.code == "R" ~ release_pos_x * -1,
                  matchup.pitchHand.code == "L" ~ release_pos_x,
            )
            
                        
      ) %>%
      #tag if a pitch is the final pitch in an ab
      group_by(game_pk, atBatIndex) %>%
      mutate(is_final_pitch = pitchNumber == max(pitchNumber, na.rm = TRUE)) %>%
      ungroup()

# applying stuff+
stuff_plus_predict <- predict(
      stuff_plus, 
      data.matrix(
            AAA %>% 
                  select(
                        release_speed , 
                        run , 
                        pfx_z , 
                        release_pos_x , 
                        release_pos_z , 
                        release_spin_rate , 
                        release_extension
                        )
            )
      )

#binding it on
AAA <- cbind(AAA, stuff_plus_predict)

# filtering down to pitches with stuff+ >= 95, and then tagging each rv event
AAA_pitches_100 <- AAA %>%
      filter(stuff_plus_predict >= 0.85 * 0.1182803) %>%
      mutate(
            rv_event = # tag rv event
                  case_when(hitData.trajectory == "ground_ball" & hitData.launchSpeed < 95.0 ~ "soft_gb",
                            hitData.trajectory == "ground_ball" & hitData.launchSpeed >= 95.0 ~ "hard_gb",
                            hitData.trajectory == "fly_ball" & hitData.launchSpeed < 95.0 ~ "soft_fb",
                            hitData.trajectory == "fly_ball" & hitData.launchSpeed >= 95.0 ~ "hard_fb",
                            hitData.trajectory == "line_drive" & hitData.launchSpeed < 95.0 ~ "soft_ld",
                            hitData.trajectory == "line_drive" & hitData.launchSpeed >= 95.0 ~ "hard_ld",
                            hitData.trajectory == "popup" ~ "popup",
                            
                            details.call.description %in% 
                                  c("Called Strike", "Swinging Strike", "Swinging Strike (Blocked)", "Foul Tip") & 
                                  is_final_pitch == FALSE
                            ~ "nd_strike",
                            
                            details.call.description %in% c("Foul", "Foul Pitchout") ~ "nd_strike",
                            
                            details.call.description %in% 
                                  c("Called Strike", "Swinging Strike", "Swinging Strike (Blocked)","Foul Tip") &
                                  is_final_pitch == TRUE
                            ~ "so", 
                            
                            details.call.description %in% c("Ball", "Ball In Dirt") & is_final_pitch ~ "nd_ball",
                            details.call.description %in% c("Hit By Pitch") ~ "nd_ball",
                            details.call.description %in% c("Ball", "Ball In Dirt") &  is_final_pitch == TRUE ~ "bb",
                            result.event == "Intent Walk" ~ "bb"
                  )
      )

player_events <- AAA_pitches_100 %>% 
      filter(!is.na(rv_event)) %>% 
      # a few na's remain from pitchouts and intentional balls, but most na's are because either launch speed or launch angle didnt read
      # removing them for now, will consider an unread launch speed as average perhaps going forward?
      group_by(matchup.batter.fullName, batter_parentOrg) %>%
      summarise(soft_gb = length(which(rv_event == "soft_gb")),
                hard_gb = length(which(rv_event == "hard_gb")),
                soft_fb = length(which(rv_event == "soft_fb")),
                hard_fb = length(which(rv_event == "hard_fb")),
                soft_ld = length(which(rv_event == "soft_ld")),
                hard_ld = length(which(rv_event == "hard_ld")),
                popup = length(which(rv_event == "popup")),
                nd_strike = length(which(rv_event == "nd_strike")),
                nd_ball = length(which(rv_event == "nd_ball")),
                so = length(which(rv_event == "so")),
                bb = length(which(rv_event == "bb"))
      ) %>%
      as.data.frame() %>%
      mutate_if(is.integer, as.numeric) %>%
      mutate(
            #PA = rowSums(select(., soft_gb:popup, so, bb)),
            BBE = rowSums(select(., soft_gb:popup)),
            xRunsCon = (
                        (soft_gb * as.numeric(run_values[9,2])) + 
                        (hard_gb * as.numeric(run_values[2,2])) + 
                        (soft_fb * as.numeric(run_values[8,2])) + 
                        (hard_fb * as.numeric(run_values[1,2])) +
                        (soft_ld * as.numeric(run_values[10,2])) + 
                        (hard_ld * as.numeric(run_values[3,2])) + 
                        (popup * as.numeric(run_values[6,2]))
                        #(nd_strike * as.numeric(run_values[5,2])) + 
                        #(so * as.numeric(run_values[7,2])) +
                        #(nd_ball * as.numeric(run_values[4,2])) + 
                        #(bb * as.numeric(run_values[11,2]))
                  )
            ) 


player_events$xRA <- player_events$xRuns / player_events$PA
player_events$xRAcon <- player_events$xRunsCon / player_events$BBE

player_events <- player_events %>%
      mutate("xRAcon+" = round(100 + 10 * scale(xRAcon)))

# i have filtered down to just contact because balls and strikes and BB's and K's were being weird

xRA_100 <- player_events %>% 
      filter(PA >= 100) %>%
      mutate("xRA+" = round(100 + 10 * scale(xRA)))
 # need to make xRA+ based on mlb mean xRA in 2023, so can show where these guys would have ranked in the mlb, also consider switching to 2023 rv