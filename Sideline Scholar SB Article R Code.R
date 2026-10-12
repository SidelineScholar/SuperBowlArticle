
###############################################
### NFL Super Bowl Analysis
###############################################

library(nflfastR)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(grid)
library(cowplot)

###############################################
# 1. Load data
###############################################

pbp <- load_pbp(2025:2025)

###############################################
# 2. Filter to offensive plays
###############################################

plays <- pbp %>%
  filter(play_type %in% c("pass", "run"))

###############################################
# 3. Unified direction
###############################################

plays <- plays %>%
  mutate(
    direction = case_when(
      play_type == "pass" & pass_location %in% c("left","middle","right") ~ pass_location,
      play_type == "run"  & run_location  %in% c("left","middle","right") ~ run_location,
      TRUE ~ NA_character_
    )
  )

###############################################
# 4. Coach variables
###############################################

plays <- plays %>%
  mutate(
    off_coach = case_when(
      posteam_type == "home" ~ home_coach,
      posteam_type == "away" ~ away_coach,
      TRUE ~ NA_character_
    ),
    def_coach = case_when(
      defteam == home_team ~ home_coach,
      defteam == away_team ~ away_coach,
      TRUE ~ NA_character_
    ),
    coach_team = paste(off_coach, posteam, sep = " - ")
  )

###############################################
# 5. Raw depth buckets
###############################################

plays <- plays %>%
  mutate(
    pass_depth_raw = case_when(
      play_type == "pass" & !is.na(air_yards) & air_yards >= 0 & air_yards <= 4  ~ "short",
      play_type == "pass" & !is.na(air_yards) & air_yards >= 5  & air_yards <= 15 ~ "intermediate",
      play_type == "pass" & !is.na(air_yards) & air_yards >= 16                  ~ "deep",
      TRUE ~ NA_character_
    ),
    run_depth_raw = case_when(
      play_type == "run" & !is.na(yards_gained) & yards_gained <= 3                 ~ "short",
      play_type == "run" & !is.na(yards_gained) & yards_gained >= 4 & yards_gained <= 7 ~ "intermediate",
      play_type == "run" & !is.na(yards_gained) & yards_gained >= 8                 ~ "deep",
      TRUE ~ NA_character_
    ),
    depth_raw = if_else(play_type == "pass", pass_depth_raw, run_depth_raw)
  )

###############################################
# 6. Coach-specific median bucket
###############################################

coach_medians <- plays %>%
  filter(!is.na(depth_raw), !is.na(off_coach)) %>%
  group_by(off_coach) %>%
  summarise(
    median_bucket = names(which.max(table(depth_raw))),
    .groups = "drop"
  )

plays <- plays %>%
  left_join(coach_medians, by = "off_coach") %>%
  mutate(
    depth = if_else(is.na(depth_raw), median_bucket, depth_raw)
  ) %>%
  select(-depth_raw, -median_bucket, -pass_depth_raw, -run_depth_raw)

###############################################
# 7. Quadrant label
###############################################

plays <- plays %>%
  mutate(
    quadrant = if_else(
      !is.na(direction) & !is.na(depth),
      paste(direction, depth, sep = "_"),
      NA_character_
    )
  )


          ###############################################
          ### 0. Identify the 2025 Super Bowl (Week 22)
          ###############################################

sb_plays <- pbp %>%
  filter(season == 2025, week == 22)

# Teams in the Super Bowl
sb_teams <- sb_plays %>%
  distinct(home_team, away_team)

# Coaches in the Super Bowl (must use `plays`, not `pbp`)
sb_coaches <- plays %>%
  filter(season == 2025, week == 22) %>%
  distinct(off_coach) %>%
  pull()

###############################################
### 1. Filter OUT the Super Bowl (pre-SB data)
###    Use ONLY raw 2025 plays (no smoothing)
###############################################

plays_preSB <- plays %>%
  filter(season == 2025, week != 22)

###############################################
### 2. Filter to only the two SB offensive coaches
###############################################

coach_plays <- plays_preSB %>%
  filter(off_coach %in% sb_coaches)

###############################################
### 3. Per-game run/pass tendencies (raw 2025)
###############################################

coach_game_tendencies <- coach_plays %>%
  group_by(off_coach, game_id) %>%
  summarise(
    pass_rate = mean(play_type == "pass", na.rm = TRUE),
    run_rate  = mean(play_type == "run",  na.rm = TRUE),
    plays = n(),
    .groups = "drop"
  )

###############################################
### 4. Per-down run/pass tendencies (raw 2025)
###############################################

coach_down_tendencies <- coach_plays %>%
  group_by(off_coach, down) %>%
  summarise(
    pass_rate = mean(play_type == "pass", na.rm = TRUE),
    run_rate  = mean(play_type == "run",  na.rm = TRUE),
    plays = n(),
    .groups = "drop"
  )

###############################################
### 5. Direction tendencies (left/middle/right)
###    Raw counts only, no weights
###############################################

coach_direction_tendencies <- coach_plays %>%
  filter(!is.na(direction)) %>%
  group_by(off_coach, direction) %>%
  summarise(
    freq = n(),
    .groups = "drop"
  ) %>%
  group_by(off_coach) %>%
  mutate(
    prob = freq / sum(freq)
  ) %>%
  ungroup()

###############################################
### 6. Depth tendencies (short/intermediate/deep)
###    Raw counts only, no weights
###############################################

coach_depth_tendencies <- coach_plays %>%
  filter(!is.na(depth)) %>%
  group_by(off_coach, depth) %>%
  summarise(
    freq = n(),
    .groups = "drop"
  ) %>%
  group_by(off_coach) %>%
  mutate(
    prob = freq / sum(freq)
  ) %>%
  ungroup()

###############################################
### 7. Quadrant tendencies (direction × depth)
###    Raw counts only, no weights
###############################################

coach_quadrant_tendencies <- coach_plays %>%
  filter(!is.na(quadrant)) %>%
  group_by(off_coach, quadrant) %>%
  summarise(
    freq = n(),
    .groups = "drop"
  ) %>%
  group_by(off_coach) %>%
  mutate(
    prob = freq / sum(freq)
  ) %>%
  ungroup()


###############################################
### Defensive Strengths by Defensive Coach
### Using all pre-Super Bowl 2025 plays
###############################################

# Identify the two Super Bowl defensive coaches
sb_def_coaches <- plays %>%
  filter(season == 2025, week == 22) %>%
  distinct(def_coach) %>%
  pull()

###############################################
### 1. Filter defensive plays for these coaches
###    Raw 2025, pre-SB only
###############################################

def_plays <- plays_preSB %>%
  filter(def_coach %in% sb_def_coaches)

###############################################
### 2. Overall defensive performance (by play type)
###    Success allowed computed within play type
###############################################

defense_overall <- def_plays %>%
  group_by(def_coach, play_type) %>%
  summarise(
    success_allowed = mean(success == 1, na.rm = TRUE),
    epa_allowed = mean(epa, na.rm = TRUE),
    explosive_allowed = mean(yards_gained >= 15, na.rm = TRUE),
    plays = n(),
    .groups = "drop"
  )

###############################################
### 3. Defensive performance by down and play type
###############################################

defense_by_down <- def_plays %>%
  group_by(def_coach, down, play_type) %>%
  summarise(
    success_allowed = mean(success == 1, na.rm = TRUE),
    epa_allowed = mean(epa, na.rm = TRUE),
    explosive_allowed = mean(yards_gained >= 15, na.rm = TRUE),
    plays = n(),
    .groups = "drop"
  )

###############################################
### 4. Defensive direction tendencies allowed
###    Raw counts only, no weights
###############################################

defense_direction <- def_plays %>%
  filter(!is.na(direction)) %>%
  group_by(def_coach, direction) %>%
  summarise(
    freq = n(),
    .groups = "drop"
  ) %>%
  group_by(def_coach) %>%
  mutate(
    prob = freq / sum(freq)
  ) %>%
  ungroup()

###############################################
### 5. Defensive depth tendencies allowed
###    Raw counts only, no weights
###############################################

defense_depth <- def_plays %>%
  filter(!is.na(depth)) %>%
  group_by(def_coach, depth) %>%
  summarise(
    freq = n(),
    .groups = "drop"
  ) %>%
  group_by(def_coach) %>%
  mutate(
    prob = freq / sum(freq)
  ) %>%
  ungroup()

###############################################
### 6. Defensive quadrant tendencies allowed
###    Raw counts only, no weights
###############################################

defense_quadrant <- def_plays %>%
  filter(!is.na(quadrant)) %>%
  group_by(def_coach, quadrant) %>%
  summarise(
    freq = n(),
    .groups = "drop"
  ) %>%
  group_by(def_coach) %>%
  mutate(
    prob = freq / sum(freq)
  ) %>%
  ungroup()


                  ##############################
                      ### Visualizations CODE
                  ##############################

library(tidyverse)
library(ggplot2)
library(extrafont)

# (Run font_import/loadfonts once per machine/session as needed)
# extrafont::font_import(prompt = FALSE)
extrafont::loadfonts(device = "win")

        ###############################################
          ### 1. OFFENSIVE TENDENCIES VISUALS
        ###############################################

###############################################
### 1a. Run/Pass Tendencies  (Pre-SB 2025)
###############################################

offense_rp <- coach_plays %>%
  group_by(off_coach, play_type) %>%
  summarise(
    plays = n(),
    epa = mean(epa, na.rm = TRUE),
    success = mean(success == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(off_coach) %>%
  mutate(
    pct = plays / sum(plays),
    pct_label = scales::percent(pct, accuracy = 1)
  ) %>%
  ungroup()

ggplot(offense_rp, aes(x = off_coach, y = pct, fill = play_type)) +
  geom_col(position = "stack", color = "white") +
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5,
    family = "Times New Roman"
  ) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  labs(
    title = "Offensive Run/Pass Tendencies (Pre-Super Bowl, 2025)",
    x = "Coach",
    y = NULL,
    fill = "Play Type"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times New Roman", face = "bold"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(family = "Times New Roman")
  )

            ############################################################
              ### COACH RUN/PASS COUNTS + PERCENTAGES + OVERALL SUCCESS
                ### (SEASON, PRE-SB, ONLY SB COACHES)
            ############################################################

sb_coaches <- c("Mike Macdonald", "Mike Vrabel")

coach_plays_preSB <- plays %>%
  filter(
    season == 2025,
    week != 22,                         # exclude Super Bowl
    off_coach %in% sb_coaches           # only Macdonald & Vrabel
  ) %>%
  group_by(off_coach, play_type) %>%
  summarise(
    plays = n(),
    successful_plays = sum(success == 1, na.rm = TRUE),
    success_rate = successful_plays / plays,
    .groups = "drop"
  ) %>%
  group_by(off_coach) %>%
  mutate(
    total_plays = sum(plays),
    pct = plays / total_plays,
    overall_success_rate = sum(successful_plays) / sum(plays),
    pct_label = scales::percent(pct, accuracy = 1),
    success_label = scales::percent(success_rate, accuracy = 1),
    overall_success_label = scales::percent(overall_success_rate, accuracy = 1)
  ) %>%
  arrange(off_coach, desc(play_type))

coach_plays_preSB


library(gt)
coach_plays_preSB <- tibble(
  `Offensive Coach` = c("Mike Macdonald", "Mike Macdonald", "Mike Vrabel", "Mike Vrabel"),
  `Play Type` = c("Pass", "Run", "Pass", "Run"),
  `Plays` = c(564, 550, 643, 557),
  `% of Total` = c("51%", "49%", "54%", "46%"),
  `Success Rate` = c("52%", "41%", "51%", "41%")
)

tbl <- coach_plays_preSB %>%
  gt() %>%
  tab_options(
    table.border.top.color = "white",
    table.border.bottom.color = "white",
    data_row.padding = px(6),
    heading.align = "left"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "#0B3D2E"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "#E0E0E0"),
    locations = cells_body()
  )

gtsave(tbl, "sideline_scholar_table.png")


###############################################
### 1b. Offensive Success Rate (Run vs Pass)
###############################################

success_rp <- coach_plays %>%
  group_by(off_coach, play_type) %>%
  summarise(
    success_rate = mean(success == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    success_label = scales::percent(success_rate, accuracy = 1)
  )

ggplot(success_rp, aes(x = play_type, y = success_rate, fill = play_type)) +
  geom_col(color = "white") +
  geom_text(
    aes(label = success_label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5,
    family = "Times New Roman"
  ) +
  facet_wrap(~ off_coach) +
  labs(
    title = "Offensive Success Rate by Coach (Run vs Pass, 2025 Pre-SB)",
    x = "Play Type",
    y = "Success Rate"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times New Roman", face = "bold"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"),
    axis.text.x = element_text(family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    strip.text = element_text(family = "Times New Roman", face = "bold"),
    legend.position = "none"
  )


          ###############################################
          ### 2. DEFENSIVE STRENGTH VISUALS
          ###############################################

###############################################
### 2a. Defensive Success Rate (Run vs Pass)
###     Defense-oriented: success = defense stops offense
###############################################

library(dplyr)
library(ggplot2)
library(scales)

# 1. Create a defensive success indicator
# success == 1 means OFFENSE succeeded
# So DEFENSE succeeded when success == 0
def_plays <- def_plays %>%
  mutate(def_success = ifelse(success == 1, 0, 1))

# 2. Compute defensive success rate by coach and play type
defense_success <- def_plays %>%
  group_by(def_coach, play_type) %>%
  summarise(
    def_success_rate = mean(def_success, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    success_label = scales::percent(def_success_rate, accuracy = 0.1),
    play_type = factor(play_type, levels = c("run", "pass"))
  )

# 3. Visual: Defensive Success Rate (higher = better defense)
ggplot(defense_success, aes(x = play_type, y = def_success_rate, fill = play_type)) +
  geom_col(color = "white") +
  geom_text(
    aes(label = success_label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5,
    family = "Times New Roman"
  ) +
  facet_wrap(~ def_coach) +
  labs(
    title = "Defensive Success Rate (Run vs Pass, 2025 Pre-SB)",
    x = "Play Type",
    y = "Defensive Success Rate"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times New Roman", face = "bold"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"),
    axis.text.x = element_text(family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    strip.text = element_text(family = "Times New Roman", face = "bold"),
    legend.position = "none"
  )

###############################################
### 2b. Defensive Success Table (Counts + Rates)
###############################################

def_success_table <- def_plays %>%
  group_by(def_coach, play_type) %>%
  summarise(
    total_plays = n(),
    defensive_successes = sum(def_success == 1, na.rm = TRUE),
    defensive_failures = sum(def_success == 0, na.rm = TRUE),
    def_success_rate = defensive_successes / total_plays,
    .groups = "drop"
  ) %>%
  mutate(
    def_success_rate_label = scales::percent(def_success_rate, accuracy = 0.1)
  ) %>%
  arrange(def_coach, play_type)

def_success_table


        ################################### NOw Look At the Super Bowl #######
        #######################
        #############



###############################################
### SUPER BOWL ONLY ANALYSIS (2025, Week 22)
### Using game_half (Half1 / Half2)
###############################################

# Filter Super Bowl plays
sb_plays <- plays %>%
  filter(season == 2025, week == 22)

# Add half indicator using game_half
sb_plays <- sb_plays %>%
  mutate(
    half = case_when(
      game_half == "Half1" ~ "1st Half",
      game_half == "Half2" ~ "2nd Half",
      TRUE ~ NA_character_
    )
  )

###########################################################
### 1. OFFENSIVE SUPER BOWL STATS (OVERALL)
###########################################################

sb_offense <- sb_plays %>%
  group_by(off_coach, play_type) %>%
  summarise(
    plays = n(),
    pass_rate = mean(play_type == "pass"),
    run_rate = mean(play_type == "run"),
    success_rate = mean(success == 1, na.rm = TRUE),
    epa = mean(epa, na.rm = TRUE),
    explosive = mean(yards_gained >= 15, na.rm = TRUE),
    .groups = "drop"
  )

###########################################################
### 2. OFFENSIVE SUPER BOWL STATS (BY HALF)
###########################################################

sb_offense_half <- sb_plays %>%
  group_by(off_coach, half, play_type) %>%
  summarise(
    plays = n(),
    pass_rate = mean(play_type == "pass"),
    run_rate = mean(play_type == "run"),
    success_rate = mean(success == 1, na.rm = TRUE),
    epa = mean(epa, na.rm = TRUE),
    explosive = mean(yards_gained >= 15, na.rm = TRUE),
    .groups = "drop"
  )

###########################################################
### 3. DEFENSIVE SUPER BOWL STATS (OVERALL)
###########################################################

sb_defense <- sb_plays %>%
  group_by(def_coach, play_type) %>%
  summarise(
    plays = n(),
    success_allowed = mean(success == 1, na.rm = TRUE),
    epa_allowed = mean(epa, na.rm = TRUE),
    explosive_allowed = mean(yards_gained >= 15, na.rm = TRUE),
    .groups = "drop"
  )

###########################################################
### 4. DEFENSIVE SUPER BOWL STATS (BY HALF)
###########################################################

sb_defense_half <- sb_plays %>%
  group_by(def_coach, half, play_type) %>%
  summarise(
    plays = n(),
    success_allowed = mean(success == 1, na.rm = TRUE),
    epa_allowed = mean(epa, na.rm = TRUE),
    explosive_allowed = mean(yards_gained >= 15, na.rm = TRUE),
    .groups = "drop"
  )

###########################################################
### OPTIONAL: QUICK SANITY CHECKS
###########################################################

# Offensive plays by coach
sb_plays %>% count(off_coach)

# Defensive plays by coach
sb_plays %>% count(def_coach)

# Plays by half
sb_plays %>% count(half)



            ###################### Now, SB Visuals given the data prior
            ##########
            ####

###############################################
### SUPER BOWL ONLY VISUALS (2025, Week 22)
###############################################

library(tidyverse)
library(ggplot2)
library(extrafont)

#extrafont::loadfonts(device = "win")

###########################################################
### 1. OFFENSIVE RUN/PASS TENDENCIES — SUPER BOWL ONLY
###########################################################


sb_offense_rp <- sb_plays %>%
  group_by(off_coach) %>%
  mutate(total_plays = n()) %>%
  group_by(off_coach, play_type) %>%
  summarise(
    plays = n(),
    pct = plays / first(total_plays),
    pct_label = scales::percent(pct, accuracy = 1),
    .groups = "drop"
  )

ggplot(sb_offense_rp, aes(x = off_coach, y = pct, fill = play_type)) +
  geom_col(position = "stack", color = "white") +
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5,
    family = "Times New Roman"
  ) +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  labs(
    title = "Super Bowl Run/Pass Tendencies (2025)",
    x = "Coach",
    y = NULL,
    fill = "Play Type"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(family = "Times New Roman"),
    legend.title = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman")
  )


###########################################################
### 2. OFFENSIVE SUCCESS RATE — SUPER BOWL ONLY
###########################################################

sb_offense_success <- sb_plays %>%
  group_by(off_coach, play_type) %>%
  summarise(
    success_rate = mean(success == 1, na.rm = TRUE),
    success_label = scales::percent(success_rate, accuracy = 1),
    .groups = "drop"
  )

ggplot(sb_offense_success, aes(x = play_type, y = success_rate, fill = play_type)) +
  geom_col(color = "white") +
  geom_text(
    aes(label = success_label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5,
    family = "Times New Roman"
  ) +
  facet_wrap(~ off_coach) +
  labs(
    title = "Super Bowl Offensive Success Rate (Run vs Pass, 2025)",
    x = "Play Type",
    y = "Success Rate"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    strip.text = element_text(family = "Times New Roman", face = "bold"),
    legend.position = "none"
  )


      ############################################################
        ### SUPER BOWL OFFENSIVE IDENTITY TABLE (GAME ONLY)
        ### Play counts, % of total, success rate by play type
      ############################################################

sb_offense_table <- sb_plays %>%
  group_by(off_coach) %>%
  mutate(total_plays = n()) %>%
  group_by(off_coach, play_type) %>%
  summarise(
    plays = n(),
    pct = plays / first(total_plays),
    success_rate = mean(success == 1, na.rm = TRUE),
    pct_label = scales::percent(pct, accuracy = 1),
    success_label = scales::percent(success_rate, accuracy = 1),
    .groups = "drop"
  ) %>%
  arrange(off_coach, desc(play_type))

sb_offense_table



          ###########################################################
              ### 3. DEFENSIVE SUCCESS ALLOWED — SUPER BOWL ONLY
          ###########################################################

sb_defense_success <- sb_plays %>%
  group_by(def_coach, play_type) %>%
  summarise(
    success_allowed = mean(success == 1, na.rm = TRUE),
    success_label = scales::percent(success_allowed, accuracy = 1),
    .groups = "drop"
  ) %>%
  mutate(play_type = factor(play_type, levels = c("run", "pass")))

ggplot(sb_defense_success, aes(x = play_type, y = success_allowed, fill = play_type)) +
  geom_col(color = "white") +
  geom_text(
    aes(label = success_label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5,
    family = "Times New Roman"
  ) +
  facet_wrap(~ def_coach) +
  labs(
    title = "Super Bowl Defensive Success Allowed (Run vs Pass, 2025)",
    x = "Play Type",
    y = "Success Rate Allowed"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    strip.text = element_text(family = "Times New Roman", face = "bold"),
    legend.position = "none"
  )





            ########### Now break it down by half
            ###########

############################################################
### SUPER BOWL OFFENSIVE OUTPUT — BY HALF (CORRECTED)
############################################################

sb_offense_halves <- sb_plays %>%
  mutate(
    half = case_when(
      game_half == "Half1" ~ "1st Half",
      game_half == "Half2" ~ "2nd Half",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(off_coach, half) %>%
  mutate(total_plays = n()) %>%
  group_by(off_coach, half, play_type) %>%
  summarise(
    plays = n(),
    pct = plays / first(total_plays),
    success_rate = mean(success == 1, na.rm = TRUE),
    pct_label = scales::percent(pct, accuracy = 1),
    success_label = scales::percent(success_rate, accuracy = 1),
    .groups = "drop"
  ) %>%
  arrange(off_coach, half, desc(play_type))

sb_offense_halves




###########################################################
### 6. DEFENSIVE SUCCESS ALLOWED — BY HALF
###########################################################

sb_defense_success_half <- sb_plays %>%
  group_by(def_coach, half, play_type) %>%
  summarise(
    success_allowed = mean(success == 1, na.rm = TRUE),
    success_label = scales::percent(success_allowed, accuracy = 1),
    .groups = "drop"
  )

ggplot(sb_defense_success_half, aes(x = play_type, y = success_allowed, fill = play_type)) +
  geom_col(color = "white") +
  geom_text(
    aes(label = success_label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5,
    family = "Times New Roman"
  ) +
  facet_grid(def_coach ~ half) +
  labs(
    title = "Super Bowl Defensive Success Allowed by Half (2025)",
    x = "Play Type",
    y = "Success Rate Allowed"
  ) +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(family = "Times New Roman"),
    axis.text.y = element_text(family = "Times New Roman"),
    strip.text = element_text(family = "Times New Roman", face = "bold"),
    legend.position = "none"
  )



     

      ###################################################################
      ###################################################################
           ############## Vrabel Runs Over Time 
            #############

library(lubridate)
library(stringr) 

#install.packages("extrafont")
#extrafont::font_import(prompt = FALSE)
#extrafont::loadfonts(device = "win")

# Filter to Vrabel offensive plays
vrabel_plays <- sb_plays %>%
  filter(off_coach == "Mike Vrabel")

# Convert MM:SS to seconds remaining in quarter
vrabel_plays <- vrabel_plays %>%
  mutate(
    # Split "MM:SS" into minutes + seconds
    min = as.numeric(str_extract(time, "^[0-9]{1,2}(?=:)")),
    sec = as.numeric(str_extract(time, "(?<=:)[0-9]{2}")),
    sec_remaining_qtr = min * 60 + sec
  )

# Convert quarter + time into continuous game time (seconds elapsed)
# Q1 = 0–900, Q2 = 900–1800, Q3 = 1800–2700, Q4 = 2700–3600
vrabel_plays <- vrabel_plays %>%
  mutate(
    game_seconds = case_when(
      qtr == 1 ~ 900 - sec_remaining_qtr,
      qtr == 2 ~ 900 + (900 - sec_remaining_qtr),
      qtr == 3 ~ 1800 + (900 - sec_remaining_qtr),
      qtr == 4 ~ 2700 + (900 - sec_remaining_qtr)
    )
  )

# Create cumulative run count
vrabel_plays <- vrabel_plays %>%
  arrange(game_seconds) %>%
  mutate(
    run_flag = ifelse(play_type == "run", 1, 0),
    cumulative_runs = cumsum(run_flag)
  )


              #####library(dplyr)
            ####library(ggplot2)library(dplyr)

library(dplyr)
library(ggplot2)

# --- Summaries for Vrabel: Run Count + Run Success Rate by Quarter ---
vrabel_runs_qtr <- sb_plays %>%
  filter(
    off_coach == "Mike Vrabel",
    play_type == "run"
  ) %>%
  group_by(qtr) %>%
  summarise(
    run_count    = n(),
    successes    = sum(success == 1, na.rm = TRUE),
    success_rate = successes / run_count,
    .groups = "drop"
  )

# --- Visualization: Bars (Run Count) + Line (Success Rate) ---
ggplot(vrabel_runs_qtr, aes(x = factor(qtr))) +
  
  # Bars: forest green fill + gold outline + slight transparency
  geom_col(
    aes(y = run_count),
    fill = alpha("#1f4d2e", 0.97),   # subtle transparency
    color = "#EFBF04",
    linewidth = 0.8,
    width = 0.6
  ) +
  
  # Data labels above bars
  geom_text(
    aes(y = run_count, label = run_count),
    vjust = -0.6,
    size = 4,
    family = "Times New Roman"
  ) +
  
  # Line: success rate (scaled) in #C3B091 with subtle shadow
  geom_line(
    aes(y = success_rate * 10, group = 1),
    color = "#C3B091",
    linewidth = 1.2
  ) +
  geom_line(
    aes(y = success_rate * 10, group = 1),
    color = "#A8977A",   # faint shadow stroke
    linewidth = 2,
    alpha = 0.35
  ) +
  geom_point(
    aes(y = success_rate * 10),
    color = "#C3B091",
    size = 1.5,
    stroke = 0.8
  ) +
  
  # Y-axis: primary (run count) + secondary (success rate)
  scale_y_continuous(
    name = "Run Count",
    limits = c(0, 10.5),
    breaks = seq(0, 10, by = 2),
    sec.axis = sec_axis(
      ~ . / 10,
      name   = "Run Success Rate",
      breaks = seq(0, 1, by = 0.2),
      labels = scales::percent_format(accuracy = 1)
    )
  ) +
  
  labs(
    title    = "Vrabel: Super Bowl Run Calls and Success Rate by Quarter",
    subtitle = "Bars Show Run Volume; Line Shows Run Efficiency",
    x        = "Quarter"
  ) +
  
  theme_minimal(base_family = "Times New Roman") +
  theme(
    # Alabaster background
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    # Title + subtitle
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.7,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 14,
      hjust = 0.5,
      margin = margin(b = 14)
    ),
    
    axis.title.x       = element_text(size = 14, margin = margin(t = 12)),
    axis.title.y       = element_text(size = 14, margin = margin(r = 12)),
    axis.title.y.right = element_text(size = 14, color = "#C3B091", margin = margin(l = 12)),
    axis.text.y.right  = element_text(color = "#C3B091"),
    
    # Lightened gridlines
    panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )




          ############## Macdonald SB Plays ###############
          ###############
          ##########



library(lubridate)
library(stringr) 

#install.packages("extrafont")
#extrafont::font_import(prompt = FALSE)
#extrafont::loadfonts(device = "win")

# Filter to Macdonald offensive plays
Mac_plays <- sb_plays %>%
  filter(off_coach == "Mike Macdonald")

# Convert MM:SS to seconds remaining in quarter
Mac_plays <- Mac_plays %>%
  mutate(
    min = as.numeric(str_extract(time, "^[0-9]{1,2}(?=:)")),
    sec = as.numeric(str_extract(time, "(?<=:)[0-9]{2}")),
    sec_remaining_qtr = min * 60 + sec
  )

# Convert quarter + time into continuous game time
Mac_plays <- Mac_plays %>%
  mutate(
    game_seconds = case_when(
      qtr == 1 ~ 900 - sec_remaining_qtr,
      qtr == 2 ~ 900 + (900 - sec_remaining_qtr),
      qtr == 3 ~ 1800 + (900 - sec_remaining_qtr),
      qtr == 4 ~ 2700 + (900 - sec_remaining_qtr)
    )
  )

# Create cumulative run count
Mac_plays <- Mac_plays %>%
  arrange(game_seconds) %>%
  mutate(
    run_flag = ifelse(play_type == "run", 1, 0),
    cumulative_runs = cumsum(run_flag)
  )

# --- Summaries for Macdonald: Run Count + Run Success Rate by Quarter ---
Mac_runs_qtr <- Mac_plays %>%
  filter(play_type == "run") %>%
  group_by(qtr) %>%
  summarise(
    run_count    = n(),
    successes    = sum(success == 1, na.rm = TRUE),
    success_rate = successes / run_count,
    .groups = "drop"
  )

# --- Visualization: Bars (Run Count) + Line (Success Rate) ---
ggplot(Mac_runs_qtr, aes(x = factor(qtr))) +
  
  geom_col(
    aes(y = run_count),
    fill = alpha("#1f4d2e", 0.97),
    color = "#EFBF04",
    linewidth = 0.8,
    width = 0.6
  ) +
  
  geom_text(
    aes(y = run_count, label = run_count),
    vjust = -0.6,
    size = 4,
    family = "Times New Roman"
  ) +
  
  geom_line(
    aes(y = success_rate * 10, group = 1),
    color = "#C3B091",
    linewidth = 1.2
  ) +
  geom_line(
    aes(y = success_rate * 10, group = 1),
    color = "#A8977A",
    linewidth = 2,
    alpha = 0.35
  ) +
  geom_point(
    aes(y = success_rate * 10),
    color = "#C3B091",
    size = 1.5,
    stroke = 0.8
  ) +
  
  scale_y_continuous(
    name = "Run Count",
    limits = c(0, 13),
    breaks = seq(0, 12, by = 2),
    sec.axis = sec_axis(
      ~ . / 10,
      name   = "Run Success Rate",
      breaks = seq(0, 1, by = 0.2),
      labels = scales::percent_format(accuracy = 1)
    )
  ) +
  
  labs(
    title    = "Macdonald: Super Bowl Run Calls and Success Rate by Quarter",
    subtitle = "Bars Show Run Volume; Line Shows Run Efficiency",
    x        = "Quarter"
  ) +
  
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.7,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 14,
      hjust = 0.5,
      margin = margin(b = 14)
    ),
    
    axis.title.x       = element_text(size = 14, margin = margin(t = 12)),
    axis.title.y       = element_text(size = 14, margin = margin(r = 12)),
    axis.title.y.right = element_text(size = 14, color = "#C3B091", margin = margin(l = 12)),
    axis.text.y.right  = element_text(color = "#C3B091"),
    
    panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )

# Quick check for the run data in the chart
Mac_runs_qtr


          #############################################
          #############################################
              ### Now, lets look at EPA and if Runs were really meaningful
          #############################################

library(dplyr)

# establish half distinction (this will include by half and quarter)
sb_plays <- sb_plays %>%
  mutate(
    half = ifelse(qtr %in% c(1, 2), "Half1", "Half2")
  )


# --- Function to compute quarter-by-quarter run EPA for any coach ---
run_epa_summaries <- function(data, coach_name) {
  
  # Filter to coach + run plays
  runs <- data %>%
    filter(
      off_coach == coach_name,
      play_type == "run"
    )
  
  # Quarter summary
  qtr_summary <- runs %>%
    group_by(qtr) %>%
    summarise(
      run_count     = n(),
      total_run_epa = sum(epa, na.rm = TRUE),
      avg_run_epa   = mean(epa, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Half summary
  half_summary <- runs %>%
    group_by(half) %>%
    summarise(
      run_count     = n(),
      total_run_epa = sum(epa, na.rm = TRUE),
      avg_run_epa   = mean(epa, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    quarter = qtr_summary,
    half    = half_summary
  )
}


# --- Generate tables for each coach ---
Vrabel_epa <- run_epa_summaries(sb_plays, "Mike Vrabel")
Mac_epa    <- run_epa_summaries(sb_plays, "Mike Macdonald")

# View quarter summaries
Vrabel_epa$quarter
Mac_epa$quarter

# View half summaries
Vrabel_epa$half
Mac_epa$half



      ########################### Drive by Drive Visual, Entire Game #######
      ##########################

      #########################
library(dplyr)
library(ggplot2)
library(tidyr)

# --- Choose coach here ---
coach_name <- "Mike Macdonald"   # or "Mike Macdonald"

# --- Step 1: Summarize run/pass counts and total EPA per drive ---
drive_summary <- sb_plays %>%
  filter(off_coach == coach_name) %>%
  group_by(drive) %>%
  summarise(
    total_epa   = sum(epa, na.rm = TRUE),
    run_count   = sum(play_type == "run"),
    pass_count  = sum(play_type == "pass"),
    total_plays = n(),
    .groups = "drop"
  ) %>%
  mutate(
    run_prop  = ifelse(total_plays > 0, run_count  / total_plays, 0),
    pass_prop = ifelse(total_plays > 0, pass_count / total_plays, 0)
  )

# --- Step 1B: Build drive_results CLEANLY from scratch ---
drive_results <- sb_plays %>%
  filter(off_coach == coach_name) %>%
  group_by(drive) %>%
  summarise(
    result_raw = dplyr::last(fixed_drive_result),
    .groups = "drop"
  ) %>%
  mutate(
    result = case_when(
      result_raw == "Touchdown" ~ "TD",
      result_raw == "Field goal" ~ "FG",
      result_raw == "Punt" ~ "Punt",
      result_raw == "Interception" ~ "INT",
      result_raw == "Fumble" ~ "Fumble",
      result_raw == "Turnover on downs" ~ "TOD",
      result_raw == "Missed field goal" ~ "Miss",
      result_raw == "Safety" ~ "Safety",
      result_raw == "End of half" ~ "End",
      result_raw == "End of game" ~ "End",
      result_raw == "Turnover" ~ "TO",
      result_raw == "Opp touchdown" ~ "Opp.TD",
      TRUE ~ result_raw
    ),
    result_color = case_when(
      result == "TD"      ~ "#1f4d2e",
      result == "Opp.TD"  ~ "#B22222",
      result == "FG"      ~ "#C3B091",
      result == "Punt"    ~ "#555555",
      result == "TO"      ~ "#8B0000",
      result == "TOD"     ~ "#8B0000",
      result == "Miss"    ~ "#A67C00",
      result == "Safety"  ~ "#00008B",
      result == "End"     ~ "#333333",
      TRUE ~ "black"
    )
  )

# --- Merge drive results into summary (ONE TIME ONLY) ---
drive_summary <- drive_summary %>%
  left_join(drive_results, by = "drive")

# --- Step 2: Long format + compute EPA share for each play type ---
drive_long <- drive_summary %>%
  pivot_longer(
    cols = c(run_prop, pass_prop),
    names_to = "play_type",
    values_to = "prop"
  ) %>%
  mutate(
    play_type   = recode(play_type,
                         "run_prop" = "Run",
                         "pass_prop" = "Pass"),
    epa_segment = total_epa * prop
  )

# --- Step 3: Compute halftime line position dynamically ---
half1_last_drive <- sb_plays %>%
  filter(off_coach == coach_name, game_half == "Half1") %>%
  summarise(last_drive = max(drive)) %>%
  pull(last_drive)

half1_line_x <- which(sort(unique(drive_long$drive)) == half1_last_drive) + 0.5

# --- Step 4: Styled Plot ---
ggplot(drive_long, aes(x = factor(drive), y = epa_segment, fill = play_type)) +
  
  geom_col(
    color = "#EFBF04",
    linewidth = 0.55,
    width = 0.65,
    position = "stack"
  ) +
  
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  
  geom_vline(
    xintercept = half1_line_x,
    color = "#C3B091",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  geom_text(
    data = drive_summary,
    inherit.aes = FALSE,
    aes(
      x = factor(drive),
      y = ifelse(total_epa >= 0, total_epa + 0.4, 0.4),
      label = result,
      color = result_color
    ),
    family = "Times New Roman",
    size = 3.5,
    fontface = "bold"
  ) +
  scale_color_identity() +
  
  scale_fill_manual(
    values = c("Pass" = alpha("#1f4d2e", 0.97),
               "Run"  = "#F1F0EF")
  ) +
  
  labs(
    title    = paste(coach_name, "- Full Game Drive-Level EPA"),
    subtitle = "Bar Height Shows Total Drive EPA; Fill Shows Run/Pass Allocation",
    x        = "Drive Number",
    y        = "Total EPA (Stacked by Play Type)",
    fill     = "Play Type"
  ) +
  
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.6,
      margin = margin(b = 4.5)
    ),
    plot.subtitle = element_text(
      size = 13,
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    
    axis.title.x = element_text(size = 13, margin = margin(t = 12.5), face = "bold"),
    axis.title.y = element_text(size = 13, margin = margin(r = 12.5), face = "bold"),
    
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    
    panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.1),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

