# Script name: script_name
# Project: project
# Script purpose: script_purpose
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: date_created
# Last Modified Date: last_modified_date
# 
# Notes: 

# Get subject ID number

library("tidyverse")
library("here")
library("rio")


# Read biographic data.
d1 <- rio::import(
  # here::here("data", "raw", "thesis_marta", "ma_va_1998_07_04_538_d", "biographical_data", "0GLB5VNRFDP_data_biographic.csv")
  here::here("data", "raw", "thesis_marta", "fr_va_1960_08_18_538_u", "biographical_data", "5X8CWN1NBBX_data_biographic.csv")
)

subject_id_string <- d1$subject_id_string
subject_id_number <- d1$subject_id_number

# Read calibration data.
d2 <- rio::import(
  # here::here("data", "raw", "thesis_marta", "ma_va_1998_07_04_538_d", "calibration_data", "0GLB5VNRFDP_data_calibration.csv")
  here::here("data", "raw", "thesis_marta", "fr_va_1960_08_18_538_u", "calibration_data", "5X8CWN1NBBX_data_calibration.csv")
)

# If ratio < threshold1, then fixation is to the left.
# If ratio > threshold3, then fixation is to the right.
# Otherwise, fixstion is to the center.
threshold1 <- d2$left_side_left_eye_ratio
threshold2 <- d2$left_side_right_eye_ratio
threshold3 <- d2$right_side_left_eye_ratio
threshold4 <- d2$right_side_right_eye_ratio

# Get eye-tracking data
# dir <- here("data", "raw", "thesis_marta", "ma_va_1998_07_04_538_d", "eye_tracking_data")
dir <- here("data", "raw", "thesis_marta", "fr_va_1960_08_18_538_u", "eye_tracking_data")

file_names <- as.character(list.files(path=dir))
n_files <- length(file_names)
n_files

d_list <- list()
  
for (i in 1:n_files) {
  
  d  <- rio::import(here(dir, file_names[i]))
  
  # For each trial there is a file, with variable number of rows. With reference
  # 
  where_fixates_most <- case_when(
    d$ratio < threshold1 ~ "sx",
    d$ratio > threshold3 ~ "dx",
    .default = "center"
  )
  
  prop_dx_fixation <- sum(where_fixates_most == "dx") / length(where_fixates_most)
  prop_sx_fixation <- sum(where_fixates_most == "sx") / length(where_fixates_most)
  prop_center_fixation <- sum(where_fixates_most == "center") / length(where_fixates_most)
  
  # trial: split string of filename according to underscore
  foo <- str_split(file_names[i], "_")
  # select the third element and remove the first character
  trial <- as.numeric(substring(foo[[1]][3], 2))
 
  # block
  block <- as.numeric(substring(foo[[1]][2], 2))
  
  stim_left <- foo[[1]][4]
  stim_left_img_number <- as.numeric(foo[[1]][5])
  stim_right <- foo[[1]][6]
  stim_right_img_number <- as.numeric(foo[[1]][7])
  
  subject_id_number2 <- foo[[1]][1]
  
  mydf <- data.frame(
    prop_dx_fixation, prop_sx_fixation, prop_center_fixation, 
    block, trial, stim_left, stim_left_img_number, stim_right,
    stim_right_img_number,
    subject_id_number2
  )
  
  d_list[[i]] <- mydf
  
  rm(mydf)
}
  
# convert list into data.frame
df <- do.call(rbind.data.frame, d_list)
df

# saveRDS(df, here("data", "processed", "prl", STIMULUS, "patients_with_psychtoolkit_code.rds"))

mean(df$prop_dx_fixation)
mean(df$prop_sx_fixation)
mean(df$prop_center_fixation)


## Read PRL data

# Get eye-tracking data
# dir <- here::here("data", "raw", "thesis_marta", "ma_va_1998_07_04_538_d", "prl_task_data")
dir <- here("data", "raw", "thesis_marta", "fr_va_1960_08_18_538_u", "prl_task_data")

file_prl_names <- as.character(list.files(path=dir))
n_files_prl <- length(file_prl_names)


prl_df <- rbind(
  rio::import(here(dir, file_prl_names[1])),
  rio::import(here(dir, file_prl_names[2])),
  rio::import(here(dir, file_prl_names[3])),
  rio::import(here(dir, file_prl_names[4])),
  rio::import(here(dir, file_prl_names[5])),
  rio::import(here(dir, file_prl_names[6]))
)

tot_df <- full_join(df, prl_df, by = "trial")

tot_df$rt <- tot_df$end_reaction_time - tot_df$start_reaction_time

# Folder orange: neutral
# Folder blue: IAPS negative images
tot_df$is_negative_img_chosen <- ifelse(
  tot_df$stimulus_choice == "blue", 1, 0
)

tot_df$feedback <- ifelse(
  tot_df$outcome == "euro", 1, 0
)

mean(tot_df$feedback)

left_img <- str_split(tot_df$left_img_name, "_")
tot_df$image_on_left <- do.call(rbind.data.frame, left_img)[, 1]

right_img <- str_split(tot_df$right_img_name, "_")
tot_df$image_on_right <- do.call(rbind.data.frame, right_img)[, 1]

tot_df |> 
  group_by(stim_left, stim_right) |> 
  summarize(
    avg_left = mean(prop_sx_fixation),
    avg_right = mean(prop_dx_fixation)
  )

tot_df$is_preference_right <- tot_df$prop_dx_fixation - tot_df$prop_sx_fixation

fm <- lm(
  is_preference_right ~ stim_right, data = tot_df
)
summary(fm)


tot_df |> 
  group_by(location_choice) |> 
  summarize(
    avg_left = mean(prop_sx_fixation),
    avg_right = mean(prop_dx_fixation)
  )


mean(tot_df$feedback)

