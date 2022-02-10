# R Setup ----
rm(list = ls(all = TRUE))

# Load packages.

# Location
library(here)

# Data manipulation.
library(data.table)
library(dplyr)
library(stringr)
library(rio)
library(tidyverse)
library(janitor)
library(bit64)

# Plots.
library(ggplot2)
library(cowplot)

# Analyses.
library(afex)
library(lme4)
library(lmerTest)

theme_set(theme_bw())


# Load in Data for Experiment 1 ----

# Get list of file names
file_names_1 <- list.files(path = "PROMT_Exp1_Data", pattern = "*.csv", all.files = FALSE,
                         full.names = TRUE, recursive = FALSE)

# Create blank data frame
experiment_1 <- data.frame()

# Loop through data files, adding to data frame
for (i in file_names_1) {
  data <- fread(i, header = TRUE, sep = ",")
  experiment_1 <- rbind(experiment_1, data)
}

# Note number of exposures per talker
experiment_1$exposure_number <- 32

# Load in Data for Experiment 2 ----

# Experiment 2A: 1x Exposure, No Feedback
file_names_2A <- list.files(path = "PROMT_Exp2A_Data", pattern = "*.csv", all.files = FALSE,
                         full.names = TRUE, recursive = FALSE)

experiment_2A <- data.frame()

for (i in file_names_2A) {
  data <- fread(i, header = TRUE, sep = ",", fill = TRUE)
  experiment_2A <- rbind(experiment_2A, data)
}
experiment_2A$exposure_number <- 32

# Experiment 2B: 1x Exposure, Feedback
file_names_2B <- list.files(path = "PROMT_Exp2B_Data", pattern = "*.csv", all.files = FALSE,
                           full.names = TRUE, recursive = FALSE)

experiment_2B <- data.frame()

for (i in file_names_2B) {
  data <- fread(i, header = TRUE, sep = ",", fill = TRUE)
  experiment_2B <- rbind(experiment_2B, data)
}

experiment_2B$exposure_number <- 32

# Experiment 2C: 2x Exposure, No Feedback
file_names_2C <- list.files(path = "PROMT_Exp2C_Data", pattern = "*.csv", all.files = FALSE,
                           full.names = TRUE, recursive = FALSE)

experiment_2C <- data.frame()

for (i in file_names_2C) {
  data <- fread(i, header = TRUE, sep = ",", fill = TRUE)
  experiment_2C <- rbind(experiment_2C, data)
}

experiment_2C$exposure_number <- 64

# Experiment 2D: 2x Exposure, Feedback
file_names_2D <- list.files(path = "PROMT_Exp2D_Data", pattern = "*.csv", all.files = FALSE,
                           full.names = TRUE, recursive = FALSE)

experiment_2D <- data.frame()

for (i in file_names_2D) {
  data <- fread(i, header = TRUE, sep = ",", fill = TRUE)
  experiment_2D <- rbind(experiment_2D, data)
}
experiment_2D$exposure_number <- 64

# Data Housekeeping ----

# Combine all sets of data into one dataframe
experiment_df <- rbind(experiment_1, experiment_2A, experiment_2B, experiment_2C, experiment_2D)

# Clean up environment
rm(data); rm(experiment_1); rm(experiment_2A); rm(experiment_2B); rm(experiment_2C); rm(experiment_2D)
rm(i); rm(file_names_1); rm(file_names_2A); rm(file_names_2B); rm(file_names_2C); rm(file_names_2D)

# Make variable names syntactically valid - everything is machine readable now
experiment_df <- clean_names(experiment_df)

# Set factors
experiment_df$spreadsheet_name <- as.factor(experiment_df$spreadsheet_name)
experiment_df$exposure_number <- as.factor(experiment_df$exposure_number)

# Create a column that has all the counterbalancing information
experiment_df$counterbalancing <- NA
experiment_df$counterbalancing[which(!is.na(experiment_df$randomiser_b834))] <- experiment_df$randomiser_b834[which(!is.na(experiment_df$randomiser_b834))]
experiment_df$counterbalancing[which(!is.na(experiment_df$randomiser_hvq1))] <- experiment_df$randomiser_hvq1[which(!is.na(experiment_df$randomiser_hvq1))]
experiment_df$counterbalancing[which(!is.na(experiment_df$randomiser_7do6))] <- experiment_df$randomiser_7do6[which(!is.na(experiment_df$randomiser_7do6))]
experiment_df$counterbalancing[which(!is.na(experiment_df$randomiser_o6lu))] <- experiment_df$randomiser_o6lu[which(!is.na(experiment_df$randomiser_o6lu))]
experiment_df$counterbalancing[which(!is.na(experiment_df$randomiser_gfgd))] <- experiment_df$randomiser_gfgd[which(!is.na(experiment_df$randomiser_gfgd))]
experiment_df$counterbalancing[which(!is.na(experiment_df$randomiser_ml5o))] <- experiment_df$randomiser_ml5o[which(!is.na(experiment_df$randomiser_ml5o))]
experiment_df$counterbalancing[which(!is.na(experiment_df$randomiser_mulp))] <- experiment_df$randomiser_mulp[which(!is.na(experiment_df$randomiser_mulp))]
experiment_df$counterbalancing[which(!is.na(experiment_df$randomiser_didj))] <- experiment_df$randomiser_didj[which(!is.na(experiment_df$randomiser_didj))]

# Give columns more informative names
colnames(experiment_df)[which(colnames(experiment_df)=="checkpoint_7g4v")] <- "headphone_check"
colnames(experiment_df)[which(colnames(experiment_df)=="spreadsheet_name")] <- "condition"
colnames(experiment_df)[which(colnames(experiment_df)=="randomiser_czzd")] <- "order"
colnames(experiment_df)[which(colnames(experiment_df)=="display")] <- "task"

# Clean up labels for Experiment 1 
exp1_df <- subset(experiment_df, condition == "Blocked")
exp1_df$condition <- NA
exp1_df <- separate(exp1_df, order, c("condition", "talker1bias", "talker2bias"), sep = "_", remove = FALSE)
exp1_df$order <- paste0(exp1_df$talker1bias,"_",exp1_df$talker2bias)
exp1_df$talker1bias <- NULL
exp1_df$talker2bias <- NULL
exp1_df$set <- NA
exp1_df$testorder <- NA
exp1_df$feedback <- "no"

# Clean up labels for Experiment 2
exp2_df <- subset(experiment_df, condition == "MixedTrain_AustinTest1st" | condition == "MixedTrain_JaneTest1st" | condition == "AustinFirst" | condition =="JaneFirst")
exp2_df$condition <- NA
exp2_df <- separate(exp2_df, order, c("condition", "set", "talker1bias", "talker2bias", "testorder"), sep = "_", remove = FALSE)
exp2_df$order <- paste0(exp2_df$talker1bias,"_",exp2_df$talker2bias)
exp2_df$talker1bias <- NULL
exp2_df$talker2bias <- NULL
exp2_df$feedback <- "no"
exp2_df$feedback[exp2_df$task_name=="Cents and Shenshibility Experiment 3 - Feedback" | exp2_df$task_name=="Cents and Shenshibility Experiment 4 - Feedback"] <- "yes"

# Create merged data frame
experiment_df <- rbind(exp1_df, exp2_df)

# Clean up environment
rm(exp1_df); rm(exp2_df) 

# Create a column with the stimulus name
experiment_df$stimulus <- NA
experiment_df$stimulus[which(experiment_df$order=="AustinS_JaneSH" & experiment_df$condition=="Blocked")] <- experiment_df$x1[which(experiment_df$order=="AustinS_JaneSH" & experiment_df$condition=="Blocked")]
experiment_df$stimulus[which(experiment_df$order=="JaneS_AustinSH" & experiment_df$condition=="Blocked")] <- experiment_df$x2[which(experiment_df$order=="JaneS_AustinSH" & experiment_df$condition=="Blocked")]
experiment_df$stimulus[which(experiment_df$order=="AustinSH_JaneS" & experiment_df$condition=="Blocked")] <- experiment_df$x3[which(experiment_df$order=="AustinSH_JaneS" & experiment_df$condition=="Blocked")]
experiment_df$stimulus[which(experiment_df$order=="JaneSH_AustinS" & experiment_df$condition=="Blocked")] <- experiment_df$x4[which(experiment_df$order=="JaneSH_AustinS" & experiment_df$condition=="Blocked")]

experiment_df$stimulus[which(experiment_df$order=="AustinS_JaneSH" & experiment_df$condition=="Mixed" & experiment_df$set == "AustinSet1")] <- experiment_df$x1[which(experiment_df$order=="AustinS_JaneSH" & experiment_df$condition=="Mixed" & experiment_df$set == "AustinSet1")]
experiment_df$stimulus[which(experiment_df$order=="AustinS_JaneSH" & experiment_df$condition=="Mixed" & experiment_df$set == "JaneSet1")] <- experiment_df$x2[which(experiment_df$order=="AustinS_JaneSH" & experiment_df$condition=="Mixed" & experiment_df$set == "JaneSet1")]
experiment_df$stimulus[which(experiment_df$order=="AustinSH_JaneS" & experiment_df$condition=="Mixed" & experiment_df$set == "AustinSet1")] <- experiment_df$x3[which(experiment_df$order=="AustinSH_JaneS" & experiment_df$condition=="Mixed" & experiment_df$set == "AustinSet1")]
experiment_df$stimulus[which(experiment_df$order=="AustinSH_JaneS" & experiment_df$condition=="Mixed" & experiment_df$set == "JaneSet1")] <- experiment_df$x4[which(experiment_df$order=="AustinSH_JaneS" & experiment_df$condition=="Mixed" & experiment_df$set == "JaneSet1")]

# Indicate the bias for each talker. For Blocked, talker1 was the first block and talker2 was the second.
experiment_df <-separate(experiment_df, order, c("talker1", "talker2"), sep="_", remove = FALSE)

# Select only columns we want.
experiment_df <- dplyr::filter(experiment_df,experiment_df$trial_number != "BEGIN TASK")
experiment_df <- dplyr::filter(experiment_df,experiment_df$trial_number != "END TASK")

experiment_df <- select(experiment_df, task_name, participant_private_id, participant_public_id,
                       headphone_check,
                       feedback, condition, set, order, testorder, talker1, talker2, counterbalancing, 
                       exposure_number, spreadsheet_row,trial_number,
                       zone_type, reaction_time,response,attempt,task,stimulus)


# Remove trials where stimulus failed to load.
experiment_df <- subset(experiment_df,condition!="")

# Set vectors.
experiment_df$spreadsheet_row <- as.numeric(experiment_df$spreadsheet_row)
experiment_df$trial_number <- as.numeric(experiment_df$trial_number)
experiment_df$reaction_time <- as.numeric(experiment_df$reaction_time)

# Remove subjects if they never passed the headphone check
experiment_df <- filter(experiment_df, headphone_check == "Pass")

# Use participant private ID as subject number
experiment_df <- experiment_df %>%
  rename(Subject = participant_private_id)
experiment_df$Subject <- as.factor(experiment_df$Subject)

# Separate data by block.
exp_TD <- subset(experiment_df,grepl("TD",task))
exp_TD <- droplevels(subset(exp_TD,zone_type=="response_keyboard_single"))

exp_PC <- droplevels(subset(experiment_df,grepl("PC",task)))
exp_PC <- droplevels(subset(exp_PC,zone_type=="response_keyboard_single"))

# Reformat responses on TD task
exp_TD$F_resp <- NA
exp_TD$F_resp[which(exp_TD$response=="MALE")] <- 0
exp_TD$F_resp[which(exp_TD$response=="FEMALE")] <- 1

# Note who the talker was on each TD trial
exp_TD$talker <- substr(exp_TD$stimulus, nchar(exp_TD$stimulus)-7, nchar(exp_TD$stimulus)-4)
exp_TD$talker[which(exp_TD$talker=="stin")] <- "MALE"
exp_TD$talker[which(exp_TD$talker=="jane")] <- "FEMALE"

# Reformat responses on PC task
exp_PC$response <- substr(exp_PC$response, 1, nchar(exp_PC$response)-4)
exp_PC$SH_resp <- NA
exp_PC$SH_resp[which(exp_PC$response=="sign")] <- 0
exp_PC$SH_resp[which(exp_PC$response=="shine")] <- 1

# Grab the step number and talker name for each trial of the PC task
exp_PC$step <- as.numeric(substr(exp_PC$stimulus,17,17))
exp_PC$talker <- as.factor(substr(exp_PC$stimulus,19,nchar(exp_PC$stimulus)-4))

# Indicate the bias for each PC trial

exp_PC$bias <- NA

exp_PC$bias[intersect(which(exp_PC$talker=="austin"), which(grepl("austin",exp_PC$talker1, ignore.case = TRUE)))] <- 
  substr(exp_PC$talker1[intersect(which(exp_PC$talker=="austin"), which(grepl("austin",exp_PC$talker1, ignore.case = TRUE)))],
         nchar(exp_PC$talker1[intersect(which(exp_PC$talker=="austin"), which(grepl("austin",exp_PC$talker1, ignore.case = TRUE)))]),
         nchar(exp_PC$talker1[intersect(which(exp_PC$talker=="austin"), which(grepl("austin",exp_PC$talker1, ignore.case = TRUE)))]))

exp_PC$bias[intersect(which(exp_PC$talker=="austin"), which(grepl("austin",exp_PC$talker2, ignore.case = TRUE)))] <- 
  substr(exp_PC$talker2[intersect(which(exp_PC$talker=="austin"), which(grepl("austin",exp_PC$talker2, ignore.case = TRUE)))],
         nchar(exp_PC$talker2[intersect(which(exp_PC$talker=="austin"), which(grepl("austin",exp_PC$talker2, ignore.case = TRUE)))]),
         nchar(exp_PC$talker2[intersect(which(exp_PC$talker=="austin"), which(grepl("austin",exp_PC$talker2, ignore.case = TRUE)))]))

exp_PC$bias[intersect(which(exp_PC$talker=="jane"), which(grepl("jane",exp_PC$talker1, ignore.case = TRUE)))] <- 
  substr(exp_PC$talker1[intersect(which(exp_PC$talker=="jane"), which(grepl("jane",exp_PC$talker1, ignore.case = TRUE)))],
         nchar(exp_PC$talker1[intersect(which(exp_PC$talker=="jane"), which(grepl("jane",exp_PC$talker1, ignore.case = TRUE)))]),
         nchar(exp_PC$talker1[intersect(which(exp_PC$talker=="jane"), which(grepl("jane",exp_PC$talker1, ignore.case = TRUE)))]))

exp_PC$bias[intersect(which(exp_PC$talker=="jane"), which(grepl("jane",exp_PC$talker2, ignore.case = TRUE)))] <- 
  substr(exp_PC$talker2[intersect(which(exp_PC$talker=="jane"), which(grepl("jane",exp_PC$talker2, ignore.case = TRUE)))],
         nchar(exp_PC$talker2[intersect(which(exp_PC$talker=="jane"), which(grepl("jane",exp_PC$talker2, ignore.case = TRUE)))]),
         nchar(exp_PC$talker2[intersect(which(exp_PC$talker=="jane"), which(grepl("jane",exp_PC$talker2, ignore.case = TRUE)))]))

# Rename the bias label to be more intuitive
exp_PC$bias[which(exp_PC$bias=="H")] <- "SH"
  
  
# Identify Good Subjects ----
# First, count non-responses in TD task
exp_TD.bySubj <- as.data.frame(exp_TD %>%
                                    group_by(Subject) %>%
                                    dplyr::summarize(na_count = 2*mean(as.numeric(as.character(exposure_number))) - sum(attempt), 
                                              na_percent = (2*mean(as.numeric(as.character(exposure_number))) - sum(attempt))/(2*mean(as.numeric(as.character(exposure_number))))))
exclude <- sort(as.numeric(levels(droplevels(exp_TD.bySubj$Subject[which(exp_TD.bySubj$na_percent>=0.10)]))))

# Next, identify poor talker identification in TD task
exp_TD$Acc <- NA
exp_TD$Acc[which(exp_TD$talker == exp_TD$response)] <- 1
exp_TD$Acc[which(exp_TD$talker != exp_TD$response)] <- 0

exp_TD.talkerAcc <- Rmisc::summarySE(exp_TD,
                                        measurevar = "Acc", 
                                        groupvars = c("Subject","condition","talker1","talker2"), 
                                        na.rm = TRUE)

# And drop subjects with less than 90% accuracy on talker decision (TD) during exposure phase
exclude <- sort(as.numeric(union(exclude, 
                                 levels(droplevels(exp_TD.talkerAcc$Subject[which(exp_TD.talkerAcc$Acc <= 0.9)])))))


# Next, count non-responses in PC task
exp_PC.bySubj <- as.data.frame(exp_PC %>%
                                  group_by(Subject) %>%
                                  dplyr::summarize(na_count = 140-sum(attempt), 
                                            na_percent = (140-sum(attempt))/140))
exclude <- sort(as.numeric(union(exclude, levels(droplevels(exp_PC.bySubj$Subject[which(exp_PC.bySubj$na_percent>=0.10)])))))


# Finally, identify poor characterization of endpoints in PC task
exp_PC$step <- as.factor(exp_PC$step)
exp_PC.endpoints <- droplevels(subset(exp_PC, step == 1 | step == 7))

exp_PC.endpoints$Acc <- NA
exp_PC.endpoints$Acc[which(exp_PC.endpoints$step==1 & exp_PC.endpoints$SH_resp == 1)] <- 0; exp_PC.endpoints$Acc[which(exp_PC.endpoints$step==1 & exp_PC.endpoints$SH_resp == 0)] <- 1
exp_PC.endpoints$Acc[which(exp_PC.endpoints$step==7 & exp_PC.endpoints$SH_resp == 1)] <- 1; exp_PC.endpoints$Acc[which(exp_PC.endpoints$step==7 & exp_PC.endpoints$SH_resp == 0)] <- 0

exp_PC.endpointAcc <- Rmisc::summarySE(exp_PC.endpoints,
                                   measurevar = "Acc", 
                                   groupvars = c("Subject"), 
                                   na.rm = TRUE)

# And drop subjects with less than 70% accuracy in the perceptual categorization of endpoints
exclude <- sort(as.numeric(union(exclude, levels(droplevels(exp_PC.endpointAcc$Subject[which(exp_PC.endpointAcc$Acc <= 0.7)])))))

# Before continuing on, set the step to a numeric value
exp_PC$step <- as.numeric(exp_PC$step)

# Now, we can drop bad subjects
exp_TD.goodSubj <- droplevels(exp_TD %>%
                                filter(!Subject %in% exclude))

exp_PC.goodSubj <- droplevels(exp_PC %>%
                                filter(!Subject %in% exclude))

# We also will randomly exclude subjects because there were too many in one of the counterbalancing conditions
exp_TD.goodSubj <- droplevels(subset(exp_TD.goodSubj, Subject != "1246132" & Subject != "1082893" & Subject != "1082832"  & Subject != "1082763" & Subject != "1082695" & Subject != "1191464" & Subject != "1092342" & Subject != "1193007" & Subject != "1080728" & Subject != "1104308"))
exp_PC.goodSubj <- droplevels(subset(exp_PC.goodSubj, Subject != "1246132" & Subject != "1082893" & Subject != "1082832"  & Subject != "1082763" & Subject != "1082695" & Subject != "1191464" & Subject != "1092342" & Subject != "1193007" & Subject != "1080728" & Subject != "1104308"))

# Summarize Data ----
# Load in demographics
demographics <- read.csv('promt_demographics.csv', header = TRUE, sep = ',', na.strings = "#N/A") 

# Clean up column names
demographics <- clean_names(demographics)
demographics <- select(demographics, participant_id, age, sex)
colnames(demographics)[which(colnames(demographics)=="participant_id")] <- "participant_public_id"

# Merge demographics in with our data
exp_PC.goodSubj <- merge(exp_PC.goodSubj, demographics, by = "participant_public_id")
exp_PC.goodSubj$age <- as.numeric(as.character(exp_PC.goodSubj$age))

# Summarise number of subjects in each condition
exp_goodSubj_grouped <- exp_PC.goodSubj %>%
  group_by(feedback, exposure_number, condition, set, order, testorder, Subject) %>% 
  dplyr::summarise(count = length(order)) %>% 
  dplyr::summarise(count = length(count))
exp_goodSubj_grouped

# Check the number of participants in each of our orders/counterbalancing conditions
exp_goodSubj_groupedByOrderAndCounterbalance <- exp_PC.goodSubj %>%
  group_by(feedback,condition, set, order, testorder, counterbalancing,exposure_number, Subject) %>% 
  dplyr::summarise(count = length(order)) %>%
  dplyr::summarise(count = length(count))
exp_goodSubj_groupedByOrderAndCounterbalance

# Summarise demographics
demographics_E1 <- exp_PC.goodSubj %>%
  filter(condition == "Blocked") %>%
  group_by(participant_public_id, sex) %>%
  dplyr::summarise(age = mean(age))
summary(demographics_E1$age) #age summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 19.00   24.75   32.50   34.28   39.50   68.00
summary(droplevels(demographics_E1$sex)) #breakdown by sex
# Female   Male 
# 22     10 

demographics_E2A <- exp_PC.goodSubj %>%
  filter(condition == "Mixed", feedback == "no", exposure_number == 32) %>%
  group_by(participant_public_id, sex) %>%
  dplyr::summarise(age = mean(age))
summary(demographics_E2A$age) #age summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   20.00   26.50   30.00   32.23   34.50   66.00       1 
summary(droplevels(demographics_E2A$sex)) #breakdown by sex
# Female   Male 
# 18     14 

demographics_E2B <- exp_PC.goodSubj %>%
  filter(condition == "Mixed", feedback == "yes", exposure_number == 32) %>%
  group_by(participant_public_id, sex) %>%
  dplyr::summarise(age = mean(age))
summary(demographics_E2B$age) #age summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   25.00   29.00   32.25   36.50   68.00 
summary(droplevels(demographics_E2B$sex)) #breakdown by sex
# Female   Male 
# 20     12 

demographics_E2C <- exp_PC.goodSubj %>%
  filter(condition == "Mixed", feedback == "no", exposure_number == 64) %>%
  group_by(participant_public_id, sex) %>%
  dplyr::summarise(age = mean(age))
summary(demographics_E2C$age) #age summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   21.00   25.50   29.47   34.00   61.00
summary(droplevels(demographics_E2C$sex)) #breakdown by sex
# Female   Male 
# 14     18 
 
demographics_E2D <- exp_PC.goodSubj %>%
  filter(condition == "Mixed", feedback == "yes", exposure_number == 64) %>%
  group_by(participant_public_id, sex) %>%
  dplyr::summarise(age = mean(age))
summary(demographics_E2D$age) #age summary
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 19.00   22.75   27.50   31.81   37.00   70.00 
summary(droplevels(demographics_E2D$sex)) #breakdown by sex
# Female   Male 
# 16     16 

TD.Acc.goodSubj.byExp <- Rmisc::summarySE(exp_TD.goodSubj, measurevar = "Acc", groupvars = c("condition","exposure_number","feedback"))
TD.Acc.goodSubj.byExp

TD.Acc.goodSubj <- Rmisc::summarySE(exp_TD.goodSubj, measurevar = "Acc")
TD.Acc.goodSubj

# Plot ---
# Rename conditions for plot
exp_PC.goodSubj$condition[which(exp_PC.goodSubj$condition == "Mixed" & exp_PC.goodSubj$feedback == "yes" & exp_PC.goodSubj$exposure_number == 32)] <- "Feedback - Mixed Train, 1x Exposure"
exp_PC.goodSubj$condition[which(exp_PC.goodSubj$condition == "Mixed" & exp_PC.goodSubj$feedback == "yes" & exp_PC.goodSubj$exposure_number == 64)] <- "Feedback - Mixed Train, 2x Exposure"
exp_PC.goodSubj$condition[which(exp_PC.goodSubj$condition == "Mixed" & exp_PC.goodSubj$feedback == "no" & exp_PC.goodSubj$exposure_number == 64)] <- "No Feedback - Mixed Train, 2x Exposure"
exp_PC.goodSubj$condition[which(exp_PC.goodSubj$condition == "Mixed" & exp_PC.goodSubj$feedback == "no" & exp_PC.goodSubj$exposure_number == 32)] <- "No Feedback - Mixed Train, 1x Exposure"

exp_PC.goodSubj$condition[which(exp_PC.goodSubj$condition == "Blocked" & exp_PC.goodSubj$feedback == "no")] <- "Blocked Train"


# Summarize all data before plotting
PC.figure.stats <- Rmisc::summarySE(exp_PC.goodSubj, measurevar="SH_resp",groupvars = c("step","bias","condition", "exposure_number"))
PC.figure.stats$condition <- factor(PC.figure.stats$condition, 
                                    levels = c("Blocked Train",
                                               "No Feedback - Mixed Train, 1x Exposure", 
                                               "Feedback - Mixed Train, 1x Exposure", 
                                               "No Feedback - Mixed Train, 2x Exposure",
                                               "Feedback - Mixed Train, 2x Exposure"))

PC.figure.stats$condition <- fct_relevel(PC.figure.stats$condition, 
                                                  "No Feedback - Mixed Train, 2x Exposure"
                                                  ,"Feedback - Mixed Train, 2x Exposure"
                                                  ,"No Feedback - Mixed Train, 1x Exposure"
                                                  ,"Feedback - Mixed Train, 1x Exposure"
                                                  ,"Blocked Train")
PC.figure.stats$bias <- fct_relevel(PC.figure.stats$bias, "S")

# Make figure
promt.plot <- ggplot(PC.figure.stats, aes(x=as.numeric(step), y = SH_resp)) + 
  facet_wrap(~condition, labeller = label_wrap_gen(), as.table = FALSE, ncol = 2) +
  geom_line(stat='summary', fun.y='mean', size = 3.0, aes(color = as.factor(bias))) +
  geom_point(aes(fill = as.factor(bias)), stat='summary', fun.y='mean', size = 4.0, stroke = 0, shape = 21) +
  geom_errorbar(aes(ymin = SH_resp - ci,ymax = SH_resp + ci, color = as.factor(bias)), width = .5, size = 1) +
  scale_x_continuous('Continuum step', breaks = c(1:7)) +
  scale_y_continuous('Percent "shine" responses', breaks = c(0,0.25,0.5,0.75,1), labels = c(0,25,50,75,100)) +
  scale_color_manual(values = c("#ff5947", "#52377d")) +
  scale_fill_manual(values = c("#f00303","#6343ba")) +
  coord_cartesian(ylim = c(0,1)) +
  theme(text = element_text(size = 40),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(size = 60, colour = "grey", face = "bold"),
        axis.title.y = element_text(size = 60, colour = "grey", margin = margin(l = 20, r =20)),
        axis.text.x = element_text(size = 50, colour = "grey"),
        axis.text.y = element_text(size = 50, colour = "grey"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_blank())
promt.plot
ggsave(promt.plot, filename = "promt.results.plots.png",  bg = "transparent", dpi = 600,
       height = 30, width = 17)

# Subset Data For Analyses ----
# Subset E1 data
exp1_PC.goodSubj <- droplevels(subset(exp_PC.goodSubj, condition == "Blocked Train"))

# Subset E2 data 
exp2_PC.goodSubj <- droplevels(subset(exp_PC.goodSubj, 
                                      condition == "No Feedback - Mixed Train, 1x Exposure" | 
                                        condition == "Feedback - Mixed Train, 1x Exposure" | 
                                        condition == "Feedback - Mixed Train, 2x Exposure" |
                                        condition == "No Feedback - Mixed Train, 2x Exposure"))

exp2_PC.goodSubj.1x.noFB <- droplevels(subset(exp_PC.goodSubj, condition == "No Feedback - Mixed Train, 1x Exposure"))
exp2_PC.goodSubj.1x.wFB <- droplevels(subset(exp_PC.goodSubj, condition == "Feedback - Mixed Train, 1x Exposure"))
exp2_PC.goodSubj.2x.noFB <- droplevels(subset(exp_PC.goodSubj, condition == "No Feedback - Mixed Train, 2x Exposure"))
exp2_PC.goodSubj.2x.wFB <- droplevels(subset(exp_PC.goodSubj, condition == "Feedback - Mixed Train, 2x Exposure"))



# ANALYSES: EXPERIMENT 1 (BLOCKED TRAIN, BLOCKED TEST) ----
cols <- c('Subject', 'bias')
exp1_PC.goodSubj[, cols] <- lapply(exp1_PC.goodSubj[, cols], as.factor)
exp1_PC.goodSubj$step <- scale(exp1_PC.goodSubj$step)
#Set contrast to deviation coding
contrasts(exp1_PC.goodSubj$bias) = contr.sum(2)


# Build models.

# BLOCKED TRAINING, BLOCKED TESTING
m.e1.full <- mixed(SH_resp ~ step*bias + 
              (step*bias|Subject) + (step|Subject) + (bias|Subject), family=binomial(link="logit"),
            data=exp1_PC.goodSubj, method="LRT",expand_re = TRUE,
            control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

m.e1.noIntxn <- mixed(SH_resp ~ step*bias + 
              (bias|Subject) + (step|Subject), family=binomial(link="logit"),
            data=exp1_PC.goodSubj, method="LRT",expand_re = TRUE,
            control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

anova(m.e1.full, m.e1.noIntxn)
# DING DING DING!!! FULL MODEL WINS!
m.e1.full
# Mixed Model Anova Table (Type 3 tests, LRT-method)
# 
# Model: SH_resp ~ step * bias + (step * bias | Subject) + (step | Subject) + 
#   Model:     (bias | Subject)
# Data: exp1_PC.goodSubj
# Df full model: 20
# Effect df     Chisq p.value
# 1      step  1 61.53 ***  <.0001
# 2      bias  1   9.35 **    .002
# 3 step:bias  1      0.09     .76
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

summary(m.e1.full)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: SH_resp ~ step * bias + (1 + re1.step + re1.bias1 + re1.step_by_bias1 |  
#                                     Subject) + (1 + re2.step | Subject) + (1 + re3.bias1 | Subject)
# Data: data
# Control: glmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 150000))
# 
# AIC      BIC   logLik deviance df.resid 
# 2204.0   2332.7  -1082.0   2164.0     4579 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -43.204  -0.158   0.002   0.133   8.218 
# 
# Random effects:
#   Groups    Name              Variance Std.Dev. Corr             
# Subject   (Intercept)       1.2942   1.1376                    
# re1.step          2.2411   1.4970    0.83            
# re1.bias1         1.2448   1.1157   -0.03  0.02      
# re1.step_by_bias1 1.1724   1.0828    0.04  0.09  0.85
# Subject.1 (Intercept)       1.0912   1.0446                    
# re2.step          1.6206   1.2730   0.85             
# Subject.2 (Intercept)       0.2283   0.4778                    
# re3.bias1         0.2511   0.5011   -0.47            
# Number of obs: 4599, groups:  Subject, 32
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.34297    0.30131   4.457 8.31e-06 ***
#   step         5.31483    0.39166  13.570  < 2e-16 ***
#   bias1       -0.80614    0.23504  -3.430 0.000604 ***
#   step:bias1  -0.09465    0.25499  -0.371 0.710505    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) step   bias1 
# step        0.765              
# bias1      -0.097 -0.034       
# step:bias1 -0.027  0.041  0.683

# ANALYSES: EXPERIMENT 2 (MIXED TRAIN, BLOCKED TEST) ----

# Set factors, contrasts, etc. for analyses
cols <- c('Subject', 'bias')

exp2_PC.goodSubj.1x.noFB[, cols] <- lapply(exp2_PC.goodSubj.1x.noFB[, cols], as.factor)
exp2_PC.goodSubj.1x.noFB$step <- scale(exp2_PC.goodSubj.1x.noFB$step)
#Set contrast to deviation coding
contrasts(exp2_PC.goodSubj.1x.noFB$bias) = contr.sum(2)

exp2_PC.goodSubj.1x.wFB[, cols] <- lapply(exp2_PC.goodSubj.1x.wFB[, cols], as.factor)
exp2_PC.goodSubj.1x.wFB$step <- scale(exp2_PC.goodSubj.1x.wFB$step)
#Set contrast to deviation coding
contrasts(exp2_PC.goodSubj.1x.wFB$bias) = contr.sum(2)

exp2_PC.goodSubj.2x.noFB[, cols] <- lapply(exp2_PC.goodSubj.2x.noFB[, cols], as.factor)
exp2_PC.goodSubj.2x.noFB$step <- scale(exp2_PC.goodSubj.2x.noFB$step)
#Set contrast to deviation coding
contrasts(exp2_PC.goodSubj.2x.noFB$bias) = contr.sum(2)

exp2_PC.goodSubj.2x.wFB[, cols] <- lapply(exp2_PC.goodSubj.2x.wFB[, cols], as.factor)
exp2_PC.goodSubj.2x.wFB$step <- scale(exp2_PC.goodSubj.2x.wFB$step)
#Set contrast to deviation coding
contrasts(exp2_PC.goodSubj.2x.wFB$bias) = contr.sum(2)


# Build models.

# NO FEEDBACK - MIXED TRAINING, BLOCKED TESTING, 1x
m9 <- mixed(SH_resp ~ step*bias + 
              (step*bias|Subject) + (step|Subject) + (bias|Subject), family=binomial(link="logit"),
            data=exp2_PC.goodSubj.1x.noFB, method="LRT",expand_re = TRUE,
            control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

m10 <- mixed(SH_resp ~ step*bias + 
              (bias|Subject) + (step|Subject), family=binomial(link="logit"),
            data=exp2_PC.goodSubj.1x.noFB, method="LRT",expand_re = TRUE,
            control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

anova(m9, m10)
#model 9 is better!!! stop here! 
m9
# Mixed Model Anova Table (Type 3 tests, LRT-method)
# 
# Model: SH_resp ~ step * bias + (step * bias | Subject) + (step | Subject) + 
#   Model:     (bias | Subject)
# Data: exp2_PC.goodSubj.1x.noFB
# Df full model: 20
# Effect df     Chisq p.value
# 1      step  1 76.63 ***  <.0001
# 2      bias  1      1.65     .20
# 3 step:bias  1      0.00     .98
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

summary(m9)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: SH_resp ~ step * bias + (1 + re1.step + re1.bias1 + re1.step_by_bias1 |  
#                                     Subject) + (1 + re2.step | Subject) + (1 + re3.bias1 | Subject)
# Data: data
# Control: glmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 150000))
# 
# AIC      BIC   logLik deviance df.resid 
# 2252.0   2381.2  -1106.0   2212.0     4721 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -56.117  -0.147   0.011   0.152  20.426 
# 
# Random effects:
#   Groups    Name              Variance Std.Dev. Corr             
# Subject   (Intercept)       0.6475   0.8047                    
# re1.step          0.5278   0.7265    0.73            
# re1.bias1         0.5633   0.7506   -0.39 -0.77      
# re1.step_by_bias1 0.8271   0.9094   -0.25 -0.67  0.76
# Subject.1 (Intercept)       0.9114   0.9547                    
# re2.step          0.7551   0.8689   0.79             
# Subject.2 (Intercept)       0.2310   0.4807                    
# re3.bias1         0.1871   0.4326   -0.03            
# Number of obs: 4741, groups:  Subject, 32
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.559086   0.251443   6.201 5.63e-10 ***
#   step         4.882960   0.252911  19.307  < 2e-16 ***
#   bias1       -0.240176   0.174649  -1.375    0.169    
# step:bias1  -0.007551   0.220955  -0.034    0.973    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) step   bias1 
# step        0.663              
# bias1      -0.201 -0.335       
# step:bias1 -0.131 -0.252  0.624

# FEEDBACK - MIXED TRAINING, BLOCKED TESTING, 1x Exposure 
m13 <- mixed(SH_resp ~ step*bias + 
              (step*bias|Subject) + (step|Subject) + (bias|Subject), family=binomial(link="logit"),
            data=exp2_PC.goodSubj.1x.wFB, method="LRT",expand_re = TRUE,
            control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

m14 <- mixed(SH_resp ~ step*bias + 
               (bias|Subject) + (step|Subject), family=binomial(link="logit"),
             data=exp2_PC.goodSubj.1x.wFB, method="LRT",expand_re = TRUE,
             control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

anova(m13, m14)
#model 13 is better!! go with the maximal one!!!!
m13
# Mixed Model Anova Table (Type 3 tests, LRT-method)
# 
# Model: SH_resp ~ step * bias + (step * bias | Subject) + (step | Subject) + 
#   Model:     (bias | Subject)
# Data: exp2_PC.goodSubj.1x.wFB
# Df full model: 20
# Effect df     Chisq p.value
# 1      step  1 70.08 ***  <.0001
# 2      bias  1      0.01     .91
# 3 step:bias  1      2.12     .15
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

# elaborated summary of 1x exposure, mixed test, block test with feedback ----
summary(m13)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: SH_resp ~ step * bias + (1 + re1.step + re1.bias1 + re1.step_by_bias1 |  
#                                     Subject) + (1 + re2.step | Subject) + (1 + re3.bias1 | Subject)
# Data: data
# Control: glmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 150000))
# 
# AIC      BIC   logLik deviance df.resid 
# 1771.0   1899.2   -865.5   1731.0     4456 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -25.2601  -0.0894   0.0016   0.0855  27.2065 
# 
# Random effects:
#   Groups    Name              Variance Std.Dev. Corr          
# Subject   (Intercept)       1.4394   1.1997                 
# re1.step          1.2662   1.1252   0.68          
# re1.bias1         2.2703   1.5067   0.57 0.68     
# re1.step_by_bias1 3.2787   1.8107   0.71 0.63 0.86
# Subject.1 (Intercept)       1.6850   1.2981                 
# re2.step          2.3247   1.5247   0.87          
# Subject.2 (Intercept)       0.4546   0.6743                 
# re3.bias1         0.4759   0.6899   0.26          
# Number of obs: 4476, groups:  Subject, 32
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  2.43250    0.35863   6.783 1.18e-11 ***
#   step         6.68319    0.41750  16.008  < 2e-16 ***
#   bias1        0.03972    0.32020   0.124   0.9013    
# step:bias1   0.69139    0.40515   1.707   0.0879 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) step  bias1
# step       0.714             
# bias1      0.345  0.325      
# step:bias1 0.380  0.358 0.755

# FEEDBACK - MIXED TRAINING, MIXED TESTING, 2x Exposure 
m17 <- mixed(SH_resp ~ step*bias + 
               (step*bias|Subject) + (step|Subject) + (bias|Subject), family=binomial(link="logit"),
             data=exp2_PC.goodSubj.2x.wFB, method="LRT",expand_re = TRUE,
             control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

m18 <- mixed(SH_resp ~ step*bias + 
               (bias|Subject) + (step|Subject), family=binomial(link="logit"),
             data=exp2_PC.goodSubj.2x.wFB, method="LRT",expand_re = TRUE,
             control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

anova(m17, m18) # no difference! keep stepping!
m19 <- mixed(SH_resp ~ step*bias +
              (bias|Subject), family=binomial(link="logit"),
             data=exp2_PC.goodSubj.2x.wFB, method="LRT",expand_re = TRUE,
             control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

anova(m18, m19)
# model m18 is better! Model 18 wins!!
m18
# Mixed Model Anova Table (Type 3 tests, LRT-method)
# 
# Model: SH_resp ~ step * bias + (bias | Subject) + (step | Subject)
# Data: exp2_PC.goodSubj.2x.wFB
# Df full model: 10
# Effect df     Chisq p.value
# 1      step  1 93.64 ***  <.0001
# 2      bias  1   7.38 **    .007
# 3 step:bias  1   8.10 **    .004
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

summary(m18)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: SH_resp ~ step * bias + (1 + re1.bias1 | Subject) + (1 + re2.step |      Subject)
# Data: data
# Control: glmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 150000))
# 
# AIC      BIC   logLik deviance df.resid 
# 2028.9   2092.9  -1004.4   2008.9     4462 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -127.526   -0.132    0.008    0.140   22.309 
# 
# Random effects:
#   Groups    Name        Variance Std.Dev. Corr
# Subject   (Intercept) 1.2902   1.1359       
# re1.bias1   0.3983   0.6311   0.90
# Subject.1 (Intercept) 2.3123   1.5206       
# re2.step    3.0447   1.7449   0.86
# Number of obs: 4472, groups:  Subject, 32
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   1.9277     0.3512   5.488 4.06e-08 ***
#   step          5.1210     0.3549  14.428  < 2e-16 ***
#   bias1        -0.3972     0.1336  -2.972  0.00296 ** 
#   step:bias1    0.3708     0.1230   3.015  0.00257 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) step   bias1 
# step        0.672              
# bias1       0.418 -0.032       
# step:bias1 -0.011  0.078  0.269

# NO FEEDBACK - MIXED TRAINING, BLOCKED TESTING, 2x Exposure
m21 <- mixed(SH_resp ~ step * bias + 
            (step * bias | Subject) + (step | Subject) + (bias | Subject), family = binomial(link = "logit"),
            data=exp2_PC.goodSubj.2x.noFB, method="LRT",expand_re = TRUE,
            control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

m22 <- mixed(SH_resp ~ step * bias + 
            (bias | Subject) + (step | Subject), family = binomial(link = "logit"),
            data=exp2_PC.goodSubj.2x.noFB, method="LRT",expand_re = TRUE,
            control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

anova(m21, m22)
#Model 21 wins!!
m21
# Mixed Model Anova Table (Type 3 tests, LRT-method)
# 
# Model: SH_resp ~ step * bias + (step * bias | Subject) + (step | Subject) + 
#   Model:     (bias | Subject)
# Data: exp2_PC.goodSubj.2x.noFB
# Df full model: 20
# Effect df     Chisq p.value
# 1      step  1 62.52 ***  <.0001
# 2      bias  1    3.41 +     .06
# 3 step:bias  1      0.06     .80
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

summary(m21)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: SH_resp ~ step * bias + (1 + re1.step + re1.bias1 + re1.step_by_bias1 |  
#                                     Subject) + (1 + re2.step | Subject) + (1 + re3.bias1 | Subject)
# Data: data
# Control: glmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 150000))
# 
# AIC      BIC   logLik deviance df.resid 
# 1968.5   2096.6   -964.3   1928.5     4452 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -107.899   -0.129    0.005    0.130   26.816 
# 
# Random effects:
#   Groups    Name              Variance Std.Dev. Corr             
# Subject   (Intercept)       0.975415 0.9876                    
# re1.step          2.199678 1.4831    0.64            
# re1.bias1         1.589239 1.2607    0.29 -0.33      
# re1.step_by_bias1 1.630050 1.2767    0.60  0.16  0.82
# Subject.1 (Intercept)       0.640133 0.8001                    
# re2.step          0.675492 0.8219   0.70             
# Subject.2 (Intercept)       0.232986 0.4827                    
# re3.bias1         0.005716 0.0756   -0.85            
# Number of obs: 4472, groups:  Subject, 32
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  2.03230    0.26165   7.767 8.01e-15 ***
#   step         5.60859    0.35425  15.832  < 2e-16 ***
#   bias1       -0.49529    0.24554  -2.017   0.0437 *  
#   step:bias1  -0.08682    0.28526  -0.304   0.7609    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) step   bias1 
# step        0.620              
# bias1       0.140 -0.257       
# step:bias1  0.286  0.080  0.765

# ALL EXPERIMENT 2 CONDITIONS IN ONE ANALYSIS

# Prep data for models.
# Set things to factor
cols <- c('Subject', 'bias', 'exposure_number', 'feedback')
exp2_PC.goodSubj[, cols] <- lapply(exp2_PC.goodSubj[, cols], as.factor)
exp2_PC.goodSubj$step <- scale(exp2_PC.goodSubj$step)

#Make sure contrast coding makes sense
exp2_PC.goodSubj$bias <- relevel(exp2_PC.goodSubj$bias, "SH") 
exp2_PC.goodSubj$exposure_number <- relevel(exp2_PC.goodSubj$exposure_number, "64") 
exp2_PC.goodSubj$feedback <- relevel(exp2_PC.goodSubj$feedback, "yes") 

# Build models
m.e2.full <- mixed(SH_resp ~ step*bias*feedback*exposure_number + 
                     (step*bias|Subject) + (step|Subject) + (bias|Subject), family=binomial(link="logit"),
                   data=exp2_PC.goodSubj, method="LRT",expand_re = TRUE,
                   control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

m.e2.noIntxn <- mixed(SH_resp ~ step*bias*feedback*exposure_number + 
                        (bias|Subject) + (step|Subject), family=binomial(link="logit"),
                      data=exp2_PC.goodSubj, method="LRT",expand_re = TRUE,
                      control = glmerControl(optimizer="bobyqa",calc.derivs = FALSE, optCtrl = list(maxfun = 150000)))

anova(m.e2.full, m.e2.noIntxn) # DING DING DING full model is better
m.e2.full
# Mixed Model Anova Table (Type 3 tests, LRT-method)
# 
# Model: SH_resp ~ step * bias * feedback * exposure_number + (step * 
#                                                                Model:     bias | Subject) + (step | Subject) + (bias | Subject)
# Data: exp2_PC.goodSubj
# Df full model: 32
# Effect df      Chisq p.value
# 1                                step  1 283.65 ***  <.0001
# 2                                bias  1    7.77 **    .005
# 3                            feedback  1       0.76     .38
# 4                     exposure_number  1       0.14     .70
# 5                           step:bias  1       0.56     .45
# 6                       step:feedback  1       1.02     .31
# 7                       bias:feedback  1       0.02     .88
# 8                step:exposure_number  1       0.32     .57
# 9                bias:exposure_number  1       2.48     .12
# 10           feedback:exposure_number  1       0.63     .43
# 11                 step:bias:feedback  1       1.31     .25
# 12          step:bias:exposure_number  1       1.65     .20
# 13      step:feedback:exposure_number  1     2.80 +     .09
# 14      bias:feedback:exposure_number  1       0.01     .94
# 15 step:bias:feedback:exposure_number  1       0.09     .77
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

summary(m.e2.full)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: SH_resp ~ step * bias * feedback * exposure_number + (1 + re1.step +  
#                                                                  re1.bias1 + re1.step_by_bias1 | Subject) + (1 + re2.step |      Subject) + (1 + re3.bias1 | Subject)
# Data: data
# Control: glmerControl(optimizer = "bobyqa", calc.derivs = FALSE, optCtrl = list(maxfun = 150000))
# 
# AIC      BIC   logLik deviance df.resid 
# 7983.0   8232.9  -3959.5   7919.0    18129 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -63.319  -0.128   0.005   0.125  26.333 
# 
# Random effects:
#   Groups    Name              Variance Std.Dev. Corr             
# Subject   (Intercept)       1.2301   1.1091                    
# re1.step          1.5524   1.2459    0.74            
# re1.bias1         0.8705   0.9330   -0.07  0.13      
# re1.step_by_bias1 1.3308   1.1536   -0.18  0.03  0.91
# Subject.1 (Intercept)       0.8084   0.8991                    
# re2.step          0.9383   0.9686   0.76             
# Subject.2 (Intercept)       0.5469   0.7395                    
# re3.bias1         0.3961   0.6294   -0.47            
# Number of obs: 18161, groups:  Subject, 128
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            1.982088   0.151308  13.100  < 2e-16 ***
#   step                                   5.556777   0.167934  33.089  < 2e-16 ***
#   bias1                                  0.342346   0.111761   3.063  0.00219 ** 
#   feedback1                              0.132587   0.151308   0.876  0.38088    
# exposure_number1                       0.057587   0.151308   0.381  0.70350    
# step:bias1                            -0.124731   0.136609  -0.913  0.36122    
# step:feedback1                         0.170077   0.167934   1.013  0.31117    
# bias1:feedback1                       -0.016398   0.111761  -0.147  0.88335    
# step:exposure_number1                 -0.095766   0.167934  -0.570  0.56850    
# bias1:exposure_number1                 0.177226   0.111761   1.586  0.11279    
# feedback1:exposure_number1            -0.120423   0.151308  -0.796  0.42610    
# step:bias1:feedback1                  -0.156832   0.136609  -1.148  0.25095    
# step:bias1:exposure_number1            0.175867   0.136609   1.287  0.19796    
# step:feedback1:exposure_number1       -0.282888   0.167934  -1.685  0.09208 .  
# bias1:feedback1:exposure_number1      -0.008577   0.111761  -0.077  0.93883    
# step:bias1:feedback1:exposure_number1 -0.040782   0.136609  -0.299  0.76530    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation matrix not shown by default, as p = 16 > 12.
# Use print(x, correlation=TRUE)  or
# vcov(x)        if you need it

# We see main effects of step and bias, but the interaction between bias and exposure number is not quite significant (p = 0.12)

save.image(file = "PROMT.Results.2020.09.18.RData")
