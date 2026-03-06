library(here)
library(tidyverse)
library(jsonlite)

processed_data_directory <- here("..","data","processed_data")
file_name <- "species_choices"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv")))

exp_data <- exp_data %>%
  select(-participant_id) %>%
  #clean up participant ids
  rename(participant_id = participant) %>%
  mutate(
    participant_id = case_when(
      participant_id == "9252" ~ "parrot",
      TRUE ~ trimws(tolower(participant_id))
    )
  )
#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

#extract questions
capacity_smart <- exp_data %>% 
  filter(task == "capacity_smart") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  select(random_id,smart_person:smart_pig)

capacity_pain <- exp_data %>% 
  filter(task == "capacity_pain") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  select(random_id,pain_person:pain_pig)

capacity_emotion <- exp_data %>% 
  filter(task == "capacity_emotion") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  select(random_id,emotion_person:emotion_pig)

hypothesis_check <- exp_data %>% 
  filter(task == "hypothesis_check") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  rename(hypothesis_check=Q0) %>%
  select(random_id,hypothesis_check)

difficulty_rating <- exp_data %>% 
  filter(task == "difficulty_rating") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  rename(difficulty_rating=Q0) %>%
  select(random_id,difficulty_rating)

technical_check <- exp_data %>% 
  filter(task == "technical_check") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  rename(technical_check =Q0) %>%
  select(random_id,technical_check )

demographics <- exp_data %>% 
  filter(task == "demographics") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  select(random_id,age_bracket:diet)

#join back in
exp_data <- exp_data %>%
  left_join(capacity_smart) %>%
  left_join(capacity_pain) %>%
  left_join(capacity_emotion) %>%
  left_join(hypothesis_check) %>%
  left_join(difficulty_rating) %>%
  left_join(demographics)
  

#filter dataset
exp_data <- exp_data %>%
  filter(!is.na(phase))

#filter participant ids
filter_ids <- c()

#identify participants from the experiment group
group_members <- c("frog","goat","rabbit","parrot","giraffe","duck")

processed_data <- exp_data %>%
  filter(!(participant_id %in% filter_ids)) %>%
  #flag for group participants
  mutate(participant_is_group_member = case_when(
    participant_id %in% group_members ~ TRUE,
    TRUE ~ FALSE
  
  )) %>%
  #remove unneeded columns
  select(-c(success,plugin_version,timeout:failed_video)) %>%
  #add trial_number
  group_by(participant_id) %>%
  mutate(trial_number = row_number()) %>%
  relocate(trial_number,.after=trial_index)
  
#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
