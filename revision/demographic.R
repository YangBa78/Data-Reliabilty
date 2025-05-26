library(readxl)
library(dplyr)

dem <- read.csv("ai-deferral Demographics.csv")
dim(dem)


head(dem)

dem = dem[, c(23:34)]
dem

colnames(dem)
colnames(dem) = c('workerid', 'age', 'gender', 'gender_', 'ethnic', 'education', 'benefit', 'computer', 'game', 'screen', 'ballotbox', 'surverycode')


dem <- dem[-c(1, 2), ]
dim(dem)


############# mapping 
gender_mapping = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = 'Perfer not to repsond', "5" = "Others")
dem$gender_mapped <- gender_mapping[as.character(dem$gender)]

ethnic_mapping = c("1" = "Black or AA", "2" = "Latino", "3" = "Asian", "4" = 'White', "5" = "Pacific", "6"= "Perfer not to repsond", "7"= "Others")
dem$ethnic_mapped <- ethnic_mapping[as.character(dem$ethnic)]
table(dem$ethnic_mapped)


education_mapping = c("1" = "high school less", "2" = "high school", "3" = "2yr college", "4" = '4yr college', "5" = "master", "6"= "Professional", "7"= "doctorate")
dem$education_mapped <- education_mapping[as.character(dem$education)]
table(dem$education_mapped)
table(dem$education)


table(dem$computer)
computer_mapping = c("1" = "daily", "2" = "every couple days", "3" = "once a week", "4" = 'every couple week', "5" = "less than once a month", "6"= "never")
dem$computer_mapped <- computer_mapping[as.character(dem$computer)]
table(dem$computer_mapped)


table(dem$game)
dem$game_mapped <- computer_mapping[as.character(dem$game)]
table(dem$game_mapped)



dem = dem[, c("workerid", "age", "gender_mapped", "ethnic_mapped", "education_mapped", "benefit", "computer_mapped", "game_mapped", "screen")]
colnames(dem) = c("workerid", "age", "gender", "ethnic", "education", "benefit", "computer", "game", "screen")
head(dem, 10)

dem$age = as.numeric(dem$age)
dem$benefit = as.numeric(dem$benefit)
dem$screen = as.numeric(dem$screen)



##############
mt <- read.csv("Data/MTurkPerformanceData.csv")
colnames(mt)

length(unique(mt$WorkerId))

dem$ds <- ifelse(dem$workerid %in% unique(mt$WorkerId), "MT", "No")
mt_dem = dem[dem$ds == 'MT',]
dim(mt_dem)



pl <- read_excel("Data/Prolific.xlsx")
pl = pl[pl['WorkerId']!='2f33f23f23',]

colnames(pl)

length(unique(pl$WorkerId))


dem$ds <- ifelse(dem$workerid %in% unique(pl$WorkerId), "PL", dem$workerid)
pl_dem = dem[dem$ds == 'PL',]
dim(pl_dem)

################################### spammers

pl_spammer = c()

pl_dem$spammer = ifelse(pl_dem$workerid %in% pl_spammer, "yes", "no")
pl_spam = pl_dem[pl_dem$spammer == 'yes',]

sum(dem$workerid %in% pl_spammer)



mt_spammer = c()

length(mt_spammer)


matched_workers <- mt[mt$Assignment.ID %in% mt_spammer, "WorkerId"]
print(matched_workers)
length(unique(matched_workers))


mt_spammer1 = unique(matched_workers)
  
mt_dem$spammer = ifelse(mt_dem$workerid %in% mt_spammer1, "yes", "no")
mt_spam = mt_dem[mt_dem$spammer == 'yes',]
dim(mt_spam)
head(mt_spam)


################### visual

# Gender
data_summary <- mt_dem %>%
  group_by(gender) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(group = "Overall") %>%
  mutate(percentage = count / sum(count) * 100)

yes_summary <- mt_dem %>%
  filter(spammer == "yes") %>%
  group_by(gender) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(group = "Spammer") %>%
  mutate(percentage = count / sum(count) * 100)

combined_data <- bind_rows(data_summary, yes_summary)

ggplot(combined_data, aes(x = gender, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Comparison of Gender Distribution",
       x = "Gender", y = "Count",
       fill = "Group") +
  theme_minimal()


# Education 
data_summary <- mt_dem %>%
  group_by(education) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(group = "Overall") %>%
  mutate(percentage = count / sum(count) * 100)

yes_summary <- mt_dem %>%
  filter(spammer == "yes") %>%
  group_by(education) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(group = "Spammer") %>%
  mutate(percentage = count / sum(count) * 100)

combined_data <- bind_rows(data_summary, yes_summary)
combined_data[4,]$education = 'Doctorate'
combined_data[5,]$education = 'Master'
combined_data[8,]$education = 'Master'


ggplot(combined_data, aes(x = education, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Comparison of Education Distribution",
       x = "Education", y = "Count",
       fill = "Group") +
  theme_minimal()


# ethnic 
data_summary <- mt_dem %>%
  group_by(ethnic) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(group = "Overall") %>%
  mutate(percentage = count / sum(count) * 100)

yes_summary <- mt_dem %>%
  filter(spammer == "yes") %>%
  group_by(ethnic) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(group = "Spammer") %>%
  mutate(percentage = count / sum(count) * 100)

combined_data <- bind_rows(data_summary, yes_summary)

# combined_data[2,]$ethnic = 'Black or African American'
# combined_data[3,]$ethnic = "Hispanic or Latino"

ggplot(combined_data, aes(x = ethnic, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Comparison of Ethnicity Distribution",
       x = "Ethnicity", y = "Count",
       fill = "Group") +
  theme_minimal()

# Age
ggplot(mt_dem, aes(x = age, fill = spammer)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.5, position = "identity") +
  labs(title = "Age Distribution Comparison",
       x = "Age", y = "Density",
       fill = "Spammer") +
  theme_minimal()


################# PL 

# Gender
data_summary <- pl_dem %>%
  filter(!is.na(gender)) %>%  # Exclude NA values
  group_by(gender) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(group = "Overall") %>%
  mutate(percentage = count / sum(count) * 100)


yes_summary <- pl_dem %>%
  filter(spammer == "yes") %>%
  group_by(gender) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(group = "Spammer") %>%
  mutate(percentage = count / sum(count) * 100)

combined_data <- bind_rows(data_summary, yes_summary)

ggplot(combined_data, aes(x = gender, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Comparison of Gender Distribution",
       x = "Gender", y = "Count",
       fill = "Group") +
  theme_minimal()


# Education 
data_summary <- pl_dem %>%
  filter(!is.na(education)) %>% 
  group_by(education) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(group = "Overall") %>%
  mutate(percentage = count / sum(count) * 100)

yes_summary <- pl_dem %>%
  filter(spammer == "yes") %>%
  group_by(education) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(group = "Spammer") %>%
  mutate(percentage = count / sum(count) * 100)

combined_data <- bind_rows(data_summary, yes_summary)
combined_data <- combined_data %>%
  mutate(education = tools::toTitleCase(education))


ggplot(combined_data, aes(x = education, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Comparison of Education Distribution",
       x = "Education", y = "Count",
       fill = "Group") +
  theme_minimal()


# ethnic 
data_summary <- pl_dem %>%
  filter(!is.na(ethnic)) %>% 
  group_by(ethnic) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(group = "Overall") %>%
  mutate(percentage = count / sum(count) * 100)

yes_summary <- pl_dem %>%
  filter(spammer == "yes") %>%
  group_by(ethnic) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(group = "Spammer") %>%
  mutate(percentage = count / sum(count) * 100)

combined_data <- bind_rows(data_summary, yes_summary)
combined_data[2,]$ethnic = 'Black or African American'
combined_data[3,]$ethnic = "Hispanic or Latino"
combined_data[6,]$ethnic = 'Black or African American'
combined_data[7,]$ethnic = "Hispanic or Latino"

ggplot(combined_data, aes(x = ethnic, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Comparison of Ethnicity Distribution",
       x = "Ethnicity", y = "Count",
       fill = "Group") +
  theme_minimal()

# Age
ggplot(pl_dem, aes(x = age, fill = spammer)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.5, position = "identity") +
  labs(title = "Age Distribution Comparison",
       x = "Age", y = "Density",
       fill = "Spammer") +
  theme_minimal()


ggplot(pl_dem %>% filter(!is.na(age) & is.finite(age)), aes(x = age, fill = spammer)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.5, position = "identity") +
  labs(title = "Age Distribution Comparison",
       x = "Age", y = "Density",
       fill = "Spammer") +
  theme_minimal()



