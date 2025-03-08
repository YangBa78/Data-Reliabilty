library(readxl)
library(dplyr)

dem <- read.csv("C:/Users/alex7/Downloads/ai-deferral Demographics.csv")
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
mt <- read.csv("E:/ASU Dropbox/Yang Ba/new_dq/Data-Reliabilty/Data/MTurkPerformanceData.csv")
colnames(mt)

length(unique(mt$WorkerId))

dem$ds <- ifelse(dem$workerid %in% unique(mt$WorkerId), "MT", "No")
mt_dem = dem[dem$ds == 'MT',]
dim(mt_dem)



pl <- read_excel("E:/ASU Dropbox/Yang Ba/new_dq/Data-Reliabilty/Data/Prolific.xlsx")
pl = pl[pl['WorkerId']!='2f33f23f23',]

colnames(pl)

length(unique(pl$WorkerId))


dem$ds <- ifelse(dem$workerid %in% unique(pl$WorkerId), "PL", dem$workerid)
pl_dem = dem[dem$ds == 'PL',]
dim(pl_dem)

################################### spammers

pl_spammer = c('61130aa6625c523beb237643', 
              '60c07e5182bf1353056371f6',
              '611276eef2fedd783d9b30ff',
              '616ec4d19fc1b4726c15d3ab',
              '5dc2c4b8e95f941e092f99ff',
              '61688905f063fbae9713285e',
              '611bf41079ffca32db5a3080',
              'cd6bc4604748d000119f0a7',
              '613e329368ac7e3cd6bb0ab7',
              '5bb511c6689fc5000149c703')

pl_dem$spammer = ifelse(pl_dem$workerid %in% pl_spammer, "yes", "no")
pl_spam = pl_dem[pl_dem$spammer == 'yes',]

sum(dem$workerid %in% pl_spammer)



mt_spammer = c("3D4CH1LGECVVA3APCBE8BV5NASD9GM", "3907X2AHF27QNWIPULPGVHB15DX2PY", "3IGI0VL649M4RIVH14ALY59963QONS", 
               "3NPI0JQDAQ7K7VC2D09CW4KR5Q3PT6", "30BUDKLTXFXEP2CM3QB3ZCF9BGFE5J", "3RRCEFRB7OEYMGW2FUQ3TC3A8N7B4B", 
               "3WI0P0II63UY2JWK42DMWV2K3M3DR3", "3VSOLARPKDBUGRY1HW7OS1GE6N8398", "33JKGHPFYEWGRFA5OOV861K4B36NM7", "3IKZ72A5B6IAL5IO21RVBB0VE2UNF0",
               "3SNLUL3WO6P9GNCTLD6FKJX9JU9LUV", "30OG32W0SWDIFRIS7Q7PTTFV21KENE", "3A7Y0R2P2QQV21IONX5POBJL9NKXJW", "3HL8HNGX473PW3EU1R8QTA5B9DD9FN",
               "3PW9OPU9PSMN64ZN9JEQULT32XK21N", "3R0T90IZ1UEX81HSOVOZDL6VGDGCG6", "33FOTY3KEON04MRP6C3W6Y2PEOYC17", "30BUDKLTXFXEP2CM3QB3ZCF9BGF5EA",
               "30X31N5D65SC5RTLZHMPW4ZA4LPASM", "3QIYRE09Y5JJVQOGV2CAM9W1DSBN1Z", "3E1QT0TDFRB9SPXAX78XWBNS5ZN8IO", "3YMU66OBIPAOP9S8AN20OC3BT5LGHJ",
               "3GFK2QRXXBJ862YDOQ59LIM9IKPW5M", "3HRMW88U18SD6JIYNITAVPOR3AV0M9", "3PXX5PX6LZ0DO5CJOV0U6FPA0X3ABL", "3SNVL38CI6U2ANDBEM9EBDYONUXKCH",
               "3Z9WI9EOZ1QTRVOR0ENFM4V40M2KHQ", "3DL65MZB8FHZONL1I0YBDILJ3X1ECR", "3K4J6M3CXGV9FMKU4HS87QG7GAVGA8", "3BXQMRHWK10TM5UELP7UKWCYJ0GMUH",
               "3DY46V3X3RKETJJTSPI0VI9WYNG559", "3PM8NZGV80ICOOM1INPYLCV7NIIQXM", "3TE3O85732ABZ9Y47DT50CBG6VE2RJ", "3EQHHY4HQUUC9G8D9L1RZF0LOWU5GR",
               "3WQ3B2KGEAIHUHZJSC71VEW7W3XB1Y")

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



