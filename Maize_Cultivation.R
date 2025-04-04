## HEADER ####
## Who: <William Banda>
## What: Perfomance of Maize on CR and DBF
## Last edited: 2024-10-17


## CONTENTS ####
## 00 Setup
## 01 Import Data
## 02 Data Tidying
## 03 Exploratory Data Analysis
## 04 Statistical Analysis and Plotting Graphs


## 00 Setup
# Get working directory
getwd() 

## Set working directory
setwd("C:/Users/WilliamBanda1/Documents/Zuza")

#Let's get the necessarly libraries
library(readr)

# Load tidyverse
library(tidyverse)

# Load the dplyr package
library(dplyr)

# Load the openxlsx package
library(openxlsx)

# install vizreg package
install.packages("readxl")

# Load readxl
library(readxl)

# Install ggplot2 
install.packages("ggplot2")

# Load required libraries
library(tidyverse)  

library(RColorBrewer)

mypal <- c(brewer.pal(7, "Set3"), 
           "#f9faeb", # accessible background
           "#0A0A0A") # accessible font colour

## 01 Import Data
Data <- read_excel("C:/Users/WilliamBanda1/Documents/Zuza/MaizeData.xlsx", 
                   col_names = TRUE) 
# Lets explore the structure of the data frame again
str(Data)

## 02 Data Tidying
# lets remove rows with missing values
Data <- Data %>%
  rename(
    year = Year,
    epa = EPA,
    section = Section,
    system = System,
    plant_pop = `Plant pop`,
    no_of_cobs = `No cobs`,
    wt_of_stalks = `Wt of stalks`,
    yield_per_unit = `Yield of 10 m by 10 m)`
  )

#I didnt omit all the empty rows at once I have handled it separately below
#Data <- na.omit(Data)
#I have left the 2022 rows as well its justifiable to leave them
#Data <- Data %>%filter(year != 2022)

## 03 Exploratory Data Analysis

# lets view the colunm names in our dataset
names(Data)

# View the first few rows of the dataset
head(Data, n=5)

# lets view the last few roles of the dataset
tail(Data, n=5)

# Lets explore the structure of the data frame
str(Data)

# Lets convert these variables to numerical variables
Data <- Data %>%
  mutate(
    no_of_cobs = as.numeric(no_of_cobs),
    wt_of_stalks = as.numeric(wt_of_stalks),
    yield_per_unit = as.numeric(yield_per_unit),
    plant_pop = as.numeric(plant_pop),
  )

# Lets explore the structure of the data frame again
str(Data)

# lets view summary statistics for numeric variables and counts
summary(Data)

Data$epa

# Lets see how many EPAs were recorded
epa_counts <- table(Data$epa)

# View the counts
print(epa_counts)

# Lets check the systems
system_counts <- table(Data$system)

# View the counts
print(system_counts)

# Remove rows with NA values in critical columns
maize_data_clean <- Data %>%
  filter(!is.na(system) & !is.na(yield_per_unit))

View(maize_data_clean)

# Summary statistics by system
summary_stats <- maize_data_clean %>%
  group_by(system) %>%
  summarise(mean_yield = mean(yield_per_unit, na.rm = TRUE),
            sd_yield = sd(yield_per_unit, na.rm = TRUE),
            count = n())

print(summary_stats)

#Lets check for normality since we will use welch's t-test
ggplot(maize_data_clean, aes(x = yield_per_unit)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Yield", x = "Yield (10m x 10m)", y = "Frequency") +
  theme_minimal()

ggplot(maize_data_clean, aes(x = wt_of_stalks )) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Yield", x = "Yield (10m x 10m)", y = "Frequency") +
  theme_minimal()

ggplot(maize_data_clean, aes(x = no_of_cobs )) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Yield", x = "Yield (10m x 10m)", y = "Frequency") +
  theme_minimal()

ggplot(maize_data_clean, aes(x = plant_pop )) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Yield", x = "Yield (10m x 10m)", y = "Frequency") +
  theme_minimal()

## 04 Statistical Analysis and Plotting Graphs
# Visualize yield distribution 
ggplot(maize_data_clean, aes(x = system, y = yield_per_unit)) +
  geom_boxplot(fill = mypal[4], color = mypal[9], outlier.shape = NA) +  # Exclude outliers
  geom_jitter(color = mypal[9], size = 1, alpha = 0.5) +  # Jittered points for visibility
  labs(title = "Yield Distribution by Farming System", 
       y = "Yield (10m x 10m)", 
       x = "Farming System") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = mypal[8], color = NA), 
    panel.background = element_rect(fill = mypal[8], color = NA), 
    legend.position = "none",
    axis.title.x = element_text(size = 14, color = mypal[9]), 
    axis.title.y = element_text(size = 14, color = mypal[9]), 
    axis.text.x = element_text(size = 12, color = mypal[9]),  
    axis.text.y = element_text(size = 12, color = mypal[9]),  
    axis.ticks = element_line(color = mypal[9])  
  )

# Plot for plant_pop
ggplot(maize_data_clean, aes(x = system, y = plant_pop)) +
  geom_boxplot(fill = mypal[2], color = mypal[9], outlier.shape = NA) +  
  geom_jitter(color = mypal[9], size = 1, alpha = 0.5) +
  labs(title = "Plant Population by Farming System", 
       y = "Plant Population", 
       x = "Farming System") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = mypal[8], color = NA), 
    panel.background = element_rect(fill = mypal[8], color = NA), 
    legend.position = "none",
    axis.title.x = element_text(size = 14, color = mypal[9]), 
    axis.title.y = element_text(size = 14, color = mypal[9]), 
    axis.text.x = element_text(size = 12, color = mypal[9]),  
    axis.text.y = element_text(size = 12, color = mypal[9]),  
    axis.ticks = element_line(color = mypal[9])
  )

# Plot for no_of_cobs
ggplot(maize_data_clean, aes(x = system, y = no_of_cobs)) +
  geom_boxplot(fill = mypal[1], color = mypal[9], outlier.shape = NA) +  
  geom_jitter(color = mypal[9], size = 1, alpha = 0.5) +
  labs(title = "Number of Cobs by Farming System", 
       y = "Number of Cobs", 
       x = "Farming System") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = mypal[8], color = NA), 
    panel.background = element_rect(fill = mypal[8], color = NA), 
    legend.position = "none",
    axis.title.x = element_text(size = 14, color = mypal[9]), 
    axis.title.y = element_text(size = 14, color = mypal[9]), 
    axis.text.x = element_text(size = 12, color = mypal[9]),  
    axis.text.y = element_text(size = 12, color = mypal[9]),  
    axis.ticks = element_line(color = mypal[9])
  )

# Plot for wt_of_stalks
ggplot(maize_data_clean, aes(x = system, y = wt_of_stalks)) +
  geom_boxplot(fill = mypal[6], color = mypal[9], outlier.shape = NA) +  
  geom_jitter(color = mypal[9], size = 1, alpha = 0.5) +
  labs(title = "Weight of Stalks by Farming System", 
       y = "Weight of Stalks", 
       x = "Farming System") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = mypal[8], color = NA), 
    panel.background = element_rect(fill = mypal[8], color = NA), 
    legend.position = "none",
    axis.title.x = element_text(size = 14, color = mypal[9]), 
    axis.title.y = element_text(size = 14, color = mypal[9]), 
    axis.text.x = element_text(size = 12, color = mypal[9]),  
    axis.text.y = element_text(size = 12, color = mypal[9]),  
    axis.ticks = element_line(color = mypal[9])
  )

#Lets test the hypotheses

#a.Main Hypothesis:
#H0: There is no difference in yield between DBF and CR systems.
#H1: DBF yields are higher than CR yields.

#Was not normally distributed
# Perform Mann-Whitney U test (Wilcoxon rank-sum test)
mann_whitney_result <- wilcox.test(yield_per_unit ~ system, data = maize_data_clean)

# Print the result of the Mann-Whitney U test
print(mann_whitney_result)

#Hypothesis on Number of Cobs:
#H0: There is no difference in the number of cobs between DBF and CR systems.
#H1: DBF has a higher number of cobs than CR.

# Perform t-test for weight of stalks
t_test_no_cobs <- t.test(no_of_cobs ~ system, data = maize_data_clean)
print(t_test_no_cobs)

# Remove rows with NA values in critical columns
maize_data_clean <- Data %>%
  filter(!is.na( plant_pop) & !is.na(wt_of_stalks))

#Hypothesis on plant population:
#H0: There is no difference in plant population between DBF and CR systems.
#H1: DBF has a higher plant population than CR.

# Perform t-test for plant population
t_test_plant_pop <- t.test(plant_pop ~ system, data = maize_data_clean)
print(t_test_plant_pop)

#Hypothesis on weight of stalks:
#H0: There is no difference in weight of stalks between DBF and CR systems.
#H1: DBF has a higher weight of stalks than CR.

#Was not normally distributed
# Perform Mann-Whitney U test (Wilcoxon rank-sum test)
mann_whitney_stalks <- wilcox.test(wt_of_stalks ~ system, data = maize_data_clean)

# Print the result of the Mann-Whitney U test
print(mann_whitney_stalks)


#Conclusion
#In summary, DBF consistently outperforms CR in all measured aspects.

