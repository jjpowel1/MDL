#For data manipulation
library(tidyverse)
#Create directory paths
library(here)
#Read Excel Files
library(readxl)


#Set Working Directory
setwd("/Users/jaylenpowell/Desktop/Research Method")
here::i_am("Chlorophyll MDL Research method.xlsx")

data <- read_excel("Chlorophyll MDL Research method.xlsx")

#MDL @ 99% Confidence - 

# Calculate degrees of freedom
df <- nrow(data) - 1  # df = number of rows - 1

# Calculate standard deviation (stdev) of 'chla_ug.L'
stdev <- sd(data$chla_ug.L)



# Perform Student's t-test at 99% confidence level
t_result <- t.test(data$chla_ug.L, conf.level = 0.99)

#See Student's t-test at 99%
print(t_result)


#Testing out value because our critical t was just at .99
critical_t_value2 <- qt(0.99, df)  # For 99% confidence level (two-tailed)

#See Critical t-value
print(critical_t_value2)

# Calculate MDL (Method Detection Limit)
MDL <- stdev * critical_t_value2

#Print MDL
print(MDL)

# Access MDL values from the column "MDL (ug/L)"
mdl_values <- data$`MDL (ug/L)`[1]

print(mdl_values)

# Define tolerance for comparison
tolerance <- 1e-2  # You can adjust this tolerance as needed

# Check if MDL calculated in R is equal to the values in the Excel column
if(abs(MDL - mdl_values) < tolerance) {
  print("MDL calculated in R is equal to the values in the Excel column.")
} else {
  print("MDL calculated in R is not equal to the values in the Excel column.")
}

