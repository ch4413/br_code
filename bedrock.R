# Work for Bedrock

# install.packages("readr")
library(readr)
library(dplyr)

setwd("/Users/chughes/Downloads/")

data <- read.csv("wp_rbst_learndash_course_completion (22).csv", sep = ";")

aa <- data %>%
  filter(display_name == "Aman Gidwani")

View(aa)

View(data)