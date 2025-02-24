###### R Script no. 1########




##########Loading Libraries########
library(dplyr)
library(readxl)
install.packages("stargazer")
library(stargazer)
###################################

##########Loading Data#############
df <- read.csv("/Users/ahmedbilal/Library/CloudStorage/OneDrive-UniversityofIllinois-Urbana/Spring 2025/Business Practicum/Clean_Data.csv", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
View(df)
###################################

##########Convert categorical variables to factors#############
df <- df %>%
  mutate(
    gender = as.factor(gender),
    race_ethnicity = as.factor(race_ethnicity),
    employment_status = as.factor(employment_status)
  )

df$individual_income <- as.numeric(gsub(",", "", df$individual_income))
df$household_income <- as.numeric(gsub(",", "", df$household_income))
###############################################################


##########Run the regression model######################################
model_1 <- lm(course_completion ~ age + gender + race_ethnicity + 
              employment_status + individual_income + household_income + 
              work_experience, data = df)

# Display summary of the model
summary(model_1)
#########################################################################



##########Viewing Results######################################
stargazer(model_1, type = "text", title = "Regression Results", 
          dep.var.labels = "Course Completion (%)",
          covariate.labels = c("Age", "Gender", "Race/Ethnicity", 
                               "Employment Status", "Individual Income", 
                               "Household Income", "Work Experience"),
          star.cutoffs = c(0.1, 0.05, 0.01))  # Significance levels (*, **, ***)
###############################################################